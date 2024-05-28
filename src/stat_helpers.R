library(elastic)
library(dplyr)

source("src/elastic_helpers.R")
source("src/table_helpers.R")
source("src/constants.R")

too_slow_request <- function(sec) {
  sec <- round(sec)
  msg <- glue::glue("Request will take {sec} seconds")
  
  rlang::signal("request_too_slow", 
        message = msg,
        sec = sec
  )
}

get_record_count <- function(con, query) {
  b <- list(query = query, size = 0)
  res <- Search(con, index = index, body = b)
  count <- res$hits$total$value
}

is_heavy_request <- function(con, query, threshold = 5 * elastic_max_response_size) {
  count <- get_record_count(con, query)
  if (count > threshold) {
    number_of_requests <- ceiling(count / elastic_max_response_size)
    too_slow_request(number_of_requests * elastic_single_request_time)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

get_user_statistics <- function(con, query, adjust = TRUE) {
  b <- build_user_statistics_query(query)
  res <- Search(con, index = index, body = b, asdf = T)
  dt <- generate_user_statistics(res, adjust = adjust)
}

build_bucket_aggregation_query <- function(fields, query) {
  custom_aggs <- list(
    "cpu_avail_sec" = build_elastic_sub_agg("AVAIL_CPU_TIME_SEC", "sum"),
    "cpu_wasted_sec" = build_elastic_sub_agg("WASTED_CPU_SECONDS", "sum"),
    "mem_avail_mb_sec" = build_elastic_sub_agg("MEM_REQUESTED_MB_SEC", "sum"),
    "mem_wasted_mb_sec" = build_elastic_sub_agg("WASTED_MB_SECONDS", "sum"),
    "wasted_cost" = wasted_cost_agg
  )

  b <- build_terms_query(
    fields = fields,
    aggs = custom_aggs,
    query = query
  )
}

build_user_statistics_query <- function(query) {
  build_bucket_aggregation_query(
    fields = c("NUM_EXEC_PROCS", "Job"),
    query = query
  )
}

generate_user_statistics <- function(res, adjust = TRUE) {
  df <- parse_elastic_multi_agg(res, column_names = c('procs', 'job_status')) %>%
    select(-doc_count)

  dt <- generate_app_wastage_statistics(df, adjust = adjust)

  dt_total <- generate_total_wastage_dt(dt)

  dt <- rbind(dt, dt_total)

  specify_wastage_reason(dt)
}

get_team_statistics <- function(con, query, adjust = TRUE, force = TRUE) {
  if(!force){
    if(is_heavy_request(con, query))
      return(NULL)
  }

  b <- list(query = query)
  res <- Search(
    con,
    index = index,
    time_scroll="1m",
    source = c('USER_NAME', 'Job',
              'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
              'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS'),
    body = b,
    asdf = T,
    size = elastic_max_response_size
  )
  df <- pull_everything(con, res)
  dt <- generate_team_statistics(df, adjust = adjust)
}

generate_team_statistics <- function (df, adjust = TRUE) {
  if (adjust) {
    df <- df %>%
      mutate(
        cpu_wasted_sec = ifelse(Job == 'Success' & NUM_EXEC_PROCS == 1, 0, WASTED_CPU_SECONDS)
      )
  } else {
    df <- rename(df, cpu_wasted_sec = WASTED_CPU_SECONDS)
  }

  df %>%
    rename(
      cpu_avail_sec = AVAIL_CPU_TIME_SEC,
      mem_avail_mb_sec = MEM_REQUESTED_MB_SEC,
      mem_wasted_mb_sec = WASTED_MB_SECONDS
    ) %>%
    mutate(
      cpu_wasted_cost = cpu_wasted_sec * cpu_second,
      mem_wasted_cost = mem_wasted_mb_sec * ram_mb_second,
      wasted_cost = pmax(cpu_wasted_cost, mem_wasted_cost)
    ) %>%
    group_by(USER_NAME) %>%
    generate_efficiency_stats(
      extra_stats = list(
        number_of_jobs = quote(n()),
        wasted_cost = quote(sum(wasted_cost)),
        fail_rate = quote(sum(Job == 'Failed') / number_of_jobs)
      )
    ) %>%
    select(USER_NAME, number_of_jobs, fail_rate, cpu_avail_hrs,
           cpu_wasted_hrs, cpu_wasted_frac, mem_avail_gb_hrs,
           mem_wasted_gb_hrs, mem_wasted_frac, wasted_cost)
}

build_bom_aggregation <- function(query) {
  build_bucket_aggregation_query(
    fields = c("ACCOUNTING_NAME", "NUM_EXEC_PROCS", "Job"),
    query = query
  )
}

generate_bom_statistics <- function(df, adjust = TRUE) {
  if (adjust) {
    df <- adjust_aggregated_statistics(df)
  }

  df %>%
    group_by(accounting_name) %>%
    generate_efficiency_stats() %>%
    select(accounting_name, cpu_avail_hrs, cpu_wasted_hrs, cpu_wasted_frac, mem_avail_gb_hrs, mem_wasted_gb_hrs, mem_wasted_frac, wasted_cost) %>%
    rename_group_column() -> dt

  ranks <- generate_ranks(dt) %>% select(accounting_name, awesomeness)

  dt %>%
    left_join(ranks, by = 'accounting_name') -> dt
}

get_bom_statistics <- function (con, query, adjust = TRUE) {
  b <- build_bom_aggregation(query)
  res <- Search(con, index = index, body = b, asdf = T)

  df <- parse_elastic_multi_agg(res, column_names = c('accounting_name', 'procs', 'job_status')) %>%
    select(-doc_count)

  generate_bom_statistics(df, adjust = adjust)
}
