library(elastic)
library(dplyr)
library(tsibble)
loadNamespace('lubridate')


source("src/elastic_helpers.R")
source("src/table_helpers.R")

generate_efficiency_extra_stats <- list(
  number_of_jobs = quote(n()),
  fail_rate = quote(sum(job_status == 'Failed') / number_of_jobs)
)

get_user_statistics <- function (con, query, adjust = TRUE) {
  b <- build_user_statistics_query(query)
  res <- Search(con, index = index, body = b, asdf = T)

  df <- parse_elastic_agg(res, b) %>%
    select(-doc_count)

  dt <- generate_user_statistics(df, adjust = adjust)
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

generate_user_statistics <- function(df, adjust = TRUE) {
  dt <- generate_app_wastage_statistics(df, adjust = adjust)

  dt_total <- generate_total_wastage_dt(dt)

  dt <- rbind(dt, dt_total)

  specify_wastage_reason(dt)
}

get_team_statistics <- function(con, query, adjust = TRUE) {
  df <- scroll_elastic(
    con = con,
    body = list(query = query),
    fields = c('USER_NAME', 'Job',
              'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
              'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS')
  )
  dt <- generate_team_statistics(df, adjust = adjust)
}

generate_team_statistics <- function (df, adjust = TRUE) {
  df <- rename_raw_elastic_fields(df)

  if (adjust)
    df <- adjust_statistics(df)

  df %>%
    generate_wasted_cost() %>%
    group_by(USER_NAME) %>%
    generate_efficiency_stats(
      extra_stats = generate_efficiency_extra_stats
    )
}

build_bom_aggregation <- function(query) {
  build_bucket_aggregation_query(
    fields = c("ACCOUNTING_NAME", "NUM_EXEC_PROCS", "Job"),
    query = query
  )
}

generate_bom_statistics <- function(df, adjust = TRUE) {
  if (adjust) {
    df <- adjust_statistics(df)
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

  df <- parse_elastic_agg(res, b) %>%
    select(-doc_count)

  generate_bom_statistics(df, adjust = adjust)
}

get_job_statistics <- function (con, query) {
  df <- scroll_elastic(
    con = con,
    body = list(query = query),
    fields = c('JOB_NAME', 'Job',
              'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
              'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS')
  )
  dt <- generate_job_statistics(df)
}

generate_job_statistics <- function (df) {
  dt <- df %>%
    rename_raw_elastic_fields() %>%
    adjust_statistics() %>%
    generate_wasted_cost() %>%
    mutate(job_type = sapply(job_name, parse_job_type)) %>%
    group_by(job_type) %>%
    generate_efficiency_stats(
      extra_stats = generate_efficiency_extra_stats
    )
}

parse_job_type <- function (job_name) {

  if (startsWith(job_name, "nf-"))
    return('nextflow')

  if (startsWith(job_name, 'wrp_'))
    return('wr')

  if (startsWith(job_name, 'bsub rstudio'))
    return('interactive')

  return('other')
}

get_gpu_records <- function(con, query) {
  queue_filter <- list(
    "prefix" = list("QUEUE_NAME" = "gpu")
  )
  query$bool$filter <- c(query$bool$filter, list(queue_filter))

  df <- scroll_elastic(
    con = con,
    body = list(query = query),
    fields = c('timestamp', 'JOB_ID', 'USER_NAME', 'QUEUE_NAME', 'Job', 'PENDING_TIME_SEC', 'RUN_TIME_SEC')
  )

  df$timestamp = lubridate::as_datetime(df$timestamp)

  return(df)
}

generate_gpu_statistics <- function(df) {
  dt <- df %>%
    group_by(USER_NAME, QUEUE_NAME) %>%
    summarise(
      number_of_jobs = n(),
      fail_rate = sum(Job == 'Failed') / number_of_jobs,
      median_wait_time = median(PENDING_TIME_SEC),
      median_run_time = median(RUN_TIME_SEC),
      .groups = 'drop'
    )
}

index_by_custom <- function(df, time_bucket) {
  validate_time_bucket(time_bucket)

  if(time_bucket == 'day')
    dt <- index_by(df, date = ~ as.Date(.))
  
  if(time_bucket == 'week')
    dt <- index_by(df, date = ~ yearweek(.))

  if(time_bucket == 'month')
    dt <- index_by(df, date = ~ yearmonth(.))
  
  return(dt)
}

generate_gpu_plot <- function(df, time_bucket, metric = PENDING_TIME_SEC) {
  colname <- paste(rlang::enquo(metric), "median", sep = "_")[2]
  # Warning: Error in validate_tsibble: A valid tsibble must have distinct rows identified by key and index.
  dt <- df %>%
    as_tsibble(key = JOB_ID, index = timestamp) %>%
    # group_by_key() %>%
    group_by(USER_NAME) %>%
    index_by_custom(time_bucket = time_bucket) %>%
    summarise(!!colname := median({{metric}}))

  ggplot(dt, aes(x = date, y = .data[[colname]], fill = USER_NAME)) + geom_bar(stat = 'identity') + theme_bw()
}
