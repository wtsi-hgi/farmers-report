library(elastic)
library(dplyr)
library(tsibble)
library(tidyr)
loadNamespace('lubridate')


source("src/elastic_helpers.R")
source("src/table_helpers.R")
source('src/timeseries_helpers.R')

generate_efficiency_extra_stats <- list(
  number_of_jobs = quote(n()),
  fail_rate = quote(sum(job_status == 'Failed') / number_of_jobs)
)

get_user_statistics <- function (con, query, adjust = TRUE, time_bucket = 'none') {
  b <- build_user_statistics_query(query, time_bucket = time_bucket)
  res <- Search(con, index = attr(con, 'index'), body = b, asdf = T)

  df <- parse_elastic_agg(res, b)

  dt <- generate_user_statistics(df, adjust = adjust, timed = time_bucket != 'none')
}

build_bucket_aggregation_query <- function(fields, query, time_bucket = 'none') {
  custom_aggs <- list(
    "cpu_avail_sec" = build_elastic_sub_agg("AVAIL_CPU_TIME_SEC", "sum"),
    "cpu_wasted_sec" = build_elastic_sub_agg("WASTED_CPU_SECONDS", "sum"),
    "mem_avail_mb_sec" = build_elastic_sub_agg("MEM_REQUESTED_MB_SEC", "sum"),
    "mem_wasted_mb_sec" = build_elastic_sub_agg("WASTED_MB_SECONDS", "sum"),
    "wasted_cost" = wasted_cost_agg
  )

  aggs <- list(
      build_multi_terms_agg(fields = fields),
      new_elastic_agg(custom_aggs, type = 'compute')
  )

  if (time_bucket != 'none') {
    aggs <- append(aggs, list(build_date_agg(interval = time_bucket)), after = 0)
  }

  b <- build_elasic_agg(
    aggs = aggs,
    query = query
  )
}

build_user_statistics_query <- function(query, time_bucket = 'none') {
  build_bucket_aggregation_query(
    fields = c("NUM_EXEC_PROCS", "Job"),
    query = query,
    time_bucket = time_bucket
  )
}

generate_user_statistics <- function(df, adjust = TRUE, timed = FALSE) {
  dt <- generate_app_wastage_statistics(df, adjust = adjust, timed = timed)

  if (!timed) {
    dt_total <- generate_total_wastage_dt(dt)
    dt <- rbind(dt, dt_total)
  }

  specify_wastage_reason(dt)
}

get_team_statistics <- function(con, query, time_bucket = "none", adjust = TRUE) {
  df <- scroll_elastic(
    con = con,
    body = list(query = query),
    fields = c('USER_NAME', 'Job', 'timestamp', 
              'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
              'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS')
  )
  df$timestamp <- lubridate::as_datetime(df$timestamp)
  dt <- generate_team_statistics(df, time_bucket, adjust = adjust)
}

generate_team_statistics <- function (df, time_bucket = "none", adjust = TRUE) {
  df <- rename_raw_elastic_fields(df)

  if (adjust)
    df <- adjust_statistics(df)

  df <- generate_wasted_cost(df)
  
  if(time_bucket != "none") {
    df <- df %>%
      as_tsibble(key = `_id`, index = timestamp) %>%
      index_by_custom(time_bucket = time_bucket)
  }

  df %>%
    group_by(USER_NAME) %>%
    generate_efficiency_stats(
      extra_stats = generate_efficiency_extra_stats
    )
}

build_bom_aggregation <- function(query, time_bucket = 'none') {
  build_bucket_aggregation_query(
    fields = c("ACCOUNTING_NAME", "NUM_EXEC_PROCS", "Job"),
    time_bucket = time_bucket,
    query = query
  )
}

generate_bom_statistics <- function(df, timed = FALSE, adjust = TRUE) {
  if (adjust) {
    df <- adjust_statistics(df)
  }

  groups <- c('accounting_name')
  cols <- c('accounting_name', 'cpu_avail_hrs', 'cpu_wasted_hrs', 'cpu_wasted_frac', 
            'mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost')
  if(timed) {
    groups <- append(groups, 'timestamp')
    cols <- append(cols, 'timestamp', after = 0)
  }

  df %>%
    group_by(across(all_of(groups))) %>%
    generate_efficiency_stats() %>%
    select(all_of(cols)) %>%
    rename_group_column() -> dt

  if(!timed) {
    ranks <- generate_ranks(dt) %>% select(accounting_name, awesomeness)

    dt %>%
      left_join(ranks, by = 'accounting_name') -> dt
  }

  return(dt)
}

get_bom_statistics <- function (con, query, adjust = TRUE, time_bucket = 'none') {
  b <- build_bom_aggregation(query, time_bucket)
  res <- Search(con, index = attr(con, 'index'), body = b, asdf = T)

  df <- parse_elastic_agg(res, b) %>%
    select(-doc_count)

  generate_bom_statistics(df, timed = time_bucket != 'none', adjust = adjust)
}

get_job_statistics <- function (con, query, time_bucket = 'none') {
  df <- scroll_elastic(
    con = con,
    body = list(query = query),
    fields = c('timestamp', 'JOB_NAME', 'Job',
              'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
              'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS')
  )
  df$timestamp <- lubridate::as_datetime(df$timestamp)
  dt <- generate_job_statistics(df, time_bucket = time_bucket)
}

generate_job_statistics <- function (df, time_bucket = 'none') {
  dt <- df %>%
    rename_raw_elastic_fields() %>%
    adjust_statistics() %>%
    generate_wasted_cost() %>%
    mutate(job_type = sapply(job_name, parse_job_type))

  if (time_bucket != 'none') {
    dt <- dt %>%
      as_tsibble(key = `_id`, index = timestamp) %>%
      index_by_custom(time_bucket = time_bucket)
  }
  
  dt %>%
    group_by(job_type) %>%
    generate_efficiency_stats(
      extra_stats = generate_efficiency_extra_stats
    )
}

get_job_failure_statistics <- function(con, query, fields, time_bucket = "none") {

  if (length(fields) == 1){
    term_agg <- build_terms_agg(field = fields)
  } else {
    term_agg <- build_multi_terms_agg(fields = fields)
  }
  
  aggs <- list(term_agg)

  if (time_bucket != 'none') {
    aggs <- append(aggs, list(build_date_agg(interval = time_bucket)), after = 0)
  }

  b <- build_elasic_agg(aggs = aggs, query = query)

  res <- Search(con, index = attr(con, 'index'), body = b, asdf = T)

  df <- parse_elastic_agg(res, b)

  if(time_bucket == "none")
    df <- rename_group_column(df)

  return(df)
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
    fields = c('timestamp', 'USER_NAME', 'QUEUE_NAME', 'Job', 'PENDING_TIME_SEC', 'RUN_TIME_SEC')
  )

  df$timestamp <- lubridate::as_datetime(df$timestamp)

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

generate_app_wastage_statistics <- function(df, adjust = TRUE, timed = FALSE) {
  if (adjust) {
    df <- adjust_statistics(df)
  }

  groups <- c('job_status')
  cols <- c('job_status', 'number_of_jobs', 'fail_rate', 'cpu_avail_hrs', 'cpu_wasted_hrs', 'cpu_wasted_frac',
            'mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost')

  if(timed) {
    groups <- append(groups, 'timestamp')
    cols <- append(cols, 'timestamp', after = 0)
  }

  df %>%
    group_by(across(all_of(groups))) %>%
    generate_efficiency_stats(extra_stats = list(
        number_of_jobs = quote(sum(doc_count)),
        fail_rate = quote(sum(ifelse(job_status == 'Failed', doc_count, 0)) / number_of_jobs)
    )) %>%
    select(all_of(cols))
}
