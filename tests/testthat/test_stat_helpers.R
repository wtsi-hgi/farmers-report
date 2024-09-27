library(testthat)

Sys.setenv(NOT_CRAN = "true")

context("Test functions to generate statistics")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/stat_helpers.R")
source("src/constants.R")

test_that("generate_bom_statistics works", {
  df <- data.frame(
    accounting_name = c('Team A', 'Team A', 'Team B'),
    cpu_avail_sec = c(300, 800, 1000),
    cpu_wasted_sec = c(100, 200, 500),
    mem_avail_mb_sec = c(3000, 6000, 10000),
    mem_wasted_mb_sec = c(1000, 2000, 5000),
    wasted_cost = c(0.5, 2, 5),
    job_status = c('Success', 'Failed', 'Success'),
    procs = c(1, 2, 6)
  )

  expected_columns <- c(
    'accounting_name', 'cpu_avail_hrs', 'cpu_wasted_hrs', 'cpu_wasted_frac', 
    'mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost', 'awesomeness'
  )

  # with adjustment
  dt <- generate_bom_statistics(df)
  expect_s3_class(dt,'data.frame')
  expect_named(dt, expected_columns)

  # without adjustment
  dt <- generate_bom_statistics(df, adjust = FALSE)
  expect_s3_class(dt,'data.frame')
  expect_named(dt, expected_columns)

  # with timed = TRUE
  df$timestamp <- as.Date(c('2024-06-22', '2024-06-23', '2024-06-24'))
  dt <- generate_bom_statistics(df, timed = TRUE)

  expected_columns <- append(expected_columns, 'timestamp', after = 0)
  expected_columns <- setdiff(expected_columns, 'awesomeness')

  expect_s3_class(dt,'data.frame')
  expect_named(dt, expected_columns)
})

test_that("build_user_statistics_query works", {
  q <- list("bool" = list())
  b <- build_user_statistics_query(q)

  expect_type(b$aggs$stats$aggs, "list")
  expect_named(b$aggs$stats$aggs, c('cpu_avail_sec', 'cpu_wasted_sec', 'mem_avail_mb_sec', 'mem_wasted_mb_sec', 'wasted_cost'))

  expect_equal(
    b$aggs$stats$multi_terms$terms,
    list(
      list("field" = "NUM_EXEC_PROCS"),
      list("field" = "Job")
    )
  )
})

test_that("generate_user_statistics works", {
  fake_df <- tibble::tibble(
    procs = c(1, 2),
    job_status = c("Success", "Failed"),
    doc_count = c(10, 20),
    mem_avail_mb_sec = c(500, 750),
    mem_wasted_mb_sec = c(1000, 2000),
    cpu_avail_sec = c (50, 75),
    cpu_wasted_sec = c(100, 200),
    wasted_cost = c(0.1, 0.2)
  )

  expected_columns <- c(
    'Reason', 'number_of_jobs', 'fail_rate', 'cpu_avail_hrs', 'cpu_wasted_hrs', 'cpu_wasted_frac', 
    'mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost'
  )

  # with adjustment
  dt <- generate_user_statistics(fake_df)
  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)
  expect_true('Total' %in% dt$Reason)

  # without adjustment
  dt <- generate_user_statistics(fake_df, adjust = FALSE)
  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)

  # with timestamp
  fake_df$timestamp <- as.Date(c('2024-06-22', '2024-06-23'))
  dt <- generate_user_statistics(fake_df, timed = TRUE)

  expected_columns <- append(expected_columns, 'timestamp', after = 0)

  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)
})

test_that("generate_team_statistics works", {
  fake_data_frame <- data.frame(
    USER_NAME = c('user1', 'user2', 'user2'),
    Job = c('Success', 'Success', 'Failed'),
    NUM_EXEC_PROCS = c(1, 1, 3),
    AVAIL_CPU_TIME_SEC = c(800, 1000, 1200),
    WASTED_CPU_SECONDS = c(600, 500, 700),
    MEM_REQUESTED_MB = c(1200, 2400, 3600), 
    MEM_REQUESTED_MB_SEC = c(12000, 42000, 60000),
    WASTED_MB_SECONDS = c(5000, 20000, 10000),
    "_id" = c(123, 456, 789),
    "timestamp" = as.Date(c("2024-01-01", "2024-01-2", "2024-01-03")),
    check.names = FALSE
  )

  expected_columns <- c(
    'USER_NAME', 'number_of_jobs', 'fail_rate', 'cpu_avail_hrs', 'cpu_wasted_hrs', 
    'cpu_wasted_frac','mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost'
  )

  # with adjustment
  dt <- generate_team_statistics(fake_data_frame)
  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)
  
  # without adjustment
  dt <- generate_team_statistics(fake_data_frame, adjust = FALSE)
  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)

  # with time_bucket
  dt <- generate_team_statistics(fake_data_frame, time_bucket = 'day')
  expect_s3_class(dt, 'tbl_ts')
  expected_columns <- append(expected_columns, 'date')
})

test_that("build_bom_aggregation works", {
  q <- list("bool" = list())
  b <- build_bom_aggregation(q)

  expect_type(b$aggs$stats$aggs, "list")
  expect_named(b$aggs$stats$aggs, c('cpu_avail_sec', 'cpu_wasted_sec', 'mem_avail_mb_sec', 'mem_wasted_mb_sec', 'wasted_cost'))

  expect_equal(
    b$aggs$stats$multi_terms$terms, 
    list(
      list("field" = "ACCOUNTING_NAME"),
      list("field" = "NUM_EXEC_PROCS"), 
      list("field" = "Job")
    )
  )
})

test_that("generate_job_statistics works", {
  fake_data_frame <- data.frame(
    Job = c('Success', 'Success', 'Failed'),
    NUM_EXEC_PROCS = c(1, 1, 3),
    AVAIL_CPU_TIME_SEC = c(800, 1000, 1200),
    WASTED_CPU_SECONDS = c(600, 500, 700),
    MEM_REQUESTED_MB = c(1200, 2400, 3600), 
    MEM_REQUESTED_MB_SEC = c(12000, 42000, 60000),
    WASTED_MB_SECONDS = c(5000, 20000, 10000),
    JOB_NAME = c('nf-hello', 'wrp_job', 'another_job')
  )

  expected_columns <- c(
    'job_type', 'number_of_jobs', 'fail_rate', 'cpu_avail_hrs', 'cpu_wasted_hrs', 
    'cpu_wasted_frac','mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost'
  )

  # when time_bucket == 'none'
  dt <- generate_job_statistics(fake_data_frame)
  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)

  # when time_bucket != 'none'
  fake_data_frame$`_id` <- c('123', '456', '789')
  fake_data_frame$timestamp <- as.Date(c('2024-01-02', '2024-01-03', '2024-01-03'))
  expected_columns <- append(expected_columns, 'date', after = 1)

  dt <- generate_job_statistics(fake_data_frame, time_bucket = 'day')
  expect_s3_class(dt, 'tbl_ts')
  expect_named(dt, expected_columns)
})

test_that("parse_job_type works", {
  expect_equal(parse_job_type("nf-NFCORE-HELLOWORLD"), "nextflow")
  expect_equal(parse_job_type("wrp_c842dac"), "wr")
  expect_equal(parse_job_type("bsub rstudio user ip13"), "interactive")
  expect_equal(parse_job_type("cromwell_ffe311a3_auto_ccs_outputs_barcoded"), "cromwell")
  expect_equal(parse_job_type("jupyter"), "interactive")
  expect_equal(parse_job_type(""), "other")
})

test_that("generate_gpu_statistics works", {
  fake_df <- data.frame(
    USER_NAME = c('user1', 'user2', 'user2'),
    Job = c('Success', 'Success', 'Failed'),
    QUEUE_NAME = c('queue2', 'queue1', 'queue1'),
    RUN_TIME_SEC = c(30, 60, 100),
    PENDING_TIME_SEC = c(1, 2, 10)
  )

  expected_df <- tibble::tibble(
    USER_NAME = c('user1', 'user2'),
    QUEUE_NAME = c('queue2', 'queue1'),
    number_of_jobs = c(1, 2),
    fail_rate = c(0, 0.5),
    median_wait_time = c(1, median(c(2, 10))),
    median_run_time = c(30, median(c(60, 100)))
  )
  expected_columns = colnames(expected_df)

  result <- generate_gpu_statistics(fake_df)

  expect_s3_class(result, 'data.frame')
  expect_named(result, expected_columns)
  expect_equal(result, expected_df)
})

test_that("build_bucket_aggregation_query works", {
  # with no time bucket
  result <- build_bucket_aggregation_query(fields = c('x', 'y'), query = NULL, time_bucket = 'none')

  expect_failure(expect_null(result$aggs$stats$aggs))
  expect_null(result$aggs$stats$aggs$stats2)
  expect_setequal(result$aggs$stats$multi_terms$terms, list(list(field = 'x'), list(field = 'y')))

  # with given time bucket
  result <- build_bucket_aggregation_query(fields = c('x', 'y'), query = NULL, time_bucket = 'day')

  expect_failure(expect_null(result$aggs$stats$aggs$stats2))
  expect_equal(result$aggs$stats$date_histogram$calendar_interval, 'day')
})

test_that("generate_app_wastage_statistics function produces correct app wastage statistics", {
  df <- data.frame(
    doc_count = c(1, 12, 2, 3, 7),
    job_status = c('Success', 'Success', 'Failed', 'Success', 'Failed'),
    mem_avail_mb_sec = c(36864, 36864, 73728, 2048, 2560),
    mem_wasted_mb_sec = c(1024000, 512, 768, 1024000, 1280),
    cpu_avail_sec = c(3600, 7200, 10800, 4000, 5000),
    cpu_wasted_sec = c(600, 1000, 1500, 2000, 2500),
    procs = c(1, 2, 1, 1, 1),
    wasted_cost = c(10, 20, 30, 40, 50)
  )

  failing_jobs <- c(3, 5)
  success_jobs <- c(1, 2, 4)
  wasting_jobs <- c(2)  # success jobs with procs > 1
  
  expected_result <- tibble::tibble(
    job_status = c('Failed', 'Success'),
    number_of_jobs = c(
      sum(df$doc_count[failing_jobs]),
      sum(df$doc_count[success_jobs])
    ),
    fail_rate = c(1, 0),
    cpu_avail_hrs = c(
      sum(df$cpu_avail_sec[failing_jobs]), 
      sum(df$cpu_avail_sec[success_jobs])
    ) / 3600,
    cpu_wasted_hrs = c(
      sum(df$cpu_wasted_sec[failing_jobs]), 
      sum(df$cpu_wasted_sec[wasting_jobs])
    ) / 3600,
    mem_avail_gb_hrs = c(
      sum(df$mem_avail_mb_sec[failing_jobs]), 
      sum(df$mem_avail_mb_sec[success_jobs])
    ) / 3600 / 1024,
    mem_wasted_gb_hrs =  c(
      sum(df$mem_wasted_mb_sec[failing_jobs]), 
      sum(df$mem_wasted_mb_sec[success_jobs])
    ) / 3600 / 1024,
    wasted_cost = c(
      sum(df$wasted_cost[failing_jobs]), 
      sum(df$wasted_cost[wasting_jobs]) + sum(df$mem_wasted_mb_sec[setdiff(success_jobs, wasting_jobs)]) * ram_mb_second
    )
  )

  expected_result$cpu_wasted_frac <- expected_result$cpu_wasted_hrs / expected_result$cpu_avail_hrs
  expected_result$mem_wasted_frac <- expected_result$mem_wasted_gb_hrs / expected_result$mem_avail_gb_hrs

  expected_result <- expected_result[c(1, 2, 3, 4, 5, 9, 6, 7, 10, 8)]

  # no timestamp
  result <- generate_app_wastage_statistics(df)
  expect_equal(result, expected_result)

  df$timestamp <- as.Date(c('2024-01-01', '2024-01-01', '2024-01-02', '2024-01-01', '2024-01-02'))
  expected_result$timestamp <- as.Date(c('2024-01-02', '2024-01-01'))
  expected_result <- dplyr::relocate(expected_result, timestamp, .before = 1)

  # with timestamp
  result <- generate_app_wastage_statistics(df, timed = TRUE)
  expect_equal(result, expected_result)
})

test_that("assign_jupyter_job_names works", {

  df <- data.frame(
    '_id' = c(1, 3, 2, 4),
    'JOB_NAME' = c('job1', NA, NA, NA),
    check.names = FALSE
  )

  ids <- c(2, 4)

  dt <- assign_jupyter_job_names(df, ids)
  expected_job_names <- c('job1', NA, 'jupyter', 'jupyter')

  expect_s3_class(dt,'data.frame')
  expect_named(dt, names(df))
  expect_equal(dt$JOB_NAME, expected_job_names)
})

test_that("decide_statistics_function works", {
  expect_identical(decide_statistics_function('all', 'all'), get_bom_statistics)
  expect_identical(decide_statistics_function('user1', 'all'), get_user_statistics)
  expect_identical(decide_statistics_function('all', 'team1'), get_team_statistics)
  expect_identical(decide_statistics_function('user1', 'team1'), get_user_statistics)
})
