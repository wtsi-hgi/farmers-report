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
  fake_elastic_response <- list(
    aggregations = list(
      stats = list(
        buckets = tibble::tibble(
          key = list(c("1", "Success"), c("2", "Failed")),
          key_as_string = c('1|Success', '2|Failed'),
          doc_count = c(10, 20),
          mem_avail_mb_sec = c(500, 750),
          mem_wasted_mb_sec = c(1000, 2000),
          cpu_avail_sec = c (50, 75),
          cpu_wasted_sec = c(100, 200),
          wasted_cost = c(0.1, 0.2)
        )
      )
    )
  )

  expected_columns <- c(
    'Reason', 'cpu_avail_hrs', 'cpu_wasted_hrs', 'cpu_wasted_frac', 
    'mem_avail_gb_hrs', 'mem_wasted_gb_hrs', 'mem_wasted_frac', 'wasted_cost'
  )

  # with adjustment
  dt <- generate_user_statistics(fake_elastic_response)
  expect_s3_class(dt, 'data.frame')
  expect_named(dt, expected_columns)
  expect_true('Total' %in% dt$Reason)

  # without adjustment
  dt <- generate_user_statistics(fake_elastic_response, adjust = FALSE)
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
    WASTED_MB_SECONDS = c(5000, 20000, 10000)
  )

  expected_columns <- c(
    'USER_NAME', 'number_of_jobs', 'fail_rate','cpu_avail_hrs', 'cpu_wasted_hrs', 
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
