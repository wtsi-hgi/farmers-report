library(testthat)

Sys.setenv(NOT_CRAN = "true")

context("Test functions to generate statistics")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/stat_helpers.R")
source("src/constants.R")

test_that("do_bom_transformation works", {
  local_edition(3)

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

  dt <- do_bom_transformation(df)

  expect_s3_class(dt,'data.frame')
  expect_true('awesomeness' %in% names(dt))
  expect_snapshot_value(dt, style = 'deparse')
})
