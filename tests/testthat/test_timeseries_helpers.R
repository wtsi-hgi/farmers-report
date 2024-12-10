library(testthat)

context("Test helper functions to work with timeseries")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/timeseries_helpers.R")

test_that("validate_time_bucket works", {
  expect_no_error(validate_time_bucket('none'))
  expect_no_error(validate_time_bucket(sample(time_buckets, 1)))
  expect_error(validate_time_bucket('year'))
})

test_that("index_by_custom works", {
  df <- data.frame(timestamp = as.Date(c('2024-06-01', '2024-06-02')))
  df <- as_tsibble(df)

  dt <- index_by_custom(df, time_bucket = 'day')
  expect_s3_class(dt$date, 'Date')

  dt <- index_by_custom(df, time_bucket = 'week')
  expect_s3_class(dt$date, 'Date')

  dt <- index_by_custom(df, time_bucket = 'month')
  expect_s3_class(dt$date, 'yearmonth')
})

test_that("isInvalidDate works", {
  expect_false(isInvalidDate("2024-12-10"))
  expect_true(isInvalidDate(NA))
  expect_true(isInvalidDate(""))
})
