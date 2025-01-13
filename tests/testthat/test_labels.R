library(testthat)

context("Test helper functions to work with labels")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/labels.R")

test_that("rename_raw_elastic_fields works", {
  df <- as.data.frame(as.list(seq_along(elastic_column_map)))
  names(df) <- elastic_column_map

  result <- rename_raw_elastic_fields(df)

  expect_s3_class(result,'data.frame')
  expect_named(result, names(elastic_column_map))
})
