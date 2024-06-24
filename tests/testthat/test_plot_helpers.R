library(testthat)

context("Test helper functions to work with elastic")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/plot_helpers.R")

gpu_plot_df <- data.frame(
    'USER_NAME' = c('x', 'y', 'z'),
    'mymetric' = c(1, 2, 3),
    'JOB_ID' = c(123, 456, 789),
    'timestamp' = as.Date(c('2024-01-01', '2024-01-02', '2024-01-03'))
  )

test_that("generate_gpu_plot works", {
  metric = 'mymetric'
  metric_agg <- paste0(metric, "_median")

  gpu_plot <- generate_gpu_plot(gpu_plot_df, time_bucket = 'day', metric)

  # labels   
  expect_equal(gpu_plot$labels$x, 'date')
  expect_equal(gpu_plot$labels$y, metric_agg)
  expect_equal(gpu_plot$labels$fill, 'USER_NAME')

  # plot
  expect_equal(as.character(gpu_plot[["layers"]][[1]][["constructor"]][[1]]), 'geom_bar')

  # data 
  expect_is(gpu_plot$data[[metric_agg]], 'numeric')
})

test_that("assert_colnames works", {
  # valid colnames
  valid_colnames <- c('USER_NAME', 'mymetric')
  expect_no_error(assert_colnames(gpu_plot_df, valid_colnames))

  # invalid colnames
  invalid_colnames <- append(valid_colnames, 'wasted_cost')
  expect_error(assert_colnames(gpu_plot_df, invalid_colnames))
})