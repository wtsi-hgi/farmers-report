library(testthat)

context("Test helper functions to work with plots")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/plot_helpers.R")

gpu_plot_df <- data.frame(
  'USER_NAME' = c('x', 'y', 'z'),
  'mymetric' = c(1, 2, 3),
  '_id' = c(123, 456, 789),
  'timestamp' = as.Date(c('2024-01-01', '2024-01-02', '2024-01-03')),
  check.names = FALSE
)

test_that("generate_gpu_plot works", {
  metric <- 'mymetric'
  metric_agg <- paste0(metric, "_median")

  gpu_plot <- generate_gpu_plot(gpu_plot_df, time_bucket = 'day', metric)

  # labels   
  expect_equal(gpu_plot$labels$x, 'date')
  expect_equal(gpu_plot$labels$y, metric_agg)
  expect_equal(gpu_plot$labels$fill, 'USER_NAME')

  # plot
  expect_equal(as.character(gpu_plot$layers[[1]]$constructor[[1]]), 'geom_bar')

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

df <- data.frame(
    timestamp = as.Date(c('2024-01-01', '2024-01-01', '2024-01-02', '2024-01-02')),
    mymetric = c(100, 200, 300, 400),
    gpu_wasted_frac = c(0.1, 0.2, 0.3, 0.4),
    cpu_avail_hrs = c(1000, 2000, 3000, 4000),
    cpu_wasted_hrs = c(400, 200, 300, 2000)
  )

test_that("generate_efficiency_plot works with bom", {
  df$accounting_name <- c('team1', 'team2', 'team1', 'team2')

  # invalid input
  expect_error(generate_efficiency_plot(df, column_to_plot = 'gpu_wasted_frac'))

  # bom statistics barplot
  p <- generate_efficiency_plot(df, column_to_plot = 'mymetric')

  expect_equal(p$labels$x, 'timestamp')
  expect_equal(p$labels$y, 'mymetric')
  expect_equal(p$labels$fill, 'accounting_name')
  expect_equal(as.character(p$layers[[1]]$constructor[[1]]), 'geom_bar')

  # bom statistics lineplot
  p <- generate_efficiency_plot(df, column_to_plot = 'cpu_wasted_frac')

  expect_equal(p$labels$x, 'timestamp')
  expect_equal(p$labels$y, 'cpu_efficiency')
  expect_null(p$labels$colour)
  expect_equal(as.character(p$layers[[1]]$constructor[[1]]), 'geom_line')
  expect_equal(p$data$cpu_efficiency, c(2400/3000, 4700/7000))
})

test_that("generate_efficiency_plot works with team", {
  # team statistics barplot
  df$date <- as.Date(df$timestamp)
  df$timestamp <- NULL
  df$USER_NAME <- c('user1', 'user2', 'user3', 'user4')

  p <- generate_efficiency_plot(df, column_to_plot = 'mymetric')

  expect_equal(p$labels$x, 'date')
  expect_equal(p$labels$y, 'mymetric')
  expect_equal(p$labels$fill, 'USER_NAME')
  expect_equal(as.character(p$layers[[1]]$constructor[[1]]), 'geom_bar')

  # team statistics lineplot - 'fail_rate'
  df$fail_rate <- c(0.7, 0.5, 0.5, 0.6)
  df$number_of_jobs <- c(10, 20, 10, 50)

  p <- generate_efficiency_plot(df, column_to_plot = 'fail_rate')

  expect_equal(p$labels$x, 'date')
  expect_equal(p$labels$y, 'fail_rate')
  expect_null(p$labels$colour)
  expect_equal(as.character(p$layers[[1]]$constructor[[1]]), 'geom_line')
  expect_equal(p$data$fail_rate, c(17/30, 35/60))
})
  
test_that("generate_efficiency_plot works with user", {
  # user statistics barplot
  df$Reason <- c('reason1', 'reason2', 'reason1', 'reason2')

  p <- generate_efficiency_plot(df, column_to_plot = 'mymetric')

  expect_equal(p$labels$x, 'timestamp')
  expect_equal(p$labels$y, 'mymetric')
  expect_equal(p$labels$fill, 'Reason')
  expect_equal(as.character(p$layers[[1]]$constructor[[1]]), 'geom_bar')

  # user statistics lineplot
  p <- generate_efficiency_plot(df, column_to_plot = 'cpu_wasted_frac')

  expect_equal(p$labels$x, 'timestamp')
  expect_equal(p$labels$y, 'cpu_efficiency')
  expect_equal(p$labels$colour, 'Reason')
  expect_equal(as.character(p$layers[[1]]$constructor[[1]]), 'geom_line')
  expect_equal(p$data$cpu_efficiency, c(0.6, 0.9, 0.9, 0.5))
})
