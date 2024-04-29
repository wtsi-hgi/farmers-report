library(testthat)

context("Test helper functions to work with tables")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/table_helpers.R")

test_that("rename_group_column function renames column appropriately", {
  df <- data.frame(accounting_name = c("A", "B", "C"))

  team_map <- data.frame(
    team_code = c("A", "C"),
    team_name = c("Team A", "Team C")
  )

  expected_result <- data.frame(accounting_name = c("Team A", "B", "Team C"))

  result <- rename_group_column(df, mapping = team_map)

  expect_equal(result, expected_result)
})

test_that("mutate_for_piechart function calculates variables correctly", {
  df <- data.frame(doc_count = c(10, 20, 30, 40))

  expected_result <- data.frame(doc_count = c(10, 20, 30, 40),
                                csum = c(100, 90, 70, 40),
                                pos = c(95, 80, 55, 20))

  result <- mutate_for_piechart(df)

  expect_equal(result, expected_result)
})

test_that("generate_ranks function generates ranks and awesomeness correctly", {
  df <- data.frame(
    accounting_name = c("A", "B", "C"),
    cpu_wasted_frac = c(0.1, 0.2, 0.3),
    mem_wasted_frac = c(0.2, 0.3, 0.4),
    cpu_avail_hrs = c(100, 150, 200),
    mem_avail_gb_hrs = c(50, 75, 100)
  )

  expected_result <- data.frame(
    accounting_name = c("A", "B", "C"),
    cpu_frac_rank = c(3, 2, 1),
    mem_frac_rank = c(3, 2, 1),
    rank = c(6, 4, 2),
    awesomeness = c(10, 20/3, 10/3)
  )

  result <- generate_ranks(df)

  expect_equal(result, expected_result)
})

test_that("generate_ranks function handles case when cpu_avail_hrs does not exceed cpu_threshold", {
  # Create a sample data frame for testing
  df <- data.frame(
    accounting_name = c("A", "B", "C"),
    cpu_wasted_frac = c(0.1, 0.2, 0.3),
    mem_wasted_frac = c(0.2, 0.3, 0.4),
    cpu_avail_hrs = c(200, 50, 400),  # One value below threshold
    mem_avail_gb_hrs = c(50, 75, 100)
  )

  expected_result <- data.frame(
    accounting_name = c("A", "B", "C"),
    cpu_frac_rank = c(3, 1, 1),
    mem_frac_rank = c(3, 2, 1),
    rank = c(6, 3, 2),
    awesomeness = c(10, 5, 10/3)
  )

  result <- generate_ranks(df)

  expect_equal(result, expected_result)
})

test_that("generate_efficiency_stats function produces correct efficiency statistics", {
  df <- data.frame(
    cpu_avail_sec = c(3600, 7200, 10800),
    cpu_wasted_sec = c(1800, 3600, 5400),
    mem_avail_mb_sec = c(36864, 36864, 73728),
    mem_wasted_mb_sec = c(9216, 9216, 18432),
    wasted_cost = c(10, 20, 30)
  )

  expected_result <- data.frame(
    wasted_cost = sum(df$wasted_cost),
    cpu_avail_hrs = 6,
    cpu_wasted_frac = 0.5,
    cpu_wasted_hrs = 3,
    mem_avail_gb_hrs = 0.04,
    mem_wasted_frac = 0.25,
    mem_wasted_gb_hrs = 0.01
  )

  result <- generate_efficiency_stats(df)

  expect_equal(result, expected_result)
})
