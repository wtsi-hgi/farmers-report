library(testthat)

context("Test helper functions to work with tables")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/constants.R")
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
    cpu_avail_hrs = 6,
    cpu_wasted_hrs = 3,
    cpu_wasted_frac = 0.5,
    mem_avail_gb_hrs = 0.04,
    mem_wasted_gb_hrs = 0.01,
    mem_wasted_frac = 0.25,
    wasted_cost = sum(df$wasted_cost)
  )

  result <- generate_efficiency_stats(df)

  expect_equal(result, expected_result)
})

test_that("specify_wastage_reason works", {
  input_df <- data.frame(
    job_status = c("Success", "Failed", "total")
  )

  expected_df <- data.frame(
    Reason = c("Due to over-requesting", "Due to job failure", "total")
  )

  df <- specify_wastage_reason(input_df)

  expect_named(df, "Reason")
  expect_equal(df, expected_df)
})

test_that("generate_total_wastage_dt works", {
  input_dt <- data.frame(
    number_of_jobs = c(10, 20, 30),
    fail_rate = c(0, 1, 1),
    cpu_wasted_hrs = c(3, 4, 3),
    cpu_avail_hrs = c(12, 6, 2),
    mem_wasted_gb_hrs = c(12, 3, 5),
    mem_avail_gb_hrs = c(18, 25, 7),
    job_status = c("Success", "Failed", "Failed")
  )

  expected_df <- data.frame(
    number_of_jobs = sum(input_dt$number_of_jobs),
    fail_rate = sum(input_dt[input_dt$job_status == "Failed", "number_of_jobs"]) / sum(input_dt$number_of_jobs),
    cpu_wasted_hrs = sum(input_dt$cpu_wasted_hrs),
    cpu_avail_hrs = sum(input_dt$cpu_avail_hrs),
    mem_wasted_gb_hrs = sum(input_dt$mem_wasted_gb_hrs),
    mem_avail_gb_hrs = sum(input_dt$mem_avail_gb_hrs),
    job_status = c("Total"),
    cpu_wasted_frac = sum(input_dt$cpu_wasted_hrs) / sum(input_dt$cpu_avail_hrs),
    mem_wasted_frac = sum(input_dt$mem_wasted_gb_hrs) / sum(input_dt$mem_avail_gb_hrs)
  )

  df <- generate_total_wastage_dt(input_dt)

  expect_equal(df, expected_df)
})

test_that("generate_total_failure_dt works", {
  input_dt <- data.frame(
    Failed = c(10, 15, 20),
    Success = c(15, 20, 30)
  )

  expected_df <- data.frame(
    Failed = sum(input_dt$Failed),
    Success = sum(input_dt$Success),
    accounting_name = c("Total"),
    fail_rate = sum(input_dt$Failed) / sum(input_dt$Failed, input_dt$Success)
    
  )

  df <- generate_total_failure_dt(input_dt)

  expect_equal(df, expected_df)

})

test_that("set_team_names generates named lists for accounting names", {
  team_code <- c("A", "B", "C")
  team_name <- c("Team A", "B", "Team C")

  team_map <- data.frame(
    team_code = c("A", "C"),
    team_name = c("Team A", "Team C")
  )

  expected_output <- c("Team A" = "A", "B" = "B", "Team C" = "C")

  converted_team_names <- set_team_names(team_code, team_map)

  expect_equal(converted_team_names, expected_output)
  expect_named(converted_team_names, team_name)
})

test_that("generate_wasted_cost works", {
  df <- data.frame(
    cpu_wasted_sec = c(100, 200, 300),
    mem_wasted_mb_sec = c(1000, 2000, 3000)
  )

  expected_df <- df
  expected_df$cpu_wasted_cost <- df$cpu_wasted_sec * cpu_second
  expected_df$mem_wasted_cost <- df$mem_wasted_mb_sec * ram_mb_second
  expected_df$wasted_cost <- pmax(expected_df$cpu_wasted_cost, expected_df$mem_wasted_cost)

  result <- generate_wasted_cost(df)

  expect_equal(result, expected_df)
})

test_that("get_colname_options works", {
  test_df <- data.frame(
    USER_NAME = c('user1', 'user2', 'user3'),
    cpu_wasted_frac = c(1.0, 0.79, 0.3),
    number_of_jobs = c(1, 2, 3),
    timestamp = as.Date(c('2024-01-01', '2024-01-02', '2024-01-03'))
  )

  expected_output <- c(
    'Number of jobs' = 'number_of_jobs',
    'Wasted CPU fraction' = 'cpu_wasted_frac'
  )
  
  test_output <- get_colname_options(test_df, exclude_columns = c('USER_NAME', 'timestamp'))
  expect_equal(test_output, expected_output)
})
