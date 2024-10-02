library(testthat)

context("Test logging functions")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/logging.R")

test_that("log action with empty accordions", {
    accordions <- c()
    bom <- "Human Genetics"
    group <- "all"
    user <- "all"
    period <- c("2024-01-01", "2024-01-02")
    time_bucket <- "none"

    log_output <- capture.output(
        log_action(accordions, bom, group, user, period, time_bucket)
    )
    expect_match(log_output, ".*USER-ACTION.*")
    expect_true(grepl('{"accordions":[],"filters":{"bom":"Human Genetics","accounting_name":"all","user":"all","period":["2024-01-01","2024-01-02"],"time_bucket":"none"}}', log_output, fixed = TRUE))
})

test_that("log action with non-empty accordions", {
    accordions <- c("accordion1", "accordion2")
    bom <- "Human Genetics"
    group <- "all"
    user <- "all"
    period <- c("2024-01-01", "2024-01-02")
    time_bucket <- "none"
})

