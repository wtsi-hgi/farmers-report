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
    expect_match(log_output, '{"accordions":[],"filters":{"bom":"Human Genetics","accounting_name":"all","user":"all","period":["2024-01-01","2024-01-02"],"time_bucket":"none"}}', fixed = TRUE)
})

test_that("log action with 1 accordion", {
    accordions <- c("unadjusted_efficiency_panel")
    bom <- "Human Genetics"
    group <- "all"
    user <- "all"
    period <- c("2024-01-01", "2024-01-02")
    time_bucket <- "none"
    log_output <- capture.output(
        log_action(accordions, bom, group, user, period, time_bucket)
    )
    expect_match(log_output, ".*USER-ACTION.*")
    expect_match(log_output, '{"accordions":["unadjusted_efficiency_panel"],"filters":{"bom":"Human Genetics","accounting_name":"all","user":"all","period":["2024-01-01","2024-01-02"],"time_bucket":"none"}}', fixed = TRUE)
})

test_that("log action with 2 accordions", {
    accordions <- c("unadjusted_efficiency_panel", "gpu_statistics_panel")
    bom <- "Human Genetics"
    group <- "all"
    user <- "all"
    period <- c("2024-01-01", "2024-01-02")
    time_bucket <- "none"
    log_output <- capture.output(
        log_action(accordions, bom, group, user, period, time_bucket)
    )
    expect_match(log_output, ".*USER-ACTION.*")
    expect_match(log_output, '{"accordions":["unadjusted_efficiency_panel","gpu_statistics_panel"],"filters":{"bom":"Human Genetics","accounting_name":"all","user":"all","period":["2024-01-01","2024-01-02"],"time_bucket":"none"}}', fixed = TRUE)
})


test_that("log request logs scroll request", {
    log_output <- capture.output(
        log_request(time_scroll = '1m')
    )
    expect_match(log_output, "REQUEST type scroll")
})

test_that("log request logs search request", {
    log_output <- capture.output(
        log_request(query = 'test')
    )
    expect_match(log_output, "REQUEST type search")
})
