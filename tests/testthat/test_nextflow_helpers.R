library(testthat)

context("Test functions to work with nextflow jobs")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/nextflow_helpers.R")

test_that("get_nf_job_names_parts works", {
  df <- data.frame(
    JOB_NAME = c(
        "nf-b",
        "nf-a",
        "nf-c",
        "nf-a_1",
        "nf-a_1_(tag)",
        "nf-a_1_2",
        "nf-a_1_2_3"
    )
  )  
  result <- get_nf_job_names_parts(df)
  expected_result <- c(
    "a", "a_1", "a_1_2",
    "b", "c"
  )
  expect_equal(result, expected_result)
})

test_that("parse_nextflow_step works", {
  expect_equal(
    parse_nextflow_step(
      c("nf-MAIN_YASCP_sillyprocess_(rnatag1)", "nf-MAIN_YASCP_sillyprocess_2_(rnatag2)", "nf-MAIN_YASCP_(rnatag2)", "nf-MAIN_YASCP_process", "nf-MAIN_YASCP"),
      "MAIN_YASCP"
    ),
    c("sillyprocess", "sillyprocess_2", "MAIN_YASCP", "process", "MAIN_YASCP")
  )
})
