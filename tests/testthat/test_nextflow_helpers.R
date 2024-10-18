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
      c(
        "nf-MAIN_YASCP_process_(tag1)",
        "nf-MAIN_YASCP_process_2_(tag2)",
        "nf-MAIN_YASCP_(tag2)",
        "nf-MAIN_YASCP_process",
        "nf-MAIN_YASCP"
      ),
      "MAIN_YASCP"
    ),
    c("process", "process_2", "MAIN_YASCP", "process", "MAIN_YASCP")
  )
})

test_that("add_zero_length_space works", {
  s <- add_zero_length_space("one_two_three")
  expect_equal(s, "one_\u200Btwo_\u200Bthree")
})

test_that("generate_nextflow_step_freq works", {
  df <- data.frame(
    step = c('step1', 'step2', 'step2')
  )

  result <- generate_nextflow_step_freq(df)

  expected_result <- tibble::tibble(
    step = c('step2', 'step1'),
    n = c(2, 1)
  )

  expect_equal(result, expected_result)
})

test_that("generate_nextflow_cpu_efficiency works", {
  df <- data.frame(
    step = c('step1', 'step1', 'step2', 'step2'),
    procs = c(1, 2, 4, 4),
    Job_Efficiency = c(0.9, 0.8, 0.5, 0.6)
  )

  result <- generate_nextflow_cpu_efficiency(df)

  expected_result <- tibble::tibble(
    step = c('step1', 'step1', 'step2'),
    procs = c(1, 2, 4),
    N = c(1, 1, 2),
    best_eff = c(0.9, 0.8, 0.6)
  )

  expect_equal(result, expected_result)
})

test_that("generate_nextflow_mem_efficiency works", {
  df <- data.frame(
   step = c('step1', 'step2', 'step2'),
   procs = c(1, 2, 2),
   MEM_REQUESTED_MB = c(1024, 2048, 2048),
   MAX_MEM_USAGE_MB = c(614.4, 1024, 819.2),
   MAX_MEM_EFFICIENCY = c(0.6, 0.5, 0.4)
  )

  result <- generate_nextflow_mem_efficiency(df)

  expected_result <- tibble::tibble(
    step = c('step1', 'step2'),
    procs = c(1, 2),
    mem_avail_gb = c(1, 2),
    N = c(1, 2),
    best_eff = c(0.6, 0.5),
    max_mem_used_gb = c(0.6, 1)
  )

  expect_equal(result, expected_result)
})
