library(testthat)

context("Test functions to work with a config file")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/config.R")

test_config_name <- "test-config.yml"
fake_config <- list(
  elastic = list(
    host = "test",
    username = "test",
    password = "test"
  )
)

test_that("read_config throws an error if the config file does not exist", {
  config_file <- "non-existent-config.yaml"
  withr::local_file(
    setNames(list(file.create(config_file)), config_file),
    {
      expect_error(read_config(config_file))
    }
  )
})

test_that("read_config produces expected result", {
  withr::local_file(
    setNames(list(yaml::write_yaml(fake_config, test_config_name)), test_config_name)
  )
  config <- read_config(test_config_name)
  expect_equal(config, fake_config)
})

test_that("read_config throws an error if not all elastic credentials present", {
  fake_config$elastic$host <- NULL
  withr::local_file(
    setNames(list(yaml::write_yaml(fake_config, test_config_name)), test_config_name)
  )
  expect_error(read_config(test_config_name))
})

test_that("read_config throws an error if there is no elastic section in a config", {
  fake_config$elastic <- NULL
  withr::local_file(
    setNames(list(yaml::write_yaml(fake_config, test_config_name)), test_config_name)
  )
  expect_error(read_config(test_config_name))
})
