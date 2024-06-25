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
    password = "test",
    port = 1234,
    index = "test*"
  )
)

test_that("read_config throws an error if the config file does not exist", {
  config_file <- "non-existent-config.yaml"
  expect_error(read_config(config_file), "No config file found!")
})

test_that("read_config produces expected result", {
  withr::local_file(
    setNames(list(yaml::write_yaml(fake_config, test_config_name)), test_config_name)
  )
  config <- read_config(test_config_name)
  expect_equal(config, fake_config)
})

test_that("read_config throws an error if not all elastic credentials present", {
  fields <- c('host', 'username', 'password', 'port', 'index')
  lapply(fields, function (field) {
    clone_config <- fake_config
    clone_config$elastic[field] <- NULL
    withr::local_file(
      setNames(list(yaml::write_yaml(clone_config, test_config_name)), test_config_name)
    )
    expect_error(read_config(test_config_name))
  })
})

test_that("read_config throws an error if there is no elastic section in a config", {
  fake_config$elastic <- NULL
  withr::local_file(
    setNames(list(yaml::write_yaml(fake_config, test_config_name)), test_config_name)
  )
  expect_error(read_config(test_config_name))
})
