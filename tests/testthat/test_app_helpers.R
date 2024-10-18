library(testthat)

context("Test functions to work with app")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/app_helpers.R")

test_that("get_height_of_facet_plot works", {
  expect_equal(get_height_of_facet_plot(n = 1), 400)
  expect_equal(get_height_of_facet_plot(n = 2), 400)
  expect_equal(get_height_of_facet_plot(n = 3), 400)
  expect_equal(get_height_of_facet_plot(n = 4), 800)
  expect_equal(get_height_of_facet_plot(n = 9), 1200)
})
