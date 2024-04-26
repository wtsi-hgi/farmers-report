library(testthat)

context("Test helper functions to work with elastic")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/elastic_helpers.R")

fake_elastic_response <- list(
  hits = list(
    hits = data.frame(
      "_index" = character(1),
      "_type" = character(1),
      "_id" = character(1),
      "_score" = double(1),
      "_source.name" = "value",
      check.names = FALSE
    )
  )
)

test_that("extract_hits_from_elastic_response works", {
  hits <- extract_hits_from_elastic_response(fake_elastic_response)

  expect_s3_class(hits,'data.frame')
  expect_length(names(hits), 1)
  expect_equal(names(hits), "name")
})

test_that("build_agg_query works", {
  field_name <- 'fieldname'
  q <- build_agg_query(field = field_name)

  expect_type(q, "list")
  expect_equal(names(q$aggs$stats), 'terms')
  expect_equal(q$aggs$stats$terms$field, field_name)
  expect_equal(q$query, humgen_query)
})

test_that("build_terms_query works", {
  field_names <- c('fieldname1', 'fieldname2')
  q <- build_terms_query(field = field_names)

  expect_type(q, "list")
  expect_equal(names(q$aggs$stats)[1], 'multi_terms')
  expect_equal(q$aggs$stats$multi_terms$terms, list(list(field='fieldname1'), list(field='fieldname2')))
  expect_equal(q$query, humgen_query)
})
