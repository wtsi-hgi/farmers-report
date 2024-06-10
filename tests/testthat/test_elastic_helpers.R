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

fake_elastic_single_agg_response <- list(
  aggregations = list(stats = list(buckets = data.frame(key = 1:3)))
)

fake_elastic_multi_agg_response <- list(
  aggregations = list(stats = list(buckets = tibble::tibble(
    key = list(
      c('value1', 'value2'),
      c('value3', 'value4'),
      c('value5', 'value6')),
    key_as_string = character(3)
  )))
)

test_that("extract_hits_from_elastic_response works", {
  hits <- extract_hits_from_elastic_response(fake_elastic_response)

  expect_s3_class(hits,'data.frame')
  expect_length(names(hits), 1)
  expect_named(hits, "name")
})

test_that("build_agg_query works", {
  field_name <- 'fieldname'
  q <- build_agg_query(field = field_name)

  expect_type(q, "list")
  expect_named(q$aggs$stats, 'terms')
  expect_equal(q$aggs$stats$terms$field, field_name)
  expect_equal(q$query, humgen_query)
})

test_that("build_terms_query works", {
  field_names <- c('fieldname1', 'fieldname2')
  q <- build_terms_query(field = field_names)

  expect_type(q, "list")
  expect_named(q$aggs$stats[1], 'multi_terms')
  expect_equal(q$aggs$stats$multi_terms$terms, list(list(field='fieldname1'), list(field='fieldname2')))
  expect_equal(q$query, humgen_query)
})

test_that("build_elastic_sub_agg works", {
  sub_agg <- build_elastic_sub_agg("myfield", "sum")
  expected_struct <- list(
    "sum" = list(
      "field" = "myfield"
    )
  )
  expect_identical(sub_agg, expected_struct)
})

test_that('build_humgen_filters works', {
  # no parameters
  f1 <- build_humgen_filters()
  expect_length(f1, 3)
  expect_named(f1, NULL)

  # with custom filters
  f2 <- build_humgen_filters(
    custom_filters = list("match_phrase" = list("field" = 'value'))
  )
  expect_length(f2, 4)
  expect_named(f2, NULL)

  # with BOM parameter as NULL
  f3 <- build_humgen_filters(BOM = NULL)
  expect_length(f3, 2)
  expect_named(f3, NULL)
  
  # with BOM parameter with value
  f4 <- build_humgen_filters(BOM = "CASM")
  expect_length(f4, 3)
  expect_named(f4, NULL)
  expect_equal(f4[[3]], list(match_phrase = list(BOM = "CASM")))

  # with date_range parameter with value
  test_date_range <- c(Sys.Date()-7, Sys.Date())
  f5 <- build_humgen_filters(date_range = test_date_range)
  expect_length(f5, 3)
  expect_named(f5, NULL)
  expect_equal(f5[[2]]$range$timestamp$gte, format_elastic_date_range(test_date_range)[1])
  expect_equal(f5[[2]]$range$timestamp$lte, format_elastic_date_range(test_date_range)[2])
})

test_that('build_humgen_query works', {
  q <- build_humgen_query()

  expect_named(q, 'bool')
  expect_named(q$bool, 'filter')
  expect_length(q$bool$filter, 3)
  expect_named(q$bool$filter, NULL)
})

test_that('parse_elastic_single_agg works', {
  df <- parse_elastic_single_agg(fake_elastic_single_agg_response)

  expect_s3_class(df,'data.frame')
  expect_named(df, "key")
})

test_that('parse_elastic_multi_agg works', {
  field_names <- c('field1', 'field2')
  df <- parse_elastic_multi_agg(fake_elastic_multi_agg_response, column_names = field_names)

  expect_s3_class(df,'data.frame')
  expect_named(df, field_names)
  expect_equal(nrow(df), 3)
})

test_that('format_elastic_date_range works', {
  # with valid input
  test_date_range <- as.Date(c("2024-01-01", "2024-01-05"))

  formatted_date_range <- format_elastic_date_range(test_date_range)
  expected_data_range <- c("2024-01-01T00:00:00Z", "2024-01-06T00:00:00Z")

  expect_equal(formatted_date_range, expected_data_range)

  # with string date input
  expect_error(
    format_elastic_date_range(date_range = c("now-1w/d", "now/d")),
    "Please provide the date as a Date object"
  )
})

test_that("build_match_phrase_filter works", {
  object <- build_match_phrase_filter("BOM", "Human Genetics")
  expected_object <- list(
    "match_phrase" = list("BOM" = "Human Genetics")
  )

  expect_equal(object, expected_object)
})
