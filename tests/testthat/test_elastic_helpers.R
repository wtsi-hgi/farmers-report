library(testthat)

context("Test helper functions to work with elastic")

if(basename(getwd()) == "testthat")
  setwd("../../")

source("src/elastic_helpers.R")

test_that("new_elastic_agg works", {
  # invalid x
  expect_error(new_elastic_agg(x = character(), type = 'date', fields = NULL))

  # invalid type
  expect_error(new_elastic_agg(x = list(), type = 'year', fields = NULL))

  # valid
  elastic_agg <- new_elastic_agg(x = list(), type = 'compute', fields = c('x'))

  expect_s3_class(elastic_agg, 'elastic_agg')
  expect_contains(names(attributes(elastic_agg)), c('type', 'fields'))
})

test_that("new_elastic_agg_query works", {
  # invalid x
  expect_error(new_elastic_agg_query(x = character(), nests = NULL), "x should be list")

  # invalid nests
  fake_nests <- list('x', 'y')
  expect_error(new_elastic_agg_query(x = list(), nests = fake_nests), "should be of type")

  # valid
  valid_nests <- list(new_elastic_agg(list(), type = 'terms', fields = NULL))
  elastic_agg_query <- new_elastic_agg_query(x = list(), nests = valid_nests)

  expect_s3_class(elastic_agg_query, 'elastic_agg_query')
  expect_contains(names(attributes(elastic_agg_query)), c('nest_levels', 'nests'))
})

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

fake_elastic_single_agg_request <- new_elastic_agg_query(
  list(
    "aggs" = list(
        "stats" = list(
          "terms" = list(
            "field" = "Job",
            "size" = 1000
          )
        )
      ),
      "size" = 0
  ),
  nests = list(new_elastic_agg(list(), type = 'terms', fields = "Job"))
)

fake_elastic_single_agg_response <- list(
  aggregations = list(stats = list(buckets = data.frame(key = 1:3)))
)

fake_elastic_nested_multi_agg_request <- new_elastic_agg_query(
  list(
    "aggs" = list(
        "stats" = list(
          "multi_terms" = list(
            "terms" = list(
              list(field = "Job"),
              list(field = "ACCOUNTING_NAME")
            ),
            "size" = 1000
          ),
          "aggs" = list(
            "cpu_avail_sec" = list(
              "sum" = list(
                "field" = "AVAIL_CPU_TIME_SEC"
              )
            )
          )
        )
      ),
      "size" = 0
  ),
  nests = list(
    new_elastic_agg(list(), type = 'multi_terms', fields = c("Job", "ACCOUNTING_NAME")),
    new_elastic_agg(list(), type = 'compute')
  )
)

fake_elastic_nested_multi_agg_response <- list(
  aggregations = list(stats = list(buckets = tibble::tibble(
    key = list(
      c('value1', 'value2'),
      c('value3', 'value4'),
      c('value5', 'value6')),
    key_as_string = character(3),
    cpu_avail_sec = c(1, 2, 3)
  )))
)

fake_elastic_nested_agg_request <- new_elastic_agg_query(
  list(
    "aggs" = list(
        "stats" = list(
          "terms" = list(
            "field" = "BOM",
            "size" = 1000
          ),
          "aggs" = list(
            "stats2" = list(
              "multi_terms" = list(
                "terms" = list(
                  list(field = "Job"),
                  list(field = "ACCOUNTING_NAME")
                ),
                "size" = 1000
              )
            )
          )
        )
      ),
      "size" = 0
  ),
  nests = list(
    new_elastic_agg(list(), type = 'terms', fields = "BOM"),
    new_elastic_agg(list(), type = 'multi_terms', fields = c("Job", "ACCOUNTING_NAME"))
  )
)

fake_elastic_nested_agg_response <- list(
  aggregations = list(stats = list(buckets = tibble::tibble(
    key = c('Humgen', 'CellGen'),
    doc_count = c(100, 200),
    stats2.doc_count_error_upper_bound = c(0, 0),
    stats2.sum_other_doc_count = c(0, 0),
    stats2.buckets = list(
      tibble::tribble(~key, ~key_as_string, ~doc_count, 
                      c('value1', 'value2'), '', c(300, 400), 
                      c('value3', 'value4'), '', c(500, 600)),
      tibble::tribble(~key, ~key_as_string, ~doc_count, 
                      c('value5', 'value6'), '', c(700, 800))
    )
  )))
)

fake_elastic_multi_agg_request <- new_elastic_agg_query(
  list(
    "aggs" = list(
      "stats" = list(
        "multi_terms" = list(
          "terms" = terms,
          "size" = 1000
        )
      )
    )
  ), 
  nests = list(new_elastic_agg(list(), type = 'multi_terms', fields = c("field1", "field2")))
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

fake_elastic_date_request <- new_elastic_agg_query(
  list(
    "aggs" = list(
      "stats" = list(
        "date_histogram" = list(
          "field" = "timestamp",
          "calendar_interval" = "day"
        )
      )
    )
  ),
  nests = list(new_elastic_agg(list(), type = "date", fields = "timestamp"))
)

fake_elastic_date_response <- list(
  aggregations = list(stats = list(buckets = tibble::tibble(
    key = c(1.718237e+12, 1.718323e+12),
    key_as_string = c('2024-06-13', '2024-06-14'),
    doc_count = c(100, 200)
  )))
)

fake_elastic_nested_date_request <- new_elastic_agg_query(
  list(
    "aggs" = list(
      "stats" = list(
        "date_histogram" = list(
          "field" = "timestamp",
          "calendar_interval" = "day"
        ),
        "aggs" = list(
          "stats2" = list(
            "multi_terms" = list(
              "terms" = terms
            )
          )
        )
      )
    )
  ),
  nests = list(
    new_elastic_agg(list(), type = "date", fields = "timestamp"),
    new_elastic_agg(list(), type = 'multi_terms', fields = c('BOM', 'Job'))
  )
)

fake_elastic_nested_date_response <- list(
  aggregations = list(stats = list(buckets = tibble::tibble(
    key = c(1.718237e+12, 1.718323e+12),
    key_as_string = c('2024-06-13', '2024-06-14'),
    doc_count = c(100, 200),
    stats2.doc_count_error_upper_bound = c(0, 0),
    stats2.sum_other_doc_count = c(0, 0),
    stats2.buckets = list(
      tibble::tribble(~key, ~key_as_string, ~doc_count, 
                      c('value1', 'value2'), '', c(300, 400), 
                      c('value3', 'value4'), '', c(500, 600)),
      tibble::tribble(~key, ~key_as_string, ~doc_count, 
                      c('value5', 'value6'), '', c(700, 800))
    )
  )))
)

test_that("extract_hits_from_elastic_response works", {
  # with records
  hits <- extract_hits_from_elastic_response(fake_elastic_response)

  expect_s3_class(hits,'data.frame')
  expect_length(names(hits), 2)
  expect_named(hits, c("_id", "name"))

  # with empty response
  fake_elastic_response$hits$hits <- data.frame()
  hits <- extract_hits_from_elastic_response(fake_elastic_response)
  expect_equal(hits, data.frame())
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
  # without custom aggs
  field_names <- c('fieldname1', 'fieldname2')
  q <- build_terms_query(field = field_names)

  expect_type(q, "list")
  expect_s3_class(q, 'elastic_agg_query')
  expect_named(q$aggs$stats[1], 'multi_terms')
  expect_setequal(q$aggs$stats$multi_terms$terms, list(list(field='fieldname1'), list(field='fieldname2')))
  expect_null(q$aggs$stats$aggs)
  expect_equal(q$query, humgen_query)
  expect_length(attr(q, 'nests'), 1)

  # with custom aggs
  custom_aggs <- list("mysum" = list("sum" = list("field" = "fieldname3")))
  q <- build_terms_query(field = field_names, aggs = custom_aggs)

  expect_failure(expect_null(q$aggs$stats$aggs))
  expect_length(attr(q, 'nests'), 2)
})

test_that("build_date_query works", {
  # time bucket is day
  q <- build_date_query(interval = 'day', fields = c("ACCOUNTING_NAME", "Job"))

  expect_type(q, "list")
  expect_s3_class(q, 'elastic_agg_query')
  expect_named(q$aggs$stats[1], 'date_histogram')
  expect_named(q$aggs$stats$aggs$stats2[1], 'multi_terms')
  expect_setequal(q$aggs$stats$aggs$stats2$multi_terms$terms, list(list(field='ACCOUNTING_NAME'), list(field='Job')))
  expect_equal(q$query, humgen_query)
  expect_length(attr(q, 'nests'), 2)
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

test_that('parse_elastic_agg works', {
  # nest level 1, test for 'terms'
  df <- parse_elastic_agg(fake_elastic_single_agg_response, fake_elastic_single_agg_request)

  expect_s3_class(df, 'data.frame')
  expect_named(df, "job_status")

  # nest level 1, test for 'multi_terms'
  field_names <- c('field1', 'field2')
  df <- parse_elastic_agg(fake_elastic_multi_agg_response, fake_elastic_multi_agg_request)

  expect_s3_class(df, 'data.frame')
  expect_named(df, field_names)
  expect_equal(nrow(df), 3)

  # nest level 2, test for 'multi_terms' and 'compute'
  field_names <- c('job_status', 'accounting_name', 'cpu_avail_sec')
  df <- parse_elastic_agg(fake_elastic_nested_multi_agg_response, fake_elastic_nested_multi_agg_request)

  expect_s3_class(df, 'data.frame')
  expect_named(df, field_names)
  expect_equal(nrow(df), 3)

  # nest level 2, test for 'terms' and 'multi_terms'
  field_names <- c('BOM', 'job_status', 'accounting_name', 'doc_count')
  df <- parse_elastic_agg(fake_elastic_nested_agg_response, fake_elastic_nested_agg_request)

  expect_s3_class(df, 'data.frame')
  expect_named(df, field_names)
  expect_equal(nrow(df), 3)

  # nest level 1, test for 'date'
  df <- parse_elastic_agg(fake_elastic_date_response, fake_elastic_date_request)

  expect_s3_class(df, 'data.frame')
  expect_named(df, c('timestamp', 'doc_count'))
  expect_equal(nrow(df), 2)

  # nest level 2, test for 'date' and 'multi_terms'
  df <- parse_elastic_agg(fake_elastic_nested_date_response, fake_elastic_nested_date_request)

  expect_s3_class(df, 'data.frame')
  expect_named(df, c('timestamp', 'BOM', 'job_status', 'doc_count'))
  expect_equal(nrow(df), 3)

  # invalid
  attr(attr(fake_elastic_single_agg_request, 'nests')[[1]], 'type') <- 'invalid_type'
  expect_error(
    parse_elastic_agg(fake_elastic_single_agg_response, fake_elastic_single_agg_request), 
    "Parser for invalid_type is not implemented."
  )
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

test_that("rename_raw_elastic_fields works", {
  df <- as.data.frame(as.list(seq_along(elastic_column_map)))
  names(df) <- elastic_column_map

  result <- rename_raw_elastic_fields(df)

  expect_s3_class(result,'data.frame')
  expect_named(result, names(elastic_column_map))
})
