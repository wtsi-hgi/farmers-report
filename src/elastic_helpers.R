library(dplyr)

source("src/constants.R")

format_elastic_date_range <- function(date_range) {
  # adding a day to lte value to include the records from the last day
  date_range[2] <- date_range[2] + 1
  strftime(date_range, format = "%Y-%m-%dT%H:%M:%SZ")
}

build_humgen_filters <- function (BOM = "Human Genetics", custom_filters = NULL, date_range = c(Sys.Date()-7, Sys.Date())) {
  date_range <- format_elastic_date_range(date_range)
  
  filters <- list(
    list(
      "match_phrase" = list(
        "META_CLUSTER_NAME" = "farm"
      )
    ),
    list(
      "range" = list(
        "timestamp" = list(
          "lte" = date_range[2],
          "gte" = date_range[1],
          "format" = "strict_date_optional_time"
        )
      )
    )
  )

  if (!is.null(BOM)) {
    bom_filter <- list(
      "match_phrase" = list(
        "BOM" = BOM
      )
    )
    filters <- c(filters, list(bom_filter))
  }

  if (!is.null(custom_filters))
    filters <- c(filters, list(custom_filters))

  return(filters)
}

humgen_filters <- build_humgen_filters()

build_humgen_query <- function (filters = humgen_filters) {
  list(
    "bool" = list(
      "filter" = filters
    )
  )
}

humgen_query <- build_humgen_query()

wasted_cost_agg <- list(
  "scripted_metric" = list(
    "init_script" = "state.costs = []",
    "map_script" = "double cpu_cost = doc.WASTED_CPU_SECONDS.value * params.cpu_second; double mem_cost = doc.WASTED_MB_SECONDS.value * params.mb_second; state.costs.add(Math.max(cpu_cost, mem_cost))",
    "combine_script" = "double total = 0; for (t in state.costs) { total += t } return total",
    "reduce_script" = "double total = 0; for (a in states) { total += a } return total",
    "params" = list("cpu_second" = cpu_second,
                    "mb_second" = ram_mb_second)
  )
)

# creates elastic query body object to aggregate over single field
build_agg_query <- function(field, query = humgen_query) {
  b <- list(
    "aggs" = list(
      "stats" = list(
        "terms" = list(
          "field" = field,
          "size" = 1000
        )
      )
    ),
    "size" = 0,
    "query" = query
  )
  return(b)
}

# creates elastic query body object to aggregate over multiple fields
build_terms_query <- function(fields, aggs = NULL, query = humgen_query) {
  terms <- lapply(fields, function(field){
    list("field" = field)
  })

  b <- list(
    "aggs" = list(
      "stats" = list(
        "multi_terms" = list(
          "terms" = terms,
          "size" = 1000
        ),
        "aggs" = aggs
      )
    ),
    "size" = 0,
    "query" = query
  )
}

build_elastic_sub_agg <- function (field, agg_fun) {
  setNames(
    list(
      list("field" = field)
    ),
    agg_fun
  )
}

build_match_phrase_filter <- function (field, value) {
  list(
    "match_phrase" = as.list(setNames(value, field))
  )
}

extract_hits_from_elastic_response <- function(x) {
  garbage_columns <- c('_index', '_type', '_id', '_score', 'sort')
  if (length(x$hits$hits) == 0) 
    return(data.frame())
  x$hits$hits %>%
    select(-any_of(garbage_columns)) %>%
    rename_with(~ gsub("^_source\\.", "", .x)) %>%
    as.data.frame()
}

pull_everything <- function(connection, response) {
  dt <- extract_hits_from_elastic_response(response)
  hits <- 1

  while(hits != 0){
    res <- scroll(connection, response$`_scroll_id`, asdf = T)
    hits <- length(res$hits$hits)
    if(hits > 0){
      df <- extract_hits_from_elastic_response(res)
      dt <- rbind(dt, df)
    }
  }

  scroll_clear(connection, response$`_scroll_id`)

  return(dt)
}

# returns data.frame from the elastic response object for single-field aggregation
parse_elastic_single_agg <- function (response) {
  response$aggregations$stats$buckets %>%
    arrange(key)
}

# returns data.frame from the elastic response object for multi-fields aggregation
# column_names is a vector of column names corresponding to the aggregation fields
parse_elastic_multi_agg <- function (response, column_names) {
  rule <- setNames(seq_along(column_names), column_names)
  response$aggregations$stats$buckets %>%
    select(-key_as_string) %>%
    tidyr::hoist(.col = key, !!!rule) %>%
    rename_all(~gsub('.value', '', .))
}

scroll_elastic <- function (con, body, fields) {
  res <- Search(
    con,
    index = index,
    time_scroll="1m",
    source = fields,
    body = body,
    asdf = T,
    size = 10000
  )
  df <- pull_everything(con, res)

  if(nrow(df) == 0)
    df <- mutate(df, !!!sapply(fields, c))

  return(df)
}
