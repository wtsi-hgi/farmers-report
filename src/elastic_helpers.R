library(dplyr)

humgen_filters <- list(
  list(
    "match_phrase" = list(
      "BOM" = "Human Genetics"
    )
  ),
  list(
    "match_phrase" = list(
      "META_CLUSTER_NAME" = "farm"
    )
  ),
  list(
    "range" = list(
      "timestamp" = list(
        "lte" = "now/d",
        "gte" = "now-1w/d"
      )
    )
  )
)

humgen_query <- list(
  "bool" = list(
    "filter" = humgen_filters
  )
)

cpu_hour <- 0.00254  # in £
gb_ram_hour <- 0.000217  # in £

wasted_cost_agg <- list(
  "scripted_metric" = list(
    "init_script" = "state.costs = []",
    "map_script" = "double cpu_cost = doc.WASTED_CPU_SECONDS.value * params.cpu_second; double mem_cost = doc.WASTED_MB_SECONDS.value * params.mb_second; state.costs.add(Math.max(cpu_cost, mem_cost))",
    "combine_script" = "double total = 0; for (t in state.costs) { total += t } return total",
    "reduce_script" = "double total = 0; for (a in states) { total += a } return total",
    "params" = list("cpu_second" = cpu_hour / 60 / 60,
                    "mb_second" = gb_ram_hour / 1024 / 60 / 60)
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

extract_hits_from_elastic_response <- function(x) {
  garbage_columns <- c('_index', '_type', '_id', '_score', 'sort')
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
