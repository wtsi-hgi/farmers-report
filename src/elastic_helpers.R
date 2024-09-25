library(dplyr)

source("src/constants.R")
source('src/timeseries_helpers.R')
loadNamespace('tidyr')

rename_raw_elastic_fields <- function (df, map = elastic_column_map) {
  rename(df, any_of(map))
}

format_elastic_date_range <- function(date_range) {
  if (!isa(date_range, "Date")) {
    stop("Please provide the date as a Date object")
  }
  # adding a day to lte value to include the records from the last day
  date_range[2] <- date_range[2] + 1
  strftime(date_range, format = "%Y-%m-%dT%H:%M:%SZ")
}

build_humgen_filters <- function (
  BOM = "Human Genetics", accounting_name = NULL, user_name = NULL,
  custom_filters = NULL, date_range = c(Sys.Date()-7, Sys.Date())
) {
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
    bom_filter <- build_match_phrase_filter("BOM", BOM)
    filters <- append(filters, bom_filter)
  }

  if (!is.null(accounting_name)) {
    accounting_name_filter <- build_match_phrase_filter("ACCOUNTING_NAME", accounting_name)
    filters <- append(filters, accounting_name_filter)
  }

  if (!is.null(user_name)) {
    user_name_filter <- build_match_phrase_filter("USER_NAME", accounting_name)
    filters <- append(filters, user_name_filter)
  }

  if (!is.null(custom_filters))
    filters <- append(filters, custom_filters)

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

new_elastic_agg_query <- function(x, nests){
  stopifnot("x should be list" = (typeof(x) == 'list'))
  stopifnot("nests should be list" = typeof(nests) == 'list')
  stopifnot("all nests elements should be of type elastic_agg" = all(
    sapply(nests, function(x) inherits(x, 'elastic_agg'))
  ))

  structure(
    x, 
    class = c("list", "elastic_agg_query"), 
    nest_levels = length(nests),
    nests = nests
  )
}

new_elastic_agg <- function(x, type, fields = NULL){
  stopifnot("x should be list" = typeof(x) == 'list')
  match.arg(type, c('terms', 'multi_terms', 'date', 'compute'))

  structure(
    x,
    class = c('list', 'elastic_agg'),
    type = type,
    fields = fields
  )
}

build_terms_agg <- function(field, size = 1000) {
  b <- list(
    "terms" = list(
      "field" = field,
      "size" = size
    )
  )
  new_elastic_agg(b, type = 'terms', fields = field)
}

build_multi_terms_agg <- function(fields, size = 1000) {
  terms <- lapply(fields, function(field) {
    list("field" = field)
  })

  b <- list(
    "multi_terms" = list(
      "terms" = terms,
      "size" = size
    )
  )

  new_elastic_agg(b, type = 'multi_terms', fields = fields)
}

build_date_agg <- function(interval, field = 'timestamp') {
  validate_time_bucket(interval)
  b <- list(
    "date_histogram" = list(
      "field" = field,
      "calendar_interval" = interval,
      "format" = "yyyy-MM-dd"
    )
  )
  new_elastic_agg(b, type = 'date', fields = field)
}

build_elasic_agg <- function(aggs, query = humgen_query, b = list(), level = length(aggs)) {
  stat_name <- paste0("stats", ifelse(level == 1, '', level))
  agg <- aggs[[level]]

  if (all(names(agg) %in% elastic_bucket_aggregations)) {

    content <- list(append(agg, b))
    names(content) <- stat_name

  } else {
    content <- append(agg, b)
  }

  b <- list("aggs" = content)

  if (level == 1) {

    result <- append(
      b,
      list(size = 0, query = query)
    )

  } else {
    result <- build_elasic_agg(aggs, query = query, b = b, level = level - 1)
  }

  return(
    new_elastic_agg_query(result, nests = aggs)
  )
}

# creates elastic query body object to aggregate over single field
build_agg_query <- function(field, query = humgen_query) {
  b <- list(
    "aggs" = list(
      "stats" = build_terms_agg(field)
    ),
    "size" = 0,
    "query" = query
  )
  return(
    new_elastic_agg_query(b, nests = list(new_elastic_agg(list(), type = 'terms', fields = field)))
  )
}

# creates elastic query body object to aggregate over multiple fields
build_terms_query <- function(fields, aggs = NULL, query = humgen_query, time_bucket = 'none') {
  b <- list(
    "aggs" = list(
      "stats" = append(
        build_multi_terms_agg(fields),
        list("aggs" = aggs)
      )
    ),
    "size" = 0,
    "query" = query
  )

  nests = list(new_elastic_agg(list(), type = 'multi_terms', fields = fields))
  if(!is.null(aggs)) nests <- append(nests, list(new_elastic_agg(list(), type = 'compute')))

  new_elastic_agg_query(b, nests = nests)
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
    list(
      "match_phrase" = as.list(setNames(value, field))
    )
  )
}

extract_hits_from_elastic_response <- function(x) {
  garbage_columns <- c('_index', '_type', '_score', 'sort')
  if (length(x$hits$hits) == 0) 
    return(data.frame())
  x$hits$hits %>%
    select(-any_of(garbage_columns)) %>%
    rename_with(~ gsub("^_source\\.", "", .x)) %>%
    as.data.frame()
}

pull_everything <- function(connection, response) {
  dt <- extract_hits_from_elastic_response(response)
  total_hits <- response$hits$total$value

  while(nrow(dt) < total_hits){
    response <- scroll(connection, response$`_scroll_id`, asdf = T)
    df <- extract_hits_from_elastic_response(response)
    dt <- rbind(dt, df)
  }

  scroll_clear(connection, response$`_scroll_id`)

  return(dt)
}

parse_elastic_agg <- function(response, request, df = data.frame(), nest_level = 1) {
  stopifnot(inherits(request, 'elastic_agg_query'))

  levels <- attr(request, 'nest_levels')
  nests <- attr(request, 'nests')

  nest <- nests[[nest_level]]
  nest_type <- attr(nest, 'type')
  nest_field <- attr(nest, 'fields')

  if(nest_level == 1){
    df <- response$aggregations$stats$buckets 
  } else {
    if (nest_type %in% elastic_bucket_aggregations) {
      stat_name <- paste0('stats', nest_level, ".buckets") 
      df <- df %>%
        select(-doc_count) %>%
        select(-`stats2.doc_count_error_upper_bound`, -`stats2.sum_other_doc_count`) %>%
        tidyr::unnest(cols = all_of(stat_name))
    }
  }

  if(nest_type == 'terms') {

    df <- arrange(df, key) %>% rename(!!nest_field := key)

  } else if(nest_type == 'multi_terms') {

    rule <- setNames(seq_along(nest_field), nest_field)
    df <- df %>%
      select(-key_as_string) %>%
      tidyr::hoist(.col = key, !!!rule)

  } else if(nest_type == 'compute') {

    df <- df %>%
      rename_all(~gsub('.value', '', .))

  } else if (nest_type == 'date') {

    df <- df %>%
      select(-key_as_string) %>%
      mutate(key = lubridate::as_datetime(key / 1e3)) %>% 
      rename(!!nest_field := key)

  } else {

    stop("Parser for ", nest_type, " is not implemented. Use terms/multi_terms/compute/date")

  }
  
  if (nest_level < levels){

    df <- parse_elastic_agg(response = NULL, request = request, df = df, nest_level = nest_level + 1)

  } 

  df <- rename_raw_elastic_fields(df)
  return(df)
}

get_numerical_colnames <- function(df) {
  numerical_colnames <- df %>%
    select(where(is.numeric)) %>%
    colnames()
}

scroll_elastic <- function(con, body, fields) {
  res <- Search(
    con,
    index = attr(con, 'index'),
    time_scroll="1m",
    source = fields,
    body = body,
    asdf = T,
    size = 10000
  )
  df <- pull_everything(con, res)

  if(nrow(df) == 0)
    df <- mutate(df, !!!sapply(fields, c))

  numerical_columns <- get_numerical_colnames(df)

  df <- df %>% 
    mutate(across(all_of(numerical_columns), ~ tidyr::replace_na(., 0)))

  return(df)
}
