library(dplyr)

source("src/constants.R")

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

new_elastic_agg_query <- function(x, nests){
  structure(
    x, 
    class = c("list", "elastic_agg_query"), 
    nest_levels = length(nests),
    nests = nests
  )
}

new_elastic_agg <- function(type, fields = NULL, ...){
  list(
    type = type,
    fields = fields,
    ...
  )
}

build_terms_agg <- function(field, size = 1000) {
  b <- list(
    "terms" = list(
      "field" = field,
      "size" = size
    )
  )
}

build_multi_terms_agg <- function(fields, size = 1000) {
  terms <- lapply(fields, function(field){
    list("field" = field)
  })

  b <- list(
    "multi_terms" = list(
      "terms" = terms,
      "size" = size
    )
  )
}

# build_elasic_agg(
#   aggs = list(
#     build_terms_agg('USER_NAME'),
#     build_multi_terms_agg('BOM', 'Job')
#   )
# )

build_elasic_agg <- function(aggs, query = humgen_query, b = list(), level = length(aggs)) {
  stat_name <- paste0("stats", ifelse(level == 1, '', level))
  agg <- aggs[[level]]

  content <- list(append(agg, b))
  names(content) <- stat_name
  a <- list(
    "aggs" = content
  )

  if (level == 1){

    b <- append(
      a, 
      list(size = 0, query = query)
    )

  } else {

    b <- build_elasic_agg(aggs, query = query, b = a, level = level - 1)

  }

  return(b)
}

build_elasic_agg(
  aggs = list(
    build_terms_agg('USER_NAME'),
    build_multi_terms_agg('BOM', 'Job')
  )
)

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
    new_elastic_agg_query(b, nests = list(new_elastic_agg('terms', fields = field)))
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

  nests = list(new_elastic_agg('multi_terms', fields = fields))
  if(!is.null(aggs)) nests <- append(nests, list(new_elastic_agg('compute')))

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

parse_elastic_agg <- function(response, request, df = data.frame(), nest_level = 1) {
  stopifnot(inherits(request, 'elastic_agg_query'))

  levels <- attr(request, 'nest_levels')
  nests <- attr(request, 'nests')

  nest = nests[[nest_level]]

  if(nest_level == 1){
    df <- response$aggregations$stats$buckets 
  } else {
    if (nest$type == 'multi_terms') {
      stat_name <- paste0('stats', nest_level) 
      df <- tidyr::unnest(df, cols = all_of(stat_name))
    }
  }

  if(nest$type == 'terms'){

    field <- nest$fields
    df <- arrange(df, key) %>% rename(!!field := key)

  } else if(nest$type == 'multi_terms') {

    rule <- setNames(seq_along(nest$fields), nest$fields)
    df <- df %>%
      select(-key_as_string) %>%
      tidyr::hoist(.col = key, !!!rule)

  } else if(nest$type == 'compute'){

    df <- df %>%
      rename_all(~gsub('.value', '', .))

  } else {

    stop("Not implemented level1 except for terms and multi_terms and compute")

  }
  
  if (nest_level < levels){

    df <- parse_elastic_agg(response = NULL, request = request, df = df, nest_level = nest_level + 1)

  } 

  df <- rename_raw_elastic_fields(df)
  return(df)
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

validate_time_bucket <- function(time_bucket) {
  match.arg(time_bucket, choices = c(time_buckets, "none"))
}
