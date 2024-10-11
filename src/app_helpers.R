library(dplyr)
loadNamespace('httr')

source('src/elastic_helpers.R')

get_bom_names <- function(con, date_range) {
  b <- build_agg_query("BOM", query = build_humgen_query(
    filters = build_humgen_filters(
      BOM = NULL,
      date_range = date_range
    )
  ))

  res <- elastic_search(con, index = attr(con, 'index'), body = b, asdf = T)

  parse_elastic_agg(res, b)$BOM
}

get_accounting_names <- function(con, bom, date_range) {
  b <- build_agg_query("ACCOUNTING_NAME", query = build_humgen_query(
    filters = build_humgen_filters(
      BOM = bom,
      date_range = date_range
    )
  ))

  res <- elastic_search(con, index = attr(con, 'index'), body = b, asdf = T)

  parse_elastic_agg(res, b)$accounting_name
}

get_user_names <- function(con, bom, accounting_name, date_range) {
  if (accounting_name %in% c('all', '')) accounting_name <- NULL
  b <- list(
    query = build_humgen_query(
      filters = build_humgen_filters(
        BOM = bom,
        accounting_name = accounting_name,
        date_range = date_range
      )
    )
  )

  res <- httr::POST(
    url = con$make_url(),
    path = 'get_usernames',
    body = b,
    encode = "json"
  )

  httr::stop_for_status(res, task = paste("get list of users for LSF group", accounting_name))

  as.character(httr::content(res))
}
