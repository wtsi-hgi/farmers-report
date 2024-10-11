library(dplyr)

source("src/elastic_helpers.R")

get_nf_job_names_parts <- function(df) {
  names <- df$JOB_NAME
  splitted <- stringr::str_split_fixed(names, "_", 4)
  combined_names <- c(
    splitted[, 1],
    paste(splitted[, 1], splitted[, 2], sep = "_"),
    paste(splitted[, 1], splitted[, 2], splitted[, 3], sep = "_")
  )
  names <- unique(combined_names)
  names <- gsub("^nf-", "", names)
  names <- gsub("_+$", "", names)
  names <- names[!grepl("\\(", names)]
  sort(unique(names))
}

get_nf_records <- function(con, query) {
  get_records(
    con = con,
    query = query,
    prefix = "nf",
    fields = c('JOB_NAME')
  )
}

get_pipeline_records <- function (con, query, pipeline_name) {
  get_records(
    con = con,
    query = query,
    prefix = paste("nf", pipeline_name, sep = "-"),
    fields = c('JOB_NAME', 'MAX_MEM_EFFICIENCY_PERCENT', 'Job_Efficiency_Percent',
               'MEM_REQUESTED_MB', 'MAX_MEM_USAGE_MB', 'NUM_EXEC_PROCS')
  )
}

get_records <- function (con, query, prefix, fields) {
  queue_filter <- list(
    "prefix" = list("JOB_NAME" = prefix)
  )
  query$bool$filter <- c(query$bool$filter, list(queue_filter))
  df <- scroll_elastic(
    con = con,
    body = list(query = query),
    fields = fields
  )

  return(df)
}
