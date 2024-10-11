library(dplyr)
loadNamespace('stringr')

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
  pipeline_prefix <- paste("nf", pipeline_name, sep = "-")
  df <- get_records(
    con = con,
    query = query,
    prefix = pipeline_prefix,
    fields = c('JOB_NAME', 'Job',
               'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
               'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS'
    )
  )

  df %>%
    mutate(
      step = stringr::str_remove_all(JOB_NAME, stringr::str_glue('^{pipeline_prefix}_|_\\(.*\\)?$')),
      Job_Efficiency_Percent = 100 * (AVAIL_CPU_TIME_SEC - WASTED_CPU_SECONDS) / AVAIL_CPU_TIME_SEC,
      MAX_MEM_EFFICIENCY_PERCENT = 100 * (MEM_REQUESTED_MB_SEC - WASTED_MB_SECONDS) / MEM_REQUESTED_MB_SEC
    ) %>%
    rename_raw_elastic_fields()
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

generate_nextflow_step_freq <- function (df) {
  df %>%
    group_by(step) %>%
    tally() %>%
    arrange(desc(n))
}
