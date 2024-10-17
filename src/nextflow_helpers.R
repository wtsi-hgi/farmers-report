library(dplyr)
loadNamespace('stringr')

source("src/elastic_helpers.R")
source("src/table_helpers.R")

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
      step = parse_nextflow_step(JOB_NAME, pipeline_name),
      RUN_TIME_SEC = MEM_REQUESTED_MB_SEC / MEM_REQUESTED_MB,
      Job_Efficiency = (AVAIL_CPU_TIME_SEC - WASTED_CPU_SECONDS) / AVAIL_CPU_TIME_SEC,
      MAX_MEM_EFFICIENCY = (MEM_REQUESTED_MB_SEC - WASTED_MB_SECONDS) / MEM_REQUESTED_MB_SEC,
      MAX_MEM_USAGE_MB = (MEM_REQUESTED_MB_SEC - WASTED_MB_SECONDS) / RUN_TIME_SEC
    ) %>%
    select(-JOB_NAME) %>%
    rename_raw_elastic_fields()
}

parse_nextflow_step <- function(job_names, pipeline_name) {
  # if the result of the parsing is empty, we replace it with prefix
  # this is to handle the case where there is no process name in the job name
  step <- stringr::str_remove_all(job_names, stringr::str_glue('^nf-{pipeline_name}_?|_?\\(.*\\)?$'))
  step <- ifelse(step == "", pipeline_name, step)
  return(step)
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

generate_nextflow_cpu_efficiency <- function (df) {
  df %>%
    group_by(step, procs) %>%
    summarise(
      N = n(),
      best_eff = max(Job_Efficiency, na.rm = TRUE),
      .groups = 'drop'
    ) 
}

generate_nextflow_mem_efficiency <- function (df) {
  df %>%
    mutate(
      mem_avail_gb = convert_mb_to_gb(MEM_REQUESTED_MB)
    ) %>%
    group_by(step, procs, mem_avail_gb) %>%
    summarise(
      N = n(),
      best_eff = max(MAX_MEM_EFFICIENCY, na.rm = TRUE),
      max_mem_used_gb = max(mem_avail_gb, na.rm = TRUE),  # debug values
      .groups = 'drop'
    )
}

add_zero_length_space <- function(pipeline_names) {
  pipeline_names <- gsub("_", "_\u200B", pipeline_names)
  return(pipeline_names)
}
