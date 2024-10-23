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
  empty_table <- data.frame(
    JOB_NAME = character(),
    Job = character(),
    NUM_EXEC_PROCS = numeric(),
    AVAIL_CPU_TIME_SEC = numeric(),
    WASTED_CPU_SECONDS = numeric(),
    MEM_REQUESTED_MB = numeric(),
    MEM_REQUESTED_MB_SEC = numeric(),
    WASTED_MB_SECONDS = numeric(),
    stringsAsFactors = FALSE
  )

  if(is.null(pipeline_name) || pipeline_name == "") {
    df <- empty_table
  } else {
    pipeline_prefix <- paste("nf", pipeline_name, sep = "-")
    df <- get_records(
      con = con,
      query = query,
      prefix = pipeline_prefix,
      fields = colnames(empty_table)
    )

    if(nrow(df) == 0) df <- empty_table
  }

  df %>%
    filter(AVAIL_CPU_TIME_SEC > 0) %>%
    mutate(
      step = parse_nextflow_step(JOB_NAME, pipeline_name),
      RUN_TIME_SEC = MEM_REQUESTED_MB_SEC / MEM_REQUESTED_MB,
      Job_Efficiency = (AVAIL_CPU_TIME_SEC - WASTED_CPU_SECONDS) / AVAIL_CPU_TIME_SEC,
      Memory_Efficiency = (MEM_REQUESTED_MB_SEC - WASTED_MB_SECONDS) / MEM_REQUESTED_MB_SEC,
      MAX_MEM_USAGE_MB = (MEM_REQUESTED_MB_SEC - WASTED_MB_SECONDS) / RUN_TIME_SEC,
      mem_avail_gb = convert_mb_to_gb(MEM_REQUESTED_MB)
    ) %>%
    select(-JOB_NAME, -MEM_REQUESTED_MB, -AVAIL_CPU_TIME_SEC, -WASTED_CPU_SECONDS, -RUN_TIME_SEC, -MEM_REQUESTED_MB_SEC, -WASTED_MB_SECONDS) %>%
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
    tally(name = "number_of_jobs") %>%
    arrange(desc(number_of_jobs))
}

# regular max that does not produce -Inf for empty vectors
no_inf_max <- function (x) {
  if(length(x) == 0)
    return(NA_real_)
  max(x, na.rm = TRUE)
}

generate_nextflow_cpu_efficiency <- function (df) {
  df %>%
    group_by(step, procs) %>%
    summarise(
      number_of_jobs = n(),
      best_eff = no_inf_max(Job_Efficiency),
      .groups = 'drop'
    ) 
}

generate_nextflow_mem_efficiency <- function (df) {
  df %>%
    group_by(step, procs, mem_avail_gb) %>%
    summarise(
      number_of_jobs = n(),
      best_eff = no_inf_max(Memory_Efficiency),
      max_mem_used_gb = convert_mb_to_gb(no_inf_max(MAX_MEM_USAGE_MB)),
      .groups = 'drop'
    )
}

add_zero_length_space <- function(pipeline_names) {
  pipeline_names <- gsub("_", "_\u200B", pipeline_names)
  return(pipeline_names)
}

generate_nextflow_plot_text <- function (resourse, number_of_plots) {
  if(number_of_plots == 0){
    paste("Select step(s) from the left control panel to investigate", resourse, "efficiency further.")
  } else {
    paste("This plot shows the", resourse, "efficiency of selected pipeline steps.")
  }
}
