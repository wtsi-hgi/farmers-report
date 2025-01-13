library(dplyr)
loadNamespace('stringr')

source("src/constants.R")
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
    fields = append(raw_stats_elastic_columns, 'JOB_NAME')
  )

  dt <- df %>%
    prepare_raw_stats_records() %>%
    mutate(
      step = parse_nextflow_step(job_name, pipeline_name),
      keep = 'unused'
    )
}

parse_nextflow_step <- function(job_names, pipeline_name) {
  # if the result of the parsing is empty, we replace it with prefix
  # this is to handle the case where there is no process name in the job name
  step <- stringr::str_remove_all(job_names, stringr::str_glue('^nf-{pipeline_name}_?|_?\\(.*\\)?$'))
  step <- ifelse(step == "", pipeline_name, step)
  return(step)
}

get_records <- function (con, query, prefix, fields) {
  job_name_filter <- build_prefix_filter("JOB_NAME", prefix)
  query$bool$filter <- append(query$bool$filter, job_name_filter)

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
    summarise(
      number_of_jobs = n(),
      fail_rate = sum(job_status == 'Failed') / n()
    ) %>%
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
      fail_rate = sum(job_status == 'Failed') / n(),
      best_eff = no_inf_max(Job_Efficiency),
      .groups = 'drop'
    ) 
}

generate_nextflow_mem_efficiency <- function (df) {
  df %>%
    group_by(step, procs, mem_avail_gb) %>%
    summarise(
      number_of_jobs = n(),
      fail_rate = sum(job_status == 'Failed') / n(),
      best_eff = no_inf_max(Memory_Efficiency),
      max_mem_used_gb = convert_bytes(no_inf_max(MAX_MEM_USAGE_MB), from = 'mb', to = 'gb'),
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
