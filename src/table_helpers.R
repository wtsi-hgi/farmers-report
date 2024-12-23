library(dplyr)
loadNamespace('gt')

source('src/constants.R')

rename_group_column <- function(df, mapping = team_map) {
  df %>%
    left_join(mapping, by = c('accounting_name' = 'team_code')) %>%
    mutate(accounting_name = ifelse(is.na(team_name), accounting_name, team_name), .keep = 'unused')
}

set_team_names <- function (teams, mapping) {
  data.frame(accounting_name = teams) %>%
    rename_group_column(mapping = mapping) %>%
    mutate(team_name = teams) %>%
    tibble::deframe()
}

specify_wastage_reason <- function(df) {
  df %>%
    rename(Reason = job_status) %>%
    mutate(
      Reason = stringr::str_replace_all(Reason, c("Success" = "Due to over-requesting", "Failed" = "Due to job failure"))
    )
}

generate_total_stats_dt <- function(dt, col_name) {
  dt %>%
    summarise(
      across(where(is.numeric), sum)
    ) %>%
    mutate(
      !!col_name := "Total"
    )
}

generate_total_failure_dt <- function(dt) {
  dt %>%
    generate_total_stats_dt(col_name = 'accounting_name') %>%
    mutate(
      fail_rate = Failed / (Failed + Success)
    )
}

generate_total_wastage_dt <- function(dt) {
  failrate <- sum(filter(dt, job_status == 'Failed')$number_of_jobs) / sum(dt$number_of_jobs)

  dt %>%
    generate_total_stats_dt(col_name = 'job_status') %>%
      mutate(
        cpu_wasted_frac = cpu_wasted_hrs / cpu_avail_hrs,
        mem_wasted_frac = mem_wasted_gb_hrs / mem_avail_gb_hrs,
        fail_rate = failrate
      )
}

adjust_statistics <- function (df) {
  df <- df %>%
    mutate(
      mem_wasted_cost = mem_wasted_mb_sec * ram_mb_second,
      cpu_wasted_sec = ifelse(job_status == 'Success' & procs == 1, 0, cpu_wasted_sec)
    )

  if('wasted_cost' %in% names(df))
    df <- mutate(df, wasted_cost = ifelse(job_status == 'Success' & procs == 1, mem_wasted_cost, wasted_cost))

  return(df)
}

adjust_interactive_statistics <- function (df, interactive_jobs) {
  is_interactive <- df$job_type == 'interactive'
  df %>%
    left_join(interactive_jobs, by = '_id') %>%
    mutate(
      cpu_wasted_sec = ifelse(is_interactive, RAW_WASTED_CPU_SECONDS, cpu_wasted_sec),
      mem_wasted_mb_sec = ifelse(is_interactive, RAW_WASTED_MB_SECONDS, mem_wasted_mb_sec),
      job_status = ifelse(is_interactive, 'Success', job_status)
    ) %>%
    select(-c('RAW_WASTED_CPU_SECONDS', 'RAW_WASTED_MB_SECONDS'))
}

generate_wasted_cost <- function (df) {
  df %>%
    mutate(
      cpu_wasted_cost = cpu_wasted_sec * cpu_second,
      mem_wasted_cost = mem_wasted_mb_sec * ram_mb_second,
      wasted_cost = pmax(cpu_wasted_cost, mem_wasted_cost)
    )
}

make_dt <- function(df, all_rows = FALSE, table_view_opts = NULL){
  sorting_column <- integer(0)

  if('wasted_cost' %in% colnames(df))
    sorting_column <- which(colnames(df) == 'wasted_cost')

  if('awesomeness' %in% colnames(df))
    sorting_column <- which(colnames(df) == 'awesomeness')

  if(nrow(df) > 0){
    if('median_wait_time' %in% colnames(df))
      df <- mutate(df, median_wait_time = gt::vec_fmt_duration(median_wait_time, input_units = 'seconds', max_output_units = 2))

    if('median_run_time' %in% colnames(df))
      df <- mutate(df, median_run_time = gt::vec_fmt_duration(median_run_time, input_units = 'seconds', max_output_units = 2))
  }

  options <- list(
    dom = table_view_opts,
    pageLength = ifelse(all_rows, nrow(df), 10)
  )

  show_rownames <- FALSE
  if(length(sorting_column) > 0)
    options$order <- list(list(sorting_column - !show_rownames, 'desc'))

  dt <- DT::datatable(
    df,
    options = options,
    rownames = show_rownames,
    colnames = column_rename[column_rename %in% colnames(df)]
  )

  if('mem_avail_gb_hrs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Requested memory (GB x hrs)', 0)

  if('mem_wasted_gb_hrs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Wasted memory (GB x hrs)', 0)

  if('mem_wasted_frac' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Wasted memory fraction', 2)

  if('best_eff' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Best efficiency', 2)

  if('cpu_avail_hrs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Requested CPU (hrs)', 0)

  if('cpu_wasted_hrs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Wasted CPU (hrs)', 0)

  if('cpu_wasted_frac' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Wasted CPU fraction', 2)

  if('wasted_cost' %in% colnames(df))
    dt <- DT::formatCurrency(dt, 'Wasted money', currency = '£', digits = 2)

  if('fail_rate' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Fail rate', 1)

  if('awesomeness' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Awesome-ness', 1)

  if('max_mem_used_gb' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Max used memory (GB)', 2)

  if('number_of_jobs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Number of jobs', 0)

  if('job_type' %in% colnames(df))
    dt <- DT::formatStyle(dt, 'Job type', cursor = 'pointer')

  return(dt)
}

generate_ranks <- function(df) {
  rank_fields <- c('cpu_wasted_frac', 'mem_wasted_frac')
  max_rank <- length(rank_fields) * nrow(df)
  cpu_threshold <- 0.5 * median(df$cpu_avail_hrs)
  mem_threshold <- 0.5 * median(df$mem_avail_gb_hrs)

  df %>%
    mutate(  # the smaller your rank the worse your performance
           cpu_frac_rank = rank(-cpu_wasted_frac),
           mem_frac_rank = rank(-mem_wasted_frac)) %>%
    mutate(
      cpu_frac_rank = ifelse(cpu_avail_hrs > cpu_threshold, cpu_frac_rank, cpu_frac_rank / 2),
      mem_frac_rank = ifelse(mem_avail_gb_hrs > mem_threshold, mem_frac_rank, mem_frac_rank / 2)
    ) %>%
    select(accounting_name, ends_with('_rank')) %>%
    mutate(rank = cpu_frac_rank + mem_frac_rank) %>%
    mutate(awesomeness = 10 * rank / max_rank)
}

mutate_for_piechart <- function(df, count_col = 'doc_count') {
  mutate(df,
    csum = rev(cumsum(rev(.data[[count_col]]))),
    pos = .data[[count_col]]/2 + lead(csum, 1),
    pos = if_else(is.na(pos), .data[[count_col]]/2, pos)
  )
}

generate_efficiency_stats <- function(df, extra_stats = list()) {
  fields <- c('cpu_avail_sec', 'cpu_wasted_sec', 'mem_avail_mb_sec', 'mem_wasted_mb_sec', 'wasted_cost')
  
  df %>%
    summarise(
      across(all_of(fields), sum),
      !!!extra_stats,
      .groups = 'drop'
    ) %>%
    mutate(
      cpu_avail_hrs = convert_sec_to_hrs(cpu_avail_sec),
      cpu_wasted_hrs = convert_sec_to_hrs(cpu_wasted_sec),
      cpu_wasted_frac = cpu_wasted_sec / cpu_avail_sec,
      mem_avail_gb_hrs = convert_mb_sec_to_gb_hrs(mem_avail_mb_sec),
      mem_wasted_gb_hrs = convert_mb_sec_to_gb_hrs(mem_wasted_mb_sec),
      mem_wasted_frac = mem_wasted_mb_sec / mem_avail_mb_sec,
    ) %>%
    relocate(wasted_cost, .after = last_col()) %>%
    select(-setdiff(fields, 'wasted_cost'))
}

prepare_commands_table <- function (df) {
  df %>%
    select(-`_id`) %>%
    mutate(MEM_REQUESTED = convert_bytes(MEM_REQUESTED_MB, from = 'mb', to = 'b'), .keep = 'unused') %>%
    rename(RUN_TIME = RUN_TIME_SEC) %>%
    gt::gt() %>%
    gt::cols_align(align = "left", columns = 'Command') %>%
    gt::fmt_percent(columns = c('Job_Efficiency_Raw_Percent', 'RAW_MAX_MEM_EFFICIENCY_PERCENT'), scale_values = FALSE) %>%
    gt::fmt_bytes(columns = MEM_REQUESTED, standard = 'binary') %>%
    gt::fmt_duration(RUN_TIME, input_units = 'seconds', max_output_units = 1) %>%
    gt::cols_label(
      Job_Efficiency_Raw_Percent = 'Raw CPU efficiency',
      RAW_MAX_MEM_EFFICIENCY_PERCENT = 'Raw memory efficiency',
      MEM_REQUESTED = 'Memory requested',
      RUN_TIME = 'Run time'
    ) %>%
    gt::cols_move_to_end(Command) %>%
    gt::cols_move_to_start(Job)
}

get_colname_options <- function(df, exclude_columns) {
  cols <- colnames(df)
  cols <- setdiff(cols, exclude_columns)
  cols <- column_rename[column_rename %in% cols]
  return(cols)
}

convert_mb_sec_to_gb_hrs <- function (mb_sec) {
  convert_sec_to_hrs(convert_bytes(mb_sec, from = 'mb', to = 'gb'))
}

convert_sec_to_hrs <- function (sec) {
  sec / 60 / 60
}

convert_bytes <- function (x, from, to) {
  bytes_prefix <- c('b', 'kb', 'mb', 'gb')
  stopifnot(from %in% bytes_prefix, to %in% bytes_prefix)

  diff <- which(from == bytes_prefix) - which(to == bytes_prefix)
  x * 1024 ^ diff
}
