library(ggplot2)
library(dplyr)
loadNamespace('ggrepel')

source('src/timeseries_helpers.R')
source('src/constants.R')

# throw an error if not all colnames are in df
assert_colnames <- function (df, colnames) {
  stopifnot(all(colnames %in% colnames(df)))
}

integer_breaks <- function(n = 5, ...) {
  function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    return(breaks)
  }
}

piechart <- function(df, count_field, key_field, legend_name) {
  assert_colnames(df, c(count_field, key_field))
  ggplot(df, aes(x = '', fill = .data[[key_field]], y = .data[[count_field]])) +
    geom_bar(stat = 'identity', width=1, color="white") +
    coord_polar("y", start=0) +
    labs(fill = legend_name) +
    ggrepel::geom_label_repel(
      aes(y = pos, label = .data[[count_field]]),
      size = 4.5,
      nudge_x = 1,
      show.legend = FALSE) +
    theme_void()
}

make_farm_job_count_plot <- function (df) {
  assert_colnames(df, c('key', 'doc_count'))
  ggplot(df, aes(x = key, y = doc_count, fill = key)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(x = '', y = 'Number of jobs', fill = 'Farm') +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() + theme(legend.position = "none")
}

make_per_team_job_plot <- function (df, static = FALSE) {
  assert_colnames(df, c('accounting_name', 'job_status', 'doc_count'))
  p <- ggplot(df, aes(x = accounting_name, fill=job_status, y=doc_count)) +
    geom_bar(stat='identity') +
    labs(y = 'Number of jobs', fill = 'Job status', x = "Accounting name") +
    theme_bw()

  if(static) {
    p + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
              axis.title.x=element_blank())
  } else {
    p + coord_flip()
  }
}

make_wastage_plot <- function (df, renamer = c()) {
  assert_colnames(df, c('accounting_name', 'value', 'name'))
  ggplot(df, aes(x=accounting_name, y=value, fill=name)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    facet_grid(.~name, scales='free_x', labeller = as_labeller(setNames(names(renamer), renamer))) +
    theme_bw() +
    theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())
}

generate_efficiency_plot <- function(df, column_to_plot){
  if ('accounting_name' %in% colnames(df)) {
    fill_column <- 'accounting_name'
    date_col <- 'timestamp'
    group_cols <- c('timestamp')
  } else if ('USER_NAME' %in% colnames(df)) {
    fill_column <- 'USER_NAME'
    date_col <- 'date'
    group_cols <- c('date')
  } else if ('job_type' %in% colnames(df)) {
    fill_column <- 'job_type'
    date_col <- 'date'
    group_cols <- c('date', fill_column)
  } else {
    fill_column <- 'Reason'
    date_col <- 'timestamp'
    group_cols <- c('timestamp', fill_column)
  }

  if(any(endsWith(column_to_plot, c('_frac', '_rate')))){
    stopifnot(column_to_plot %in% c('cpu_wasted_frac', 'mem_wasted_frac', 'fail_rate'))

    if (column_to_plot == 'fail_rate') {
      df %>%
        mutate(failed_jobs = number_of_jobs * fail_rate) %>%
        group_by(across(all_of(date_col))) %>%
        summarise(fail_rate = sum(failed_jobs) / sum(number_of_jobs)) -> dt

      ggplot(dt, aes(x = .data[[date_col]], y = .data[[column_to_plot]])) +
        geom_line() + theme_bw()
    } else {
      prefix <- strsplit(column_to_plot, split = "_")[[1]][1]
      gb <- ifelse(prefix == 'mem', '_gb', '')
      efficiency_col <- paste0(prefix, '_efficiency')
      total_col <- paste0(prefix, '_avail', gb, '_hrs')
      wasted_col <- paste0(prefix, '_wasted', gb, '_hrs')

      df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(across(all_of(c(total_col, wasted_col)), sum), .groups = 'drop') %>%
        mutate(!!efficiency_col := (.data[[total_col]] - .data[[wasted_col]]) / .data[[total_col]]) -> dt

      p <- ggplot(dt, aes(x = .data[[date_col]], y = .data[[efficiency_col]])) + theme_bw()

      if (fill_column == 'Reason' || fill_column == 'job_type') {
        p + geom_line(aes(color = .data[[fill_column]]))
      } else {
        p + geom_line()
      }
    }
  } else {
    ggplot(df, aes(x = .data[[date_col]], y = .data[[column_to_plot]], fill = .data[[fill_column]])) +
      geom_bar(stat = 'identity') + theme_bw()
  }
}

generate_gpu_plot <- function(df, time_bucket, metric = 'PENDING_TIME_SEC') {
  colname <- paste(sub("_SEC", "", metric), "median", sep = "_")
  dt <- df %>%
    as_tsibble(key = `_id`, index = timestamp) %>%
    group_by(USER_NAME) %>%
    index_by_custom(time_bucket = time_bucket) %>%
    summarise(!!colname := median(.data[[metric]]))

  ggplot(dt, aes(x = date, y = .data[[colname]], fill = USER_NAME)) + geom_bar(stat = 'identity') + theme_bw() +
    scale_y_continuous(trans = scales::transform_timespan(unit = "secs"))
}

make_job_failure_timeplot <- function(df) {
  ggplot(df, mapping = aes(x = timestamp, y = doc_count, fill = job_status)) + 
    geom_bar(position="stack", stat="identity") + 
    theme_bw() +
    labs(x = 'Date', y = 'Number of jobs', fill = 'Job status')
}

generate_nextflow_efficiency_plot <- function (df, steps, grouping_col, efficiency_col) {
  assert_colnames(df, c('step', grouping_col, 'job_status', efficiency_col))
  df %>%
    filter(step %in% steps) %>%
    filter(job_status == 'Success') %>%  # should be removed once Failed statistics is correct
    ggplot(aes(x = .data[[grouping_col]], y = .data[[efficiency_col]], group = interaction(.data[[grouping_col]], job_status), fill = job_status)) +
      geom_violin() +
      facet_wrap(. ~ step, scales = 'free_x', ncol = 3) +
      theme_bw() +
      labs(
        x = column_rename_inv[[grouping_col]],
        y = column_rename_inv[[efficiency_col]],
        fill = column_rename_inv[['job_status']]
      ) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_x_continuous(breaks = integer_breaks())
}


generate_nextflow_cpu_plots <- function (df, steps) {
  generate_nextflow_efficiency_plot(
    df = df,
    steps = steps,
    grouping_col = 'procs',
    efficiency_col = 'Job_Efficiency'
  )
}

generate_nextflow_mem_plots <- function (df, steps) {
  generate_nextflow_efficiency_plot(
    df = df,
    steps = steps,
    grouping_col = 'mem_avail_gb',
    efficiency_col = 'Memory_Efficiency'
  )
}
