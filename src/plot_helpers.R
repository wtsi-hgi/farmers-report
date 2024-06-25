library(ggplot2)
library(dplyr)
loadNamespace('ggrepel')

source('src/timeseries_helpers.R')

# throw an error if not all colnames are in df
assert_colnames <- function (df, colnames) {
  stopifnot(all(colnames %in% colnames(df)))
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
    group_cols <- c('timestamp')
  } else {
    fill_column <- 'Reason'
    group_cols <- c('timestamp', fill_column)
  }

  if(endsWith(column_to_plot, '_frac')){
    stopifnot(column_to_plot %in% c('cpu_wasted_frac', 'mem_wasted_frac'))

    prefix <- strsplit(column_to_plot, split = "_")[[1]][1]
    gb <- ifelse(prefix == 'mem', '_gb', '')
    efficiency_col <- paste0(prefix, '_efficiency')
    total_col <- paste0(prefix, '_avail', gb, '_hrs')
    wasted_col <- paste0(prefix, '_wasted', gb, '_hrs')

    df %>%
      group_by(across(group_cols)) %>%
      summarise(across(c(total_col, wasted_col), sum), .groups = 'drop') %>%
      mutate(!!efficiency_col := (.data[[total_col]] - .data[[wasted_col]]) / .data[[total_col]]) -> dt

    p <- ggplot(dt, aes(x = timestamp, y = .data[[efficiency_col]])) + theme_bw()

    if (fill_column == 'Reason') {
      p + geom_line(aes(color = .data[[fill_column]]))
    } else {
      p + geom_line()
    }
  } else {
    ggplot(df, aes(x = timestamp, y = .data[[column_to_plot]], fill = .data[[fill_column]])) +
      geom_bar(stat = 'identity') + theme_bw()
  }
}

generate_gpu_plot <- function(df, time_bucket, metric = 'PENDING_TIME_SEC') {
  colname <- paste(metric, "median", sep = "_")
  # Warning: Error in validate_tsibble: A valid tsibble must have distinct rows identified by key and index.
  dt <- df %>%
    as_tsibble(key = JOB_ID, index = timestamp) %>%
    # group_by_key() %>%
    group_by(USER_NAME) %>%
    index_by_custom(time_bucket = time_bucket) %>%
    summarise(!!colname := median(.data[[metric]]))

  ggplot(dt, aes(x = date, y = .data[[colname]], fill = USER_NAME)) + geom_bar(stat = 'identity') + theme_bw()
}
