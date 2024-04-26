library(ggplot2)
loadNamespace('ggrepel')

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
