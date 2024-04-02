library(ggplot2)
loadNamespace('ggrepel')

piechart <- function(df, count_field = 'doc_count', key_field = 'key', legend_name) {
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
