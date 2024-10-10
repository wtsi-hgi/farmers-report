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
