library(dplyr)

source("src/constants.R")

rename_raw_elastic_fields <- function (df, map = elastic_column_map) {
  rename(df, any_of(map))
}
