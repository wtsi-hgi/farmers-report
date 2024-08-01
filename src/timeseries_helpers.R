library(tsibble)
loadNamespace('lubridate')

source('src/constants.R')

validate_time_bucket <- function(time_bucket) {
  match.arg(time_bucket, choices = c(time_buckets, "none"))
}

index_by_custom <- function(df, time_bucket) {
  validate_time_bucket(time_bucket)

  if(time_bucket == 'day')
    dt <- index_by(df, date = ~ as.Date(.))
  
  if(time_bucket == 'week'){
    dt <- index_by(df, date = ~ as.Date(yearweek(.)))
  }

  if(time_bucket == 'month')
    dt <- index_by(df, date = ~ yearmonth(.))
  
  return(dt)
}

isInvalidDate <- function(date) {
  is.na(lubridate::parse_date_time(date, orders = "ymd"))
}
