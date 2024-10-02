library(logging)

basicConfig()

log_action <- function (accordions, bom, group, user, period, time_bucket) {
  message <- paste(
    "USER-ACTION",
    jsonlite::toJSON(
      list(
        accordions = accordions,
        filters = list(
          bom = jsonlite::unbox(bom),
          accounting_name = jsonlite::unbox(group),
          user = jsonlite::unbox(user),
          period = period,
          time_bucket = jsonlite::unbox(time_bucket)
        )
      )
    )
  )
  loginfo(message)
}

log_request <- function (...) {
  args <- list(...)
  request_type <- ifelse('time_scroll' %in% names(args), 'scroll', 'search')
  message <- paste("REQUEST", "type", request_type)
  loginfo(message)
}
