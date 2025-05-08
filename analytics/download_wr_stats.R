library(logging)
library(dplyr, warn.conflicts = FALSE)

source('src/config.R')
source('src/table_helpers.R')
source('src/elastic_helpers.R')

# inputs
start_date <- Sys.Date() - 30*12
config_file <- "config-elastic.yaml"  # works only with real elastic because of BOM-unspecific query
outfolder <- "./data"

dir.create(outfolder, showWarnings = FALSE)

config <- read_config(config_file, section = "elastic")

elastic_con <- elastic::connect(
  host = config$elastic$host,
  path = "",
  user = config$elastic$username,
  pwd = config$elastic$password,
  port = config$elastic$port,
  transport_schema = "http"
)

attr(elastic_con, 'index') <- config$elastic$index

fields_to_download <- c(
  "CLUSTER_NAME", "BOM", "ACCOUNTING_NAME", "USER_NAME",
  "AVAIL_CPU_TIME_SEC", "MEM_REQUESTED_MB_SEC")

date <- start_date
last_date <- Sys.Date() - 1

while (date <= last_date) {
    outfile <- file.path(outfolder, paste0(date, ".csv"))

    if (file.exists(outfile)) {
        date <- date + 1
        next
    }

    loginfo("Processing date: %s", date)

    query <- build_humgen_query(
        filters = build_humgen_filters(
            BOM = NULL,
            accounting_name = NULL,
            user_name = NULL,
            meta_cluster_name = NULL,
            date_range = c(date, date),
            custom_filters = build_prefix_filter("JOB_NAME", "wrp_")
        )
    )

    loginfo("..Downloading data")

    df <- scroll_elastic(
        con = elastic_con,
        body = list(query = query),
        fields = fields_to_download
    )

    loginfo("..Downloaded %d records", nrow(df))
    loginfo("..Agregating data")

    df <- df %>%
        mutate(date = date) %>%
        group_by(date, CLUSTER_NAME, BOM, ACCOUNTING_NAME, USER_NAME) %>%
        summarise(
            number_of_jobs = n(),
            total_cpu_usage = convert_sec_to_hrs(sum(AVAIL_CPU_TIME_SEC)),
            total_memory_usage = convert_mb_sec_to_gb_hrs(sum(MEM_REQUESTED_MB_SEC)),
            .groups = "drop"
    )

    if (nrow(df) > 0) {
        loginfo("..Writing data to file %s", outfile)
        data.table::fwrite(df, file = outfile, sep = ",")
    }

    date <- date + 1
}
