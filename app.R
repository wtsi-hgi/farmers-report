library(shiny)
library(bslib)
library(elastic)
library(ggplot2)
library(dplyr)

source('src/table_helpers.R')
source('src/elastic_helpers.R')
source('src/plot_helpers.R')

params <- list(
  elastic_host = "",
  elastic_username = "",
  elastic_password = ""
)

index <- "user-data-ssg-isg-lsf-analytics-*"
elastic_con <- connect(
  host = params$elastic_host,
  path = "",
  user = params$elastic_username,
  pwd = params$elastic_password,
  port = 19200,
  transport_schema = "http"
)

get_accounting_names <- function(con) {
  b <- build_agg_query("ACCOUNTING_NAME")

  res <- Search(con, index = index, body = b, asdf = T)

  res$aggregations$stats$buckets$key
}

get_user_names <- function(con, accounting_name) {
  b <- list(
    "query" = list(
      "bool" = list(
        "filter" = c(
          humgen_filters,
          list(
            list(
              "match_phrase" = list(
                "ACCOUNTING_NAME" = accounting_name
              )
            )
          )
        )
      )
    )
  )

  res <- Search(
    con,
    index = index,
    time_scroll="1m",
    source = c('USER_NAME'),
    body = b,
    asdf = T,
    size = 10000
  )

  df <- pull_everything(con, res)

  unique(df$USER_NAME)
}

ui <- page_sidebar(
  title = "HGI Farm Dashboard",
  sidebar = sidebar(
    selectInput(
      "accounting_name", "LSF Group",
      choices = get_accounting_names(elastic_con)
    ),
    selectInput(
      "user_name", "User",
      choices = NULL
    ),
    dateRangeInput(
      "period", "Period",
      start = Sys.Date() - 30,
      end = NULL,
      weekstart = 1
    )
  ),
  accordion(
    accordion_panel(
      "Farm Usage",
      plotOutput("farm_usage")
    ),
    accordion_panel(
      "Job failure statistics",
      plotOutput("job_failure")
    ),
    accordion_panel(
      "Efficiency",
      DT::DTOutput("efficiency")
    ),
    open = FALSE
  )
)

server <- function(input, output, session) {
  observeEvent(input$accounting_name, {
    updateSelectInput(
      inputId = "user_name",
      choices = get_user_names(elastic_con, input$accounting_name)
    )
  })

  humgen_user_query <- eventReactive(input$user_name, {
    list(
      "bool" = list(
        "filter" = c(
          humgen_filters,
          list(
            list(
              "match_phrase" = list(
                "USER_NAME" = input$user_name
              )
            )
          )
        )
      )
    )
  })

  output$farm_usage <- renderPlot({
    b <- build_agg_query("CLUSTER_NAME", query = humgen_user_query())

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- res$aggregations$stats$buckets %>%
      arrange(key) %>%
      mutate_for_piechart()

    piechart(df, legend_name = 'Farm')
  })

  output$job_failure <- renderPlot({
    b <- build_agg_query("Job", query = humgen_user_query())

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- res$aggregations$stats$buckets %>%
      arrange(key) %>%
      mutate_for_piechart()

    piechart(df, legend_name = 'Job status')
  })

  output$efficiency <- DT::renderDT({
    custom_aggs <- list(
      "cpu_avail_sec" = list(
        "sum" = list(
          "field" = "AVAIL_CPU_TIME_SEC"
        )
      ),
      "cpu_wasted_sec" = list(
        "sum" = list(
          "field" = "WASTED_CPU_SECONDS"
        )
      ),
      "mem_avail_mb_sec" = list(
        "sum" = list(
          "field" = "MEM_REQUESTED_MB_SEC"
        )
      ),
      "mem_wasted_mb_sec" = list(
        "sum" = list(
          "field" = "WASTED_MB_SECONDS"
        )
      ),
      "wasted_cost" = wasted_cost_agg
    )

    b <- build_terms_query(
      fields = c("ACCOUNTING_NAME", "NUM_EXEC_PROCS", "Job"),
      aggs = custom_aggs
    )

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- res$aggregations$stats$buckets %>%
      select(-key_as_string, -doc_count) %>%
      tidyr::hoist(.col = key, accounting_name = 1L, procs = 2L, job_status = 3L) %>%
      rename_all(~gsub('.value', '', .))

    df %>%
      mutate(mem_wasted_cost = mem_wasted_mb_sec * gb_ram_hour / 1024 / 60 / 60) %>%
      mutate(cpu_wasted_sec = ifelse(job_status == 'Success' & procs == 1, 0, cpu_wasted_sec),
             wasted_cost = ifelse(job_status == 'Success' & procs == 1, mem_wasted_cost, wasted_cost)) %>%
      group_by(accounting_name) %>%
      summarise_at(c('cpu_avail_sec', 'cpu_wasted_sec', 'mem_avail_mb_sec', 'mem_wasted_mb_sec', 'wasted_cost'), sum) %>%
      mutate(
        cpu_avail_hrs = cpu_avail_sec / 60 / 60,
        cpu_wasted_frac = cpu_wasted_sec / cpu_avail_sec,
        cpu_wasted_hrs = cpu_wasted_sec / 60 / 60,
        mem_avail_gb_hrs = mem_avail_mb_sec / 1024 / 60 / 60,
        mem_wasted_frac = mem_wasted_mb_sec / mem_avail_mb_sec,
        mem_wasted_gb_hrs = mem_wasted_mb_sec / 1024 / 60 / 60
      ) %>%
      select(accounting_name, cpu_avail_hrs, cpu_wasted_hrs, cpu_wasted_frac, mem_avail_gb_hrs, mem_wasted_gb_hrs, mem_wasted_frac, wasted_cost) %>%
      rename_group_column() -> dt

    ranks <- generate_ranks(dt) %>% select(accounting_name, awesomeness)
    dt %>%
      left_join(ranks, by = 'accounting_name') -> dt

    make_dt(dt, all_rows = params$static)
  })
}

shinyApp(ui = ui, server = server)
