library(shiny)
library(bslib)
library(elastic)
library(ggplot2)
library(dplyr)

source('src/table_helpers.R')
source('src/elastic_helpers.R')
source('src/plot_helpers.R')
source('src/constants.R')
source('src/config.R')

config <- read_config("config.yaml")

elastic_con <- connect(
  host = config$elastic$host,
  path = "",
  user = config$elastic$username,
  pwd = config$elastic$password,
  port = 19200,
  transport_schema = "http"
)

get_accounting_names <- function(con) {
  b <- build_agg_query("ACCOUNTING_NAME")

  res <- Search(con, index = index, body = b, asdf = T)

  parse_elastic_single_agg(res)$key
}

get_user_names <- function(con, accounting_name) {
  b <- list(
    query = build_humgen_query(
      filters = build_humgen_filters(
        custom_filters = list(
          "match_phrase" = list(
            "ACCOUNTING_NAME" = accounting_name
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
    build_humgen_query(
      filters = build_humgen_filters(
        custom_filters = list(
          "match_phrase" = list(
            "USER_NAME" = input$user_name
          )
        )
      )
    )
  })

  output$farm_usage <- renderPlot({
    b <- build_agg_query("CLUSTER_NAME", query = humgen_user_query())

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- parse_elastic_single_agg(res) %>%
      mutate_for_piechart()

    piechart(df, count_field = 'doc_count', key_field = 'key', legend_name = 'Farm')
  })

  output$job_failure <- renderPlot({
    b <- build_agg_query("Job", query = humgen_user_query())

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- parse_elastic_single_agg(res) %>%
      mutate_for_piechart()

    piechart(df, count_field = 'doc_count', key_field = 'key', legend_name = 'Job status')
  })

  output$efficiency <- DT::renderDT({
    custom_aggs <- list(
      "cpu_avail_sec" = build_elastic_sub_agg("AVAIL_CPU_TIME_SEC", "sum"),
      "cpu_wasted_sec" = build_elastic_sub_agg("WASTED_CPU_SECONDS", "sum"),
      "mem_avail_mb_sec" = build_elastic_sub_agg("MEM_REQUESTED_MB_SEC", "sum"),
      "mem_wasted_mb_sec" = build_elastic_sub_agg("WASTED_MB_SECONDS", "sum"),
      "wasted_cost" = wasted_cost_agg
    )

    b <- build_terms_query(
      fields = c("NUM_EXEC_PROCS", "Job"),
      aggs = custom_aggs,
      query = humgen_user_query()
    )

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- parse_elastic_multi_agg(res, column_names = c('procs', 'job_status')) %>%
      select(-doc_count)

    dt <- generate_app_wastage_statistics(df)
    
    dt_total <- generate_total_wastage_dt(dt)
    
    dt <- rbind(dt, dt_total)

    specify_wastage_reason(dt) %>%
      make_dt(table_view_opts = 't')
  })
}

shinyApp(ui = ui, server = server)
