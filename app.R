library(shiny)
library(bslib)
library(elastic)
library(ggplot2)
library(dplyr)

source('src/table_helpers.R')
source('src/elastic_helpers.R')
source('src/plot_helpers.R')
source('src/stat_helpers.R')
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

get_bom_names <- function(con) {
  b <- build_agg_query("BOM", query = build_humgen_query(filters = build_humgen_filters(BOM = NULL)))

  res <- Search(con, index = index, body = b, asdf = T)

  parse_elastic_single_agg(res)$key
}

get_accounting_names <- function(con, bom) {
  b <- build_agg_query("ACCOUNTING_NAME", query = build_humgen_query(filters = build_humgen_filters(BOM = bom)))

  res <- Search(con, index = index, body = b, asdf = T)

  parse_elastic_single_agg(res)$key
}

get_user_names <- function(con, bom, accounting_name) {
  b <- list(
    query = build_humgen_query(
      filters = build_humgen_filters(
        BOM = bom,
        custom_filters = build_match_phrase_filter("ACCOUNTING_NAME", accounting_name)
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
      "bom", "BOM",
      selected = "Human Genetics",
      choices = get_bom_names(elastic_con)
    ),
    selectInput(
      "accounting_name", "LSF Group",
      choices = NULL
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
  observeEvent(input$bom, {
    accounting_names <- get_accounting_names(elastic_con, input$bom)
    updateSelectInput(
      inputId = "accounting_name",
      choices = set_team_names(accounting_names, mapping = team_map)
    )
  })

  observeEvent(input$accounting_name, {
    req(input$bom, input$accounting_name)
    user_names <- get_user_names(elastic_con, input$bom, input$accounting_name)
    if (length(user_names) > 1){
      user_names <- c('all', user_names)
    }
    updateSelectInput(
      inputId = "user_name",
      choices = user_names
    )
  })

  elastic_query <- eventReactive(c(input$user_name, input$accounting_name), {
    req(input$bom, input$accounting_name, input$user_name)

    if(input$user_name == 'all'){
      custom_filters <- build_match_phrase_filter("ACCOUNTING_NAME", input$accounting_name)
    } else {
      custom_filters <- build_match_phrase_filter("USER_NAME", input$user_name)
    }

    build_humgen_query(
      filters = build_humgen_filters(
        BOM = input$bom,
        custom_filters = custom_filters)
    )
  })

  output$job_failure <- renderPlot({
    b <- build_agg_query("Job", query = elastic_query())

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- parse_elastic_single_agg(res) %>%
      mutate_for_piechart()

    piechart(df, count_field = 'doc_count', key_field = 'key', legend_name = 'Job status')
  })

  output$efficiency <- DT::renderDT({
    req(input$accounting_name, input$user_name)

    if(input$user_name == 'all') {
      if(input$accounting_name == 'all') {
        DT::datatable(data.frame())
      } else {
        dt <- get_team_statistics(elastic_con, query = elastic_query())
        make_dt(dt, table_view_opts = 'ftp')
      }
    } else {
      dt <- get_user_statistics(elastic_con, query = elastic_query())
      make_dt(dt, table_view_opts = 't')
    }
  })
}

shinyApp(ui = ui, server = server)
