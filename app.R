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

generate_efficiency <- function (input, con, query, adjust, team_statistics) {
  req(input$accounting_name)
  if (input$accounting_name != 'all') {
    req(input$user_name)
  }

  if (input$accounting_name == 'all') {
    dt <- get_bom_statistics(con, query = query, adjust = adjust)
  } else {
    if (input$user_name == 'all') {
      if (adjust){
        dt <- team_statistics()
      } else {
        dt <- get_team_statistics(con, query = query, adjust = FALSE)
      }
    } else {
      dt <- get_user_statistics(con, query = query, adjust = adjust)
    }
  }

  make_dt(dt, table_view_opts = 'ftp')
}

ui <- page_sidebar(
  title = "HGI Farm Dashboard",
  sidebar = sidebar(
    width = 270,
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
      plotOutput("job_failure"),
      plotOutput("per_bucket_job_failure"),
      DT::DTOutput("per_bucket_job_failure_table"),
      value = "job_failure_panel"
    ),
    accordion_panel(
      "Unadjusted Efficiency",
      DT::DTOutput("unadjusted_efficiency")
    ),
    accordion_panel(
      "Efficiency",
      textOutput("adjustments_explanation"),
      DT::DTOutput("efficiency"),
      htmlOutput("awesomeness_formula")
    ),
    id = "myaccordion",
    open = FALSE
  )
)

server <- function(input, output, session) {
  observeEvent(input$bom, {
    req(input$bom)
    accounting_names <- get_accounting_names(elastic_con, input$bom)
    team_names <- set_team_names(accounting_names, mapping = team_map)

    updateSelectInput(
      inputId = "accounting_name",
      choices = c('all', team_names)
    )
  })

  observeEvent(input$accounting_name, {
    req(input$bom, input$accounting_name)
    if (input$accounting_name == 'all') {
       user_names <- ""
    } else {
      user_names <- get_user_names(elastic_con, input$bom, input$accounting_name)
      if (length(user_names) > 1){
        user_names <- c('all', user_names)
      }
    }

    updateSelectInput(
      inputId = "user_name",
      choices = user_names
    )
  })

  elastic_query <- reactive({
    req(input$bom, input$accounting_name)
    if (input$accounting_name != 'all'){
      req(input$user_name)
    }

    custom_filters = NULL
    if(input$accounting_name != 'all'){
      if(input$user_name == 'all'){
        custom_filters <- build_match_phrase_filter("ACCOUNTING_NAME", input$accounting_name)
      } else {
        custom_filters <- build_match_phrase_filter("USER_NAME", input$user_name)
      }
    }

    build_humgen_query(
      filters = build_humgen_filters(
        BOM = input$bom,
        custom_filters = custom_filters,
        date_range = input$period
      )
    )
  })

  team_statistics <- reactive({
    get_team_statistics(elastic_con, query = elastic_query())
  })

  per_bucket_job_failure_df <- reactive({
    if (input$accounting_name == 'all') {

      b <- build_terms_query(fields = c("ACCOUNTING_NAME", "Job"), query = elastic_query())

      res <- Search(elastic_con, index = index, body = b, asdf = T)

      parse_elastic_multi_agg(res, column_names = c('accounting_name', 'job_status')) %>%
        rename_group_column() -> df

    } else {
      req(input$user_name)
      if (input$user_name == 'all') {
        # statistics for only one team
        df <- team_statistics()

        # transform df
        df <- df %>%
          rename(accounting_name = USER_NAME) %>%
          mutate(Failed = number_of_jobs * fail_rate,
                Success = number_of_jobs - Failed) %>%
          tidyr::pivot_longer(cols = c('Success', 'Failed'), names_to = 'job_status', values_to = 'doc_count')

        # we need by this point: c('accounting_name', 'job_status', 'doc_count')
      }
    }
  })

  output$job_failure <- renderPlot({
    b <- build_agg_query("Job", query = elastic_query())

    res <- Search(elastic_con, index = index, body = b, asdf = T)

    df <- parse_elastic_single_agg(res) %>%
      mutate_for_piechart()

    piechart(df, count_field = 'doc_count', key_field = 'key', legend_name = 'Job status')
  })

  output$per_bucket_job_failure <- renderPlot({
    if (input$accounting_name == 'all' || input$user_name == 'all') {
      df <- per_bucket_job_failure_df()
      make_per_team_job_plot(df)
    }
  })

  output$per_bucket_job_failure_table <- DT::renderDT({
    if (input$accounting_name == 'all' || input$user_name == 'all') {
      df <- per_bucket_job_failure_df()
      df %>%
        tidyr::pivot_wider(id_cols = 'accounting_name', names_from = 'job_status', values_from = 'doc_count', values_fill = 0) %>%
        mutate(fail_rate = Failed / (Failed + Success)) %>%
        arrange(desc(Failed)) -> dt
      total_dt <- generate_total_failure_dt(dt)
      dt <- rbind(dt, total_dt)
      dt <- dt %>%
        mutate(
          Total = Success + Failed
        ) %>%
        select(accounting_name, Total, Success, Failed, fail_rate) %>%
        arrange(desc(Total))
      make_dt(dt, table_view_opts = 'ftp')
    }
  })

  output$unadjusted_efficiency <-  DT::renderDT({
    generate_efficiency(input, elastic_con, adjust = FALSE, query = elastic_query(), team_statistics = team_statistics)
  })

  output$adjustments_explanation <- renderText({
    adjustments_explanation
  })

  output$efficiency <- DT::renderDT({
     generate_efficiency(input, elastic_con, adjust = TRUE, query = elastic_query(), team_statistics = team_statistics)
  })

  output$awesomeness_formula <- renderUI({
    if (input$accounting_name == 'all') {
      withMathJax(awesomeness_explanation, awesomeness_formula)
    }
  })

  observe({
    if (input$accounting_name == 'all' || input$user_name == 'all') {
      accordion_panel_update(id = 'myaccordion', target = 'job_failure_panel',
        plotOutput("per_bucket_job_failure"),
        DT::DTOutput("per_bucket_job_failure_table")
      ) 
    } else {
      accordion_panel_update('myaccordion', target = 'job_failure_panel',
        plotOutput("job_failure")
      ) 
    }
  })

}

shinyApp(ui = ui, server = server)
