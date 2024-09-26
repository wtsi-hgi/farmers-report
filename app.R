library(shiny)
library(bslib)
library(elastic)
library(ggplot2)
library(dplyr)
loadNamespace('shinycssloaders')
loadNamespace('stringr')

source('src/table_helpers.R')
source('src/elastic_helpers.R')
source('src/plot_helpers.R')
source('src/stat_helpers.R')
source('src/constants.R')
source('src/config.R')

config <- read_config(section = 'proxy')

elastic_con <- connect(
  host = config$elastic$host,
  path = "",
  user = config$elastic$username,
  pwd = config$elastic$password,
  port = config$elastic$port,
  transport_schema = "http"
)

attr(elastic_con, 'index') <- config$elastic$index

get_bom_names <- function(con) {
  b <- build_agg_query("BOM", query = build_humgen_query(filters = build_humgen_filters(BOM = NULL)))

  res <- Search(con, index = attr(con, 'index'), body = b, asdf = T)

  parse_elastic_agg(res, b)$BOM
}

get_accounting_names <- function(con, bom, date_range) {
  b <- build_agg_query("ACCOUNTING_NAME", query = build_humgen_query(
    filters = build_humgen_filters(
      BOM = bom,
      date_range = date_range
    )
  ))

  res <- Search(con, index = attr(con, 'index'), body = b, asdf = T)

  parse_elastic_agg(res, b)$accounting_name
}

get_user_names <- function(con, bom, accounting_name, date_range) {
  if (accounting_name %in% c('all', '')) accounting_name <- NULL
  b <- list(
    query = build_humgen_query(
      filters = build_humgen_filters(
        BOM = bom,
        accounting_name = accounting_name,
        date_range = date_range
      )
    )
  )

  res <- httr::POST(
    url = con$make_url(),
    path = 'get_usernames',
    body = b,
    encode = "json"
  )

  httr::stop_for_status(res, task = paste("get list of users for LSF group", accounting_name))

  as.character(httr::content(res))
}

decide_statistics_function <- function (input) {
  if (input$user_name != 'all') {
    return(get_user_statistics)
  }

  if (input$accounting_name != 'all') {
    return(get_team_statistics)
  }

  return(get_bom_statistics)
}

generate_efficiency <- function (input, con, query, adjust, time_bucket) {
  req(input$accounting_name, input$user_name)

  get_statistics <- decide_statistics_function(input)
  get_statistics(con, query = query, time_bucket = time_bucket, adjust = adjust)
}

doc_link <- tags$a(
  shiny::icon("book"), "Docs",
  href = "https://docs.google.com/document/d/1U55kxuEJpvksGG2we_tMhzYeVLXv5YakOGNwH2Rscd8/edit?usp=sharing",
  target = "_blank"
)

ui <- page_navbar(
  title = "HGI Farm Dashboard",
  nav_spacer(),
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
      "period", 
      label = tooltip(
        trigger = list(
          "Period",
          shiny::icon("circle-exclamation")
        ),
        "The dashboard is responsive for periods up to 6 months. Expect longer waiting time for periods over one year."
      ),
      start = Sys.Date() - 30,
      end = Sys.Date(),
      min = as.Date("2014-09-21"),
      max = Sys.Date(),
      weekstart = 1
    ),
    selectInput(
      "time_bucket", "Time Bucket",
      choices = c("none", "day", "week", "month")
    )
  ),
  nav_panel(title = "Dashboard", 
    accordion(
      accordion_panel(
        "Job failure statistics",
        shinycssloaders::withSpinner(
          tagList(
            plotOutput("job_failure"),
            plotOutput("per_bucket_job_failure"),
            DT::DTOutput("per_bucket_job_failure_table"),
            plotOutput("job_failure_time_plot")
          )
        ),
        value = "job_failure_panel"
      ),
      accordion_panel(
        "Unadjusted Efficiency",
        shinycssloaders::withSpinner(
          tagList(
            DT::DTOutput("unadjusted_efficiency"),
            selectInput(
              "unadjusted_efficiency_column", "Column to plot",
              choices = NULL
            ),
            plotOutput("unadjusted_efficiency_plot")
          )
        ),
        value = 'unadjusted_efficiency_panel'
      ),
      accordion_panel(
        "Efficiency",
        shinycssloaders::withSpinner(
          tagList(
            textOutput("adjustments_explanation"),
            DT::DTOutput("efficiency"),
            htmlOutput("awesomeness_formula"),
            selectInput(
              "efficiency_column", "Column to plot",
              choices = NULL
            ),
            plotOutput("efficiency_plot")
          )
        ),
        value = 'efficiency_panel'
      ),
      accordion_panel(
        "Job Breakdown",
        shinycssloaders::withSpinner(
          tagList(
            DT::DTOutput("job_breakdown"),
            selectInput(
              "job_breakdown_column", "Column to plot",
              choices = NULL
            ),
            plotOutput("job_breakdown_plot")
          )
        ),
        value = "job_breakdown_panel"
      ),
      accordion_panel(
        "GPU Statistics",
        shinycssloaders::withSpinner(
          DT::DTOutput("gpu_statistics")
        ),
        value = "gpu_statistics_panel"
      ),
      id = "myaccordion",
      open = FALSE
    )
  ),
  nav_item(doc_link)
)

server <- function(input, output, session) {
  observeEvent(c(input$bom, input$period), {
    req(input$bom, input$period)
    req(all(!isInvalidDate(input$period)))
    req(input$period[1] <= input$period[2])
    accounting_names <- get_accounting_names(elastic_con, input$bom, input$period)
    team_names <- set_team_names(accounting_names, mapping = team_map)

    selected_accounting_name <- isolate(input$accounting_name)
    if (!(selected_accounting_name %in% team_names)) {
      selected_accounting_name <- 'all'
    }

    freezeReactiveValue(input, "accounting_name")

    updateSelectInput(
      inputId = "accounting_name",
      choices = c('all', team_names),
      selected = selected_accounting_name
    )
  }, priority = 2)

  observeEvent(c(input$bom, input$accounting_name, input$period), {
    req(input$bom, input$period)
    req(all(!isInvalidDate(input$period)))
    req(input$period[1] <= input$period[2])
    # if (input$accounting_name == 'all') {
    #    user_names <- c("Select a group" = "")
    # } else {
      user_names <- get_user_names(elastic_con, input$bom, input$accounting_name, input$period)
      if (length(user_names) > 1){
        user_names <- c('all', user_names)
      }
    # }

    selected_user_name <- isolate(input$user_name)
    if ( !is.null(input$user_name) && !(selected_user_name %in% user_names)) {
      selected_user_name <- user_names[1]
    }

    freezeReactiveValue(input, "user_name")

    updateSelectInput(
      inputId = "user_name",
      choices = user_names,
      selected = selected_user_name
    )
  }, priority = 1)

  elastic_query <- reactive({
    req(input$bom, input$accounting_name)
    if (input$accounting_name != 'all'){
      req(input$user_name)
    }
    req(all(!isInvalidDate(input$period)))
    req(input$period[1] <= input$period[2])

    build_humgen_query(
      filters = build_humgen_filters(
        BOM = input$bom,
        accounting_name = input$accounting_name,
        user_name = input$user_name,
        date_range = input$period
      )
    )
  })

  per_bucket_job_failure_df <- reactive({
    if (input$accounting_name == 'all') {
      get_job_failure_statistics(con = elastic_con, query = elastic_query(), fields = c("ACCOUNTING_NAME", "Job"))
    } else {
      req(input$user_name)
      if (input$user_name == 'all') {
        # statistics for only one team
        df <- get_team_statistics(elastic_con, query = elastic_query())

        # transform df
        df <- df %>%
          rename(accounting_name = USER_NAME) %>%
          mutate(Failed = as.integer(number_of_jobs * fail_rate),
                Success = number_of_jobs - Failed) %>%
          tidyr::pivot_longer(cols = c('Success', 'Failed'), names_to = 'job_status', values_to = 'doc_count')

        # we need by this point: c('accounting_name', 'job_status', 'doc_count')
      }
    }
  })

  output$job_failure_time_plot <- renderPlot({
    if(input$time_bucket != "none"){
      df <- get_job_failure_statistics(con = elastic_con, query = elastic_query(), fields = "Job", time_bucket = input$time_bucket)
      make_job_failure_timeplot(df)
    }
  })

  output$job_failure <- renderPlot({
    b <- build_agg_query("Job", query = elastic_query())

    res <- Search(elastic_con, index = attr(elastic_con, 'index'), body = b, asdf = T)

    df <- parse_elastic_agg(res, b) %>%
      mutate_for_piechart()

    piechart(df, count_field = 'doc_count', key_field = 'job_status', legend_name = 'Job status')
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
    dt <- generate_efficiency(input, elastic_con, adjust = FALSE, query = elastic_query(), time_bucket = 'none')
    make_dt(dt, table_view_opts = 'ftp')
  })

  unadjusted_efficiency_timed_table <- reactive({
    generate_efficiency(input, elastic_con, adjust = FALSE, query = elastic_query(), time_bucket = input$time_bucket)
  })

  unadjusted_efficiency_table_colnames <- reactive({
    req('unadjusted_efficiency_panel' %in% input$myaccordion)
    df <- unadjusted_efficiency_timed_table()
    cols <- get_colname_options(df, exclude_columns = c('timestamp', 'accounting_name', 'USER_NAME'))
    names(cols)[grep('cpu_wasted_frac', cols)] <- 'CPU Efficiency'
    names(cols)[grep('mem_wasted_frac', cols)] <- 'Memory Efficiency'
    cols
  }) 

  output$unadjusted_efficiency_plot <- renderPlot({
    if(input$time_bucket != "none")
      generate_efficiency_plot(
        df = unadjusted_efficiency_timed_table(), 
        column_to_plot = input$unadjusted_efficiency_column
      )
  })

  output$adjustments_explanation <- renderText({
    adjustments_explanation
  })

  output$efficiency <- DT::renderDT({
    dt <- generate_efficiency(input, elastic_con, adjust = TRUE, query = elastic_query(), time_bucket = 'none')
    make_dt(dt, table_view_opts = 'ftp')
  })

  efficiency_timed_table <- reactive({
    generate_efficiency(input, elastic_con, adjust = TRUE, query = elastic_query(), time_bucket = input$time_bucket)
  })

  efficiency_table_colnames <- reactive({
    req('efficiency_panel' %in% input$myaccordion)
    df <- efficiency_timed_table()
    cols <- get_colname_options(df, exclude_columns = c('timestamp', 'accounting_name', 'USER_NAME'))
    names(cols)[grep('cpu_wasted_frac', cols)] <- 'CPU Efficiency'
    names(cols)[grep('mem_wasted_frac', cols)] <- 'Memory Efficiency'
    cols
  })

  output$efficiency_plot <- renderPlot({
    if(input$time_bucket != "none") {
      generate_efficiency_plot(
        df = efficiency_timed_table(),
        column_to_plot = input$efficiency_column
      )
    }
  })

  output$awesomeness_formula <- renderUI({
    if (input$accounting_name == 'all') {
      withMathJax(awesomeness_explanation, awesomeness_formula)
    }
  })

  output$job_breakdown <- DT::renderDT({
    if (input$accounting_name != 'all') {
      dt <- get_job_statistics(elastic_con, query = elastic_query())
      make_dt(dt, table_view_opts = 'ftp')
    }
  })

  timed_job_statistics <- reactive({
    if (input$accounting_name != 'all')
      get_job_statistics(elastic_con, time_bucket = input$time_bucket, query = elastic_query())
  })

  timed_job_statistics_colnames <- reactive({
    req('job_breakdown_panel' %in% input$myaccordion)
    df <- timed_job_statistics()
    cols <- get_colname_options(df, exclude_columns = c('date', 'job_type'))
    names(cols)[grep('cpu_wasted_frac', cols)] <- 'CPU Efficiency'
    names(cols)[grep('mem_wasted_frac', cols)] <- 'Memory Efficiency'
    cols
  })

  output$job_breakdown_plot <- renderPlot({
    if(input$time_bucket != "none"){
      df <- timed_job_statistics()
      generate_efficiency_plot(df, column_to_plot = input$job_breakdown_column)
    }
  })

  gpu_records <- reactive({
    get_gpu_records(elastic_con, query = elastic_query())
  })

  output$gpu_statistics <- DT::renderDT({
    if(input$accounting_name != 'all'){
      dt <- generate_gpu_statistics(gpu_records())
      make_dt(dt, table_view_opts = 'ftp')
    }
  })

  gpu_records_colnames <- reactive({
    req('gpu_statistics_panel' %in% input$myaccordion)
    df <- gpu_records()
    get_colname_options(df, exclude_columns = c('timestamp', 'USER_NAME', 'Job', 'QUEUE_NAME', '_id'))
  })

  output$gpu_plot <- renderPlot({
    if(input$time_bucket != "none")
      generate_gpu_plot(
        df = gpu_records(),
        time_bucket = input$time_bucket,
        metric = input$gpu_statistics_column)
  })

  observe({
    if (input$accounting_name != 'all') {
      accordion_panel_update('myaccordion', target = 'gpu_statistics_panel',
        shinycssloaders::withSpinner(
          DT::DTOutput("gpu_statistics")
        ),
        if (input$time_bucket != "none") {
          shinycssloaders::withSpinner(
            tagList(
              selectInput(
                "gpu_statistics_column", "Column to plot",
                choices = gpu_records_colnames(),
                selected = isolate(input$gpu_statistics_column)
              ),
              plotOutput("gpu_plot")
            )
          )
        }
      )
    } else {
      accordion_panel_update('myaccordion', target = 'gpu_statistics_panel',
        "To see GPU statistics please pick a LSF Group in the left panel"
      )
    }
  })

  observe({
    if (input$time_bucket == "none") {
      if (input$accounting_name == 'all' || input$user_name == 'all') {
        accordion_panel_update(id = 'myaccordion', target = 'job_failure_panel',
          shinycssloaders::withSpinner(
            tagList(
              plotOutput("per_bucket_job_failure"),
              DT::DTOutput("per_bucket_job_failure_table")
            )
          )
        ) 
      } else {
        accordion_panel_update(id = 'myaccordion', target = 'job_failure_panel',
          shinycssloaders::withSpinner(
            plotOutput("job_failure")
          )
        ) 
      }
    } else {
      if (input$accounting_name == 'all' || input$user_name == 'all') {
        accordion_panel_update(id = 'myaccordion', target = 'job_failure_panel',
          shinycssloaders::withSpinner(
            tagList(
              plotOutput("per_bucket_job_failure"),
              DT::DTOutput("per_bucket_job_failure_table")
            )
          ),
          shinycssloaders::withSpinner(
            plotOutput("job_failure_time_plot")
          )
        ) 
      } else {
        accordion_panel_update('myaccordion', target = 'job_failure_panel',
          shinycssloaders::withSpinner(
            plotOutput("job_failure")
          ),
          shinycssloaders::withSpinner(
            plotOutput("job_failure_time_plot")
          )
        ) 
      }
    }
  })

  observe({
    if (input$time_bucket == "none") {
      accordion_panel_update('myaccordion', target = 'unadjusted_efficiency_panel',
        shinycssloaders::withSpinner(
          DT::DTOutput("unadjusted_efficiency")
        )
      )
    } else {
      accordion_panel_update('myaccordion', target = 'unadjusted_efficiency_panel',
        shinycssloaders::withSpinner(
          DT::DTOutput("unadjusted_efficiency")
        ),
        shinycssloaders::withSpinner(
          tagList(
            selectInput(
              "unadjusted_efficiency_column", "Column to plot",
              choices = unadjusted_efficiency_table_colnames(),
              selected = isolate(input$unadjusted_efficiency_column)
            ),
            plotOutput("unadjusted_efficiency_plot")
          )
        )
      )
    }
  })

  observe({
    if (input$time_bucket == "none") {
      accordion_panel_update('myaccordion', target = 'efficiency_panel',
        shinycssloaders::withSpinner(
          tagList(
            DT::DTOutput("efficiency"),
            htmlOutput("awesomeness_formula")
          )
        )
      )
    } else {
      accordion_panel_update('myaccordion', target = 'efficiency_panel',
        shinycssloaders::withSpinner(
          tagList(
            DT::DTOutput("efficiency"),
            htmlOutput("awesomeness_formula")
          )
        ),
        shinycssloaders::withSpinner(
          tagList(
            selectInput(
              "efficiency_column", "Column to plot",
              choices = efficiency_table_colnames(),
              selected = isolate(input$efficiency_column)
            ),
            plotOutput("efficiency_plot")
          )
        )
      )
    }
  })

  observe({
    if (input$accounting_name != 'all') {
      accordion_panel_update('myaccordion', target = 'job_breakdown_panel',
        shinycssloaders::withSpinner(
          DT::DTOutput("job_breakdown")
        ),
        if (input$time_bucket != "none") {
          shinycssloaders::withSpinner(
            tagList(
              selectInput(
                "job_breakdown_column", "Column to plot",
                choices = timed_job_statistics_colnames(),
                selected = isolate(input$job_breakdown_column)
              ),
              plotOutput("job_breakdown_plot")
            )
          ) 
        }
      )
    } else {
      accordion_panel_update('myaccordion', target = 'job_breakdown_panel',
        "To see job breakdown statistics please pick a LSF Group in the left panel"
      )
    }
  })

  observe({
    if(any(isInvalidDate(input$period))) {
      showNotification("Please enter a valid date", type = "error")
    } else {
      if(input$period[2] < input$period[1]) {
        updateDateRangeInput(
          inputId = "period",
          end = input$period[1]
        )
      }
    }
  })
}

shinyApp(ui = ui, server = server)
