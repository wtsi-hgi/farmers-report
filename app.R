library(shiny)
library(dplyr)
loadNamespace('shinycssloaders')
loadNamespace('elastic')

source('src/ui.R')
source('src/table_helpers.R')
source('src/elastic_helpers.R')
source('src/plot_helpers.R')
source('src/stat_helpers.R')
source('src/constants.R')
source('src/config.R')
source('src/logging.R')
source('src/nextflow_helpers.R')
source('src/app_helpers.R')

config <- read_config(section = 'proxy')

elastic_con <- elastic::connect(
  host = config$elastic$host,
  path = "",
  user = config$elastic$username,
  pwd = config$elastic$password,
  port = config$elastic$port,
  transport_schema = "http"
)

attr(elastic_con, 'index') <- config$elastic$index

generate_efficiency <- function (input, con, query, adjust, time_bucket) {
  req(input$accounting_name, input$user_name)

  get_statistics <- decide_statistics_function(input$user_name, input$accounting_name)
  get_statistics(con, query = query, time_bucket = time_bucket, adjust = adjust)
}

server <- function(input, output, session) {
  # set bom names
  observe({
    req(all(!isInvalidDate(input$period)))
    req(input$period[1] <= input$period[2])
    bom_names <- get_bom_names(elastic_con, input$period)
    selected_bom_name <- isolate(input$bom)
    freezeReactiveValue(input, "bom")
    updateSelectInput(
      inputId = "bom",
      choices = bom_names,
      selected = selected_bom_name
    )
  }, priority = 3)

  # set accounting names
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

  # set pipeline name
  observe({
    if (input$nav == 'Nextflow Report'){
      if (input$user_name == "" || input$accounting_name == "") {
        choices <- c("Select both User and Group" = "")
      }
      else {
        query <- elastic_query()  # to evaluate reqs before spinner
        shinycssloaders::showPageSpinner()
        job_name_parts <- get_nf_job_names_parts(df = get_nf_records(elastic_con, query))
        if (length(job_name_parts) >= 1) {
          choices <- c("Select pipeline name..." = "", job_name_parts)
        } else {
          choices <- c("No pipelines found" = "")
        }
      }
      freezeReactiveValue(input, "pipeline_name")
      updateSelectInput(
        inputId = "pipeline_name",
        choices = choices
      )
      shinycssloaders::hidePageSpinner()
    }
  })

  # set user names
  observeEvent(c(input$bom, input$accounting_name, input$period), {
    req(input$bom, input$accounting_name, input$period)
    req(all(!isInvalidDate(input$period)))
    req(input$period[1] <= input$period[2])
    user_names <- get_user_names(elastic_con, input$bom, input$accounting_name, input$period)
    if (length(user_names) > 1){
        user_names <- c('all', user_names)
      }

    selected_user_name <- isolate(input$user_name)
    if ( !is.null(input$user_name) && !(selected_user_name %in% user_names)) {
      selected_user_name <- user_names[1]
    }

    freezeReactiveValue(input, "user_name")

    updateSelectInput(
      inputId = "user_name",
      choices = user_names,
      selected = "al37"
    )
  }, priority = 1)

  elastic_query <- reactive({
    req(input$bom, input$accounting_name, input$user_name)
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

    res <- elastic_search(elastic_con, index = attr(elastic_con, 'index'), body = b, asdf = T)

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
    if (input$accounting_name == 'all' && input$user_name == 'all') {
      withMathJax(awesomeness_explanation, awesomeness_formula)
    }
  })

  output$job_breakdown <- DT::renderDT({
    if (input$accounting_name != 'all' || input$user_name != 'all') {
      dt <- get_job_statistics(elastic_con, query = elastic_query())
      make_dt(dt, table_view_opts = 'ftp')
    }
  })

  timed_job_statistics <- reactive({
    if (input$accounting_name != 'all' || input$user_name != 'all') {
      get_job_statistics(elastic_con, time_bucket = input$time_bucket, query = elastic_query())
    }
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
    if (input$accounting_name != 'all' || input$user_name != 'all'){
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

  pipeline_records <- reactive({
    req(input$pipeline_name)
    get_pipeline_records(elastic_con, query = elastic_query(), pipeline_name = input$pipeline_name)
  })

  output$nextflow_step_freq <- DT::renderDT({
    df <- generate_nextflow_step_freq(pipeline_records())
    make_dt(df, table_view_opts = 'ftp')
  })

  output$nextflow_cpu_efficiency <- DT::renderDT({
    df <- generate_nextflow_cpu_efficiency(pipeline_records())
    make_dt(df, table_view_opts = 'ftp')
  })

  observe({
    req(input$nav == "Nextflow Report")
    if (input$user_name == "" || input$accounting_name == "") {
      choices <- c("Select both User and Group" = "")
    } else if(input$pipeline_name == "") {
      choices <- c("Select pipeline name" = "")
    } else {
      steps <- pipeline_records() %>% group_by(step) %>% tally() %>% filter(n > 1) %>% pull(step)
      choices <- c("Select steps to plot..." = "", setNames(steps, add_zero_length_space(steps)))
    }
      
    freezeReactiveValue(input, "nextflow_cpu_plots")    
    updateSelectInput(
      inputId = "nextflow_cpu_plots",
      choices = choices
    )
  })

  output$nextflow_cpu_efficiency_plot <- renderPlot({
    req(input$nextflow_cpu_plots)
    generate_nextflow_cpu_plots(
      df = pipeline_records(),
      steps = input$nextflow_cpu_plots
    )
  }, height = function () {
    get_height_of_facet_plot(n = length(input$nextflow_cpu_plots))
  })

  output$nextflow_mem_efficiency <- DT::renderDT({
    df <- generate_nextflow_mem_efficiency(pipeline_records())
    make_dt(df, table_view_opts = 'ftp')
  })

  output$nextflow_mem_efficiency_plot <- renderPlot({
    req(input$nextflow_cpu_plots)
    generate_nextflow_mem_plots(
      df = pipeline_records(),
      steps = input$nextflow_cpu_plots
    )
  }, height = function () {
    get_height_of_facet_plot(n = length(input$nextflow_cpu_plots))
  })

  observe({
    if (input$accounting_name != 'all' || input$user_name != 'all') {
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
        "To see GPU statistics please pick a LSF Group or a user in the left panel"
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
    if (input$accounting_name != 'all' || input$user_name != 'all') {
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
        "To see job breakdown statistics please pick a LSF Group or a user in the left panel"
      )
    }
  })

  # correct date
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

  observe({
    log_action(input$myaccordion, input$bom, input$accounting_name, input$user_name, input$period, input$time_bucket)
  })
}

shinyApp(ui = ui, server = server)
