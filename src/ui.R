library(shiny)
library(bslib)
loadNamespace('DT')
loadNamespace('shinycssloaders')
loadNamespace('shinyjs')

source("src/constants.R")

doc_link <- tags$a(
  shiny::icon("book"), "Docs",
  href = "https://docs.google.com/document/d/1U55kxuEJpvksGG2we_tMhzYeVLXv5YakOGNwH2Rscd8/edit?usp=sharing",
  target = "_blank"
)

ui <- tagList(
  shinyjs::useShinyjs(),
  page_navbar(
    title = "HGI Farm Dashboard",
    id = 'nav',
    nav_spacer(),
    sidebar = sidebar(
      width = 270,
      selectInput(
        "bom", "BOM",
        choices = c("Human Genetics")
      ),
      selectInput(
        "accounting_name", "LSF Group",
        choices = c('Loading LSF groups...' = '')
      ),
      selectInput(
        "user_name", "User",
        choices = c('Loading user names...' = '')
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
      conditionalPanel(
        "input.nav === 'Dashboard'",
        selectInput(
          "time_bucket", "Time Bucket",
          choices = c("none", "day", "week", "month")
        ),
        card(
          card_title("Adjustments"),
          input_switch(
            "adjust_cpu",
            label = tooltip(
              trigger = list("1-CPU processes", shiny::icon("question-circle")),
              adjustments_explanation
            ),
            value = TRUE
          )
        )
      ),
      conditionalPanel(
        "input.nav === 'Nextflow Report'",
        selectInput(
          "pipeline_name",
          "Nextflow Pipeline",
          choices = c("Loading pipeline names..." = "")
        ),
        selectizeInput(
          'nextflow_cpu_plots',
          'Steps to plot',
          choices = c("Select pipeline name..." = ""),
          multiple = TRUE,
          width = '100%',
          options = list(
            searchField = "value"
          )
        )
      )

    ),
    nav_panel(title = "Dashboard",
      accordion(
        id = "myaccordion",
        open = FALSE,

        accordion_panel(
          "Job failure statistics",
          shinycssloaders::withSpinner(
              plotOutput("job_failure"),
          ),
          shinycssloaders::withSpinner(
            tagList(
              plotOutput("per_bucket_job_failure"),
              DT::DTOutput("per_bucket_job_failure_table"),
            )
          ),
          shinycssloaders::withSpinner(
              plotOutput("job_failure_time_plot")
          ),
          value = "job_failure_panel"
        ),

        accordion_panel(
          "Efficiency",
          shinycssloaders::withSpinner(
            DT::DTOutput("efficiency")
          ),
          htmlOutput("awesomeness_formula"),
          selectInput(
            "efficiency_column", "Column to plot",
            choices = c("Loading..." = "")
          ),
          shinycssloaders::withSpinner(
            plotOutput("efficiency_plot")
          ),
          value = 'efficiency_panel'
        ),

        accordion_panel(
          "Job Breakdown",
          p(id = "job_breakdown_placeholder", "To see job breakdown statistics please pick a LSF Group or a user in the left panel"),
          p(id = "job_breakdown_help", shiny::icon("question-circle"), "Click on a job type name to see examples of those jobs"),
          input_switch(
            "adjust_interactive",
            label = tooltip(
              trigger = list("adjust interactive jobs", shiny::icon("question-circle")),
              "Asume that interactive jobs are always successful"
            ),
            value = FALSE
          ),
          shinycssloaders::withSpinner(
            DT::DTOutput("job_breakdown")
          ),
          selectInput(
            "job_breakdown_column", "Column to plot",
            choices = c("Loading..." = "")
          ),
          shinycssloaders::withSpinner(
            plotOutput("job_breakdown_plot")
          ),
          value = "job_breakdown_panel"
        ),

        accordion_panel(
          "Command Statistics",
          layout_column_wrap(
            textInput(
              "command_pattern",
              "Type command substring to see statistics for all jobs containing this substring",
              placeholder = "python my_script.py"
            ),
            value_box(
              title = "Number of records",
              value = textOutput("command_pattern_count"),
              showcase = shiny::icon("magnifying-glass"),
              p("(successfull / total)")
            )
          ),
          layout_columns(
            shinycssloaders::withSpinner(plotOutput("command_cpu_efficiency_plot")),
            shinycssloaders::withSpinner(plotOutput("command_mem_efficiency_plot"))
          )
        ),

        accordion_panel(
          "GPU Statistics",
          shinycssloaders::withSpinner(
            DT::DTOutput("gpu_statistics")
          ),
          selectInput(
            "gpu_statistics_column", "Column to plot",
            choices = c("Loading..." = "")
          ),
          shinycssloaders::withSpinner(
            plotOutput("gpu_plot")
          ),
          value = "gpu_statistics_panel"
        )

      )
    ),
    nav_panel(title = 'Nextflow Report',

      card(
        card_header("Step frequency", style="font-size: 24px;"),
        p(paste("This table shows the frequency of each pipeline step.",
                "This should help you decide which step should be optimised first.")),
        shinycssloaders::withSpinner(
          DT::DTOutput("nextflow_step_freq")
        ),
        min_height = "43rem"
      ),

      card(
        card_header("CPU Efficiency", style="font-size: 24px;"),
        p("This table shows the CPU efficiency of each pipeline step."),
        shinycssloaders::withSpinner(
          DT::DTOutput("nextflow_cpu_efficiency")
        ),
        min_height = "43rem"
      ),

      card(
        textOutput("nextflow_cpu_efficiency_description"),
        shinycssloaders::withSpinner(
            plotOutput("nextflow_cpu_efficiency_plot")
        ),
        min_height = "31rem"
      ),

      card(
        card_header("RAM Efficiency", style="font-size: 24px;"),
        p("This table shows the RAM efficiency of each pipeline step."),
        shinycssloaders::withSpinner(
          DT::DTOutput("nextflow_mem_efficiency")
        ),
        min_height = "43rem"
      ),

      card(
        textOutput("nextflow_mem_efficiency_description"),
        shinycssloaders::withSpinner(
          plotOutput("nextflow_mem_efficiency_plot")
        ),
        min_height = "31rem"
      )

    ),
    nav_item(doc_link)
  )
)
