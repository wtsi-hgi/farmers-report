library(shiny)
library(bslib)
loadNamespace('DT')
loadNamespace('shinycssloaders')

doc_link <- tags$a(
  shiny::icon("book"), "Docs",
  href = "https://docs.google.com/document/d/1U55kxuEJpvksGG2we_tMhzYeVLXv5YakOGNwH2Rscd8/edit?usp=sharing",
  target = "_blank"
)

ui <- page_navbar(
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
      )
    )
  ),
  # nav_panel(title = "Dashboard",
  #   accordion(
  #     accordion_panel(
  #       "Job failure statistics",
  #       shinycssloaders::withSpinner(
  #         tagList(
  #           plotOutput("job_failure"),
  #           plotOutput("per_bucket_job_failure"),
  #           DT::DTOutput("per_bucket_job_failure_table"),
  #           plotOutput("job_failure_time_plot")
  #         )
  #       ),
  #       value = "job_failure_panel"
  #     ),
  #     accordion_panel(
  #       "Unadjusted Efficiency",
  #       shinycssloaders::withSpinner(
  #         tagList(
  #           DT::DTOutput("unadjusted_efficiency"),
  #           selectInput(
  #             "unadjusted_efficiency_column", "Column to plot",
  #             choices = NULL
  #           ),
  #           plotOutput("unadjusted_efficiency_plot")
  #         )
  #       ),
  #       value = 'unadjusted_efficiency_panel'
  #     ),
  #     accordion_panel(
  #       "Efficiency",
  #       shinycssloaders::withSpinner(
  #         tagList(
  #           textOutput("adjustments_explanation"),
  #           DT::DTOutput("efficiency"),
  #           htmlOutput("awesomeness_formula"),
  #           selectInput(
  #             "efficiency_column", "Column to plot",
  #             choices = NULL
  #           ),
  #           plotOutput("efficiency_plot")
  #         )
  #       ),
  #       value = 'efficiency_panel'
  #     ),
  #     accordion_panel(
  #       "Job Breakdown",
  #       shinycssloaders::withSpinner(
  #         tagList(
  #           DT::DTOutput("job_breakdown"),
  #           selectInput(
  #             "job_breakdown_column", "Column to plot",
  #             choices = NULL
  #           ),
  #           plotOutput("job_breakdown_plot")
  #         )
  #       ),
  #       value = "job_breakdown_panel"
  #     ),
  #     accordion_panel(
  #       "GPU Statistics",
  #       shinycssloaders::withSpinner(
  #         DT::DTOutput("gpu_statistics")
  #       ),
  #       value = "gpu_statistics_panel"
  #     ),
  #     id = "myaccordion",
  #     open = FALSE
  #   )
  # ),
  nav_panel(title = 'Nextflow Report',
    selectInput("pipeline_name", "Nextflow Pipeline", choices = c("Loading pipeline names..." = "")),
    # actionButton("submit_button", "Submit"),
    shinycssloaders::withSpinner(
      tagList(
        # --- Step frequency ---
        h3("Step frequency"),
        p(paste("This table shows the frequency of each pipeline step.",
                "This should help you decide which step should be optimised first.")),
        DT::DTOutput("nextflow_step_freq"),

        # --- CPU Efficiency ---
        h3("CPU Efficiency"),
        p("This table shows the CPU efficiency of each pipeline step."),
        DT::DTOutput("nextflow_cpu_efficiency"),
        p("This plot shows the CPU efficiency of each pipeline step."),
        selectInput('nextflow_cpu_plots', 'Steps to plot', 
          choices = c("Select pipeline name..." = ""),
          multiple = TRUE
        ),
        plotOutput("nextflow_cpu_efficiency_plot")

      )
    )
  ),
  nav_item(doc_link)
)
