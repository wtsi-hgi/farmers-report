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
            plotOutput("job_breakdown_plot"),
            gt::gt_output("debug_text")
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
