library(shiny)
library(shinythemes)
library(shinyWidgets)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("cosmo"),

  navbarPage(
    "CUSUM",
    # Standard CUSUM Chart ----
    tabPanel(
      "Standard CUSUM Chart",

      fluidRow(
        column(
          2,
          inputPanel(
            radioButtons("data_source", "Choose Data",
              choices = c(
                "Example data" = 1,
                "User data" = 2
              ),
              selected = 1
            )
          ),
          inputPanel(
            h4(tags$b("Baseline Failure Probability")),
            
            sliderInput("failure_prob",
              label = "",
              min = 0,
              max = 50,
              value = 10,
              step = 0.01,
              post = "%"
            ),
            
            h4(tags$b("Detection Level")),

            sliderInput("detection_level_d",
              label = "Deterioriation",
              min = 1,
              max = 4,
              value = 2,
              step = .1
            ),
            
            sliderInput("detection_level_i",
                        label = "Improvement",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = .1
            ),

            materialSwitch("reset",
              label = "Reset CUSUM after a signal",
              value = FALSE
            )
          ),

          inputPanel(
            h4(tags$b("Fixed Parameter")),
            radioButtons("fix_term",
              label = "",
              choices = list(
                "False Signal Probability (FSP)" = 1,
                "Control Limit (CL)" = 2
              )
            ),

            conditionalPanel(
              condition = "input.fix_term == 1",
              sliderInput("false_signal",
                label = "",
                min = 0, max = 100,
                value = 5,
                post = "%",
                step = .01
              )
            ),
            conditionalPanel(
              condition = "input.fix_term == 2",
              sliderInput("control_limit_d",
                label = "Deterioriation",
                min = 0, max = 10,
                value = 2,
                step = .01
              ),
              sliderInput("control_limit_i",
                          label = "Improvement",
                          min = -10, max = 0,
                          value = -2,
                          step = .01
              )
            ),


            # tags$div(title="Click here to slide through years",
            #
            #          ),
            htmlOutput("other_fixed_term"),

            tags$br(),
            dropdownButton(sliderInput("n_sim",
              label = "Number of Simulations",
              min = 1000,
              max = 10000,
              value = 1000,
              step = 1000
            ),
            circle = FALSE,
            width = "300px",
            status = "secondary",
            icon = icon("gear"),
            size = "sm",
            tooltip = tooltipOptions(title = "Click to change number of simulations")
            )
          )
        ),

        column(
          10,
          plotOutput(outputId = "distPlot"),

          h4(tags$u("Signals")),

          tableOutput("signals")
        )
      ),
      fluidRow(
        column(2,
          offset = 10,
          downloadButton("downloadData", "Download CUSUM data")
        )
      )
    ),
    # Risk-adjusted CUSUM Chart ----
    tabPanel(
      "Risk-adjusted CUSUM Chart",
      fluidRow(
        column(
          2,
          inputPanel(
            radioButtons("data_source_ra", "Choose Data",
              choices = c(
                "Example data" = 1,
                "User data" = 2
              ),
              selected = 1
            )
          ),
          inputPanel(
            sliderInput("detection_level_ra",
              label = "Detection Level",
              min = 1,
              max = 4,
              step = .1,
              value = 2
            ),
            materialSwitch("reset_ra",
              label = "Reset RA-CUSUM after a signal",
              value = FALSE
            )
          ),
          inputPanel(
            selectInput("cl_method",
              label = "Control Limit Method",
              choices = list("constant" = 1, "dynamic" = 2),
              selected = 1
            ),

            conditionalPanel(
              "input.cl_method == 1",

              radioButtons("fixed_term_ra",
                label = "Fixed Parameter",
                choices = list(
                  "False Signal Probability (FSP)" = 1,
                  "Control Limit (CL)" = 2
                )
              ),
              conditionalPanel(
                "input.fixed_term_ra == 1",
                sliderInput("false_signal_ra",
                  label = "",
                  min = 0, max = 100,
                  value = 5,
                  post = "%",
                  step = .01
                )
              ),
              conditionalPanel(
                "input.fixed_term_ra == 2",
                sliderInput("control_limit_ra",
                  label = "",
                  min = 1, max = 10,
                  value = 2,
                  step = .01
                )
              )
            ),
            conditionalPanel(
              "input.cl_method == 2",
              sliderInput("false_signal_ra",
                          label = "",
                          min = 0, max = 100,
                          value = 5,
                          post = "%",
                          step = .01)
              ),
            dropdownButton(
              sliderInput("n_sim_ra",
                          label = "Number of Simulations",
                          min = 1000,
                          max = 10000,
                          value = 1000,
                          step = 1000
                          ),
              circle = FALSE,
              width = "300px",
              status = "secondary",
              icon = icon("gear"),
              size = "sm",
              tooltip = tooltipOptions(title = "Click to change number of simulations")
              )
            )
          ),
        column(
          10,
          plotOutput("plot_ra")
          )
        ),
      fluidRow(
        column(2,
          offset = 10,
          downloadButton("downloadData_ra", "Download RA-CUSUM data")
        )
      )
    ),

    # Upload Data ----

    tabPanel(
      p(icon("file-upload"), "Upload data"),
      fluidRow(
        column(
          2,
          inputPanel(
            fileInput("user_data", 
                      "Upload CSV File",
                      multiple = FALSE, 
                      accept = NULL,
                      width = NULL, 
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"
                      ),
            
            checkboxInput("header", "Header", TRUE),

            radioButtons("sep", 
                         "Separator",
                         choices = c(
                           Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"
                           ),
                         selected = ","
                         ),

            radioButtons("quote", 
                         "Quote",
                         choices = c(
                           None = "",
                           "Double Quote" = '"',
                           "Single Quote" = "'"
                           ),
                         selected = '"'
                         )
            ),
          inputPanel(
            h4(tags$b("Variable Names:")),
            
            textInput("outcome_var",
                      label = "Patient Outcomes",
                      value = "y",
                      placeholder = "patient_outcomes"
                      ),
            
            textInput("risk_var",
                      label = "Patient Risks (for risk-adjustment)",
                      #value = "score",
                      placeholder = "patient_risks"
                      )
            )
          ),
        column(9, 
               verbatimTextOutput("contents"))
        )
      ),

    # Help ----

    navbarMenu(
      "Help",
      tabPanel("Summary"),
      "----",
      "Section header",
      tabPanel("Table")
    )
  )
)
