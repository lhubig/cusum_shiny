library(shiny)
library(shinythemes)
library(shinyWidgets)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-message { visibility: hidden; }",
             ".shiny-output-message:before { visibility: hidden; }"
  ),
  
  theme = shinytheme("cosmo"),

  navbarPage(
    "CUSUM",
    # Standard CUSUM Chart ----
    tabPanel(
      "Standard CUSUM",

      fluidRow(
        column(
          2,
          inputPanel(
            tags$div(title="Click to select data source",
                     radioButtons("data_source", "Select Data",
                                  choices = c("Example data" = 1,
                                              "User data" = 2
                                              ),
                                  selected = 1
                                  )
                     )
            ),
          inputPanel(
            h4(tags$b("Baseline Failure Probability")),
            
            tags$div(title="Click here to set failure probability",
                     sliderInput("failure_prob",
                                 label = "",
                                 min = 0,
                                 max = 50,
                                 value = 10,
                                 step = 0.01,
                                 post = "%")
                     ),
            
            h4(tags$b("Detection Level")),

            tags$div(title="Click here to set detection level (odds multiplier) to detect process deteriorations",
                     sliderInput("detection_level_d",
                                 label = "Deterioration",
                                 min = 1,
                                 max = 4,
                                 value = 2,
                                 step = .1)
                     ),
            
            tags$div(title="Click here to set detection level (odds multiplier) to detect process improvements", 
                     sliderInput("detection_level_i",
                                 label = "Improvement",
                                 min = 0,
                                 max = 1,
                                 value = 0.5,
                                 step = .1)
                     ),

            tags$div(title="Click here to reset the CUSUM to zero when it signaled", 
                     materialSwitch("reset",
                                    label = "Reset CUSUM",
                                    value = FALSE)
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
                min = 0, max = 50,
                value = 5,
                post = "%",
                step = .01
              )
            ),
            conditionalPanel(
              condition = "input.fix_term == 2",
              sliderInput("control_limit_d",
                label = "Deterioration ",
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


            # 
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

        column(9,
               h4(tags$b("Chart")),
               plotOutput(outputId = "distPlot"),
               h4(tags$b("Signals")),
               column(2,
                      h5(tags$b("Deterioration ")),
                      tableOutput("signals") ),
               column(2,
                      h5(tags$b("Improvement")),
                      tableOutput("signals_i") )
        #        ),
        # column(1,
        #        dropdownButton(
        #          circle = TRUE,
        #          width = "300px",
        #          #status = "info",
        #          icon = icon("question"),
        #          h4("Set control limit")
        #          )
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
      "Risk-adjusted CUSUM",
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
            h4(tags$b("Detection Level")),
            
            sliderInput("detection_level_ra",
                        label = "Deterioration ",
                        min = 1,
                        max = 4,
                        step = .1,
                        value = 2),
            
            sliderInput("detection_level_ra_i",
                        label = "Improvement",
                        min = 0,
                        max = 1,
                        step = .1,
                        value = .5)
            ),
          inputPanel(
            materialSwitch("reset_ra",
                           label = "Reset RA-CUSUM after a signal",
                           value = FALSE
                           )
            ),
          inputPanel(
            h4(tags$b("Fixed Parameter")),
            
              radioButtons("fixed_term_ra",
                label = "",
                choices = list(
                  "False Signal Probability (FSP)" = 1,
                  "Control Limit (CL)" = 2
                )
              ),
              conditionalPanel(
                "input.fixed_term_ra == 1",
                sliderInput("false_signal_ra",
                  label = "",
                  min = 0, max = 50,
                  value = 5,
                  post = "%",
                  step = .01
                )
              ),
              conditionalPanel(
                "input.fixed_term_ra == 2",
                sliderInput("control_limit_d_ra",
                  label = "Deterioration ",
                  min = 1, max = 10,
                  value = 2,
                  step = .01
                ),
                sliderInput("control_limit_i_ra",
                            label = "Improvement",
                            min = -10, max = 0,
                            value = -2,
                            step = .01
                )
              # )
            ),
            htmlOutput("other_fixed_term_ra"),
            
            # conditionalPanel(
            #   "input.cl_method == 2",
            #   sliderInput("false_signal_ra",
            #               label = "",
            #               min = 0, max = 100,
            #               value = 5,
            #               post = "%",
            #               step = .01)
            #   ),
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
          h4(tags$b("Chart")),
          plotOutput("plot_ra"),
          h4(tags$b("Signals")),
          column(2,
                 h5(tags$b("Deterioration ")),
                 tableOutput("signals_ra") ),
    
          column(2,
                 h5(tags$b("Improvement")),
                 tableOutput("signals_i_ra"))
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
                         ),
            numericInput("head", 
                         label = "Show number of rows", 
                         value = 10)
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

    tabPanel("Background",
             column(2, 
                    h3(tags$b("About")),
                    p("This app lets you calculate CUSUM chart with the help of the",
                      tags$a(href = "https://github.com/lhubig/cusum" , "cusum"), "package. All functions used for this app are available in the cusum package."),
                    p("Created by Lena Hubig"),
                    p("Last updated: 10/09/2019")
                    ),
             column(8,
                    
                    tabsetPanel(
                    tabPanel("CUSUM Charts",
                             tags$br(),
                             withMathJax(),
                            
                             helpText("CUSUM charts monitor a process for any performance change. The tabular CUSUM consists of two distinct CUSUM runs. 
                               The CUSUM run detecting performance", tags$b("deteriorations"), "is restricted to non-negative values and the CUSUM run detecting performance ", tags$b("improvements") ,"to non-positive."),
                             helpText("For signaling process deterioration: \\( {C_t}^d = \\text{max}(0, C_{t-1} + W_t),\\; t = 1,2,3, \\dots\\)"),
                             helpText("For signaling process improvements: \\( {C_t}^i = \\text{min}(0, C_{t-1} - W_t),\\; t = 1,2,3, \\dots\\)"),
                             
                             helpText("In the simplest, non-risk-adjusted case ", tags$b("(Standard CUSUM)"), " every subjects has the same risk of failure, calculated as the log-likelihood:"),
                             helpText("\\( W_t =\\text{log}\\left(\\frac{c_A}{c_0}\\right)\\) if the person had an adverse event/negative outcome; 
                                       \\(W_t = \\text{log}\\left(\\frac{1-c_A}{1-c_0}\\right) \\) if the person had no adverse event/positive outcome."),
                             helpText("Here, \\(c_0\\) is the baseline failure probability and \\(c_A\\) the lowest failure probability that is detectable by the CUSUM chart."),
                        
                             
                             helpText("If risk-adjustment is available, i.e. patient-individual risk scores, the risk-adjusted CUSUM", tags$b("(RA-CUSUM)"), "can be calculated. 
                               Instead of specifying a global baseline failure probability, each patient has its individual risk probability that results in individual CUSUM weights."),
                             helpText("\\( W_t = \\text{log}\\left(\\frac{R_A}{1-p_t + R_A p_t}\\right) \\) if the person had an adverse event/negative outcome.
                                      \\(W_t = \\text{log}\\left(\\frac{1}{1-p_t + R_A p_t}\\right)\\) if the person had no adverse event/positive outcome."),
                             
                             helpText("The CUSUM signals a process change if the CUSUM statistic \\(C_t\\) crosses than the control limit.")
                             ),
                    tabPanel("Customize CUSUM Chart",
                             tags$br(),
                             
                             h4(tags$b("Baseline Failure Probability")),
                             "Specifies the acceptable baseline probability. This is the estimated failure probability that reflects acceptable performance. 
                             This parameter must be specified for each monitored process, either by previous experience of standard performance, or an acceptable reference value.",
                             h4(tags$b("Detection Level")),
                             "Specifies the detectable change in odds. 
                             Values larger than 1 detect process deteriorations and values between 0 and 1 process improvements. 
                             A detection level of 2 signals a doubling of odds for failure, while a detection level of 0.5 detects a halving of odds. 
                             These are the most intuitive values of detection levels, and a good choice for standard process monitoring.
                             ",
                             h4(tags$b("Reset CUSUM")),
                             "After a signal (and its successful intervention), the CUSUM can be restarted by resetting the CUSUM to 0.",
                             h4(tags$b("Fixed Parameter")),
                             p("Specifies either ", tags$b("Control Limits (CL)"), "or the ", tags$b("False Signal Probability (FSP)"), ", which are dependent on each other. 
                             CLs signal a performance change, wider CLs result in fewer signals and a lower FSP, tighter CLs in more signals. 
                               The FSP is the type 1 error rate of this CUSUM chart, the probability of a signal when the process is truly in control. 
                               It makes more sense to set the FSP and estimate the corresponding CL, and use the fixed CL setting only when evaluating existing CUSUM charts."),
                             p("For a more exact simulation, the number of simulations can be changed in the gearbox setting. Be aware that large simulations may take awhile to load.")
                             ),
                    tabPanel("Interpret Results",
                             tags$br(),
                             
                             h4(tags$b("CUSUM Chart")),
                             p("The CUSUM statistic is plotted in a CUSUM chart, that displays the CUSUM statistic (Y-axis) for every observation in their sequence (X-axis). 
                               The upper CUSUM run (C_t >= 0) detects deterioration, and the lower CUSUM run (C[t] <= 0) detects improvements."), 
                             p("Crossing a control limit (blue horizontal line) triggers a signal (orange cross for deterioration and green cross for improvements)."),
                             h4(tags$b("CUSUM Signals")),
                             p("The observations that trigger a new signal are returned below the CUSUM chart. If the previous observation is already outside the control limit, the observation is not returned in the table."),
                             p("These observations can be used as a starting point for investigations. 
                               A process change likely occurred around these observations. 
                               Notice that previous adverse events should also be investigated, as they all built up the CUSUM and triggered the signal.")
                             ),
                    
                    # tabPanel("Risk-adjusted CUSUM",
                    #          h4(tags$b("Description"),
                    #             "Risk-adjusted CUSUM charts can be used when patient-individual risks are available. Risk of failure can vary greatly between patients and "),
                    #          tags$hr(),
                    #          
                    #          h4(tags$b("Customize RA-CUSUM Chart")),
                    #          h5(tags$b("Detection Level")),
                    #          "Specifies the detectable change in odds. Values larger than 1 detect process deteriorations and values between 0 and 1 process improvements. E.g. a detection level of 2 signals a doubling of odds for failure, while a detection level of 0.5 detects a halving of odds. 
                    #          ",
                    #          h5(tags$b("Reset RA-CUSUM")),
                    #          "After a signal (and its successful intervention), the RA-CUSUM can be restarted by resetting the current statistic to 0.",
                    #          
                    #          h5(tags$b("Fixed Parameter")),
                    #          p("Specifies either ", tags$b("Control Limits (CL)"), "or the ", tags$b("False Signal Probability (FSP)"), ", which are dependent on each other. 
                    #          CLs signal a performance change, wider CLs result in fewer signals and a lower FSP, and tighter CLs in more signals. 
                    #            The FSP is the type 1 error rate of this RA-CUSUM chart, the probability of a signal when the process is truly in control."),
                    #          p("The corresponding parameter is returned."),
                    #          p("For a more exact simulation, the number of simulations can be changed by clicking on the gearbox-button."),
                    #          
                    #          tags$hr(),
                    #          
                    #          h4(tags$b("Interpret Results"))),
                    
                    tabPanel("User Data",
                             tags$br(),
                             
                             h4(tags$b("Data format")),
                             p("Data can be uploaded as CSV file."),
                             p("To calculate and plot custom CUSUM charts, make sure to check ", tags$i("Select Data - User data"),"next to the CUSUM chart."),
                             p("For the", tags$b("standard CUSUM chart"), 
                             "a column containing the binary patient outcomes in their sequential order must be available. 
                             Patient outcome = 1 refers to a failure or adverse event and patient outcome = 0 to success or no adverse event. The name of this column is specified under", 
                               tags$i("Variable Names"), "in the tab ", tags$i("Upload data.")),
                             p("For the ", tags$b("risk-adjusted CUSUM chart"), ", a column containing the patient risks must be provided. ")
                             ),
                    tabPanel("Literature",
                             tags$br(),
                             p("For a general introduction to Statistical Process Control: 
                               Montgomery, D. C.", 
                               tags$a(href = "http://dl4a.org/uploads/pdf/581SPC.pdf" , 
                                      "Introduction to statistical quality control."
                                      ), 
                               "(Wiley, 2009)."
                               ),
                               p("For calculating CUSUM weights for the standard CUSUM and risk-adjusted CUSUM: 
                                 Steiner, S. H., Cook, R. J., Farewell, V. T. & Treasure, T. ",
                                 tags$a(href = "https://uwaterloo.ca/statistics-and-actuarial-science/sites/ca.statistics-and-actuarial-science/files/uploads/files/2000_risk_adjusted_cusum_biostats.pdf" , 
                                        "Monitoring surgical performance using risk-adjusted cumulative sum charts."
                                        ),
                                 "Biostatistics 1, 441–452 (2000)."
                                 ),
                             p("For an introduction and analysis of specified parameters and simulate control limits: Hubig, L., Lack, N., Mansmann, U. 
                               Statistical Process Monitoring to Improve Quality Assurance of Inpatient Care. Manuscript submitted for publication (2019)."),
                             p("For an overview of the ", tags$b("cusum"),  "R-Package take a look at the vignettes and package documentation available on ", 
                               tags$a(href = "https://cran.rstudio.com//web/packages/cusum/index.html", "CRAN."))
                             )
                    
                    )
                    )
             )

    # # Help ----
    # 
    # navbarMenu(
    #   "Help",
    #   tabPanel("CUSUM Chart Overview"),
    #   "----",
    #   
    #   tabPanel("How to upload data"),
    #   tabPanel("How to set parameters",
    #   h4("Baseline Failure Probability"),
    #   h4("Detection Level"),
    #   h4("Reset CUSUM"),
    #   h4("False signal probability"),
    #   h4("Control Limit")),
    #   tabPanel("How to interpret results",
    #   h4("CUSUM Chart"),
    #   h4("CUSUM Signals")),
    #   "----",
    #   tabPanel("About")
    #   
    # )
  )
)



