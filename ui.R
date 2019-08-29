library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CUSUM Charts"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      ###
      sliderInput("failure_prob",
                  label = "Baseline Failure Probability",
                  min = 0,
                  max = 0.5,
                  value = 0.1,
                  step = 0.01
                  ),
      
      sliderInput("detection_level_d",
                  label = "Detection Level ",
                  min = 1,
                  max = 4,
                  value = 2,
                  step = .1
                  ),
      
      # sliderInput("detection_level_i",
      #             label = "Detection Level (Improvement)",
      #             min = 0,
      #             max = 1,
      #             value = 0.5,
      #             step = .1
      #             ),
      
      checkboxInput("reset",
                    label = "Reset the CUSUM after a signal",
                    value = FALSE
                    ),
      
      tags$hr(),
      radioButtons("fix_term", 
                   label = "Set fixed term", 
                   choices = list("Control limit" = 1, 
                                  "False signal probability (%)" = 2)
                   ),
      
      
      conditionalPanel(
        condition = "input.fix_term == 1",
        sliderInput("control_limit", label = "",
                    min = 1, max = 10,
                    value = 2,
                    step = .01)
        ),
      
      conditionalPanel(
        condition = "input.fix_term == 2",
        sliderInput("false_signal", label = "",
                    min = 0, max = 100,
                    value = 0.05,
                    step = .01)
        ),
      
      htmlOutput("other_fixed_term"),
      
      tags$hr(),
      
      h4(),
      
      
      tags$hr(),
      
      h4(checkboxInput("load_data",
                   label = p(icon("file-upload"),"Upload own data"),
                   value = FALSE)),

      conditionalPanel(
        condition = "input.load_data == 1",
        fileInput("user_data", "Upload CSV File",
                  multiple = FALSE, accept = NULL,
                  width = NULL, buttonLabel = "Browse...",
                  placeholder = "No file selected"
                  ),
        
        
        checkboxInput("header", "Header", TRUE),
        
        radioButtons("sep", "Separator",
                     choices = c(
                       Comma = ",",
                       Semicolon = ";",
                       Tab = "\t"
                     ),
                     selected = ","
        ),
        
        # Input: Select quotes
        radioButtons("quote", "Quote",
                     choices = c(
                       None = "",
                       "Double Quote" = '"',
                       "Single Quote" = "'"
                     ),
                     selected = '"'
        )
        )
      
      ),

    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Chart",
                 plotOutput(outputId = "distPlot")),
        tabPanel("Signals",
                 tableOutput("signals")
        )
      )
      
      
      # actionButton(inputId = "action_cusum", 
      #              label = "Calculate CUSUM",
      #              icon = icon("procedures")),
      # 
      # h3("CUSUM Chart"),
      # plotOutput(outputId = "distPlot"),
      # 
      # h3("CUSUM Signals"),
      # tableOutput("signals")
      # )
    )
  )
)