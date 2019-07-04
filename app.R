# Load packages ----
library(shiny)
library(cusum)

# Load data ----
load("data/cusum_example_data.rda")

# Source helper functions -----


# User interface ----
ui <- fluidPage(
  titlePanel("CUSUM"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create CUSUM Charts"),
      
      sliderInput("failure_prob",
                  label = "Baseline Failure Probability",
                  min = 0,
                  max = 0.5,
                  value = 0.1,
                  round = -20),
      
      sliderInput("limit",
                  label = "Control Limit",
                  min = 0.1,
                  max = 10,
                  value = 2.5),
      
      sliderInput("detection_level",
                  label = "Detection Level",
                  min = 0,
                  max = 5,
                  value = 2,
                  round = 1),
      
      checkboxInput("reset",
                    label = "Reset the CUSUM after a signal",
                    value = FALSE)
    ),
    
    mainPanel(plotOutput("cs_plot"))
  )
)

# Server logic ----
server <- function(input, output) {
  cs <- reactive({
    cusum(input$failure_prob, patient_outcomes = cusum_example_data$y[cusum_example_data$year == 2017],
          limit = input$limit,
          odds_multiplier = input$detection_level,
          reset = input$reset)
  }) 
  
  output$cs_plot <- renderPlot({
    plot(cs())
  })
}

# Run app ----
shinyApp(ui, server)