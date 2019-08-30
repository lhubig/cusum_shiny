## app.R ##
library(shiny)
library(cusum)
library(ggplot2)
library(shinydashboard)

# Load data ----
load("data/gcusum_example_data.RData")

# User interface ----
ui <- dashboardPage(
  
                    
  dashboardHeader(title = "CUSUM Charts"),

  dashboardSidebar(
    sliderInput("failure_prob",
      label = "Baseline Failure Probability",
      min = 0,
      max = 0.5,
      value = 0.1,
      step = 0.01
    ),

    sliderInput("limit",
      label = "Control Limit",
      min = 0,
      max = 10,
      value = 4.35,
      round = -20,
      step = 0.1
    ),
    
    sliderInput("false_signal",
                label = "False Signal Probability",
                min = 0,
                max = 1,
                value = 0.05,
                step = 0.01
    ),

    sliderInput("detection_level",
      label = "Detection Level",
      min = 1.5,
      max = 4,
      value = 2,
      step = .5
    ),

    checkboxInput("reset",
      label = "Reset the CUSUM after a signal",
      value = FALSE
    ),
    tags$hr(),

    sidebarMenu(
      menuItem("CUSUM", tabName = "CUSUM", icon = icon("procedures")), # heartbeat
      menuItem("Upload new data", tabName = "upload_data", icon = icon("file-upload"))
    ),
    tags$hr()
  ),

  dashboardBody(
    tabItems(
# First tab content ----
      tabItem(
        tabName = "CUSUM",
        h2("CUSUM Chart"),

        plotOutput("cs_plot"),
        
        # Horizontal line
        tags$hr(),
        
        h2("CUSUM signals"),
        tableOutput("signals")
      ),

# Second tab content ----
      tabItem(
        tabName = "upload_data",
        h2("Upload new data"),
        box(
          fileInput("file1", "Upload CSV File",
            multiple = FALSE, accept = NULL,
            width = NULL, buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),

          # Horizontal line
          tags$hr(),

          # Input: Checkbox if file has header
          checkboxInput("header", "Header", TRUE),

          # Input: Select separator
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
          ),

          # Horizontal line
          tags$hr(),

          # Input: Select number of rows to display
          radioButtons("disp", "Display",
            choices = c(
              Head = "head",
              All = "all"
            ),
            selected = "head"
          )
        ),
        box(
          tableOutput("contents")
        )
      )
    )
  )
)

# server ----
server <- function(input, output, session) { 

  observeEvent(input$limit, {
    alpha <- cusum_alpha_sim(failure_probability = input$failure_prob,
                             n_patients = nrow(infile()),
                             odds_multiplier = input$detection_level,
                             n_simulation = 10000,
                             limit = input$limit,
                             seed = 10082018)
    alpha <- round(alpha,3)
    
    updateSliderInput(session = session, inputId = "false_signal", value =  round(alpha,3))
  })
  
  observeEvent(input$false_signal, {
    limit <- cusum_limit_sim(failure_probability = input$failure_prob,
                             n_patients = nrow(infile()),
                             odds_multiplier = input$detection_level,
                             n_simulation = 10000,
                             alpha = input$false_signal,
                             seed = 10082018)
    limit <- round(limit, 2)
    
    updateSliderInput(session = session, inputId = "limit", value =  round(limit,2))
  })
  
  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )

    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })

  data <- reactive({
    req(input$file1)

    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)

    return(df)
    
  })
  
  infile <- reactive({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
  })
 
  
  cs <- reactive({
    cusum(input$failure_prob, patient_outcomes = infile()$y,
          limit = input$limit,
          odds_multiplier = input$detection_level,
          reset = input$reset)
  }) 
 
    fsp <-  reactive({cusum_alpha_sim(failure_probability = input$failure_prob,
                              n_patients = nrow(infile()),
                             limit = input$limit,
                             odds_multiplier = input$detection_level,
                             n_simulation = 1000)
  }
  )
  
  output$alpha_text <- renderText({
    paste0("False signal probability: ", round(fsp() * 100,2), "%")
  })


  output$cs_plot <- renderPlot({
    plot <- ggplot(cs(), aes(x = t, y = ct))

    signals <- cs()[cs()$signal == 1, ]
    if (nrow(signals) > 0) {
      x <- NULL
      for (i in 2:nrow(signals) - 1) {
        if ((signals$t[i + 1] - signals$t[i]) > 1) {
          x <- rbind(x, signals$t[i + 1])
        }
      }
      x <- rbind(x, min(signals$t))
      x <- data.frame(
        t = x,
        ct = cs()$ct[x],
        y = rep(input$limit, nrow(x))
      )
      plot <- plot + geom_point(
        data = x,
        aes(x = t, y = ct),
        col = "#f7ba02",
        size = 4,
        pch = 8, stroke = 2
      )
    }
    plot + geom_line() +
      geom_point(size = 1) +
      geom_hline(aes(yintercept = input$limit), size = 1, col = "#4063bc") +
      theme_bw()

    # plot(cs())
  })
  
  output$signals <- renderTable({
    signals <- cs()[cs()$signal == 1, ]
    x <- NULL
    if (nrow(signals) > 0) {
      
      for (i in 2:nrow(signals) - 1) {
        if ((signals$t[i + 1] - signals$t[i]) > 1) {
          x <- rbind(x, signals$t[i + 1])
        }
      }
      x <- rbind(x, min(signals$t))
      x <- data.frame(
        Observation = as.integer(x),
        CUSUM_value = cs()$ct[x]
      )
      x <- x[order(x$Observation),]
    } else {
      x <- data.frame(
        Observation = 0,
        CUSUM_value = 0
      )
    }
    return(x)
    
  })
}

shinyApp(ui, server)
