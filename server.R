server <- function(input, output, session) {
  
  data <- reactive({
    if(input$load_data == 1){
      req(input$user_data)
      
      df <- read.csv(input$user_data$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
      return(df)
    } else {
      return(gcusum_example_data)
      }
  })

  # set control limit ----
  limit <- reactive({
    if(input$fix_term == 1){
      return(input$control_limit)
    } else {
      return(cusum_limit_sim(input$failure_prob, 
                               n_patients = nrow(data()),
                               odds_multiplier = input$detection_level_d,
                               n_simulation = 1000,
                               alpha = (input$false_signal)/100))
    }
  })
  
  # Calculate CUSUM ----
  cs <- reactive({
    cusum(input$failure_prob, patient_outcomes = data()$y,
          limit = limit(),
          odds_multiplier = input$detection_level_d,
          reset = input$reset)
  }) 
  
  # Calculate other fixed term ----
  output$other_fixed_term <- renderText({
    if (input$fix_term == 1){
      alpha <- cusum_alpha_sim(failure_probability = input$failure_prob,
                               n_patients = nrow(data()),
                               odds_multiplier = input$detection_level_d,
                               n_simulation = 1000,
                               limit = input$control_limit)
      return(paste0('<B>Corresponding FSP:</B> ', round(alpha * 100,2), "%"))
      
    } else {
      return(paste0('<B>Corresponding CL:</B> ', round(limit(),2)))
    }
    
  })
  
  # Output Plot ----
  output$distPlot <- renderPlot({
    
    plot <- ggplot(cs(), aes(x = t, y = ct))
    
    signals <- cs()[cs()$signal == 1, ]

    if (nrow(signals) > 0) {
      x <- NULL
      if (nrow(signals) > 1){
        for (i in 2:nrow(signals) - 1) {
          if ((signals$t[i + 1] - signals$t[i]) > 1) {
            x <- rbind(x, signals$t[i + 1])
          }
        }
      }
      x <- rbind(x, min(signals$t))
      x <- data.frame(
        t = x,
        ct = cs()$ct[x],
        y = rep(limit(), nrow(x))
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
      geom_hline(aes(yintercept = limit()), size = 1, col = "#4063bc") +
      theme_bw()
    
  })
  
  # Output Signals ----
  output$signals <- renderTable({
    signals <- cs()[cs()$signal == 1, ]
    x <- NULL
    if (nrow(signals) > 0) {
      
      if (nrow(signals) > 1){
        for (i in 2:nrow(signals) - 1) {
          if ((signals$t[i + 1] - signals$t[i]) > 1) {
            x <- rbind(x, signals$t[i + 1])
          }
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