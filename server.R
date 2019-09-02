library(cusum)
library(ggplot2)
library(dplyr)
library(cowplot)

load("data/example_data.RData")
load("data/ra_example_data.RData")



server <- function(input, output, session) {

  # Upload data ----
  data <- reactive({
    if (input$data_source == 2) {
      req(input$user_data)

      df <- read.csv(input$user_data$datapath,
        header = input$header, sep = input$sep,
        quote = input$quote
      )
      
      y <- select(df, matches(input$outcome_var))
      t <- seq(1, nrow(y))
      if(input$risk_var == ""){
        df <- data.frame(t, y)

      } else {
        score <- select(df, matches(input$risk_var))
        df <- as.data.frame(t, y,score)
      }
      
      return(df)
    } else {
      return(example_data)
    }
  })
  
  data_ra <- reactive({
    if (input$data_source == 2) {
      req(input$user_data)
      df <- read.csv(input$user_data$datapath,
                     header = input$header, 
                     sep = input$sep,
                     quote = input$quote
                     )
      
      y <- select(df, matches(input$outcome_var))
      t <- seq(1, nrow(y))
      if(input$risk_var == ""){
        return(ra_example_data)
      } else {
        score <- select(df, matches(input$risk_var))
        df <- as.data.frame(t, y,score)
      }
      
      return(df)
    } else {
      return(ra_example_data)
    }
  })

  # set control limit ----
  limit_d <- reactive({
    if (input$fix_term == 2) {
      return(input$control_limit_d)
    } else {
      return(round(cusum_limit_sim((input$failure_prob / 100),
        n_patients = nrow(data()),
        odds_multiplier = input$detection_level_d,
        n_simulation = input$n_sim,
        alpha = (input$false_signal) / 100
      ), 4))
    }
  })
  
  limit_i <- reactive({
    if (input$fix_term == 2) {
      return(input$control_limit_i)
    } else {
      return(round(cusum_limit_sim((input$failure_prob / 100),
                                   n_patients = nrow(data()),
                                   odds_multiplier = input$detection_level_i,
                                   n_simulation = input$n_sim,
                                   alpha = (input$false_signal) / 100
      ), 4))
    }
  })

  # Calculate CUSUM ----
  cs <- reactive({
    cusum((input$failure_prob / 100),
      patient_outcomes = data()$y,
      limit = limit_d(),
      odds_multiplier = input$detection_level_d,
      reset = input$reset
    )
  })
  
  cs_i <- reactive({
    cusum((input$failure_prob/100),
          patient_outcomes = data()$y,
          limit = limit_i(),
          odds_multiplier = input$detection_level_i, 
          reset = input$reset)
  })

  # Calculate other fixed term ----
  output$other_fixed_term <- renderText({
    if (input$fix_term == 2) {
      alpha_d <- cusum_alpha_sim(
        failure_probability = (input$failure_prob / 100),
        n_patients = nrow(data()),
        odds_multiplier = input$detection_level_d,
        n_simulation = input$n_sim,
        limit = input$control_limit_d
      )
      
      alpha_i <- cusum_alpha_sim(
        failure_probability = (input$failure_prob / 100),
        n_patients = nrow(data()),
        odds_multiplier = input$detection_level_i,
        n_simulation = input$n_sim,
        limit = input$control_limit_i
      )
      return(paste0("<B>Corresponding FSP:</B><br>",
                    "Deterioriation: ", round(alpha_d * 100, 2), "% <br> ", 
                    "Improvement:" , round(alpha_i * 100, 2), "%"))
    } else {
      return(paste0("<B>Corresponding CL:</B> <br>",
                    "Deterioriation: ", round(limit_d(), 2), "<br>",
                    "Improvement: " , round(limit_i(), 2)))
    }
  })

  # Output Plot ----
  output$distPlot <- renderPlot({
    plot <- ggplot(cs(), aes(x = t, y = ct)) +
      geom_hline(aes(yintercept = limit_d()), size = 1, col = "#267ee2") +
      geom_hline(aes(yintercept = limit_i()), size = 1, col = "#267ee2")


    signals <- cs()[cs()$signal == 1, ]

    if (nrow(signals) > 0) {
      x <- NULL
      if (nrow(signals) > 1) {
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
        y = rep(limit_d(), nrow(x))
      )
      plot <- plot + geom_point(
        data = x,
        aes(x = t, y = ct),
        col = "#ff7316",
        size = 4,
        pch = 8, stroke = 2
      )
    }
    
    signals_i <- cs_i()[cs_i()$signal == 1, ]
    
    if (nrow(signals_i) > 0) {
      x <- NULL
      if (nrow(signals_i) > 1) {
        for (i in 2:nrow(signals_i) - 1) {
          if ((signals_i$t[i + 1] - signals_i$t[i]) > 1) {
            x <- rbind(x, signals_i$t[i + 1])
          }
        }
        }
      x <- rbind(x, min(signals_i$t))
      x <- data.frame(
        t = x,
        ct = cs_i()$ct[x],
        y = rep(limit_d(), nrow(x))
      )
      
      plot <- plot + geom_point(
        data = x,
        aes(x = t, y = ct),
        col = "#3eb517",
        size = 4,
        pch = 8, stroke = 2
      )
    }
    plot <- plot + geom_line() +
      geom_point(size = 1) +
      scale_x_continuous(name = "Sequence of observation") +
      scale_y_continuous(name = "CUSUM Statistic") +
      theme_bw()+ 
      geom_line(data = cs_i(), aes(x = t, y = ct)) +
      geom_point(data =cs_i(), aes(x = t, y = ct), size = 1) +
      geom_hline(aes(yintercept = 0), col = "black", size =1) 
    plot
  })

  # Output Signals ----
  output$signals <- renderTable({
    signals <- cs()[cs()$signal == 1, ]
    x <- NULL
    if (nrow(signals) > 0) {
      if (nrow(signals) > 1) {
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
      x <- x[order(x$Observation), ]
    } else {
      x <- data.frame(
        Observation = 0,
        CUSUM_value = 0
      )
    }
    return(x)
  })



  # Risk-adjusted CUSUM ----

  # Set RA Control Limit ----
  limit_ra <- reactive({
    if (input$cl_method == 1) {
      if (input$fixed_term_ra == 2) {
        return(input$control_limit_ra)
      } else {
        return(round(racusum_limit_sim(data_ra()$score, 
                                       odds_multiplier = input$detection_level_ra, 
                                       alpha = (input$false_signal_ra / 100),
                                       n_simulation = input$n_sim_ra
                                       )
                     )
               )
      }
    } else {
      return(round(racusum_limit_dpcl(patient_risks = data_ra()$score, 
                                      odds_multiplier = input$detection_level_ra, 
                                      alpha = (input$false_signal_ra / 100),
                                      N = input$n_sim_ra
                                      )
                   )
             )
    }
  })


  # Calculate RA-CUSUM ----
  cs_ra <- reactive({
    racusum(
      patient_risks = data_ra()$score,
      patient_outcomes = data_ra()$y,
      limit = limit_ra(),
      odds_multiplier = input$detection_level_ra,
      reset = input$reset_ra
    )
  })

  ## Output RA-Plot ----
  output$plot_ra <- renderPlot({
    plot <- ggplot(cs_ra(), aes(x = t, y = ct)) +
      geom_line(aes(y = limit_ra(), x = t), size = 1, col = "#267ee2")
    
    
     signals <- cs_ra()[cs_ra()$signal == 1, ]

    if (nrow(signals) > 0) {
      x <- NULL
      if (nrow(signals) > 1) {
        for (i in 2:nrow(signals) - 1) {
          if ((signals$t[i + 1] - signals$t[i]) > 1) {
            x <- rbind(x, signals$t[i + 1])
          }
        }
      }
      x <- rbind(x, min(signals$t))
      x <- data.frame(
        t = x,
        ct = cs_ra()$ct[x]
      )
      plot <- plot + geom_point(
        data = x,
        aes(x = t, y = ct),
        col = "#ff7316",
        size = 4,
        pch = 8, stroke = 2
      )
    }
     plot +
       geom_line() +
       geom_point(size = 1) +
       theme_bw() +
       scale_x_continuous(name = "Sequence of observation") +
       scale_y_continuous(name = "RA-CUSUM Statistic") 
  })

  # Output data table ----
  output$contents <- renderPrint({
    req(input$user_data)

    df <- read.csv(input$user_data$datapath,
      header = input$header, sep = input$sep,
      quote = input$quote
    )
    return(summary(df))
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      d <- Sys.time()
      d <- gsub(":", "", d)
      d <- gsub(" ", "", d)
      d <- gsub("-", "", d)
      paste0("cusum_", d, ".csv")
    },
    content = function(file) {
      write.csv(cs(), file, row.names = FALSE)
    }
  )
}
