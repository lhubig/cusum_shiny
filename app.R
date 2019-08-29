library(shiny)
library(cusum)
library(ggplot2)
load("data/gcusum_example_data.RData")

shinyApp(ui = ui, server = server)