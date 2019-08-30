library(shiny)
library(cusum)
library(ggplot2)
load("data/example.RData")
load("data/ra_example.RData")

shinyApp(ui = ui, server = server)