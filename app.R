library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("tidyverse")
library("data.table")


source("app_ui.R")
source("app_server.R")

shinyApp(ui = ui, server = server) 
