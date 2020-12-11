library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")
library("tidyverse")
library("data.table")

source("app_server.R")
source("app_ui.R")

shinyApp(ui = ui, server = server)
