#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
# Define UI for application that draws a histogram
source("app_ui.R")

ui<-fluidPage(
    theme="style.css",
    tt,
    tt1,
    tt2
    
)
# Define server logic required to draw a histogram
source("app_server.R")
# Run the application 
shinyApp(ui = ui, server = server)
