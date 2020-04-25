# COVID-19 with Shiny
#Run these commands from RStudio to run the App.
list.of.packages <- c("shiny","shinydasboard",
    "data.table","ggplot2", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(plotly)
shiny::runGitHub("COVID-19_West_Africa_Tracking", "damonzon")
