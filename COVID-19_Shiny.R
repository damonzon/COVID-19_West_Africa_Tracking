# COVID-19 with Shiny
#Run these commands from RStudio to run the App.
list.of.packages <- c("shiny",
    "data.table","ggplot2",
    "plotly","shinythemes",
    "ggthemes","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(shinythemes)
library(ggthemes)
library(lubridate)
shiny::runGitHub("COVID-19_West_Africa_Tracking", "damonzon")
# source("https://raw.github.com/Damonzon/COVID-19_West_Africa_Tracking/master/COVID-19_Shiny.R")
