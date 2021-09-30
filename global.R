# Libraries 
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(tools)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(htmlwidgets)
library(DBI)
library(dbplyr)

# Suppress Warnings
options(warn = - 1)   

# Issue Connection Stop --- This should be moved to the bottom of global.R
shiny::onStop(function(){DBI::dbDisconnect(con)})
