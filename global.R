######## LIBRARIES ########
library(shinydashboard)
library(tidyverse)
#library(plyr)
library(lubridate)
library(tools)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)

#detach("package:plyr", unload = TRUE)

####### SETUP  ####### 

# Spinner 
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

# Issue Connection Stop --- This should be moved to the bottom of global.R
shiny::onStop(function(){dbDisconnect(con)})
