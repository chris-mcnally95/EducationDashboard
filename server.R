####### SETUP  ####### 

# Libraries
library(shinydashboard)
library(tidyverse)
library(plyr)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)

#detach("package:plyr", unload = TRUE)


# Source scripts
source("./azure_functions.R")

# Load data
locations <- getTable("Locations")
collectclosecontacts <- getTable("CollectContactsCalls")
cases <- getTable("cases")

# Function
function(input, output, session) {
  
  # Build Data Frame
  education <- locations %>%
    filter(TypeOfPlace == "School or college") %>%
    left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
    left_join(cases, by = "CaseNumber")
  
  
  # Group Schools 
  
  education.group.list <- education %>%
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally()
  
  education.group.list.month <- education %>%
    filter(CreatedOn.x >= Sys.Date()-28) %>% 
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally()
  education.group.list.month <-education.group.list.month[order(-education.group.list.month$n), ]
  colnames(education.group.list.month) <- c("Institution Name", "City", "Institution Type", "Reference Number", "Number of Associatied Cases")
  
  # InfoBoxes
  
  ## Total Cases
  output$total_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Education Insitutions", 
      paste0(nrow(filter(education))), 
      icon = icon("chart-bar"), 
      color ="blue")
  })
  ## Total Cases this Month
  output$cases_this_month <- renderInfoBox({
    infoBox(
      "Reported Cases This Month from Education Insitutions", 
      paste0(nrow(education %>% 
                         filter(CreatedOn.x >= Sys.Date()-28))), 
      icon = icon("virus"), 
      color = "light-blue")
  })
  
  ## Total Schools
  output$total_groups <- renderInfoBox({
    infoBox(
      "Total Reported Affected Education Insitutions", 
      paste0(nrow(filter(education.group.list))), 
      icon = icon("school"), 
      color ="blue")
  })
  ## Total Schools this Month
  output$groups_this_month <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected This Month", 
      paste0(nrow(education.group.list.month)), 
      icon = icon("graduation-cap"), 
      color = "light-blue")
  })

  # Frequency Table
  output$education_cases_table = DT::renderDataTable({
    DT::datatable(education.group.list.month[-1, ], options = list(pageLength = 25))
  })
  
}