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

# Build Data Frame
education <- locations %>%
  filter(TypeOfPlace == "School or college") %>%
  left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
  left_join(cases, by = "CaseNumber")

# Function
function(input, output, session) {
  
  output$opn_education_cases_table = DT::renderDataTable({
    
    education.group.list <- education %>%
      filter(CreatedOn.x >= Sys.Date()-28) %>% 
      select(InstitutionName.x, InstitutionReferenceNumber, InstitutionType, TypeOfPlace, AddressLine1.x, AddressLine2.x, AddressLine3.x, AddressCity, AddressCounty, Country, PostCode.x,  AlreadyCompletedViaSelfTrace) %>% 
      group_by(InstitutionName.x) %>% 
      tally()
    
  })
  
}