####### SETUP  ####### 

# Libraries
library(shinydashboard)
library(tidyverse)
library(plyr)
library(lubridate)
library(tools)
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
  
  # Build Data Frames
  
  ## Education
  education <- locations %>%
    filter(TypeOfPlace == "School or college") %>%
    left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
    left_join(cases, by = "CaseNumber")
  
  
  ## Education Group Lists
  education.group.list <- education %>%
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally()
  
  education.group.list.28days <- education %>%
    filter(CreatedOn.x >= Sys.Date()-28) %>% 
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally() 
  
  education.group.list.7days <- education %>%
    filter(CreatedOn.x >= Sys.Date()-7) %>% 
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally() 
  
  education.group.list.prev7days <- education %>%
    filter(CreatedOn.x >= Sys.Date()-14 & CreatedOn.x <= Sys.Date()-8) %>% 
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally() 
  
  education.group.list.4to6days <- education %>%
    filter(CreatedOn.x >= Sys.Date()-6 & CreatedOn.x <= Sys.Date()-4) %>% 
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally() 
  
  education.group.list.3days <- education %>%
    filter(CreatedOn.x >= Sys.Date()-3) %>% 
    group_by(InstitutionName.x, City.x, InstitutionType, InstitutionReferenceNumber) %>% 
    tally() 
  
  ## Create Table
  education.group.table <- education.group.list.28days %>% 
    left_join(education.group.list.7days[ , -c(2:4)], by = "InstitutionName.x") %>% 
    left_join(education.group.list.4to6days[ , -c(2:4)], by = "InstitutionName.x") %>%
    left_join(education.group.list.3days[ , -c(2:4)], by = "InstitutionName.x") %>% 
    #drop_na() %>%
    distinct(InstitutionReferenceNumber, .keep_all = TRUE) %>% 
    mutate(CaseTrend = case_when(
      n.y.y > n.x.x ~ c('Up'),
      n.y.y < n.x.x ~ c('Down'),
      n.y.y == n.x.x ~ c('Stable')))
  
  ## Tidy Table 
  education.group.table <- education.group.table[order(-education.group.table$n.x), ]
  education.group.table$City.x <- tolower(education.group.table$City.x)
  education.group.table$City.x <- toTitleCase(education.group.table$City.x)
  colnames(education.group.table) <- c("Institution Name", "City", "Institution Type", "Reference Number", "Cases Last 28 Days", "Cases Last 7 Days", "Cases Last 4 to 6 Days", "Cases Last 3 Days", "Trend")
  
  ## Downloadable csv of selected dataset 
  output$DownloadHomeReport <- downloadHandler(
    filename = function() {
      paste("Prev28DayEducationReport-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(education.group.table, file, row.names = FALSE)
    }
  )
  
  # InfoBoxes
  
  ## Total Cases
  output$total_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Education Insitutions", 
      paste0(formatC(nrow(filter(education)), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color ="blue")
  })
  
  ## Total Schools
  output$total_groups <- renderInfoBox({
    infoBox(
      "Total Reported Affected Education Insitutions", 
      paste0(formatC(nrow(filter(education.group.list %>%
                                   distinct(InstitutionReferenceNumber, .keep_all = TRUE))), format="d", big.mark=",")), 
      icon = icon("school"), 
      color ="blue")
  })
  
  ## Total Cases Last Week
  output$cases_last_week <- renderInfoBox({
    infoBox(
      "Reported Cases Last Week from Education Insitutions", 
      paste0(formatC(nrow(education %>% 
                            filter(CreatedOn.x >= Sys.Date()-14 & CreatedOn.x <= Sys.Date()-8)), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color = "light-blue")
  })
  
  ## Total Schools Last Week
  output$groups_last_week <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected Last Week", 
      paste0(formatC(nrow(education.group.list.prev7days %>%
                            distinct(InstitutionReferenceNumber, .keep_all = TRUE)), format="d", big.mark=",")), 
      icon = icon("school"), 
      color = "light-blue")
  })
  
  ## Total Cases This Week
  output$cases_this_week <- renderInfoBox({
    infoBox(
      "Reported Cases This Week from Education Insitutions", 
      paste0(formatC(nrow(education %>% 
                         filter(CreatedOn.x >= Sys.Date()-7)), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color = "navy")
  })
  
  ## Total Schools This Week
  output$groups_this_week <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected This Week", 
      paste0(formatC(nrow(education.group.list.7days %>%
                            distinct(InstitutionReferenceNumber, .keep_all = TRUE)), format="d", big.mark=",")), 
      icon = icon("school"), 
      color = "navy")
  })

  
  # Frequency Table
  
  output$education_cases_table = DT::renderDataTable({
    DT::datatable(education.group.table[ -1, ], options = list(pageLength = 25))
  })
  
}
