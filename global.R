
# Libraries
library(shinydashboard)
library(tidyverse)
#library(plyr)
library(lubridate)
library(tools)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)

######## SPINNER ########
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

#detach("package:plyr", unload = TRUE)

# Source scripts
source("./azure_functions.R")

####### SETUP  ####### 

# Variables
today <- Sys.Date()
yesterday <- today - 1
oneWeek <- today - 7
fourteendays <- today - 15
twentyeightdays <- today - 29

twentyFourHours <- today - 1
fourtyEightHours <- today - 2
seventyTwoHours <- today - 3
ninetySixHours <- today - 4
oneHundredFourtyFourHours <- today - 6


# Load data
locations <- getTable("Locations")
collectclosecontacts <- getTable("CollectContactsCalls")
cases <- getTable("cases")
wgscases <- getTable("Wgscases")

# Load in the school stat RDS
schools_stats <- readRDS("schools_stats.RDS")

##---- 

# Build the schools cases table
schools_cases <- locations %>%
  filter(TypeOfPlace == "School or college") %>%
  # Add the cases data from collect_contacts --- collectclosecontacts
  left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
  # Add the cases data from all_cases --- cases here
  left_join(cases, by = "CaseNumber") 

# Fix DENI number
schools_cases <- schools_cases %>%
  mutate(InstitutionReferenceNumber = gsub('-', '', InstitutionReferenceNumber))

# Add WGS data
schools_cases_w_wgs <- left_join(schools_cases, wgscases, by = "ContactId")

# Generate some stats about each school
schools_cases_stats <- schools_cases_w_wgs %>%
  group_by(InstitutionReferenceNumber) %>%
  dplyr::summarise(
    TotalCases = n(), 
    TotalCloseContacts = sum(CloseContactCount, na.rm = TRUE),
    CasesPrev28Days = sum(DateOfSample.x >= twentyeightdays, na.rm = TRUE),
    CasesWithinLast3Days = sum(DateOfSample.x <= today & DateOfSample.x >= seventyTwoHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSample.x < seventyTwoHours & DateOfSample.x >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSample.x <= today & DateOfSample.x >= oneHundredFourtyFourHours, na.rm = TRUE),
    CaseTrend = case_when(
      CasesWithinLast3Days > CasesWithinLast4to6Days ~ c('<i class="fas fa-arrow-up"></i> Up'),
      CasesWithinLast3Days < CasesWithinLast4to6Days ~ c('<i class="fas fa-arrow-down"></i> Down'),
      CasesWithinLast3Days == CasesWithinLast4to6Days ~ c('<i class="fas fa-arrows-alt-h"></i> Nil')),
    EarliestOnset = date(min(DateOfOnset, na.rm = TRUE)),
    EarliestSample = date(min(DateOfSample.x, na.rm = TRUE)),
    EarliestResult = date(min(DateOfResult, na.rm = TRUE)),
    MostRecentOnset = date(max(DateOfOnset, na.rm = TRUE)),
    MostRecentSample = date(max(DateOfSample.x, na.rm = TRUE)),
    MostRecentResult = date(max(DateOfResult, na.rm = TRUE)),
    MinAge = min(AgeAtPositiveResult, na.rm= TRUE), 
    MaxAge = max(AgeAtPositiveResult, na.rm= TRUE),
    MedianAge = median(AgeAtPositiveResult, na.rm= TRUE),
    MaleCases = sum(Gender.x == "Male", na.rm= TRUE),
    FemaleCases = sum(Gender.x == "Female", na.rm= TRUE),
    AlphaVariants = sum(WgsVariant == "VOC-20DEC-01", na.rm = TRUE),
    BetaVariants = sum(WgsVariant == "VOC-20DEC-02", na.rm = TRUE),
    DeltaVariants = sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    DeltaReflex = sum(WgsReflexAssay == "Delta", na.rm = TRUE) + sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    #DeltaReflex = sum(str_detect(WgsReferralOtherReason, "Reflex|REFLEX|reflex") == TRUE, na.rm = TRUE),
    `VOC-21FEB-02Variants` = sum(WgsVariant == "E484K", na.rm = TRUE),
    KappaVariants = sum(WgsVariant == "VUI-21APR-01", na.rm = TRUE),
    `VUI-21FEB-04Variants` = sum(WgsVariant == "VUI-21FEB-04", na.rm = TRUE),
    `VUI-21MAY-02Variants` = sum(WgsVariant == "VUI-21MAY-02", na.rm = TRUE))

# Join schools_cases_stats to schools_stats
schools_stats_overall <- left_join(schools_stats, schools_cases_stats, by = c("DENINumber" = "InstitutionReferenceNumber"))



# Issue Connection Stop --- This should be moved to the bottom of global.R
shiny::onStop(function(){dbDisconnect(con)})
