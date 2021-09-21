####### BUILD DATA FRAMES  ####### 
######## LIBRARIES ########
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(tools)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)
source("./azure_functions.R")

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

currentYear <- year(today)


# Load data
locations <- getTable("Locations")
collectclosecontacts <- getTable("CollectContactsCalls")
cases <- getTable("cases")
wgscases <- getTable("Wgscases")

# Load in the school stat RDS
schools_stats <- readRDS("schools_stats.RDS")

# Build the schools cases table
schools_cases <- locations %>%
  filter(TypeOfPlace == "School or college") %>%
  # Add the cases data from collect_contacts --- collectclosecontacts
  left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
  # Fix the names of columns
  rename_with(~ gsub(".x", "Locations", .x, fixed = TRUE)) %>%
  rename_with(~ gsub(".y", "CollectCloseContacts", .x, fixed = TRUE)) %>%
  # Add the cases data from all_cases --- cases here
  left_join(cases, by = "CaseNumber") %>%
  # Fix the names of columns
  rename_with(~ gsub(".x", "Merged", .x, fixed = TRUE)) %>%
  rename_with(~ gsub(".y", "Cases", .x, fixed = TRUE)) %>%
  # Only keep Cases.
  filter(!is.na(CaseNumber))

# Fix DENI number
schools_cases <- schools_cases %>%
  mutate(InstitutionReferenceNumber = gsub('-', '', InstitutionReferenceNumber))

# Add School Year variable
  ##
  ## TO DO :
  ## Change the year inside each date to be dynamic. 
  ## Get current year and subtract x years. 
  ## If done, this will never need to be rewritten.
  ##
schools_cases <- schools_cases %>% 
  mutate(DateOfBirth = date(as_datetime(DateOfBirth))) %>%
  mutate(SchoolYear = case_when(DateOfBirth >= as.Date(paste0(currentYear-3,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-2,"-07-01")) ~ "Nursery",
                                DateOfBirth >= as.Date(paste0(currentYear-4,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-3,"-07-01")) ~ "Reception",
                                DateOfBirth >= as.Date(paste0(currentYear-5,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-4,"-07-01")) ~ "Primary 1",
                                DateOfBirth >= as.Date(paste0(currentYear-6,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-5,"-07-01")) ~ "Primary 2",
                                DateOfBirth >= as.Date(paste0(currentYear-7,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-6,"-07-01")) ~ "Primary 3",
                                DateOfBirth >= as.Date(paste0(currentYear-8,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-7,"-07-01")) ~ "Primary 4",
                                DateOfBirth >= as.Date(paste0(currentYear-9,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-8,"-07-01")) ~ "Primary 5",
                                DateOfBirth >= as.Date(paste0(currentYear-10,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-9,"-07-01")) ~ "Primary 6",
                                DateOfBirth >= as.Date(paste0(currentYear-11,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-10,"-07-01")) ~ "Primary 7",
                                DateOfBirth >= as.Date(paste0(currentYear-12,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-1,"-07-01")) ~ "Year 8",
                                DateOfBirth >= as.Date(paste0(currentYear-13,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-12,"-07-01")) ~ "Year 9",
                                DateOfBirth >= as.Date(paste0(currentYear-14,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-13,"-07-01")) ~ "Year 10",  
                                DateOfBirth >= as.Date(paste0(currentYear-15,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-14,"-07-01")) ~ "Year 11",  
                                DateOfBirth >= as.Date(paste0(currentYear-16,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-15,"-07-01")) ~ "Year 12",
                                DateOfBirth >= as.Date(paste0(currentYear-17,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-16,"-07-01")) ~ "Year 13",
                                DateOfBirth >= as.Date(paste0(currentYear-18,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-17,"-07-01")) ~ "Year 14")) %>% 
  mutate(SchoolYear = ifelse(InstitutionType %in% "Special", "Special Needs", SchoolYear)) %>% 
  mutate(SchoolYear = ifelse(InstitutionType %in% "Primary" & DateOfBirth < as.Date(paste0(currentYear-12,"-07-01")), "Outlier", SchoolYear)) %>%  #this is if they are an older student in a primary setting, may be staff/placement/special needs
  mutate(SchoolYear = ifelse(AgeAtPositiveResult >= 19,  "Staff", SchoolYear)) %>%
  mutate(SchoolYear = ifelse(InstitutionType %in% "Secondary" & AgeAtPositiveResult >= 18, "Year 14", SchoolYear)) %>%
  mutate(SchoolYear = ifelse(InstitutionType %in% "Grammar" & AgeAtPositiveResult >= 18, "Year 14", SchoolYear)) %>%
  mutate(SchoolYear = ifelse(InstitutionType %in% "Further Education", "FE Student", SchoolYear))


# Add WGS data
schools_cases_w_wgs <- left_join(schools_cases, wgscases, by = "ContactId")

# Generate some stats about each school
schools_cases_stats <- schools_cases_w_wgs %>%
  group_by(InstitutionReferenceNumber) %>%
  dplyr::summarise(
    TotalCases = n(), 
    TotalCloseContacts = sum(CloseContactCount, na.rm = TRUE),
    CasesPrev28Days = sum(DateOfSampleCases >= twentyeightdays, na.rm = TRUE),
    CasesWithinLast3Days = sum(DateOfSampleCases <= today & DateOfSampleCases >= seventyTwoHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSampleCases < seventyTwoHours & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSampleCases <= today & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CaseTrend = case_when(
      CasesWithinLast3Days > CasesWithinLast4to6Days ~ 'Up',
      CasesWithinLast3Days < CasesWithinLast4to6Days ~ 'Down',
      CasesWithinLast3Days == CasesWithinLast4to6Days ~ 'Stable'),
    EarliestOnset = date(min(DateOfOnset, na.rm = TRUE)),
    EarliestSample = date(min(DateOfSampleCases, na.rm = TRUE)),
    EarliestResult = date(min(DateOfResult, na.rm = TRUE)),
    MostRecentOnset = date(max(DateOfOnset, na.rm = TRUE)),
    MostRecentSample = date(max(DateOfSampleCases, na.rm = TRUE)),
    MostRecentResult = date(max(DateOfResult, na.rm = TRUE)),
    MinAge = min(AgeAtPositiveResult, na.rm= TRUE), 
    MaxAge = max(AgeAtPositiveResult, na.rm= TRUE),
    MedianAge = median(AgeAtPositiveResult, na.rm= TRUE),
    MaleCases = sum(GenderCases == "Male", na.rm= TRUE),
    FemaleCases = sum(GenderCases == "Female", na.rm= TRUE),
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

# Add 28 Day Attack Rate (%)
schools_stats_overall <- schools_stats_overall %>% 
  mutate(AttackRate = round((CasesPrev28Days/TotalPupils)*100, digits = 2))

