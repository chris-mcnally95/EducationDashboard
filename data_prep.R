
#----------------------------------------------------------------#
# Data table preparation
#----------------------------------------------------------------#

##### Time Variables ##### 
today <- Sys.Date()
yesterday <- today - 1
oneWeek <- today - 7
fourteendays <- today - 14
twentyeightdays <- today - 28

twentyFourHours <- today - 1
fourtyEightHours <- today - 2
seventyTwoHours <- today - 3
ninetySixHours <- today - 4
oneHundredFourtyFourHours <- today - 6

currentYear <- lubridate::year(today)

##### Connect To Synapse ##### 
synapse_server <- "swhscphipprduks01.sql.azuresynapse.net"
synapse_database <- "exploratorydb"
connection_driver <- "{ODBC Driver 17 for SQL Server}"

con <- DBI::dbConnect(odbc::odbc(),
                      driver = connection_driver,
                      database = synapse_database,
                      Authentication="ActiveDirectoryMSI",
                      server = synapse_server)

##### Table Functions #####
# Standard Query
getTable <- function(table) {
  query <- paste("SELECT * FROM", table)
  data <- DBI::dbGetQuery(con, query)
  message(paste0("Data retrieved from ", table))
  return(data)
}

# Adjusted Table Function Using Standard SQL Language
getTableFiltered <- function(table) {
  query <- paste("SELECT * FROM", table, "WHERE ('CreatedOn' >= '20210830')")
  print(query)
  data <- DBI::dbGetQuery(con, query)
  message(paste0("Data retrieved from ", table))
  return(data)
}

# Combined Schools Table Function Using dplyr (requires dbplyr loaded)
getTableFilteredCombined <- function(table1, table2, table3) {
  
  short_locations <- function(x) { 
    dplyr::tbl(con, x) %>% 
      dplyr::filter(CreatedOn >= '20210830',
                    TypeOfPlace == "School or college") %>% 
      dplyr::select(CollectCallId,
                    TypeOfPlace,
                    InstitutionReferenceNumber,
                    InstitutionType,
                    InstitutionName,
                    City)
  }
  
  short_collectcontactcalls <- function(x) {
    dplyr::tbl(con, x) %>% 
      dplyr::filter(CreatedOn >= '20210830') %>%  
      dplyr::select(Id, 
                    CaseNumber)
  } 
  
  short_cases <- function(x) {
    dplyr::tbl(con, x) %>% 
      dplyr::filter(CreatedOn >= '20210830') %>%  
      dplyr::select(CaseNumber,
                    PostCode,
                    CaseFileStatus,
                    AgeAtPositiveResult,
                    DateOfOnset,
                    DateOfSample,
                    DateOfResult,
                    DateOfBirth,
                    CreatedOn,
                    AddressLine1,
                    AddressLine2,
                    AddressLine3,
                    ContactId,
                    Cluster1Id,
                    Cluster2Id,
                    Cluster3Id,
                    Cluster1AdditionDate,
                    Cluster2AdditionDate,
                    Cluster3AdditionDate,
                    Cluster1Name,
                    Cluster2Name,
                    Cluster3Name,
                    CloseContactCount,
                    Gender,
                    FirstName,
                    LastName) 
  }
  
  query <- short_locations(table1) %>%
    dplyr::left_join(short_collectcontactcalls(table2),
                     by = c("CollectCallId" = "Id"),
                     suffix = c("Locations", "CollectCloseContacts")) %>%
    dplyr::left_join(short_cases(table3), 
                     by = "CaseNumber", 
                     suffix = c("Merged", "Cases")) %>% 
    dplyr::filter(#!is.na(CaseNumber),
                  #CaseFileStatus != 'Cancelled'
      ) 
  
  dplyr::show_query(query)
  data <- as.data.frame(query)
  message(paste0("Successfully retrieved data from ", table1, " & ", table2, " & ", table3, ". Filtered from 20210830 and School or College"))
  return(data)
}

# Get all case postcodes Table Function Using dplyr (requires dbplyr loaded)
getTableFilteredPostcode <- function(table1) {
  
  short_cases <- function(x) {
    dplyr::tbl(con, x) %>%
      dplyr::filter(DateOfSample >= '20210830') %>% 
      dplyr::select(CaseNumber,
                    PostCode,
                    CaseFileStatus, 
                    DateOfSample,
                    CreatedOn)
  }
    
  query <- short_cases(table1) %>% 
    dplyr::filter(!is.na(CaseNumber),
                  CaseFileStatus != 'Cancelled'
                  ) 
  
  dplyr::show_query(query)
  data <- as.data.frame(query)
  message(paste0("Successfully retrieved postcode data from ", table1, ". Filtered from 20210830"))
  return(data)
}

##### Execute SQL Functions ##### 
CombinedQueryTables <- getTableFilteredCombined("Locations", "CollectContactsCalls", "Cases")
AllCasesPostcode <- getTableFilteredPostcode("Cases")
wgscases <- getTableFiltered("Wgscases")
cluster_cases <- getTableFiltered("ClusterCases")

##### Load in the school stat RDS ##### 
#schools_stats <- readRDS("schools_stats.RDS") # these were inserted into /data file using usethis::

##### Build the schools cases table #####
schools_cases <- CombinedQueryTables %>% 
  # Pull DENI number from AddressLineMerged1,2,3 where InstitutionReferenceNumber is blank.
  dplyr::mutate(
    InstitutionReferenceNumber = dplyr::case_when(
      is.na(InstitutionReferenceNumber) & stringr::str_detect(AddressLine3, "\\d\\d\\d-\\d\\d\\d\\d") ~ AddressLine3,
      is.na(InstitutionReferenceNumber) & stringr::str_detect(AddressLine2, "\\d\\d\\d-\\d\\d\\d\\d") ~ AddressLine2,
      is.na(InstitutionReferenceNumber) & stringr::str_detect(AddressLine1, "\\d\\d\\d-\\d\\d\\d\\d") ~ AddressLine1,
      TRUE ~ InstitutionReferenceNumber)) %>%
  # remove duplicates
  dplyr::distinct(CaseNumber, .keep_all = T)

##### Fix DENI number ##### 
schools_cases <- schools_cases %>%
  dplyr::mutate(InstitutionReferenceNumber = gsub('-', '', InstitutionReferenceNumber))

##### Add School Year variable ##### 
schools_cases <- schools_cases %>% 
  dplyr::mutate(
    DateOfBirth = lubridate::date(lubridate::as_datetime(DateOfBirth)),
    PositiveInEpiweek = lubridate::isoweek(DateOfSample),
    PositiveInYear = lubridate::isoyear(DateOfSample)) %>%
  dplyr::mutate(SchoolYear = dplyr::case_when(DateOfBirth >= as.Date(paste0(currentYear-2,"-07-02")) ~ "Pre-Nursery",
                                       DateOfBirth >= as.Date(paste0(currentYear-3,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-2,"-07-01")) ~ "Nursery",
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
                                       DateOfBirth >= as.Date(paste0(currentYear-18,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-17,"-07-01")) ~ "Year 14",
                                       TRUE ~ "Outlier")) %>% 
  dplyr::mutate(SchoolYear = ifelse(InstitutionType %in% "Special", "Special Needs", SchoolYear)) %>% 
  dplyr::mutate(SchoolYear = ifelse(InstitutionType %in% "Primary" & DateOfBirth < as.Date(paste0(currentYear-12,"-07-01")), "Outlier", SchoolYear)) %>%  #this is if they are an older student in a primary setting, may be staff/placement/special needs
  dplyr::mutate(SchoolYear = ifelse(InstitutionType %in% "Secondary" & AgeAtPositiveResult >= 18, "Year 14", SchoolYear)) %>%
  dplyr::mutate(SchoolYear = ifelse(InstitutionType %in% "Grammar" & AgeAtPositiveResult >= 18, "Year 14", SchoolYear)) %>%
  dplyr::mutate(SchoolYear = ifelse(AgeAtPositiveResult >= 19,  "Staff", SchoolYear)) %>%
  dplyr::mutate(SchoolYear = ifelse(InstitutionType %in% "Further Education", "FE Student", SchoolYear)) %>% 
  dplyr::mutate(SchoolYear = factor(SchoolYear, levels = c("Pre-Nursery", "Nursery", "Reception", "Primary 1", "Primary 2", "Primary 3", "Primary 4", "Primary 5",
                                                           "Primary 6", "Primary 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "Year 14",
                                                           "Special Needs", "FE Student", "Outlier", "Staff")))


#####  Add WGS data ##### 
schools_cases_w_wgs <- dplyr::left_join(schools_cases, wgscases, by = "ContactId", suffix = c("Merged", "WGS")) #no cases from wgs join, don't know why

##### School Clusters ##### 
schools_cases_w_clusters <- dplyr::left_join(schools_cases_w_wgs, cluster_cases, by = "ContactId", suffix = c("SC", "ClusterCases"))

# Create old schools cluster cases from existing schools_cases
# These are schools cases with clusterID that are in Cluster1Id,Cluster2Id,Cluster3Id
c1_cluster_cases <- schools_cases_w_clusters %>%
  dplyr::filter(!is.na(Cluster1Id)) %>%
  dplyr::mutate(ClusterID = Cluster1Id,
                AdditionDate = Cluster1AdditionDate,
                ClusterName = Cluster1Name)

c2_cluster_cases <- schools_cases_w_clusters %>%
  dplyr::filter(!is.na(Cluster2Id)) %>%
  dplyr::mutate(ClusterID = Cluster2Id,
                AdditionDate = Cluster2AdditionDate,
                ClusterName = Cluster2Name)

c3_cluster_cases <- schools_cases_w_clusters %>%
  dplyr::filter(!is.na(Cluster3Id)) %>%
  dplyr::mutate(ClusterID = Cluster3Id,
                AdditionDate = Cluster3AdditionDate,
                ClusterName = Cluster3Name)

old_cluster_cases_1and2 <- rbind(c1_cluster_cases, c2_cluster_cases)
old_schools_cluster_cases <- rbind(old_cluster_cases_1and2, c3_cluster_cases)

schools_cases_w_clusters <- dplyr::left_join(schools_cases_w_clusters, old_schools_cluster_cases)

schools_cases_w_clusters <- schools_cases_w_clusters %>%
  dplyr::mutate(CreatedOn = lubridate::date(lubridate::as_datetime(CreatedOn)),
                DateOfSampleCases = lubridate::date(lubridate::as_datetime(DateOfSample)),
                AdditionDate = lubridate::as_datetime(AdditionDate),
                DateOfBirth = lubridate::date(lubridate::as_date(DateOfBirth)))

#####  Postcode District Grouping #####
# Group schools by postcode district 
school_pcd_clusters <- schools_cases_w_clusters %>%
  dplyr::mutate(PostcodeDistrict = gsub(".{4}$", "", PostCode)) %>% 
  dplyr::group_by(PostcodeDistrict) %>%
  dplyr::summarise(
    TotalSchoolCases = dplyr::n(),
    TotalPossibleCloseContacts = sum(CloseContactCount),
    EarliestOnset = lubridate::date(min(DateOfOnsetSC, na.rm = TRUE)),
    EarliestSample = lubridate::date(min(DateOfSample, na.rm = TRUE)),
    EarliestResult = lubridate::date(min(DateOfResult, na.rm = TRUE)),
    MostRecentOnset = lubridate::date(max(DateOfOnsetSC, na.rm = TRUE)),
    MostRecentSample = lubridate::date(max(DateOfSample, na.rm = TRUE)),
    MostRecentResult = lubridate::date(max(DateOfResult, na.rm = TRUE)),
    MostRecentAdditionDate = lubridate::date(max(AdditionDate, na.rm = TRUE)),
    MedianAge = median(AgeAtPositiveResultClusterCases),
    CasesPrev28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today, na.rm = TRUE),
    CasesPrev14Days = sum(DateOfSample >= fourteendays & DateOfSample < today, na.rm = TRUE),
    CasesPrev7Days = sum(DateOfSample >= oneWeek & DateOfSample < today, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSample <= today & DateOfSample >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSample < seventyTwoHours & DateOfSample >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast3Days = sum(DateOfSample <= today & DateOfSample >= seventyTwoHours, na.rm = TRUE),
    CaseTrend = dplyr::case_when(
      CasesWithinLast3Days > CasesWithinLast4to6Days ~  'Up',
      CasesWithinLast3Days < CasesWithinLast4to6Days ~ 'Down',
      CasesWithinLast3Days == CasesWithinLast4to6Days ~ 'Stable'),
    MinAge = min(AgeAtPositiveResultClusterCases), 
    MaxAge = max(AgeAtPositiveResultClusterCases),
    MaleCases = sum(Gender == "Male"),
    FemaleCases = sum(Gender == "Female"),
    AlphaVariants = sum(WgsVariant == "VOC-20DEC-01", na.rm = TRUE),
    BetaVariants = sum(WgsVariant == "VOC-20DEC-02", na.rm = TRUE),
    DeltaVariants = sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    DeltaReflex = sum(WgsReflexAssay == "Delta", na.rm = TRUE) + sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    #DeltaReflex = sum(str_detect(WgsReferralOtherReason, "Reflex|REFLEX|reflex") == TRUE, na.rm = TRUE),
    `VOC-21FEB-02Variants` = sum(WgsVariant == "E484K", na.rm = TRUE),
    KappaVariants = sum(WgsVariant == "VUI-21APR-01", na.rm = TRUE),
    `VUI-21FEB-04Variants` = sum(WgsVariant == "VUI-21FEB-04", na.rm = TRUE),
    `VUI-21MAY-02Variants` = sum(WgsVariant == "VUI-21MAY-02", na.rm = TRUE)) 

# Group all cases by postcode district
all_cases_pcd <- AllCasesPostcode %>%
  dplyr::mutate(PostcodeDistrict = gsub(".{4}$", "", PostCode)) %>% 
  dplyr::group_by(PostcodeDistrict) %>%
  dplyr::summarise(
    TotalCases = dplyr::n(),
    CasesPrev28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today, na.rm = TRUE),
    CasesPrev7Days = sum(DateOfSample >= oneWeek & DateOfSample < today, na.rm = TRUE))
  
#####  Generate some stats about each school ##### 
schools_cases_stats <- schools_cases_w_wgs %>%
  dplyr::group_by(InstitutionReferenceNumber) %>%
  dplyr::summarise(
    TotalCases = dplyr::n(), 
    TotalCloseContacts = sum(CloseContactCount, na.rm = TRUE),
    CloseContacts28Days = sum(CloseContactCount > 0 & DateOfSample >= twentyeightdays & DateOfSample < today, na.rm = TRUE),
    CasesPrev28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today, na.rm = TRUE),
    CasesPrev14Days = sum(DateOfSample >= fourteendays & DateOfSample < today, na.rm = TRUE),
    CasesPrev7Days = sum(DateOfSample >= oneWeek & DateOfSample < today, na.rm = TRUE),
    CasesWithinLast3Days = sum(DateOfSample < today & DateOfSample >= seventyTwoHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSample < seventyTwoHours & DateOfSample >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSample < today & DateOfSample >= oneHundredFourtyFourHours, na.rm = TRUE),
    CaseTrend = dplyr::case_when(
      CasesWithinLast3Days > CasesWithinLast4to6Days ~ 'Up',
      CasesWithinLast3Days < CasesWithinLast4to6Days ~ 'Down',
      CasesWithinLast3Days == CasesWithinLast4to6Days ~ 'Stable'),
    EarliestOnset = lubridate::date(min(DateOfOnset, na.rm = TRUE)),
    EarliestSample = lubridate::date(min(DateOfSample, na.rm = TRUE)),
    EarliestResult = lubridate::date(min(DateOfResult, na.rm = TRUE)),
    MostRecentOnset = lubridate::date(max(DateOfOnset, na.rm = TRUE)),
    MostRecentSample = lubridate::date(max(DateOfSample, na.rm = TRUE)),
    MostRecentResult = lubridate::date(max(DateOfResult, na.rm = TRUE)),
    MinAge = min(AgeAtPositiveResult, na.rm= TRUE), 
    MaxAge = max(AgeAtPositiveResult, na.rm= TRUE),
    MedianAge = median(AgeAtPositiveResult, na.rm= TRUE),
    MaleCases = sum(Gender == "Male", na.rm= TRUE),
    FemaleCases = sum(Gender == "Female", na.rm= TRUE),
    NurseryCases = sum(SchoolYear == "Nursery", na.rm = TRUE),
    ReceptionCases = sum(SchoolYear == "Reception", na.rm = TRUE),
    NurseryCases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Nursery", na.rm = TRUE),
    ReceptionCases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Reception", na.rm = TRUE),
    Y1Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 1", na.rm = TRUE),
    Y2Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 2", na.rm = TRUE),
    Y3Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 3", na.rm = TRUE),
    Y4Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 4", na.rm = TRUE),
    Y5Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 5", na.rm = TRUE),
    Y6Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 6", na.rm = TRUE),
    Y7Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Primary 7", na.rm = TRUE),
    Y8Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 8", na.rm = TRUE),
    Y9Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 9", na.rm = TRUE),
    Y10Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 10", na.rm = TRUE),
    Y11Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 11", na.rm = TRUE),
    Y12Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 12", na.rm = TRUE),
    Y13Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 13", na.rm = TRUE),
    Y14Cases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Year 14", na.rm = TRUE),
    StaffCases7Days = sum(DateOfSample >= oneWeek & DateOfSample < today & SchoolYear == "Staff", na.rm = TRUE),
    StaffCases14Days = sum(DateOfSample >= fourteendays & DateOfSample < today & SchoolYear == "Staff", na.rm = TRUE),
    StaffCases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Staff", na.rm = TRUE),
    OCases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Outlier", na.rm = TRUE),
    SNCases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "Special Needs", na.rm = TRUE),
    FECases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear == "FE Student", na.rm = TRUE),
    PupilCases7Days = sum(DateOfSample >= oneWeek & DateOfSample < today & SchoolYear != "Outlier" & SchoolYear != "Staff", na.rm = TRUE),
    PupilCases14Days = sum(DateOfSample >= fourteendays & DateOfSample < today & SchoolYear != "Outlier" & SchoolYear != "Staff", na.rm = TRUE),
    PupilCases28Days = sum(DateOfSample >= twentyeightdays & DateOfSample < today & SchoolYear != "Outlier" & SchoolYear != "Staff", na.rm = TRUE),
    AlphaVariants = sum(WgsVariant == "VOC-20DEC-01", na.rm = TRUE),
    BetaVariants = sum(WgsVariant == "VOC-20DEC-02", na.rm = TRUE),
    DeltaVariants = sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    DeltaReflex = sum(WgsReflexAssay == "Delta", na.rm = TRUE) + sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    #DeltaReflex = sum(str_detect(WgsReferralOtherReason, "Reflex|REFLEX|reflex") == TRUE, na.rm = TRUE),
    `VOC-21FEB-02Variants` = sum(WgsVariant == "E484K", na.rm = TRUE),
    KappaVariants = sum(WgsVariant == "VUI-21APR-01", na.rm = TRUE),
    `VUI-21FEB-04Variants` = sum(WgsVariant == "VUI-21FEB-04", na.rm = TRUE),
    `VUI-21MAY-02Variants` = sum(WgsVariant == "VUI-21MAY-02", na.rm = TRUE))

#####  School_Stats_Overall ##### 
schools_stats_overall <- dplyr::left_join(schools_stats, schools_cases_stats, by = c("DENINumber" = "InstitutionReferenceNumber")) %>% 
  tidyr::drop_na(DENINumber) %>% 
  dplyr::mutate(Quotient = TotalPupils/50,
                Prevalance50 = TotalCases/Quotient,
                PostcodeDistrict = gsub(".{4}$", "", Postcode))

#####  Add 28 Day Attack Rate (%) ##### 
schools_stats_overall <- schools_stats_overall %>% 
  dplyr::mutate(
    AttackRate7Days = round((CasesPrev7Days/(TotalPupils+StaffCases7Days))*100, digits = 2),
    AttackRate14Days = round((CasesPrev14Days/(TotalPupils+StaffCases14Days))*100, digits = 2),
    AttackRate28Days = round((CasesPrev28Days/(TotalPupils+StaffCases28Days))*100, digits = 2),
    AttackRatePupils7Days = round((PupilCases7Days/(TotalPupils))*100, digits = 2),
    AttackRatePupils14Days = round((PupilCases14Days/(TotalPupils))*100, digits = 2),
    AttackRatePupils28Days = round((PupilCases28Days/(TotalPupils))*100, digits = 2),
    AttackRateNursery = round((NurseryCases28Days/NurseryPupils)*100, digits = 2),
    AttackRateReception = round((ReceptionCases28Days/ReceptionPupils)*100, digits = 2),
    AttackRateY1 = round((Y1Cases28Days/Year1)*100, digits = 2),
    AttackRateY2 = round((Y2Cases28Days/Year2)*100, digits = 2),
    AttackRateY3 = round((Y3Cases28Days/Year3)*100, digits = 2),
    AttackRateY4 = round((Y4Cases28Days/Year4)*100, digits = 2),
    AttackRateY5 = round((Y5Cases28Days/Year5)*100, digits = 2),
    AttackRateY6 = round((Y6Cases28Days/Year6)*100, digits = 2),
    AttackRateY7 = round((Y7Cases28Days/Year7)*100, digits = 2),
    AttackRateY8 = round((Y8Cases28Days/Year8)*100, digits = 2),
    AttackRateY9 = round((Y9Cases28Days/Year9)*100, digits = 2),
    AttackRateY10 = round((Y10Cases28Days/Year10)*100, digits = 2),
    AttackRateY11 = round((Y11Cases28Days/Year11)*100, digits = 2),
    AttackRateY12 = round((Y12Cases28Days/Year12)*100, digits = 2),
    AttackRateY13 = round((Y13Cases28Days/Year13)*100, digits = 2),
    AttackRateY14 = round((Y14Cases28Days/Year14)*100, digits = 2),
    AttackRateSN = round((SNCases28Days/TotalPupils)*100, digits = 2))

#### LGD Data ####
#Add  postcode district and join with LGD and ward data
schools_stats_overall$BT_area <- schools_stats_overall$Postcode
schools_stats_overall$BT_area <- gsub(' ', '', schools_stats_overall$BT_area)
schools_stats_overall$BT_area <- toupper(schools_stats_overall$BT_area)
schools_stats_overall$BT_area <- as.character(gsub('.{3}$', '', schools_stats_overall$Postcode))
schools_stats_overall$BT_area <- stringr::str_trim(schools_stats_overall$BT_area, side = "both")

#PCD_LGD <- readRDS("ward_PCD_LGD.RDS") # these were inserted into data file using usethis::

PCD_LGD <- ward_PCD_LGD %>%
  dplyr::select("BT_area", "LGDName") %>%
  dplyr::distinct(BT_area, .keep_all = TRUE)

schools_stats_overall <- dplyr::full_join(schools_stats_overall, PCD_LGD, by = "BT_area") %>% 
  tidyr::drop_na(DENINumber)

#### Statistical Modelling ####

# # Free School Meals
# cor.test(schools_stats_overall$TotalCases, schools_stats_overall$NumberFSME)
# 
# # Case wise
# stats_cases_df <- schools_cases %>% 
#   dplyr::select(PostCodeCases) %>% 
#   dplyr::mutate(PostCodeCases = gsub(".{4}$", "", PostCodeCases)) %>% 
#   dplyr::left_join(PCD_LGD, by = c("PostCodeCases" = "BT_area")) %>% 
#   dplyr::group_by(LGDName) %>% 
#   dplyr::summarise(TotalCases = dplyr::n()) %>% 
#   dplyr::left_join(NI_Multiple_Deprivation_Measure_14, by = c("LGDName" = "LGD2014")) %>% 
#   tidyr::drop_na(LGDName)
# 
# correlation_results.cases <- correlation::correlation(data = stats_cases_df[, 2], 
#                                                       data2 = stats_cases_df[, c(4:40)],
#                                                       method = "auto")
# 
# # School wise
# stats_schools_df <- schools_stats_overall %>% 
#   dplyr::select(TotalPupils,
#                 TotalCases, 
#                 CasesPrev28Days,
#                 LGDName) %>% 
#   dplyr::group_by(LGDName) %>% 
#   dplyr::summarise_at(c("TotalPupils", "TotalCases", "CasesPrev28Days"), sum, na.rm = T) %>% 
#   dplyr::left_join(NI_Multiple_Deprivation_Measure_14, by = c("LGDName" = "LGD2014")) %>% 
#   tidyr::drop_na(LGDName) %>% 
#   dplyr::mutate(Quotient = TotalPupils/50) %>% 
#   dplyr::mutate(Prevalence50 = TotalCases/Quotient)
# 
# ## Correlation 
# correlation.df <- stats_df %>% 
#    dplyr::select(-c(
#      TotalPupils,
#      LGDName,
#      TotalCases,
#      CasesPrev28Days,
#      Population,
#      Quotient))
# 
# correlation_results.schools <- correlation::correlation(data = correlation.df[, 38], 
#                                                         data2 = correlation.df[, -38],
#                                                         method = "auto")

##### Community Close Contacts #####  

# Adjusted Table Function to reduce load times

getCloseContactCallsTableFiltered <- function(table) {
  
  closecontactcallshort <- function(x) {
    dplyr::tbl(con, x) %>%
      dplyr::select(FirstName,
                    LastName,
                    ContactPhoneNumber,
                    CaseNumber,
                    DateOfLastContact,
                    CreatedOn) %>% 
      dplyr::filter(CreatedOn >= '20210830')
  }
  
  query <- closecontactcallshort(table)   

  dplyr::show_query(query)
  data <- as.data.frame(query)
  message(paste0("Successfully retrieved Data from ", table, ". Filtered from 20210830"))
  return(data)
}

closecontactcalls <- getCloseContactCallsTableFiltered("CloseContactCalls")

## shrink the size of closecontactcalls for only contacts associated with school cases
closecontactcalls <- closecontactcalls  %>%
  dplyr::filter(CaseNumber %in% schools_cases_w_wgs$CaseNumber)

## get DENI and casenumbers from schools_cases_w_wgs
case_numbers_and_deni <- schools_cases_w_wgs %>%
  dplyr::select(
    CaseNumber,
    InstitutionReferenceNumber)

## Join Data Frames
close_contacts_for_schools <- dplyr::left_join(closecontactcalls, case_numbers_and_deni, by = "CaseNumber")


#### Case rate per LGD/PCD ####
PCDpops <- PCD2018_populations %>%
  as.data.frame() %>%
  dplyr::mutate(Postcode = `Postcode District`) %>%
  dplyr::select('0_19':'Postcode')

LGDpops <- LGDpops %>%
  as.data.frame() %>%
  dplyr::select('area_name', 'MYE')

PCDnames <-  c("0_19", "20_39", "40_59", "60_79", "80plus", "All.Ages")
#PCDpops[PCDnames] <- sapply(PCDpops[PCDnames], gsub, pattern= ",", replacement= "")

PCDpops <- PCDpops %>%
  dplyr::mutate(`0_19` = as.numeric(`0_19`),
                `20_39` = as.numeric(`20_39`), 
                `40_59` = as.numeric(`40_59`), 
                `60_79` = as.numeric(`60_79`), 
                `80plus` = as.numeric(`80plus`),
                `All Ages` = as.numeric(`All Ages`))


PCDpops[is.na(PCDpops)] <- 0

#Filter cases by week
#group cases by SPC & LGD
#join to pcd & lgd pops
#calculate case rate
#join with schools stats
cases4weeks = AllCasesPostcode %>%
  dplyr::filter(DateOfSample >= twentyeightdays - 1 & DateOfSample < today) %>%
  tidyr::drop_na(PostCode) %>%
  dplyr::mutate(DateOfSample = as.Date(DateOfSample),
                PostCode = toupper(PostCode),
                BT_area = gsub(".{4}$", "", PostCode)) %>%
  dplyr::select(CaseNumber,
                DateOfSample,
                BT_area) %>% 
  dplyr::full_join(PCD_LGD, by = "BT_area") %>% 
  dplyr::mutate(LGDName = dplyr::case_when(
    is.na(LGDName) ~ "Unknown",
    TRUE ~ LGDName
  ))

##join PCD to the cases and group the cases by PCD and LGD
#4weeks
# cases4weeksPCD <- cases4weeks %>% 
#   dplyr::group_by(BT_area, .drop = FALSE) %>%
#   dplyr::count()

cases4weeksLGD <- cases4weeks %>%
  dplyr::group_by(LGDName, .drop = FALSE) %>%
  dplyr::count()

# colnames(cases4weeksPCD)[colnames(cases4weeksPCD) == "n"] <- "CasesLast4Week"
colnames(cases4weeksLGD)[colnames(cases4weeksLGD) == "n"] <- "CasesLast4Week"

#2 weeks
# cases2weeksPCD <- cases4weeks %>% 
#   dplyr::filter(DateOfSample > fourteendays & DateOfSample < today) %>% 
#   dplyr::group_by(BT_area, .drop = FALSE) %>%
#   dplyr::count()

cases2weeksLGD <- cases4weeks %>%
  dplyr::filter(DateOfSample > fourteendays -1 & DateOfSample < today) %>% 
  dplyr::group_by(LGDName, .drop = FALSE) %>%
  dplyr::count()

# colnames(cases2weeksPCD)[colnames(cases2weeksPCD) == "n"] <- "CasesLast2Week"
colnames(cases2weeksLGD)[colnames(cases2weeksLGD) == "n"] <- "CasesLast2Week"

#1week
# cases1weekPCD <- cases4weeks %>% 
#   dplyr::filter(DateOfSample > oneWeek & DateOfSample < today) %>%
#   dplyr::group_by(BT_area, .drop = FALSE) %>%
#   dplyr::count()

cases1weekLGD <- cases4weeks %>%
  dplyr::filter(DateOfSample > oneWeek -1 & DateOfSample < today) %>%
  dplyr::group_by(LGDName, .drop = FALSE) %>%
  dplyr::count()

# colnames(cases1weekPCD)[colnames(cases1weekPCD) == "n"] <- "CasesLastWeek"
colnames(cases1weekLGD)[colnames(cases1weekLGD) == "n"] <- "CasesLastWeek"

##calculate case rate
#4 weeks
# casesrate4weeksPCD <- dplyr::left_join(cases4weeksPCD, PCDpops %>% 
#                                          dplyr::select(`All Ages`,
#                                                 Postcode), by = c("BT_area" = "Postcode")) %>% 
#   dplyr::rename("Cases4WeeksPCD" = "CasesLast4Week",
#                 "TotalPop4weeksPCD" = "All Ages") %>% 
#   dplyr::mutate(Quotient4WeeksPCD = TotalPop4weeksPCD/100000,
#                 Prevalance100k4WeeksPCD = round((Cases4WeeksPCD/Quotient4WeeksPCD), 1))

casesrate4weeksLGD <- dplyr::left_join(cases4weeksLGD, LGDpops, by = c("LGDName" = "area_name")) %>%
  dplyr::rename("Cases4WeeksLGD" = "CasesLast4Week",
                "TotalPop4WeeksLGD" = "MYE") %>% 
  dplyr::mutate(Quotient4WeeksLGD = TotalPop4WeeksLGD/100000,
                Prevalance100k4WeeksLGD = round((Cases4WeeksLGD/Quotient4WeeksLGD), 1))

#2 weeks
# casesrate2weeksPCD <- dplyr::left_join(cases2weeksPCD, PCDpops %>% 
#                                          dplyr::select(`All Ages`,
#                                                        Postcode), by = c("BT_area" = "Postcode")) %>% 
#   dplyr::rename("Cases2WeeksPCD" = "CasesLast2Week",
#                 "TotalPop2weeksPCD" = "All Ages") %>% 
#   dplyr::mutate(Quotient2WeeksPCD = TotalPop2weeksPCD/100000,
#                 Prevalance100k2WeeksPCD = round((Cases2WeeksPCD/Quotient2WeeksPCD), 1))

casesrate2weeksLGD <- dplyr::left_join(cases2weeksLGD, LGDpops, by = c("LGDName" = "area_name")) %>%
  dplyr::rename("Cases2WeeksLGD" = "CasesLast2Week",
                "TotalPop2WeeksLGD" = "MYE") %>% 
  dplyr::mutate(Quotient2WeeksLGD = TotalPop2WeeksLGD/100000,
                Prevalance100k2WeeksLGD = round((Cases2WeeksLGD/Quotient2WeeksLGD), 1))

#1week
# casesrate1weekPCD <- dplyr::left_join(cases1weekPCD, PCDpops %>% 
#                                          dplyr::select(`All Ages`,
#                                                        Postcode), by = c("BT_area" = "Postcode")) %>% 
#   dplyr::rename("Cases1WeekPCD" = "CasesLastWeek",
#                 "TotalPop1weekPCD" = "All Ages") %>% 
#   dplyr::mutate(Quotient1WeekPCD = TotalPop1weekPCD/100000,
#                 Prevalance100k1WeekPCD = round((Cases1WeekPCD/Quotient1WeekPCD), 1))

casesrate1weekLGD <- dplyr::left_join(cases1weekLGD, LGDpops, by = c("LGDName" = "area_name")) %>%
  dplyr::rename("Cases1WeekLGD" = "CasesLastWeek",
                "TotalPop1WeekLGD" = "MYE") %>% 
  dplyr::mutate(Quotient1WeekLGD = TotalPop1WeekLGD/100000,
                Prevalance100k1WeekLGD = round((Cases1WeekLGD/Quotient1WeekLGD), 1))

# Join Case Rate Data Frames
## PCD
# PCDCasesRateFull <- dplyr::left_join(casesrate1weekPCD %>% 
#                                        dplyr::select(BT_area,
#                                                      Prevalance100k1WeekPCD),
#                                      casesrate2weeksPCD %>% 
#                                        dplyr::select(BT_area,
#                                                      Prevalance100k2WeeksPCD), by = "BT_area") %>% 
#   dplyr::left_join(casesrate4weeksPCD %>% 
#                      dplyr::select(BT_area,
#                                    Prevalance100k4WeeksPCD), by = "BT_area") %>% 
#   tidyr::drop_na(BT_area)

## LGD
LGDCasesRateFull <- dplyr::left_join(casesrate1weekLGD %>% 
                                       dplyr::select(LGDName,
                                                     Prevalance100k1WeekLGD),
                                     casesrate2weeksLGD %>% 
                                       dplyr::select(LGDName,
                                                     Prevalance100k2WeeksLGD), by = "LGDName") %>% 
  dplyr::left_join(casesrate4weeksLGD %>% 
                     dplyr::select(LGDName,
                                   Prevalance100k4WeeksLGD), by = "LGDName") %>% 
  tidyr::drop_na()

#add to schools data
# schools_stats_overall <- dplyr::left_join(schools_stats_overall, PCDCasesRateFull, by = "BT_area")
schools_stats_overall <- dplyr::left_join(schools_stats_overall, LGDCasesRateFull, by = "LGDName")

#Calculate ni caserate
NICases7days <- as.numeric(colSums(cases1weekLGD[ , c(2)], na.rm=TRUE))
NICases14days <- as.numeric(colSums(cases2weeksLGD[ , c(2)], na.rm=TRUE))
NICases28days <- as.numeric(colSums(cases4weeksLGD[ , c(2)], na.rm=TRUE))
NITotalPop <- as.numeric(colSums(casesrate2weeksLGD[ , c(3)], na.rm=TRUE))

NI7dayrate <- as.numeric(round(NICases7days/(NITotalPop/100000), digits = 1))
NI14dayrate <- as.numeric(round(NICases14days/(NITotalPop/100000), digits = 1))
NI28dayrate <- as.numeric(round(NICases28days/(NITotalPop/100000), digits = 1))

#### End Script ####

DBI::dbDisconnect(con)

message("Data preparation script successfuly executed")
