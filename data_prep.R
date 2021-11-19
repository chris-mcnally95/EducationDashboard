
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

# Get All Cases Info Function Using dplyr (requires dbplyr loaded)
getTableFilteredPostcode <- function(table1, table2) {
  
  short_contacts <- function(x) {
    dplyr::tbl(con, x) %>% 
      dplyr::filter(DateofSample >= "20210830") %>% 
      dplyr::select(Id, 
                    CaseNumber,
                    CallStatus)%>%
      dplyr::rename(CallStatusCollect = CallStatus)
  }
  
  short_cases <- function(x) {
    dplyr::tbl(con, x) %>%
      dplyr::filter(DateOfSample >= '20210830') %>% 
      dplyr::select(CaseNumber,
                    PostCode,
                    DateOfSample,
                    CreatedOn,
                    AgeAtPositiveResult)
  }
  
  query <- short_contacts(table1) %>% 
    dplyr::left_join(short_cases(table2), by = "CaseNumber") %>% 
    dplyr::filter(!is.na(CaseNumber),
                  !is.na(CreatedOn)) 
  
  dplyr::show_query(query)
  data <- as.data.frame(query)
  message(paste0("Successfully retrieved postcode data from ", table1, "&", table2, ". Filtered from 20210830"))
  return(data)
}

##### Execute SQL Functions ##### 
CombinedQueryTables <- getTableFilteredCombined("Locations", "CollectContactsCalls", "Cases")

AllCasesPostcode <- getTableFilteredPostcode("CollectContactsCalls", "Cases") %>%
  dplyr::distinct(CaseNumber, .keep_all = TRUE)

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


#### Case rate per LGD Eimhears Code ####

casesNoDup <- AllCasesPostcode %>%
  dplyr::mutate(CreatedOnCase = lubridate::ymd_hms(CreatedOn)) %>%
  dplyr::distinct(CaseNumber, .keep_all = TRUE)

AllCases <- casesNoDup %>%
  dplyr::filter(!is.na(CreatedOn)) %>%
  dplyr::filter(complete.cases(CaseNumber))

# get all cases with cancelled removed
LGDData <- AllCases %>%
  dplyr::filter(CallStatusCollect != "Canceled") %>%
  dplyr::mutate(PostCode = stringr::str_replace(PostCode, " ", ""))

# read in postcode to LGD/ward mapping table
# postcodes <- read.csv("./postcodes.csv") # added using usethis::use_data

# define LGD spatial data
# DistrictLGD <- read.csv("./DistrictLGD.csv") # added using usethis::use_data

# join the case data to postcode table that allows us to map postcode to lgd code
# select relevant columns
ClusterDistrictLGD <-
  dplyr::left_join(LGDData, postcodes, by = c("PostCode" = "PC5")) %>%
  dplyr::select(CaseNumber, CreatedOnCase, PostCode, LGD2014)

#select the name and code from the lgd data
DistrictCodeLGD <- as.data.frame(DistrictLGD) %>%
  dplyr::select(LGDNAME, LGDCode)

#join name and code to case data to get LGD name
DistrictJoinLGD <-
  dplyr::left_join(ClusterDistrictLGD,
                   DistrictCodeLGD,
                   by = c("LGD2014" = "LGDCode")) %>%
  replace(is.na(.), 0)

# group by LGD name and count cases per LGD
CasesPerLGD <- DistrictJoinLGD %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::mutate(LGDNAME = dplyr::case_when(LGDNAME != "0" ~ LGDNAME,
                                    LGDNAME == "0" ~ "Unknown")) %>%
  dplyr::summarise(Count = dplyr::n())

# remove unknown lgd cases counts
NamesDF <- CasesPerLGD %>%
  dplyr::filter(LGDNAME != "Unknown")

# get the lgdname and stroe as vector
LGDNAME <- as.vector(NamesDF$LGDNAME)

# define lgd population counts
# pulled from kainos python scripts
# assuming from NISRA data
PopulationLGD <- c(143500,
                   161700,
                   216200,
                   343500,
                   144800,
                   151300,
                   117400,
                   146000,
                   148500,
                   139300,
                   181400)

#define variable to hold ni pop value
Population_NI <- 1893700

# lgd population data counts were put in order to match order of name vector
# combine lgdname vector and their lgd pop count
LGDPop <-data.frame(LGDNAME, PopulationLGD)

# get total counts per day and daily cases per 100k
NIData <- LGDData %>%
  dplyr::mutate(Day = as.Date(CreatedOnCase)) %>%
  dplyr::group_by(Day) %>%
  dplyr::summarise(DailyCounts = dplyr::n(),
                   CasesPer100kDaily = round((DailyCounts / Population_NI) * 100000, digits = 1)) %>%
  dplyr::mutate(LGDNAME = "Northern Ireland") %>%
  dplyr::select(Day, LGDNAME, DailyCounts, CasesPer100kDaily)

# left join cases per lgd and lgdpop dataframe to get cases count per lgd
# and overall population per lgd in same table
FinalLGD <-
  dplyr::left_join(CasesPerLGD, LGDPop, by = "LGDNAME") %>%
  dplyr::filter(LGDNAME != "Unknown")

# create cases per 100k per lgd by dividing case count by lgd pop multiplied by 100000
FinalLGD <- FinalLGD %>%
  dplyr::mutate(CountPer100k = round((Count / PopulationLGD) * 100000, digits = 1)) %>%
  dplyr::select(LGDNAME, CountPer100k)

# get daily lgd case count table
CasesPerLGDDaily <- DistrictJoinLGD %>%
  dplyr::mutate(Day = as.Date(CreatedOnCase)) %>%
  dplyr::mutate(LGDNAME = dplyr::case_when(LGDNAME != "0" ~ LGDNAME,
                                    LGDNAME == "0" ~ "Unknown")) %>%
  dplyr::filter(LGDNAME != "Unknown") %>%
  dplyr::group_by(LGDNAME, Day) %>%
  dplyr::summarise(LGDDailyCount = dplyr::n())

# join cases per 100k and leg pop data to get daily lgd cases per 100k daily
FinalDailyCasesper100k <- CasesPerLGDDaily %>%
  dplyr::left_join(LGDPop, by = "LGDNAME") %>%
  dplyr::mutate(CasesPer100kDaily = round((LGDDailyCount / PopulationLGD) * 100000, digits = 1)) %>%
  dplyr::select(Day, LGDNAME, CasesPer100kDaily)

# seven days cumulative
casesper100k.7days <- FinalDailyCasesper100k %>%
  dplyr::filter(Day >= oneWeek - 1 & Day < today) %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::tally(CasesPer100kDaily) %>% 
  dplyr::rename("LGD7Day100k" = n,
                "LGDName" = LGDNAME)

NITotalCasesPer100k.7days <- NIData %>%
  dplyr::filter(Day >= oneWeek - 1 & Day < today) %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::tally(CasesPer100kDaily) %>% 
  dplyr::rename("LGDName" = LGDNAME)
  
# fourteen days cumulative
casesper100k.14days <- FinalDailyCasesper100k %>%
  dplyr::filter(Day >= fourteendays - 1 & Day < today) %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::tally(CasesPer100kDaily) %>% 
  dplyr::rename("LGD14Day100k" = n,
                "LGDName" = LGDNAME)

NITotalCasesPer100k.14days <- NIData %>%
  dplyr::filter(Day >= fourteendays - 1 & Day < today) %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::tally(CasesPer100kDaily) %>% 
  dplyr::rename("LGDName" = LGDNAME)

# twentyeight days cumulative
casesper100k.28days <- FinalDailyCasesper100k %>%
  dplyr::filter(Day >= twentyeightdays - 1 & Day < today) %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::tally(CasesPer100kDaily) %>% 
  dplyr::rename("LGD28Day100k" = n,
                "LGDName" = LGDNAME)

NITotalCasesPer100k.28days <- NIData %>%
  dplyr::filter(Day >= twentyeightdays - 1 & Day < today) %>%
  dplyr::group_by(LGDNAME) %>%
  dplyr::tally(CasesPer100kDaily) %>% 
  dplyr::rename("LGDName" = LGDNAME)

# Join Case Rate Data Frames
LGDCasesRateFull <- dplyr::left_join(casesper100k.7days,
                                     casesper100k.14days,
                                     by = "LGDName") %>%
  dplyr::left_join(casesper100k.28days, by = "LGDName") %>%
  tidyr::drop_na()

NICasesRateFull <- dplyr::left_join(NITotalCasesPer100k.7days,
                                    NITotalCasesPer100k.14days,
                                    by = "LGDName") %>%
  dplyr::left_join(NITotalCasesPer100k.28days, by = "LGDName") %>%
  tidyr::drop_na()

# add to schools data
schools_stats_overall <- dplyr::left_join(schools_stats_overall, LGDCasesRateFull, by = "LGDName")

# NI Rate
NI7dayrate <- as.numeric(NICasesRateFull[1,2])
NI14dayrate <- as.numeric(NICasesRateFull[1,3])
NI28dayrate <- as.numeric(NICasesRateFull[1,4])

#### End Script ####

DBI::dbDisconnect(con)

message("Data preparation script successfuly executed")
