
#----------------------------------------------------------------
# Data table preparation
#----------------------------------------------------------------

# Assign Database Connection
synapse_server <- "swhscphipprduks01.sql.azuresynapse.net"
synapse_database <- "exploratorydb"
connection_driver <- "{ODBC Driver 17 for SQL Server}"

con <- DBI::dbConnect(odbc::odbc(),
                      driver = connection_driver,
                      database = synapse_database,
                      Authentication="ActiveDirectoryMSI",
                      server = synapse_server)

# Table Function
getTable <- function(table) {
  query <- paste("SELECT * FROM", table)
  data <- DBI::dbGetQuery(con, query)
  message(paste0("Data retrieved from ", table))
  return(data)
}

# Standard SQL Language
getTableFiltered <- function(table, date) {
  query <- paste("SELECT * FROM", table, "WHERE ('CreatedOn' >= '20210830')")
  print(query)
  data <- DBI::dbGetQuery(con, query)
  message(paste0("Data retrieved from ", table))
  return(data)
}

# # Adjusted Table Function Using dplyr
# getTableFiltered <- function(table, date) {
#   query <- dplyr::tbl(con, table) %>%
#     dplyr::filter(CreatedOn >= date)
# 
#   dplyr::show_query(query)
#   data <- as.data.frame(query)
#   message(paste0("Data retrieved from ", table, ". Filtered from ", date))
#   return(data)
# }

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

##### Load data ##### 
locations <- getTableFiltered("Locations")
collectclosecontacts <- getTableFiltered("CollectContactsCalls")
closecontactcalls <- getTableFiltered("CloseContactCalls")
cases <- getTableFiltered("Cases")
wgscases <- getTableFiltered("Wgscases")
cluster_cases <- getTableFiltered("ClusterCases")
clusters_new <- getTableFiltered("Clusters")
#reflex_assay <- getTableFiltered("Reflexassay")

DBI::dbDisconnect(con)

##### Load in the school stat RDS ##### 
#schools_stats <- readRDS("schools_stats.RDS") # these were inserted into /data file using usethis::

##### Build the schools cases table ##### 
schools_cases <- locations %>%
  dplyr::filter(TypeOfPlace == "School or college") %>%
  # Add the cases data from collect_contacts --- collectclosecontacts
  dplyr::left_join(collectclosecontacts, by = c("CollectCallId" = "Id")) %>%
  # Fix the names of columns
  dplyr::rename_with(~ gsub(".x", "Locations", .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub(".y", "CollectCloseContacts", .x, fixed = TRUE)) %>%
  # Add the cases data from all_cases --- cases here
  dplyr::left_join(cases, by = "CaseNumber") %>%
  # Fix the names of columns
  dplyr::rename_with(~ gsub(".x", "Merged", .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub(".y", "Cases", .x, fixed = TRUE)) %>%
  # Only keep Cases.
  dplyr::filter(!is.na(CaseNumber)) %>%
  # Pull DENI number from AddressLineMerged1,2,3 where InstitutionReferenceNumber is blank.
  dplyr::mutate(
    InstitutionReferenceNumber = dplyr::case_when(
      is.na(InstitutionReferenceNumber) & stringr::str_detect(AddressLine3Merged, "\\d\\d\\d-\\d\\d\\d\\d") ~ AddressLine3Merged,
      is.na(InstitutionReferenceNumber) & stringr::str_detect(AddressLine2Merged, "\\d\\d\\d-\\d\\d\\d\\d") ~ AddressLine2Merged,
      is.na(InstitutionReferenceNumber) & stringr::str_detect(AddressLine1Merged, "\\d\\d\\d-\\d\\d\\d\\d") ~ AddressLine1Merged,
      TRUE ~ InstitutionReferenceNumber)) %>%
  #remove cancelled
  dplyr::filter(CaseFileStatus != 'Cancelled') %>%
  #remove prepilot
  dplyr::filter(CreatedOn > "2020-05-25") %>%
  #remove duplicates
  dplyr::distinct(CaseNumber, .keep_all = TRUE)

# %>%
# mutate(
#   InstitutionNameMerged = case_when(
#     is.na(InstitutionNameMerged) & !is.na(InstitutionNameCases) ~ InstitutionNameCases,
#     TRUE ~ InstitutionNameMerged
#   )
# )

##### Fix DENI number ##### 
schools_cases <- schools_cases %>%
  dplyr::mutate(InstitutionReferenceNumber = gsub('-', '', InstitutionReferenceNumber))

##### Add School Year variable ##### 
schools_cases <- schools_cases %>% 
  dplyr::mutate(
    DateOfBirth = lubridate::date(lubridate::as_datetime(DateOfBirth)),
    PositiveInEpiweek = lubridate::isoweek(DateOfSampleCases),
    PositiveInYear = lubridate::isoyear(DateOfSampleCases)) %>%
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
schools_cases_w_wgs <- dplyr::left_join(schools_cases, wgscases, by = "ContactId") %>%
  # Fix the names of columns
  dplyr::rename_with(~ gsub(".x", "Merged", .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub(".y", "WGS", .x, fixed = TRUE))

schools_cases_w_clusters <- dplyr::left_join(schools_cases_w_wgs, cluster_cases, by = "ContactId") %>%
  # Fix the names of columns
  dplyr::rename_with(~ gsub(".x", "SC", .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub(".y", "ClusterCases", .x, fixed = TRUE))


##### Clusters ##### 
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
  dplyr::mutate(CreatedOnLocations = lubridate::date(lubridate::as_datetime(CreatedOnLocations)),
                DateOfSampleCases = lubridate::date(lubridate::as_datetime(DateOfSampleCases)),
                AdditionDate = lubridate::as_datetime(AdditionDate),
                DateOfBirth = lubridate::date(lubridate::as_date(DateOfBirth)))

# School CLuster Stats
school_cluster_stats <- schools_cases_w_clusters %>%
  dplyr::group_by(ClusterID) %>%
  dplyr::summarise(
    TotalCases = dplyr::n(),
    TotalPossibleCloseContacts = sum(CloseContactCount),
    EarliestOnset = lubridate::date(min(DateOfOnsetSC, na.rm = TRUE)),
    EarliestSample = lubridate::date(min(DateOfSampleCases, na.rm = TRUE)),
    EarliestResult = lubridate::date(min(DateOfResult, na.rm = TRUE)),
    MostRecentOnset = lubridate::date(max(DateOfOnsetSC, na.rm = TRUE)),
    MostRecentSample = lubridate::date(max(DateOfSampleCases, na.rm = TRUE)),
    MostRecentResult = lubridate::date(max(DateOfResult, na.rm = TRUE)),
    MostRecentAdditionDate = lubridate::date(max(AdditionDate, na.rm = TRUE)),
    MedianAge = median(AgeAtPositiveResultClusterCases),
    CasesWithinLast3Days = sum(DateOfSampleCases <= today & DateOfSampleCases >= seventyTwoHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSampleCases < seventyTwoHours & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSampleCases <= today & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CaseTrend = dplyr::case_when(
      CasesWithinLast3Days > CasesWithinLast4to6Days ~  'Up',
      CasesWithinLast3Days < CasesWithinLast4to6Days ~ 'Down',
      CasesWithinLast3Days == CasesWithinLast4to6Days ~ 'Stable'),
    MinAge = min(AgeAtPositiveResultClusterCases), 
    MaxAge = max(AgeAtPositiveResultClusterCases),
    MaleCases = sum(GenderCases == "Male"),
    FemaleCases = sum(GenderCases == "Female"),
    AlphaVariants = sum(WgsVariant == "VOC-20DEC-01", na.rm = TRUE),
    BetaVariants = sum(WgsVariant == "VOC-20DEC-02", na.rm = TRUE),
    DeltaVariants = sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    DeltaReflex = sum(WgsReflexAssay == "Delta", na.rm = TRUE) + sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    #DeltaReflex = sum(str_detect(WgsReferralOtherReason, "Reflex|REFLEX|reflex") == TRUE, na.rm = TRUE),
    `VOC-21FEB-02Variants` = sum(WgsVariant == "E484K", na.rm = TRUE),
    KappaVariants = sum(WgsVariant == "VUI-21APR-01", na.rm = TRUE),
    `VUI-21FEB-04Variants` = sum(WgsVariant == "VUI-21FEB-04", na.rm = TRUE),
    `VUI-21MAY-02Variants` = sum(WgsVariant == "VUI-21MAY-02", na.rm = TRUE)
  )

#join stats to clusters_new
school_cluster_w_stats <- dplyr::left_join(school_cluster_stats, clusters_new, by = c("ClusterID" = "ClusterID"))

#add lat long
#clusters_w_stats <- left_join(clusters_w_stats, postcodes, by = c("Postcode" = "postcode"))

school_cluster_w_stats <- school_cluster_w_stats %>%
  dplyr::mutate(
    DateOfCmtReferralResponse = lubridate::date(lubridate::as_datetime(DateOfCmtReferralResponse)),        
    DateOfDutyRoomReferralResponse = lubridate::date(lubridate::as_datetime(DateOfDutyRoomReferralResponse)),
    DateOfEHOReferralResponse = lubridate::date(lubridate::as_datetime(DateOfEHOReferralResponse)),          
    DateOfhseniReferralResponse = lubridate::date(lubridate::as_datetime(DateOfhseniReferralResponse)),       
    DateOfimtReferralResponse = lubridate::date(lubridate::as_datetime(DateOfimtReferralResponse)),          
    DateOfInitialReport = lubridate::date(lubridate::as_datetime(DateOfInitialReport)),                
    DateOfrqiaReferralResponse = lubridate::date(lubridate::as_datetime(DateOfrqiaReferralResponse)),        
    DateOfTrustipcReferralResponse = lubridate::date(lubridate::as_datetime(DateOfTrustipcReferralResponse)),     
    DateReferredToClusterManagementTeam = lubridate::date(lubridate::as_datetime(DateReferredToClusterManagementTeam)),
    DateReferredToDutyRoom = lubridate::date(lubridate::as_datetime(DateReferredToDutyRoom)),            
    DateReferredToeho = lubridate::date(lubridate::as_datetime(DateReferredToeho)),                  
    DateReferredTohseni = lubridate::date(lubridate::as_datetime(DateReferredTohseni)),                
    DateReferredToimt = lubridate::date(lubridate::as_datetime(DateReferredToimt)),                 
    DateReferredTorqia = lubridate::date(lubridate::as_datetime(DateReferredTorqia)),                 
    DateReferredToTrustipc = lubridate::date(lubridate::as_datetime(DateReferredToTrustipc))) %>%
  dplyr::mutate(
    ReportedInEpiweek = lubridate::isoweek(DateOfInitialReport),
    ReportedInYear = lubridate::isoyear(DateOfInitialReport),
    ReferralMade = dplyr::case_when(
      !is.na(DateReferredToClusterManagementTeam) |
        !is.na(DateReferredToDutyRoom) |             
        !is.na(DateReferredToeho) |                 
        !is.na(DateReferredTohseni) |                
        !is.na(DateReferredToimt) |                
        !is.na(DateReferredTorqia) |                  
        !is.na(DateReferredToTrustipc) ~ "Yes"),
    MostRecentReferralDate = pmax(
      DateReferredToClusterManagementTeam,
      DateReferredToDutyRoom,            
      DateReferredToeho,                
      DateReferredTohseni,                
      DateReferredToimt,                
      DateReferredTorqia,                  
      DateReferredToTrustipc,
      na.rm = TRUE),
    DateClosed = dplyr::case_when(
      Status == "Closed" & Outbreak == "No" & stringr::str_detect(ClusterName, "VOC|VUI") == FALSE ~ lubridate::as_date(MostRecentSample)+15,
      Status == "Closed" & Outbreak == "Yes" & stringr::str_detect(ClusterName, "VOC|VUI") == FALSE ~ lubridate::as_date(MostRecentSample)+29),
    ClosedInEpiweek = lubridate::isoweek(DateClosed),
    ClosedInYear = lubridate::isoyear(DateClosed),
    ShortPostCode = stringr::str_trim(stringr::str_sub(Postcode, 1, 4)))

#####  Generate some stats about each school ##### 
schools_cases_stats <- schools_cases_w_wgs %>%
  dplyr::group_by(InstitutionReferenceNumber) %>%
  dplyr::summarise(
    TotalCases = dplyr::n(), 
    TotalCloseContacts = sum(CloseContactCount, na.rm = TRUE),
    CloseContacts28Days = sum(CloseContactCount > 0 & DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today, na.rm = TRUE),
    CasesPrev28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today, na.rm = TRUE),
    CasesPrev14Days = sum(DateOfSampleCases >= fourteendays & DateOfSampleCases < today, na.rm = TRUE),
    CasesPrev7Days = sum(DateOfSampleCases >= oneWeek & DateOfSampleCases < today, na.rm = TRUE),
    CasesWithinLast3Days = sum(DateOfSampleCases < today & DateOfSampleCases >= seventyTwoHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSampleCases < seventyTwoHours & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSampleCases < today & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CaseTrend = dplyr::case_when(
      CasesWithinLast3Days > CasesWithinLast4to6Days ~ 'Up',
      CasesWithinLast3Days < CasesWithinLast4to6Days ~ 'Down',
      CasesWithinLast3Days == CasesWithinLast4to6Days ~ 'Stable'),
    EarliestOnset = lubridate::date(min(DateOfOnset, na.rm = TRUE)),
    EarliestSample = lubridate::date(min(DateOfSampleCases, na.rm = TRUE)),
    EarliestResult = lubridate::date(min(DateOfResult, na.rm = TRUE)),
    MostRecentOnset = lubridate::date(max(DateOfOnset, na.rm = TRUE)),
    MostRecentSample = lubridate::date(max(DateOfSampleCases, na.rm = TRUE)),
    MostRecentResult = lubridate::date(max(DateOfResult, na.rm = TRUE)),
    MinAge = min(AgeAtPositiveResult, na.rm= TRUE), 
    MaxAge = max(AgeAtPositiveResult, na.rm= TRUE),
    MedianAge = median(AgeAtPositiveResult, na.rm= TRUE),
    MaleCases = sum(GenderCases == "Male", na.rm= TRUE),
    FemaleCases = sum(GenderCases == "Female", na.rm= TRUE),
    NurseryCases = sum(SchoolYear == "Nursery", na.rm = TRUE),
    ReceptionCases = sum(SchoolYear == "Reception", na.rm = TRUE),
    NurseryCases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Nursery", na.rm = TRUE),
    ReceptionCases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Reception", na.rm = TRUE),
    Y1Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 1", na.rm = TRUE),
    Y2Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 2", na.rm = TRUE),
    Y3Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 3", na.rm = TRUE),
    Y4Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 4", na.rm = TRUE),
    Y5Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 5", na.rm = TRUE),
    Y6Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 6", na.rm = TRUE),
    Y7Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Primary 7", na.rm = TRUE),
    Y8Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 8", na.rm = TRUE),
    Y9Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 9", na.rm = TRUE),
    Y10Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 10", na.rm = TRUE),
    Y11Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 11", na.rm = TRUE),
    Y12Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 12", na.rm = TRUE),
    Y13Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 13", na.rm = TRUE),
    Y14Cases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Year 14", na.rm = TRUE),
    StaffCases7Days = sum(DateOfSampleCases >= oneWeek & DateOfSampleCases < today & SchoolYear == "Staff", na.rm = TRUE),
    StaffCases14Days = sum(DateOfSampleCases >= fourteendays & DateOfSampleCases < today & SchoolYear == "Staff", na.rm = TRUE),
    StaffCases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Staff", na.rm = TRUE),
    OCases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Outlier", na.rm = TRUE),
    SNCases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "Special Needs", na.rm = TRUE),
    FECases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear == "FE Student", na.rm = TRUE),
    PupilCases7Days = sum(DateOfSampleCases >= oneWeek & DateOfSampleCases < today & SchoolYear != "Outlier" & SchoolYear != "Staff", na.rm = TRUE),
    PupilCases14Days = sum(DateOfSampleCases >= fourteendays & DateOfSampleCases < today & SchoolYear != "Outlier" & SchoolYear != "Staff", na.rm = TRUE),
    PupilCases28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today & SchoolYear != "Outlier" & SchoolYear != "Staff", na.rm = TRUE),
    AlphaVariants = sum(WgsVariant == "VOC-20DEC-01", na.rm = TRUE),
    BetaVariants = sum(WgsVariant == "VOC-20DEC-02", na.rm = TRUE),
    DeltaVariants = sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    DeltaReflex = sum(WgsReflexAssay == "Delta", na.rm = TRUE) + sum(WgsVariant == "VOC-21APR-02", na.rm = TRUE),
    #DeltaReflex = sum(str_detect(WgsReferralOtherReason, "Reflex|REFLEX|reflex") == TRUE, na.rm = TRUE),
    `VOC-21FEB-02Variants` = sum(WgsVariant == "E484K", na.rm = TRUE),
    KappaVariants = sum(WgsVariant == "VUI-21APR-01", na.rm = TRUE),
    `VUI-21FEB-04Variants` = sum(WgsVariant == "VUI-21FEB-04", na.rm = TRUE),
    `VUI-21MAY-02Variants` = sum(WgsVariant == "VUI-21MAY-02", na.rm = TRUE))

#####  Join schools_cases_stats to schools_stats ##### 
schools_stats_overall <- dplyr::left_join(schools_stats, schools_cases_stats, by = c("DENINumber" = "InstitutionReferenceNumber")) %>% 
  tidyr::drop_na(DENINumber)

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
    AttackRateSN = round((SNCases28Days/TotalPupils)*100, digits = 2),
    Quotient = TotalPupils/50,
    Prevalance50 = TotalCases/Quotient)

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

##### Close Contacts #####  
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

