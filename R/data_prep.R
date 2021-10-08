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
locations <- getTableFiltered("Locations", "20210830")
collectclosecontacts <- getTableFiltered("CollectContactsCalls", "20210830")
closecontactcalls <- getTableFiltered("CloseContactCalls", "20210830")
cases <- getTableFiltered("Cases", "20210830")
wgscases <- getTableFiltered("Wgscases", "20210830")
cluster_cases <- getTableFiltered("ClusterCases", "20210830")

##### Load in the school stat RDS ##### 
schools_stats <- readRDS("schools_stats.RDS")

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
    InstitutionReferenceNumber = case_when(
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
  dplyr::mutate(SchoolYear = case_when(DateOfBirth >= as.Date(paste0(currentYear-2,"-07-02")) ~ "Pre-Nursery",
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
                                DateOfBirth >= as.Date(paste0(currentYear-18,"-07-02")) & DateOfBirth <= as.Date(paste0(currentYear-17,"-07-01")) ~ "Year 14")) %>% 
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
# Create old schools cluster cases from exisitng schools_cases
# These are schools cases with clusterID that are in Cluser1Id,Cluster2Id,Cluster3Id
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

schools_cases_w_clusters <- left_join(schools_cases_w_clusters, old_schools_cluster_cases)

schools_cases_w_clusters <- schools_cases_w_clusters %>%
  dplyr::mutate(CreatedOnLocations = lubridate::date(as_datetime(CreatedOnLocations)),
                DateOfSampleCases = lubridate::date(as_datetime(DateOfSampleCases)),
                AdditionDate = lubridate::as_datetime(AdditionDate),
                DateOfBirth = lubridate::date(as_date(DateOfBirth)))

#####  Generate some stats about each school ##### 
schools_cases_stats <- schools_cases_w_wgs %>%
  dplyr::group_by(InstitutionReferenceNumber) %>%
  dplyr::summarise(
    TotalCases = n(), 
    TotalCloseContacts = sum(CloseContactCount, na.rm = TRUE),
    CloseContacts28Days = sum(CloseContactCount > 0 & DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today, na.rm = TRUE),
    CasesPrev28Days = sum(DateOfSampleCases >= twentyeightdays & DateOfSampleCases < today, na.rm = TRUE),
    CasesPrev14Days = sum(DateOfSampleCases >= fourteendays & DateOfSampleCases < today, na.rm = TRUE),
    CasesPrev7Days = sum(DateOfSampleCases >= oneWeek & DateOfSampleCases < today, na.rm = TRUE),
    CasesWithinLast3Days = sum(DateOfSampleCases < today & DateOfSampleCases >= seventyTwoHours, na.rm = TRUE),
    CasesWithinLast4to6Days = sum(DateOfSampleCases < seventyTwoHours & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CasesWithinLast6Days = sum(DateOfSampleCases < today & DateOfSampleCases >= oneHundredFourtyFourHours, na.rm = TRUE),
    CaseTrend = case_when(
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
schools_stats_overall <- dplyr::left_join(schools_stats, schools_cases_stats, by = c("DENINumber" = "InstitutionReferenceNumber"))

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

#Add  postcode district and join with LGD and ward data
schools_stats_overall$BT_area <- schools_stats_overall$Postcode
schools_stats_overall$BT_area <- gsub(' ', '', schools_stats_overall$BT_area)
schools_stats_overall$BT_area <- toupper(schools_stats_overall$BT_area)
schools_stats_overall$BT_area <- as.character(gsub('.{3}$', '', schools_stats_overall$BT_area))
schools_stats_overall$BT_area <- stringr::str_trim(schools_stats_overall$BT_area, side = "both")

PCD_LGD <- readRDS("ward_PCD_LGD.RDS")
PCD_LGD <- PCD_LGD %>%
  dplyr::select("BT_area", "LGDName") %>%
  dplyr::distinct(BT_area, .keep_all = TRUE)

schools_stats_overall <- dplyr::full_join(schools_stats_overall, PCD_LGD, by = "BT_area")

##### Close Contacts #####  
schools_cases_w_wgs_vector <- schools_cases_w_wgs$CaseNumber
## shrink the size of closecontactcalls for only contacts associated with school cases
closecontactcalls <- closecontactcalls  %>%
  dplyr::filter(CaseNumber %in% schools_cases_w_wgs_vector)

## get DENI and casenumbers from schools_cases_w_wgs
case_numbers_and_deni <- schools_cases_w_wgs %>%
  dplyr::select(
    CaseNumber,
    InstitutionReferenceNumber)

## Join Data Frames
close_contacts_for_schools <- dplyr::left_join(closecontactcalls, case_numbers_and_deni, by = "CaseNumber")

##case rate per LGD/PCD
PCDpops <- read.csv("PCD2018_populations.csv") %>%
  as.data.frame() %>%
  mutate(Postcode = Postcode.District) %>%
  select('X0_19':'Postcode')

LGDpops <- read.csv("LGDpops.csv") %>%
  as.data.frame() %>%
  select('area_name', 'MYE')

PCDnames <-  c("X0_19", "X20_39", "X40_59", "X60_79", "X80plus", "All.Ages")
PCDpops[PCDnames] <- sapply(PCDpops[PCDnames], gsub, pattern= ",", replacement= "")

PCDpops <- PCDpops %>%
  mutate(X0_19 = as.numeric(X0_19)) %>%
  mutate(X20_39 = as.numeric(X20_39)) %>%
  mutate(X40_59 = as.numeric(X40_59)) %>%
  mutate(X60_79 = as.numeric(X60_79)) %>%
  mutate(X80plus = as.numeric(X80plus)) %>%
  mutate(All.Ages = as.numeric(All.Ages))


PCDpops[is.na(PCDpops)] <- 0

#Filter cases by  1 week
#group cases by SPC
#join to pcd pops
#calculate case rate
#Create data frames for date ranges
cases4weeks = cases %>%
  filter(CreatedOn > (today()-days(x=29)) & CreatedOn < today())%>%
  mutate(CreatedOn = as.Date(CreatedOn)) %>%
  filter(CaseFileStatus != 'Cancelled') %>% 
  mutate(PostCode = str_replace(PostCode, " ", "")) %>%
  drop_na(PostCode) %>%
  select('CaseNumber', 'CreatedOn', 'PostCode')
cases4weeks$BT_area <- toupper(cases4weeks$PostCode)
cases4weeks$BT_area <- as.character(gsub('.{3}$', '', cases4weeks$BT_area))
cases4weeks <- dplyr::full_join(cases4weeks, PCD_LGD, by = "BT_area")

cases2weeks = cases4weeks %>%
  filter(CreatedOn > (today()-days(x=15)) & CreatedOn < today())

#join PCD to the cases and group the cases by PCD and LGD

cases4weeksPCD <- cases4weeks %>% 
  group_by(BT_area, .drop = FALSE) %>%
  count()

cases4weeksLGD <- cases4weeks %>%
  group_by(LGDName, .drop = FALSE) %>%
  count()

colnames(cases4weeksPCD)[colnames(cases4weeksPCD) == "n"] <- "CasesLast4Week"
colnames(cases4weeksLGD)[colnames(cases4weeksLGD) == "n"] <- "CasesLast4Week"

cases2weeksPCD <- cases2weeks %>% 
  group_by(BT_area, .drop = FALSE) %>%
  count()

cases2weeksLGD <- cases2weeks %>%
  group_by(LGDName, .drop = FALSE) %>%
  count()

colnames(cases2weeksPCD)[colnames(cases2weeksPCD) == "n"] <- "CasesLast2Week"
colnames(cases2weeksLGD)[colnames(cases2weeksLGD) == "n"] <- "CasesLast2Week"

#calculate case rate
cases4weeksPCD <- left_join(cases4weeksPCD, PCDpops, by = c("BT_area" = "Postcode"))
caserate4weeksPCD <- cases4weeksPCD %>%
  summarise(CasesPer100kPCD = round((CasesLast4Week/All.Ages) * 100000, digits = 1))

cases4weeksLGD <- left_join(cases4weeksLGD, LGDpops, by = c("LGDName" = "area_name"))
caserate4weeksLGD <- cases4weeksLGD %>%
  summarise(CasesPer100kLGD = round((CasesLast4Week/MYE) * 100000, digits = 1))

cases2weeksPCD <- left_join(cases2weeksPCD, PCDpops, by = c("BT_area" = "Postcode"))
caserate2weeksPCD <- cases2weeksPCD %>%
  summarise(CasesPer100kPCD = round((CasesLast2Week/All.Ages) * 100000, digits = 1))

cases2weeksLGD <- left_join(cases2weeksLGD, LGDpops, by = c("LGDName" = "area_name"))
caserate2weeksLGD <- cases2weeksLGD %>%
  summarise(CasesPer100kLGD = round((CasesLast2Week/MYE) * 100000, digits = 1))

#Calculate ni caserate
NICasesLast2Week <- as.numeric(colSums(cases2weeksLGD[ , c(2)], na.rm=TRUE))
NICasesLast4Week <- as.numeric(colSums(cases4weeksLGD[ , c(2)], na.rm=TRUE))
NITotalPop <- as.numeric(colSums(cases2weeksLGD[ , c(3)], na.rm=TRUE))
NI14dayrate <- as.numeric(round(((NICasesLast2Week)/(NITotalPop))*100000, digits = 1))
NI28dayrate <- as.numeric(round(((NICasesLast4Week)/(NITotalPop))*100000, digits = 1))

#add to schools data
schools_stats_overall <- dplyr::left_join(schools_stats_overall, caserate2weeksPCD, by = "BT_area")
schools_stats_overall <- dplyr::left_join(schools_stats_overall, caserate2weeksLGD, by = "LGDName")
schools_stats_overall <- dplyr::left_join(schools_stats_overall, caserate4weeksPCD, by = "BT_area")
schools_stats_overall <- dplyr::left_join(schools_stats_overall, caserate4weeksLGD, by = "LGDName")
