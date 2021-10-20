
#----------------------------------------------------------------
# Module Testing ( run devtools:check() )
#----------------------------------------------------------------


## Assign Reactive Data Frames
school <- shiny::reactive({
  dplyr::filter(schools_stats_overall, DENINumber == input$input_school_id)
})

schoolCases <- shiny::reactive({
  shiny::req(input$input_school_id)
  get_school_id <- input$input_school_id
  dplyr::filter(schools_cases_w_wgs, InstitutionReferenceNumber == get_school_id)
})

schoolContacts <- shiny::reactive({
  shiny::req(input$input_school_id)
  get_school_id <- input$input_school_id
  filter(close_contacts_for_schools, InstitutionReferenceNumber == get_school_id)
})

locations_report_dates <- shiny::reactive ({
  input$locations_report_daterange
})


#### Overview Module ####
testthat::test_that("The overview datatable renders", {
  shiny::testServer(app = mod_sch_overview_server,
                    args = list(stats_df = schools_stats_overall), {
                      testthat::expect_type(
                        home.page.table, 
                        "list")
                    })
})

#### School Report Epicurve Module ####
testthat::test_that("The epicurve plot renders", {
  shiny::testServer(app = mod_sch_report_epi_server,
                    args = list(df = schoolCases,
                                school_id =  shiny::reactive("2230301")), {
                                  testthat::expect_type(
                                    epicurve.table.plot, 
                                    "list")
                                })
})

#### School Report Year Group Module ####
testthat::test_that("The year group plot renders", {
  shiny::testServer(app = mod_sch_report_year_server,
                    args = list(df = schoolCases,
                                school_id =  shiny::reactive("2230301")), {
                                  testthat::expect_type(
                                    school.year.table.plot, 
                                    "list")
                                })
})

#### School Report Attack Rate Module ####
testthat::test_that("The attack rate plot renders", {
  shiny::testServer(app = mod_sch_report_ar_server,
                    args = list(df = school,
                                school_id =  shiny::reactive("2230301")), {
                                  testthat::expect_type(
                                    school.year.table.plot, 
                                    "list")
                                })
})

#### School Report Case List Module ####
testthat::test_that("The cases line list renders", {
  shiny::testServer(app = mod_sch_report_ar_server,
                    args = list(df = schoolCases,
                                school_id =  shiny::reactive("2230301")), {
                                  testthat::expect_type(
                                    school_report_cases, 
                                    "list")
                                })
})

#### School Report Contacts List Module ####
testthat::test_that("The contacts line list renders", {
  shiny::testServer(app = mod_sch_report_ar_server,
                    args = list(df = schoolCases,
                                school_id =  shiny::reactive("2230301")), {
                                  testthat::expect_type(
                                    school_report_contacts, 
                                    "list")
                                })
})

#### Primary School Module ####
testthat::test_that("The primary school table renders", {
  shiny::testServer(app = mod_primary_sch_server,
                    args = list(df = schools_stats_overall), {
                                  testthat::expect_type(
                                    primary_schools_data, 
                                    "list")
                                })
})

#### Locations Module ####
testthat::test_that("The locations table renders", {
  shiny::testServer(app = mod_locations_server,
                    args = list(df = schools_cases_w_clusters,
                                DateRange = locations_report_dates), {
                      testthat::expect_type(
                        locations_report_data, 
                        "list")
                    })
  
  
})

#### EWR Module ####
testthat::test_that("The ewr1 table renders", {
  shiny::testServer(app = mod_ewr_helper_server,
                    args = list(df = schools_cases_w_clusters,
                                DateRange = early_warning_report_dates), {
                                  testthat::expect_type(
                                    ewr_cases_per_cluster_data, 
                                    "list")
                                })
})

testthat::test_that("The ewr2 table renders", {
  shiny::testServer(app = mod_ewr_helper_server,
                    args = list(df = school_cluster_w_stats,
                                DateRange = early_warning_report_dates), {
                                  testthat::expect_type(
                                    ewr_cases_per_spc_data, 
                                    "list")
                                })
})
 

  

