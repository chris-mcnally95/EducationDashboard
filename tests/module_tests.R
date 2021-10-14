
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

#### Overview Module ####
testthat::test_that("The datatable renders", {
  shiny::testServer(app = mod_sch_overview_server,
                    args = list(stats_df = schools_stats_overall), {
                      testthat::expect_type(
                        home.page.table, 
                        "list")
                    })
})

#### Overview Module ####
testthat::test_that("The plot renders", {
  shiny::testServer(app = mod_sch_report_epi_server,
                    args = list(df = schoolCases,
                                school_id =  shiny::reactive("2230301")), {
                                  testthat::expect_type(
                                    epicurve.table.plot, 
                                    "list")
                                })
             
})
 

  

