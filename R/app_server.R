#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Load in Dynamic Data Sets
  
  source("./data_prep.R")
  
  # Your application server logic
  
  
  #--------------HOME--------------
  
  # InfoBoxes
  
  mod_home_infoboxes_server("home_infoboxes_ui_1")

  #--------------SCHOOLS OVERVIEW--------------
  
  ## Run Schools Overview Table Server Module
  mod_sch_overview_server("sch_overview_ui_1",
                           stats_df = schools_stats_overall)
  
  #--------------SCHOOL REPORT--------------
  
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
    dplyr::filter(close_contacts_for_schools, InstitutionReferenceNumber == get_school_id)
  })
  
  
  ## Key Info
  output$schoolName <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), InstitutionName))
  })
  
  output$schoolID <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), DENINumber))
  })
  
  output$schoolType <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), InstitutionType))
  })
  
  output$TotalPupils <- shiny::renderText({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), TotalPupils))
  })
  
  output$schoolAR7 <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), AttackRate7Days))
  })
  
  output$schoolAR14 <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), AttackRate14Days))
  })
  
  output$schoolAR28 <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), AttackRate28Days))
  })
  
  output$Area <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), Town))
  })
  
  output$PostCode <- shiny::renderText ({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), Postcode))
  })
  
  output$LGD <- shiny::renderText({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), LGDName))
  })
  
  ## InfoBoxes
  output$totalCases <- shinydashboard::renderInfoBox ({
    shiny::req(input$input_school_id)
    totalCases <- school()$TotalCases
    shinydashboard::infoBox(
      "Total Cases",paste(totalCases),
      icon = icon("male"),
      color = "blue"
    )
  })
  
  output$totalContacts <- shinydashboard::renderInfoBox ({
    shiny::req(input$input_school_id)
    totalContacts <- school()$TotalCloseContacts
    shinydashboard::infoBox(
      "Total Contacts",paste(totalContacts),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ## Plot EpiCurve
  ### Run School Report Epicurve Server Module  
  mod_sch_report_epi_server(id = "sch_report_epi_ui_1",
                         df = schoolCases,
                         school_id = shiny::reactive(input$input_school_id))
  
  ## Plot School Year Data 
  ### Run School Report Year Server Module  
  mod_sch_report_year_server(id = "sch_report_year_ui_1",
                     df = schoolCases,
                     school_id = shiny::reactive(input$input_school_id))
  
  ## Plot Attack Rate by Year
  ### Run School Report AR Server Module  
  mod_sch_report_ar_server(id = "sch_report_ar_ui_1",
                   df = school,
                   school_id = shiny::reactive(input$input_school_id))
  
  ## School Cases Data Table
  ### Run School Report Cases Server Module  
  mod_sch_report_cases_server(id = "sch_report_cases_ui_1",
                      df = schoolCases,
                      school_id = shiny::reactive(input$input_school_id))
  
  ## School Contacts Data Table
  ### Run School Report Cases Server Module  
  mod_sch_report_contacts_server(id = "sch_report_contacts_ui_1",
                         df = schoolContacts,
                         school_id = shiny::reactive(input$input_school_id))
  
  #--------------PRIMARY SCHOOLS------------------
  
  ## Run Primary Schools Table Module
  mod_primary_sch_server(id ="primary_sch_ui_1",
                     df = schools_stats_overall)
  
  
  #--------------LOCATIONS REPORT------------------
  
  ## get the dates from location report
  locations_report_dates <- shiny::reactive ({
    input$locations_report_daterange
  })
  
  ## Run School Report Cases Module  
  mod_locations_server(id = "locations_ui_1",
                          df = schools_cases_w_clusters,
                          DateRange = locations_report_dates)
  
  #--------------EARLY WARNING REPORT------------------
  
  ## Run School Report Cases Module
  mod_ewr_helper_server(id = "ewr_helper_ui_1",
                        df1 = schools_stats_overall,
                        df2 = school_spc_clusters)
}
