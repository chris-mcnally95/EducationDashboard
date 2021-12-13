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
    dplyr::filter(schools_cases_w_clusters, InstitutionReferenceNumber == get_school_id)
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
  
  output$CCR7DayLGD <- shiny::renderText({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), LGD7Day100k))
  })
  
  output$CCR14DayLGD <- shiny::renderText({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), LGD14Day100k))
  })
  
  output$CCR28DayLGD <- shiny::renderText({
    shiny::req(input$input_school_id)
    paste(dplyr::select(school(), LGD28Day100k))
  })
  
  output$NI7dayrate <- shiny::renderText({
    paste(NI7dayrate)
  })
  
  output$NI14dayrate <- shiny::renderText({
    paste(NI14dayrate)
  })
  
  output$NI28dayrate <- shiny::renderText({
    paste(NI28dayrate)
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
  
  ## Download Report
  #create the output for the rmd report
  output$report <- shiny::downloadHandler(
    #shiny::req(input$input_school_id),
    
    #create a function to store filename
    filename = function() {
      paste0("SchoolID_", input$input_school_id, "_report.html")
    },
    
    #show a progress bar for the download and running of the rmd report
    content = function(file) {
      shiny::withProgress(message = 'Rendering report, please wait!', {
        src <- normalizePath('school_report.Rmd')
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'school_report.Rmd', overwrite = TRUE)
        
        #read in required reactive variables needed to create report
        #these are also listed in the params tag at the top of the
        #rmd itself
        out <- rmarkdown::render(
          'school_report.Rmd',
          params = list(
            SchoolID = input$input_school_id,
            school = school(),
            schoolCases = schoolCases(),
            #postcodes = postcodes,
            rendered_by_shiny = TRUE
          )
        )
        
        file.rename(out, file)
      })
    }
  )
  
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
  
  # ## Run Primary Schools Table Module
  # mod_primary_sch_server(id ="primary_sch_ui_1",
  #                    df = schools_stats_overall)
  
  
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
                        df2 = school_pcd_clusters, 
                        df3 = all_cases_pcd)
}
