#' sch_report_cases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_report_cases_ui <- function(id){
  DT::dataTableOutput(NS(id, "school_cases_table"))
}
    
#' sch_report_cases Server Functions
#'
#' @noRd 
mod_sch_report_cases_server <- function(id, df, school_id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$school_cases_table = DT::renderDataTable({
      shiny::req(school_id())
      name <- df()$InstitutionName[1]
      
      school_report_cases <- DT::datatable(df() %>% 
                                             dplyr::select(CaseNumber,
                                                           FirstNameSC,
                                                           LastNameSC,
                                                           SchoolYear, 
                                                           AgeAtPositiveResultSC, 
                                                           Gender,
                                                           DateOfResult,
                                                           DateOfOnsetSC,
                                                           CloseContactCount) %>% 
                                             dplyr::mutate(DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y")), 
                                           caption = paste("Line List for", name),
                                           filter = "top",
                                           extensions = c('Buttons'),
                                           options = list(
                                             pageLength = 15,
                                             dom = 'lBftrip',
                                             scrollX = T,
                                             buttons = list(
                                               list(extend = 'csv', filename = paste0(school_id(),"_cases_line_listing")),
                                               list(extend = 'excel', filename = paste0(school_id(),"_cases_line_listing")))))
      
      school_report_cases
    })
  })
}
    
## To be copied in the UI
# mod_sch_report_cases_ui("sch_report_cases_ui_1")
    
## To be copied in the server
# mod_sch_report_cases_server("sch_report_cases_ui_1")
