#' sch_report_contacts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_report_contacts_ui <- function(id){
  DT::dataTableOutput(NS(id, "school_contacts_table"))
}
    
#' sch_report_contacts Server Functions
#'
#' @noRd 
mod_sch_report_contacts_server <- function(id, df, school_id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$school_contacts_table <- DT::renderDataTable ({
      shiny::req(school_id())
      
      school_report_contacts <- DT::datatable(df() %>%
                                                dplyr::mutate(ContactOfCase = CaseNumber),
                                              filter = "top",
                                              extensions = c('Buttons'),
                                              options = list(
                                                dom = 'lBftrip',
                                                scrollX = T,
                                                buttons = list(
                                                  list(extend = 'csv', filename = paste0(school_id(),"_contacts_line_listing")),
                                                  list(extend = 'excel', filename = paste0(school_id(),"_contacts_line_listing"))),
                                                order = list(
                                                  5,
                                                  "desc"),
                                                columnDefs = list(
                                                  list(visible = FALSE, targets = 0))))

      
      school_report_contacts
    })
  })
}
    
## To be copied in the UI
# mod_sch_report_contacts_ui("sch_report_contacts_ui_1")
    
## To be copied in the server
# mod_sch_report_contacts_server("sch_report_contacts_ui_1")
