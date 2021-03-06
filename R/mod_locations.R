#' locations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_locations_ui <- function(id){
  DT::dataTableOutput(NS(id, "locations_report_table"))
}
    
#' locations Server Functions
#'
#' @noRd 
mod_locations_server <- function(id, df, DateRange){
  shiny::moduleServer(id, function(input, output, session){
    locations_report_data <- reactive ({
      
      ## Build Locations Report Table
      shiny::req(DateRange())
      df %>%
        dplyr::filter(CreatedOn >= DateRange()[1],
                      CreatedOn <= DateRange()[2]) %>%
        dplyr::select(CreatedOn,
                      CaseNumber, 
                      FirstNameSC,
                      LastNameSC,
                      AgeAtPositiveResultSC,
                      DateOfBirth, 
                      DateOfSample,
                      ClusterID,
                      ClusterName, 
                      #AdditionDate, 
                      InstitutionReferenceNumber,
                      InstitutionName,
                      AddressLine1, 
                      #AddressLine2, 
                      #AddressLine3, 
                      City) %>%
          
        dplyr::rename(
          "Created On" = CreatedOn, 
          "Case Number" = CaseNumber, 
          "First Name" = FirstNameSC, 
          "Last Name" = LastNameSC,
          "Age" = AgeAtPositiveResultSC, 
          "Date Of Birth" = DateOfBirth, 
          "Date Of Sample" = DateOfSample, 
          "Cluster ID" = ClusterID, 
          "Cluster Name" = ClusterName, 
          #"Cluster Addition Date" = AdditionDate, 
          "DENI Number" = InstitutionReferenceNumber, 
          "Institutuion Name" = InstitutionName, 
          "Address Line 1" = AddressLine1, 
          #"Address Line 2" = AddressLine2,
          #"Address Line 3" = AddressLine3, 
          "City" = City
        )

      
    })
    
    ## Render Locations Report Table
    output$locations_report_table <- DT::renderDataTable({
      locations_report_data()
    },
    filter = "top",
    server = FALSE,
    extensions = c('Buttons'),
    options = list(
      dom = 'lBftrip',
      pageLength = 10,
      scrollX = T,
      buttons = list(
        list(extend = 'csv', filename = "locations_report_data"),
        list(extend = 'excel', filename = "locations_report_data")),
      order = list(
        7,
        "desc"),
      columnDefs = list(
        list(visible = FALSE, targets = 0)))
    )
  })
}
    
## To be copied in the UI
# mod_locations_ui("locations_ui_1")
    
## To be copied in the server
# mod_locations_server("locations_ui_1")
