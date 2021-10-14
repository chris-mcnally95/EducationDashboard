#' primary_sch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_primary_sch_ui <- function(id){
  DT::dataTableOutput(NS(id, "primary_schools_table"))
}
    
#' primary_sch Server Functions
#'
#' @noRd 
mod_primary_sch_server <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){ 
    
    ## Build Primary Schools Table
    primary_schools_data <- df %>%
      dplyr::filter(InstitutionType == "Primary" | InstitutionType == "Preps") %>%
      dplyr::select(DENINumber,
                    InstitutionName,
                    CasesPrev7Days,
                    CasesPrev14Days,
                    CasesPrev28Days,
                    PupilCases28Days,
                    StaffCases28Days,
                    TotalPupils,
                    AttackRate7Days,
                    AttackRate14Days,
                    AttackRate28Days,
                    CloseContacts28Days,
                    TotalCases,
                    TotalCloseContacts,
                    Y1Cases28Days,
                    Year1,
                    AttackRateY1,
                    Y2Cases28Days,
                    Year2,
                    AttackRateY2,
                    Y3Cases28Days,
                    Year3,
                    AttackRateY3,
                    Y4Cases28Days,
                    Year4,
                    AttackRateY4,
                    Y5Cases28Days,
                    Year5,
                    AttackRateY5,
                    Y6Cases28Days,
                    Year6,
                    AttackRateY6,
                    Y7Cases28Days,
                    Year7,
                    AttackRateY7,
                    NurseryCases28Days,
                    NurseryPupils,
                    AttackRateNursery,
                    ReceptionCases28Days,
                    ReceptionPupils,
                    AttackRateReception) %>%
      dplyr::rename("DENI Number" = DENINumber,
                    "Name" = InstitutionName,
                    "Cases 7 Days" = CasesPrev7Days,
                    "Cases 14 Days" = CasesPrev14Days,
                    "Cases 28 Days" = CasesPrev28Days,
                    "Pupil Cases 28 Days" = PupilCases28Days,
                    "Staff Cases 28 Days" = StaffCases28Days,
                    "Total Enrolled Pupils" = TotalPupils,
                    "7 Day Attack Rate(%)" = AttackRate7Days,
                    "14 Day Attack Rate(%)" = AttackRate14Days,
                    "28 Day Attack Rate(%)" = AttackRate28Days,
                    "28 Days Close Contacts" = CloseContacts28Days,
                    "Total Cases Overall" = TotalCases,
                    "Total Close Contacts Overall" = TotalCloseContacts,
                    "Year 1 Cases 28 Days" = Y1Cases28Days,
                    "Year 1 Enrolled Pupils" = Year1,
                    "Year 1 28 Day Attack Rate(%)" = AttackRateY1,
                    "Year 2 Cases 28 Days" = Y2Cases28Days,
                    "Year 2 Enrolled Pupils" = Year2,
                    "Year 2 28 Day Attack Rate(%)" = AttackRateY2,
                    "Year 3 Cases 28 Days" = Y3Cases28Days,
                    "Year 3 Enrolled Pupils" = Year3,
                    "Year 3 28 Day Attack Rate(%)" = AttackRateY3,
                    "Year 4 Cases 28 Days" = Y4Cases28Days,
                    "Year 4 Enrolled Pupils" = Year4,
                    "Year 4 28 Day Attack Rate(%)" = AttackRateY4,
                    "Year 5 Cases 28 Days" = Y5Cases28Days,
                    "Year 5 Enrolled Pupils" = Year5,
                    "Year 5 28 Day Attack Rate(%)" = AttackRateY5,
                    "Year 6 Cases 28 Days" = Y6Cases28Days,
                    "Year 6 Enrolled Pupils" = Year6,
                    "Year 6 28 Day Attack Rate(%)" = AttackRateY6,
                    "Year 7 Cases 28 Days" = Y7Cases28Days,
                    "Year 7 Enrolled Pupils" = Year7,
                    "Year 7 28 Day Attack Rate(%)" = AttackRateY7,
                    "Nursery Cases 28 Days" = NurseryCases28Days,
                    "Nursery Enrolled Pupils" = NurseryPupils,
                    "Nursery 28 Day Attack Rate(%)" = AttackRateNursery,
                    "Reception Cases 28 Days" = ReceptionCases28Days,
                    "Reception Enrolled Pupils" = ReceptionPupils,
                    "Reception 28 Day Attack Rate(%)" = AttackRateReception)
    
    ## Render Primary Schools Table
    output$primary_schools_table <- DT::renderDataTable({
      primary_schools_data
    },
    filter = "top",
    server = FALSE,
    extensions = c('Buttons'),
    options = list(
      dom = 'lBftrip',
      pageLength = 10,
      scrollX = T,
      buttons = list(
        list(extend = 'csv', filename = "primary_schools_data"),
        list(extend = 'excel', filename = "primary_schools_data")),
      order = list(
        9,
        "desc"),
      columnDefs = list(
        list(visible = FALSE, targets = 0)))
    )
  })
}
    
## To be copied in the UI
# mod_primary_sch_ui("primary_sch_ui_1")
    
## To be copied in the server
# mod_primary_sch_server("primary_sch_ui_1")
