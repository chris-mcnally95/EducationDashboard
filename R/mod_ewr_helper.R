#' ewr_helper UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ewr_helper_ui <- function(id){
  shinydashboard::tabItem(
    tabName = "ewr_helper",

    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Cases Per Post Code District",
        p("The table below shows situations associated with schools, with their post code district as well as the number of cases associated with each situation."),
        hr(),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(shiny::NS(id,"ewr_cases_per_spc"))
        )
      )
    )
  )

}
    
#' ewr_helper Server Functions
#'
#' @noRd 
mod_ewr_helper_server <- function(id, df){
  moduleServer( id, function(input, output, session){

    #outbreaks summary object
    ewr_cases_per_spc_data <- shiny::reactive ({

      data <- df %>% 
        dplyr::select(
          PostcodeDistrict,
          TotalCases,
          CasesPrev28Days,
          CasesPrev14Days,
          CasesPrev7Days,
          CasesWithinLast6Days,
          CasesWithinLast4to6Days,
          CasesWithinLast3Days,
          CaseTrend) %>% 
        dplyr::arrange(desc(CasesPrev28Days))
    })
    
    output$ewr_cases_per_spc <- DT::renderDataTable({
      ewr_cases_per_spc_data()
    },
      filter = "top",
      server= FALSE,
      extensions = c('Buttons', 'FixedHeader'),
      options = list(
        stateSave = TRUE,
        searchCols = NULL,
        fixedHeader=TRUE,
        lengthMenu = c(5, 10, 20, 50, 100),
        dom = 'lBftrip',
        pageLength = 10,
        scrollY = "",
        scrollX = T,
        buttons = list(
          list(extend = 'csv', filename = paste0(Sys.Date(),"_cases_per_cluster")),
          list(extend = 'excel', filename = paste0(Sys.Date(),"_cases_per_cluster")))
      )
    )
  })
}
    
## To be copied in the UI
# mod_ewr_helper_ui("ewr_helper_ui_1")
    
## To be copied in the server
# mod_ewr_helper_server("ewr_helper_ui_1")
