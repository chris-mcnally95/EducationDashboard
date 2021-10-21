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
        title = "Cases Per School in a Postcode District",
        p("The table below shows situations associated with a single school, with their postcode district."),
        hr(),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(shiny::NS(id,"ewr_cases_per_school"))
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Cases Per Postcode District",
        p("The table below shows situations associated with all schools in a postcode district."),
        hr(),
        # shiny::selectizeInput(
        #   inputId = "postcode_district_selection",
        #   label = "Select Postcode District(s)",
        #   choices = as.list(school_spc_clusters$PostcodeDistrict),
        #   selected = "BT14",
        #   multiple = TRUE,
        #   options = list(
        #     'plugins' = list('remove_button'),
        #     'create' = TRUE,
        #     'persist' = TRUE)),
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
mod_ewr_helper_server <- function(id, df1, df2){
  moduleServer( id, function(input, output, session){
    
    ## Build First Table
    ewr_cases_per_school_data <- df1 %>% 
      dplyr::select(PostcodeDistrict,
                    InstitutionName,
                    DENINumber,
                    CasesPrev28Days,
                    CasesPrev7Days) %>% 
      dplyr::arrange(desc(CasesPrev28Days))
    
    colnames(ewr_cases_per_school_data) <- c("Postcode District",
                                             "Institution Name",
                                             "DENI Number",
                                             "28 Days Cases",
                                             "7 Days Cases")
    
    ## Render First  Table
    output$ewr_cases_per_school = DT::renderDT({
      ewr_cases_per_school_data},
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
          list(extend = 'csv', filename = paste0(Sys.Date(),"ewr_cases_per_school")),
          list(extend = 'excel', filename = paste0(Sys.Date(),"ewr_cases_per_school")))
      )
    )

    ## Build Second Table
    ewr_cases_per_spc_data <- df2 %>%
      dplyr::select(
        PostcodeDistrict,
        TotalCases,
        CasesPrev28Days,
        CasesPrev7Days) %>%
      dplyr::arrange(desc(CasesPrev28Days)) %>%
      dplyr::rename(
        "Postcode District" = PostcodeDistrict, 
        "Total Cases" = TotalCases, 
        "28 Days Cases" = CasesPrev28Days,
        "7 Days Cases" = CasesPrev7Days)

    output$ewr_cases_per_spc <- DT::renderDataTable({
      ewr_cases_per_spc_data},
      filter = "top",
      server= FALSE,
      extensions = c('Buttons', 'FixedHeader'),
      options = list(
        stateSave = TRUE,
        searchCols = NULL,
        fixedHeader=TRUE,
        dom = 'lBftrip',
        pageLength = 10,
        scrollY = "",
        scrollX = T,
        buttons = list(
          list(extend = 'csv', filename = paste0(Sys.Date(),"ewr_cases_per_spc")),
          list(extend = 'excel', filename = paste0(Sys.Date(),"ewr_cases_per_spc")))
      )
    )
  })
}
    
## To be copied in the UI
# mod_ewr_helper_ui("ewr_helper_ui_1")
    
## To be copied in the server
# mod_ewr_helper_server("ewr_helper_ui_1")
