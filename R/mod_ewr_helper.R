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
        p("The table below shows situations associated with a single school, with their postcode district since 30/08/21."),
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
        p("The table below shows situations associated with all cases and then schools in a postcode district since 30/08/21. The national total is also given as a baseline"),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(shiny::NS(id,"ewr_cases_all"))
        ),
        hr(),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(shiny::NS(id,"ewr_cases_per_pcd"))
        )
      )
    )
  )
}
    
#' ewr_helper Server Functions
#'
#' @noRd 
mod_ewr_helper_server <- function(id, df1, df2, df3){
  moduleServer( id, function(input, output, session){
    
    ## Build First Table
    ewr_cases_per_school_data <- df1 %>% 
      dplyr::select(PostcodeDistrict,
                    InstitutionName,
                    DENINumber,
                    CasesPrev28Days,
                    CasesPrev7Days) %>% 
      tidyr::drop_na(CasesPrev28Days) %>% 
      dplyr::arrange(desc(CasesPrev28Days)) %>% 
      dplyr::filter(CasesPrev28Days > 0)
    
    colnames(ewr_cases_per_school_data) <- c("Postcode District",
                                             "Institution Name",
                                             "DENI Number",
                                             "28 Days Cases",
                                             "7 Days Cases")
    
    ## Render First Table
    output$ewr_cases_per_school = DT::renderDT({
      ewr_cases_per_school_data},
      filter = "top",
      server= FALSE,
      extensions = c('Buttons'),
      options = list(
        stateSave = TRUE,
        searchCols = NULL,
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
    ewr_cases_per_pcd_data <- df2 %>%
      dplyr::select(
        PostcodeDistrict,
        CasesPrev28Days,
        CasesPrev7Days) %>%
      dplyr::left_join(df3, by = "PostcodeDistrict", suffix = c("Schools", "All")) %>%
      dplyr::mutate(DayProportion28 = round((CasesPrev28DaysSchools/CasesPrev28DaysAll)*100, 2),
                    DayProportion7 = round((CasesPrev7DaysSchools/CasesPrev7DaysAll)*100, 2)) %>% 
      dplyr::select(PostcodeDistrict,
                    CasesPrev28DaysAll,
                    CasesPrev28DaysSchools,
                    DayProportion28,
                    CasesPrev28DaysAll,
                    CasesPrev7DaysAll,
                    CasesPrev7DaysSchools,
                    DayProportion7) %>% 
      dplyr::arrange(desc(CasesPrev28DaysAll)) %>%
      tidyr::drop_na(PostcodeDistrict) %>% 
      dplyr::rename(
        "Postcode District" = PostcodeDistrict, 
        "28 Day School Cases" = CasesPrev28DaysSchools,
        "7 Day School Cases" = CasesPrev7DaysSchools,
        "28 Day Total Cases" = CasesPrev28DaysAll,
        "7 Day Total Cases" = CasesPrev7DaysAll,
        "Proportion Schools 28 Day (%)" = DayProportion28,
        "Proportion Schools 7 Day (%)" = DayProportion7) 
      


    ## Render Second Table
    output$ewr_cases_per_pcd <- DT::renderDataTable({
      ewr_cases_per_pcd_data},
      filter = "top",
      server= FALSE,
      extensions = c('Buttons', 'FixedHeader'),
      options = list(
        Filter = F,
        stateSave = TRUE,
        searchCols = NULL,
       #fixedHeader=TRUE,
        dom = 'lBftrip',
        pageLength = 10,
        scrollY = "",
        scrollX = T,
        buttons = list(
          list(extend = 'csv', filename = paste0(Sys.Date(),"ewr_cases_per_pcd")),
          list(extend = 'excel', filename = paste0(Sys.Date(),"ewr_cases_per_pcd")))
      )
    )
    
    # Whole of Northern Ireland
    ewr_cases_all_data <- df2 %>%
      dplyr::select(
        PostcodeDistrict,
        CasesPrev28Days,
        CasesPrev7Days) %>%
      dplyr::left_join(df3, by = "PostcodeDistrict", suffix = c("Schools", "All")) %>%
      dplyr::mutate(PostcodeDistrict = "Northern Ireland Total") %>% 
      dplyr::group_by(PostcodeDistrict) %>% 
      dplyr::summarise(CasesPrev28DaysAll = sum(CasesPrev28DaysAll),
                       CasesPrev28DaysSchools = sum(CasesPrev28DaysSchools),
                       CasesPrev7DaysAll = sum(CasesPrev7DaysAll),
                       CasesPrev7DaysSchools = sum(CasesPrev7DaysSchools)) %>% 
      dplyr::mutate(DayProportion28 = round((CasesPrev28DaysSchools/CasesPrev28DaysAll)*100, 2),
                    DayProportion7 = round((CasesPrev7DaysSchools/CasesPrev7DaysAll)*100, 2)) %>%
      dplyr::select(PostcodeDistrict,
                    CasesPrev28DaysAll,
                    CasesPrev28DaysSchools,
                    DayProportion28,
                    CasesPrev7DaysAll,
                    CasesPrev7DaysSchools,
                    DayProportion7) %>% 
      dplyr::arrange(desc(CasesPrev28DaysAll)) %>%
      dplyr::rename(
        " " = PostcodeDistrict, 
        "28 Day School Cases" = CasesPrev28DaysSchools,
        "7 Day School Cases" = CasesPrev7DaysSchools,
        "28 Day Total Cases" = CasesPrev28DaysAll,
        "7 Day Total Cases" = CasesPrev7DaysAll,
        "Proportion Schools 28 Day (%)" = DayProportion28,
        "Proportion Schools 7 Day (%)" = DayProportion7)

    ## Render Whole of NI
    output$ewr_cases_all <- DT::renderDataTable({
      ewr_cases_all_data},
      options = list(
        bFilter = F,
        lengthChange = F,
        info = F,
        bPaginate = F
      )
    )
  })
}
    
## To be copied in the UI
# mod_ewr_helper_ui("ewr_helper_ui_1")
    
## To be copied in the server
# mod_ewr_helper_server("ewr_helper_ui_1")
