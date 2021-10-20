#' home_infoboxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_infoboxes_ui <- function(id){
  
  shinydashboard::tabItem(
    tabName = "open_education_cases",
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Welcome to the Education Dasboard Application",
        p("This application aims to capture children of school going age and is based on successfully 
              completed cases which report association with DENI registered schools or colleges from the 30/08/2021."), 
        p("There is an approximate 24 hour delay between a case being made and it appearing within the 
              Synpase data frame used in the making of this application."), 
        p("Each case shown in this application has been selected as it is a confirmed case with a known association to a school or college."),
        p(strong("Please Note: This application is under development and will undergo frequent updates and changes."))
      )
    ),
    
    shiny::fluidRow(
      shinycssloaders::withSpinner(shinydashboard::infoBoxOutput(shiny::NS(id, "total_cases"), width = 6), type = 2, color.background = "#ecf0f5"),
      shinydashboard::infoBoxOutput(shiny::NS(id, "total_groups"), width = 6)
    ),
    
    shiny::fluidRow(
      shinydashboard::infoBoxOutput(shiny::NS(id, "cases_last_week"), width = 6),
      shinydashboard::infoBoxOutput(shiny::NS(id, "groups_last_week"), width = 6)
    ),
    
    shiny::fluidRow(
      shinydashboard::infoBoxOutput(shiny::NS(id, "cases_this_week"), width = 6),
      shinydashboard::infoBoxOutput(shiny::NS(id, "groups_this_week"), width = 6)
    )# ,
    # 
    # shiny::fluidRow(
    #   shinydashboard::infoBoxOutput(shiny::NS(id, "difference_cases_this_week"), width = 6),
    #   shinydashboard::infoBoxOutput(shiny::NS(id, "difference_groups_this_week"), width = 6)
    # )
  )
}
    
#' home_infoboxes Server Functions
#'
#' @noRd 
mod_home_infoboxes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ## Total Cases
    output$total_cases <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Cases from Education Insitutions since 30/08/21", 
        paste0(formatC(nrow(schools_cases), format="d", big.mark=",")), 
        icon = icon("graduation-cap"), 
        color ="blue")
    })
    
    ## Total Schools
    output$total_groups <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Affected Education Insitutions since 30/08/21", 
        paste0(formatC(nrow(schools_stats_overall %>%
                              dplyr::distinct(DENINumber, .keep_all = TRUE) %>%
                              dplyr::filter(TotalCases >= 1)%>% 
                              tidyr::drop_na(InstitutionName)),
                       format="d", big.mark=","), "/", 
               formatC(nrow(schools_stats_overall),
                       format="d", big.mark=",")), 
        icon = icon("school"), 
        color ="blue")
    })
    
    ## Total Cases Last Week
    output$cases_last_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Cases Last Week from Education Insitutions", 
        paste0(formatC(nrow(schools_cases %>% 
                              dplyr::filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8)),
                       format="d", big.mark=",")), 
        icon = icon("graduation-cap"), 
        color = "light-blue")
    })
    
    ## Total Schools Last Week
    output$groups_last_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Education Insitutions Affected Last Week", 
        paste0(formatC(nrow(schools_cases_w_wgs %>%
                              dplyr::filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8) %>% 
                              dplyr::group_by(InstitutionReferenceNumber) %>% 
                              dplyr::tally()),
                       format="d", big.mark=","), "/", 
               formatC(nrow(schools_stats_overall),
                       format="d", big.mark=",")), 
        icon = icon("school"), 
        color = "light-blue")
    })
    
    ## Total Cases This Week
    output$cases_this_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Cases This Week from Education Insitutions", 
        paste0(formatC(nrow(schools_cases %>% 
                              dplyr::filter(DateOfSampleCases >= Sys.Date()-7)),
                       format="d", big.mark=",")), 
        icon = icon("graduation-cap"), 
        color = "navy")
    })
    
    ## Total Schools This Week
    output$groups_this_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Education Insitutions Affected This Week", 
        paste0(formatC(nrow(schools_cases_w_wgs %>%
                              dplyr::filter(DateOfSampleCases >= Sys.Date()-7) %>% 
                              dplyr::group_by(InstitutionReferenceNumber) %>%
                              dplyr::tally()),
                       format="d", big.mark=","), "/", 
               formatC(nrow(schools_stats_overall),
                       format="d", big.mark=",")), 
        icon = icon("school"), 
        color = "navy")
    })
    
    # ## Difference in Cases This Week compared to last
    # output$difference_cases_this_week <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(
    #     "Difference in Cases Compared to Last Week", 
    #     paste0(formatC(nrow(schools_cases %>% 
    #                           dplyr::filter(DateOfSampleCases >= Sys.Date()-7)) -
    #                      nrow(schools_cases %>% 
    #                             dplyr::filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8)),
    #                    format="d", big.mark=",")), 
    #     icon = icon("calculator"), 
    #     color = "aqua")
    # })
    # 
    # ## Difference in Schools This Week compared to last
    # output$difference_groups_this_week <- shinydashboard::renderInfoBox({
    #   shinydashboard::infoBox(
    #     "Difference in Affected Schools Compared to Last Week", 
    #     paste0(formatC(nrow(schools_cases_w_wgs %>%
    #                           dplyr::filter(DateOfSampleCases >= Sys.Date()-7) %>% 
    #                           dplyr::group_by(InstitutionReferenceNumber) %>%
    #                           dplyr::tally()) -
    #                      nrow(schools_cases_w_wgs %>%
    #                             dplyr::filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8) %>% 
    #                             dplyr::group_by(InstitutionReferenceNumber) %>% 
    #                             dplyr::tally()),
    #                    format="d", big.mark=",")), 
    #     icon = icon("calculator"), 
    #     color = "aqua")
    # })
  })
}
    
## To be copied in the UI
# mod_home_infoboxes_ui("home_infoboxes_ui_1")
    
## To be copied in the server
# mod_home_infoboxes_server("home_infoboxes_ui_1")
