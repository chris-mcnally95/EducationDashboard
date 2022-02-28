#' sch_report_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_report_year_ui <- function(id){
  plotly::plotlyOutput(NS(id, "school_year_plot"), height = NULL)
}

    
#' sch_report_year Server Functions
#'
#' @noRd 
mod_sch_report_year_server <- function(id, df, school_id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$school_year_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionName[1]
      
      school.year.table <- df() %>% 
        dplyr::select(CaseNumber, 
                      Gender, 
                      SchoolYear,
                      DateOfResult) %>%
        dplyr::filter(DateOfResult >= twentyeightdays+1 & DateOfResult < today)
      
      school.year.table.plot <- ggplot2::ggplot(data = school.year.table, ggplot2::aes(x = SchoolYear, fill = Gender)) + 
        ggplot2::geom_bar(data = subset(school.year.table, Gender == "Female")) + 
        ggplot2::geom_bar(data = subset(school.year.table, Gender == "Male"), ggplot2::aes(y =..count..*(-1))) + 
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(drop=FALSE) +
        ggplot2::labs(title = paste("Case Frequencies for", name), 
                      x = "School Group", 
                      y = "Frequency",
                      fill = "Gender") +
        ggplot2::theme_bw()
      
      
      plotly::ggplotly(school.year.table.plot)
    }) 
  })
}

## To be copied in the UI
# mod_sch_report_year_ui("sch_report_year_ui_1")
    
## To be copied in the server
# mod_sch_report_year_server("sch_report_year_ui_1")
