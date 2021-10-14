#' sch_report_epi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_report_epi_ui <- function(id){
  plotly::plotlyOutput(NS(id, "epicurve_plot"))
}
    
#' sch_report_epi Server Functions
#'
#' @noRd 
mod_sch_report_epi_server <- function(id, df, school_id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$epicurve_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionNameMerged[1]
      
      epicurve.table <- df() %>%
        dplyr::select(CaseNumber,
                      DateOfResult,
                      WgsVariant,
                      FirstName,
                      LastName) %>%
        dplyr::mutate(WgsVariant = ifelse(is.na(WgsVariant), 'Unknown', WgsVariant)) %>%
        dplyr::mutate(DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"),
                      NamesJoined = paste(FirstName, LastName, sep = " ")) %>% 
        dplyr::group_by(DateOfResult, WgsVariant) %>% 
        dplyr::tally()
      
      epicurve.table.plot <- ggplot2::ggplot(epicurve.table,
                                             ggplot2::aes(x = DateOfResult,
                                                          y = n,
                                                          fill = WgsVariant,
                                                          text = paste('Variant:', WgsVariant,
                                                                       '<br>Date: ', format(DateOfResult, "%d-%m-%Y"),
                                                                       '<br>Obs (n): ', n))) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_x_date(date_labels = "%d-%m-%y", breaks = "2 days") +
        ggplot2::labs(title = paste("Epicurve for", name),
                      x = "Date of Sample",
                      y = "Frequency") +
        ggplot2::theme_bw()
      
      plotly::ggplotly(epicurve.table.plot, tooltip = c("text")) %>% 
        plotly::layout(xaxis = list(tickangle = 45)) 
    })
  })
}
    
## To be copied in the UI
# mod_sch_report_epi_ui("sch_report_epi_ui_1")
    
## To be copied in the server
# mod_sch_report_epi_server("sch_report_epi_ui_1")
