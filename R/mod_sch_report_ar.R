#' sch_report_ar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_report_ar_ui <- function(id){
  plotly::plotlyOutput(NS(id, "attack_rate_plot"), height = NULL)
}
    
#' sch_report_ar Server Functions
#'
#' @noRd 
mod_sch_report_ar_server <- function(id, df, school_id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$attack_rate_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionName
      
      attack.rate.table <- df() %>% 
        dplyr::select(AttackRateNursery,
                      AttackRateReception,
                      AttackRateY1,
                      AttackRateY2,
                      AttackRateY3,
                      AttackRateY4,
                      AttackRateY5,
                      AttackRateY6,
                      AttackRateY7,
                      AttackRateY8,
                      AttackRateY9,
                      AttackRateY10,
                      AttackRateY11,
                      AttackRateY12,
                      AttackRateY13,
                      AttackRateY14,
                      AttackRateSN) %>% 
        dplyr::rename(Nursery = AttackRateNursery, 
                      Reception = AttackRateReception,
                      Primary1 = AttackRateY1,
                      Primary2 = AttackRateY2,
                      Primary3 = AttackRateY3,
                      Primary4 = AttackRateY4,
                      Primary5 = AttackRateY5,
                      Primary6 = AttackRateY6,
                      Primary7 = AttackRateY7,
                      Year8 = AttackRateY8,
                      Year9 = AttackRateY9,
                      Year10 = AttackRateY10,
                      Year11 = AttackRateY11,
                      Year12 = AttackRateY12, 
                      Year13 = AttackRateY13,
                      Year14 = AttackRateY14) %>% 
        tidyr::pivot_longer(cols = everything(), names_to = "AttackRate", values_to = "count") %>% 
        dplyr::mutate(AttackRate = factor(AttackRate, levels = c("Nursery", "Reception", "Primary1", "Primary2", "Primary3", "Primary4", "Primary5",
                                                                 "Primary6", "Primary7", "Year8", "Year9", "Year10", "Year11", "Year12", "Year13", "Year14")))
      
      attack.rate.table.plot <- ggplot2::ggplot(data = attack.rate.table, ggplot2::aes(AttackRate, count)) + 
        ggplot2::geom_bar(stat = "identity", fill = "#408cbc") + 
        ggplot2::scale_x_discrete(drop=FALSE) +
        ggplot2:: coord_flip() +
        ggplot2:: labs(title = paste("Attack Rates per Year for", name), 
                       x = "School Year", 
                       y = "28 Day Attack Rate (%)") +
        ggplot2::scale_y_continuous(limits = c(0, 100), n.breaks = 10) +
        ggplot2::theme_bw()
      
      
      plotly::ggplotly(attack.rate.table.plot) %>% 
        plotly::layout(xaxis = list(tickangle = 45)) 
    }) 
  })
}
    
## To be copied in the UI
# mod_sch_report_ar_ui("sch_report_ar_ui_1")
    
## To be copied in the server
# mod_sch_report_ar_server("sch_report_ar_ui_1")
