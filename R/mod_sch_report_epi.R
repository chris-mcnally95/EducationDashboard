#' sch_report_epi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_report_epi_ui <- function(id) {
  
  shinydashboard::box(
    width = 12,
    status = "primary",
    solidHeader = TRUE,
    title = "Epicurve by Sample Date",
    p("Selected school cases dates of sample are shown below."),
    hr(),
    
    shinydashboard::tabBox(
      width = 12,
      
      shiny::tabPanel(
        title = "VOC/VUI",
        p("Epicurve Plot with Case Variant Information"),
        hr(),
        shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, "variant_epi_plot")))
      ),
      
      shiny::tabPanel(
        title = "Symptomatic",
        p("Epicurve Plot with Case Symptom Status Information"),
        hr(),
        shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, "symptom_epi_plot")))
      ),
      
      shiny::tabPanel(
        title = "School Year",
        p("Epicurve Plot with Case School Year Information"),
        hr(),
        shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, "year_epi_plot")))
      ),
      
      shiny::tabPanel(
        title = "Clusters",
        p("Epicurve Plot with Case Cluster Information"),
        hr(),
        shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, "cluster_epi_plot")))
      )
    )
  )
}
    
#' sch_report_epi Server Functions
#'
#' @noRd 
mod_sch_report_epi_server <- function(id, df, school_id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Variant
    output$variant_epi_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionName[1]
      
      epicurve.table <- df() %>%
        dplyr::select(CaseNumber,
                      DateOfResult,
                      WgsVariant,
                      FirstNameSC,
                      LastNameSC) %>%
        dplyr::mutate(WgsVariant = ifelse(is.na(WgsVariant), 'Unknown', WgsVariant)) %>%
        dplyr::mutate(
          DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"),
          NamesJoined = paste(FirstNameSC, LastNameSC, sep = " ")
        ) %>%
        dplyr::group_by(DateOfResult, WgsVariant) %>%
        dplyr::tally()
      
      epicurve.variant.plot <- ggplot2::ggplot(
        epicurve.table,
        ggplot2::aes(
          x = DateOfResult,
          y = n,
          fill = WgsVariant,
          text = paste(
            'Variant:',
            WgsVariant,
            '<br>Date: ',
            format(DateOfResult, "%d-%m-%Y"),
            '<br>Obs (n): ',
            n
          )
        )
      ) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_x_date(date_labels = "%d-%m-%y", breaks = "2 days") +
        ggplot2::labs(title = paste("Epicurve for", name),
                      x = "Date of Sample",
                      y = "Frequency") +
        ggplot2::theme_bw()
      
      plotly::ggplotly(epicurve.variant.plot, tooltip = c("text")) %>%
        plotly::layout(xaxis = list(tickangle = 45))
    })
    
    # Symptomatic
    output$symptom_epi_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionName[1]
      
      epicurve.table <- df() %>%
        dplyr::select(CaseNumber,
                      DateOfResult,
                      IsSymptomatic,
                      FirstNameSC,
                      LastNameSC) %>%
        dplyr::mutate(
          DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"),
          NamesJoined = paste(FirstNameSC, LastNameSC, sep = " ")
        ) %>%
        dplyr::group_by(DateOfResult, IsSymptomatic) %>%
        dplyr::tally()
      
      epicurve.symptom.plot <- ggplot2::ggplot(
        epicurve.table,
        ggplot2::aes(
          x = DateOfResult,
          y = n,
          fill = IsSymptomatic,
          text = paste(
            'Symptomatic:',
            IsSymptomatic,
            '<br>Date: ',
            format(DateOfResult, "%d-%m-%Y"),
            '<br>Obs (n): ',
            n
          )
        )
      ) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_x_date(date_labels = "%d-%m-%y", breaks = "2 days") +
        ggplot2::labs(title = paste("Epicurve for", name),
                      x = "Date of Sample",
                      y = "Frequency") +
        ggplot2::theme_bw()
      
      plotly::ggplotly(epicurve.symptom.plot, tooltip = c("text")) %>%
        plotly::layout(xaxis = list(tickangle = 45))
    })
    
    # School Year
    output$year_epi_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionName[1]
      
      epicurve.table <- df() %>%
        dplyr::select(CaseNumber,
                      DateOfResult,
                      SchoolYear,
                      FirstNameSC,
                      LastNameSC) %>%
        dplyr::mutate(
          DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"),
          NamesJoined = paste(FirstNameSC, LastNameSC, sep = " ")
        ) %>%
        dplyr::group_by(DateOfResult, SchoolYear) %>%
        dplyr::tally()
      
      epicurve.year.plot <- ggplot2::ggplot(
        epicurve.table,
        ggplot2::aes(
          x = DateOfResult,
          y = n,
          fill = forcats::fct_rev(SchoolYear),
          text = paste(
            'Year:',
            SchoolYear,
            '<br>Date: ',
            format(DateOfResult, "%d-%m-%Y"),
            '<br>Obs (n): ',
            n
          )
        )
      ) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_x_date(date_labels = "%d-%m-%y", breaks = "2 days") +
        ggplot2::labs(title = paste("Epicurve for", name),
                      x = "Date of Sample",
                      y = "Frequency") +
        ggplot2::theme_bw()
      
      plotly::ggplotly(epicurve.year.plot, tooltip = c("text")) %>%
        plotly::layout(xaxis = list(tickangle = 45))
    })
    
    # Cluster
    output$cluster_epi_plot <- plotly::renderPlotly({
      shiny::req(school_id())
      name <- df()$InstitutionName[1]
      
      epicurve.table <- df() %>%
        dplyr::select(CaseNumber,
                      DateOfResult,
                      ClusterID,
                      FirstNameSC,
                      LastNameSC) %>%
        dplyr::mutate(ClusterID = ifelse(is.na(ClusterID), 'Not Assigned', ClusterID)) %>%
        dplyr::mutate(
          DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"),
          NamesJoined = paste(FirstNameSC, LastNameSC, sep = " ")
        ) %>%
        dplyr::group_by(DateOfResult, ClusterID) %>%
        dplyr::tally()
      
      epicurve.variant.plot <- ggplot2::ggplot(
        epicurve.table,
        ggplot2::aes(
          x = DateOfResult,
          y = n,
          fill = ClusterID,
          text = paste(
            'Cluster ID:',
            ClusterID,
            '<br>Date: ',
            format(DateOfResult, "%d-%m-%Y"),
            '<br>Obs (n): ',
            n
          )
        )
      ) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_x_date(date_labels = "%d-%m-%y", breaks = "2 days") +
        ggplot2::labs(title = paste("Epicurve for", name),
                      x = "Date of Sample",
                      y = "Frequency") +
        ggplot2::theme_bw()
      
      plotly::ggplotly(epicurve.variant.plot, tooltip = c("text")) %>%
        plotly::layout(xaxis = list(tickangle = 45))
    })
  })
}
    
## To be copied in the UI
# mod_sch_report_epi_ui("sch_report_epi_ui_1")
    
## To be copied in the server
# mod_sch_report_epi_server("sch_report_epi_ui_1")
