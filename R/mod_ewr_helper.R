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
        title = "EWR Report Helper",
        p("Select a date range below to show probable outbreaks and clusters associated with schools broken down by 'Short' Post Code."),
        shiny::dateRangeInput(
          "daterange_ewr",
          "Date Range:",
          start = Sys.Date()-14,
          end = Sys.Date(),
          helpText("Select a date range below to show probable outbreaks and clusters broken down by 'Short' Post Code.")))
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Cases Per Cluster",
        p("The table below shows situations associated with schools, with their short post code as well as the number of cases associated with each situation."),
        hr(),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(shiny::NS(id,"ewr_cases_per_cluster"))
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Cases Associated with Situations Per Short Post Code",
        p("The table below shows the total number of cases associated with situations from a short post code."),
        hr(),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(shiny::NS(id,"ewr_cases_per_spc"))
        )
      )
    ) #,
    
    # shiny::fluidRow(
    #   shinydashboard::box(
    #     width = 12,
    #     status = "primary",
    #     solidHeader = TRUE,
    #     title = "Cases from SPC assoc. with any Situation",
    #     p("The table below shows the total number of cases that live in a short post code associated with any situations."),
    #     hr(),
    #     shinycssloaders::withSpinner(
    #       DT::dataTableOutput(shiny::NS(id,"ewr_cases_spc"))
    #     )
    #   )
    # )
  )

}
    
#' ewr_helper Server Functions
#'
#' @noRd 
mod_ewr_helper_server <- function(id, df, dates_ewr){
  moduleServer( id, function(input, output, session){

    #outbreaks summary object
    ewr_cases_per_cluster_data <- shiny::reactive ({
      
      shiny::req(dates_ewr())
      
      data <- df %>% #clusters_w_stats
        dplyr::filter(
          stringr::str_detect(ClusterName, "VOC|VUI") == FALSE,
          DateOfInitialReport >= dates_ewr()[1],
          DateOfInitialReport <= dates_ewr()[2]) %>%
        #group_by(
        #ShortPostCode) %>%
        dplyr::select(
          ShortPostCode,
          ClusterName,
          ClusterID,
          TotalCases) %>% 
        dplyr::arrange(TotalCases)
    })
    
    output$ewr_cases_per_cluster <- DT::renderDataTable({
      ewr_cases_per_cluster_data()
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
    
    ewr_cases_per_spc_data <- reactive ({
      
      shiny::req(dates_ewr())
      
      data <- df %>%
        dplyr::filter(
          stringr::str_detect(ClusterName, "VOC|VUI") == FALSE,
          DateOfInitialReport >= dates_ewr()[1],
          DateOfInitialReport <= dates_ewr()[2]) %>%
        dplyr::group_by(
          ShortPostCode) %>%
        dplyr::summarise(
          CasesPerSPC = sum(TotalCases, na.rm = TRUE)) %>% 
        dplyr::arrange(desc(CasesPerSPC))
      
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
          list(extend = 'csv', filename = paste0(Sys.Date(),"_cases_per_spc")),
          list(extend = 'excel', filename = paste0(Sys.Date(),"_cases_per_spc")))
      )
    )
    
    # ewr_cases_spc_data <- reactive ({
    #   
    #   shiny::req(dates_ewr())
    #   
    #   data <- all_cluster_cases_w_type_reflex %>%
    #     dplyr::filter(
    #       str_detect(ClusterName, "VOC|VUI") == FALSE,
    #       DateOfSample >= dates_ewr()[1],
    #       DateOfSample <= dates_ewr()[2]) %>%
    #     dplyr::distinct(CaseNumber, .keep_all = TRUE) %>%
    #     dplyr::group_by(
    #       ShortPostCode) %>%
    #     dplyr::summarise(
    #       TotalAnySituationCasesPerSPC = n())
    #   
    # })
    # 
    # output$ewr_cases_spc <- DT::renderDataTable({
    #   ewr_cases_spc_data()
    # },
    # server = FALSE,
    # extensions = 'Buttons',
    # options = list(
    #   dom = 'lBrtip',
    #   buttons = list(
    #     list(extend = 'csv', filename = paste("ewr_clustercases_per_spc")),
    #     list(extend = 'excel', filename = paste("ewr_clustercases_per_spc"))),
    #   order = list(
    #     2, 'desc'),
    #   columnDefs = list(
    #     list(visible = FALSE, targets = 0))
    # )
    # )
 
  })
}
    
## To be copied in the UI
# mod_ewr_helper_ui("ewr_helper_ui_1")
    
## To be copied in the server
# mod_ewr_helper_server("ewr_helper_ui_1")
