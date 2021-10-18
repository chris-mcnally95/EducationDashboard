#' sch_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sch_overview_ui <- function(id){
  shinydashboard::tabItem(
    tabName = "school_cases_table",
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Education Institution Frequencies",
        p("Schools and their associate cases recorded after the 30/08/21 are grouped and tallied below. If a school has yet to be associated with a case they will not appear in the table."),
        # p(strong("Please note:"), "There is a hyperlink function enabled on each row of the table. If a row is selected, the DENI number of that school is automatically inserted
        #    into the School Report tab in the next section. When using multiple filters at once, to remove the box that opens after you have selected your desired input, please click the whitespace",
        #    strong("above"), "the table and not the table itself."),
        hr(),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(NS(id, "overview_table"))
        )
      )
    )
  )
}
    
#' sch_overview Server Functions
#'
#' @noRd 
mod_sch_overview_server <- function(id, stats_df){
  shiny::moduleServer(id, function(input, output, session){ 
    
    ## Build Overview Table
    home.page.table <- stats_df %>% #schools_stats_overall
      dplyr::mutate(CaseChange = CasesWithinLast3Days - CasesWithinLast4to6Days) %>% 
      dplyr::select(InstitutionName,
                    Town,
                    InstitutionType,
                    DENINumber,
                    EarliestSample,
                    MostRecentSample,
                    CasesPrev28Days,
                    CasesPrev14Days,
                    CasesPrev7Days,
                    #CasesWithinLast6Days,
                    CasesWithinLast4to6Days,
                    CasesWithinLast3Days,
                    TotalPupils,
                    AttackRatePupils28Days,
                    AttackRatePupils14Days,
                    AttackRatePupils7Days,
                    CaseChange,
                    CaseTrend) %>%
      dplyr::mutate(AttackRatePupils7Days = round(AttackRatePupils7Days, 0),
             AttackRatePupils14Days = round(AttackRatePupils14Days, 0),
             AttackRatePupils28Days = round(AttackRatePupils28Days, 0)) %>% 
      dplyr::mutate(AttackRatePupils7Days = as.integer(AttackRatePupils7Days),
             AttackRatePupils14Days = as.integer(AttackRatePupils14Days),
             AttackRatePupils28Days = as.integer(AttackRatePupils28Days)) %>% 
      tidyr::drop_na(EarliestSample) 
    #%>% 
    # mutate(EarliestSample = format(EarliestSample,"%d-%m-%Y"),
    #        MostRecentSample = format(MostRecentSample, "%d-%m-%Y")) #This changes the date to standard format but you lose the ability to sort 
    
    home.page.table <- home.page.table[order(-home.page.table$CasesPrev28Days), ]
    home.page.table$Town <- tolower(home.page.table$Town)
    home.page.table$Town <- tools::toTitleCase(home.page.table$Town)
    colnames(home.page.table) <- c("Institution Name",
                                   "Town",
                                   "Institution Type",
                                   "DENI Number",
                                   "Earliest Sample",
                                   "Latest Sample",
                                   "28 Days Cases",
                                   "14 Days Cases",
                                   "7 Days Cases",
                                   #"6 Days Cases",
                                   "4 to 6 Days Cases",
                                   "3 Days Cases",
                                   "Total Pupils",
                                   "28 Day Attack Rate (%)",
                                   "14 Day Attack Rate (%)",
                                   "7 Day Attack Rate (%)",
                                   "Case Change (0-3 v 4-6)",
                                   "Trend")
    
    ## Render Overview Page Table
    output$overview_table = DT::renderDT({
      home.page.table},
      #   callback=JS(
      #    'table.on("click.dt", "tr", function() {
      #      
      #       
      #     tabs = $("#shiny-tab-school_report a");
      #     var data=table.row(this).data();
      #     document.getElementById("input_school_id").value=data[4];
      #     Shiny.onInputChange("input_school_id",data[4]);
      #     $(tabs[1]).click();
      #     table.row(this).deselect();
      #     
      #    
      #     })'                     
      #   ), 
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
          list(extend = 'csv', filename = paste0(Sys.Date(),"_schools_overview")),
          list(extend = 'excel', filename = paste0(Sys.Date(),"_schools_overview")))
      )
    )
  }) 
}
    
## To be copied in the UI
# mod_sch_overview_ui("sch_overview_ui_1")
    
## To be copied in the server
# mod_sch_overview_server("sch_overview_ui_1")
