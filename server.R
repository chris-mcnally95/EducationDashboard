
#------------------------------------------------------#
#### Server Script ####                                          
#------------------------------------------------------#

function(input, output, session) {
  
  #--------------HOME--------------
  
    # InfoBoxes
    
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
               formatC(nrow(schools_stats_overall %>%
                              dplyr::distinct(DENINumber, .keep_all = TRUE) %>%
                              tidyr::drop_na(InstitutionName)),
               format="d", big.mark=",")), 
        icon = icon("school"), 
        color ="blue")
    })
    
    ## Total Cases Last Week
    output$cases_last_week <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Reported Cases Last Week from Education Insitutions", 
        paste0(formatC(nrow(schools_cases %>% 
                              dplyr::filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8)), format="d", big.mark=",")), 
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
               formatC(nrow(schools_stats_overall %>%
                              dplyr::distinct(DENINumber, .keep_all = TRUE) %>%
                              tidyr::drop_na(InstitutionName)),
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
               formatC(nrow(schools_stats_overall %>%
                              dplyr::distinct(DENINumber, .keep_all = TRUE) %>%
                              tidyr::drop_na(InstitutionName)),
               format="d", big.mark=",")), 
        icon = icon("school"), 
        color = "navy")
    })
    
    
    #--------------WEEKLY REPORT--------------
    # Current case status
    # schools_cases_w_wgs_consolidated <- schools_cases_w_wgs %>% 
    #   mutate(InstitutionType = gsub("Grammar", "Secondary", InstitutionType)) %>% 
    #   mutate(InstitutionType = gsub("Secondary", "Post Primary", InstitutionType)) %>% 
    #   mutate(InstitutionType = gsub("Preps", "Primary", InstitutionType)) %>%
    #   mutate(InstitutionType = gsub("Nursery", "Preschool", InstitutionType)) %>% 
    #   mutate(InstitutionType = gsub("Further Education", "Post Primary", InstitutionType)) %>% 
    #   mutate(InstitutionType = factor(InstitutionType, levels = c("Preschool", "Primary", "Post Primary", "Independent", "Special")))
    # 
    # schools_cases_w_wgs_consolidated.today <- schools_cases_w_wgs_consolidated %>% 
    #   filter(DateOfSampleCases >= "2021-08-30")
    # 
    # current.status <- as.data.frame(table(schools_cases_w_wgs_consolidated$InstitutionType))
    # add.cases <- data.frame("Total", sum(current.status$Freq))
    # names(add.cases) <- c("Var1", "Freq")
    # current.status <- rbind(current.status, add.cases)
    # current.status$proportion <- c(round((current.status[1,2]/current.status[6,2])*100, 2),
    #                                round((current.status[2,2]/current.status[6,2])*100, 2),
    #                                round((current.status[3,2]/current.status[6,2])*100, 2),
    #                                round((current.status[4,2]/current.status[6,2])*100, 2),
    #                                round((current.status[5,2]/current.status[6,2])*100, 2),
    #                                "")
    # current.status$Var1 <-  factor(current.status$Var1, levels = c("Preschool", "Primary", "Post Primary", "Independent", "Special", "Total"))
    # current.status <- arrange(current.status, current.status$Var1)
    # colnames(current.status) <- c("SchoolType", "TotalToDate", "Proportion")
    # 
    # # Last week case status
    # schools_cases_w_wgs_consolidated.last.week <- schools_cases_w_wgs_consolidated %>% 
    #   filter(DateOfSampleCases >= "2021-08-30" & DateOfSampleCases <= Sys.Date()-8)
    # 
    # last.week.status <- as.data.frame(table(schools_cases_w_wgs_consolidated.last.week$InstitutionType))
    # add.cases.last.week <- data.frame("Total", sum(last.week.status$Freq))
    # names(add.cases.last.week) <- c("Var1", "Freq")
    # last.week.status <- rbind(last.week.status, add.cases.last.week)
    # last.week.status$Var1 <-  factor(last.week.status$Var1, levels = c("Preschool", "Primary", "Post Primary", "Independent", "Special", "Total"))
    # colnames(last.week.status) <- c("SchoolType", "TotalToDate")
    # 
    # current.status$LastWeekTotal <- last.week.status$TotalToDate
    # 
    # current.status.table <- current.status %>% 
    #   mutate(PercentageChange = round(((TotalToDate-LastWeekTotal)/TotalToDate)*100, 2)) %>% 
    #   mutate(Proportion = as.numeric(Proportion)) %>% 
    #   mutate(Increase = TotalToDate - LastWeekTotal)
    # current.status.table <- current.status.table[, c(1, 4, 2, 6, 5, 3)]
    # colnames(current.status.table) <- c("School Type", "Last Week Cumulative Cases", "This Week Cumulative Cases", "Weekly Increase",
    #                                     "Percentage Increase (%)", "Percentage Proportion of Total (%)")
    # 
    # ## Render Weekly Report Table
    # output$weekly_report_table = DT::renderDataTable({
    #   DT::datatable(current.status.table,
    # extensions = c('Buttons'),
    # options = list(
    #   dom = 'lBftrip',
    #   scrollX = T,
    #   buttons = list(
    #     list(extend = 'csv', filename = paste0(Sys.Date(),"_weekly_report")),
    #     list(extend = 'excel', filename = paste0(Sys.Date(),"_weekly_report")))))
    #   })
    
    
    #--------------SCHOOLS OVERVIEW--------------
    
    ## Run Schools Overview Table Server Module
    overview_server(id ="schools_overview",
                    stats_df = schools_stats_overall)
    
    
    #--------------SCHOOL REPORT--------------
    
    ## Assign Reactive Data Frames
    school <- shiny::reactive({
      dplyr::filter(schools_stats_overall, DENINumber == input$input_school_id)
    })
    
    schoolCases <- shiny::reactive({
      shiny::req(input$input_school_id)
      get_school_id <- input$input_school_id
      dplyr::filter(schools_cases_w_wgs, InstitutionReferenceNumber == get_school_id)
    })
    
    schoolContacts <- reactive({
      shiny::req(input$input_school_id)
      get_school_id <- input$input_school_id
      filter(close_contacts_for_schools, InstitutionReferenceNumber == get_school_id)
    })
    
    
    ## Key Info
    output$schoolName <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), InstitutionName))
    })
    
    output$schoolID <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), DENINumber))
    })
    
    output$schoolType <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), InstitutionType))
    })
    
    output$TotalPupils <- shiny::renderText({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), TotalPupils))
    })
    
    output$schoolAR7 <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), AttackRate7Days))
    })
    
    output$schoolAR14 <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), AttackRate14Days))
    })
    
    output$schoolAR28 <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), AttackRate28Days))
    })
    
    output$Area <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), Town))
    })
    
    output$PostCode <- shiny::renderText ({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), Postcode))
    })
    
    output$LGD <- shiny::renderText({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), LGDName))
    })
    
    output$CCR7DayLGD <- shiny::renderText({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), Cases7dayPer100kLGD))
    })
    
    output$CCR14DayLGD <- shiny::renderText({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), Cases14dayPer100kLGD))
    })
    
    output$CCR28DayLGD <- shiny::renderText({
      shiny::req(input$input_school_id)
      paste(dplyr::select(school(), Cases28dayPer100kLGD))
    })
    
    output$NI7dayrate <- shiny::renderText({
      paste(NI7dayrate)
    })
    
    output$NI14dayrate <- shiny::renderText({
      paste(NI14dayrate)
    })
    
    output$NI28dayrate <- shiny::renderText({
      paste(NI28dayrate)
    })
    
    ## InfoBoxes
    output$totalCases <- shinydashboard::renderInfoBox ({
      shiny::req(input$input_school_id)
      totalCases <- school()$TotalCases
      shinydashboard::infoBox(
        "Total Cases",paste(totalCases),
        icon = icon("male"),
        color = "blue"
      )
    })
    
    output$totalContacts <- shinydashboard::renderInfoBox ({
      shiny::req(input$input_school_id)
      totalContacts <- school()$TotalCloseContacts
      shinydashboard::infoBox(
        "Total Contacts",paste(totalContacts),
        icon = icon("users"),
        color = "light-blue"
      )
    })
    
    ## Plot EpiCurve
      ### Run School Report Epicurve Server Module  
      report_epicurve_server(id = "report_epicurve_plot",
                             df = schoolCases,
                             school_id = shiny::reactive(input$input_school_id))
  
    ## Plot School Year Data 
      ### Run School Report Year Server Module  
      report_year_server(id = "report_year_plot",
                         df = schoolCases,
                         school_id = shiny::reactive(input$input_school_id))
    
    ## Plot Attack Rate by Year
      ### Run School Report AR Server Module  
      report_ar_server(id = "report_ar_plot",
                       df = school,
                       school_id = shiny::reactive(input$input_school_id))
    
    ## School Cases Data Table
      ### Run School Report Cases Server Module  
      report_cases_server(id = "report_cases_table",
                          df = schoolCases,
                          school_id = shiny::reactive(input$input_school_id))
    
    ## School Contacts Data Table
      ### Run School Report Cases Server Module  
      report_contacts_server(id = "report_contacts_table",
                          df = schoolContacts,
                          school_id = shiny::reactive(input$input_school_id))
  
    
    #--------------PRIMARY SCHOOLS------------------
  
    ## Run Primary Schools Table Module
    primary_sch_server(id ="primary_schools_list",
                       df = schools_stats_overall)
      
    
    #--------------LOCATIONS REPORT------------------
    
    ## get the dates from location report
    locations_report_dates <- shiny::reactive ({
      input$locations_report_daterange
    })
    
    ## Run School Report Cases Module  
    locations_report_server(id = "locations_table",
                            df = schools_cases_w_clusters,
                            DateRange = locations_report_dates)
}





