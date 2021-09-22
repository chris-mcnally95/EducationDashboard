function(input, output, session) {
  
####### HOME  ####### 

  # InfoBoxes
  
  ## Total Cases
  output$total_cases <- renderInfoBox({
    infoBox(
      "All Time Reported Cases from Education Insitutions", 
      paste0(formatC(nrow(schools_cases), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color ="blue")
  })
  
  ## Total Schools
  output$total_groups <- renderInfoBox({
    infoBox(
      "All Time Reported Affected Education Insitutions", 
      paste0(formatC(nrow(schools_stats_overall %>%
                            distinct(DENINumber, .keep_all = TRUE) %>%
                            filter(TotalCases >= 1)%>% 
                            drop_na(InstitutionName)),
             format="d", big.mark=","), "/", 
             formatC(nrow(schools_stats_overall %>%
             distinct(DENINumber, .keep_all = TRUE) %>%
             drop_na(InstitutionName)),
             format="d", big.mark=",")), 
      icon = icon("school"), 
      color ="blue")
  })
  
  ## Total Cases Last Week
  output$cases_last_week <- renderInfoBox({
    infoBox(
      "Reported Cases Last Week from Education Insitutions", 
      paste0(formatC(nrow(schools_cases %>% 
                            filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8)), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color = "light-blue")
  })
  
  ## Total Schools Last Week
  output$groups_last_week <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected Last Week", 
      paste0(formatC(nrow(schools_cases_w_wgs %>%
                            filter(DateOfSampleCases >= Sys.Date()-14 & DateOfSampleCases <= Sys.Date()-8) %>% 
                            group_by(InstitutionReferenceNumber) %>% 
                            tally()),
                     format="d", big.mark=","), "/", 
             formatC(nrow(schools_stats_overall %>%
             distinct(DENINumber, .keep_all = TRUE) %>%
             drop_na(InstitutionName)),
             format="d", big.mark=",")), 
      icon = icon("school"), 
      color = "light-blue")
  })
  
  ## Total Cases This Week
  output$cases_this_week <- renderInfoBox({
    infoBox(
      "Reported Cases This Week from Education Insitutions", 
      paste0(formatC(nrow(schools_cases %>% 
                            filter(DateOfSampleCases >= Sys.Date()-7)),
                     format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color = "navy")
  })
  
  ## Total Schools This Week
  output$groups_this_week <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected This Week", 
      paste0(formatC(nrow(schools_cases_w_wgs %>%
                            filter(DateOfSampleCases >= Sys.Date()-7) %>% 
                            group_by(InstitutionReferenceNumber) %>% 
                            tally()),
                     format="d", big.mark=","), "/", 
             formatC(nrow(schools_stats_overall %>%
             distinct(DENINumber, .keep_all = TRUE) %>%
             drop_na(InstitutionName)),
             format="d", big.mark=",")), 
      icon = icon("school"), 
      color = "navy")
  })
  
  
  
  #--------------PRIMARY SCHOOLS------------------
  
  primary_schools_data <- schools_stats_overall %>%
    filter(InstitutionType == "Primary" | InstitutionType == "Preps") %>%
    select(
      DENINumber,
      InstitutionName,
     #InstitutionType,
      CasesPrev28Days,
      TotalPupils,
      AttackRateOverall,
      TotalCloseContacts,
      NurseryCases28Days,
      NurseryPupils,
      AttackRateNursery,
      ReceptionCases28Days,
      ReceptionPupils,
      AttackRateReception,
      Y1Cases28Days,
      Year1,
      AttackRateY1,
      Y2Cases28Days,
      Year2,
      AttackRateY2,
      Y3Cases28Days,
      Year3,
      AttackRateY3,
      Y4Cases28Days,
      Year4,
      AttackRateY4,
      Y5Cases28Days,
      Year5,
      AttackRateY5,
      Y6Cases28Days,
      Year6,
      AttackRateY6,
      Y7Cases28Days,
      Year7,
      AttackRateY7,
      StaffCases28Days
    )
  
  output$primary_schools_table <- DT::renderDataTable({
    primary_schools_data
    },
    server = FALSE,
    extensions = c('Buttons'),
    options = list(
      dom = 'lBftrip',
      pageLength = 10,
      scrollX = T,
      buttons = list(
        list(extend = 'csv', filename = "primary_schools_data"),
        list(extend = 'excel', filename = "primary_schools_data")),
      order = list(
        6,
        "desc"),
      columnDefs = list(
        list(visible = FALSE, targets = 0)))
  )
  
  #--------------SCHOOLS CASES TABLE--------------
  ## Build Table 
  
  home.page.table <- schools_stats_overall %>% 
    select(InstitutionName, Town, InstitutionType, DENINumber, CasesPrev28Days, CasesWithinLast6Days, CasesWithinLast4to6Days,
           CasesWithinLast3Days, TotalPupils, AttackRateOverall, CaseTrend)

  home.page.table <- home.page.table[order(-home.page.table$CasesPrev28Days), ]
  home.page.table$Town <- tolower(home.page.table$Town)
  home.page.table$Town <- toTitleCase(home.page.table$Town)
  colnames(home.page.table) <- c("Institution Name", "Town", "Institution Type", "DENI Number", "28 Days Cases", "7 Days Cases",
                                 "4 to 6 Days Cases", "3 Days Cases", "Total Pupils",
                                 "28 Day Attack Rate (%)", "Trend")

  ## Render Home Page Table
  output$education_cases_table = DT::renderDataTable({
   home.page.table
  },
  filter = "top",
  server= FALSE,
  extensions = c('Buttons'),
  options = list(
    pageLength = 25,
    dom = 'lBftrip',
    scrollX = T,
    buttons = list(
      list(extend = 'csv', filename = paste0(input$input_school_id,"_cases_line_listing")),
      list(extend = 'excel', filename = paste0(input$input_school_id,"_cases_line_listing")))))
  
  ####### SCHOOL REPORT #######
  
  # Assign Reactives
  school <- reactive({
    filter(schools_stats_overall, DENINumber == input$input_school_id)
  })
  
  schoolCases <- reactive({
    req(input$input_school_id)
    get_school_id <- input$input_school_id
    filter(schools_cases_w_wgs, InstitutionReferenceNumber == get_school_id)
  })
  
  schoolContacts <- reactive({
    req(input$input_school_id)
    filter(close_contacts_for_clusters, ClusterID == input$input_cluster_id)
  })
  
  # Key Info
  output$schoolName <- renderText ({
    req(input$input_school_id)
    paste(select(school(), InstitutionName))
  })
  
  output$schoolID <- renderText ({
    req(input$input_school_id)
    paste(select(school(), DENINumber))
  })
  
  output$schoolType <- renderText ({
    req(input$input_school_id)
    paste(select(school(), InstitutionType))
  })
  
  output$Area <- renderText ({
    req(input$input_school_id)
    paste(select(school(), Town))
  })
  
  output$PostCode <- renderText ({
    req(input$input_school_id)
    paste(select(school(), Postcode))
  })
  
  # InfoBox
  output$totalCases <- renderInfoBox ({
    req(input$input_school_id)
    totalCases <- school()$TotalCases
    infoBox(
      "Total Cases",paste(totalCases),
      icon = icon("male"),
      color = "blue"
    )
  })
  
  output$totalContacts <- renderInfoBox ({
    req(input$input_school_id)
    totalContacts <- school()$TotalCloseContacts
    infoBox(
      "Total Contacts",paste(totalContacts),
      icon = icon("male"),
      color = "light-blue"
    )
  })
  
  # # Plot EpiCurve
  # output$epicurve_plot <- renderPlotly({
  #   req(input$input_school_id)
  #   
  #   epicurve.table <- schoolCases() %>%
  #     select(CaseNumber,
  #            DateOfResult,
  #            WgsVariant) %>%
  #     mutate(WgsVariant = ifelse(is.na(WgsVariant), 'Unknown', WgsVariant)) %>% 
  #     mutate(DateOfResult = as.Date(DateOfResult))
  #   
  #   epicurve.table.plot <- ggplot(epicurve.table) + 
  #     geom_histogram(aes(x = DateOfResult, fill = WgsVariant)) + 
  #     labs(title = paste("Epicurve for", input$input_school_id), 
  #          x = "Date of Sample", 
  #          y = "Frequency") +
  #     theme_bw()
  #   
  #   ggplotly(epicurve.table.plot)
  # })

  # Plot School Year Data  
  output$school_year_plot <- renderPlotly({
    req(input$input_school_id)
    
    school.year.table <- schoolCases() %>% 
      select(CaseNumber, GenderCases, SchoolYear)
    
    school.year.table.plot <- ggplot(data = school.year.table, aes(x = SchoolYear, fill = GenderCases)) + 
      geom_bar(data = subset(school.year.table, GenderCases == "Female")) + 
      geom_bar(data = subset(school.year.table, GenderCases == "Male"), aes(y =..count..*(-1))) + 
      coord_flip() +
      scale_x_discrete(drop=FALSE) +
      labs(title = paste("Case Frequencies for", input$input_school_id), 
           x = "School Group", 
           y = "Frequency",
           fill = "Gender") +
      theme_bw()
    

    ggplotly(school.year.table.plot)
    
    
  }) 
  
  # Plot Attack Rate by Year
  output$attack_rate_plot <- renderPlotly({
    req(input$input_school_id)
    
    attack.rate.table <- school() %>% 
      select(AttackRateNursery,
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
      rename(Nursery = AttackRateNursery) %>% 
      rename(Reception = AttackRateReception) %>% 
      rename(Primary1 = AttackRateY1) %>% 
      rename(Primary2 = AttackRateY2) %>% 
      rename(Primary3 = AttackRateY3) %>% 
      rename(Primary4 = AttackRateY4) %>% 
      rename(Primary5 = AttackRateY5) %>% 
      rename(Primary6 = AttackRateY6) %>% 
      rename(Primary7 = AttackRateY7) %>% 
      rename(Year8 = AttackRateY8) %>% 
      rename(Year9 = AttackRateY9) %>% 
      rename(Year10 = AttackRateY10) %>% 
      rename(Year11 = AttackRateY11) %>% 
      rename(Year12 = AttackRateY12) %>% 
      rename(Year13 = AttackRateY13) %>% 
      rename(Year14 = AttackRateY14) %>% 
      pivot_longer(cols = everything(), names_to = "AttackRate", values_to = "count") %>% 
      mutate(AttackRate = factor(AttackRate, levels = c("Nursery", "Reception", "Primary1", "Primary2", "Primary3", "Primary4", "Primary5",
                                                        "Primary6", "Primary7", "Year8", "Year9", "Year10", "Year11", "Year12", "Year13", "Year14")))
    
    attack.rate.table.plot <- ggplot(data = attack.rate.table, aes(AttackRate, count)) + 
      geom_bar(stat = "identity", fill = "#408cbc") + 
      scale_x_discrete(drop=FALSE) +
      labs(title = paste("Attack Rates per Year for", input$input_school_id), 
           x = "School Year", 
           y = "28 Day Attack Rate (%)") +
      theme_bw()
    
    
    ggplotly(attack.rate.table.plot) %>% 
      layout(xaxis = list(tickangle = 45)) 
  }) 
  
  # Build School Cases Data Table
  output$school_cases_table = DT::renderDataTable({
    req(input$input_school_id)
    
    schoolCases() %>% 
      select(CaseNumber,
             FirstName,
             LastName,
             SchoolYear, 
             AgeAtPositiveResult, 
             GenderCases,
             DateOfResult,
             DateOfOnset,
             CloseContactCount) %>% 
      mutate(DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"))
  },
  server= FALSE,
  filter = "top",
  extensions = c('Buttons'),
  options = list(
    pageLength = 15,
    dom = 'lBftrip',
    scrollX = T,
    buttons = list(
      list(extend = 'csv', filename = paste0(input$input_school_id,"_cases_line_listing")),
      list(extend = 'excel', filename = paste0(input$input_school_id,"_cases_line_listing"))))
  )
  
}





