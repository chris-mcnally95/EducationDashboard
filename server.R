function(input, output, session) {
  
#--------------HOME--------------

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
  
  #--------------SCHOOLS CASES TABLE--------------
  ## Build Table 
  
  home.page.table <- schools_stats_overall %>% 
    select(InstitutionName, Town, InstitutionType, DENINumber, CasesPrev28Days, CasesPrev14Days, CasesPrev7Days, CasesWithinLast6Days, CasesWithinLast4to6Days,
           CasesWithinLast3Days, TotalPupils, AttackRate7Days, AttackRate14Days, AttackRate28Days, CaseTrend)

  home.page.table <- home.page.table[order(-home.page.table$CasesPrev28Days), ]
  home.page.table$Town <- tolower(home.page.table$Town)
  home.page.table$Town <- toTitleCase(home.page.table$Town)
  colnames(home.page.table) <- c("Institution Name", "Town", "Institution Type", "DENI Number", "28 Days Cases", "14 Days Cases", "7 Days Cases", "6 Days Cases",
                                 "4 to 6 Days Cases", "3 Days Cases", "Total Pupils", "7 Day Attack Rate (%)", "14 Day Attack Rate (%)",
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
  
  #--------------SCHOOLS REPORT--------------
  
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
    get_school_id <- input$input_school_id
    filter(close_contacts_for_schools, InstitutionReferenceNumber == get_school_id)
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
  
  output$schoolAR <- renderText ({
    req(input$input_school_id)
    paste(select(school(), AttackRate28Days))
  })
  
  output$Area <- renderText ({
    req(input$input_school_id)
    paste(select(school(), Town))
  })
  
  output$PostCode <- renderText ({
    req(input$input_school_id)
    paste(select(school(), Postcode))
  })
  
  # InfoBoxes
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
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  # Plot EpiCurve
  output$epicurve_plot <- renderPlotly({
    req(input$input_school_id)
    name <- schoolCases()$InstitutionNameCases[1]

    epicurve.table <- schoolCases() %>%
      select(CaseNumber,
             DateOfResult,
             WgsVariant) %>%
      mutate(WgsVariant = ifelse(is.na(WgsVariant), 'Unknown', WgsVariant)) %>%
      mutate(DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y"))

    epicurve.table.plot <- ggplot(epicurve.table) +
      geom_bar(aes(x = DateOfResult, fill = WgsVariant), position = "stack") +
      scale_x_date(date_labels = "%d-%m-%y", breaks = "1 day") +
      labs(title = paste("Epicurve for", name),
           x = "Date of Sample",
           y = "Frequency") +
      theme_bw()

    ggplotly(epicurve.table.plot) %>% 
      layout(xaxis = list(tickangle = 45)) 
  })

  # Plot School Year Data  
  output$school_year_plot <- renderPlotly({
    req(input$input_school_id)
    name <- schoolCases()$InstitutionNameCases[1]
    
    school.year.table <- schoolCases() %>% 
      select(CaseNumber, 
             GenderCases, 
             SchoolYear)
    
    school.year.table.plot <- ggplot(data = school.year.table, aes(x = SchoolYear, fill = GenderCases)) + 
      geom_bar(data = subset(school.year.table, GenderCases == "Female")) + 
      geom_bar(data = subset(school.year.table, GenderCases == "Male"), aes(y =..count..*(-1))) + 
      coord_flip() +
      scale_x_discrete(drop=FALSE) +
      labs(title = paste("Case Frequencies for", name), 
           x = "School Group", 
           y = "Frequency",
           fill = "Gender") +
      theme_bw()
    

    ggplotly(school.year.table.plot)
    
    
  }) 
  
  # Plot Attack Rate by Year
  output$attack_rate_plot <- renderPlotly({
    req(input$input_school_id)
    name <- school()$InstitutionName
    
    attack.rate.table <- school() %>% 
      select(
        AttackRateNursery,
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
      rename(
        Nursery = AttackRateNursery, 
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
      pivot_longer(cols = everything(), names_to = "AttackRate", values_to = "count") %>% 
      mutate(AttackRate = factor(AttackRate, levels = c("Nursery", "Reception", "Primary1", "Primary2", "Primary3", "Primary4", "Primary5",
                                                        "Primary6", "Primary7", "Year8", "Year9", "Year10", "Year11", "Year12", "Year13", "Year14")))
    
    attack.rate.table.plot <- ggplot(data = attack.rate.table, aes(AttackRate, count)) + 
      geom_bar(stat = "identity", fill = "#408cbc") + 
      scale_x_discrete(drop=FALSE) +
      labs(title = paste("Attack Rates per Year for", name), 
           x = "School Year", 
           y = "28 Day Attack Rate (%)") +
      theme_bw()
    
    
    ggplotly(attack.rate.table.plot) %>% 
      layout(xaxis = list(tickangle = 45)) 
  }) 
  
  # School Cases Data Table
  output$school_cases_table = DT::renderDataTable({
    req(input$input_school_id)
    name <- schoolCases()$InstitutionNameCases[1]
    
    DT::datatable(schoolCases() %>% 
      select(CaseNumber,
             FirstName,
             LastName,
             SchoolYear, 
             AgeAtPositiveResult, 
             GenderCases,
             DateOfResult,
             DateOfOnset,
             CloseContactCount) %>% 
      mutate(DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y")), 
      caption = paste("Line List for", name),
      filter = "top",
      extensions = c('Buttons'),
      options = list(
        pageLength = 15,
        dom = 'lBftrip',
        scrollX = T,
        buttons = list(
          list(extend = 'csv', filename = paste0(input$input_school_id,"_cases_line_listing")),
          list(extend = 'excel', filename = paste0(input$input_school_id,"_cases_line_listing")))))
  })
  
  # School Contacts Data Table
  output$school_contacts_table <- DT::renderDataTable ({
    req(input$input_school_id)
    
    DT::datatable(schoolContacts() %>%
      mutate(
          ContactOfCase = CaseNumber,
          DateOfLastContact = date(as.character.Date(DateOfLastContact)),
          DateSelfIsolationBegan = date(as.character.Date(DateSelfIsolationBegan))) %>% 
      select(
        FirstName,
        LastName,
        ContactPhoneNumber,
        ContactOfCase,
        DateOfLastContact,
        DateSelfIsolationBegan),
      filter = "top",
      extensions = c('Buttons'),
      options = list(
        dom = 'lBftrip',
        scrollX = T,
        buttons = list(
          list(extend = 'csv', filename = paste0(input$input_school_id,"_contacts_line_listing")),
          list(extend = 'excel', filename = paste0(input$input_school_id,"_contacts_line_listing"))),
        order = list(
          5,
          "desc"),
        columnDefs = list(
          list(visible = FALSE, targets = 0))))
  })

  
  #--------------PRIMARY SCHOOLS------------------
  
  primary_schools_data <- schools_stats_overall %>%
    filter(InstitutionType == "Primary" | InstitutionType == "Preps") %>%
    select(
      DENINumber,
      InstitutionName,
      CasesPrev7Days,
      CasesPrev14Days,
      CasesPrev28Days,
      PupilCases28Days,
      StaffCases28Days,
      TotalPupils,
      AttackRate7Days,
      AttackRate14Days,
      AttackRate28Days,
      CloseContacts28Days,
      TotalCases,
      TotalCloseContacts,
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
      NurseryCases28Days,
      NurseryPupils,
      AttackRateNursery,
      ReceptionCases28Days,
      ReceptionPupils,
      AttackRateReception) %>%
    rename(
      "DENI Number" = DENINumber,
      "Name" = InstitutionName,
      "Cases 7 Days" = CasesPrev7Days,
      "Cases 14 Days" = CasesPrev14Days,
      "Cases 28 Days" = CasesPrev28Days,
      "Pupil Cases 28 Days" = PupilCases28Days,
      "Staff Cases 28 Days" = StaffCases28Days,
      "Total Enrolled Pupils" = TotalPupils,
      "7 Day Attack Rate(%)" = AttackRate7Days,
      "14 Day Attack Rate(%)" = AttackRate14Days,
      "28 Day Attack Rate(%)" = AttackRate28Days,
      "28 Days Close Contacts" = CloseContacts28Days,
      "Total Cases Overall" = TotalCases,
      "Total Close Contacts Overall" = TotalCloseContacts,
      "Year 1 Cases 28 Days" = Y1Cases28Days,
      "Year 1 Enrolled Pupils" = Year1,
      "Year 1 28 Day Attack Rate(%)" = AttackRateY1,
      "Year 2 Cases 28 Days" = Y2Cases28Days,
      "Year 2 Enrolled Pupils" = Year2,
      "Year 2 28 Day Attack Rate(%)" = AttackRateY2,
      "Year 3 Cases 28 Days" = Y3Cases28Days,
      "Year 3 Enrolled Pupils" = Year3,
      "Year 3 28 Day Attack Rate(%)" = AttackRateY3,
      "Year 4 Cases 28 Days" = Y4Cases28Days,
      "Year 4 Enrolled Pupils" = Year4,
      "Year 4 28 Day Attack Rate(%)" = AttackRateY4,
      "Year 5 Cases 28 Days" = Y5Cases28Days,
      "Year 5 Enrolled Pupils" = Year5,
      "Year 5 28 Day Attack Rate(%)" = AttackRateY5,
      "Year 6 Cases 28 Days" = Y6Cases28Days,
      "Year 6 Enrolled Pupils" = Year6,
      "Year 6 28 Day Attack Rate(%)" = AttackRateY6,
      "Year 7 Cases 28 Days" = Y7Cases28Days,
      "Year 7 Enrolled Pupils" = Year7,
      "Year 7 28 Day Attack Rate(%)" = AttackRateY7,
      "Nursery Cases 28 Days" = NurseryCases28Days,
      "Nursery Enrolled Pupils" = NurseryPupils,
      "Nursery 28 Day Attack Rate(%)" = AttackRateNursery,
      "Reception Cases 28 Days" = ReceptionCases28Days,
      "Reception Enrolled Pupils" = ReceptionPupils,
      "Reception 28 Day Attack Rate(%)" = AttackRateReception)
  
  output$primary_schools_table <- DT::renderDataTable({
    primary_schools_data
  },
  filter = "top",
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
      9,
      "desc"),
    columnDefs = list(
      list(visible = FALSE, targets = 0)))
  )
  
  #--------------SCHOOLS CLUSTERS CASES------------------
  # 
  # locations_report_data <- schools_cases_w_wgs %>%
  #   filter(CreatedOnLocations == today) %>%
  #   select(
  #     CreatedOnMerged,
  #     
  #   ) %>%
  #   rename(
  #     )
  # 
  # output$locations_report_table <- DT::renderDataTable({
  #   locations_report_data
  # },
  # filter = "top",
  # server = FALSE,
  # extensions = c('Buttons'),
  # options = list(
  #   dom = 'lBftrip',
  #   pageLength = 10,
  #   scrollX = T,
  #   buttons = list(
  #     list(extend = 'csv', filename = "locations_report_data"),
  #     list(extend = 'excel', filename = "locations_report_data")),
  #   order = list(
  #     7,
  #     "desc"),
  #   columnDefs = list(
  #     list(visible = FALSE, targets = 0)))
  # )
  
}





