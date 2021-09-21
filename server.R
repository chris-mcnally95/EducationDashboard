function(input, output, session) {
  
  ####### HOME  ####### 
  # InfoBoxes
  
  ## Total Cases
  output$total_cases <- renderInfoBox({
    infoBox(
      "Total Reported Cases from Education Insitutions", 
      paste0(formatC(nrow(schools_cases), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color ="blue")
  })
  
  ## Total Schools
  output$total_groups <- renderInfoBox({
    infoBox(
      "Total Reported Affected Education Insitutions", 
      paste0(formatC(nrow(schools_stats_overall %>%
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
                            filter(DateOfSample.x >= Sys.Date()-14 & DateOfSample.x <= Sys.Date()-8)), format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color = "light-blue")
  })
  
  ## Total Schools Last Week
  output$groups_last_week <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected Last Week", 
      paste0(formatC(nrow(schools_cases_w_wgs %>%
                            filter(DateOfSample.x >= Sys.Date()-14 & DateOfSample.x <= Sys.Date()-8) %>% 
                            group_by(InstitutionReferenceNumber) %>% 
                            tally()),
                     format="d", big.mark=",")), 
      icon = icon("school"), 
      color = "light-blue")
  })
  
  ## Total Cases This Week
  output$cases_this_week <- renderInfoBox({
    infoBox(
      "Reported Cases This Week from Education Insitutions", 
      paste0(formatC(nrow(schools_cases %>% 
                            filter(DateOfSample.x >= Sys.Date()-7)),
                     format="d", big.mark=",")), 
      icon = icon("graduation-cap"), 
      color = "navy")
  })
  
  ## Total Schools This Week
  output$groups_this_week <- renderInfoBox({
    infoBox(
      "Education Insitutions Affected This Week", 
      paste0(formatC(nrow(schools_cases_w_wgs %>%
                            filter(DateOfSample.x >= Sys.Date()-7) %>% 
                            group_by(InstitutionReferenceNumber) %>% 
                            tally()),
                     format="d", big.mark=",")), 
      icon = icon("school"), 
      color = "navy")
  })
  
  
  # Frequency Table
  
  ## Build Table 
  
  home.page.table <- schools_stats_overall %>% 
    select(InstitutionName, Town, InstitutionType, DENINumber, CasesPrev28Days, CasesWithinLast6Days, CasesWithinLast4to6Days,
           CasesWithinLast3Days, TotalPupils, AttackRate, CaseTrend)
  
  home.page.table <- home.page.table[order(-home.page.table$CasesPrev28Days), ]
  home.page.table$Town <- tolower(home.page.table$Town)
  home.page.table$Town <- toTitleCase(home.page.table$Town)
  colnames(home.page.table) <- c("Institution Name", "Town", "Institution Type", "DENI Number", "28 Days Cases", "7 Days Cases",
                                 "4 to 6 Days Cases", "3 Days Cases", "Total Pupils",
                                 "28 Day Attack Rate (%)", "Trend")
  
  ## Downloadable csv of Table 
  output$DownloadHomeReport <- downloadHandler(
    filename = function() {
      paste("Prev28DayEducationReport-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(home.page.table, file, row.names = FALSE)
    }
  )
  
  ## Render Home Page Table
  output$education_cases_table = DT::renderDataTable({
    DT::datatable(home.page.table,
                  options = list(pageLength = 25))
  })
  
  ####### SCHOOL YEAR GRAPH ####### 
  
  # Build School Data Frame
  output$school_year_table <- renderPlotly({
    
    school.year.table <- schools_cases %>% 
      filter(InstitutionReferenceNumber == input$input_school_id) %>% 
      select(CaseNumber, Gender.x, SchoolYear) %>% 
      mutate(SchoolYear = as.factor(SchoolYear)) 
    
    
    school.year.table.plot <- ggplot(data = school.year.table, aes(x = SchoolYear, fill = Gender.x)) + 
      geom_bar(data = subset(school.year.table, Gender.x == "Female")) + 
      geom_bar(data = subset(school.year.table, Gender.x == "Male"), aes(y =..count..*(-1))) + 
      coord_flip() +
      labs(title = paste("Case Frequencies for", input$input_school_id), 
           x = "School Group", 
           y = "Frequency",
           fill = "Gender") +
      theme_bw()
    
    ggplotly(school.year.table.plot)
    
  }) 
  
}





