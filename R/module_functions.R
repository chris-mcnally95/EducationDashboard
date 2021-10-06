#------------------------------------------------------#
#### School Overview Modules                        ####                                          
#------------------------------------------------------#

  # Server
    ## Schools Overview Table Server Module
    overview_server <- function(id, stats_df){
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
                        AttackRate28Days,
                        AttackRate14Days,
                        AttackRate7Days,
                        CaseChange,
                        CaseTrend) %>%
                 mutate(AttackRate7Days = round(AttackRate7Days, 0),
                        AttackRate14Days = round(AttackRate14Days, 0),
                        AttackRate28Days = round(AttackRate28Days, 0)) %>% 
                 mutate(AttackRate7Days = as.integer(AttackRate7Days),
                        AttackRate14Days = as.integer(AttackRate14Days),
                        AttackRate28Days = as.integer(AttackRate28Days)) %>% 
                 drop_na(EarliestSample) 
              #%>% 
              # mutate(EarliestSample = format(EarliestSample,"%d-%m-%Y"),
              #        MostRecentSample = format(MostRecentSample, "%d-%m-%Y")) #This changes the date to standard format but you lose the ability to sort 
        
        home.page.table <- home.page.table[order(-home.page.table$CasesPrev28Days), ]
        home.page.table$Town <- tolower(home.page.table$Town)
        home.page.table$Town <- toTitleCase(home.page.table$Town)
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
  
  # UI
    ## Schools Overview Table UI Module
    overview_UI <- function(id){
      DT::dataTableOutput(NS(id, "overview_table"))
    }
 
#------------------------------------------------------#
#### School Report Modules                          ####                                          
#------------------------------------------------------# 
  # Server
  ## School Report Server Modules
    ### School Report Epicurve Server Module
    report_epicurve_server <- function(id, df, school_id){
      shiny::moduleServer(id, function(input, output, session){
        
        output$epicurve_plot <- renderPlotly({
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
  
    ### School Report Year Server Module
    report_year_server <- function(id, df, school_id){
      shiny::moduleServer(id, function(input, output, session){
        
        output$school_year_plot <- renderPlotly({
          shiny::req(school_id())
          name <- df()$InstitutionNameMerged[1]
          
          school.year.table <- df() %>% 
            dplyr::select(CaseNumber, 
                          GenderCases, 
                          SchoolYear)
          
          school.year.table.plot <- ggplot2::ggplot(data = school.year.table, aes(x = SchoolYear, fill = GenderCases)) + 
            ggplot2::geom_bar(data = subset(school.year.table, GenderCases == "Female")) + 
            ggplot2::geom_bar(data = subset(school.year.table, GenderCases == "Male"), ggplot2::aes(y =..count..*(-1))) + 
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
    
    ### Build School Report AR Server Module
    report_ar_server <- function(id, df, school_id){
      shiny::moduleServer(id, function(input, output, session){
        
        output$attack_rate_plot <- renderPlotly({
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
          
          attack.rate.table.plot <- ggplot2::ggplot(data = attack.rate.table, aes(AttackRate, count)) + 
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
    
    ### School Report Cases Server Module
    report_cases_server <- function(id, df, school_id){
      shiny::moduleServer(id, function(input, output, session){
        
        output$school_cases_table = DT::renderDataTable({
          shiny::req(school_id())
          name <- df()$InstitutionNameMerged[1]
          
          DT::datatable(df() %>% 
                          dplyr::select(CaseNumber,
                                        FirstName,
                                        LastName,
                                        SchoolYear, 
                                        AgeAtPositiveResult, 
                                        GenderCases,
                                        DateOfResult,
                                        DateOfOnset,
                                        CloseContactCount) %>% 
                          dplyr::mutate(DateOfResult = as.Date(DateOfResult, format = "%d-%m-%Y")), 
                        caption = paste("Line List for", name),
                        filter = "top",
                        extensions = c('Buttons'),
                        options = list(
                          pageLength = 15,
                          dom = 'lBftrip',
                          scrollX = T,
                          buttons = list(
                            list(extend = 'csv', filename = paste0(school_id(),"_cases_line_listing")),
                            list(extend = 'excel', filename = paste0(school_id(),"_cases_line_listing")))))
        })
      })
    }
    
    ### School Report Contacts Server Module
    report_contacts_server <- function(id, df, school_id){
      shiny::moduleServer(id, function(input, output, session){
        
        output$school_contacts_table <- DT::renderDataTable ({
          shiny::req(school_id())
          
          DT::datatable(df() %>%
                          dplyr::mutate(ContactOfCase = CaseNumber,
                                        DateOfLastContact = date(as.character.Date(DateOfLastContact)),
                                        DateSelfIsolationBegan = date(as.character.Date(DateSelfIsolationBegan))) %>% 
                          dplyr::select(FirstName,
                                        LastName,
                                        ContactPhoneNumber,
                                        ContactOfCase,
                                        DateOfLastContact),
                        filter = "top",
                        extensions = c('Buttons'),
                        options = list(
                          dom = 'lBftrip',
                          scrollX = T,
                          buttons = list(
                            list(extend = 'csv', filename = paste0(school_id(),"_contacts_line_listing")),
                            list(extend = 'excel', filename = paste0(school_id(),"_contacts_line_listing"))),
                          order = list(
                            5,
                            "desc"),
                          columnDefs = list(
                            list(visible = FALSE, targets = 0))))
        })
      })
    }
      
  # UI
  ## Schools Report UI Modules
      ### School Report Epicurve UI Module
      report_epicurve_UI <- function(id){
        plotly::plotlyOutput(NS(id, "epicurve_plot"))
      }
      
      ### School Report Year UI Module
      report_year_UI <- function(id){
        plotly::plotlyOutput(NS(id, "school_year_plot"), height = NULL)
      }
      
      ### Build School Report AR UI Module
      report_ar_UI <- function(id){
        plotly::plotlyOutput(NS(id, "attack_rate_plot"), height = NULL)
      }
      
      ### School Report Cases UI Module
      report_cases_UI <- function(id){
        DT::dataTableOutput(NS(id, "school_cases_table"))
      }
      
      ### School Report Contacts UI Module
      report_contacts_UI <- function(id){
        DT::dataTableOutput(NS(id, "school_contacts_table"))
      }
  
#------------------------------------------------------#
#### Primary Schools Modules                        ####                                          
#------------------------------------------------------#
  
  # Server
    ## Primary Schools Table Server Module
    primary_sch_server <- function(id, df){
      shiny::moduleServer(id, function(input, output, session){ 
        
        ## Build Primary Schools Table
        primary_schools_data <- df %>%
          dplyr::filter(InstitutionType == "Primary" | InstitutionType == "Preps") %>%
          dplyr::select(DENINumber,
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
         dplyr::rename("DENI Number" = DENINumber,
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
        
        ## Render Primary Schools Table
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
      })
    }

  # UI
    ## Primary Schools UI Module
    primary_sch_UI <- function(id){
      DT::dataTableOutput(NS(id, "primary_schools_table"))
    }
    
#------------------------------------------------------#
#### Locations Report Modules                       ####                                          
#------------------------------------------------------#   

    # Server
    ## Locations Report Table Server Module
    locations_report_server <- function(id, df, DateRange){
      shiny::moduleServer(id, function(input, output, session){
        locations_report_data <- reactive ({
          
          ## Build Locations Report Table
          shiny::req(DateRange())
           df %>%
           dplyr::filter(
              CreatedOnLocations >= DateRange()[1],
              CreatedOnLocations <= DateRange()[2]) %>%
            dplyr::select(
              CreatedOnLocations, CaseNumber, FirstNameSC, LastNameSC,
              AgeAtPositiveResultSC, DateOfBirth, DateOfSampleCases, ClusterID, ClusterName, 
              #AdditionDate, 
              InstitutionReferenceNumber, InstitutionNameMerged, AddressLine1Merged, 
              #AddressLine2Merged, AddressLine3Merged, 
              CityLocations) %>%
            dplyr::rename(
              "Created On" = CreatedOnLocations, 
              "Case Number" = CaseNumber, 
              "First Name" = FirstNameSC, 
              "Last Name" = LastNameSC,
              "Age" = AgeAtPositiveResultSC, 
              "Date Of Birth" = DateOfBirth, 
              "Date Of Sample" = DateOfSampleCases, 
              "Cluster ID" = ClusterID, 
              "Cluster Name" = ClusterName, 
              #"Cluster Addition Date" = AdditionDate, 
              "DENI Number" = InstitutionReferenceNumber, 
              "Institutuion Name" = InstitutionNameMerged, 
              "Address Line 1" = AddressLine1Merged, 
              #"Address Line 2" = AddressLine2Merged,
              #"Address Line 3" = AddressLine3Merged, 
              "City" = CityLocations
            )
          
        })
        
        ## Render Locations Report Table
        output$locations_report_table <- DT::renderDataTable({
          locations_report_data()
        },
        filter = "top",
        server = FALSE,
        extensions = c('Buttons'),
        options = list(
          dom = 'lBftrip',
          pageLength = 10,
          scrollX = T,
          buttons = list(
            list(extend = 'csv', filename = "locations_report_data"),
            list(extend = 'excel', filename = "locations_report_data")),
          order = list(
            7,
            "desc"),
          columnDefs = list(
            list(visible = FALSE, targets = 0)))
        )
      })
    }
    
  # UI
    ## Locations Report Table UI Module
    locations_report_UI <- function(id){
      DT::dataTableOutput(NS(id, "locations_report_table"))
    }