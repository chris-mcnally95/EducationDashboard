#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Your application UI logic 
  shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(title = "Education Dashboard"),
    
    shinydashboard::dashboardSidebar(
      shinydashboard:: sidebarMenu(
        shinydashboard::menuItem("Home", tabName = "open_education_cases", icon = icon("home")),
        #shinydashboard::menuItem("Weekly Report", tabName = "weekly_report", icon = icon("calendar-check")),
        shinydashboard::menuItem("Schools Overview", tabName = "school_cases_table", icon = icon("table")),
        shinydashboard::menuItem("School Report", tabName = "school_report", icon = icon("chart-bar")), 
        #shinydashboard::menuItem("Primary Schools", tabName = "primary_schools", icon = icon("school")),
        shinydashboard:: menuItem("Locations Report", tabName = "locations_report", icon = icon("columns")),
        shinydashboard:: menuItem("Early Warning Report", tabName = "ewr_helper", icon = icon("bell")),
        shinydashboard:: menuItem("Methodology", tabName = "methodology", icon = icon("microscope"))#,
        #shinydashboard:: menuItem("Change Log", tabName = "change_log", icon = icon("list"))
      )
    ),
    
    shinydashboard::dashboardBody(
      
      shinydashboard::tabItems(
        
        #--------------HOME--------------
        
        mod_home_infoboxes_ui("home_infoboxes_ui_1"),

        #--------------SCHOOLS OVERVIEW--------------

        mod_sch_overview_ui("sch_overview_ui_1"),

        #--------------SCHOOL REPORT--------------
        shinydashboard::tabItem(
          tabName = "school_report", 
          
          shiny::fluidRow(
            # Insert DENI Number
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Insert DENI Number",
              shiny::p("Please enter the DENI Number of the School you would like to investigate in the box below"),
              shiny::textInput(inputId = "input_school_id", label ="", value = "")
            ),
            
            # Key Info
            shinydashboard::box(
              title = "School Info",
              status = "primary",
              solidHeader = TRUE,
              p("Name:", strong(textOutput("schoolName", inline = TRUE))),
              p("DENI Number:", strong(textOutput("schoolID", inline = TRUE))),
              p("School Type:", strong(textOutput("schoolType", inline = TRUE))),
              p("Total Number of Pupils:", strong(textOutput("TotalPupils", inline = TRUE))),
              p("Town Area:", strong(textOutput("Area", inline = TRUE))),
              p("Post Code:", strong(textOutput("PostCode", inline = TRUE))),
              p("LGD:", strong(textOutput("LGD", inline = TRUE))), width = 4
            ), 
            
            shinydashboard::box(
              title = "Key Statistics",
              status = "primary",
              solidHeader = TRUE,
              p("7 Day Attack Rate:", strong(textOutput("schoolAR7", inline = TRUE), "%")),
              p("14 Day Attack Rate:", strong(textOutput("schoolAR14", inline = TRUE), "%")),
              p("28 Day Attack Rate:", strong(textOutput("schoolAR28", inline = TRUE), "%")),
              p("7 Day Cumulative case rate/100k for LGD:", strong(textOutput("CCR7DayLGD", inline = TRUE)), "   NI rate:", strong(textOutput("NI7dayrate", inline = TRUE))),
              p("14 Day Cumulative case rate/100k for LGD:", strong(textOutput("CCR14DayLGD", inline = TRUE)), "   NI rate:", strong(textOutput("NI14dayrate", inline = TRUE))),
              p("28 Day Cumulative case rate/100k for LGD:", strong(textOutput("CCR28DayLGD", inline = TRUE)), "   NI rate:", strong(textOutput("NI28dayrate", inline = TRUE))),
              p(strong("Please note:"), "These values are based on successfully contacted cases"),
              width = 4
            ), 
            
            shinydashboard::infoBoxOutput("totalCases", width = 4),
            shinydashboard::infoBoxOutput("totalContacts", width = 4)
          ),
          
          shiny::fluidRow(  
            # Epicurve
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Epicurve by Sample Date",
              p("Selected school cases dates of sample are shown below."),
              hr(),
              shinycssloaders::withSpinner(
                mod_sch_report_epi_ui("sch_report_epi_ui_1"))
            ),
            
            # Cases by Year
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Cases by School Year",
              p("The graph below shows the frequncies of cases by school year group of the selected school"),
              hr(),
              shinycssloaders::withSpinner(
                mod_sch_report_year_ui("sch_report_year_ui_1"))
            ),
            
            # Attack Rate by Year
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Attack Rate by School Year",
              p("The graph below shows the 28 Day attack rate by school year group of the selected school"),
              hr(),
              shinycssloaders::withSpinner(
                mod_sch_report_ar_ui("sch_report_ar_ui_1")
              )
            ),
            
            # Case Line List
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "School Cases Table",
              shinycssloaders::withSpinner(
                mod_sch_report_cases_ui("sch_report_cases_ui_1")
              )
            ),
            
            # Contact Line List
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "School Case Close Contacts Table",
              shinycssloaders::withSpinner(
                mod_sch_report_contacts_ui("sch_report_contacts_ui_1")
              )
            )
          )
        ),
        
        #--------------PRIMARY SCHOOLS------------------
        # shinydashboard::tabItem(
        #   tabName = "primary_schools",
        #   
        #   shiny::fluidRow(
        #     shinydashboard::box(
        #       width = 12,
        #       status = "primary",
        #       solidHeader = TRUE,
        #       title = "Primary Schools Stats",
        #       p("Primary schools are listed below with stats around total pupils, cases in the last 28 days, an esimated attack rate for 
        #         the previous 28 days, along with a breakdown for cases, attack rates and pupils in each year group."),
        #       hr(),
        #       shinycssloaders::withSpinner(
        #         mod_primary_sch_ui("primary_sch_ui_1")
        #       )
        #     ) 
        #   )
        # ),
        
        #--------------LOCATIONS REPORT------------------
        shinydashboard::tabItem(
          tabName = "locations_report",
          
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Date Picker for Locations Report",
              p("Select a date range below to look at schools cases for."),
              p("The CreatedOn date from the Locations table on MSD/Synapse is used for date reference."),
              shiny::dateRangeInput(
                "locations_report_daterange",
                "Date Range:",
                start = Sys.Date()-2,
                end = Sys.Date(),
                helpText("Select a period of time to look at schools cases for.")))
          ),
          
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Locations Report Table",
              p("Admin staff look for cases associted with schools and want to see if these cases have Cluster IDs associated with them or not."),
              p("If a schools cases doesn't have a Cluster ID, it either needs linked on MSD to an existing cluster or a cluster for that school doesn't exist yet, and needs created, and then the case linked."),
              hr(),
              shinycssloaders::withSpinner(
                mod_locations_ui("locations_ui_1")
              )
            )
          )
        ),
        
        #--------------EARLY WARNING REPORT--------------
        
        mod_ewr_helper_ui("ewr_helper_ui_1"),
        
        #--------------METHODOLOGY------------------
        shinydashboard::tabItem(
          tabName = "methodology",
          
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Notes and Data Limitations",
              
              h5(strong("Data Sources")),
              p("Data for schools has been taken from:"),
              tags$ul(
                tags$li(
                  a(
                    href="https://www.eani.org.uk/admissions-guides/transfer-between-schools/ages", 
                    "Education Authority - Qualifying Ages - DOB by Year Group 2021/2022 School Year.")),
                tags$li(
                  a(
                    href="http://apps.education-ni.gov.uk/appinstitutes/default.aspx", 
                    "Department of Education - Institution Search")),
                tags$li(
                  a(
                    href="https://www.education-ni.gov.uk/publications/school-enrolment-school-level-date-202021", 
                    "Department of Education - School Enrollments"))),
              hr(),
              
              h5(strong("Stats / Variables")),
              p(strong("28 Days Cases :"), "This is the number of cases associated with a school that have had a Date of Sample 
              within the previous 28 days, excluding any cases from today."),
              p(strong("14 Days Cases :"), "This is the number of cases associated with a school that have had a Date of Sample 
              within the previous 14 days, excluding any cases from today."),
              p(strong("7 Days Cases :"), "This is the number of cases associated with a school that have had a Date of Sample 
              within the previous 7 days, excluding any cases from today."),
              p(strong("28 Day Attack Rate :"), "This is the number of cases associated with a school that have had a Date of Sample 
              within the previous 28 days, divded by the sum of total number of pupils enrolled in this academic year and number of 
              staff cases within the last 28 days."),
              p(strong("14 Day Attack Rate :"), "This is the number of cases associated with a school that have had a Date of Sample 
              within the previous 14 days, divded by the sum of total number of pupils enrolled in this academic year and number of 
              staff cases within the last 14 days."),
              p(strong("7 Day Attack Rate :"), "This is the number of cases associated with a school that have had a Date of Sample 
              within the previous 7 days, divded by the sum of total number of pupils enrolled in this academic year and number of 
              staff cases within the last 7 days."),
              hr(),
              
              h5(strong("Completeness")),
              p("Cases are only included in this data set if they have been contacted successfully by CTC."),
              p("All cases and contacts are those recorded after the 30th of August 2021."),
              p("On average, 20% of all positive cases reported to CTC do not answer the phone."), 
              p("Also, any cases reported to CTC via Digital Self Trace tend to be incomplete and may not associate a school aged case with a school."), 
              p("Therefore, it is likely that the school cases reported within this application are an underestimate of the true total."),
              hr(),
              
              h5(strong("Timeliness")),
              p("At least a 24 hour delay needs to be allowed for when using this application and so, it is likely 
              the school principal will have more accurate and up-to-date figures for the number of cases in their school."),
              p("This is because the positive case results first have to come to CTC and be uploaded onto MSD. The contact tracers then call the cases 
              but this can take several call attempts."),
              p("The information they collect is then recorded on MSD which then has to mirror across onto Synapse and then subsequently R Connect."),
              p("The processing of data collected via Digital Self Trace can also add further delays, as many times this data is incomplete and will need to be manually cleaned first."),
              hr(),
              
              h5(strong("Accuracy")),
              p("As with all CTC data, information on school cases is self-reported either by school cases themselves or their parents, which can 
              involve inaccurate information."),
              p("Also, this application uses age as a proxy for what year the school case is in, as the tracers do not ask this. 
              And any case associated with a school and aged 18+, is considered a school staff member. This may not always be the case."),
              hr()
            )
          )
        )
      )
    )
  )
}



