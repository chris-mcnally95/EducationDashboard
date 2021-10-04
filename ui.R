######## UI ######## 

# Spinner 
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

ui <- dashboardPage(
  
  dashboardHeader(title = "Education Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "open_education_cases", icon = icon("home")),
      #menuItem("Weekly Report", tabName = "weekly_report", icon = icon("calendar-check")),
      menuItem("Schools Overview", tabName = "school_cases_table", icon = icon("table")),
      menuItem("School Report", tabName = "school_report", icon = icon("chart-bar")), 
      menuItem("Primary Schools", tabName = "primary_schools", icon = icon("school")),
      menuItem("Locations Report", tabName = "locations_report", icon = icon("columns")),
      menuItem("Methodology", tabName = "methodology", icon = icon("microscope")),
      menuItem("Change Log", tabName = "change_log", icon = icon("list"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      #--------------HOME--------------
      tabItem(
        tabName = "open_education_cases",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Welcome to the Education Dasboard Application",
            p("This application aims to capture children of school going age and is based on successfully 
              completed cases which report association with DENI registered schools or colleges from the 30/08/2021."), 
            p("There is an approximate 24 hour delay between a case being made and it appearing within the 
              Synpase data frame used in the making of this application."), 
            p("Each case shown in this application has been selected as it is a confirmed case with a known association to a school or college."),
            p(strong("Please Note: This application under development and will undergo frequent updates and changes."))
          )
        ),
        
        fluidRow(
          withSpinner(infoBoxOutput("total_cases", width = 6), type = 2, color.background = "#ecf0f5"),
          infoBoxOutput("total_groups", width = 6)
        ),
        
        fluidRow(
          infoBoxOutput("cases_last_week", width = 6),
          infoBoxOutput("groups_last_week", width = 6)
        ),
        
        fluidRow(
          infoBoxOutput("cases_this_week", width = 6),
          infoBoxOutput("groups_this_week", width = 6)
        )
        
      ),
      
      #--------------WEEKLY REPORT--------------
      tabItem(
        tabName = "weekly_report",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Education Institutions Weekly Report",
            p("A break down of cases reported from schools within the last seven days since schools reopened on 30th August 2021"),
            ),
          
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Cumulative Cases Breakdown Over 7 Days",
            shinycssloaders::withSpinner(
            DT::dataTableOutput("weekly_report_table"))
          ) 
        )
      ),
    
      #--------------SCHOOLS OVERVIEW--------------
      tabItem(
        tabName = "school_cases_table",
        
        fluidRow(
          box(
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
              DT::dataTableOutput("education_cases_table"))
          ) 
        )
      ),
      
      #--------------SCHOOL REPORT--------------
      tabItem(
        tabName = "school_report", 
        
        fluidRow(
          # Insert DENI Number
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Insert DENI Number",
            p("Please enter the DENI Number of the School you would like to investigate in the box below"),
            textInput(inputId = "input_school_id", label ="", value = "")
          ),
          
          # Key Info
          box(
            title = "Key Info",
            status = "primary",
            solidHeader = TRUE,
            p("Name:", strong(textOutput("schoolName", inline = TRUE))),
            p("DENI Number:", strong(textOutput("schoolID", inline = TRUE))),
            p("School Type:", strong(textOutput("schoolType", inline = TRUE))),
            p("Total Number of Pupils:", strong(textOutput("TotalPupils", inline = TRUE))),
            p("7 Day Attack Rate:", strong(textOutput("schoolAR7", inline = TRUE), "%")),
            p("14 Day Attack Rate:", strong(textOutput("schoolAR14", inline = TRUE), "%")),
            p("28 Day Attack Rate:", strong(textOutput("schoolAR28", inline = TRUE), "%")),
            p("Town Area:", strong(textOutput("Area", inline = TRUE))),
            p("Post Code:", strong(textOutput("PostCode", inline = TRUE))),
            p("LGD:", strong(textOutput("LGD", inline = TRUE))),
          ), 
          
          infoBoxOutput("totalCases", width = 6),
          infoBoxOutput("totalContacts", width = 6)
          
        ),
        
        fluidRow(  
          # Epicurve
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Epicurve by Sample Date",
            p("Selected school cases dates of sample are shown below."),
            hr(),
            shinycssloaders::withSpinner(
              plotlyOutput("epicurve_plot"))
          ),
          
          # Cases by Year
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Cases by School Year",
            p("The graph below shows the frequncies of cases by school year group of the selected school"),
            hr(),
            shinycssloaders::withSpinner(
              plotlyOutput("school_year_plot", height = NULL))
          ),
          
          # Attack Rate by Year
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Attack Rate by School Year",
            p("The graph below shows the 28 Day attack rate by school year group of the selected school"),
            hr(),
            shinycssloaders::withSpinner(
              plotlyOutput("attack_rate_plot", height = NULL)
            )
          ),
          
          # Case Line List
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "School Cases Table",
            shinycssloaders::withSpinner(
              DT::dataTableOutput("school_cases_table"))
          ),
          
          # Contact Line List
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "School Case Close Contacts Table",
            shinycssloaders::withSpinner(
              DT::dataTableOutput("school_contacts_table"))
          )
        )
      ),
      
      #--------------PRIMARY SCHOOLS------------------
      tabItem(
        tabName = "primary_schools",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Primary Schools Stats",
            p("Primary schools are listed below with stats around total pupils, cases in the last 28 days, an esimated attack rate for 
                the previous 28 days, along with a breakdown for cases, attack rates and pupils in each year group."),
            hr(),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("primary_schools_table"))
          ) 
        )
      ),
      
      #--------------LOCATIONS REPORT------------------
      tabItem(
        tabName = "locations_report",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Date Picker for Locations Report",
            p("Select a date range below to look at schools cases for."),
            p("The CreatedOn date from the Locations table on MSD/Synapse is used for date reference."),
            dateRangeInput(
              "locations_report_daterange",
              "Date Range:",
              start = Sys.Date()-2,
              end = Sys.Date(),
              helpText("Select a period of time to look at schools cases for.")))
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Locations Report Table",
            p("Admin staff look for cases associted with schools and want to see if these cases have Cluster IDs associated with them or not."),
            p("If a schools cases doesn't have a Cluster ID, it either needs linked on MSD to an existing cluster or a cluster for that school doesn't exist yet, and needs created, and then the case linked."),
            hr(),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("locations_report_table"))
          )
        )
      ),
      
      #--------------METHODOLOGY------------------
      tabItem(
        tabName = "methodology",
        
        fluidRow(
          box(
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
            p("School cases are only included in this data set if they have been contacted successfully by CTC."),
            p("All school cases and contacts are those recorded after the 30th of August 2021."),
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
      ),
      
      #--------------CHANGE LOG--------------
      tabItem(
        tabName = "change_log",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Application Change Log",
            p(strong("29-09-21")),
            p("Hyperlink added to Schools Overview Tab"),
            p("Earliest Sample and Latest Sample added to Schools Overview Tab"),
            p("Added Case Change variable to Schools Overview Tab which is the 0-3 day cases sum minus 4-6 day cases sum."),
            p(strong("24-09-21")),
            p("Locations Report tab added"),
            p("Methodology tab added"),
            p("Minor Report tab fixes"),
            p(strong("23-09-21")),
            p("School Report Tab: School Case Close Contacts line list sections added."),
            p(strong("22-09-21")),
            p("School Report Tab: Key Info Box, Epicurve, Attack Rate By Year and Line List sections added."),
            p("Attack rate per year graph added."),
            p("Duplicate cases removed"),
            p(strong("20-09-21")),
            p("Home tab finalised with filter added to each column."),
            p("Moved School Cases Table to a separate tab."),
            p("School years assigned to each case by date of birth."),
            p("School frequency chart added to tab.")
          )
        )
      )
    )
  )
)
