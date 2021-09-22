  ######## UI ######## 

ui <- dashboardPage(
  
  dashboardHeader(title = "Education Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "open_education_cases", icon = icon("home")),
      menuItem("Primary Schools", tabName = "primary_schools", icon = icon("school")),
      menuItem("School Cases", tabName = "school_cases_table", icon = icon("table")),
      menuItem("School Report", tabName = "school_report", icon = icon("chart-bar")),
      menuItem("Change Log", tabName = "ChangeLog", icon = icon("list"))
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
              completed cases which report association with DENI registered schools or colleges"), 
            p("There is an approximate 24 hour delay between a case being made and it appearing within the 
              Synpase data frame used in the making of this application."), 
            p("Each case shown in this application has been selected as it is a confirmed case with a known association to a school or college."),
            p(strong("Please Note: This application under development and will undergo frequent updates and changes"))
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
      
      #--------------SCHOOL CASES TABLE--------------
      tabItem(
        tabName = "school_cases_table",
        
        fluidRow(
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title = "Education Institution Frequencies",
          p("Schools and their associate cases are grouped and tallied below. Please note that NAs are omitted."),
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
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Insert DENI Number",
            p("Please enter the DENI Number of the School you would like to investigate in the box below"),
            textInput(inputId = "input_school_id", label ="", value = "")
          ),
          
          box(
            title = "Key Info",
            status = "primary",
            solidHeader = TRUE,
            p("Name:", strong(textOutput("schoolName", inline = TRUE))),
            p("DENI Number:", strong(textOutput("schoolID", inline = TRUE))),
            p("School Type:", strong(textOutput("schoolType", inline = TRUE))),
            p("Town Area:", strong(textOutput("Area", inline = TRUE))),
            p("Post Code:", strong(textOutput("PostCode", inline = TRUE)))
          ),  
          
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "School Cases Table",
            p("Selected school cases are shown below."),
            hr(),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("school_cases_table"))
          ),   
        
         box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title = "Cases by School Year",
          p("The graph below shows the frequncies of cases by school year group of the selected school"),
          hr(),
          shinycssloaders::withSpinner(
              plotlyOutput("school_year_plot", height = NULL)
          )
        )
      )
    ),
      
      #--------------CHANGE LOG--------------
      tabItem(
        tabName = "ChangeLog",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Application Change Log",
            p(strong("22-09-21")),
            p("Grammar and Preps Schools merged into Secondary and Primary Schools Respectively.
              School Report Tab Key Info Box and Line List added"),
            p(strong("20-09-21")),
            p("Home tab finalised with filter added to each column.
              Moved School Cases Table to a separate tab.
              School years assigned to each case by date of birth.  
              School frequency chart added to tab.")
          )
        )
      )
    )
  )
)
