  ######## UI ######## 

ui <- dashboardPage(
  
  dashboardHeader(title = "Education Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "open_education_cases", icon = icon("home")),
      menuItem("Primary Schools", tabName = "primary_schools", icon = icon("school")),
      menuItem("School Cases", tabName = "school_cases_table", icon = icon("table")),
      menuItem("School Graph", tabName = "school_year_graph", icon = icon("chart-bar")),
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
          downloadButton("DownloadHomeReport", "Download Report"),
          hr(),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("education_cases_table"))
        ) 
      )
    ),
      
      #--------------SCHOOL YEAR GRAPH--------------
      tabItem(
        tabName = "school_year_graph",
        
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
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title = "Cases by School Year",
          p("The graph below shows the frequncies of cases by school year group"),
          hr(),
          shinycssloaders::withSpinner(
              plotlyOutput("school_year_table", height = NULL)
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
