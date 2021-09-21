######## UI ######## 

ui <- dashboardPage(
  
  dashboardHeader(title = "Education Report"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "open_education_cases", icon = icon("home")),
      menuItem("School Graph", tabName = "school_year_graph", icon = icon("chart-bar"))
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
            title = "Welcome to the Education Report Application",
            p("This application has been built to aid the CTC Data Management Team / Surveillance Team montior, manage and report on cases associated with nursery, primary, secondary, grammar, prep, special and further education institutions within Northern Ireland in 2021")
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
        ),
        
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
            title = "Cases by School Year Graph",
            p("Please enter the DENI Number of the School you would like to investigate in the box below"),
            textInput(inputId = "input_school_id", label ="", value = "")
          )
        ),
        
        tabBox(
          width = 12,
          tabPanel(
            title = "Cases by School Year",
            p("The graph below shows the frequncies of cases by school year group"),
            hr(),
            shinycssloaders::withSpinner(
              plotlyOutput("school_year_table", height = NULL)
            )  
          )
        )
      )
    )
  )
)
