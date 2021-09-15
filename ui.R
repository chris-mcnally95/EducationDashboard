######## SETUP ######## 

# Libraries
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(plotly)

######## SPINNER ########
options(spinner.color = "#0275D8", spinner.color.background="#ffffff", spinner.size =2)

######## UI ######## 

ui <- dashboardPage(
  
  dashboardHeader(title = "Education Report"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "open_education_cases", icon = icon("home"))
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
      )
    )
  )
)
