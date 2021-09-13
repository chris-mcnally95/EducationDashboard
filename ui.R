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
            title = "Educational Insitution Counts within last 28 Days",
            p("Welcome to the Education Report Application"),
            hr()
          ),
          
          tabBox(
            width = 12,
            
            tabPanel(
              title = "",
              p("School cases within the last 28 days are listed and tallied below"),
              hr(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("opn_education_cases_table"))
            )
          ) 
        )
      )
    )
  )
)
