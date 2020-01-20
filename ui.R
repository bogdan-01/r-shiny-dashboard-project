library(plotly)
library(shiny)
library(shinydashboard)
library(lubridate)
library(leaflet)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = tags$h4("Regional Performance Dashboard"),
                  titleWidth = 300),
  dashboardSidebar(width = 300,
                   
                   fluidRow(
                   ## Input Location
                   checkboxGroupInput(
                     inputId = "location",
                     label =  "Select Location",
                     choices = c("Edinburgh", 
                                 "Glasgow", 
                                 "Inverness"), 
                     selected = c("Edinburgh", "Glasgow", "Inverness")
                   ),
                   radioButtons(
                     inputId = "rolling_avg", 
                     label = tags$i("View a weekly rolling average?"), 
                     choices = c("No", "Yes"), 
                     selected = c("No")
                   ), 
                   ##Input Metric
                   selectInput(
                     inputId = "metrics",
                     label = "Select Metric",
                     choices = c("Sessions"                   = "total_sessions_per_day",
                                 "Users"                      = "total_users_per_day", 
                                 "Data Analysis Applications" = "total_da_apps",
                                 "Programming Applications"   = "total_prog_apps"
                     ), 
                     selected = "total_sessions_per_day"
                     
                   ),
                   dateRangeInput(
                     inputId = "date_range",
                     label = "Select date range",
                     start = "2019-01-01", 
                     end = today(),
                     format = "yyyy-mm-dd",
                     language = "en",
                     separator = "to",
                     autoclose = TRUE
                   )
                   ),
                   
                   fluidRow( 
                     
                     box(tags$img(src = "CodeClan_logo.png", height = 300, width = 300),
                         background = "light-blue", height = 300, width = 12
                         )
                     )
                   
  ),
  
  dashboardBody(
    ##top box for time series graph
    fluidRow(
      box(title = "Regional Performance Time Series", width = 12, background = "blue", height = 300, 
      plotlyOutput("time_series", height = "220px")
    )
    ),
    
    ## bottom box split for bar chart, targets and catchment map
    column(6,
      

      fluidRow( 

       box(title = "Time Series Totals",width = 12, height = 250, background = "light-blue", solidHeader = TRUE, 
      plotOutput("bar_graph", height = "175px"))),
      
      fluidRow(
        box(height = 300, width = 12,
            background = "olive", 
            title = fluidRow(column(6, "Monthly Application Targets"),
                             column(6, 
                                    selectInput(
                                      inputId = "month", 
                                      label   = "Select Month", 
                                      choices = c("January"  = 1, 
                                                  "February" = 2, 
                                                  "March"    = 3, 
                                                  "April"    = 4, 
                                                  "May"      = 5, 
                                                  "June"     = 6, 
                                                  "July"     = 7, 
                                                  "August"   = 8, 
                                                  "September"= 9, 
                                                  "October"  = 10, 
                                                  "November" = 11, 
                                                  "December" = 12), 
                                      selectize = FALSE)
                             )
            ),
            plotOutput("targets_graph", height = "175px")
      
    ))),
    
      column(6, 
        
      box(title = "Catchment map", width = 12, height = 500, background = "aqua", solidHeader = TRUE,
          leafletOutput("catchment_map", height = "420px"))            
             )
    
    
    )
  

)
