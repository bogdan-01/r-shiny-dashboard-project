source("data_wrangle.R")
library(shiny)
library(tidyverse)
library(ggthemes)
library(plotly)
library(forcats)
library(leaflet)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  filtered_data <- reactive({

    first_filter <- ga_tsibble %>%
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
      filter(scottish_city %in% input$location)
    
    if (input$metrics == "total_sessions_per_day") {
      
      first_filter <- first_filter %>%
      mutate( 
             weekly_rolling_avg = slide_dbl(total_sessions_per_day, mean, .size = 7)
             )
             
    }
    
    if (input$metrics == "total_users_per_day") {
      
      first_filter <- first_filter %>% 
        mutate(
          weekly_rolling_avg = slide_dbl(total_sessions_per_day, mean, .size = 7)
          )
        
    }
    
    if (input$metrics == "total_da_apps") {
      
      first_filter <- first_filter %>% 
        mutate(
          weekly_rolling_avg = slide_dbl(total_da_apps, mean, .size = 7)
        )
      
    }
    
    if (input$metrics == "total_prog_apps") {
      
      first_filter <- first_filter %>% 
        mutate(
          weekly_rolling_avg = slide_dbl(total_prog_apps, mean, .size = 7)
        )
    }
    
    return(first_filter)
      

      })


  output$time_series <-


    renderPlotly({
     
      if (input$rolling_avg == "No") {
      time_series <- filtered_data() %>% 
        ggplot() +
        aes_string(x = "date", y = input$metrics, colour = "scottish_city") + 
        geom_line() + 
        scale_color_codeclan(reverse = TRUE) + 
        theme_minimal()
      
      ggplotly(time_series)
      }
      
      else {
        #browser()
        time_series_ra <- filtered_data() %>%
          ggplot() + 
          aes_string(x = "date", y = "weekly_rolling_avg", colour = "scottish_city") +
          geom_line() + 
            scale_color_codeclan(reverse = TRUE) + 
            theme_minimal()
          
          ggplotly(time_series_ra)
      }
      
    })


  output$bar_graph <- renderPlot({
    this_will_work <- syms(input$metrics)

    filtered_data() %>%
      group_by(scottish_city) %>%
      summarise(total = sum(!!!this_will_work)) %>%
      mutate(scottish_city = fct_reorder(scottish_city, total)) %>%
      ggplot() +
      aes_string(x = "scottish_city", y = "total", fill = "scottish_city") +
      geom_col() +
      coord_flip() +
      labs(x = "City", y = case_when(
        input$metrics == "total_sessions_per_day" ~ "Sessions",
        input$metrics == "total_users_per_day" ~ "Users",
        input$metrics == "total_da_apps" ~ "Data Analysis Applications",
        input$metrics == "total_prog_apps" ~ "Programming Applications"
      ))  +
      theme_minimal() + 
      theme(legend.position = "none") +
      scale_fill_codeclan()
  })


output$targets_graph <- renderPlot({


  
  targets_plot <- ga_tsibble %>%
    mutate(month = month(date)) %>%
    index_by(month) %>%
    summarise(monthly_metric = sum(get(input$metrics))) %>%
    filter(month == input$month) %>%
    ggplot() +
    aes(x = input$month, y = monthly_metric, fill = scottish_city) +
    geom_col(position = "dodge") +
    theme_minimal() +
    coord_flip() +
    scale_fill_codeclan(reverse = TRUE)
  
  if (input$metrics %in% c("total_da_apps", "total_prog_apps")) {
    targets_plot + 
      geom_hline(yintercept = 25, colour = "red", linetype = "dotted", size = 3) +
      geom_hline(yintercept = 10, colour = "blue", linetype = "dotted", size = 3) 
  }
  
  else {
    targets_plot
  }
  
})



output$catchment_map <- renderLeaflet({
  
  leaflet() %>% 
    addTiles() %>%
    addRectangles(
      lng1=-2.3150, lat1=55.4060,
      lng2=-3.7393, lat2=56.6450,
      fillColor = "transparent",
      color = "#e7c27a") %>%
    addRectangles(
      lng1=-3.7393, lat1=54.6141,
      lng2=-5.2901, lat2=56.6450,
      fillColor = "transparent",
      color = "#1a3644") %>%
    addRectangles(
      lng1=-0.2994, lat1=56.6450,
      lng2=-9.1667, lat2=60.8532,
      fillColor = "transparent",
      color = "#5dbcd2")
  })


})
   
