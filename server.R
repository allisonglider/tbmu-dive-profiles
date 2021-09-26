library(dplyr)
library(ggplot2)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, sesion) {
    
    rv <- reactiveValues(prev_date = '2018-01-01')
    
    observeEvent(input$bird, {
        # If event occurs, then run the following append function
        rv$prev_bins <- as.Date(ifelse(is.null(input$date), 
                               '2018-01-01', 
                               input$date) , tz = 'UTC')
    })
    
    output$dateSelect <- renderUI({

        dd <- unique(dat$Date[dat$Band == input$bird])
        if (rv$prev_bins %in% dd) {
            selected_date <- rv$prev_bins
            } else{
            selected_date <- min(dd)
        }
        
        selectInput("date", "Select date to show:",
                    choices = dd, selected = selected_date)
    })
    
    output$timeSelect <- renderUI({
        dd <- ifelse(is.null(input$date), '2018-01-01', input$date)
        sliderInput("time", "Select time range:", 
                    value = c(as.POSIXct(paste(dd, '00:00:10'), tz = 'UTC'), 
                              as.POSIXct(paste(dd, '23:59:50'), tz = 'UTC')), 
                    min = as.POSIXct(paste(dd, '00:00:10'), tz = 'UTC'),
                    max = as.POSIXct(paste(dd, '23:59:50'), tz = 'UTC'),
                    timeFormat = '%H:%M', timezone = "+0000")
    })
    
    output$divePlot <- renderPlot({
        if (!is.null(input$date) & !is.null(input$time)) {
            
        plot_daily_dives(data = dat, 
                         bird = input$bird, date = input$date,
                         min_time = input$time[1], 
                         max_time = input$time[2])
        }
    })
    
    output$diveProfile <- renderPlot({
        
        if (!is.null(input$date) & !is.null(input$time)) {
            plot_dive_profile(data = dat, lag_time = 4, 
                                    bird = input$bird, date = input$date,
                                    min_time = input$time[1], 
                                    max_time = input$time[2])
        }
    })
    
    output$tempProfile <- renderPlot({
        if (!is.null(input$date) & !is.null(input$time)) {
        plot_temp_raster(data = dat,
                          bird = input$bird, date = input$date,
                          min_time = input$time[1], 
                          max_time = input$time[2])
        }
    })
    
    output$locationMap <- renderPlot({
        plot_location(data = locs, bird = input$bird, date = input$date,
                      base = coast)
    })
    
    output$moon_nao <- renderPlot({
        plot_moon(date = input$date)
    })
    
})
