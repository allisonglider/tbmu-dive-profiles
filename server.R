library(dplyr)
library(ggplot2)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, sesion) {
    
    # output$dateSelect <- renderUI({
    #     dateInput("date", "Select date to plot:", value = min(dat$Date[dat$Band == input$bird]),
    #               min = min(dat$Date[dat$Band == input$bird]),
    #               max = max(dat$Date[dat$Band == input$bird]))
    # })
    
    output$dateSelect <- renderUI({
        selectInput("date", "Select date to show:", 
                    choices = unique(dat$Date[dat$Band == input$bird]),
                    selected = min(dat$Date[dat$Band == input$bird]))
    })
    
    output$timeSelect <- renderUI({
        sliderInput("time", "Select time range:", 
                    value = c(as.POSIXct(paste(input$date, '00:00:00'), tz = 'UTC'), 
                              as.POSIXct(paste(input$date, '23:59:50'), tz = 'UTC')),
                    min = as.POSIXct(paste(input$date, '00:00:00'), tz = 'UTC'),
                    max = as.POSIXct(paste(input$date, '23:59:50'), tz = 'UTC'),
                    #min = tdr_data$min_time, max = tdr_data$max_time,
                    timeFormat = '%H:%M', timezone = "+0000")
    })
    
    output$divePlot <- renderPlot({
        plot_daily_dives(data = dat, 
                         bird = input$bird, date = input$date,
                         min_time = input$time[1], 
                         max_time = input$time[2])
    })
    
    output$diveProfile <- renderPlot({
        plot_dive_profile(data = dat, lag_time = 4, 
                          bird = input$bird, date = input$date,
                          min_time = input$time[1], 
                          max_time = input$time[2])
    })
    
    output$locationMap <- renderPlot({
        plot_location(data = locs, bird = input$bird, date = input$date,
                      base = coast)
    })
    
})
