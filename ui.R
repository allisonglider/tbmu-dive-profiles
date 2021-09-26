library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("TBMU Dive Profiles"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h6('Use this app to explore the dive behaviour of thick-billed murres originating 
               from a colony at Coats Island, Nunavut, through winter of 2018 and 2019.'),
            
            selectInput("date", "Select date to show:",  choices = unique(dat$Date), 
                        selected = '2018-01-01', multiple = FALSE
            ),
            
            # selectInput(inputId = 'bird', label = 'Select bird to show:', choices = unique(dat$Band), 
            #             selected = '118600758', multiple = FALSE
            # ),
            
            uiOutput('birdSelect'),
            
            #h6('Select date to plot'),
            # uiOutput('dateSelect'),
            
            h6('Use the slider to zoom in to a particular time range'),
            uiOutput('timeSelect'),
            
            h4('Environmental conditions'),
            
            plotOutput('moon_nao'),
            
            h6(''),
            
            h6('\n Created by Allison Patterson @allisonglider'),
            
        ),

        mainPanel(
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), 
                            plotOutput('divePlot'),
                            plotOutput('locationMap'))
            ), 
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), 
                            plotOutput('diveProfile'),
                            plotOutput('tempProfile'))
            )
            # plotOutput('diveProfile')
        )
       
    )
))
