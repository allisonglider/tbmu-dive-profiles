library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("TBMU Dive Profiles"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            #h6('Select bird to plot'),
            
            selectInput("date", "Select date to show:",  choices = unique(dat$Date), 
                        selected = '2018-01-01', multiple = FALSE
            ),
            
            # selectInput(inputId = 'bird', label = 'Select bird to show:', choices = unique(dat$Band), 
            #             selected = '118600758', multiple = FALSE
            # ),
            
            uiOutput('birdSelect'),
            
            #h6('Select date to plot'),
            # uiOutput('dateSelect'),
            
            h6('Use the time slider to zoom into  dive bouts'),
            uiOutput('timeSelect'),
            
            h4('Environmental conditions'),
            
            plotOutput('moon_nao')
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
                            plotOutput('lightProfile'))
            )
            # plotOutput('diveProfile')
        )
       
    )
))
