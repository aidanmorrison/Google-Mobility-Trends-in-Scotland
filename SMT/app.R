#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)

indicators_lkp <- read.csv("/cloud/project/Data/Indicator Lookup.csv")
la_lkp <- read.csv("/cloud/project/Data/Scottish Local Authorities.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Google Mobility Trends Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("council_area", "Scottish Council Area",
                        choices = c(la_lkp$la, "Scotland"),
                        multiple = FALSE, selected = "Scotland"),
            selectInput("indicator", "Select Indicator",
                        choices = setNames(indicators_lkp$variable_name,
                                           indicators_lkp$formatted),
                        multiple = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # Get data
    gmr <- reactive({
        choices <- indicators_lkp$variable_name
        read.csv("/cloud/project/Data/Global Mobility Report - Scotland.csv") %>%
            filter(sub_region_1 == input$council_area) %>%
            select(sub_region_1, date, as.numeric(which(choices == input$indicator)+8)) %>%
            setNames(., c("LA", "date", "value")) %>%
            mutate(date = ymd(date))
    })

    output$Plot <- renderPlot({
        
        ggplot(gmr(), aes(x = date, y = value)) +
            geom_line(size = 0.5) +
            geom_area(fill = "darkgreen", alpha = 0.4) +
            theme_minimal() +
            labs(x = "Date", y = "Percent Change from Baseline",
                 title = paste(names(which(choices == input$indicator)),
                               "Mobility in", input$council_area))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
