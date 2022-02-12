#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

esports = read.csv("esports.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$genres <- renderPlot({
      esports %>%
        filter(Genre == input$genre) %>%
        ggplot(aes(Year, Earnings/1000000, fill = Game)) +
        geom_col(position = 'dodge')
    })

})
