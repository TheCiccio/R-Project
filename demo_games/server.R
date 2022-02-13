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
library(ggpubr)

esports = read.csv("esports.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$genres1 <- renderPlot({
      esports %>%
        filter(Genre == input$genre) %>%
        ggplot(aes(Year, Earnings/1000000, 
                   fill = Game,
                   sort.val = "asc")) +
        geom_col(position = 'dodge')
    })
    
    output$genres2 <- renderPlot({
      esports %>%
        filter(Genre == input$genre) %>%
        ggplot(aes(Year, Avg_viewers, 
                   fill = Game,
                   sort.val = "asc")) +
        geom_col(position = 'dodge')
    })
    
    output$secondSelection <- renderUI({
      choice_second <- as.list(unique(esports$Game[which(esports$Genre == input$genre)]))
      selectInput(inputId = "game", choices = choice_second,
                  label = "Choose the game")
    })

})
