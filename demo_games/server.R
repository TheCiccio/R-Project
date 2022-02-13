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
    
    output$genres3 <- renderPlot({
      grouped_df <- esports %>% 
        filter(Genre == input$genre) %>% 
        group_by(Genre, Year, Month) %>% 
        summarise(monthly_earnings = sum(Earnings))
      
      grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")
      
      cleaned_df <- grouped_df %>% 
        ungroup() %>%
        select(Genre, Earnings = monthly_earnings, Year_Month)
      
      cleaned_df %>%
        ggplot(aes(Year_Month, log10(Earnings), color = 'red')) +
        geom_line() +
        geom_smooth()
    })
    
    output$genres4 <- renderPlot({
      grouped_df <- esports %>% 
        filter(Genre == input$genre) %>% 
        group_by(Genre, Year, Month) %>% 
        summarise(monthly_avg_viewers = sum(Avg_viewers))
      
      grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")
      
      cleaned_df <- grouped_df %>% 
        ungroup() %>%
        select(Genre, Avg_viewers = monthly_avg_viewers, Year_Month)
      
      cleaned_df %>%
        ggplot(aes(Year_Month, log10(Avg_viewers), color = 'red')) +
        geom_line() +
        geom_smooth()
    })
    
    output$genres5 <- renderPlot({
      grouped_df <- esports %>% 
        filter(Genre == input$genre) %>% 
        group_by(Genre, Year, Month) %>% 
        summarise(monthly_peak = sum(Peak_viewers))
      
      grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")
      
      cleaned_df <- grouped_df %>% 
        ungroup() %>%
        select(Genre, Peak_viewers = monthly_peak, Year_Month)
      
      cleaned_df %>%
        ggplot(aes(Year_Month, log10(Peak_viewers), color = 'red')) +
        geom_line() +
        geom_smooth()
    })
    
})
