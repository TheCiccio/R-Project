library(shiny)
library(tidyverse)
library(ggpubr)
library(zoo)

esports = read.csv("esports.csv")
top_teams = read.csv("top_ten_teams.csv")
top_players = read.csv("top_players.csv")
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
    
    output$secondSelection <- renderUI({
      choice_second <- as.list(unique(esports$Game[which(esports$Genre == input$genre)]))
      selectInput(inputId = "game", choices = choice_second,
                  label = "Choose the game")
    })
    
    output$games1 <- renderPlot({
      grouped_df <- esports %>% 
        filter(Game == input$game) %>% 
        group_by(Game, Year, Month) %>% 
        summarise(monthly_earnings = sum(Earnings))
      
      grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")
      
      cleaned_df <- grouped_df %>% 
        ungroup() %>%
        select(Game, Earnings = monthly_earnings, Year_Month)
      
      cleaned_df %>%
        ggplot(aes(Year_Month, log10(Earnings), color = 'red')) +
        geom_line() +
        geom_smooth()
    })
    
    output$games2 <- renderPlot({
      grouped_df <- esports %>% 
        filter(Game == input$game) %>% 
        group_by(Game, Year, Month) %>% 
        summarise(monthly_avg_viewers = sum(Avg_viewers))
      
      grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")
      
      cleaned_df <- grouped_df %>% 
        ungroup() %>%
        select(Game, Avg_viewers = monthly_avg_viewers, Year_Month)
      
      cleaned_df %>%
        ggplot(aes(Year_Month, log10(Avg_viewers), color = 'red')) +
        geom_line() +
        geom_smooth()
    })
    
    output$games3 <- renderPlot({
      grouped_df <- esports %>% 
        filter(Game == input$game) %>% 
        group_by(Game, Year, Month) %>% 
        summarise(monthly_peak = sum(Peak_viewers))
      
      grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")
      
      cleaned_df <- grouped_df %>% 
        ungroup() %>%
        select(Game, Peak_viewers = monthly_peak, Year_Month)
      
      cleaned_df %>%
        ggplot(aes(Year_Month, log10(Peak_viewers), color = 'red')) +
        geom_line() +
        geom_smooth()
    })
    
    output$thirdSelection <- renderUI({
      choice_third <- as.list(unique(top_teams$TeamName[which(top_teams$Genre == input$genre & top_teams$Game == input$game)]))
      selectInput(inputId = "team", choices = choice_third,
                  label = "Choose the team")
    })
    
    output$teams1 <- renderPlot({
      top_teams %>% 
        filter(Game == input$game) %>%
        ggplot(aes(TeamName, TotalUSDPrize/100000, fill = TeamName)) +
        geom_col()
      
    })
    
    output$earningBox <- renderValueBox({
      earning <- top_teams %>% filter(TeamName == input$team & Game == input$game) %>% select(TotalUSDPrize)
      valueBox(
        paste0(round(earning/1000000, 2), "$"), "Million Dollars", icon = icon("credit-card"),
        color = "purple"
      )
    })
    
    output$tournamentBox <- renderValueBox({
      tournament <- top_teams %>% filter(TeamName == input$team & Game == input$game) %>% select(TotalTournaments)
      valueBox(
        paste0(tournament), "Tournaments", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "purple"
      )
    })
    
    output$fourthSelection <- renderUI({
      choice_fourth <- as.list(unique(top_players$CurrentHandle[which(top_players$Genre == input$genre & top_players$Game == input$game)]))
      selectInput(inputId = "player", choices = choice_fourth,
                  label = "Choose the player")
    })
    
    output$players1 <- renderPlot({
      top_players %>% 
        filter(Game == input$game) %>%
        ggplot(aes(CurrentHandle, TotalUSDPrize/100000, fill = CurrentHandle)) +
        geom_col()
      
    })
    
    output$earningBox2 <- renderValueBox({
      earning <- top_players %>% filter(CurrentHandle == input$player & Game == input$game) %>% select(TotalUSDPrize)
      valueBox(
        paste0(round(earning/1000000, 2), "$"), "Million Dollars", icon = icon("credit-card"),
        color = "purple"
      )
    })
    
    output$countryBox2 <- renderValueBox({
      Country <- top_players %>% filter(CurrentHandle == input$player & Game == input$game) %>% select(CountryCode)
      valueBox(
        paste(Country), "Country", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "purple"
      )
    })
    
})
