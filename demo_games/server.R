library(shiny)
library(tidyverse)
library(ggpubr)
library(zoo)
library(scales)

esports = read.csv("esports.csv")
top_teams = read.csv("top_ten_teams.csv")
top_players = read.csv("top_players.csv")
manualcolors<-c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown", 'darkblue')
# Define server
shinyServer(function(input, output) {
    
    # Genres
    output$genres1 <- renderPlot({
      esports %>%
        filter(Genre == input$genre) %>%
        ggplot(aes(x= Year, y = Earnings/1000000, 
                   fill = Game,
                   sort.val = "asc")) +
        geom_col(position = 'dodge') +
        scale_fill_manual(values = manualcolors) +
        labs(y = "Earnings (millions of dollars)")
    })
    
    output$genres2 <- renderPlot({
      esports %>%
        filter(Genre == input$genre) %>%
        ggplot(aes(Year, Avg_viewers/1000, 
                   fill = Game,
                   sort.val = "asc")) +
        geom_col(position = 'dodge') +
        scale_fill_manual(values = manualcolors) +
        labs(y = "Avg viewers (thousands of people)")
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
        ggplot(aes(Year_Month, Earnings/1000, color = 'red')) +
        geom_line() +
        geom_smooth() +
        labs(y = "Earnings (thousands)", x = "Year") +
        scale_y_continuous( trans= 'log10') +
        theme(legend.position="none")
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
        ggplot(aes(Year_Month, Avg_viewers/1000, color = 'red')) +
        geom_line() +
        geom_smooth() +
        labs(y = "Avg viewers (thousands)", x = "Year") +
        scale_y_continuous(trans = 'log10') +
        theme(legend.position="none")
        
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
        ggplot(aes(Year_Month, Peak_viewers/1000, color = 'red')) +
        geom_line() +
        geom_smooth() +
        labs(y = "Peak viewers (thousands)", x = "Year") +
        theme(legend.position="none")
    })
    
    # Games
    output$secondSelection <- renderUI({
      choice_second <- as.list(unique(esports$Game[which(esports$Genre == input$genre)]))
      selectInput(inputId = "game", choices = choice_second,
                  label = "Choose your game")
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
        ggplot(aes(Year_Month, Earnings/1000, color = 'red')) +
        geom_line() +
        geom_smooth() +
        labs(y = "Earnings (thousands)", x = "Year") +
        theme(legend.position="none")
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
        ggplot(aes(Year_Month, Avg_viewers/1000, color = 'red')) +
        geom_line() +
        geom_smooth() +
        labs(y = "Avg viewers (thousands)", x = "Year") +
        theme(legend.position="none")
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
        ggplot(aes(Year_Month, Peak_viewers/1000, color = 'red')) +
        geom_line() +
        geom_smooth() +
        labs(y = "Peak viewers (thousands)", x = "Year") +
        theme(legend.position="none")
    })
    
    # Teams
    output$thirdSelection <- renderUI({
      choice_third <- as.list(unique(top_teams$TeamName[which(top_teams$Genre == input$genre & top_teams$Game == input$game)]))
      selectInput(inputId = "team", choices = choice_third,
                  label = "Choose your team")
    })
    
    output$teams1 <- renderPlot({
      top_teams %>% 
        filter(Game == input$game) %>%
        ggplot(aes(TeamName, TotalUSDPrize/1000000, fill = TeamName)) +
        geom_col() +
        scale_fill_manual(values = manualcolors) +
        labs(y = "Total Prizes (Million dollars)", x = "Team Name") +
        theme(axis.text.x=element_text(angle=45, hjust=1))
      
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
    
    # Player
    output$fourthSelection <- renderUI({
      choice_fourth <- as.list(unique(top_players$CurrentHandle[which(top_players$Genre == input$genre & top_players$Game == input$game)]))
      selectInput(inputId = "player", choices = choice_fourth,
                  label = "Choose your player")
    })
    
    output$players1 <- renderPlot({
      top_players %>% 
        filter(Game == input$game) %>%
        ggplot(aes(CurrentHandle, TotalUSDPrize/1000000, fill = CurrentHandle)) +
        geom_col() +
        scale_fill_manual(values = manualcolors) +
        labs(y = "Total Prizes (Million dollars)", x = "Player") +
        theme(axis.text.x=element_text(angle=45, hjust=1))
      
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
        paste(Country), "Country", icon = icon("globe"),
        color = "purple"
      )
    })
    
})
