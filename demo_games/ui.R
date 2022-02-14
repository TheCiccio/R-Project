library(shiny)
library(shinydashboard)
library(zoo)
library(scales)

esports = read.csv("esports.csv")
top_teams = read.csv("top_ten_teams.csv")
top_players = read.csv("top_players.csv")
# Define UI for application that draws a histogram
dashboardPage(skin = "purple",
  dashboardHeader(title = "Choose your Champion", titleWidth = 250),
  
  # Sidebar contents
  dashboardSidebar(width = 250,
    sidebarMenu(
      menuItem("Genre", tabName = "Genre", icon = icon("eye")),
      menuItem("Game", tabName = "Game", icon = icon("play")),
      menuItem("Team", tabName = "Team", icon = icon("thumbs-up")),
      menuItem("Player", tabName = "Player", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Genre tab content
      tabItem(tabName = "Genre",
              fluidRow(
                box(
                  title = 'Choose your Genre', status = "warning", solidHeader = TRUE,
                  selectizeInput(inputId = "genre",
                                 label = "Genres",
                                 choices = unique(esports$Genre))

                ),
                box(
                  title = "Years", status = "warning", solidHeader = TRUE,
                  sliderInput("slider", "Select a range of years:", 2016, 2021, 2021),
                ),
              ),
              fluidRow(
                box(
                  title = "Earnings", background = "yellow", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres1", height = 300)
                ),
                box(
                  title = "Viewers", background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres2", height = 300)
                ),
              ),
              fluidRow(
                box(
                  title = "Rate of earnings per month", width = 4, background = "yellow", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres3", height = 300)
                ),
                box(
                  title = "Rate of average viewers per month", width = 4, background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres4", height = 300)
                ),
                box(
                  title = "Peak of viewers per month", width = 4, background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres5", height = 300)
                )
              ),
      ),
      # Game tab content
      tabItem(tabName = "Game",
              fluidRow(
                box(
                  title = 'Choose your Game', status = "warning", solidHeader = TRUE,
                  uiOutput("secondSelection")
                ),
                box(
                  title = "Years", status = "warning", solidHeader = TRUE,
                  sliderInput("slider", "Select a range of years:", 2016, 2021, 2021),
                ),
              ),

              fluidRow(
                box(
                  title = "Rate of earnings per month", width = 4, background = "yellow", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("games1", height = 300)
                ),
                box(
                  title = "Rate of average viewers per month", width = 4, background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("games2", height = 300)
                ),
                box(
                  title = "Peak of viewers per month", width = 4, background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("games3", height = 300)
                )
              ),
      ),
      
      # Team tab content
      tabItem(tabName = "Team",
              fluidRow(
                box(
                  title = 'Choose your Team', status = "warning", solidHeader = TRUE,
                  uiOutput("thirdSelection")
                ),
                valueBoxOutput("earningBox")
              ),
              fluidRow(
                box(
                  title = "Best ten teams", background = "yellow", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("teams1", height = 300)
                ),
                valueBoxOutput("tournamentBox")
                )
      ),
      
      #Players tab content
      tabItem(tabName = "Player",
              fluidRow(
                box(
                  title = 'Choose your Player', status = "warning", solidHeader = TRUE,
                  uiOutput("fourthSelection")
                ),
                valueBoxOutput("earningBox2")
              ),
              fluidRow(
                box(
                  title = "Best ten players", background = "yellow", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("players1", height = 300)
                ),
                valueBoxOutput("countryBox2")
              )
      )
    )
  )
)