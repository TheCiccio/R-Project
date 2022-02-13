library(shiny)
library(shinydashboard)

esports = read.csv("esports.csv")
# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "E-Sports"),
  
  # Sidebar contents
  dashboardSidebar(
    sidebarMenu(
      menuItem("Genre", tabName = "Genre", icon = icon("dashboard")),
      menuItem("Game", tabName = "Game", icon = icon("th")),
      menuItem("Team", tabName = "Team", icon = icon("th")),
      menuItem("Player", tabName = "Player", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # Genre tab content
      tabItem(tabName = "Genre",
              fluidRow(
                box(
                  title = 'Choose your Genre', status = "primary", solidHeader = TRUE,
                  selectizeInput(inputId = "genre",
                                 label = "Genres",
                                 choices = unique(esports$Genre),
                                 selected = unique(esports$Genre))

                ),
                box(
                  title = "Years", status = "warning", solidHeader = TRUE,
                  sliderInput("slider", "Select a range of years:", 2016, 2021, 2021),
                ),
              ),
              fluidRow(
                box(
                  title = "Earnings", background = "red", solidHeader = TRUE,
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
                  title = "Rate of earnings per month", background = "red", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres3", height = 300)
                ),
                box(
                  title = "Rate of average viewers per month", background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres4", height = 300)
                ),
                box(
                  title = "Peak of viewers per month", background = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("genres5", height = 300)
                )
              ),
      ),
      # Game tab content
      tabItem(tabName = "Game",
              h2("Widgets tab content")
      ),
      
      # Team tab content
      tabItem(tabName = "Team",
              h2("Team tab content")
      ),
      
      #Players tab content
      tabItem(tabName = "Player",
              h2("Team tab content")
      )
      )
  )
)
