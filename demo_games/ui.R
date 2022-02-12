#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

esports = read.csv("esports.csv")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  h1("Choose your Champion"),

    # Application title
    titlePanel("Choose your Genre"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "genre",
                         label = "Genres",
                         choices = unique(esports$Genre)),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("genres")
        )
    )
))
