#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Load libraries ####
library(shiny)
library(lubridate)
library(DBI)
library(dbplyr)

#### Load custom functions ####
source("scripts/databaseOperations.R")

#### Connect to database ####
con = connectToLichthausDB(pwd = readLines(con = "secrets.txt", warn = F)[2])

#### Define UI for application ####
ui <- fluidPage(
  
  titlePanel("Filmabend verarbeiten"),
  
  # Heutiger Film
  fluidRow(
    h4("Film"),
    # Datum und Titel
    column(
      width = 2,
      dateInput(inputId = "in_movieDate", label = "Datum Filmabend"),
      textInput(inputId = "in_movieToday", label = "Heutiger Film"),
      textOutput(outputId = "out_movieToday")
    ),
  ),
  fluidRow(
    # Bewertungen
    h4("Bewertungen"),
    column(
      width = 1,
      numericInput(inputId = "rating_Ben", label = "Ben", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      numericInput(inputId = "rating_Cem", label = "Cem", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      numericInput(inputId = "rating_Jan", label = "Jan", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      numericInput(inputId = "rating_Patrick", label = "Patrick", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      numericInput(inputId = "rating_Stefan", label = "Stefan", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      numericInput(inputId = "rating_Timo", label = "Timo", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      numericInput(inputId = "rating_Toni", label = "Toni", min = 1, max = 10, value = NA, step = 1)
    ),
    column(
      width = 1,
      actionButton(inputId = "btn_rating", label = "Bewertungen hochladen")
    ),
  ),
  # Vorschläge
  fluidRow(
    h4("Vorschläge"),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Ben_1", label = "Ben"),
      textOutput(outputId = "out_vorschlag_Ben_1"),
      textInput(inputId = "in_vorschlag_Ben_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Ben_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Cem_1", label = "Cem"),
      textOutput(outputId = "out_vorschlag_Cem_1"),
      textInput(inputId = "in_vorschlag_Cem_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Cem_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Jan_1", label = "Jan"),
      textOutput(outputId = "out_vorschlag_Jan_1"),
      textInput(inputId = "in_vorschlag_Jan_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Jan_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Patrick_1", label = "Patrick"),
      textOutput(outputId = "out_vorschlag_Patrick_1"),
      textInput(inputId = "in_vorschlag_Patrick_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Patrick_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Stefan_1", label = "Stefan"),
      textOutput(outputId = "out_vorschlag_Stefan_1"),
      textInput(inputId = "in_vorschlag_Stefan_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Stefan_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Timo_1", label = "Timo"),
      textOutput(outputId = "out_vorschlag_Timo_1"),
      textInput(inputId = "in_vorschlag_Timo_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Timo_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_vorschlag_Toni_1", label = "Toni"),
      textOutput(outputId = "out_vorschlag_Toni_1"),
      textInput(inputId = "in_vorschlag_Toni_2", label = NULL),
      textOutput(outputId = "out_vorschlag_Toni_2"),
    ),
    column(
      width = 1,
      textInput(inputId = "in_murmeled", label = "Gemurmelete Vorschläge", placeholder = "ids durch Komma trennen"),
    ),
    column(
      width = 1,
      actionButton(inputId = "btn_vorschlag", label = "Vorschläge hochladen")
    ),
  ),
  # Abstimmung
  fluidRow(
    h4("Abstimmung"),
      column(
        width = 1,
        textInput(inputId = "in_voting_Ben", label = "Ben"),
        textOutput(outputId = "out_voting_Ben"),
      ),
      column(
        width = 1,
        textInput(inputId = "in_voting_Cem", label = "Cem"),
        textOutput(outputId = "out_voting_Cem"),
      ),
      column(
        width = 1,
        textInput(inputId = "in_voting_Jan", label = "Jan"),
        textOutput(outputId = "out_voting_Jan"),
      ),
      column(
        width = 1,
        textInput(inputId = "in_voting_Patrick", label = "Patrick"),
        textOutput(outputId = "out_voting_Patrick"),
      ),
      column(
        width = 1,
        textInput(inputId = "in_voting_Stefan", label = "Stefan"),
        textOutput(outputId = "out_voting_Stefan"),
      ),
      column(
        width = 1,
        textInput(inputId = "in_voting_Timo", label = "Timo"),
        textOutput(outputId = "out_voting_Timo"),
      ),
      column(
        width = 1,
        textInput(inputId = "in_voting_Toni", label = "Toni"),
        textOutput(outputId = "out_voting_Toni"),
      ),
    column(
      width = 1,
      numericInput(inputId = "in_wahldurchgang", label = "Wahldurchgang", value = 1, min = 1, step = 1),
    ),
    column(
      width = 1,
      actionButton(inputId = "btn_voting", label = "Wahldurchgang hochladen")
    ),
    
  )
)

#### Define server logic required to draw a histogram ####
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
