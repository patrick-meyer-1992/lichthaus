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
library(DT)
library(dplyr)
library(stringr)

devMode = F
testTable = tibble(
  "rows" = c("Bewertung", "Vorschlag", "Vorschlag", "Abstimmung"),
  "Ben" = c("", "", "", ""),
  "Cem" = c("6", "tt2229511", "tt2229511", "tt2229511"),
  "Daniel" = c("8", "tt0120812", "tt2229511", "tt2229511"),
  "Jan" = c("5", "tt2210479", "tt12593682", "tt12593682"),
  "Patrick" = c("6", "tt5649108", "tt6644200", "tt0120812"),
  "Stefan" = c("5", "tt0116996", "tt0094761", "tt12593682"),
  "Timo" = c("8", "tt0092644", "tt1396484", "tt2229511"),
  "Toni" = c("4", "tt4479380", "tt1734493", "tt4479380"),
)

testMurmel = tibble(
  murmeled = c("tt2229511", "tt12593682", "tt4479380", "tt0120812")
)

#### Load custom functions ####
source("databaseOperations.R")

#### Connect to database ####
con = connectToLichthausDB(pwd = readLines(con = "../../secrets.txt", warn = F)[2])

teilnehmer = c("Ben", "Cem", "Daniel", "Jan", "Patrick", "Stefan", "Timo", "Toni")
film = tbl(con, "film") %>% select(!upload_time)

#### Define UI for application ####
ui <- fluidPage(
  titlePanel("Filmabend verarbeiten"),
  
    fluidRow(
      column(
        width = 2,
        dateInput(inputId = "in_movieDate", label = "Datum Filmabend"),
        textInput(inputId = "in_movieToday", label = "Heutiger Film"),
        textOutput(outputId = "out_movieToday"),
        
      ),
      
      column(
        width = 2,
        numericInput(inputId = "in_numMurmeled", label = "Anzahl Murmelbahngewinner", 
                     value = 4, min = 1, step = 1),
        textInput(inputId = "in_winner", label = "Heutiger Gesamtsieger"),
        textOutput(outputId = "out_winner"),
      ),
      column(
        width = 2,
        numericInput(inputId = "in_numVotings", label = "Anzahl Wahldurchgänge", 
                     value = 1, min = 1, step = 1),
        textInput(inputId = "in_host", label = "Heutiger Host")
      )
    ),
  
  fluidRow(
    column(
      width = 6,
      DTOutput("my_datatable"),
    ),
    column(
      width = 3,
      DTOutput("murmelbahn_id"),
    )
  ),

  fluidRow(
    column(
      width = 6,
      DTOutput("titleTable"),
    ),
    column(
      width = 3,
      DTOutput("murmelbahn_title"),
    )
  ),
  fluidRow(
    column(
      width = 12,
      actionButton(inputId = "btn_confirm", label = "Bestätigen")
    )
  )
)

#### Define server logic required to draw a histogram ####
server <- function(input, output) {
  
  #initialize a blank dataframe
  idTable <- reactiveValues(data = {
    if(devMode){
      testTable
    }else{
      rows = c("Bewertung", "Vorschlag", "Vorschlag", rep("Abstimmung", 1))
      df = tibble(rows)
      df[teilnehmer] = ""
      df
    }
  })
  
  # Initialize a blank dataframe for the surivors of the murmelbahn (only ids)
  murmelTable_id <- reactiveValues(data = { 
    if(devMode){
      testMurmel
    }else{
      df = tibble(murmeled = character(0))
      df
    }
  })
  
  titleTable <- reactiveValues(data = { 
    rows = c("Bewertung", "Vorschlag", "Vorschlag", rep("Abstimmung", 1))
    df = tibble(rows)
    df[teilnehmer] = ""
    df
  })
  
  # Initialize a blank dataframe for the surivors of the murmelbahn (with titles)
  murmelTable_title <- reactiveValues(data = { 
    df = tibble(murmeled = character(0))
    df
  })
  
  # Update the dataframe if the number of votings changes
  observeEvent(input$in_numVotings, ignoreInit = devMode, {
    rows = c("Bewertung", "Vorschlag", "Vorschlag", rep("Abstimmung", input$in_numVotings))
    df = tibble(rows)
    df[teilnehmer] = ""
    idTable$data = df
    titleTable$data = df
  })
  
  # Update the dataframe if the number of murmel survivors changes
  observeEvent(input$in_numMurmeled, ignoreInit = devMode,{
    df = tibble(murmeled = character(0)) %>% 
      add_row(murmeled = rep("", input$in_numMurmeled))
    murmelTable_id$data = df
    murmelTable_title$data = df
  })
  

  output$my_datatable <- renderDT({
    DT::datatable(idTable$data, 
                  editable = TRUE, 
                  options = list(dom = 't',
                                 autoWidth = F,
                                 columnDefs = list(
                                   (
                                     list(
                                       width = "200px", 
                                       targets = "_all"
                                     )
                                   )
                                 )
                  ),
                  rownames = FALSE
    )
  })
  
  output$titleTable <- renderDT({
    DT::datatable(titleTable$data, 
                  editable = FALSE, 
                  options = list(dom = 't',
                                 autoWidth = F,
                                 columnDefs = list(
                                   (
                                     list(
                                       width = "200px", 
                                       targets = "_all"
                                     )
                                   )
                                 )
                  ),
                  rownames = FALSE
    )
  })
  
  
  output$murmelbahn_id <- renderDT({
    DT::datatable(murmelTable_id$data, 
                  editable = TRUE, 
                  options = list(dom = 't',
                                 autoWidth = F,
                                 columnDefs = list(
                                   list(
                                     width = '1px',
                                     targets = c(0)
                                   ),
                                   list(
                                     width = "200px", 
                                     targets = c(1)
                                   )
                                 )
                  ),
    )
  })
  
  output$murmelbahn_title <- renderDT({
    DT::datatable(murmelTable_title$data, 
                  editable = FALSE, 
                  options = list(dom = 't',
                                 autoWidth = F,
                                 columnDefs = list(
                                     list(
                                       width = '1px',
                                       targets = c(0)
                                     ),
                                     list(
                                       width = "200px", 
                                       targets = c(1)
                                     )
                                 )
                  ),
    )
  })
  
  output$out_movieToday <- renderText({
    ID = input$in_movieToday
    as.character(film %>% filter(id == ID) %>% select(titel) %>% pull())
  })
  
  output$out_winner <- renderText({
    ID = input$in_winner
    as.character(film %>% filter(id == ID) %>% select(titel) %>% pull())
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, { # For some reason this variable needs the suffix _cell_edit
    #get values
    info = input$my_datatable_cell_edit
    i = as.character(info$row)
    j = names(idTable$data[info$col + 1])
    k = str_remove_all(as.character(info$value), " ")
    #write values to reactive
    idTable$data[i,j] <- k
    title = film %>% filter(id == k) %>% select(titel) %>% pull()
    title = if(length(title) == 0) "" else title
    titleTable$data[i,j] <- title
  })
  
  observeEvent(input$murmelbahn_id_cell_edit, { # For some reason this variable needs the suffix _cell_edit
    #get values
    info = input$murmelbahn_id_cell_edit
    i = as.character(info$row)
    j = "murmeled"
    k = str_remove_all(as.character(info$value), " ")
    #write values to reactive
    murmelTable_id$data[i,j] <- k
    title = film %>% filter(id == k) %>% select(titel) %>% pull()
    title = if(length(title) == 0) "" else title
    murmelTable_title$data[i,j] <- title
  })
  
  observeEvent(input$btn_confirm, {
    
    print("The following table was uploaded to film:")
    print(update_film(con, idTable$data))
    print("The following table was uploaded to schlaegt_vor:")
    print(update_schlaegt_vor(con, idTable$data, murmelTable_id$data, input$in_movieDate))
    print("The following table was uploaded to stimmt_fuer:")
    print(update_stimmt_fuer(con, idTable$data, input$in_winner, input$in_movieDate))
    print("The following table was uploaded to bewertet:")
    print(update_bewertet(con, idTable$data, input$in_movieToday))
    print("The following table was uploaded to filmabend:")
    print(update_filmabend(con, input$in_movieToday, input$in_host, input$in_movieDate))
    print("Upload finished")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
