library(shiny)
library(tidyr)
library(stringr)
library(googlesheets4)
ui <- fluidPage(
  titlePanel("Random Presenter"),
  sidebarPanel(
    numericInput(
      inputId = "seed",
      label = "Random Seed",
      value = as.integer(gsub("-", "", Sys.Date()))
    ),
    numericInput(
      inputId = "size",
      label = "Group Size",
      value = 2
    ),
    textInput(
      inputId = "sheet",
      label = "Enter the sheet ID of the roster",
      value = '1i_hJiSk-TOfqtNOtm7ZUUcJe1gEZSPwowEd5VZlHFNo'
    ),
    hr(),
    a("Created by Jim Bang", href='https://github.com/bangecon'),
    a("St. Ambrose University", href='https://www.sau.edu/')
  ),
  mainPanel(textOutput("groups"), style = "font-size:20px; ")
)

server <- function(input, output) {
  output$groups <- renderTable( {
    marketGames::randomGroups(
      sheet = input$sheet, size = input$size, seed = input$seed)
  })
}

shinyApp(ui = ui, server = server)
