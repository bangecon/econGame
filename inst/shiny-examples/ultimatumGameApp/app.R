#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
#
library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(gargle)
library(googledrive)
library(googlesheets4)
# Define UI for application
ui <- fluidPage(
  titlePanel("Ultimatum Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot", width = '600px', height = '600px')),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    sheet <- input$sheet
    g <- ultimatumGame(sheet)
    g
  })
   output$grades <- renderTable({
    g <- data()
    g$grades
  })
  output$results <- renderTable({
    g <- data()
    g$results[,4:length(g$results)]
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g, round = input$round)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
