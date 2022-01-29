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
  titlePanel("Public Good Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    textInput(
      inputId = "endowment",
      label = "Enter the initial endowment that students receive.",
      value = 0
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot", width = '600px', height = '600px')),
      tabPanel("Results", tableOutput("blindedResults")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    sheet <- input$sheet
    g <- publicgoodGame(sheet)
    g
  })
   output$grades <- renderTable({
    g <- data()
    g$grades
  })
  output$blindedResults <- renderTable({
    g <- data()
    g$blindedResults
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
