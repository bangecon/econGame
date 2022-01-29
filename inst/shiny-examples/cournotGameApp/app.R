#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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
  titlePanel("Stackelberg Duopoly Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = '1umW2zv2s7UaNMTe6POgEabCqXXXQxnLDY2TJrG_6TuQ'
    ),
    numericInput(
      inputId = "round",
      label = "Enter the round you want to calculate.",
      value = 1
    ),
    numericInput(
      inputId = "a",
      label = "Enter the intercept of the inverse demand function.",
      value = 10
    ),
    numericInput(
      inputId = "b",
      label = "Enter the slope of the inverse demand function.",
      value = -1
    ),
    numericInput(
      inputId = "c",
      label = "Enter the marginal cost of the output.",
      value = 6
    ),
    numericInput(
      inputId = "f",
      label = "Enter the fixed cost of the output.",
      value = 0
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Payoffs",
               tableOutput("payoff"),
               tableOutput("output"),
               tableOutput("price")),
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    sheet <- input$sheet
    round <- input$round
    a <- input$a
    b <- input$b
    c <- input$c
    f <- input$f
    g <- cournotGame(sheet, a = a, b = b, c = c, f = f)
    g
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g,
         round = input$round)
  }, width = 600)
  output$payoff <- renderTable({
    g <- data()
    g$payoff
  }, rownames = TRUE, align = 'lcc',
  caption = "Student Payoff Matrix", caption.placement = "top")
  output$output <- renderTable({
    g <- data()
    g$output
  }, rownames = TRUE, align = 'lcc',
  caption = "Quantity Outcome Matrix", caption.placement = "top")
  output$price <- renderTable({
    g <- data()
    g$price
  }, rownames = TRUE, align = 'lcc',
  caption = "Price Outcome Matrix", caption.placement = "top")
  output$results <- renderTable({
    g <- data()
    subset(g$results, Round = round)
  }, align = 'c')
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
}

# Run the application
shinyApp(ui = ui, server = server)
