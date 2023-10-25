#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Multi-Player Prisoner's Dilemma Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "round",
      label = "Enter the round you want to calculate.",
      value = 1
    ),
    numericInput(
      inputId = "payoff1",
      label = "Enter the payoff for the compete-compete outcome.",
      value = 0
    ),
    numericInput(
      inputId = "payoff2",
      label = "Enter the payoff for the collude-compete outcome.",
      value = 0.5
    ),
    numericInput(
      inputId = "payoff3",
      label = "Enter the payoff for the compete-collude outcome.",
      value = 3
    ),
    numericInput(
      inputId = "payoff4",
      label = "Enter the payoff for the collude-collude outcome.",
      value = 1
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Equilibrium", tableOutput("equilibrium")),
      tabPanel("Payoffs", tableOutput("payoff")),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    payoff <- c(input$payoff1, input$payoff2, input$payoff3, input$payoff4)
    g <- multipdGame(sheet, payoff)
    g
  })
  output$equilibrium <- renderTable({
    g <- data()
    g$equilibria$Equilibrium[input$round]
  }, colnames = FALSE)
  output$payoff <- renderTable({
    g <- data()
    g$payoff
  }, rownames = TRUE)
  output$results <- renderTable({
    g <- data()
    g$results
  })
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
}

# Run the application
shinyApp(ui = ui, server = server)
