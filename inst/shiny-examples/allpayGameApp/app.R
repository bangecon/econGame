#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("All-Pay Auction Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "endowment",
      label = "Enter the endowment each player receives at the start of the game.",
      value = 5
    ),
    numericInput(
      inputId = "prize",
      label = "Enter the ID of the Google Sheet with the output.",
      value = 4
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Winner", tableOutput("winner")),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    endowment <- input$endowment
    prize <- input$prize
    g <- allpayGame(sheet, endowment, prize)
    g
  })
  output$winner <- renderTable({
    g <- data()
    g$winner
  })
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
