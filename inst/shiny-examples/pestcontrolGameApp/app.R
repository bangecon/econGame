#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)
# Define UI for application
ui <- fluidPage(
  titlePanel("Crop Choice Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    textInput(
      inputId = "roleSheet",
      label = "Enter the ID of the Google Sheet with the list of participants.",
      value = NULL
    ),
    numericInput(
      inputId = "round",
      label = "Enter the round you want to calculate.",
      value = 3
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "payoff1",
      label = "Enter Anil's payoff for the IPC-IPC outcome.",
      value = 3
    ),
    numericInput(
      inputId = "payoff2",
      label = "Enter Anil's payoff for the Terminator-IPC outcome.",
      value = 4
    ),
    numericInput(
      inputId = "payoff3",
      label = "Enter Anil's payoff for the IPC-Terminator outcome.",
      value = 1
    ),
    numericInput(
      inputId = "payoff4",
      label = "Enter Anil's payoff for the Terminator-Terminator outcome.",
      value = 2
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Outcomes", tableOutput("payoffMatrix")),
      tabPanel("Plot", plotOutput("outcomePlot")),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    roleSheet <- input$roleSheet
    round <- input$round
    payoff <- c(input$payoff1, input$payoff2, input$payoff3, input$payoff4)
    g <- pestcontrolGame(resultsSheet = sheet, roleSheet = roleSheet, round = round, payoff = payoff)
    g
  })
  output$treePlot <- renderPlot({
    g <- data()
    g$tree
  }, width = 600)
  output$outcomePlot <- renderPlot({
    g <- data()
    plot(g,
         round = input$round)
  }, width = 600)
  output$payoffMatrix <- renderTable({
    g <- data()
    g$payoff
  }, rownames = TRUE, align = 'lcc',
  caption = "Student Payoff Matrix", caption.placement = "top")
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
