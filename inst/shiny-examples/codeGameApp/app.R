#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)
# Define UI for application
ui <- fluidPage(
  titlePanel("Code Coordination Game"),
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
      value = 1
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "payoffA1",
      label = "Enter Astrid's payoff for the (Java, Java) outcome.",
      value = 4
    ),
    numericInput(
      inputId = "payoffA2",
      label = "Enter Astrid's payoff for the (C++, Java) outcome.",
      value = 0
    ),
    numericInput(
      inputId = "payoffA3",
      label = "Enter Astrid's payoff for the (Java, C++) outcome.",
      value = 2
    ),
    numericInput(
      inputId = "payoffA4",
      label = "Enter Astrid's payoff for the (C++, C++) outcome.",
      value = 3
    ),
    numericInput(
      inputId = "payoffB1",
      label = "Enter Bettina's payoff for the (Java, Java) outcome.",
      value = 3
    ),
    numericInput(
      inputId = "payoffB2",
      label = "Enter Bettina's payoff for the (C++, Java) outcome.",
      value = 0
    ),
    numericInput(
      inputId = "payoffB3",
      label = "Enter Bettina's payoff for the (Java, C++) outcome.",
      value = 2
    ),
    numericInput(
      inputId = "payoffB4",
      label = "Enter Bettina's payoff for the (C++, C++) outcome.",
      value = 6
    )
  ),
  mainPanel(tabsetPanel(
    tabPanel("Outcomes", tableOutput("payoffMatrix")),
    tabPanel("Plot", plotOutput("outcomePlot")),
    tabPanel("Results", tableOutput("results")),
    tabPanel("Grades", tableOutput("grades"))
  ))
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    roleSheet <- input$roleSheet
    round <- input$round
    payoff.A <-
      c(input$payoffA1,
        input$payoffA2,
        input$payoffA3,
        input$payoffA4)
    payoff.B <-
      c(input$payoffB1,
        input$payoffB2,
        input$payoffB3,
        input$payoffB4)
    g <-
      codeGame(
        resultsSheet = sheet,
        roleSheet = roleSheet,
        round = round,
        payoff.A = payoff.A,
        payoff.B = payoff.B
      )
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
