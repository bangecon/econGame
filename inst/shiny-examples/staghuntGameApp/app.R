#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Stag Hunt Game"),
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
      label = "Enter the payoff for the Rabbit-Rabbit outcome.",
      value = 0.5
    ),
    numericInput(
      inputId = "payoff2",
      label = "Enter the payoff for the Stag-Rabbit outcome.",
      value = -0.5
    ),
    numericInput(
      inputId = "payoff3",
      label = "Enter the payoff for the Rabbit-Stag outcome.",
      value = 1
    ),
    numericInput(
      inputId = "payoff4",
      label = "Enter the payoff for the Stag-Stag outcome.",
      value = 2
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Payoffs", tableOutput("payoff")),
      tabPanel("Plot", plotOutput("plot")),
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
    g <- staghuntGame(sheet, payoff)
    g
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g,
         round = input$round)
  })
  output$payoff <- renderTable({
    g <- data()
    g$payoff
  }, rownames = TRUE, align = 'lcc')
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
