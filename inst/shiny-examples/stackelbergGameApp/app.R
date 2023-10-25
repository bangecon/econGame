#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Stackelberg Duopoly Game"),
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
      tabPanel("Outcomes",
               tabsetPanel(
                 tabPanel("Tables",
                          tableOutput("payoffMatrix"),
                          tableOutput("outputMatrix"),
                          tableOutput("priceMatrix")),
                 tabPanel("Tree", plotOutput("treePlot"))
               )),
      tabPanel("Plot", plotOutput("outcomePlot")),
      tabPanel("Results",
               tabsetPanel(
                 tabPanel("Leaders", tableOutput("leaderResults")),
                 tabPanel("Followers", tableOutput("followerResults")),
                 tabPanel("All", tableOutput("results"))
               )),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    round <- input$round
    a <- input$a
    b <- input$b
    c <- input$c
    f <- input$f
    g <- stackelbergGame(sheet, a = a, b = b, c = c, f = f)
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
  output$outputMatrix <- renderTable({
    g <- data()
    g$output
  }, rownames = TRUE, align = 'lcc',
  caption = "Quantity Outcome Matrix", caption.placement = "top")
  output$priceMatrix <- renderTable({
    g <- data()
    g$price
  }, rownames = TRUE, align = 'lcc',
  caption = "Price Outcome Matrix", caption.placement = "top")
  output$results <- renderTable({
    g <- data()
    subset(g$results, Round = round)
  }, align = 'c')
  output$leaderResults <- renderTable({
    g <- data()
    subset(g$leaderResults, Round = round)
  }, align = 'c')
  output$followerResults <- renderTable({
    g <- data()
    subset(g$followerResults, Round = round)
  }, align = 'c')
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
}

# Run the application
shinyApp(ui = ui, server = server)
