#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Anchoring Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    actionButton("go", "Load New Responses")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput("table")),
      tabPanel("Plot", plotOutput("plot", width = '600px', height = '600px')),
      tabPanel("Results", tableOutput("results"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    g <- anchoringGame(sheet)
    g$gtable <- as_gt(g$gtable)
    g
  })
  output$results <- renderTable({
    g <- data()
    g$results
  })
  output$table <- render_gt({
    g <- data()
    g$gtable
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g, round = input$round)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
