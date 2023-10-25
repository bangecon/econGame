#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Market Equilibrium Game"),
  sidebarPanel(
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "round",
      label = "Enter the round you want to display.",
      value = 1
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Schedule", tableOutput("schedule")),
      tabPanel("Plot", plotOutput("plot", width = '600px', height = '600px')),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    g <- equilibriumGame(sheet)
    g
  })
  output$schedule <- renderTable({
    g <- data()
    g$schedule[[input$round]]
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g, round = input$round)
  })
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
  output$results <- renderTable({
    g <- data()
    subset(g$results, Round == input$round)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
