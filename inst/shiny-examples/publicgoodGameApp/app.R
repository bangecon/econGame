#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
#
library(econGame)

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
    ),
    textInput(
      inputId = "return",
      label = "Enter the group return on contributions.",
      value = 0.1
    ),
    actionButton("go", "Load New Responses")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Results", tableOutput("blindedResults")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    endowment <- input$endowment
    return <- input$return
    g <- publicgoodGame(sheet = sheet, endowment = endowment, return = return)
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
    p <- plot(g)
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
