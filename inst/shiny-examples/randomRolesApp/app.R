library(econGame)

ui <- fluidPage(
  titlePanel("Random Roles"),
  sidebarPanel(
    numericInput(
      inputId = "seed",
      label = "Random Seed",
      value = 8675309
    ),
    numericInput(
      inputId = "size",
      label = "Group Size",
      value = 2
    ),
    textInput(
      inputId = "sheet",
      label = "Enter the sheet ID of the roster",
      value = NULL
    ),
    textInput(
      inputId = "role1",
      label = "Enter the name of the first role.",
      value = "Leader"
    ),
    textInput(
      inputId = "role2",
      label = "Enter the name of the second role.",
      value = "Follower"
    ),
    actionButton("go", "Load New Responses"),
    hr(),
    a("Created by Jim Bang", href='https://github.com/bangecon'),
    a("St. Ambrose University", href='https://www.sau.edu/')
  ),
  mainPanel(textOutput("groups"), style = "font-size:20px; ")
)

server <- function(input, output) {
  output$groups <- renderTable( {
    econGame::randomRoles(
      sheet = input$sheet, size = input$size, seed = input$seed, roleLabs = c(input$role1, input$role2))
  })
}

shinyApp(ui = ui, server = server)
