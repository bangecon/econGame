library(econGame)

ui <- fluidPage(
  titlePanel("Random Groups"),
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
    hr(),
    a("Created by Jim Bang", href='https://github.com/bangecon'),
    a("St. Ambrose University", href='https://www.sau.edu/')
  ),
  mainPanel(textOutput("groups"), style = "font-size:20px; ")
)

server <- function(input, output) {
  output$groups <- renderTable( {
    econGames::randomGroups(
      sheet = input$sheet, size = input$size, seed = input$seed)
  })
}

shinyApp(ui = ui, server = server)
