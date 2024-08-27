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
  mainPanel(tableOutput("groups"), style = "font-size:20px; ")
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
    sheet <- input$sheet
    size <- input$size
    seed <- input$seed
    roleLabs = c(input$role1, input$role2)
    g <- randomRoles(
      sheet = sheet,
      size = size,
      seed = seed,
      roleLabs = roleLabs
    )
    colnames(g$long) <- c("Round", "First Name", "Last Name", "Role")
    g
  })
  output$groups <- renderTable( {
    g <- data()
    g$long[, -1]
  }, rownames = FALSE, align = 'c',
  caption = "Student Roles", caption.placement = "top")
}

shinyApp(ui = ui, server = server)
