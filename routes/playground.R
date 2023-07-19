source("../usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","bs4Dash", "shinyauthr", "sodium", "shinyBS", "plotly", "shinyWidgets", "zeallot", "ggalt", "fresh", "ggdark", "RMySQL")
loadPkgs(pkgnames)

# Define the UI for a module
counterUI <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}

# Define the server logic for a module
counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}
# Use the module in an app
ui <- fluidPage(
  counterUI("counter1", "Counter #1"),
  counterUI("counter2", "Counter #2")
)
server <- function(input, output, session) {
  counterServer("counter1")
  counterServer("counter2")
}

shinyApp(ui, server)
