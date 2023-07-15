source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","bs4Dash", "shinyauthr", "sodium", "shinyBS", "plotly", "shinyWidgets", "zeallot", "ggalt", "fresh", "ggdark", "RMySQL")
loadPkgs(pkgnames)


#feature Modules
source("routes/AppInterfaceTesting.R")

#Helper Functions
source("routes/Test Helper Functions.R")

ui <- routerModuleUI("router")

server <- function(input, output) {
  routerModuleServer("router")
}

shinyApp(ui = ui, server = server)