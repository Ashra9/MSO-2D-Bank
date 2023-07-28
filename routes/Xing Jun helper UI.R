

# UI function to display progress bars
stateofProgressUI <- function(session){
  fluidRow(
    uiOutput(session$ns("loanProgressBars"))  # Display the progress bars in a column of width 6
  )
}

# Server function for the progress tracker
serverProgressTracker <- function(input, output, session) {
  # Calculate the progress percentage and add it as a new column in loanData
  loanData$progress <- 100 * (loanData$loanmaturity / 5)
  
  # Sort loanData by progress percentage in descending order
  loanData <- loanData[order(-loanData$progress), ]
  
  output$loanProgressBars <- renderUI({
    progress_bars <- list()
    for (i in 1:nrow(loanData)) {
      loan_value <- loanData$loanValue[i]
      loan_title <- paste("Loan", loanData$loanID[i])
      progress <- loanData$progress[i]
      
      pb <- progressBar(
        id = paste0("loan_", i),  # Unique id for each progress bar
        value = progress
      )
      
      div_container <- div(
        span(loan_title, "-", loan_value, "-", sprintf("%.2f%%", progress)),
        pb,
        style = "margin-bottom: 10px;"
      )
      
      progress_bars[[i]] <- div_container
    }
    return(progress_bars)
  })
}

# Create the Shiny app
ui <- fluidPage(
  titlePanel("Loan Progress Tracker"),
  stateofProgressUI(session)
)

server <- function(input, output, session) {
  serverProgressTracker(input, output, session)
}

shinyApp(ui, server)
