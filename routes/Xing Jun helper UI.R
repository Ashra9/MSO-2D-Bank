

# UI function to display progress bars
stateofProgressUI <- function(session){
  fluidRow(
    uiOutput(session$ns("loanProgressBars"))  # Display the progress bars in a column of width 6
  )
}

# Server function for the progress tracker
serverProgressTracker <- function(input, output, loanData) {
  # Calculate the progress percentage and add it as a new column in loanData
  silly <- loanData
  silly$progress <- 100 * (silly$loanmaturity / 5)
  
  # Sort loanData by progress percentage in descending order
  silly <- silly[order(-silly$progress), ]
  
  output$loanProgressBars <- renderUI({
    progress_bars <- list()
    for (i in 1:nrow(silly)) {
      loan_value <- silly$loanValue[i]
      loan_title <- paste("Loan", silly$loanID[i])
      progress <- silly$progress[i]
      
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

