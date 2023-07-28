stateofProgressUI <- function(session){
  fluidRow(
    column(6, uiOutput(session$ns("loanProgressBars")))  # Display the progress bars in a column of width 6
  )
}
#server function for the progress tracker


serverProgressTracker <- function(input, output, loanData) {
  output$loanProgressBars <- renderUI({
    progress_bars <- list()
    for (i in 1:nrow(loanData)) {
      loan_value <- loanData$loanValue[i]
      loan_title <- paste("Loan", loanData$loanID[i])
      loan_duration <- loanData$loanmaturity[i]
      progress <- 100 * (loan_duration / 5)  # Calculate the progress percentage
      
      pb <- progressBar(
        id = paste0("loan_", i),  # Unique id for each progress bar
        value = progress
      )
      
      div_container <- div(
        span(loan_title, "-", loan_value),
        pb,
        style = "margin-bottom: 10px;"
      )
      
      progress_bars[[i]] <- div_container
    }
    return(progress_bars)
  })
}