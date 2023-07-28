source("routes/Loans.R")

#data to test
loanData <- data.frame(
  loanID = c(1, 2),
  loanType = c(1, 2),
  loanValue = c(1000, 2000),
  loanmaturity = c(4, 3),
  loan_risk = c(0.03, 0.05)
)


stateofProgressUI <- function(session){
  fluidRow(
    uiOutput(session$ns("loanProgressBars")  # Display the progress bars in a column of width 6
  )
}
#server function for the progress tracker


serverProgressTracker <- function(input, output, loanData) {
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
        span(loan_title, "-", loan_value, "-", sprintf("%.2f%%", progress))),
        pb,
        style = "margin-bottom: 10px;"
      )
      
      progress_bars[[i]] <- div_container
    }
    return(progress_bars)
  })
}

With