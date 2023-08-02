source("usePackages.R")
loadPkgs(c("sqldf","shiny","shinyalert","dplyr"))

# UI function to display progress bars
stateofProgressUI <- function(session){
  fluidRow(
    uiOutput(session$ns("loanProgressBars"))  # Display the progress bars in a column of width 6
  )
}

# Make sure you have shinyWidgets package installed
# install.packages("shinyWidgets")

# Make sure you have shinyWidgets package installed
# install.packages("shinyWidgets")

serverProgressTracker <- function(input, output, vals) {
  
    output$loanProgressBars <- renderUI({
      
      print("output")
      silly <- vals$loanData
      print(silly)
      print(is.null(silly))
      print(nrow(silly))
      
      if (is.null(silly)) {
        print("Silly is NULL")
        return (span("No loans in inventory currently, purchase some from the loans purchasing panel!"))
      } else if (nrow(silly) == 0) {
        print("Silly 0 rows")
        return (span("No loans in inventory currently, purchase some from the loans purchasing panel!"))
      }
      
      silly$progress <- 100 * ((silly$loanDuration - silly$durationToMaturity)/silly$loanDuration)
      silly <- silly[order(-silly$progress), ]
      
      progress_bars <- list()
      for (i in 1:nrow(silly)) {
        loan_value <- silly$loanValue[i]
        loan_title <- paste("Loan", silly$loanID[i])
        progress <- silly$progress[i]
        
        pb_id <- paste0("loan_", i)  # Unique id for each progress bar
        
        pb <- shinyWidgets::progressBar(
          id = pb_id,
          value = progress
        )
        
        div_container <- div(
          span(loan_title, "|", "Loan value - $", loan_value, "|", "Duration to maturity -", silly$durationToMaturity[i], "month", "| progress -", sprintf("%.2f%%", progress)),
          pb,
          style = "margin-bottom: 10px;"
        )
        
        progress_bars[[i]] <- div_container
      }
      return(progress_bars)
    })
}

completedLoansTracker <- function(input, output, vals) {
  
  # Update loans that reached maturity
  output$loanCompletedMaturity <- renderUI({
    if (is.null(vals$completedLoansReachMaturity)) {
      span("No loans that reached maturity yet!")
    }
    else if (nrow(vals$completedLoansReachMaturity) == 0) {
      span("No loans that reached maturity yet!")
    }
    else {
      div(
        span("Loans that reached maturity"),
        renderTable({
          vals$completedLoansReachMaturity
        })
      )
    }
  })
  
  # Update loans that were defaulted on
  output$loanCompletedDefault <- renderUI({
    if (is.null(vals$completedLoansDefaulted)) {
      span("No loans that were defaulted on yet!")
    }
    else if (nrow(vals$completedLoansDefaulted) == 0) {
      span("No loans that were defaulted on yet!")
    }
    else {
      div(
        span("Loans that were defaulted on"),
        renderTable({
          vals$completedLoansDefaulted
        })
      )
    }
  })
  
  # Update loans that were liquidated
  output$loanCompletedLiquidated <- renderUI({
    if (is.null(vals$completedLoansLiquidated)) {
      span("No loans that were liquidated yet!")
    }
    else if (nrow(vals$completedLoansLiquidated) == 0) {
      span("No loans that were liquidated yet!")
    }
    else {
      div(
        span("Loans that were liquidated"),
        renderTable({
          vals$completedLoansLiquidated
        })
      )
    }
  })
  
}

