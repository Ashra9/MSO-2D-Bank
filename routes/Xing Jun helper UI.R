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
  
    # Create a reactive expression for loanData
    reactiveLoanData <- reactive({
      print("XJ helper!!!!!!!")
      if (is.null(vals$loanData)) {
        print("XJ helper null")
        data.frame(
          loanID = c(1, 2, 3, 4, 5),
          loanType = c(1, 2, 3, 2, 2),
          loanValue = c(200, 300, 600, 300, 300),
          durationToMaturity = c(3, 1, 2, 2, 3)
        )
      } else {
        print("XJ not null")
        loanData <- getloanData(vals$current_month)  # Access the loanData using reactive context
        loanData
      }
    })
    
    output$loanProgressBars <- renderUI({
      print("output")
      silly <- reactiveLoanData()
      print(silly)
      silly$progress <- 100 * (silly$durationToMaturity / 5)
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
          span(loan_title, "-", loan_value, "-", sprintf("%.2f%%", progress)),
          pb,
          style = "margin-bottom: 10px;"
        )
        
        progress_bars[[i]] <- div_container
      }
      return(progress_bars)
    })
               
               
  

}


cash_balance_graph <- function(){
  
}







# #State Progress and bars
# stateofProgressUI <- function(session){
#   fluidRow(
#     column(6, tableOutput(session$ns("loanTable"))),  # Display the table in a column of width 6
#     column(6, uiOutput(session$ns("loanProgressBars")))  # Display the progress bars in a column of width 6
#   )
# }
# #server function for the progress tracker
# serverProgressTracker <- function(input, output, loanData) {
#   output$loanTable <- renderTable({
#     loan_table <- data.frame("Loan Value" = loanData$loanValue, "Months to maturity" = loanData$durationToMaturity)
#     colnames(loan_table) <- c("Loan Value", "Months to maturity")
#     return(loan_table)
#   })
#   
#   output$loanProgressBars <- renderUI({
#     progress_bars <- list()
#     for (i in 1:nrow(loanData)) {
#       loan_duration <- loanData$loanmaturity[i]
#       progress <- 100 * (vals$loan_duration / 5)  # Calculate the progress percentage
#       pb <- progressBar(label = paste0("progress", i), value = progress, status = "primary")
#       progress_bars[[i]] <- div(pb, style = "margin-bottom: 10px;")
#     }
#     return(progress_bars)
#   })
# }

