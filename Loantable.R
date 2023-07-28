library(shiny)
library(shinyWidgets)
library(dplyr)

loanData <- data.frame(
  loanID = c(1, 2),
  loanType = c(1, 2),
  loanValue = c(1000, 2000),
  loanmaturity = c(4, 3),
  loan_risk = c(0.03, 0.05)
)

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           uiOutput("loanProgressBars")  # Display the progress bars
    )
  )
)

server <- function(input, output) {
  output$loanProgressBars <- renderUI({
    progress_bars <- list()
    # Calculate the progress percentage and add it as a new column in loanData
    loanData$progress <- 100 * (loanData$loanmaturity / 5)
    
    # Sort loanData by progress percentage in descending order
    loanData <- loanData[order(-loanData$progress), ]
    
    for (i in 1:nrow(loanData)) {
      loan_value <- loanData$loanValue[i]
      loan_title <- paste("Loan", loanData$loanID[i])
      loan_duration <- loanData$loanmaturity[i]
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

shinyApp(ui, server)
