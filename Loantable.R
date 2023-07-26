library(shiny)
library(shinyWidgets)

loanData <- data.frame(
  loanID = c(1, 2),
  loanType = c(1, 2),
  loanValue = c(1000, 2000),
  loanmaturity = c(2, 3),
  loan_risk = c(0.03, 0.05)
)

ui <- fluidPage(
  box(
    title = "State of each inventory",
    width = 12,
    height = "100px",
    uiOutput("loanProgressBars")  # Display the progress bars
  )
)

server <- function(input, output) {
  output$loanProgressBars <- renderUI({
    progress_bars <- list()
    for (i in 1:nrow(loanData)) {
      loan_value <- loanData$loanValue[i]
      loan_title <- paste("Loan", loanData$loanID[i])
      loan_duration <- loanData$loanmaturity[i]
      progress <- 100 * (loan_duration / 5)  # Calculate the progress percentage
      
      pb <- progressBar(
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
