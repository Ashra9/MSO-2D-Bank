library(shiny)

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
    fluidRow(
      column(6, tableOutput("loanTable")),  # Display the table in a column of width 6
      column(6, uiOutput("loanProgressBars"))  # Display the progress bars in a column of width 6
    )
  )
)

server <- function(input, output) {
  output$loanTable <- renderTable({
    loan_table <- data.frame("Loan Value" = loanData$loanValue, "Months to maturity" = loanData$loanmaturity)
    colnames(loan_table) <- c("Loan Value", "Months to maturity")
    return(loan_table)
  })
  
  output$loanProgressBars <- renderUI({
    progress_bars <- list()
    for (i in 1:nrow(loanData)) {
      loan_duration <- loanData$loanmaturity[i]
      progress <- 100 * (loan_duration / 5)  # Calculate the progress percentage
      pb <- progressBar(id = paste0("progress", i), value = progress, status = "primary")
      progress_bars[[i]] <- div(pb, style = "margin-bottom: 10px;")
    }
    return(progress_bars)
  })
}

shinyApp(ui, server)
