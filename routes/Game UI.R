ingameUI <- function(session){
  fluidPage(
  gridPanel(
    template = "sidebar-right",
    box(
      title = "Current Stats",
      width = 12,
      
      fluidRow(
        bs4Card(
          background = "maroon",
          title = uiOutput(session$ns("currMonth")),
          width = 6,
          height = NULL,
          descriptionBlock(
            header = uiOutput(session$ns("totalCash")), 
            text = "Total Cash",
            rightBorder = FALSE,
            marginBottom = FALSE
          ),
          descriptionBlock(
            number = "Placeholder", 
            numberColor = "secondary", 
            numberIcon = icon("caret-down"),
            header = "Placeholder", 
            text = "Placeholder", 
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        bs4Card(
          background = "lime",
          title = "Title",
          width = 6,
          height = NULL,
          descriptionBlock(
            header = "Placeholder", 
            text = "Placeholder",
            rightBorder = FALSE,
            marginBottom = FALSE
          ),
          descriptionBlock(
            number = "Placeholder", 
            numberColor = "secondary", 
            numberIcon = icon("caret-down"),
            header = "Placeholder", 
            text = "Placeholder", 
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      )
    ),
    
    actionButton(
      session$ns("nextmonth"), 
      "Next Month",
      status = "primary", 
      outline = TRUE,
      flat = TRUE,
      size = "lg",
      class = "nxtbtn"
    ),
    tags$style(".nxtbtn {height: 100px;}")
  ),
  fluidRow(
    box(
      title = "Deposits and Withdrawals",
      width = 4,
      height = "100px",
      "Welcome to the dashboard!"
    ),
    box(
      title = "Loan Purchasing",
      width = 4,
      #height = "100px",
      "Select No. of each type of loan!",
      numericInput(session$ns("loan1"), label = "Loan 1 | Cost: $200 | Interest Rate: 1% | Default Rate: 50%", value = 0, min=0),
      numericInput(session$ns("loan2"), label = "Loan 2 | Cost: $300  | Interest Rate: 5% | Default Rate: 50%", value = 0, min=0),
      numericInput(session$ns("loan3"), label = "Loan 3 | Cost: $600  | Interest Rate: 10% | Default Rate: 50%", value = 0, min=0)
    ),
    box(
      title = "Completed Loans (reached maturity)",
      width = 4,
      height = "100px",
      uiOutput(session$ns("loanCompletedMaturity")),
      uiOutput(session$ns("loanCompletedDefault")),
      uiOutput(session$ns("loanCompletedLiquidated"))
    ),
    box(
      title = "State of each inventory",
      width = 12,
      
      uiOutput(session$ns("progressTrackers"))
    )
  )
  )
}

endgameUI <-function(session){
  div(id = "ending_screen",
      fluidRow(
        column(12,
               h2("Congratulations!"),
               h4("You have completed the game."),
               actionButton(session$ns("reset"), "Play Again")
        )
     )
  )
}
endgameServer <- function(input, output, session, vals){

  output$ingame <- renderUI({
  #display game
  if (vals$endgame=="F"){
    ingameUI(session)
  } else if(vals$endgame=="T"){
    endgameUI(session)
  }
  })
  observeEvent(input$reset,{
    session$reload()
  })
}