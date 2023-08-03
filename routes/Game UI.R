ingameUI <- function(session){
  fluidPage(
    fluidRow(
        bs4Card(
          background = "maroon",
          title = uiOutput(session$ns("currMonth")),
          width = 9,
          height = NULL,
          fluidRow(
            bs4InfoBox(
              title = "Total Cash", 
              value = uiOutput(session$ns("totalCash")),
              color = "info"
            ),
            box(
              title = "State of each inventory",
              width = 12,
              uiOutput(session$ns("progressTrackers"))
            )
          )
        ),
      column(width = 3,
             actionButton(
               width = "100%",
               session$ns("nextmonth"), 
               "Next Month",
               status = "primary",
               outline = FALSE,
               flat = TRUE,
               size = "lg",
               class = "nxtbtn"
             ),
             tags$style(".nxtbtn {height: 100px;}")
             )
      
    ),
  fluidRow(
    box(
      title = "Graphs",
      width = 4,
      uiOutput(session$ns("graphs"))
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
      #height = "100px",
      uiOutput(session$ns("loanCompletedMaturity")),
      uiOutput(session$ns("loanCompletedDefault")),
      uiOutput(session$ns("loanCompletedLiquidated"))
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
               uiOutput(session$ns("endCash")),
               actionButton(session$ns("publishscore"), "Publish Your Score"),
               actionButton(session$ns("reset"), "Play Again")
        )
     )
  )
}
endgameServer <- function(input, output, session, vals){

  output$ingame <- renderUI({
    #check if the player is logged in
    if (is.null(vals$playername))
      "Not logged in yet."
    #display game
    else if (vals$endgame=="F"){
    ingameUI(session)
  } else if(vals$endgame=="T"){
    endgameUI(session)
  }
  })
  #when play again button is pressed, restart
  observeEvent(input$reset,{
    session$reload()
  })
  
  #renders the final cash value as the score
  output$endCash <- renderUI(paste0("Final Cash Balance: $",vals$cashOnHand))
  
  #Publishes score to leaderboard
  observeEvent(input$publishscore,{
    updateTabItems(session, "sidebar", selected = "leaderboard")
    publishScore(vals$playerid,vals$gamevariantid,vals$cashOnHand)
  })
}