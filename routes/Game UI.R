ingameUI <- function(session){
  fluidPage(
    fluidRow(
        bs4Card(
          background = "lightblue",
          title = uiOutput(session$ns("currMonth")),
          width = 8,
          height = NULL,
          fluidRow(
            column(width = 5,
            box(
              title = div(
                tags$div(
                  style = "display: inline-block; margin-right: 10px;",
                  tags$img(src = "sprites/money.png", height = "50px", width = "50px", alt = "Monopoly man")
                ),
                "Cash balance"
              ),
              width = 12,
              uiOutput(session$ns("totalCash")),
              background = "info"
            ),
            bs4ValueBox(
              width = 12,
              color = "info",
              subtitle = "",
              footer = "Loans Information",
              value = div(tags$b("Loan 1"), " Cost: $200 | Interest Rate: 10% | Default Rate: 10%", tags$br(),
                          tags$b("Loan 2"), " Cost: $300  | Interest Rate: 20% | Default Rate: 20%",tags$br(),
                          tags$b("Loan 3"), " Cost: $600  | Interest Rate: 40% | Default Rate: 40%")
            )
            ),
            box(
              title = div(
                tags$div(
                  style = "display: inline-block; margin-right: 10px;",
                  tags$img(src = "sprites/signing.png", height = "50px", width = "50px", alt = "Monopoly man")
                ),
                "Loan Purchasing"
              ),
              width = 7,
              #height = "100px",
              "Select No. of each type of loan!",
              numericInput(session$ns("loan1"), label = "Loan 1", value = 0, min=0),
              numericInput(session$ns("loan2"), label = "Loan 2", value = 0, min=0),
              numericInput(session$ns("loan3"), label = "Loan 3", value = 0, min=0)
            ),
            box(
              title = div(
                tags$div(
                  style = "display: inline-block; margin-right: 10px;",
                  tags$img(src = "sprites/inventory.png", height = "50px", width = "50px", alt = "Monopoly man")
                ),
                "State of each inventory"
              ),
              width = 12,
              uiOutput(session$ns("progressTrackers"))
            )
          )
        ),
      column(width = 4,
             
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
             tags$style(".nxtbtn {height: 100px;}"),
             div(
               style = "color: seagreen;",
               "Go to Next Month!"
                 ),
             box(
               title = div(
                 tags$div(
                   style = "display: inline-block; margin-right: 10px;",
                   tags$img(src = "sprites/loading.png", height = "50px", width = "50px", alt = "Monopoly man")
                 ),
                 "Completed Loans"
               ),
               width = 12,
               uiOutput(session$ns("loanCompletedMaturity")),
               uiOutput(session$ns("loanCompletedDefault")),
               uiOutput(session$ns("loanCompletedLiquidated"))
             )
             
             )
      
    ),
  fluidRow(
    box(
      title = div(
        tags$div(
          style = "display: inline-block; margin-right: 10px;",
          tags$img(src = "sprites/growth.png", height = "50px", width = "50px", alt = "Monopoly man")
        ),
        "Graphs"
      ),
      width = 12,
      uiOutput(session$ns("graphs"))
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
               actionButton(session$ns("reset"), "Play Again"),
               div(
                 style = "color: seagreen;",
                 "Graphs below!"
               ),
               uiOutput(session$ns("graphs"))
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