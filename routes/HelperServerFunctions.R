#Ezra, Yu Liang, YiXiu, Xing Jun Contributed to this script 
source("routes/Loans.R")
source("routes/Progress Tracker.R")
#Password modal for registering from ESA class
passwordModal <- function(failed = FALSE, session) {
  modalDialog(
    title = "Create a new password",
    passwordInput(session$ns("password1"), "Enter a new password:"),
    passwordInput(session$ns("password2"), "Confirm by re-entering the new password:"),
    "If successful, you will be assigned a Player Name to go with this password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(session$ns("passwordok"), "OK")
    )
  )
}


#Function for when the next month button is clicked
next_button <- function(input,output,session, vals){
  # Add any server logic here
  ns <- session$ns
  
  observeEvent(input$nextmonth,{
    if (!is.numeric(input$loan1) | !is.numeric(input$loan2) | !is.numeric(input$loan3)) {
      # If it's not numeric, show an error message
      showModal(modalDialog(
        title = sprintf("Select Loans"),
        paste("You need to assign a value to number of loans to purchase"),
        easyClose = FALSE))
      return (NULL)
    }
    
    
    ### End of current month
    print(paste("End of current month:", vals$current_month))
    
    # Get loan data
    vals$loanData <- getloanData(vals$current_month)
    print(vals$loanData)
    
    # update loans purchased
    output <- buy_loans(input, output, vals)
    if (is.null(output)) {
      return (NULL)
    }
    
    # Reset loan purchase buttons
    updateNumericInput(session, "loan1", value = 0)
    updateNumericInput(session, "loan2", value = 0)
    updateNumericInput(session, "loan3", value = 0)

    # Get game state for withdrawal and deposits
    vals$gamestate <- getGameState(vals$current_month)
    print("Game State")
    print(vals$gamestate)
    
    # Enact withdrawals and ensure demand is met
    vals$withdrawals <- randomiser(vals$gamestate$withdrawalMean, vals$gamestate$withdrawalSTD)
    print(paste("Withdrawal amount:", vals$withdrawals))
    
    ##### Withdrawal liquidation portion #####
    # Get loan data
    print("This is the current cash balance:")
    print(vals$cashOnHand)
    print("This is the current loan data:")
    vals$loanData <- getloanData(vals$current_month)
    print(vals$loanData)
    #Note: when testing below chunk outside of app, comment out all the showModals and shinyAlerts.
    if(vals$withdrawals <= vals$cashOnHand){
      vals$cashOnHand <- updateCashBalance(vals$cashOnHand, -1*vals$withdrawals)
      print("No liquidation needed")
      
      ##### Show different modal for Chinese New Yearv #####
      if (vals$current_month == 2 | vals$current_month == 14 | vals$current_month == 26) {
        showModal(modalDialog(
          title = div(
            tags$div(
              style = "display: inline-block; margin-right: 10px;",
              tags$img(src = "sprites/money-with-wings-joypixels.gif", height = "50px", width = "50px", alt = "Monopoly man")
            ),
            sprintf("End of month %s", vals$current_month)
          ),
          HTML(
            sprintf(
              '<div style="display: flex; align-items: center;">
                 <img src="sprites/chinese-new-year2019-celebrate.gif" height="200px" width="200px" alt="CNY" style="margin-right: 10px;">
                 <div>
                   <p style="margin: 0;">Withdrawal amount: <b>%s</b><br><br>
                   Congratulations and <b>Happy Chinese New Year</b>! Despite higher withdrawal demand, cash balance is enough to cover withdrawals!</p>
                 </div>
              </div>',
              vals$withdrawals
            )
          ),
          easyClose = FALSE,
          footer = list(
            actionButton(ns("endMonth"), "OK")
          )
        ))
      }
      else if (vals$current_month == 12 | vals$current_month == 24 | vals$current_month == 36) {
        showModal(modalDialog(
          title = div(
            tags$div(
              style = "display: inline-block; margin-right: 10px;",
              tags$img(src = "sprites/money-with-wings-joypixels.gif", height = "50px", width = "50px", alt = "Monopoly man")
            ),
            sprintf("End of month %s", vals$current_month)
          ),
          HTML(
            sprintf(
              '<div style="display: flex; align-items: center;">
                 <img src="sprites/sports-sportsmanias.gif" height="200px" width="200px" alt="CNY" style="margin-right: 10px;">
                 <div>
                   <p style="margin: 0;">Withdrawal amount: <b>%s</b><br><br>
                   Congratulations and <b>Merry Christmas</b>! Despite higher withdrawal demand, cash balance is enough to cover withdrawals!</p>
                 </div>
              </div>',
              vals$withdrawals
            )
          ),
          easyClose = FALSE,
          footer = list(
            actionButton(ns("endMonth"), "OK")
          )
        ))
      }
      else {
        showModal(modalDialog(
          title = div(
            tags$div(
              style = "display: inline-block; margin-right: 10px;",
              tags$img(src = "sprites/money-with-wings-joypixels.gif", height = "50px", width = "50px", alt = "Monopoly man")
            ),
            sprintf("End of month %s", vals$current_month)
          ),
          HTML(
            paste("Withdrawal amount:", "<b>", vals$withdrawals, "</b>", "<br>", "<br>"),
            "Congratulations! Cash balance is enough to cover withdrawals!"
          ),
          easyClose = FALSE,
          footer = list(
            actionButton(ns("endMonth"), "OK")
          )
        ))
      }
      
    }else{
      if(vals$percentage*sum(vals$loanData$loanValue)+ vals$cashOnHand < vals$withdrawals){
        # print("Game has ended due to inability to meet withdrawal demand")
        
        #notification to tell player the game has ended
        shinyalert("You do not have enough loans to liquidate and cover the withdrawal. The game has ended.", type = "error",
                  callbackR=function(){
                     #update the endgame state to T
                     vals$endgame <- "T"
                   })
        return (NULL)
      }else{
        print("im here")
        max_number_list <- getMaxLoan(vals$loanData)
        
        #window with drop-down selections on loans for players to select from to liquidate
        showModal(selectLoansLiquidateModal(loan.type.1.min=0, loan.type.1.max=max_number_list$one, 
                                            loan.type.2.min=0, loan.type.2.max=max_number_list$two, 
                                            loan.type.3.min=0, loan.type.3.max=max_number_list$three,
                                            notenough = FALSE,
                                            session))
        
        
        
      }
    }
    
    print("This is the current loan data:")
    print(vals$loanData)
    print("This is the current cash balance:")
    print(vals$cashOnHand)
    
    # Record updates in cash inventory
    vals$cashOnHand <- vals$cashOnHand
    print(paste("Cash balance:", vals$cashOnHand))
    updateCashInventory(month=vals$current_month, deposits=vals$deposits, withdrawals=vals$withdrawals, loanPayout=vals$loanPayout,cashOnHand=vals$cashOnHand)
    vals$cashInventory <- getcashInventory()
    print("Cash Inventory")
    print(vals$cashInventory)
    
    })
  
    observeEvent(input$loanliquidatesubmission, {
          removeModal()
          vals$numberofeachtypeofloan <- c(input$loantype1, input$loantype2, input$loantype3)
          eachtypeofloan <- c(1,2,3)
          req(vals$numberofeachtypeofloan)
          loansselected <- SelectLoans(vals$numberofeachtypeofloan, eachtypeofloan) #now assuming both are vectors of numbers
          print('loans selected:')
          print(loansselected) #for debugging
          vals$numberofeachtypeofloan <- NULL
          
          result_list <- LiquidateLoans(vals$cashOnHand, vals$withdrawals, vals$loanData, loansselected, vals$percentage)
          print(result_list)
          #print(percentage*result_list$removed_loans_value)
          if(vals$percentage*result_list$removed_loans_value+vals$cashOnHand < vals$withdrawals){
            # print("Did not meet withdrawal demand, liquidate more loans")
            
            # showModal(...) #window to show the loans selected are not enough to cover the withdrawal
            max_number_list <- getMaxLoan(vals$loanData)
            showModal(selectLoansLiquidateModal(loan.type.1.min=0, loan.type.1.max=max_number_list$one, 
                                                loan.type.2.min=0, loan.type.2.max=max_number_list$two, 
                                                loan.type.3.min=0, loan.type.3.max=max_number_list$three,
                                                notenough = TRUE,
                                                session))
          } else {
            print(result_list) #for debugging
            vals$loanData <- result_list$resultloanData
            # Update loans that were liquidated
            loan_liquidated(input, output, vals)
            vals$cashOnHand <- result_list$resultcashbalance
            showModal(modalDialog(
              title = div(
                tags$div(
                  style = "display: inline-block; margin-right: 10px;",
                  tags$img(src = "sprites/money-with-wings-joypixels.gif", height = "50px", width = "50px", alt = "Monopoly man")
                ),
                sprintf("End of month %s", vals$current_month)
              ),
              paste("Withdrawal amount:", vals$withdrawals),
              "Congratulations! You have were able to meet withdrawal demand by prematurely liquidating some loans in your inventory!",
              easyClose = FALSE,
              footer = list(
                actionButton(ns("endMonth"), "OK")
              )
            ))
          }
    })
  
    output$needed <- renderUI({
      line1 <- "Cash balance:"
      line2 <- "Amount still needed to satisfy withdrawals:"
      line3 <- "Note that only 70% of the value of liquidated loans will be added to your cash balance."
      max_number_list <- getMaxLoan(vals$loanData)
      HTML(paste("<p style='font-size: 14px;'>", 
                 line1, vals$cashOnHand,"<br>", 
                 line2, vals$withdrawals - vals$cashOnHand, "<br>",
                 line3, "<br>", "<br>",
                 "Number of $200 loans you still have:", max_number_list$one, "<br>",
                 "Number of $300 loans you still have:", max_number_list$two, "<br>",
                 "Number of $600 loans you still have:", max_number_list$three,
                 "</p>"))
                })

  
}

after_withdrawal <- function(input, output, session, vals) {
  observeEvent(input$endMonth, {
    
    ### Start of new month

    # Update new month
    vals$current_month <- vals$current_month + 1
    
    #update the endgame state to T
    if (vals$current_month > 36){
      vals$endgame <- "T"
    }
    # Get new loan data
    vals$loanData <- getloanData(vals$current_month)
    print(vals$loanData)
    
    # Update loans that reached maturity
    loan_maturity(input, output, vals)
    
    vals$loanData <- subset(vals$loanData, vals$loanData$durationToMaturity>0)
    vals$cashOnHand <- vals$cashOnHand + vals$loanPayout
    
    # Update loans defaulted on
    loan_default_update(vals)
    loanDefault <- loan_default(input, output, vals)
    updateStats(vals$current_month, loanDefault)
    
    # Update deposit amount for next month
    vals$deposits <- randomiser(vals$gamestate$depositsMean, vals$gamestate$depositsSTD)
    print(paste("Deposits amount:", vals$deposits))
    
    vals$cashOnHand <- vals$cashOnHand + vals$deposits
    print(paste("Start of month Cash balance:", vals$cashOnHand))
    
    if (vals$current_month == 3 | vals$current_month == 15 | vals$current_month == 27) {
      showModal(modalDialog(
        title = sprintf("Start of month %s", vals$current_month),
        HTML(
          sprintf(
            '<p>Hooray! Deposits have increased as account holders cash in their red packets from Chinese New Year!</p>
          <div style="display: flex; align-items: center;">
         <img src="sprites/niu-ox.gif" height="100px" width="100px" alt="stonks man" style="margin-right: 10px;">
         <div>
           <p style="margin: 0;">Deposit amount: <b>%s</b></p>
           <p style="margin: 0;">Loan payout amount: <b>%s</b></p>
           <p style="margin: 0;">Cash balance: <b>%s</b></p>
           <p style="margin: 0;">Loan default amount: <b>%s</b></p>
           <br>
         </div>
      </div>',
            vals$deposits, vals$loanPayout, vals$cashOnHand, loanDefault
          )
        ),
        easyClose = FALSE
      ))
    } else {
      showModal(modalDialog(
        title = sprintf("Start of month %s", vals$current_month),
        HTML(
          sprintf(
            '<p>Start of a new month! Time to purchase more loans!</p>

            <div style="display: flex; align-items: center;">
         <img src="sprites/stonks-up-stongs.gif" height="100px" width="100px" alt="stonks man" style="margin-right: 10px;">
         <div>
           <p style="margin: 0;">Deposit amount: <b>%s</b></p>
           <p style="margin: 0;">Loan payout amount: <b>%s</b></p>
           <p style="margin: 0;">Cash balance: <b>%s</b></p>
           <p style="margin: 0;">Loan default amount: <b>%s</b></p>
           <br>
         </div>
      </div>',
            vals$deposits, vals$loanPayout, vals$cashOnHand, loanDefault
          )
        ),
        easyClose = FALSE
      ))
    }
    
    
    ### Update completed loans
    # Update loan maturity
    vals$completedLoansReachMaturity <- getcompletedLoans(0, 0)
    # Update defaulted loans
    vals$completedLoansDefaulted <- getcompletedLoans(1, 0)
    # Update liquidated loans
    vals$completedLoansLiquidated <- getcompletedLoans(0, 1)
  })
}

# Given mean and standard deviation of bank deposit and withdrawal
randomiser <- function(mean_val,std_dev){
  # Convert to shape and scale parameters
  shape_param <- (mean_val / std_dev)^2
  scale_param <- (std_dev^2) / mean_val
  
  # Generate a random variable with the gamma distribution
  randomiser_output <- round(rgamma(n = 1, shape = shape_param, scale = scale_param), digits=2)
  randomiser_output
}

#  Loan default function
loan_default_update <- function(vals) {
  rows_to_remove <- c()
  if (nrow(vals$loanData) > 0) {
    for (i in 1:nrow(vals$loanData)) {
      risk <- vals$loanData$risk[i]
      print(paste("Risk:",risk))
      if (runif(1) < risk) {
        print("Loan defaulted on")
        rows_to_remove <- c(rows_to_remove, i)
      }
    }
    if (length(rows_to_remove) > 0) {
      vals$loanData <- vals$loanData[-rows_to_remove, ]
    }
  }
}
