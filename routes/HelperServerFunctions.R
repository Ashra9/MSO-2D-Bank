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

#function for when the login is sucessful, go to instructions in tutorial
# login_checker <- function(input,output,session){
#   observeEvent(input$loginButton,{
#     #if credentials match
#     if (TRUE) {
#       #goes to instructions page
#       updateTabItems(session, "sidebar", selected = "tutorial")
#     }
#   })
# }


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
      showModal(modalDialog(
        title = sprintf("End of month %s", vals$current_month),
        paste("Withdrawal amount:", vals$withdrawals),
        "Congratulations! Cash balance is enough to cover withdrawals!",
        easyClose = FALSE,
        footer = list(
          actionButton(ns("endMonth"), "OK")
        )
      ))
    }else{
      if(vals$percentage*sum(vals$loanData$loanValue)+ vals$cashOnHand < vals$withdrawals){
        # print("Game has ended due to inability to meet withdrawal demand")
        
        #notification to tell player the game has ended
        shinyalert("You do not have enough loans to liquidate and cover the withdrawal. The game has ended.", type = "error")
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
    print(paste("Cash on Hand:", vals$cashOnHand))
    updateCashInventory(month=vals$current_month, deposits=vals$deposits, withdrawals=vals$withdrawals, loanPayout=vals$loanPayout,cashOnHand=vals$cashOnHand)
    vals$cashInventory <- getcashInventory()
    print("Cash Inventory")
    print(vals$cashInventory)
    
    # Update loans that were liquidated
    loan_liquidated(input, output, vals)
    
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
            vals$cashOnHand <- result_list$resultcashbalance
            showModal(modalDialog(
              title = sprintf("End of month %s", vals$current_month),
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
      line2 <- "Amount still needed:"
      line3 <- "Note that only 70% of the value of liquidated loans will be added to your cash balance."
      HTML(paste("<p>", 
                 line1, vals$cashOnHand,"<br>", 
                 line2, withdrawals - vals$cashOnHand, "<br>",
                 line3,
                 "</p>"))
                })

  
}

after_withdrawal <- function(input, output, session, vals) {
  observeEvent(input$endMonth, {
    
    ### Start of new month

    # Update new month
    vals$current_month <- vals$current_month + 1
    
    #update the endgame state to T
    if (vals$current_month > 3){
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
    print(paste("Start of month cash on hand::", vals$cashOnHand))
    
    showModal(modalDialog(
      title = sprintf("Start of month %s", vals$current_month),
      paste("Deposit amount:", vals$deposits, "|"), 
      paste("Loan payout amount:", vals$loanPayout, "|"),
      paste("Cash on hand: ", vals$cashOnHand, "|"),
      paste("Loan default amount:", loanDefault),
      easyClose = FALSE
    ))
    
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
      risk <- 1-(1-vals$loanData$risk[i])^(1/12)  
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
