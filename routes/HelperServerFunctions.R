source("routes/Loans.R")
#Password modal for registering from ESA class
passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Create a new password",
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    "If successful, you will be assigned a Player Name to go with this password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

#function for when the login is sucessful, go to instructions in tutorial
login_checker <- function(input,output,session){
  observeEvent(input$loginButton,{
    #if credentials match
    if (TRUE) {
      #goes to instructions page
      updateTabItems(session, "sidebar", selected = "tutorial")
    }
  })
}


#Function for when the next month button is clicked
next_button <- function(input,output,session, vals){
  
  observeEvent(input$nextmonth,{
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

    # Get game state for withdrawal and deposits
    gamestate <- getGameState(vals$current_month)
    print(gamestate)
    
    # Enact withdrawals and ensure demand is met
    vals$withdrawals <- randomiser(gamestate$withdrawalMean, gamestate$withdrawalSTD)
    print(paste("Withdrawal amount:", vals$withdrawals))
    showModal(modalDialog(
      title = "Withdrawals",
      paste("Withdrawal amount:", vals$withdrawals),
      easyClose = FALSE
    ))
    
    # Record updates in cash inventory
    vals$cashOnHand <- vals$cashOnHand
    print(paste("Cash on Hand:", vals$cashOnHand))
    updateCashInventory(month=vals$current_month, deposits=vals$deposits, withdrawals=vals$withdrawals, loanPayout=vals$loanPayout,cashOnHand=vals$cashOnHand)

    print("This is the current cash balance:")
    print(vals$cashOnHand)
    print("This is the current loan data:")
    print(vals$loanData)
    #Note: when testing below chunk outside of app, comment out all the showModals and shinyAlerts.
    if(vals$withdrawals <= vals$cashOnHand){
      vals$cashOnHand <- updateCashBalance(vals$cashOnHand, -1*vals$withdrawals)
      print("No liquidation needed")
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
                                            notenough = FALSE))
        
        
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
          #print(percentage*result_list$removed_loans_value)
          if(vals$percentage*result_list$removed_loans_value+vals$cashOnHand < vals$withdrawals){
            # print("Did not meet withdrawal demand, liquidate more loans")
            
            # showModal(...) #window to show the loans selected are not enough to cover the withdrawal
            showModal(selectLoansLiquidateModal(loan.type.1.min=0, loan.type.1.max=max_number_list$one, 
                                                loan.type.2.min=0, loan.type.2.max=max_number_list$two, 
                                                loan.type.3.min=0, loan.type.3.max=max_number_list$three,
                                                notenough = TRUE))
          } else {
            print(result_list) #for debugging
            vals$loanData <- result_list$resultloanData
            vals$cashOnHand <- result_list$resultcashbalance
          }
        })
      }
    }
    
    print("This is the current loan data:")
    print(vals$loanData)
    print("This is the current cash balance:")
    print(vals$cashOnHand)
    
    
    ### Start of new month
    # Update new month
    vals$current_month <- vals$current_month + 1
    print(paste("Start of new month:", vals$current_month))
    showModal(modalDialog(
      title = "Month",
      paste("Start of new month:", vals$current_month),
      easyClose = TRUE
    ))
    
    # Update loans that reached maturity
    loan_maturity(input, output, vals, vals$loanData)
    if (vals$loanPayout > 0) {
      showModal(modalDialog(
        title = "Loan Payout",
        paste("Loan Payout amount:", vals$loanPayout),
        easyClose = TRUE
      ))
    }
    vals$loanData <- subset(vals$loanData, vals$loanData$durationToMaturity>0)
    vals$cashOnHand <- vals$cashOnHand + vals$loanPayout
    
    # Update loans defaulted on
    # ......
    
    # Update deposit amount for next month
    vals$deposits <- randomiser(gamestate$depositsMean, gamestate$depositsSTD)
    print(paste("Deposits amount:", vals$deposits))
    
    vals$cashOnHand <- vals$cashOnHand + vals$deposits
    print(paste("Start of month cash on hand::", vals$cashOnHand))
    
    showModal(modalDialog(
      title = "Deposits",
      paste("Deposit amount:", vals$deposits, "\n",
            "Loan payout amount:", vals$loanPayout, "\n",
            "Loan default amount:",
            "Cash on hand: ", vals$cashOnHand
      ),
      easyClose = FALSE
    ))
    
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

