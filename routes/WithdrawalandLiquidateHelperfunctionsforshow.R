# import database data
source("usePackages.R")
loadPkgs(c("sqldf","shiny","shinyalert"))


withdrawalamount <- 1860 #for testing
cashbalance <- 1400 #for testing
loanData <- data.frame(loanID = c(1,2,3,4,5), loanType=c(1,2,3,2,2), loanValue = c(200, 300, 600, 300, 300), durationToMaturity = c(3,1,2,2,3)) #for testing
# loanData <- getloanData()

#loanValue is how much the loan is initially worth
#durationToMaturity is the no. of months the loan has left before it matures
numberofeachtypeofloan <- c(2,1) #for testing
eachtypeofloan <- c(2,1) #for testing

percentage <- 0.7



#### Helper Function Section
selectLoansLiquidateModal <- function(loan.type.1.min=0, loan.type.1.max=2, 
                                      loan.type.2.min=0, loan.type.2.max=2, 
                                      loan.type.3.min=0, loan.type.3.max=2){
  modalDialog(
    title = "Select loans to liquidate.",
    numericInput("loantype1", "Select number of $200 Loans to liquidate", value = 0, 
                 min = loan.type.1.min, max = loan.type.1.max),
    numericInput("loantype2", "Select number of $300 Loans to liquidate", value = 0, 
                 min = loan.type.2.min, max = loan.type.2.max),
    numericInput("loantype3", "Select number of $600 Loans to liquidate", value = 0,
                 min = loan.type.3.min, max = loan.type.3.max),
    
    footer = tagList(
      actionButton("loanliquidatesubmission", "Confirm loans to liquidate")
    )
    
  )
}

updateCashBalance <- function(cashbalance, newamt){
  newcashbal <- cashbalance + newamt
  return(newcashbal)
}

SelectLoans <- function(numberofeachtypeofloan,eachtypeofloan){
  loansselected <- data.frame(loan.type = eachtypeofloan, no.of.each.type.of.loan = numberofeachtypeofloan)
  return(loansselected)
}

LiquidateLoans <- function(cashbalance=1400, withdrawalamount=1860, 
                           loanData=data.frame(loanID = c(1,2,3,4,5), 
                                               loanType=c(1,2,3,2,2), 
                                               loanValue = c(200, 300, 600, 300, 300), 
                                               durationToMaturity = c(3,1,2,2,3)), 
                           loansselected=SelectLoans(c(2,1),c(2,1)), percentage=0.7){
  
  # Keep track of loanValue of liquidated loans
  removed_loans_value = 0
  
  for (i in 1:nrow(loansselected)) {
    loan_type <- loansselected$loan.type[i]
    num_rows_to_remove <- loansselected$no.of.each.type.of.loan[i]
    
    # Filter the rows in loanData with loanValue equal to loan_type
    filtered_rows <- loanData[loanData$loanType == loan_type, ]
    
    if (nrow(filtered_rows) >= num_rows_to_remove) {
      # Sort the filtered rows by durationToMaturity in descending order
      sorted_rows <- filtered_rows[order(filtered_rows$durationToMaturity, decreasing = TRUE), ]
      
      # Find the rows to remove
      rows_to_remove <- sorted_rows[1:num_rows_to_remove, ]
      print(rows_to_remove) #for debugging
      
      # Calculate the loanValue of the loans that will be removed
      removed_loans_value <- sum(rows_to_remove$loanValue) + removed_loans_value
      print(removed_loans_value) #for debugging
      
      # Remove the top num_rows_to_remove rows from loanData
      loanData <- loanData[!(loanData$loanID %in% rows_to_remove$loanID), ]
    }
    else{
      stop(sprintf("You liquidated more loans of type %s than what you own",i))
    }
  }
  
  # Update cash balance after liquidating loans and satisfying withdrawals
  cashbalance <- updateCashBalance(cashbalance, percentage*removed_loans_value-withdrawalamount)
  result_list <- list(resultloanData = loanData, resultcashbalance = cashbalance, removed_loans_value = removed_loans_value)
  return(result_list)
}

#Note: when testing below function outside of app, comment out all the showModals and shinyAlerts.
whenWithdrawal <- function(withdrawalamount, cashbalance, loanData, numberofeachtypeofloan, eachtypeofloan){
  if(withdrawalamount <= cashbalance){
    cashbalance <- updateCashBalance(cashbalance, -1*withdrawalamount)
    print("No liquidation needed")
  }else{
    # showModal(...) #notification to force player to liquidate loans
    if(percentage*sum(loanData$loanValue)+cashbalance < withdrawalamount){
      stop("Game has ended due to inability to meet withdrawal demand")

      # showModal(...) #notification to tell player the game has ended
      shinyalert("You do not have enough loans to liquidate and cover the withdrawal. The game has ended.", type = "error")
    }else{
      # showModal(...) #window with drop-down selections on loans for players to select from to liquidate
      showModal(selectLoansLiquidateModal(loan.type.1.min=0, loan.type.1.max=2, 
                                          loan.type.2.min=0, loan.type.2.max=2, 
                                          loan.type.3.min=0, loan.type.3.max=2)) #0s and 2s are test values
      #TO-DO: get the min and max number of each type of loans the player has currently
      #from database; replace the 0s and 2s with the actual values
      
      loansselected <- SelectLoans(numberofeachtypeofloan,eachtypeofloan) #may need to change the arguments; now assuming both are vectors of numbers
      # print('loans selected:')
      # print(loansselected) #for debugging
      #TO-DO: need to find a way to retrieve the type of loan and the number of each type of loan
      #from player's selection in drop-down lists on the loans picked
      
      
      result_list <- LiquidateLoans(cashbalance, withdrawalamount, loanData, loansselected, percentage)
      #print(percentage*result_list$removed_loans_value)
      if(percentage*result_list$removed_loans_value+cashbalance < withdrawalamount){
        stop("Did not meet withdrawal demand, liquidate more loans")

        # showModal(...) #window to show the loans selected are not enough to cover the withdrawal
        shinyalert("You have not selected enough loans to cover the withdrawal. Please select more loans", type = "error")
      } else {
        print(result_list) #for debugging
        loanData <- result_list$resultloanData
        cashbalance <- result_list$resultcashbalance
      }
    }
  }
  result_result_list <- list(loanData = loanData, cashbalance = cashbalance)
  return(result_result_list)
}

updateCashInventory(month=3, deposits=3000, withdrawals=withdrawalamount, loanPayout=0,cashOnHand=cashbalance)      
# need to update loan inventory too
# when testing app for now, comment out above function too

test <- function(){
  # test liquidate loans
  loanData <- data.frame(loanID = c(1,2,3,4,5), loanType=c(1,2,3,2,2), loanValue = c(200, 300, 600, 300, 300), durationToMaturity = c(3,1,2,2,3)) #for testing
  numberofeachtypeofloan <- c(2,1) #for testing
  eachtypeofloan <- c(2,1) #for testing
  loansselected <- SelectLoans(numberofeachtypeofloan,eachtypeofloan)
  LiquidateLoans(cashbalance=1400, withdrawalamount=1860, loanData, loansselected, percentage=0.7)
}

#### UI
ui <- fluidPage(
  actionButton("start", "Start")
)


#### Server Section
#the following will be in the server function after some observe event of a customer withdrawing:
server <- function(input, output, session){

  observeEvent(input$start, {
    showModal(selectLoansLiquidateModal(1,2,1,2,1,2))
  })


  
}      

shinyApp(ui, server)
