buy_loans <- function(input, output, vals, gamestate) {
  # Check cash balance first
  loanTerms <- getloanTerms()
  
  # Retrieve loans purchased
  purchase_list = list(type=c(1,2,3), num=c(input$loan1,input$loan2,input$loan3))
  loanTerms$num <- purchase_list$num
  total_value_loans_purchased <- sum(loanTerms$num*loanTerms$loanValue)
  print("Purchase List")
  print(purchase_list)
  print(paste("Cash on hand: ", vals$cashOnHand))
  
  # Update loans purchased
  if (total_value_loans_purchased < vals$cashOnHand) {
    updateLoansPurchased(purchase_list, current_month=vals$current_month)
    # update cash balance
    vals$cashOnHand <- vals$cashOnHand - total_value_loans_purchased
    print(paste("Cash balance after purchasing loans:", vals$cashOnHand))
  }
  else {
    print("Not enough cash")
    showModal(modalDialog(
      title = "Insufficient Cash",
      "You do not have enough cash to buy these loans.",
      easyClose = TRUE
    ))
    return (NULL)
  }
}

loan_maturity <- function(input, output, vals) {
  loanID_left_in_query <- generate_loanID_left_in_query(vals$loanData)
  vals$loanPayout <- updateLoansRemoved(vals$loanData, defaulted=0, liquidated=0, current_month=vals$current_month)
  print(paste("Loan Payout: ", vals$loanPayout))
}

loan_default <- function(input, output, vals) {
  print("Loan Default")
  print(vals$loanData)
  loanID_left_in_query <- generate_loanID_left_in_query(vals$loanData)
  updateLoansRemoved(vals$loanData, defaulted=1, liquidated=0, current_month=vals$current_month)
}