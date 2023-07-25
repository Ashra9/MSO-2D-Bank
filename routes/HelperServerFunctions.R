loan_select <- function(input,output,session, vals){
  observeEvent(input$nextmonth,{
    print("Current month:")
    print(vals$current_month)
    loanData <- getloanData(vals$current_month)
    print(loanData)
    
    # Update loans purchased
    # Check cash balance first.......
    purchase_list = list(type=c(1,2,3), num=c(input$loan1,input$loan2,input$loan3))
    print("Purchase List")
    print(purchase_list)
    #updateLoansPurchased(purchase_list, current_month=vals$current_month)
    
    # Enact withdrawals and ensure demand is met
    # ......
    
    # Update loans that reached maturity
    loanData <- subset(loanData, loanData$durationToMaturity>0)
    print("Loan Maturity")
    print(loanData)
    #loanID_left_in_query <- generate_loanID_left_in_query(loanData)
    #updateLoansRemoved(loanID_left_in_query, defaulted=0, liquidated=0, current_month=3)
    
    # Update loans defaulted on
    # ......
    
    # Update new month
    vals$current_month <- vals$current_month + 1
  })
}