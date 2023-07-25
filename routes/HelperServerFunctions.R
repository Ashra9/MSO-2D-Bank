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
loan_select <- function(input,output,session, vals){
  observeEvent(input$nextmonth,{
    ### End of current month
    print("End of current month:")
    print(vals$current_month)
    
    # Check cash balance first
    loanTerms <- getloanTerms()
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

    # Get game state for withdrawal and deposits
    gamestate <- getGameState(vals$current_month)
    print(gamestate)
    
    # Enact withdrawals and ensure demand is met
    vals$withdrawals <- randomiser(gamestate$withdrawalMean, gamestate$withdrawalSTD)
    print(paste("Withdrawal amount:", vals$withdrawals))
    # ......
    
    ### Start of new month
    # Update new month
    vals$current_month <- vals$current_month + 1
    print("Start of new month:")
    print(vals$current_month)
    
    # Get loan data
    loanData <- getloanData(vals$current_month)
    print(loanData)
    
    # Update loans that reached maturity
    loanData <- subset(loanData, loanData$durationToMaturity>0)
    print("Loan Maturity")
    print(loanData)
    loanID_left_in_query <- generate_loanID_left_in_query(loanData)
    vals$loanPayout <- updateLoansRemoved(loanID_left_in_query, defaulted=0, liquidated=0, current_month=3)
    print(paste("Loan Payout: ", loanPayout))
    
    # Update loans defaulted on
    # ......
    
    # Record updates in cash inventory
    vals$cashOnHand <- vals$cashOnHand + vals$deposits + vals$loanPayout
    print(vals$cashOnHand)
    updateCashInventory(month=vals$current_month, deposits=vals$deposits, withdrawals=vals$withdrawals, loanPayout=vals$loanPayout,cashOnHand=vals$cashOnHand)
    
    # Update deposit amount for next month
    vals$deposits <- randomiser(gamestate$depositsMean, gamestate$depositsSTD)
    print(paste("Deposits amount:", vals$deposits))
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

#State Progress and bars
stateofProgressUI <- function(session){
  fluidRow(
    column(6, tableOutput(session$ns("loanTable"))),  # Display the table in a column of width 6
    column(6, uiOutput(session$ns("loanProgressBars")))  # Display the progress bars in a column of width 6
  )
}
#server function for the progress tracker
serverProgressTracker <- function(input, output, loanData) {
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
      pb <- progressBar(label = paste0("progress", i), value = progress, status = "primary")
      progress_bars[[i]] <- div(pb, style = "margin-bottom: 10px;")
    }
    return(progress_bars)
  })
}