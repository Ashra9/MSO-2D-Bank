# import database data
source("usePackages.R")
loadPkgs(c("sqldf","shiny","shinyalert","dplyr"))


# withdrawals <- 1860 #for testing
# cashbalance <- 1400 #for testing
# loanData <- data.frame(loanID = c(1,2,3,4,5), loanType=c(1,2,3,2,2), loanValue = c(200, 300, 600, 300, 300), durationToMaturity = c(3,1,2,2,3)) #for testing
# # loanData <- getloanData()
# 
# #loanValue is how much the loan is initially worth
# #durationToMaturity is the no. of months the loan has left before it matures
# numberofeachtypeofloan <- c(1,2,0) #for testing
# eachtypeofloan <- c(1,2,3) #for testing
# 
# percentage <- 0.7



#### Helper Function Section
selectLoansLiquidateModal <- function(loan.type.1.min=0, loan.type.1.max=2, 
                                      loan.type.2.min=0, loan.type.2.max=2, 
                                      loan.type.3.min=0, loan.type.3.max=2,
                                      notenough = FALSE,
                                      session){
  modalDialog(
    title = div(
      tags$div(
        style = "display: inline-block; margin-right: 10px;",
        tags$img(src = "sprites/Monopoly men.png", height = "50px", width = "50px", alt = "Monopoly man")
      ),
      htmlOutput(session$ns("needed")),
      "Select loans to liquidate."
    ),
    numericInput(session$ns("loantype1"), "Select number of $200 Loans to liquidate", value = 0, 
                 min = loan.type.1.min, max = loan.type.1.max),
    numericInput(session$ns("loantype2"), "Select number of $300 Loans to liquidate", value = 0, 
                 min = loan.type.2.min, max = loan.type.2.max),
    numericInput(session$ns("loantype3"), "Select number of $600 Loans to liquidate", value = 0,
                 min = loan.type.3.min, max = loan.type.3.max),
    if (notenough)
      div(tags$b("You have not selected enough loans to cover the withdrawal. Please select more loans.", style = "color: red;")),
    footer = tagList(
      actionButton(session$ns("loanliquidatesubmission"), "Confirm loans to liquidate")
    )
  )
  
}

updateCashBalance <- function(cashbalance, newamt){
  newcashbal <- cashbalance + newamt
  return(newcashbal)
}

SelectLoans <- function(numberofeachtypeofloan,eachtypeofloan){
  loansselected <- data.frame(loan.type = eachtypeofloan, no.of.each.type.of.loan = numberofeachtypeofloan)
  #need both arguments to be numerical vectors
  return(loansselected)
}

LiquidateLoans <- function(cashbalance=1400, withdrawalamount=1860, 
                           loanData=data.frame(loanID = c(1,2,3,4), 
                                               loanType=c(1,2,2,2), 
                                               loanValue = c(200, 300, 300, 300), 
                                               durationToMaturity = c(3,1,2,3)), 
                           loansselected=SelectLoans(c(1,2),c(1,2)), percentage=0.7){
  
  # Keep track of loanValue of liquidated loans
  removed_loans_value = 0
  
  for (i in 1:nrow(loansselected)){
    loan_type <- loansselected$loan.type[i]
    num_rows_to_remove <- loansselected$no.of.each.type.of.loan[i]
    
    print(loanData)
    # Filter the rows in loanData with loanValue equal to loan_type
    filtered_rows <- loanData[loanData$loanType == loan_type, ]
    
    if (num_rows_to_remove != 0 & nrow(filtered_rows) >= num_rows_to_remove) {
      # Sort the filtered rows by durationToMaturity in descending order
      sorted_rows <- filtered_rows[order(filtered_rows$durationToMaturity, decreasing = TRUE), ]
      
      # Find the rows to remove
      rows_to_remove <- sorted_rows[1:num_rows_to_remove, ]
      print(rows_to_remove) #for debugging
      
      # Calculate the loanValue of the loans that will be removed
      removed_loans_value <- sum(rows_to_remove$loanValue) + removed_loans_value
      
      
      # Remove the top num_rows_to_remove rows from loanData
      loanData <- loanData[!(loanData$loanID %in% rows_to_remove$loanID), ]
    }
    
    if(nrow(filtered_rows) < num_rows_to_remove){
      stop(sprintf("You liquidated more loans of type %s than what you own",i))
    }
    
    print(removed_loans_value) #for debugging
  }
  
  # Update cash balance after liquidating loans and satisfying withdrawals
  cashbalance <- updateCashBalance(cashbalance, percentage*removed_loans_value-withdrawalamount)
  result_list <- list(resultloanData = loanData, resultcashbalance = cashbalance, removed_loans_value = removed_loans_value)
  return(result_list)
}

getMaxLoan <- function(loanData){
  # loan_type_counts <- loanData %>%group_by(loanType) %>% tally()
  # if (is.na(loan_type_counts[which(loan_type_counts$loanType == 1),2])){
  #   loan_type_counts[which(loan_type_counts$loanType == 1),2] <- 0
  # }
  # if (is.na(loan_type_counts[which(loan_type_counts$loanType == 2),2])){
  #   loan_type_counts[which(loan_type_counts$loanType == 2),2] <- 0
  # }
  # if (is.na(loan_type_counts[which(loan_type_counts$loanType == 3),2])){
  #   loan_type_counts[which(loan_type_counts$loanType == 3),2] <- 0
  # }
  # Group loanData by loanValue and count occurrences
  loanData_counts <- loanData %>%
    group_by(loanValue) %>%
    summarize(Count = n())
  
  # Create a dataframe with all loanValues (200, 300, 600)
  all_loanValues <- data.frame(loanValue = c(200, 300, 600))
  
  # Merge the two dataframes to get the desired structure
  loan_type_counts <- merge(all_loanValues, loanData_counts, all.x = TRUE)
  
  # Fill NA values with 0 in the Count column
  loan_type_counts$Count[is.na(loan_type_counts$Count)] <- 0
  
  result_list <- list(one = loan_type_counts[which(loan_type_counts$loanValue == 200),2], 
                      two = loan_type_counts[which(loan_type_counts$loanValue == 300),2], 
                      three = loan_type_counts[which(loan_type_counts$loanValue == 600),2])
  return(result_list)
}



# updateCashInventory(month=3, deposits=3000, withdrawals=withdrawalamount, loanPayout=0,cashOnHand=cashbalance)      
# need to update loan inventory too

test <- function(){
  # test liquidate loans
  loanData <- data.frame(loanID = c(1,2,3,4,5), loanType=c(1,2,3,2,2), loanValue = c(200, 300, 600, 300, 300), durationToMaturity = c(3,1,2,2,3)) #for testing
  numberofeachtypeofloan <- c(1,2,0) #for testing
  eachtypeofloan <- c(1,2,3) #for testing
  loansselected <- SelectLoans(numberofeachtypeofloan,eachtypeofloan)
  print(loansselected)
  result_list <- LiquidateLoans(cashbalance=1400, withdrawalamount=1860, loanData, loansselected, percentage=0.7)
  return(result_list)
}

#### UI
ui <- fluidPage(
  actionButton("nextmonth", "Next Month")
)


#### Server Section
#the following will be in the server function after some observe event of a customer withdrawing:
server <- function(input, output, session){
  vals <- reactiveValues(cashOnHand = 18.63, 
                         loanData = data.frame(loanID = c(1,2), 
                                               loanType=c(1,3), 
                                               loanValue = c(200,600), 
                                               durationToMaturity = c(1,6)),
                         numberofeachtypeofloan=NULL,
                         percentage=0.7)

  observeEvent(input$nextmonth, {
    print("This is the current cash balance:")
    print(vals$cashOnHand)
    print("This is the current loan data:")
    print(vals$loanData)
    
    withdrawals <- 116.4 #as test
    #Note: when testing below chunk outside of app, comment out all the showModals and shinyAlerts.
    if(withdrawals <= vals$cashOnHand){
      vals$cashOnHand <- updateCashBalance(vals$cashOnHand, -1*withdrawals)
      print("No liquidation needed")
    }else{
      if(vals$percentage*sum(vals$loanData$loanValue)+ vals$cashOnHand < withdrawals){
        # print("Game has ended due to inability to meet withdrawal demand")
        
        #notification to tell player the game has ended
        shinyalert("You do not have enough loans to liquidate and cover the withdrawal. The game has ended.", type = "error",
                   callbackR=function(){
                     #update the endgame state to T
                     vals$endgame <- "T"
                   })
      }else{
        print("im here")
        max_number_list <- getMaxLoan(vals$loanData)
        
        #window with drop-down selections on loans for players to select from to liquidate
        showModal(selectLoansLiquidateModal(loan.type.1.min=0, loan.type.1.max=max_number_list$one, 
                                            loan.type.2.min=0, loan.type.2.max=max_number_list$two, 
                                            loan.type.3.min=0, loan.type.3.max=max_number_list$three,
                                            notenough = FALSE,
                                            session))
      
        observeEvent(input$loanliquidatesubmission, {
          removeModal()
          vals$numberofeachtypeofloan <- c(input$loantype1, input$loantype2, input$loantype3)
          eachtypeofloan <- c(1,2,3)
          req(vals$numberofeachtypeofloan)
          loansselected <- SelectLoans(vals$numberofeachtypeofloan, eachtypeofloan) #now assuming both are vectors of numbers
          print('loans selected:')
          print(loansselected) #for debugging
          print("going to set each type of loan selected to null")
          vals$numberofeachtypeofloan <- NULL
          
          result_list <- LiquidateLoans(vals$cashOnHand, withdrawals, vals$loanData, loansselected, vals$percentage)
          #print(percentage*result_list$removed_loans_value)
          if(vals$percentage*result_list$removed_loans_value+vals$cashOnHand < withdrawals){
            # print("Did not meet withdrawal demand, liquidate more loans")
            
            # showModal(...) #window to show the loans selected are not enough to cover the withdrawal
            showModal(selectLoansLiquidateModal(loan.type.1.min=0, loan.type.1.max=max_number_list$one, 
                                                loan.type.2.min=0, loan.type.2.max=max_number_list$two, 
                                                loan.type.3.min=0, loan.type.3.max=max_number_list$three,
                                                notenough = TRUE,
                                                session))
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
  })
  
  
  
}      

shinyApp(ui, server)
