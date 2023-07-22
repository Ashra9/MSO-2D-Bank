# At the beginning of any R session, record your AWS database password:
source("database/setAWSPassword.R")

# Now, anywhere in your code where the password is needed you can get it using
#getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

source("usePackages.R")
loadPkgs(c("tidyverse","shiny","DBI"))

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student105",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student105",
    password = getOption("AWSPassword"))
  conn
}


getloanData <- function(current_month){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  query <- "SELECT LI.loanID, LT.loanValue, l.loanType, LT.loanDuration, l.monthPurchased FROM loanInventory LI INNER JOIN loan l ON l.loanID = LI.loanID INNER JOIN loanTerms LT ON LT.loanType  = l.loanType"
  result <- dbGetQuery(conn,query)
  
  result$durationToMaturity <- result$loanDuration - (current_month-result$monthPurchased)
  
  # return the dataframe
  result
}

getcashInventory <- function(month){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM cashInventory ci WHERE `month` = ?id1"
  query <- sqlInterpolate(conn, querytemplate,id1=month)
  print(query)
  result <- dbGetQuery(conn,query)
  
  # return the dataframe
  result
}

updateCashInventory <- function(month, deposits, withdrawals, loanPayout, cashOnHand){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO cashInventory (month, deposits, withdrawals,loanPayout, cashOnHand) VALUES (?id1,?id2,?id3,?id4,?id5)"
  query <- sqlInterpolate(conn, querytemplate,id1=month,id2=deposits,id3=withdrawals, id4=loanPayout, id5=cashOnHand)
  print(query) #for debug
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print("Score published")
        success <- TRUE
      }, error=function(cond){print("publishScore: ERROR")
        print(cond)
      }, 
      warning=function(cond){print("publishScore: WARNING")
        print(cond)},
      finally = {}
    )
  } # end while loop
  dbDisconnect(conn)
}


##### Update Loan Inventory #####

# Add loans purchased by player
#purchase_list = list(type=c(1,2,3), num=c(1,0,0))
updateLoansPurchased <- function(purchase_list, current_month){
  conn <- getAWSConnection()
  
  for (loantype in purchase_list$type) {
    if (purchase_list$num[loantype] != 0) {
      for (num in 1:purchase_list$num[loantype]) {
        ##### Update loan table #####
        query <- sprintf("INSERT INTO loan (loanType, month) VALUES (%s, %s)", loantype, current_month)
        print(query) #for debug
        success <- FALSE
        iter <- 0
        MAXITER <- 5
        while(!success & iter < MAXITER){
          iter <- iter+1
          tryCatch(
            {  # This is not a SELECT query so we use dbExecute
              result <- dbExecute(conn,query)
              print("Score published")
              success <- TRUE
            }, error=function(cond){print("publishScore: ERROR")
              print(cond)
            }, 
            warning=function(cond){print("publishScore: WARNING")
              print(cond)},
            finally = {}
          )
      } # end while loop
        
        ##### Update loanInventory table #####
        # find latest loanID from loan table, which is the new loan that was just bought
        query <- "SELECT loanID FROM loan ORDER BY loanID DESC LIMIT 1"
        print(query) #for debug
        result <- dbGetQuery(conn,query)

        query <- sprintf("INSERT INTO loanInventory (loanID) VALUES (%s)", result$loanID)
        print(query) #for debug
        success <- FALSE
        iter <- 0
        MAXITER <- 5
        while(!success & iter < MAXITER){
          iter <- iter+1
          tryCatch(
            {  # This is not a SELECT query so we use dbExecute
              result <- dbExecute(conn,query)
              print("Score published")
              success <- TRUE
            }, error=function(cond){print("publishScore: ERROR")
              print(cond)
            }, 
            warning=function(cond){print("publishScore: WARNING")
              print(cond)},
            finally = {}
          )
        } # end while loop
        ##### #####
    }
  }
  }
  
  
  dbDisconnect(conn)
}

# Delete loans from loanInventory that matured, liquidated due to inability to meet withdrawal demand, or defaulted on.
updateLoansRemoved <- function(loanID_left_in_query, defaulted=0, liquidated=0, current_month){
  # Query loans to remove
  query <- "SELECT loanID FROM loanInventory WHERE loanID NOT IN"
  query <- paste(query, loanID_left_in_query)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  
  # Add removed loans to loans completed
  for (loanID in result$loanID) {
    query <- sprintf("INSERT INTO loanCompleted (loanID, defaulted, liquidated, month) VALUES (%s, %s, %s, %s)", loanID, defaulted, liquidated, current_month)
    print(query) #for debug
    success <- FALSE
    iter <- 0
    MAXITER <- 5
    while(!success & iter < MAXITER){
      iter <- iter+1
      tryCatch(
        {  # This is not a SELECT query so we use dbExecute
          result <- dbExecute(conn,query)
          print("Score published")
          success <- TRUE
        }, error=function(cond){print("publishScore: ERROR")
          print(cond)
        }, 
        warning=function(cond){print("publishScore: WARNING")
          print(cond)},
        finally = {}
      )
    } # end while loop
  }
  
  # Delete loans from loanInventory
  conn <- getAWSConnection()
  querytemplate <- "DELETE FROM loanInventory WHERE loanID NOT IN"
  query <- paste(querytemplate, loanID_left_in_query)
  print(query) #for debug
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print("Score published")
        success <- TRUE
      }, error=function(cond){print("publishScore: ERROR")
        print(cond)
      }, 
      warning=function(cond){print("publishScore: WARNING")
        print(cond)},
      finally = {}
    )
  } # end while loop
  dbDisconnect(conn)
  }

##### #####

test <- function(){
  # start of the month data, prepare duration to maturity for calculations as well
  loanData <- getloanData(current_month=3)
  # after next button is clicked, purchase loans and add to database
  purchase_list = list(type=c(1,2,3), num=c(1,2,1))
  updateLoansPurchased(purchase_list, current_month=3)
  updateLoansRemoved("(1, 2, 3)", defaulted=0, liquidated=0, current_month=3)
  
  getcashInventory(3)
  updateCashInventory(month=3, deposits=3000, withdrawals=1860, loanPayout=0,cashOnHand=1400)
  
}

