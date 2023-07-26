# At the beginning of any R session, record your AWS database password:
#source("database/setAWSPassword.R")

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
    password = "j6FNuN7aSsE+")
  conn
}

getGameState <- function(current_month){
  
  #open the connection
  conn <- getAWSConnection()
  query <- "SELECT * FROM gameState gs WHERE gs.month ="
  query <- paste(query, current_month)
  result <- dbGetQuery(conn,query)
  
  dbDisconnect(conn)
  
  # return the dataframe
  result
}

getloanTerms <- function() {
  #open the connection
  conn <- getAWSConnection()
  #Create a template for the query with placeholders for playername and password
  query <- "SELECT * FROM loanTerms"
  result <- dbGetQuery(conn,query)
  
  dbDisconnect(conn)
  
  # return the dataframe
  result
}

getloanData <- function(current_month){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  query <- "SELECT l.loanID, lt.loanValue, l.loanType, lt.loanDuration, lt.risk, l.month FROM loan l INNER JOIN loanInventory li ON l.loanID = li.loanID INNER JOIN loanTerms lt ON lt.loanType  = l.loanType"
  result <- dbGetQuery(conn,query)
  
  result$durationToMaturity <- result$loanDuration - (current_month-result$month)
  
  dbDisconnect(conn)
  
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
  ##print(query)
  result <- dbGetQuery(conn,query)
  
  dbDisconnect(conn)
  
  # return the dataframe
  result
}

updateCashInventory <- function(month, deposits, withdrawals, loanPayout, cashOnHand){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO cashInventory (month, deposits, withdrawals,loanPayout, cashOnHand) VALUES (?id1,?id2,?id3,?id4,?id5)"
  query <- sqlInterpolate(conn, querytemplate,id1=month,id2=deposits,id3=withdrawals, id4=loanPayout, id5=cashOnHand)
  ##print(query) #for debug
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
        ##print(query) #for debug
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
        #print(query) #for debug
        result <- dbGetQuery(conn,query)

        query <- sprintf("INSERT INTO loanInventory (loanID) VALUES (%s)", result$loanID)
        #print(query) #for debug
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

# Helper fucntion for updateLoansRemoved to generate loanID_left_in_query
generate_loanID_left_in_query <- function(loanData) {
  loanID_left_in_query <- ""
  for (id in loanData["loanID"]){
    loanID_left_in_query <- paste(loanID_left_in_query, id, collapse=",")
  }
  loanID_left_in_query <- paste("(", loanID_left_in_query, ")")
  loanID_left_in_query
}

# Delete loans from loanInventory that matured, liquidated due to inability to meet withdrawal demand, or defaulted on.
updateLoansRemoved <- function(loanID_left_in_query, defaulted=0, liquidated=0, current_month){
  
  #open the connection
  conn <- getAWSConnection()
  
  # Query loans to remove
  query <- "SELECT li.loanID AS loanID, lt.loanValue, lt.interest, lt.loanDuration FROM loanInventory li INNER JOIN loan l ON li.loanID = l.loanID INNER JOIN loanTerms lt ON lt.loanType = l.loanType WHERE li.loanID NOT IN"
  query <- paste(query, loanID_left_in_query)
  #print(query) #for debug
  result <- dbGetQuery(conn,query)
  
  dbDisconnect(conn)
  
  # Give value to loanPayout if loan has reached maturity
  loanPayout <- 0
  if (defaulted==0 & liquidated==0) {
    print("There are loans that reached maturity")
    result$payout <- result$loanValue*(1 + result$interest)^(result$loanDuration/12)
    loanPayout <- sum(result$payout)
  }
  
  # Add removed loans to loans completed
  for (loanID in result$loanID) {
    #open the connection
    conn <- getAWSConnection()
    
    query <- sprintf("INSERT INTO loanCompleted (loanID, defaulted, liquidated, month) VALUES (%s, %s, %s, %s)", loanID, defaulted, liquidated, current_month)
    #print(query) #for debug
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
  
  # Delete loans from loanInventory
  conn <- getAWSConnection()
  querytemplate <- "DELETE FROM loanInventory WHERE loanID NOT IN"
  query <- paste(querytemplate, loanID_left_in_query)
  #print(query) #for debug
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
  
  # return loan payout
  loanPayout
}

##### #####

test <- function(){
  # get game state for deposits and withdrawal
  getGameState(1)
  
  # start of the month data, prepare duration to maturity for calculations as well
  loanData <- getloanData(current_month=3)
  # after next button is clicked, purchase loans and add to database
  purchase_list = list(type=c(1,2,3), num=c(1,2,1))
  updateLoansPurchased(purchase_list, current_month=3)
  generate_loanID_left_in_query(loanData)
  loanPayout <- updateLoansRemoved(loanID_left_in_query="(1, 2, 3)", defaulted=0, liquidated=0, current_month=3)
  loanPayout
  
  getcashInventory(3)
  updateCashInventory(month=3, deposits=3000, withdrawals=1860, loanPayout=0,cashOnHand=1400)
  getloanTerms()
}

