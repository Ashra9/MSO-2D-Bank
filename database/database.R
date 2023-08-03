# At the beginning of any R session, record your AWS database password:
#source("database/setAWSPassword.R")

# Now, anywhere in your code where the password is needed you can get it using
#getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

source("usePackages.R")
loadPkgs(c("tidyverse","shiny","DBI"))

start_game_clear_tables <- function() {
  ##### delete loan inventory table
  conn <- getAWSConnection()
  query <- "DELETE FROM loanInventory"
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
  
  ##### delete loan completed table
  conn <- getAWSConnection()
  query <- "DELETE FROM loanCompleted"
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
  
  ##### delete loans table
  conn <- getAWSConnection()
  query <- "DELETE FROM loan"
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
  
  ##### delete cashInventory table
  conn <- getAWSConnection()
  query <- "DELETE FROM cashInventory"
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
  
  ##### delete stats table
  conn <- getAWSConnection()
  query <- "DELETE FROM stats"
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

getcompletedLoans <- function(defaulted, liquidated){
  
  #open the connection
  conn <- getAWSConnection()
  query <- sprintf("SELECT lc.month, lc.loanID, lt.loanValue FROM loanCompleted lc INNER JOIN loan l on l.loanID = lc.loanID INNER JOIN loanTerms lt ON lt.loanType = l.loanType WHERE defaulted = %s AND liquidated = %s", defaulted, liquidated)
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

getcashInventory <- function(){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  query <- "SELECT * FROM cashInventory"
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

updateStats <- function(month, loanDefault){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO stats (month, loanDefault) VALUES (?id1, ?id2)"
  query <- sqlInterpolate(conn, querytemplate,id1=month, id2=loanDefault)
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
updateLoansRemoved <- function(loanData, defaulted=0, liquidated=0, current_month){
  loanPayout <- 0
  loanDefault <- 0
  
  # only update database if there are loans that reach maturity, or loans that are defaulted on
  if ((nrow(subset(loanData, loanData$durationToMaturity==0)) > 0 & defaulted == 0 & liquidated == 0) | defaulted == 1 | liquidated == 1) {
    loanData <- subset(loanData, loanData$durationToMaturity>0)
    print(loanData)
    loanID_left_in_query <- generate_loanID_left_in_query(loanData)
    #open the connection
    conn <- getAWSConnection()
    if (nrow(subset(loanData, loanData$durationToMaturity>0)) > 0){
      # Query loans to remove
      query <- "SELECT li.loanID AS loanID, lt.loanValue, lt.interest, lt.loanDuration FROM loanInventory li INNER JOIN loan l ON li.loanID = l.loanID INNER JOIN loanTerms lt ON lt.loanType = l.loanType WHERE li.loanID NOT IN"
      query <- paste(query, loanID_left_in_query)
    }
    else {   # hard code impossible values for loan ids not to be part of 
      query <- "SELECT li.loanID AS loanID, lt.loanValue, lt.interest, lt.loanDuration FROM loanInventory li INNER JOIN loan l ON li.loanID = l.loanID INNER JOIN loanTerms lt ON lt.loanType = l.loanType WHERE li.loanID NOT IN"
      query <- paste(query, "(", 10^10, ")")    
    }
    
    print(query) #for debug
    result <- dbGetQuery(conn,query)
    
    dbDisconnect(conn)
    
    ### Give value to loanPayout if loan has reached maturity
    if (defaulted==0 & liquidated==0) {
      print("There are loans that reached maturity")
      result$payout <- result$loanValue*(1 + result$interest)^(result$loanDuration/12)
      print(result)
      loanPayout <- sum(result$payout)
      print(paste("Loan Payout:", loanPayout))
    }
    
    ### Give value to loanDefault
    if (defaulted==1 & liquidated==0) {
      loanDefault <- sum(result$loanValue)
      print(paste("Value of loans defaulted upon:", loanDefault))
    }
    
    # Add removed loans to loans completed
    for (loanID in result$loanID) {
      #open the connection
      conn <- getAWSConnection()
      
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
      dbDisconnect(conn)
    }
    
    # Delete loans from loanInventory
    conn <- getAWSConnection()
    if (nrow(subset(loanData, loanData$durationToMaturity>0)) > 0){
      # Query loans to remove
      querytemplate <- "DELETE FROM loanInventory WHERE loanID NOT IN"
      query <- paste(querytemplate, loanID_left_in_query)
    }
    else {   # hard code impossible values for loan ids not to be part of 
      query <- "DELETE FROM loanInventory WHERE loanID NOT IN"
      query <- paste(query, "(", 10^10, ")")    
    }
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
  
  # return loan payout
  if (defaulted==0 & liquidated == 0) {return (round(loanPayout, 2))}
  # return loan defaults
  if (defaulted==1 & liquidated == 0) {return (round(loanDefault, 2))}
  # return loan liquidated
  if (defaulted==0 & liquidated == 1) {return (round(loanPayout, 2))}
}

#for logging in and register
getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}

getRandomPlayerName <- function(conn){
  #Given a connection, call the View 'LeaderRandomName' and return the resulting name
  result <- dbGetQuery(conn,"SELECT * FROM LeaderRandomName")
  # result should be a dataframe with a single row and a column named 'playername'
  playername <- result$playername[1]
  # To test what happens when there is a duplicate entry, we can override the random result
  #playername <- "SophisticatedImaginaryZoo" # This matches an existing player in my database
  playername
}

createNewPlayerQuery <- function(conn,playername,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}

registerPlayer <- function(password){
  #open the connection
  conn <- getAWSConnection()
  playername <- getRandomPlayerName(conn)
  query <- createNewPlayerQuery(conn,playername,password)
  print(query) #for debug
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print(result)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        playername <- getRandomPlayerName(conn)
        query <- createNewPlayerQuery(conn,playername,password) }, 
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  dbDisconnect(conn)
  playername
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
  loanPayout <- updateLoansRemoved(loanData, defaulted=0, liquidated=0, current_month=4)
  loanPayout
  loanDefault <- updateLoansRemoved(loanData, defaulted=1, liquidated=0, current_month=4)
  loanDefault
  
  updateLoansRemoved(loanData, defaulted=0, liquidated=1, current_month=3)
  
  getcashInventory()
  updateCashInventory(month=3, deposits=3000, withdrawals=1860, loanPayout=0,cashOnHand=1400)
  getloanTerms()
}

