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


getloanData <- function(){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  query <- "SELECT loanID, LT.loanValue, durationToMaturity FROM loanInventory LI INNER JOIN loanTerms LT ON LT.loanType  = LI.loanType"
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
  dbDisconnect(conn)
}

test <- function(){
  loan_df <- print(getloanData())
  updateCashInventory(month=3, deposits=3000, withdrawals=1860, loanPayout=0,cashOnHand=1400)
}

