#Ezra contributed to this script
#for leaderboard
publishScore <- function(playerid,gamevariantid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,gamevariantid,asoftime,score) VALUES (?id1,?id2,NOW(),?id3)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=gamevariantid,id3=score)
  #print(query) #for debug
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

getLeaderBoard <- function(gamevariantid){
  conn <- getAWSConnection()
  #First, we need to know whether highscorewins for this game variant
  query <- paste0("SELECT highscorewins FROM LeaderGameVariant WHERE gamevariantid=",gamevariantid)
  result <- dbGetQuery(conn,query)
  #result should return a single row
  highscorewins <- result$highscorewins[1]
  #Assemble the query for this gamevariantid
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  if (highscorewins)
    query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  else
    query <- paste0(query, " ORDER BY ls.score ASC,ls.asoftime ASC")
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}