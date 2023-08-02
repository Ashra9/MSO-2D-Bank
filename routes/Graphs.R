source("usePackages.R")

cashGraphData <- function(vals) {
  
  data.frame(
    Month = 1:vals$current_month,
    CashOnHand = cumsum(c(vals$cashOnHand, rep(0, vals$current_month - 1)))
  )
}




      
      
