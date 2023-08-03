source("usePackages.R")
library(ggplot2)

plotCashGraph <- function(input, output, vals){
  # update graphs box in UI
  output$graphs <- renderUI({
    print("Plot Graph!!!!")
    if (is.null(vals$cashInventory)) {
      span("No graphs to plot yet")
    }
    else if (nrow(vals$cashInventory) < 2) {
      span("No graphs to plot yet")

    }
    else {
      cashInventory <- vals$cashInventory
      cashInventory$cumulativeDeposits <- cumsum(cashInventory$deposits)
      cashInventory$cumulativeWithdrawals <- cumsum(cashInventory$withdrawals)
      renderPlot(
        ggplot(data = cashInventory) +
          geom_ribbon(aes(x = month, y = cumulativeDeposits, ymin = 0, ymax = cumulativeDeposits, fill = "Deposits")) +
          geom_ribbon(aes(x = month, y = cumulativeWithdrawals, ymin = 0, ymax = cumulativeWithdrawals, fill = "Withdrawals")) +
          labs(x = "Month", y = "Cumulative value") +
          scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red"))
      )
    }
  })
}
      
      
cashInventory <- data.frame(month=c(1,2), deposits=c(300, 400), withdrawals=c(200,300))
plot(cashInventory$month, cashInventory$deposits)
cashInventory$cumulativeDeposits <- cumsum(cashInventory$deposits)
cashInventory$cumulativeWithdrawals <- cumsum(cashInventory$withdrawals)


# Basic line plot with points
ggplot() +
  geom_line(data=cashInventory, aes(x=month, y=cumulativeDeposits), colour="blue") +
geom_line(data=cashInventory, aes(x=month, y=cumulativeWithdrawals), colour="red")

ggplot(data = cashInventory) +
  geom_ribbon(aes(x = month, y = cumulativeDeposits, ymin = 0, ymax = cumulativeDeposits, fill = "Deposits")) +
  geom_ribbon(aes(x = month, y = cumulativeWithdrawals, ymin = 0, ymax = cumulativeWithdrawals, fill = "Withdrawals")) +
  labs(x = "Month", y = "Cumulative value") +
  scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red"))

