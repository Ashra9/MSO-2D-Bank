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
      cashInventory$cumulativeLoanPayout <- cumsum(cashInventory$loanPayout)
      
      cumLoansPurchased <- getCumLoanPurchased()
      print(cumLoansPurchased)
      cum_value_loans_purchased <- cumLoansPurchased %>% group_by(month) %>% 
        summarise(sum_loanValue = sum(loanValue),
                  .groups = 'drop') %>% mutate(cum_value_loans_purchased = cumsum(sum_loanValue))
      print("cum_value_loans_purchased")
      print(cum_value_loans_purchased)
      
      renderPlot(
        ggplot(data = cashInventory) +
          geom_ribbon(aes(x = month, y = cumulativeDeposits, ymin = 0, ymax = cumulativeDeposits, fill = "Deposits"), alpha=0.8) +
          geom_ribbon(aes(x = month, y = cumulativeWithdrawals, ymin = 0, ymax = cumulativeWithdrawals, fill = "Withdrawals"), alpha=0.8) +
          geom_ribbon(data=cum_value_loans_purchased, aes(x = month, y = cum_value_loans_purchased, ymin = 0, ymax = cum_value_loans_purchased, fill = "Loan Value"), alpha=0.8) +
          geom_ribbon(aes(x = month, y = cumulativeLoanPayout, ymin = 0, ymax = cumulativeLoanPayout, fill = "Loan Payout"), alpha=0.8) +
          labs(x = "Month", y = "Cumulative value") +
          scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red", "Loan Value" = "green", "Loan Payout" = "yellow"))
      )
    }
  })
}
      
      
cashInventory <- data.frame(month=c(1,2), deposits=c(300, 400), withdrawals=c(200,300))
# plot(cashInventory$month, cashInventory$deposits)
cashInventory$cumulativeDeposits <- cumsum(cashInventory$deposits)
cashInventory$cumulativeWithdrawals <- cumsum(cashInventory$withdrawals)

loanData <- data.frame(loanValue=c(200,200,300,600), month=c(1,2,2,3))

df <-   getCumLoanPurchased()
cum_value_loans_purchased <- df %>% group_by(month) %>% 
  summarise(sum_loanValue = sum(loanValue),
            .groups = 'drop') %>% mutate(cum_value_loans_purchased = cumsum(sum_loanValue))

# Basic line plot with points
ggplot() +
  geom_line(data=cashInventory, aes(x=month, y=cumulativeDeposits), colour="blue") +
geom_line(data=cashInventory, aes(x=month, y=cumulativeWithdrawals), colour="red")

ggplot(data = cashInventory) +
  geom_ribbon(aes(x = month, y = cumulativeDeposits, ymin = 0, ymax = cumulativeDeposits, fill = "Deposits")) +
  geom_ribbon(aes(x = month, y = cumulativeWithdrawals, ymin = 0, ymax = cumulativeWithdrawals, fill = "Withdrawals")) +
  geom_ribbon(data=cum_value_loans_purchased, aes(x = month, y = cum_value_loans_purchased, ymin = 0, ymax = cum_value_loans_purchased, fill = "Loan Value")) +
  labs(x = "Month", y = "Cumulative value") +
  scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red", "Loan Value" = "green"))

