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
      
      df_cumDep <- bind_rows(old = cashInventory, 
                             new = cashInventory %>% mutate(cumulativeDeposits = lag(cumulativeDeposits)),
                             .id = "source") %>%
        arrange(month, source)
      df_cumWith <- bind_rows(old = cashInventory, 
                              new = cashInventory %>% mutate(cumulativeWithdrawals = lag(cumulativeWithdrawals)),
                              .id = "source") %>%
        arrange(month, source)
      
      df_cumPayout <- bind_rows(old = cashInventory, 
                                new = cashInventory %>% mutate(cumulativeLoanPayout = lag(cumulativeLoanPayout)),
                                .id = "source") %>%
        arrange(month, source)
      
      cumLoansPurchased <- getCumLoanPurchased()
      print(cumLoansPurchased)
      if (nrow(subset(cumLoansPurchased, month == vals$current_month-1))==0) {
        cumLoansPurchased <- rbind(cumLoansPurchased, data.frame(month=c(vals$current_month-1),loanID=c(0), loanValue=c(0)))
      }
      
      cum_value_loans_purchased <- cumLoansPurchased %>% group_by(month) %>% 
        summarise(sum_loanValue = sum(loanValue),
                  .groups = 'drop') %>% mutate(cum_value_loans_purchased = cumsum(sum_loanValue))
      print("cum_value_loans_purchased")
      print(cum_value_loans_purchased)
      
      df_cum_value_loans_purchased <- bind_rows(old = cum_value_loans_purchased, 
                              new = cum_value_loans_purchased %>% mutate(cum_value_loans_purchased = lag(cum_value_loans_purchased)),
                              .id = "source") %>%
        arrange(month, source)
      
      renderPlot(
        ggplot() +
          geom_ribbon(aes(x = month, y = cumulativeDeposits, ymin = 0, ymax = cumulativeDeposits, fill = "Deposits"), data = df_cumDep, alpha=0.8) +
          geom_ribbon(aes(x = month, y = cumulativeWithdrawals, ymin = 0, ymax = cumulativeWithdrawals, fill = "Withdrawals"), data = df_cumWith, alpha=0.8) +
          geom_ribbon(aes(x = month, y = cum_value_loans_purchased, ymin = 0, ymax = cum_value_loans_purchased, fill = "Loan Value"), data = df_cum_value_loans_purchased, alpha=0.8) +
          geom_ribbon(aes(x = month, y = cumulativeLoanPayout, ymin = 0, ymax = cumulativeLoanPayout, fill = "Loan Payout"), data = df_cumPayout, alpha=0.8) +
          labs(x = "Month", y = "Cumulative value") +
          scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red", "Loan Value" = "green", "Loan Payout" = "yellow"))
      )
    }
  })
}
      
      
cashInventory <- data.frame(month=c(1,2,3,4), deposits=c(300, 400, 500, 600), withdrawals=c(200,300,400,500))
# plot(cashInventory$month, cashInventory$deposits)
cashInventory$cumulativeDeposits <- cumsum(cashInventory$deposits)
cashInventory$cumulativeWithdrawals <- cumsum(cashInventory$withdrawals)

loanData <- data.frame(loanValue=c(200,200,300,600), month=c(1,2,2,3))

df <-   data.frame(month=c(1,2,2,3, 4), loanID=c(1,2,3,4,5), loanValue=c(200,200,300,400, 600))
if (nrow(subset(df, month == 5))==0) {
  df <- rbind(df, data.frame(month=5, loanID=6, loanValue=c(0)))
}

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

df_cumDep <- bind_rows(old = cashInventory, 
                       new = cashInventory %>% mutate(cumulativeDeposits = lag(cumulativeDeposits)),
                       .id = "source") %>%
  arrange(month, source)
df_cumWith <- bind_rows(old = cashInventory, 
                        new = cashInventory %>% mutate(cumulativeWithdrawals = lag(cumulativeWithdrawals)),
                        .id = "source") %>%
  arrange(month, source)

df_cum_value_loans_purchased <- bind_rows(old = cum_value_loans_purchased, 
                                          new = cum_value_loans_purchased %>% mutate(cum_value_loans_purchased = lag(cum_value_loans_purchased)),
                                          .id = "source") %>%
  arrange(month, source)

ggplot(data=cashInventory) +
  geom_ribbon(aes(x = month, y = cumulativeDeposits, ymin = 0, ymax = cumulativeDeposits, fill = "Deposits"), data = df_cumDep, alpha=0.8) +
  geom_ribbon(aes(x = month, y = cumulativeWithdrawals, ymin = 0, ymax = cumulativeWithdrawals, fill = "Withdrawals"), data = df_cumWith, alpha=0.8) +
  geom_ribbon(aes(x = month, y = cum_value_loans_purchased, ymin = 0, ymax = cum_value_loans_purchased, fill = "Loan Value"), data = df_cum_value_loans_purchased, alpha=0.8) +
  labs(x = "Month", y = "Cumulative value") +
  scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red", "Loan Value" = "green", "Loan Payout" = "yellow"))
