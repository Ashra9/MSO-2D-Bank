#Yu Liang contributed to this script
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
      if (nrow(subset(cashInventory, month == vals$current_month))==0) {
        cashInventory <- rbind(cashInventory, data.frame(deposits=c(0), withdrawals=c(0), loanPayout=c(0), cashOnHand=c(0), month=c(vals$current_month)))
      }
      
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
      if (nrow(subset(cumLoansPurchased, month == vals$current_month))==0) {
        cumLoansPurchased <- rbind(cumLoansPurchased, data.frame(month=c(vals$current_month),loanID=c(0), loanValue=c(0)))
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
          geom_ribbon(aes(x = month, y = cum_value_loans_purchased, ymin = 0, ymax = cum_value_loans_purchased, fill = "Loan Purchase"), data = df_cum_value_loans_purchased, alpha=0.8) +
          geom_ribbon(aes(x = month, y = cumulativeLoanPayout, ymin = 0, ymax = cumulativeLoanPayout, fill = "Loan Payout"), data = df_cumPayout, alpha=0.8) +
          #geom_ribbon(aes(x = month, y = cum_loanLiquidation, ymin = 0, ymax = cum_loanLiquidation, fill = "Loan Liquidation"), data = df_cum_value_loan_liquidation, alpha=0.8) +
          #geom_ribbon(aes(x = month, y = cum_loanDefault, ymin = 0, ymax = cum_loanDefault, fill = "Loan Default"), data = df_cum_value_loan_default, alpha=0.8) +
          
          labs(x = "Month", y = "Cumulative value") +
          scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red", "Loan Purchase" = "green", "Loan Payout" = "yellow"))
          #scale_fill_manual(name = "Legends", values = c("Deposits" = "blue", "Withdrawals" = "red", "Loan Purchase" = "green", "Loan Payout" = "yellow", "Loan Liquidation" = "orange", "Loan Default" = "brown"))
      )
    }
  })
}
