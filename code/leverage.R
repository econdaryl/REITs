leverage <- function(){
  library(readxl)
  library(dplyr)
  library(policyPlot)
  data <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data", skip = 211, n_max = 15) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data[,-c(3,7,11)]
  
  data <- data %>%
    mutate_all(as.numeric)
  
  debt_book <- tis(data$`All Equity REITs`, start = 20000101, frequency = 4)
  debt_market <- tis(data$`All Equity REITs.1`, start = 20000101, frequency = 4)
  int_noi <- tis(data$`All Equity REITs.3`, start = 20000101, frequency = 4)
  
  rplot.line(list(debt_book, debt_market, int_noi),
             Title = "Leverage Ratios",
             Y2lim = c(10, 70),
             Y2int = 10,
             legend = TRUE,
             legend.y.loc = 70,
             legend.text = c("Debt-Book Assets", "Debt-Market Assets", "Interest Expense-NOI"),
             footvec = c("Source: Nareit"))
}
