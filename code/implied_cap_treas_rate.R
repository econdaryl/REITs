imp_cap_rate <- function(){
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(policyPlot)
  library(frb)

  data <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data", skip = 284, n_max = 20) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)

  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data[,-19] # blank space in file

  data <- data %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, `Self Storage`, `Health Care`, `All Equity REITs`) %>%
    mutate_all(as.numeric)

  off <- tis(data$Office, start = 20000101, frequency = 4)
  ind <- tis(data$Industrial, start = 20000101, frequency = 4)
  ret <- tis(data$Retail, start = 20000101, frequency = 4)
  apt <- tis(data$Apartments, start = 20000101, frequency = 4)
  lod <- tis(data$`Lodging/Resorts`, start = 20000101, frequency = 4)
  stor <- tis(data$`Self Storage`, start = 20000101, frequency = 4)
  hea <- tis(data$`Health Care`, start = 20000101, frequency = 4)
  all <- tis(data$`All Equity REITs`, start = 20000101, frequency = 4)


  # pull treasury rates from fame
  treas <- getfame(c(yield="riflgfcy10_n.m"), db = "us", start = 20000101)

  treas_df<- tis2df(treas)
  treas_df$date<- as.Date(treas_df$date)

  treas_df<-cbind(treas_df,qtr=quarter(treas_df$date), year=year(treas_df$date))

  treas_df<- treas_df %>%
    group_by(year,qtr) %>%
    arrange(date) %>%
    mutate(qtr_rate=mean(yield))%>%
    filter(row_number(yield)==1)

  treas.qtr <- tis(treas_df$qtr_rate,start=20000101, freq=4)



  #treas.avg <- as.tis(stats::filter(treas$yield, rep(1/3,3), sides = 1))

  #Create the spread
  off.spd <- off - treas.qtr
  ind.spd <- ind - treas.qtr
  ret.spd <- ret - treas.qtr
  apt.spd <- apt - treas.qtr
  lod.spd <- lod - treas.qtr
  stor.spd <- stor - treas.qtr
  hea.spd <- hea - treas.qtr
  all.spd <- all- treas.qtr

  tislist <- list(all.spd, off.spd, ind.spd, ret.spd, apt.spd, lod.spd)

  rplot.line(tislist,
             Title = "Spread of Implied Capitalization Rate at Origination\nto Treasury Yield",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(-2, 12),
             legend = FALSE,
             Y2lab = "Percent",
             Freqlab2 = "Quarterly",
             legend.text = c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
             legend.y.loc = 18,
             legend.x.loc = 2010,
             footvec = c("Source: NAREIT"))

  legend("top",
         c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
         ncol = 2,
         col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
         xpd = TRUE,
         inset = 0.025,
         pch = NULL,
         x.intersp = 0.5,
         y.intersp = 1,
         bty = "n",
         cex = .65,
         lty = c(1, 2, 1, 2, 1, 2),
         lwd = 2,)
}
