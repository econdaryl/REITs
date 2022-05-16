ffo <- function(){
  library(readxl)
  library(dplyr)
  library(policyPlot)
  data <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data", skip = 1, n_max = 21) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data[,-19] # blank space in file
  
  data <- data %>%
    mutate_all(as.numeric) %>%
    mutate_all(funs(./1000))
  
  all <- window(tis(data$`All Equity REITs`, start = 20000101, frequency = 4), start = 20100101)
  off <- window(tis(data$Office, start = 20000101, frequency = 4), start = 20100101)
  ind <- window(tis(data$Industrial, start = 20000101, frequency = 4), start = 20100101)
  ret <- window(tis(data$Retail, start = 20000101, frequency = 4), start = 20100101)
  res <- window(tis(data$Residential, start = 20000101, frequency = 4), start = 20100101)
  lod <- window(tis(data$`Lodging/Resorts`, start = 20000101, frequency = 4), start = 20100101)
  oth <- window(tis(data$Diversified+data$`Self Storage`+data$`Health Care`+data$Timber+data$Infrastructure+data$`Data Centers`+data$Specialty,
             start = 20000101, frequency = 4), start = 20100101)
  
  bardata <- list(off = list(annual=NULL,semi=NULL,qtrly=off,monthly=NULL,daily=NULL),
                  ind = list(annual=NULL,semi=NULL,qtrly=ind,monthly=NULL,daily=NULL),
                  ret = list(annual=NULL,semi=NULL,qtrly=ret,monthly=NULL,daily=NULL),
                  res = list(annual=NULL,semi=NULL,qtrly=res,monthly=NULL,daily=NULL),
                  lod = list(annual=NULL,semi=NULL,qtrly=lod,monthly=NULL,daily=NULL),
                  oth = list(annual=NULL,semi=NULL,qtrly=oth,monthly=NULL,daily=NULL))
  
  pinfo <- rplot.bar(bardata,
            Title = "Funds from Operations",
            Y2lab = "Billions of dollars",
            Y2lim = c(-2,18),
            Space.end = c(0.05,0.1),
            Col = c("blue", "deepskyblue", "forestgreen", "purple", "goldenrod", "grey"),
            Border = c("blue4", "deepskyblue4", "darkgreen", "purple4", "goldenrod3", "grey60"),
            footvec = c("Note: Among Listed REITs. Other includes Self-Storage, Health Care, Timber, Infrastructure, Data Centers, Specialty, and Diversified REITs",
                        "Source: Nareit"))
  legend("topleft",
         c("Office", "Industrial", "Retail", "Residential", "Lodging", "Other"),
         fill = c("blue", "deepskyblue", "forestgreen", "purple", "goldenrod", "grey"),
         border = TRUE,
         bty = "n",
         cex = 0.7)
  y1 <- year(end(all))
  q <- quarter(end(all))
  t <- paste0(y1,"\nQ",q)
  text(x = 0.95, y = last(tail(pinfo$y.top,1)), labels = t, cex = 0.7)
}
