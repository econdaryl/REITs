require(readxl)
require(dplyr)
require(frb)
require(policyPlot)

caprate_1q <- function(){
  
  ### Cap Rate ----
  sec_debt <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data",
                         skip = 312, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(sec_debt) <- as.character(unlist(sec_debt[1,]))
  sec_debt <- sec_debt[-1,]
  sec_debt <- sec_debt %>%
    mutate_all(as.numeric) %>%
    mutate(All = Office + Industrial + Retail + Residential + Diversified + `Lodging/Resorts` + `Self Storage` + `Health Care` + Timber + Infrastructure + `Data Centers` + Specialty)
  
  unsec_debt <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data",
                           skip = 336, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(unsec_debt) <- as.character(unlist(unsec_debt[1,]))
  unsec_debt <- unsec_debt[-1,]
  unsec_debt <- unsec_debt %>%
    mutate_all(as.numeric) %>%
    mutate(All = Office + Industrial + Retail + Residential + Diversified + `Lodging/Resorts` + `Self Storage` + `Health Care` + Timber + Infrastructure + `Data Centers` + Specialty)
  
  debt_off <- 1000*tis(sec_debt$Office + unsec_debt$Office, start = 20000101, frequency = 4)
  debt_ind <- 1000*tis(sec_debt$Industrial + unsec_debt$Industrial, start = 20000101, frequency = 4)
  debt_ret <- 1000*tis(sec_debt$Retail + unsec_debt$Retail, start = 20000101, frequency = 4)
  debt_apt <- 1000*tis(sec_debt$Apartments + unsec_debt$Apartments, start = 20000101, frequency = 4)
  debt_lod <- 1000*tis(sec_debt$`Lodging/Resorts` + unsec_debt$`Lodging/Resorts`, start = 20000101, frequency = 4)
  debt_all <- 1000*tis(sec_debt$All + unsec_debt$All, start = 20000101, frequency = 4)
  
  data <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data", skip = 32, n_max = 20) %>%
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
  
  tislist <- getfame(c(all = "mktcap_all.m",
                       off = "mktcap_off.m",
                       ind = "mktcap_ind.m",
                       ret = "mktcap_ret.m",
                       apt = "mktcap_apt.m",
                       lod = "mktcap_lod.m"), db = "/href/prod/cre/data/reitcmbs.db")
  
  offq <- convert(tislist$off, "q", method = "discrete", observed = "end")
  allq <- convert(tislist$all, "q", method = "discrete", observed = "end")
  indq <- convert(tislist$ind, "q", method = "discrete", observed = "end")
  retq <- convert(tislist$ret, "q", method = "discrete", observed = "end")
  aptq <- convert(tislist$apt, "q", method = "discrete", observed = "end")
  lodq <- convert(tislist$lod, "q", method = "discrete", observed = "end")
  
  
  offrate <- naWindow(4*100*1000*off/(offq+debt_off)) # NOI data in millions, market cap data in thousands
  indrate <- naWindow(4*100*1000*ind/(indq+debt_ind))
  retrate <- naWindow(4*100*1000*ret/(retq+debt_ret))
  aptrate <- naWindow(4*100*1000*apt/(aptq+debt_apt))
  lodrate <- naWindow(4*100*1000*lod/(lodq+debt_lod))
  allrate <- naWindow(4*100*1000*all/(allq+debt_all))
  
  tislist <- list(allrate, offrate, indrate, retrate, aptrate, lodrate)
  
  rplot.line(tislist,
             Title = "Capitalization Rate",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(-5, 15),
             Y2int = 5,
             legend = FALSE,
             legend.text = c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
             legend.y.loc = 18,
             legend.x.loc = 2010,
             footvec = c("Note: Cap rate calculated as annualized NOI divided by implied market capitalization and total debt.", "Source: NAREIT"))
  legend("topright",
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