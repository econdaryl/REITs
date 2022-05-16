# put reit TTracker data into fame
require(readxl)
require(dplyr)
require(frb)
require(tibble)

#### Cap Rates ----
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

putfame(c(caprate_off.q = "off",
          caprate_ind.q = "ind",
          caprate_ret.q = "ret",
          caprate_apt.q = "apt",
          caprate_lod.q = "lod",
          caprate_stor.q = "stor",
          caprate_hea.q = "hea",
          caprate_all.q = "all"), db = "reitcmbs")

#### Occupancy Rates ----
data <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data", skip = 203, n_max = 5) %>%
  t() %>%
  data.frame(stringsAsFactors = FALSE)

names(data) <- as.character(unlist(data[1,]))
data <- data[-1,]

data <- data %>%
  mutate_all(as.numeric)

off <- tis(data$Office, start = 20000101, frequency = 4)
ind <- tis(data$Industrial, start = 20000101, frequency = 4)
ret <- tis(data$Retail, start = 20000101, frequency = 4)
apt <- tis(data$Apartments, start = 20000101, frequency = 4)
all <- tis(data$`All Equity REITs`, start = 20000101, frequency = 4)

putfame(c(occrate_off.q = "off",
          occrate_ind.q = "ind",
          occrate_ret.q = "ret",
          occrate_apt.q = "apt",
          occrate_lod.q = "lod",
          occrate_stor.q = "stor",
          occrate_hea.q = "hea",
          occrate_all.q = "all"), db = "reitcmbs")

#### Leverage Ratios  ----
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

putfame(c(debt_book.q = "debt_book",
          debt_market.q = "debt_market",
          int_noi.q = "int_noi"), db = "reitcmbs")

### Total Returns and Dividend Yield ----
download.file("https://www.reit.com/sites/default/files/returns/Office.xls", "/href/prod/mortgage/prod/notebook/reits/data/Office.xls", method="wget")
download.file("https://www.reit.com/sites/default/files/returns/Industrial.xls", "/href/prod/mortgage/prod/notebook/reits/data/Industrial.xls", method="wget")
download.file("https://www.reit.com/sites/default/files/returns/Residential.xls", "/href/prod/mortgage/prod/notebook/reits/data/Residential.xls", method="wget")
download.file("https://www.reit.com/sites/default/files/returns/Lodging-Resorts.xls", "/href/prod/mortgage/prod/notebook/reits/data/Lodging-Resorts.xls", method="wget")
download.file("https://www.reit.com/sites/default/files/returns/Retail.xls", "/href/prod/mortgage/prod/notebook/reits/data/Retail.xls", method="wget")

office <- read_excel("/href/prod/mortgage/prod/notebook/reits/data/Office.xls")
ind <- read_excel("/href/prod/mortgage/prod/notebook/reits/data/Industrial.xls")
res <- read_excel("/href/prod/mortgage/prod/notebook/reits/data/Residential.xls")
lod <- read_excel("/href/prod/mortgage/prod/notebook/reits/data/Lodging-Resorts.xls")
ret <- read_excel("/href/prod/mortgage/prod/notebook/reits/data/Retail.xls")

div_column <- grep("Dividend", office)
tot_column <- grep("Total", office)+1

num_row <- 7
div_office <- tis(as.numeric(deframe(office[-(1:num_row-1),div_column])), start = 19931201, frequency = 12)
totret_office <- tis(as.numeric(deframe(office[-(1:num_row-1),tot_column])), start = 19931201, frequency = 12)
div_ind <- tis(as.numeric(deframe(ind[-(1:num_row-1),div_column])), start = 19931201, frequency = 12)
totret_ind <- tis(as.numeric(deframe(ind[-(1:num_row-1),tot_column])), start = 19931201, frequency = 12)
div_res <- tis(as.numeric(deframe(res[-(1:num_row-1),div_column])), start = 19931201, frequency = 12)
totret_res <- tis(as.numeric(deframe(res[-(1:num_row-1),tot_column])), start = 19931201, frequency = 12)
div_lod <- tis(as.numeric(deframe(lod[-(1:num_row-1),div_column])), start = 19931201, frequency = 12)
totret_lod <- tis(as.numeric(deframe(lod[-(1:num_row-1),tot_column])), start = 19931201, frequency = 12)
div_ret <- tis(as.numeric(deframe(ret[-(1:num_row-1),div_column])), start = 19931201, frequency = 12)
totret_ret <- tis(as.numeric(deframe(ret[-(1:num_row-1),tot_column])), start = 19931201, frequency = 12)

putfame(c(div_yld_off.m = "div_office",
          div_yld_ind.m = "div_ind",
          div_yld_res.m = "div_res",
          div_yld_lod.m = "div_lod",
          div_yld_ret.m = "div_ret",
          tot_ret_off.m = "totret_office",
          tot_ret_ind.m = "totret_ind",
          tot_ret_res.m = "totret_res",
          tot_ret_lod.m = "totret_lod",
          tot_ret_ret.m = "totret_ret"), db = "reitcmbs")

### Total Return by Investment Type ----
download.file("https://www.reit.com/sites/default/files/returns/MonthlyHistoricalReturns.xls", "/href/prod/mortgage/prod/notebook/reits/MonthlyHistoricalReturns.xls", method = "wget")
data <- read_excel("/href/prod/mortgage/prod/notebook/reits/MonthlyHistoricalReturns.xls", sheet = "Index Data")

all_col <- intersect(grep("Total", data), grep("All REITs", data))
eq_col <- intersect(grep("Total", data), grep("All Equity REITs", data))
mort_col <- intersect(grep("Total", data), grep("Mortgage REITs", data))

dividend_cols <- grep("Dividend", data)
alldiv_col <- min(dividend_cols[which(dividend_cols>all_col)])
eqdiv_col <- min(dividend_cols[which(dividend_cols>eq_col)])
mortdiv_col <- min(dividend_cols[which(dividend_cols>mort_col)])

num_row <- 9

div_all <- tis(as.numeric(deframe(data[-(1:num_row-1),alldiv_col])), start = 19711201, frequency = 12)
div_eq <- tis(as.numeric(deframe(data[-(1:num_row-1),eqdiv_col])), start = 19711201, frequency = 12)
div_mort <- tis(as.numeric(deframe(data[-(1:num_row-1),mortdiv_col])), start = 19711201, frequency = 12)
totret_all <- tis(as.numeric(deframe(data[-(1:num_row-1),all_col+1])), start = 19711201, frequency = 12)
totret_eq <- tis(as.numeric(deframe(data[-(1:num_row-1),eq_col+1])), start = 19711201, frequency = 12)
totret_mort <- tis(as.numeric(deframe(data[-(1:num_row-1),mort_col+1])), start = 19711201, frequency = 12)

putfame(c(div_yld_all.m = "div_all",
          div_yld_equity.m = "div_eq",
          div_yld_mort.m = "div_mort",
          tot_ret_all.m = "totret_all",
          tot_ret_equity.m = "totret_eq",
          tot_ret_mort.m = "totret_mort"), db = "reitcmbs")
