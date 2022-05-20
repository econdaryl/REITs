require(readxl)
require(dplyr)
require(frb)
require(policyPlot)
caprate <- function(){
  ### The Moving Average Function ----
  mave5 <- function(variable,start.year,start.period,series.freq) {
    #### Part I: Remove all NAs that come after the last value
    nonblanks <- NULL
    for (i in 1:length(variable)) {
      if (is.na(variable[i])==FALSE) {
        nonblanks[i] <- i      
      }
      else if (is.na(variable[i])==TRUE) {
        nonblanks[i] <- NA
      }
    }
    last.value <- max(nonblanks,na.rm=TRUE)
    # Remove all NAs after the last value
    variable.new <- NULL
    for (i in 1:last.value) {
      variable.new[i] <- variable[i]
    }
    # Rename
    variable <- variable.new
    #### Part II: create moving average time series
    # Create blank series (needs to be defined before the "for (i in 4:..." loop)
    series.mave <- NULL
    # Create moving averages
    for (i in 20:length(variable)) {
      series.mave[i] <- mean(c(variable[i],variable[i-1],variable[i-2],variable[i-3],variable[i-4],variable[i-5],variable[i-6],variable[i-7],variable[i-8],variable[i-9],variable[i-10],variable[i-11],variable[i-12],variable[i-13],variable[i-14],variable[i-15],variable[i-16],variable[i-17],variable[i-18],variable[i-19]),na.rm=TRUE)
    }
    # Make a time series
    series.mave <- tis(series.mave,start=c(start.year,start.period),freq=series.freq)
    series.mave <- naWindow(series.mave)
    return(series.mave)
  }
  
  ### Cap Rate ----
  
  sec_debt <- read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data",
                         skip = 312, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(sec_debt) <- as.character(unlist(sec_debt[1,]))
  sec_debt <- sec_debt[-1,]
  sec_debt <- sec_debt %>%
    mutate_all(as.numeric) %>%
    mutate(All = Office + Industrial + Retail + Residential + Diversified + `Lodging/Resorts` + `Self Storage` + `Health Care` + Timber + Infrastructure + `Data Centers` + Specialty) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(sec_debt$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
  
  unsec_debt <- read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data",
                           skip = 336, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(unsec_debt) <- as.character(unlist(unsec_debt[1,]))
  unsec_debt <- unsec_debt[-1,]
  unsec_debt <- unsec_debt %>%
    mutate_all(as.numeric) %>%
    mutate(All = Office + Industrial + Retail + Residential + Diversified + `Lodging/Resorts` + `Self Storage` + `Health Care` + Timber + Infrastructure + `Data Centers` + Specialty) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(unsec_debt$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, All, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
  debt <- sec_debt %>%
    left_join(unsec_debt, by=c("date","type"), suffix=c("_sec", "_unsec")) %>%
    mutate(tot=100*(value_sec+value_unsec)/value)
  
  data <- read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data", skip = 32, n_max = 20) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data[,-19] # blank space in file
  
  data <- data %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(data$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, `All Equity REITs`, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
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
  
  offm <- mave5(off, 2000, 1, 4)
  indm <- mave5(ind, 2000, 1, 4)
  retm <- mave5(ret, 2000, 1, 4)
  aptm <- mave5(apt, 2000, 1, 4)
  lodm <- mave5(lod, 2000, 1, 4)
  allm <- mave5(all, 2000, 1, 4)
  
  offrate <- naWindow(4*100*1000*offm/(offq+debt_off)) # NOI data in millions, market cap data in thousands, debt in millions
  indrate <- naWindow(4*100*1000*indm/(indq+debt_ind)) # Multiply by 100 to get cap rates not as decimals
  retrate <- naWindow(4*100*1000*retm/(retq+debt_ret))
  aptrate <- naWindow(4*100*1000*aptm/(aptq+debt_apt))
  lodrate <- naWindow(4*100*1000*lodm/(lodq+debt_lod))
  allrate <- naWindow(4*100*1000*allm/(allq+debt_all))
  
  tislist <- list(allrate, offrate, indrate, retrate, aptrate, lodrate)
  
  rplot.line(tislist,
             Title = " Smoothed Capitalization Rate",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(0, 15),
             Y2int = 5,
             legend = FALSE,
             legend.text = c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
             legend.y.loc = 18,
             legend.x.loc = 2010,
             footvec = c("Note: Cap rate calculated as 5-year moving average of annualized NOI divided by implied market capitalization plus total debt.", "Source: NAREIT"))
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
