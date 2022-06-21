div_yld <- function(){
  require(purrr)
  require(dplyr)
  require(readxl)
  require(tidyr)
  require(ggplot2)
  require(zoo)
  ### Total Returns and Dividend Yield ----
  download.file("https://www.reit.com/sites/default/files/returns/Office.xls", "/href/prod/cre/reits/REITs/data/Office.xls", method="wget")
  download.file("https://www.reit.com/sites/default/files/returns/Industrial.xls", "/href/prod/cre/reits/REITs/data/Industrial.xls", method="wget")
  download.file("https://www.reit.com/sites/default/files/returns/Residential.xls", "/href/prod/cre/reits/REITs/data/Residential.xls", method="wget")
  download.file("https://www.reit.com/sites/default/files/returns/Lodging-Resorts.xls", "/href/prod/cre/reits/REITs/data/Lodging-Resorts.xls", method="wget")
  download.file("https://www.reit.com/sites/default/files/returns/Retail.xls", "/href/prod/cre/reits/REITs/data/Retail.xls", method="wget")
  download.file("https://www.reit.com/sites/default/files/returns/MonthlyHistoricalReturns.xls", "/href/prod/cre/reits/REITs/data/MonthlyHistoricalReturns.xls", method = "wget")
  
  all <- read_excel("/href/prod/cre/reits/REITs/data/MonthlyHistoricalReturns.xls", sheet = "Index Data")
  office <- read_excel("/href/prod/cre/reits/REITs/data/Office.xls")
  ind <- read_excel("/href/prod/cre/reits/REITs/data/Industrial.xls")
  res <- read_excel("/href/prod/cre/reits/REITs/data/Residential.xls")
  lod <- read_excel("/href/prod/cre/reits/REITs/data/Lodging-Resorts.xls")
  ret <- read_excel("/href/prod/cre/reits/REITs/data/Retail.xls")
  
  div_column <- grep("Dividend", office)

  num_row <- 8
  div_all <- all[-(1:274-1),28] %>%
    mutate_all(as.numeric) %>%
    rename(`All Equity Reits`=...28) %>%
    mutate(date = seq.Date(from=as.Date("1994-01-01"), by = 'month', length.out=nrow(all[-(1:274-1),28])))
  div_off <- office[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Office=...11) %>%
    mutate(date = seq.Date(from=as.Date("1994-01-01"), by = 'month', length.out=nrow(office[-(1:num_row-1),div_column])))
  div_ind <- ind[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Industrial=...11) %>%
    mutate(date = seq.Date(from=as.Date("1994-01-01"), by = 'month', length.out=nrow(ind[-(1:num_row-1),div_column])))
  div_res <- res[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Residential=...11) %>%
    mutate(date = seq.Date(from=as.Date("1994-01-01"), by = 'month', length.out=nrow(res[-(1:num_row-1),div_column])))
  div_lod <- lod[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Lodging=...11) %>%
    mutate(date = seq.Date(from=as.Date("1994-01-01"), by = 'month', length.out=nrow(lod[-(1:num_row-1),div_column])))
  div_ret <- ret[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Retail=...11) %>%
    mutate(date = seq.Date(from=as.Date("1994-01-01"), by = 'month', length.out=nrow(ret[-(1:num_row-1),div_column])))
  
  div <- list(div_all, div_off, div_ind, div_res, div_lod, div_ret) %>%
    reduce(full_join, by='date') %>%
    pivot_longer(!date, names_to="type", values_to="div")
  
  ggplot(data=div, aes(x=date, y=div)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("black", "purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) +
    geom_text(label=as.character(as.yearqtr(max(div$date))), x=max(div$date)-150, y=max(tail(div$lev, n=24)+5)) +
    labs(x="", y="", title = "Dividend Yield", colour="Asset Type", linetype="Asset Type", caption = "Source: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}
