tot_return <- function(){
  all <- read_excel("/href/prod/cre/reits/REITs/data/MonthlyHistoricalReturns.xls", sheet = "Index Data")
  office <- read_excel("/href/prod/cre/reits/REITs/data/Office.xls")
  ind <- read_excel("/href/prod/cre/reits/REITs/data/Industrial.xls")
  res <- read_excel("/href/prod/cre/reits/REITs/data/Residential.xls")
  lod <- read_excel("/href/prod/cre/reits/REITs/data/Lodging-Resorts.xls")
  retail <- read_excel("/href/prod/cre/reits/REITs/data/Retail.xls")
  
  div_column <- grep("Total", office)+1
  
  num_row <- 7
  ret_all <- all[-(1:273-1),24] %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(all[-(1:273-1),24])),
           `All Equity REITs` = 100*`...24`/`...24`[1]) %>%
    select(-`...24`)
  ret_off <- office[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Office=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(office[-(1:num_row-1),div_column])))
  ret_ind <- ind[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Industrial=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(ind[-(1:num_row-1),div_column])))
  ret_res <- res[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Residential=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(res[-(1:num_row-1),div_column])))
  ret_lod <- lod[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Lodging=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(lod[-(1:num_row-1),div_column])))
  ret_ret <- retail[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Retail=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(retail[-(1:num_row-1),div_column])))
  
  ret <- list(ret_all, ret_off, ret_ind, ret_res, ret_lod, ret_ret) %>%
    reduce(full_join, by='date') %>%
    pivot_longer(!date, names_to="type", values_to="ret")
  
  ggplot(data=ret, aes(x=date, y=ret)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("black", "purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) +
    geom_text(label=as.character(format(max(ret$date), "%b %Y")), x=max(ret$date)-150, y=60) +
    labs(x="", y="", title = "Total Return Index", colour="Asset Type", linetype="Asset Type", caption = "Note: December 1993=100\nSource: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}

tot_return_20 <- function(){
  all <- read_excel("/href/prod/cre/reits/REITs/data/MonthlyHistoricalReturns.xls", sheet = "Index Data")
  office <- read_excel("/href/prod/cre/reits/REITs/data/Office.xls")
  ind <- read_excel("/href/prod/cre/reits/REITs/data/Industrial.xls")
  res <- read_excel("/href/prod/cre/reits/REITs/data/Residential.xls")
  lod <- read_excel("/href/prod/cre/reits/REITs/data/Lodging-Resorts.xls")
  retail <- read_excel("/href/prod/cre/reits/REITs/data/Retail.xls")
  
  div_column <- grep("Total", office)+1
  
  num_row <- 7
  ret_all <- all[-(1:273-1),24] %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(all[-(1:273-1),24])),
           `All Equity REITs` = 100*`...24`/`...24`[1]) %>%
    select(-`...24`)
  ret_off <- office[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Office=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(office[-(1:num_row-1),div_column])))
  ret_ind <- ind[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Industrial=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(ind[-(1:num_row-1),div_column])))
  ret_res <- res[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Residential=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(res[-(1:num_row-1),div_column])))
  ret_lod <- lod[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Lodging=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(lod[-(1:num_row-1),div_column])))
  ret_ret <- retail[-(1:num_row-1),div_column] %>%
    mutate_all(as.numeric) %>%
    rename(Retail=...4) %>%
    mutate(date = seq.Date(from=as.Date("1993-12-01"), by = 'month', length.out=nrow(retail[-(1:num_row-1),div_column])))
  
  ret <- list(ret_all, ret_off, ret_ind, ret_res, ret_lod, ret_ret) %>%
    reduce(full_join, by='date') %>%
    pivot_longer(!date, names_to="type", values_to="ret") %>%
    filter(date>=as.Date("2020-01-01")) %>%
    group_by(type) %>%
    mutate(ret = 100*ret/ret[1])
  
  ggplot(data=ret, aes(x=date, y=ret)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("black", "purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) +
    geom_text(label=as.character(format(max(ret$date), "%b %Y")), x=max(ret$date)-50, y=60) +
    labs(x="", y="", title = "Total Return Index", colour="Asset Type", linetype="Asset Type", caption = "Note: January 2020=100\nSource: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}