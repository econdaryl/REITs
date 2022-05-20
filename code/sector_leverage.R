sector_lev <- function(){
  require(readxl)
  require(readr)
  require(dplyr)
  prop_tot <- read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data", skip = 260, n_max = 20) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(prop_tot) <- as.character(unlist(prop_tot[1,]))
  prop_tot <- prop_tot[-1,]
  
  prop_tot <- prop_tot %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(prop_tot$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
  sec_debt <- read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data",
                         skip = 312, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(sec_debt) <- as.character(unlist(sec_debt[1,]))
  sec_debt <- sec_debt[-1,]
  sec_debt <- sec_debt %>%
    mutate_all(as.numeric) %>%
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
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(unsec_debt$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
  data <- sec_debt %>%
    left_join(unsec_debt, by=c("date","type"), suffix=c("_sec", "_unsec")) %>%
    left_join(prop_tot, by=c("date","type")) %>%
    mutate(lev=100*(value_sec+value_unsec)/value)
  
  ggplot(data=data, aes(x=date, y=lev)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(2,1,2,1,2)) +
    geom_text(label=as.character(as.yearqtr(max(data$date))), x=max(data$date), y=max(tail(data$lev, n=24)+2)) +
    labs(x="", y="", title = "Leverage Ratio", colour="Asset Type", linetype="Asset Type", caption = "Source: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}
