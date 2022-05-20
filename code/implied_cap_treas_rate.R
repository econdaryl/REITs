imp_cap_rate <- function(){
  require(readxl)
  require(dplyr)
  require(lubridate)
  require(quantmod)
  require(tidyr)
  require(ggplot2)
  
  data <- read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data", skip = 284, n_max = 20) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)

  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data[,-19] # blank space in file

  data_post <- data %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(data$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, `All Equity REITs`, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
  # Get rid of old zeros but not new ones
  data_pre <- data_post %>%
    filter(date<=as.Date("2012-10-01")) %>%
    na_if(0)
  data <- rbind(data_pre,filter(data_post, date>as.Date("2012-10-01")))

  # pull treasury rates from FRED
  getSymbols("GS10", src='FRED')
  treas <- data.frame(date=index(GS10),coredata(GS10))

  # Convert to quarterly
  treas<- treas %>%
    mutate(qtr=quarter(date),
           year=year(date)) %>%
    filter(date>=as.Date("2000-01-01")) %>%
    group_by(year,qtr) %>%
    arrange(date) %>%
    mutate(qtr_rate=mean(GS10))%>%
    filter(row_number(GS10)==1)

  #Create the spread
  data <- data %>%
    merge(treas) %>%
    mutate(spread = value - qtr_rate)
    
  ggplot(data=data, aes(x=date, y=spread)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("black", "purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) +
    geom_line(data=filter(data, type=="All Equity REITs"), size=1.2) +
    geom_text(label=as.character(as.yearqtr(max(data$date))), x=max(data$date), y=max(tail(data$spread, n=24))) +
    labs(x="", y="", title = "Spread of Implied Capitalization Rate at Origination\nto Treasury Yield", colour="Asset Type", linetype="Asset Type", caption = "Source: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}
