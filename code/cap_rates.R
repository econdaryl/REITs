cap_rate <- function(){
  require(ggplot2)
  require(readxl)
  require(dplyr)
  require(tibble)
  require(tidyr)
  
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
    
  
  ggplot(data=data, aes(x=date, y=value)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("black", "purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) +
    geom_line(data=filter(data, type=="All Equity REITs"), size=1.2) +
    annotate(geom="text", label=as.character(as.yearqtr(max(data$date))), x=max(data$date), y=max(tail(data$value, n=24)+1)) +
    labs(x="", y="", title = "Implied Capitalization Rate", colour="Asset Type", linetype="Asset Type", caption = "Source: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}
