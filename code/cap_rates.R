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
  
  data <- data %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(data$Industrial))) %>%
    select(Office, Industrial, Retail, Apartments, `Lodging/Resorts`, `All Equity REITs`, date) %>%
    pivot_longer(!date, names_to="type", values_to="value")

  ggplot(data=data, aes(x=date, y=value)) +
    geom_line(aes(color=type, linetype=type)) +
    scale_color_manual(values=c("black", "purple", "deepskyblue", "goldenrod", "blue", "forestgreen")) +
    scale_linetype_manual(values = c(1,2,1,2,1,2)) +
    geom_line(data=filter(data, type=="All Equity REITs"), size=1.2) +
    labs(x="", y="", title = "Implied Capitalization Rate", colour="Asset Type", linetype="Asset Type", caption = "Source: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}
