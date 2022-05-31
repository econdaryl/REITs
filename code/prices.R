# Daily REIT index prices
# atm looks like I can only get All Equity REITs

prices <- function(){
  require(quantmod)
  require(ggplot2)
  
  getSymbols('^FNER', auto.assign = FALSE)
  
  FNER <- data.frame(FNER) %>%
    mutate(FNER.Close=100*FNER.Close/head(FNER.Close,1),
           date=as.Date(rownames(FNER)))
  
  ggplot(data=FNER, aes(x=date, y=FNER.Close, group=1)) +
    geom_line() +
    geom_text(label=format(max(FNER$date), "%b %d"), x=max(FNER$date)-50, y=min(tail(FNER$FNER.Close, n=24)-10)) +
    labs(x="", y="", title = "Price Index, All Equity REITs", caption = "Note: January 2012 = 100\nSource: FTSE") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}

prices30 <- function(){
  require(quantmod)
  require(ggplot2)
  
  getSymbols('^FNER', auto.assign = FALSE)
  
  FNER <- data.frame(FNER) %>%
    mutate(date = as.Date(rownames(FNER))) %>%
    filter(date>=max(date)-30) %>%
    mutate(FNER.Close=100*FNER.Close/head(FNER.Close,1))

  ggplot(data=FNER, aes(x=date, y=FNER.Close, group=1)) +
    geom_line() +
    geom_text(label=format(max(FNER$date), "%b %d"), x=max(FNER$date)-50, y=min(tail(FNER$FNER.Close, n=24)-10)) +
    labs(x="", y="", title = "Price Index, All Equity REITs", caption = "Note: Last month = 100\nSource: FTSE") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0))
}