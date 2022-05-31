acq_disp <- function(){
  require(dplyr)
  
  data <- readxl::read_excel("/href/prod/cre/reits/REITs/data/ttracker.xlsx", sheet = "Data formatted for chart",
                             n_max = 3) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data %>%
    mutate_all(as.numeric) %>%
    mutate(date = seq.Date(from=as.Date("2000-01-01"), by = 'quarter', length.out=length(data$Dispositions))) %>%
    pivot_longer(!date, names_to="type", values_to="value")
  
  ggplot(data=filter(data, type!="Net Acquisitions"), aes(x=date, y=value, color=type, fill=type)) +
    geom_col() +
    geom_line(data=filter(data, type=="Net Acquisitions")) +
    scale_color_manual(values=c("#b8e1f2", "#0066B3", "red")) +
    scale_fill_manual(values=c("#b8e1f2", "#0066B3", "black")) +
    labs(x="", y="", title = "Property Acquisitions and Dispositions", caption = "Note: In billions of dollars\nSource: NAREIT") +
    theme_bw() +
    theme(plot.caption=element_text(hjust=0), legend.position = "none")
}
