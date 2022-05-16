acq_disp <- function(){
  library(dplyr)
  library(policyPlot)
  #data <- getfame(c("r.acq_n.q", "r.disp_n.q"), db = "/href/prod/cre/data/reitcmbs.db", start = 20100101)
  data <- readxl::read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data formatted for chart",
                             n_max = 3) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]
  data <- data %>%
    mutate_all(as.numeric)
  
  r.acq_n.q <- window(tis(data$`Gross Acquisitions`, start = 20000101, frequency = 4), start = 20100101)
  r.disp_n.q <- window(tis(data$Dispositions, start = 20000101, frequency = 4), start = 20100101)
  
  #r.disp_n.q <- r.disp_n.q + 0.000001 # Zeros mess things up in teh graph
  diff <- r.acq_n.q + r.disp_n.q
  
  barlist <- list(s1=list(annual=NULL, semi=NULL,qtrly=r.acq_n.q,monthly=NULL,daily=NULL),
                  s2=list(annual=NULL, semi=NULL,qtrly=r.disp_n.q,monthly=NULL,daily=NULL))
  
  pinfo <- rplot.bar(barlist,
                     Freqlab2   = "",  
                     Col        = c("#b8e1f2", "#0066B3"),
                     Border     = c("#b8e1f2", "#0066B3"),
                     Y2int      = 10,
                     Y2lim      = c(-20,40),
                     #Width.rel  = c(1,.5,.35,.3),
                     #Width.tot  = .5,
                     Space.end = c(0.05,0.1),
                     Y2lab      = "Billions of dollars",
                     footvec    = c("Source: NAREIT"),
                     Title      = "Property Acquisitions and Dispositions")
  lines(x = pinfo$x.mid, y = as.vector(diff), col = "red", lty = 1, lwd = 1)
  legend(
    x = pinfo$xinfo$xlab.loc[2]+0.4,
    y = 40,
    legend = c("Acquisitions", "Dispositions", "Net"),
    col = c("#b8e1f2", "#0066B3", "red"),
    pch = c(15,15,NA),
    lty = c(0,0,1),
    cex = 0.7,
    pt.cex = 1.5,
    bty = "n"
  )
  y1 <- year(end(diff))
  q <- quarter(end(diff))
  t <- paste0(y1,"\nQ",q)
  text(x = 0.95, y = last(head(tail(pinfo$y.top,2),1)), labels = t, cex = 0.7)
}
