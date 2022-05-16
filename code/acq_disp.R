acq_disp <- function(){
  library(dplyr)
  library(policyPlot)
  data <- getfame(c("r.acq_n.q", "r.disp_n.q"), db = "/href/prod/cre/data/reitcmbs.db", start = 20100101)
  data$r.disp_n.q <- data$r.disp_n.q + 0.000001 # Zeros mess things up in teh graph
  data$diff <- data$r.acq_n.q - data$r.disp_n.q
  
  barlist <- list(s1=list(annual=NULL, semi=NULL,qtrly=data$r.acq_n.q,monthly=NULL,daily=NULL),
                  s2=list(annual=NULL, semi=NULL,qtrly=-data$r.disp_n.q,monthly=NULL,daily=NULL))
  
  pinfo <- rplot.bar(barlist,
                     Freqlab2   = "",  
                     Col        = c("#b8e1f2", "#0066B3"),
                     Border     = c("#b8e1f2", "#0066B3"),
                     Y2int      = 5,
                     Y2lim      = c(-10,15),
                     #Width.rel  = c(1,.5,.35,.3),
                     #Width.tot  = .5,
                     Space.end = c(0.05,0.1),
                     Y2lab      = "Billions of dollars",
                     footvec    = c("Source: NAREIT"),
                     Title      = "Property Acquisitions and Dispositions")
  lines(x = pinfo$x.mid, y = as.vector(data$diff), col = "red", lty = 1, lwd = 1)
  legend(
    x = pinfo$xinfo$xlab.loc[2]-0.08,
    y = 15,
    legend = c("Acquisitions", "Dispositions", "Net"),
    col = c("#b8e1f2", "#0066B3", "red"),
    pch = c(15,15,NA),
    lty = c(0,0,1),
    cex = 0.7,
    pt.cex = 1.5,
    bty = "n"
  )
  y1 <- year(end(data$diff))
  q <- quarter(end(data$diff))
  t <- paste0(y1,"\nQ",q)
  text(x = 0.95, y = last(head(tail(pinfo$y.top,2),1)), labels = t, cex = 0.7)
}
