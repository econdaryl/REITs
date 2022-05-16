occup <- function(){
  library(policyPlot)
  library(frb)

  tislist <- getfame(c(all = "occrate_all.q",
                       off = "occrate_off.q",
                       ind = "occrate_ind.q",
                       ret = "occrate_ret.q",
                       apt = "occrate_apt.q"), db = "/href/prod/cre/data/reitcmbs.db")

  rplot.line(tislist,
             Title = "Occupancy Rates",
             Y2lab = "Percent",
             Lwd = c(1.5, 1,1,1,1),
             Lty = c(1,2,1,2,1),
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple"),
             Y2lim = c(85, 100),
             Y2at = seq(85,100,5),
             legend = FALSE,
             legend.text = c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily"),
             legend.x.loc = 2002,
             legend.y.loc = 105,
             footvec = c("Source: NAREIT"))
  legend("top",
         c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily"),
         ncol = 2,
         col = c("black", "blue", "deepskyblue", "forestgreen", "purple"),
         xpd = TRUE,
         inset = 0.025,
         pch = NULL,
         x.intersp = 0.5,
         y.intersp = 1,
         bty = "n",
         cex = .65,
         lty = c(1,2,1,2,1),
         lwd = 2,)
}
