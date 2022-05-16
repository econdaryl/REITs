cap_rate <- function(){
  library(policyPlot)

  tislist <- getfame(c(all = "caprate_all.q",
            off = "caprate_off.q",
            ind = "caprate_ind.q",
            ret = "caprate_ret.q",
            apt = "caprate_apt.q",
            lod = "caprate_lod.q"), db = "/href/prod/cre/data/reitcmbs.db")

  rplot.line(tislist,
             Title = "Implied Capitalization Rate",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(-2, 18),
             legend = FALSE,
             legend.text = c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
             legend.y.loc = 18,
             legend.x.loc = 2010,
             footvec = c("Source: NAREIT"))
  legend("top",
         c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
         ncol = 2,
         col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
         xpd = TRUE,
         inset = 0.025,
         pch = NULL,
         x.intersp = 0.5,
         y.intersp = 1,
         bty = "n",
         cex = .65,
         lty = c(1, 2, 1, 2, 1, 2),
         lwd = 2,)
}
