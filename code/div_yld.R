div_yld <- function(){
  data <- getfame(c("div_yld_equity.m", "div_yld_off.m", "div_yld_ind.m",
                    "div_yld_res.m", "div_yld_lod.m", "div_yld_ret.m"), db = "reitcmbs")
  all <- data$div_yld_equity.m
  off <- data$div_yld_off.m
  ind <- data$div_yld_ind.m
  res <- data$div_yld_res.m
  lod <- data$div_yld_lod.m
  ret <- data$div_yld_ret.m
  
  all <- window(all, start = 19930101)
  
  tislist <- list(all, off, ind, ret, res, lod)
  
  rplot.line(tislist,
             Title = "Dividend Yield",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(0, 15),
             Y2int = 3,
             legend = FALSE,
             legend.text = c("All Equity REITs", "Office", "Industrial", "Retail", "Multifamily", "Lodging"),
             legend.y.loc = 18,
             legend.x.loc = 2010,
             footvec = c("Source: NAREIT"))
  legend("topleft",
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