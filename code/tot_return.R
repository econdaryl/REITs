tot_return <- function(){
  data <- getfame(c("tot_ret_equity.m", "tot_ret_off.m", "tot_ret_ind.m",
                    "tot_ret_res.m", "tot_ret_lod.m", "tot_ret_ret.m"), db = "reitcmbs")
  all <- 100*data$tot_ret_equity.m/as.numeric(window(data$tot_ret_equity.m, start=20070101, end = 20070101))
  off <- 100*data$tot_ret_off.m/as.numeric(window(data$tot_ret_off.m, start=20070101, end = 20070101))
  ind <- 100*data$tot_ret_ind.m/as.numeric(window(data$tot_ret_ind.m, start=20070101, end = 20070101))
  res <- 100*data$tot_ret_res.m/as.numeric(window(data$tot_ret_res.m, start=20070101, end = 20070101))
  lod <- 100*data$tot_ret_lod.m/as.numeric(window(data$tot_ret_lod.m, start=20070101, end = 20070101))
  ret <- 100*data$tot_ret_ret.m/as.numeric(window(data$tot_ret_ret.m, start=20070101, end = 20070101))
  
  all <- window(all, start = 20000101)
  off <- window(off, start = 20000101)
  ind <- window(ind, start = 20000101)
  ret <- window(ret, start = 20000101)
  res <- window(res, start = 20000101)
  lod <- window(lod, start = 20000101)
  
  tislist <- list(all, off, ind, ret, res, lod)
  
  rplot.line(tislist,
             Title = "Total Return Index",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Y2lab = "Jan. 2007 = 100",
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(0, 350),
             Y2int = 50,
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

tot_return_20 <- function(){
  data <- getfame(c("tot_ret_equity.m", "tot_ret_off.m", "tot_ret_ind.m",
                    "tot_ret_res.m", "tot_ret_lod.m", "tot_ret_ret.m"), db = "reitcmbs")
  all <- 100*data$tot_ret_equity.m/as.numeric(window(data$tot_ret_equity.m, start=20200101, end = 20200101))
  off <- 100*data$tot_ret_off.m/as.numeric(window(data$tot_ret_off.m, start=20200101, end = 20200101))
  ind <- 100*data$tot_ret_ind.m/as.numeric(window(data$tot_ret_ind.m, start=20200101, end = 20200101))
  res <- 100*data$tot_ret_res.m/as.numeric(window(data$tot_ret_res.m, start=20200101, end = 20200101))
  lod <- 100*data$tot_ret_lod.m/as.numeric(window(data$tot_ret_lod.m, start=20200101, end = 20200101))
  ret <- 100*data$tot_ret_ret.m/as.numeric(window(data$tot_ret_ret.m, start=20200101, end = 20200101))
  
  all <- window(all, start = 20190701)
  off <- window(off, start = 20190701)
  ind <- window(ind, start = 20190701)
  ret <- window(ret, start = 20190701)
  res <- window(res, start = 20190701)
  lod <- window(lod, start = 20190701)
  
  tislist <- list(all, off, ind, ret, res, lod)
  
  rplot.line(tislist,
             Title = "Total Return Index",
             Col = c("black", "blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Y2lab = "Jan. 2020 = 100",
             Lty = c(1, 2, 1, 2, 1, 2),
             Lwd = c(1.5, 0.75, 0.75, 0.75, 0.75, 0.75),
             Y2lim = c(40, 140),
             Y2int = 20,
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
         cex = .45,
         lty = c(1, 3, 1, 3, 1, 3),
         lwd = 2,)
}