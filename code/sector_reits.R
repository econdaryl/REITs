sector_reits <- function() {
  library(policyPlot)
  library(frb)
  #setcmpar()
  print("check1")
  data <- getfame(c(Healthcare = "eqreit_health_index",
                    Apartments = "eqreit_apt_index",
                    Industrial = "eqreit_ind_index",
                    Lodgings   = "eqreit_lodge_index",
                    Residential= "eqreit_res_index",
                    Retail     = "eqreit_ret_index",
                    Office     = "eqreit_off_index"), db = "/href/prod/cre/data/reitcmbs.db", start = 20200105)
  print("check2")
  ## reindex to feb 14, 2020
  hc.temp <- window(data$Healthcare, start = 20200214, end = 20200214)[1]
  rs.temp <- window(data$Residential, start = 20200214, end = 20200214)[1]
  ap.temp <- window(data$Apartments, start = 20200214, end = 20200214)[1]
  rt.temp <- window(data$Retail, start = 20200214, end = 20200214)[1]
  lg.temp <- window(data$Lodgings, start = 20200214, end = 20200214)[1]
  of.temp <- window(data$Office, start = 20200214, end = 20200214)[1]
  in.temp <- window(data$Industrial, start = 20200214, end = 20200214)[1]
  Healthcare <- 100*(data$Healthcare/hc.temp)
  Residential<- 100*(data$Residential/rs.temp)
  Apartments <- 100*(data$Apartments/ap.temp)
  Retail     <- 100*(data$Retail/rt.temp)
  Lodgings   <- 100*(data$Lodgings/lg.temp)
  Office     <- 100*(data$Office/of.temp)
  Industrial <- 100*(data$Industrial/in.temp)
  plotdata <- list(Healthcare, Residential,
                   Apartments,
                   Retail, Lodgings, Office,
                   Industrial)
  rplot.line(plotdata,
             #allow_gaps = TRUE,
             Col = c("black", "red",
                     "blue", "green3", "yellow3", "darkorange1", "purple"),
             Lty = c(1,2,3),
             legend = FALSE,
             Monthgaps = 1,
             #legend.x.loc = 2020.05,
             #legend.y.loc = 115,
             legend.text.col = "black",
             Title = "Sector Equity REIT Indices",
             Y2lab = "Feb. 14 = 100",
             footvec = "Source: Bloomberg",
             Freqlab1 = "Daily")
  legend(x = 2020, y = 140, pt.cex = 0.75, 
         ncol = 2,
         legend = c("Healthcare", "Residential",
                    "Apartments", "Retail", "Lodgings",
                    "Office", "Industrial"),
         cex = style$legend.cex,
         col = c("black", "red",
                 "blue",
                 "green3", "yellow3",
                 "darkorange1", "purple"),
         border = NULL,
         lty = c(1,2,3),
         bty = 'n')


}

# ps.prepare("sector_reits.ps")
# par(mar=c(1,1,1,1))
# par(oma = c(1,1,4,1.25), bty = "u")
# # separator for margins
# sep <- 0.05
# ###############################################################################
# # chart 1.1:
# par(fig = c(0, 1/2, 2/3, 1), new = TRUE)
# sector_reits()
# ###############################################################################
# # chart 1.2:
# # par(fig = c(1/2, 1, 2/3, 1), new = TRUE)
# # rlt()
# ###############################################################################
# # chart 2.1:
# # par(fig = c(0, 1/2 - sep, 1/3 + sep, 2/3 - sep/2), new = TRUE)
# ###############################################################################
# # chart 2.2:
# ###############################################################################
# # chart 3.1:
# #par(fig = c(0, 1/2, 0, 1/3 - sep/2), new = TRUE)
# ###############################################################################
# # chart 3.2:
# #par(fig = c(1/2, 1 - sep/3, 0, 1/3 - sep/2), new = TRUE)
# ###############################################################################
#
# dev.off()
#
# system("ps2pdf sector_reits.ps")
