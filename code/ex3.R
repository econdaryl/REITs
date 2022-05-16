library(policyPlot)

setwd("/href/prod/mortgage/prod/notebook/reits")

source("caprate_1q.R")
source("div_yld.R")
source("tot_return.R")

ps.prepare("ex3.ps")

# Mortgage rate
par(fig = c(0, 1/2, 2/3, 1), new = TRUE)
caprate_1q()

# REITS
par(fig = c(1/2, 1, 2/3, 1), new = TRUE)
div_yld()

# RRE
par(fig = c(0, 2/3, 1/3, 2/3), new = TRUE)
tot_return()

# CLD
par(fig = c(2/3, 1, 1/3, 2/3), new = TRUE)
tot_return_20()

# MF
par(fig = c(1/2, 1, 0, 1/3), new = TRUE)
#caprate_treas()

# NFNR
par(fig = c(1/2, 1, 0, 1/3), new = TRUE)



mtext("",
      outer = TRUE, side = 3, line = 1, cex = 1.20, adj = 0.5, font = 2)

dev.off()
system("ps2pdf ex3.ps")
