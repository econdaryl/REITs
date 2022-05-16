library(policyPlot)

setwd("/href/prod/mortgage/prod/notebook/reits")

source("sector_reits.R")
source("implied_cap_treas_rate.R")
source("sector_leverage.R")
source("caprate_mktcap.R")
source("caprate_mktcap_treas.R")

ps.prepare("ex2.ps")

# Mortgage rate
par(fig = c(0, 1, 2/3, 1), new = TRUE)
sector_reits()

# REITS
par(fig = c(0, 1/2, 1/3, 2/3), new = TRUE)
imp_cap_rate()

# RRE
par(fig = c(1/2, 1, 1/3, 2/3), new = TRUE)
sector_lev()

# CLD
par(fig = c(0, 1/2, 0, 1/3), new = TRUE)
caprate()

# MF
par(fig = c(1/2, 1, 0, 1/3), new = TRUE)
caprate_treas()

# NFNR
par(fig = c(1/2, 1, 0, 1/3), new = TRUE)



mtext("",
      outer = TRUE, side = 3, line = 1, cex = 1.20, adj = 0.5, font = 2)

dev.off()
system("ps2pdf ex2.ps")
