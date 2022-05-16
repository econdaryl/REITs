source("/href/prod/mortgage/prod/notebook/reits/cap_rates.R")
source("/href/prod/mortgage/prod/notebook/reits/occ_rates.R")
source("/href/prod/mortgage/prod/notebook/reits/acq_disp_tt.R")
source("/href/prod/mortgage/frequent/ex1/reits.R")
#source("/href/prod/mortgage/frequent/ex1.1/sector_reits.R")
source("/href/prod/mortgage/prod/notebook/reits/ffo.R")
source("/href/prod/mortgage/prod/notebook/reits/leverage.R")

require(policyPlot)
setwd("/href/prod/mortgage/prod/notebook/reits")
# prepare pdf for exhibit
ps.prepare("reits.ps")
par(omi = c(0.5,0.5,0.75,0.5), bty = "u")

# space for margins
sep <- 0.05


###############################################################################

# chart 1.1: Nominal Prices of Existing Homes
par(fig = c(0, 1/2, 2/3, 1), new = TRUE)
reits()

###############################################################################

# chart 1.2: Growth of Nominal Prices of Existing Homes
par(fig = c(1/2, 1, 2/3, 1), new = TRUE)
acq_disp()

###############################################################################

# chart 2: Real Prices of Existing Homes
par(fig = c(0, 1/2, 1/3, 2/3 + sep/3), new = TRUE)
occup()

###############################################################################

# chart 2.2: Aggregate Price-Rent Ratio
par(fig = c(1/2, 1, 1/3, 2/3 + sep/3), new = TRUE)
cap_rate()

###############################################################################

# chart 3.1: House Price Overvaluation Measure
par(fig = c(0, 1/2, 0, 1/3 + sep/3), new = TRUE)
ffo()


###############################################################################

# chart 3.2: Median Price-Rent Ratio
par(fig = c(1/2, 1, 0, 1/3), new = TRUE)
leverage()

###############################################################################

# Plot meta-data for entire exhibit.
# makeheader("myfile.pdf")


mtext("REIT Indicators",
      outer = TRUE, side = 3, line = 1, cex = 1.20, adj = 0.5, font = 2)
#mtext(format(Sys.time(), "%b %e, %Y"),
#      outer = TRUE, side = 3, line = 1, cex = 0.75, adj = 1.0)

dev.off()

system("ps2pdf reits.ps")
system("pdftk reits.pdf /href/prod/mortgage/prod/notebook/reits/ex2.pdf /href/prod/mortgage/prod/notebook/reits/ex3.pdf cat output reit_packet.pdf")
system("cp reit_packet.pdf /href/prod/lib/www/html/docs/reit_packet.pdf")

