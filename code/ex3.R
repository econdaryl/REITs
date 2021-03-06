require(ggplot2)
require(gridExtra)

setwd("/href/prod/cre/reits/REITs/code")

source("tot_return.R")

pdf("../charts/ps3.pdf", onefile = TRUE)
grid.arrange(tot_return(), tot_return_20(), 
             #caprate_mktcap(), caprate_mktcap_treas(), sector_reits(),
             ncol=1, nrow=2)
dev.off()