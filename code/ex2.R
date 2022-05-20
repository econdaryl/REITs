require(ggplot2)
require(gridExtra)

setwd("/href/prod/cre/reits/REITs/code")

source("caprate_1q.R")
source("div_yld.R")

pdf("../charts/ps2.pdf", onefile = TRUE)
grid.arrange(caprate_1q(), div_yld(), 
             #caprate_mktcap(), caprate_mktcap_treas(), sector_reits(),
             ncol=1, nrow=2)
dev.off()