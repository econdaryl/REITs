require(ggplot2)
require(gridExtra)

setwd("/href/prod/cre/reits/REITs/code")

source("sector_reits.R")
source("implied_cap_treas_rate.R")
source("sector_leverage.R")
source("caprate_mktcap.R")
source("caprate_mktcap_treas.R")

pdf("../charts/ps1.pdf", onefile = TRUE)
grid.arrange(sector_reits(), imp_cap_rate(), sector_lev(), caprate_mktcap(), caprate_mktcap_treas(), ncol=2)
dev.off()