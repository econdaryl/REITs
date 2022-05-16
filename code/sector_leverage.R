sector_lev <- function(){
  library(readxl)
  library(readr)
  library(dplyr)
  library(policyPlot)
  prop_tot <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data", skip = 260, n_max = 20) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(prop_tot) <- as.character(unlist(prop_tot[1,]))
  prop_tot <- prop_tot[-1,]
  
  prop_tot <- prop_tot %>%
    mutate_all(as.numeric)
  
  sec_debt <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data",
                         skip = 312, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(sec_debt) <- as.character(unlist(sec_debt[1,]))
  sec_debt <- sec_debt[-1,]
  sec_debt <- sec_debt %>%
    mutate_all(as.numeric)
  
  unsec_debt <- read_excel("/href/prod/cre/data/quarterly/reits/TTracker.xlsx", sheet = "Data",
                           skip = 336, n_max = 19) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)
  
  names(unsec_debt) <- as.character(unlist(unsec_debt[1,]))
  unsec_debt <- unsec_debt[-1,]
  unsec_debt <- unsec_debt %>%
    mutate_all(as.numeric)
  
  off <- 100*tis(sec_debt$Office + unsec_debt$Office, start = 20000101, frequency = 4)/tis(prop_tot$Office, start = 20000101, frequency = 4)
  ind <- 100*tis(sec_debt$Industrial + unsec_debt$Industrial, start = 20000101, frequency = 4)/tis(prop_tot$Industrial, start = 20000101, frequency = 4)
  ret <- 100*tis(sec_debt$Retail + unsec_debt$Retail, start = 20000101, frequency = 4)/tis(prop_tot$Retail, start = 20000101, frequency = 4)
  res <- 100*tis(sec_debt$Apartments + unsec_debt$Apartments, start = 20000101, frequency = 4)/tis(prop_tot$Apartments, start = 20000101, frequency = 4)
  lod <- 100*tis(sec_debt$`Lodging/Resorts` + unsec_debt$`Lodging/Resorts`, start = 20000101, frequency = 4)/window(tis(prop_tot$`Lodging/Resorts`, start = 20000101, frequency = 4), start = 20060401)
  
  write_csv(tis2df(list(off, ind, ret, res, lod)), path = "/href/scratch3/m1dbl01/Eileen/sector_lev.csv")
  
  rplot.line(list(off, ind, ret, res, lod),
             Title = "Leverage Ratio by Property Type",
             Lty = c(2,1),
             Col = c("blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
             Y2lim = c(30, 80),
             Y2int = 10,
             Y2lab = "Percent",
             Freqlab2 = "Quarterly",
             legend = FALSE,
             legend.text = c("Office", "Industrial", "Retail", "Multifamily", "Lodging"),
             legend.y.loc = 90,
             legend.x.loc = 2015,
             footvec = c("Note: Leverage ratio calculated as total debt divided by undepreciated book value of total property holdings.",
                         "Source: Nareit"))
  
  legend("topright",
         c("Office", "Industrial", "Retail", "Multifamily", "Lodging"),
         ncol = 2,
         col = c("blue", "deepskyblue", "forestgreen", "purple", "goldenrod"),
         xpd = TRUE,
         inset = 0.025,
         pch = NULL,
         x.intersp = 0.5,
         y.intersp = 1,
         bty = "n",
         cex = .65,
         lty = c(2, 1, 2, 1, 2),
         lwd = 2,)

}
