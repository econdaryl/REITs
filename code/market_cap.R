### Download Nareit market cap by sector
library(tabulizer)
library(dplyr)
library(frb)

info <- commandArgs(trailingOnly=TRUE)
print(info)
puby <- as.numeric(paste0("20", substr(info, 1, 2)))
pubm <- as.numeric(substr(info, 3, 4))
year <- paste0("20", substr(info, 1, 2))
month <- sprintf("%02.0f", as.numeric(substr(info, 3, 4)) - 1)
if (month == "00"){
  year <- as.character(as.numeric(year)-1)
  month <- "12"
}
tisdate <- as.numeric(paste0(year, month, "01"))

url <- paste0("https://www.reit.com/sites/default/files/reitwatch/RW", info, ".pdf")
if (puby == 2008 & pubm > 5) {
  url <- paste0("https://www.reit.com/sites/default/files/media/Portals/0/PDF/RW", info, ".pdf")
} else if (puby == 2008 & pubm <=5){
  url <- paste0("https://www.reit.com/sites/default/files/media/Portals/0/Files/Nareit/htdocs/REITWatch/RW", info, ".pdf")
} else if (puby == 2005 & pubm %in% c(2,5)){
  url <- paste0("https://www.reit.com/sites/default/files/media/Portals/0/Files/Nareit/htdocs/REITWatch/rw", info, ".pdf")
}
download.file(url, "/href/prod/cre/data/quarterly/reits/reitwatch.pdf", method="wget")

tables <- extract_tables("/href/prod/cre/data/quarterly/reits/reitwatch.pdf")
if (puby>2011){
  tbl <- intersect(grep("Implied", tables), grep("Sector", tables))[[1]]
} else if (puby<=2011){
  tbl <- intersect(grep("Capitalization", tables), grep("Sector", tables))[[1]]
}
data <- data.frame(tables[tbl])
sector_col <- grep("Office", data)
if (puby>2011){
  implied_col <- grep("Implied", data) 
} else {
  implied_col <- grep("Capitalization", names(data))[[2]]
}

mktcap_off.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Office"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_ind.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Industrial"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_ret.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Retail"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_res.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Residential"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_apt.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Apartments"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_lod.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Lodging/Resorts"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_hea.m <- tis(as.numeric(gsub(",", "", (data[which(data[,sector_col]=="Health Care"),implied_col]))), start = tisdate, end = tisdate, frequency = 12)
mktcap_all.m <- tis(as.numeric(gsub(",", "", (data[2,implied_col]))), start = tisdate, end = tisdate, frequency = 12)

mktcap_all.m

putfame(c(mktcap_off.m="mktcap_off.m",
          mktcap_ind.m="mktcap_ind.m",
          mktcap_ret.m="mktcap_ret.m",
          mktcap_res.m="mktcap_res.m",
          mktcap_apt.m="mktcap_apt.m",
          mktcap_lod.m="mktcap_lod.m",
          mktcap_hea.m="mktcap_hea.m",
          mktcap_all.m="mktcap_all.m"), db = "reitcmbs")
