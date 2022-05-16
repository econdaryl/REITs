# francisco
# last edit: 2/6/18 - to account for Jan 2018 numbers appearing in the 2017 pdf
# last edit: 03/15/19 - a new column appeared!
# REITS Capital Offerings update
# what this script does
# 1. grab current YTD capital offerings pdf from the reit.com site
# 2. parse that into a table
# 3. put that into fame, update appropriate series

suppressPackageStartupMessages(library(pdftools))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(policyPlot))
suppressPackageStartupMessages(library(grDevices))

# enter yymm to find the right pdf to read in
#yearmon <- readline(prompt = "Please enter the date of the most recently downloaded Historical Offerings (HistOff) file in the following format: yymm ")

# Instead of the R program prompting you for input, as above. now we make the shell script prompt you for the most recent update 
# trailingOnly= TRUE means that R takes the arguments from the shell script and uses them to define the yearmon object.
# yearmon <- 1903 # For testing 
yearmon <- commandArgs(trailingOnly=TRUE)

dat <- pdf_text(paste0("/href/prod/mortgage/publications/REITS/HistOff", yearmon,".pdf"))

# break pdf apart line by line

lines <- strsplit(dat, "\n")

# this is how you access elements inside of 1 element list: lines[[1]][1]
# find publication date
pub_date <- lines[[1]][2]
pub_date <- trimws(pub_date)
pub_date <- lubridate::mdy(pub_date)

# find monthly totals table (its the last table on the page)
monthly <- grep("Monthly Totals", lines[[1]])
# using Source as the ending line
stop <- grep("Source", lines[[1]])

# trim ws and subset using monthly and stop as start/stop values
work <- trimws(lines[[1]][(monthly+1):(stop-1)])
# trim ws, then replace lots of spaces with just 1 space, 
# then strsplit on 1 space, to separate all elements
work <- strsplit(gsub("\\s+", " ", str_trim(work)), ' ')

# find the years
years <- grep("20[0-9][0-9]:", work, perl = TRUE)
# extract the years
year1 <- as.numeric(substring(work[[years[1]]][1],1,4))

if(length(years) > 2) {
  year2 <- as.numeric(substring(work[[years[2]]][1],1,4))
  year3 <- as.numeric(substring(work[[years[3]]][1],1,4))
  } else {
  year2 <- as.numeric(substring(work[[years[2]]][1],1,4))
  }
  
# strip off the year so we can paste it back later
#### Here work[[years[1]]][2:13] refers to the number of elements after the year.
#### IMPORTANT: If the number of columns changes, then we have to account for that
#### by changing that last number. 
#### sometimes companies also omit placeholders so we have to reinsert them 
# this is the line that does that
# work[[length(work)]] <- append(x = work[[length(work)]], values = "-", after = length(work[[length(work)]])-2)

work[[years[1]]] <- work[[years[1]]][2:13] 

if(length(years) > 2){
  work[[years[2]]] <- work[[years[2]]][2:13]
  work[[years[3]]] <- work[[years[3]]][2:13]
  } else {
  work[[years[2]]] <- work[[years[2]]][2:13]
  }

# put observations into a data.frame
yearone <- as.data.frame(t(as.data.table(work[years[1]:(years[2]-1)])), stringsAsFactors = FALSE) #, row.names = FALSE)

if(length(years) > 2) {
  yeartwo <- as.data.frame(t(as.data.table(work[years[2]:(years[3]-1)])), stringsAsFactors = FALSE) #, row.names = FALSE)
  yearthree <- as.data.frame(t(as.data.table(work[years[3]:length(work)])), stringsAsFactors = FALSE) #, row.names = FALSE) 
  } else {
  yeartwo <- as.data.frame(t(as.data.table(work[years[2]:length(work)])), stringsAsFactors = FALSE) #, row.names = FALSE)
  }
# had to comment out row.names = FALSE for this to work. pay attention because this could be an issue. 

# paste back on the year
yearone <- cbind(as.data.frame(rep(year1, nrow(yearone)), row.names = NULL), yearone)

if(length(years) > 2){
  yeartwo <- cbind(as.data.frame(rep(year2, nrow(yeartwo)), row.names = NULL), yeartwo)
  yearthree <- cbind(as.data.frame(rep(year3, nrow(yearthree)), row.names = NULL), yearthree)
  } else {
  yeartwo <- cbind(as.data.frame(rep(year2, nrow(yeartwo)), row.names = NULL), yeartwo)
  }


# name columns 
# 03/2019 - inserted new column - atms 
# dont think we're going to use it
names(yearone) <- c("year","mon","yr.n.ipo","yr.tot.capital","m.n.ipo","ipo.capital",
                    "eq.n.com","eq.cap.com","eq.n.pref","eq.cap.pref","atms", "dt.n.unsec","dt.cap.unsec")
if(length(years) > 2) {
  names(yeartwo) <- c("year","mon","yr.n.ipo","yr.tot.capital","m.n.ipo","ipo.capital",
                      "eq.n.com","eq.cap.com","eq.n.pref","eq.cap.pref","atms", "dt.n.unsec","dt.cap.unsec")
  names(yearthree) <- c("year","mon","yr.n.ipo","yr.tot.capital","m.n.ipo","ipo.capital",
                        "eq.n.com","eq.cap.com","eq.n.pref","eq.cap.pref","atms","dt.n.unsec","dt.cap.unsec")
  } else {
  names(yeartwo) <- c("year","mon","yr.n.ipo","yr.tot.capital","m.n.ipo","ipo.capital",
                      "eq.n.com","eq.cap.com","eq.n.pref","eq.cap.pref","atms","dt.n.unsec","dt.cap.unsec")
  }


# combine all data.frames
if(length(years) > 2) {
  reits <- rbind(yearone, yeartwo, yearthree)
  } else {
  reits <- rbind(yearone, yeartwo)
  }
    

# set aside years and mons for some weird wrangling
time <- reits[,1:2]
date <- as.Date(paste(time$year, time$mon,"01", sep = "-"), format = "%Y-%b-%d")

# remove commas from observations
reits <- as.data.frame(apply(reits[3:13], 2, function(y) as.numeric(gsub(",", "", y))))

# put the years back on 
reits <- cbind(date, reits)


# total equity number = number of preferred shares + number of common shares issued
# will be set to FAME series: r.seceqd_n.m 
reits$total.n.equity <- reits$eq.n.com + reits$eq.n.pref

# total equity capital raised = common shares capital raised + pref shares capital raised
# will be set to FAME series: r.seceq_n.m 
reits$total.eq.cap.raised <- reits$eq.cap.com + reits$eq.cap.pref

# total number of reit unsecured debt offerings is defined above as dt.n.unsec
# will be set to FAME series: r.usecdtd_n.m

# total capital raised from offerings of unsecured debt is defined above as dt.cap.unsec
# will be set to FAME series: r.usecdt_n.m

# total number of secured (by MBS) debt offerings is no longer given.
# will be set to FAME series: r.mbsd_n.m 

# total capital raised from offerings of secured (by MBS) debt offerings is no longer given.
# will be set to FAME series: r.mbs_n.m 

# total number of offerings is defined above as dt.n.unsec
# will be set to FAME series: r.totdtd_n.m

# total capital raised from offerings of debt issuance is defined above as dt.cap.unsec
# will be set to FAME series: r.totdt_n.m 

# total offerings of reit equity is defined as total.n.equity + ipo offerings
# will be set to FAME series: r.toteqd_n.m

# total capital raised from equity issuance is defined as total.eq.cap.raised + ipo cap
# will be set to FAME series: r.toteq_n.m
# save/rename stuff to be in the right format for to put into fame
r.ipod_n.m <- as.tis(reits$m.n.ipo, start = reits$date[1], frequency = 12)
r.ipo_n.m <- as.tis(reits$ipo.capital, start = reits$date[1], frequency = 12)
r.seceqd_n.m <- as.tis(reits$total.n.equity, start = reits$date[1], frequency = 12)
r.seceq_n.m <- as.tis(reits$total.eq.cap.raised, start = reits$date[1], frequency = 12)
r.usecdtd_n.m <- as.tis(reits$dt.n.unsec, start = reits$date[1], frequency = 12)
r.usecdt_n.m <- as.tis(reits$dt.cap.unsec, start = reits$date[1], frequency = 12)
# r.mbsd_n.m  
# r.mbs_n.m 
r.totdtd_n.m <- as.tis(reits$dt.n.unsec, start = reits$date[1], frequency = 12)
r.totdt_n.m  <- as.tis(reits$dt.cap.unsec, start = reits$date[1], frequency = 12)
r.toteqd_n.m <- as.tis(reits$m.n.ipo + reits$total.n.equity, start = reits$date[1], frequency = 12)
r.toteq_n.m <- as.tis(reits$ipo.capital + reits$total.eq.cap.raised, start = reits$date[1], frequency = 12)

reits2fame <- cbind(r.ipod_n.m, r.ipo_n.m, r.seceqd_n.m, r.seceq_n.m, r.usecdtd_n.m,
          r.usecdt_n.m, r.totdtd_n.m, r.totdt_n.m,
          r.toteqd_n.m, r.toteq_n.m)

print(reits2fame)

 #can add these to cbind if needed: r.mbsd_n.m, r.mbs_n.m.  Would have to change code above as well to ensure that these objects exist

# put fame statement assigns all the variables to update to a series mapped above
putfame(reits2fame, db = "reitcmbs")




