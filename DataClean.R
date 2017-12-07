###############################
### Michaela Palumbo
### BIOS 6640 Project
### Importing and Cleaning Data
###############################

## IMPORT INCIDENCE DATA

inc <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/incidence.csv")

## IMPORT INTERVENTION DATA

int <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/intervention.csv")

## IMPORT WEATHER DATA

# looked at lec9 R script notes for getting data from website

#zam <- read.table("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/ALTO_MOLOCUE_ZAM_fldas_daily_20100101-20170724.txt",
                 # skip=3, header = FALSE, sep ='', stringsAsFactors = FALSE, dec=".")
#colnames(zam) <- c("year", "mo", "day", "raint", "tavg", "rh", "sd", "psfc")

# the website http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/
# has lots of datasets for the weather data, need to read in them all

# adapted code from:
# https://stackoverflow.com/questions/15954463/read-list-of-file-names-from-web-into-r
# http://www.columbia.edu/~cjd11/charles_dimaggio/DIRE/styled-4/styled-6/code-13/
# https://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r
install.packages("XML")
library(XML)

# base url for all txt files
# htmlParse turns URL into an R object
url <- htmlParse("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/")
class(url)
# get specific links that match criterion
links <- xpathSApply(url, "//a/@href")
# recognizing patterns that have the end of the txt file name we're looking for
wanted <- links[grepl("*_fldas_daily_20100101-20170724.txt", links)]
# paste base url followed by the rest of the link we wante for each text file
GetMe <- paste("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/", wanted, sep = "")
# GetMe is now a character vector of all the urls to each specific .txt file we need

# read in each of the data frames from each text file
# note lapply creates a list object so will have list of all the 142 dataframes
dfs <- lapply(seq_along(GetMe),
              function(x) read.table(GetMe[x], skip=3, header = FALSE, sep = '', stringsAsFactors = FALSE, dec = "."))

# add column names to the data frames
for(i in seq_along(dfs)){
  colnames(dfs[[i]]) <- c("year", "mo", "day", "raint", "tavg", "rh", "sd", "psfc")
}

# get list of district names from links to datasets for each of the 142 districts
distlist <- as.list(gsub("_.{3}_fldas_daily_20100101-20170724.txt", "", links))
# first 6 elements returned are not district names
distlist <- distlist[6:147] # now have list of 142 district names
distlist <- gsub("_", " ", distlist) # replace underscores with space, now it is a character vector

# add column for district name using Map to apply to all dataframes in list
dfs <- Map(cbind, dfs, District = distlist)

# create new date variable by combining day, month, and year
for(i in seq_along(dfs)){
  dfs[[i]]$date <- as.Date(paste(dfs[[i]]$year, dfs[[i]]$mo, dfs[[i]]$day, sep = "/"), format = "%Y/%m/%d")
}

# create epiweek variable
library(data.table)
for(i in seq_along(dfs)){
  dfs[[i]]$epiweek <- week(dfs[[i]]$date)
}

# creating new variables that summarize the variables on a weekly basis
library(plyr)
byweek <- NULL
for(i in seq_along(dfs)){
  byweek[[i]] <- ddply(dfs[[i]], .(epiweek, year), summarize, raint=sum(raint), tavg=mean(tavg),
                       rh=mean(rh), sd=mean(sd), psfc=mean(psfc))
}

# add the district names column
byweek <- Map(cbind, byweek, district = distlist)

# so we now have 142 byweek dataframes 
# one for each districts dataset that contain the weekly variables
# this is what we want to combine with the other datasets inc and int
# first have to combine the 142 byweek datasets into one dataset
weather <- do.call("rbind", byweek)
head(weather)

length(unique(int$DISTCODE)) # 135 district codes, some are repeats? 
length(unique(inc$DISTCODE)) # 142 distict codes, 
length(unique(weather$district)) #142 districts - good!


## Save combined weather data so I don't have to run this all again every time ##
write.csv(weather, "~/Documents/CU AMC Fall 2017/BIOS6640/Project/weather.csv")

## IMPORT POLYGON FILE 
library(maptools)
poly1 <- readShapePoly("/Users/Michaela/Documents/CU AMC Fall 2017/BIOS6640/Project/Moz_admin2.shp", IDvar="DISTCODE")





