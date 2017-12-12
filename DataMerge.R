################################
### Michaela Palumbo
### BIOS 6640 Project
### More Cleaning & Merging Data
################################

## IMPORT INCIDENCE DATA

inc <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/incidence.csv")
summary(inc)
length(unique(inc$DISTCODE)) # 142 unique
length(unique(inc$District)) # 142 levels here
str(inc$District) # factor with 142 levels
levels(inc$District)
# the names of these factor levels have dashes like word-word or word - word
# that won't match with weather district names so have to fix this
distnames <- as.list(levels(inc$District))
distnames <- gsub("-", " ", distnames)
distnames <- gsub("  ", " ", distnames)
distnames <- gsub("  ", " ", distnames)
levels(inc$District) <- distnames
levels(inc$District) # now formatting of district names will match with weather

## IMPORT INTERVENTION DATA

int <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/intervention.csv")
summary(int)
length(unique(int$DISTCODE)) # only 134 unique district codes here (because simulate)
# or because not every district had intervention 

## IMPORT WEATHER DATA

weather <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/weather.csv")
weather$X <- NULL # remove unnecessary X column
length(unique(weather$district)) # this data reads in with 142 districts
str(weather$district) # factor with 142 levels


## IMPORT POLYGON FILE
library(maptools)
poly1 <- readShapePoly("/Users/Michaela/Documents/CU AMC Fall 2017/BIOS6640/Project/Moz_admin2.shp", IDvar="DISTCODE")

## CREATE LAG VARIABLES FOR WEATHER DATASET

# think this way I tried didn't keep the lag variables I needed when merging
# 2 week lag for epiweek
# weather$epiweekl2 <- weather$epiweek + 2
# 4 week lag for epiweek
# weather$epiweekl4 <- weather$epiweek + 4
# 8 week lag for epiweek
# weather$epiweekl8 <- weather$epiweek + 8

# ----------------------------------------------------- #
# don't know if need sepearate dataset for each lag (what I ended up doing)
# might need to uncomment this code and comment out the code above and create new merged data
weather2 <- weather
weather2$epiweek <- weather2$epiweek + 2
names(weather2) <- c("epiweek2", "year2", "raint2", "tavg2", "rh2", "sd2", "psfc2", "district2")
# cool way Alyssa did the names:
# names(weather2) <- paste0(names(weather2), "2")

weather4 <- weather
weather4$epiweek <- weather4$epiweek + 4
names(weather4) <- c("epiweek4", "year4", "raint4", "tavg4", "rh4", "sd4", "psfc4", "district4")

weather8 <- weather
weather8$epiweek <- weather8$epiweek + 8
names(weather8) <- c("epiweek8", "year8", "raint8", "tavg8", "rh8", "sd8", "psfc8", "district8")
# ------------------------------------------------------------ #

## MERGE INTERVENTION AND INCIDENCE DATA

# create indicator variable for weather ITNS were given as intervention 
intITN <- int[, c(1, 4:5)]
intITN$ITNind <- ifelse(is.na(intITN$ITNyear), 0, 1)

# create indicator variable for weather IRS was given as intervention
intIRS <- int[, c(1:3)]
intIRS$IRSind <- ifelse(is.na(intIRS$IRSyear), 0, 1)

# merge these updatated intervention datasets with incidence datasets
datm1 <- merge(inc, intITN, by.x = c("Epiyear", "Epiweek", "DISTCODE"),
               by.y = c("ITNyear", "ITNepiWeek", "DISTCODE"), all = TRUE)

length(unique(datm1$DISTCODE)) # 142 district codes
length(unique(datm1$District)) # 143 districts? probably an NA added on?
unique(datm1$District) # yes there was an NA added in 

datm2 <- merge(datm1, intIRS, by.x = c("Epiyear", "Epiweek", "DISTCODE"),
               by.y = c("IRSyear", "IRSepiWeek", "DISTCODE"), all = TRUE)
length(unique(datm2$DISTCODE)) # 142 district codes
length(unique(datm1$District)) # 143 (142 districts + NA)
str(datm2$District) # factor with 142 levels

# change the intervention indicator variable NAs to 0s
datm2$ITNind <- ifelse(is.na(datm2$ITNind), 0, 1)
datm2$IRSind <- ifelse(is.na(datm2$IRSind), 0, 1)

# MERGE WITH WEATHER DATA
# datm3 <- merge(datm2, weather, by.x = c("District", "Epiweek", "Epiyear"),by.y = c("district", "epiweek", "year"))

#-------------------------------------------------------#
# Might need to merge separate lagged weather datasets
# This is what I ended up doing
# this first merge in the code below is where we lose the levels of district and distcode
# try merging differently
datmtest <- merge(datm2, weather, by.x = c("District", "Epiweek", "Epiyear"),
               by.y = c("district", "epiweek", "year"))
length(unique(datmtest$DISTCODE)) # 142
length(unique(datmtest$District)) # 142
unique(datmtest$District) # didn't match with districts that had NA for the District
str(datmtest$District) # factor w/ 142 levels

datm3 <- merge(datmtest, weather2, by.x = c("District", "Epiweek", "Epiyear"),
               by.y = c("district2", "epiweek2", "year2"))
length(unique(datm3$District)) # 142
length(unique(datm3$DISTCODE)) # 142 
str(datm3$District)

datm4 <- merge(datm3, weather4, by.x = c("District", "Epiweek", "Epiyear"),
               by.y = c("district4", "epiweek4", "year4"))
length(unique(datm4$District))
length(unique(datm4$DISTCODE))
# both 142

datm5 <- merge(datm4, weather8, by.x = c("District", "Epiweek", "Epiyear"),
               by.y = c("district8", "epiweek8", "year8"))
length(unique(datm5$District))
length(unique(datm5$DISTCODE))
# both 142!

#--------------------------------------------------------#

# create an incidence variable
datm5$incid <- datm5$cases/datm5$u5total*1000


## Save merged data so don't have to run all this again ##
write.csv(datm5, "~/Documents/CU AMC Fall 2017/BIOS6640/Project/CleanMerged.csv")


