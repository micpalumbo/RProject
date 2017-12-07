################################
### Michaela Palumbo
### BIOS 6640 Project
### More Cleaning & Merging Data
################################

## IMPORT INCIDENCE DATA

inc <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/incidence.csv")

## IMPORT INTERVENTION DATA

int <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/intervention.csv")

## IMPORT WEATHER DATA

weather <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/weather.csv")
weather$X <- NULL # remove unnecessary X column

## IMPORT POLYGON FILE
library(maptools)
poly1 <- readShapePoly("/Users/Michaela/Documents/CU AMC Fall 2017/BIOS6640/Project/Moz_admin2.shp", IDvar="DISTCODE")

## CREATE LAG VARIABLES FOR WEATHER DATASET

# 2 week lag for epiweek
weather$epiweekl2 <- weather$epiweek + 2
# 4 week lag for epiweek
weather$epiweekl4 <- weather$epiweek + 4
# 8 week lag for epiweek
weather$epiweekl8 <- weather$epiweek + 8

# don't know if need sepearate dataset for each lag 
# weather2 <- weather
# weather2$epiweek <- weather2$epiweek + 2
# names(weather2) <- c("epiweek2", "year2", "raint2", "tavg2", "rh2", "sd2", "psfc2")
# cool way Alyssa did the names:
# names(weather2) <- paste0(names(weather2), "2")

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

datm2 <- merge(datm1, intIRS, by.x = c("Epiyear", "Epiweek", "DISTCODE"),
               by.y = c("IRSyear", "IRSepiWeek", "DISTCODE"), all = TRUE)

# change the intervention indicator variable NAs to 0s
datm2$ITNind <- ifelse(is.na(datm2$ITNind), 0, 1)
datm2$IRSind <- ifelse(is.na(datm2$IRSind), 0, 1)

# MERGE WITH WEATHER DATA
datm3 <- merge(datm2, weather, by.x = c("District", "Epiweek", "Epiyear"),
               by.y = c("district", "epiweek", "year"))

# create an incidence variable
datm3$incid <- datm3$cases/datm3$u5total*1000

## Save merged data as csv so don't have to run all this again ##
write.csv(datm3, "~/Documents/CU AMC Fall 2017/BIOS6640/Project/CleanMerged.csv")


