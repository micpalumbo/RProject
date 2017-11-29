###############################
### Michaela Palumbo
### BIOS 6640 Project
### Importing and Merging Data
###############################

## IMPORT INCIDENCE DATA

inc <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/incidence.csv")

## IMPORT INTERVENTION DATA

int <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/intervention.csv")

## IMPORT WEATHER DATA

# looked at lec9 R script notes for getting data from website

zam <- read.table("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/ALTO_MOLOCUE_ZAM_fldas_daily_20100101-20170724.txt",
                  skip=3, header = FALSE, sep ='', stringsAsFactors = FALSE, dec=".")
colnames(zam) <- c("year", "mo", "day", "raint", "tavg", "rh", "sd", "psfc")

# the website http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/
# has lots of datasets.. not sure which to use that has the weather data

## MERGING DATASETS

# code for reading shp file in lec15 R script notes
