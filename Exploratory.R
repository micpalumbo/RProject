#########################
### Michaela Palumbo
### BIOS 6640 Project
### Exploratory Analysis
#########################

# load cleaned and merged final dataset
dat <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/CleanMerged.csv")
dat$X <- NULL # unnecessary X column

# load polygon file
poly1 <- readShapePoly("/Users/Michaela/Documents/CU AMC Fall 2017/BIOS6640/Project/Moz_admin2.shp", IDvar="DISTCODE")

## see lec 15 R script notes on spatial graphics and mapping

## see lec 14 for graphics in R and examples of exploratory analysis plots