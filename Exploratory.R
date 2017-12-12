#########################
### Michaela Palumbo
### BIOS 6640 Project
### Exploratory Analysis
#########################

# load final dataset
dat <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/FinalData.csv")
dat$X <- NULL # unnecessary X column

#check
length(unique(dat$DISTCODE))
length(unique(dat$District))
# both 142- good!

# load polygon file
library(maptools)
poly1 <- readShapePoly("/Users/Michaela/Documents/CU AMC Fall 2017/BIOS6640/Project/Moz_admin2.shp", IDvar="DISTCODE")

## referenced lectures 14 and 15 R scripts ##

library(EpiWeek)
library(lattice) 
library(ggmap)
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyr)
library(tmap)
library(rgdal)
library(RColorBrewer)
library(sp)
library(latticeExtra) # For layer()

# basic exploratory plots
# examining the weather variables
par(mfrow=c(2,3))
hist(dat$raint, breaks=20, xlab="Weekly tot rainfall", main="", col="blue")
hist(dat$tavg, breaks=20, xlab="Weekly ave temp (C)", main="", col="red")
hist(dat$rh, breaks=20, xlab="Weekly ave humidity", main="", col="green")
hist(dat$sd, breaks=20, xlab="Weekly ave sat vap pres def", main="", col="purple")
hist(dat$psfc, breaks=20, xlab="Weekly ave bar pres", main="", col="yellow")
# can see the weekly total rainfall looks pretty skewed, the others look more normal

# normalized weather plot epiweek 
# create normalized weather variables
dat$normRain <- (dat$raint-mean(dat$raint))/sd(dat$raint)
dat$normTavg <- (dat$tavg-mean(dat$tavg))/sd(dat$tavg)
dat$normrh <- (dat$rh-mean(dat$rh))/sd(dat$rh)
dat$normsd <- (dat$sd-mean(dat$sd))/sd(dat$sd)
dat$normincid <- (dat$incid-mean(dat$incid, na.rm = TRUE))/sd(dat$incid, na.rm = TRUE)
datnona <- na.omit(dat)

# looking at rainfall and temp with incidence
# no lag
par(mfrow=c(1,1))
plot(smooth.spline(datnona$Epiweek, datnona$normRain), type="l", col="blue", ylim=c(-2,2), 
     xlab="Epidemiology week", ylab="Standard deviations", 
     main = "Normalized Weather and Malaria Incidence \n No Lags")
lines(smooth.spline(datnona$Epiweek, datnona$normTavg), type="l", col="red")
lines(smooth.spline(datnona$Epiweek, datnona$normincid), type="l", col="green")
legend("bottomright", c("Rain", "Temp", "Incidence"), 
       text.col = c("blue", "red", "green"), 
       cex = 0.7, bty = "n")

# 2 week lag
datnona$normRain2 <- (datnona$raint2-mean(datnona$raint2))/sd(datnona$raint2)
datnona$normTavg2 <- (datnona$tavg2-mean(datnona$tavg2))/sd(datnona$tavg2)
plot(smooth.spline(datnona$Epiweek, datnona$normRain2), type="l", col="blue", ylim=c(-2,2), 
     xlab="Epidemiology week", ylab="Standard deviations", 
     main = "Normalized Weather and Malaria Incidence \n 2-Week Lag")
lines(smooth.spline(datnona$Epiweek, datnona$normTavg2), type="l", col="red")
lines(smooth.spline(datnona$Epiweek, datnona$normincid), type="l", col="green")
legend("bottomright", c("Rain 2-wk lag", "Temp 2-wk lag", "Incidence"), 
       text.col = c("blue", "red", "green"), 
       cex = 0.7, bty = "n")

# 4 week lag
datnona$normRain4 <- (datnona$raint4-mean(datnona$raint4))/sd(datnona$raint4)
datnona$normTavg4 <- (datnona$tavg4-mean(datnona$tavg4))/sd(datnona$tavg4)
plot(smooth.spline(datnona$Epiweek, datnona$normRain4), type="l", col="blue", ylim=c(-2,2), 
     xlab="Epidemiology week", ylab="Standard deviations", 
     main = "Normalized Weather and Malaria Incidence \n 4-Week Lag")
lines(smooth.spline(datnona$Epiweek, datnona$normTavg4), type="l", col="red")
lines(smooth.spline(datnona$Epiweek, datnona$normincid), type="l", col="green")
legend("bottomright", c("Rain 4-wk lag", "Temp 4-wk lag", "Incidence"), 
       text.col = c("blue", "red", "green"), 
       cex = 0.7, bty = "n")

# 8 week lag
datnona$normRain8 <- (datnona$raint8-mean(datnona$raint8))/sd(datnona$raint8)
datnona$normTavg8 <- (datnona$tavg8-mean(datnona$tavg8))/sd(datnona$tavg8)
plot(smooth.spline(datnona$Epiweek, datnona$normRain8), type="l", col="blue", ylim=c(-2,2), 
     xlab="Epidemiology week", ylab="Standard deviations", 
     main = "Normalized Weather and Malaria Incidence \n 8-Week Lag")
lines(smooth.spline(datnona$Epiweek, datnona$normTavg8), type="l", col="red")
lines(smooth.spline(datnona$Epiweek, datnona$normincid), type="l", col="green")
legend("bottomright", c("Rain 8-wk lag", "Temp 8-wk lag", "Incidence"), 
       text.col = c("blue", "red", "green"), 
       cex = 0.7, bty = "n")


## Splines by week and region (for each year)
## 2010 ##
dat.10 <- subset(dat, Epiyear==2010)
dat.10 <- na.omit(dat.10) # smooth.spline does not like NAs so we remove them

plot(smooth.spline(dat.10$Epiweek[dat.10$Region=="Center"], 
                   dat.10$tavg[dat.10$Region=="Center"], df=4), 
                   type="l", col="red", xlab="Epidemiology week in 2010", 
                   ylab="Ave temperature (Celcius)", 
                   main="2010 Average Temperature \n by Week and Region", 
                   ylim=c(15, 30), lty=5, lwd=2)
lines(smooth.spline(dat.10$Epiweek[dat.10$Region=="Coastal"], 
                    dat.10$tavg[dat.10$Region=="Coastal"], df=4), 
                    type="l", col="blue", lty=4, lwd=2)
lines(smooth.spline(dat.10$Epiweek[dat.10$Region=="Northern"], 
                    dat.10$tavg[dat.10$Region=="Northern"], df=4), 
                    type="l", col="green", lty=3, lwd=2)
lines(smooth.spline(dat.10$Epiweek[dat.10$Region=="Southern"], 
                    dat.10$tavg[dat.10$Region=="Southern"], df=4),
                    type="l", col="purple", lty=2, lwd=2)
legend("bottomright", c("Center", "Coastal", "Northern", "Southern"), 
       text.col = c("blue", "red", "green", "purple"), lty=c(5,4,3,2), 
       col = c("blue", "red", "green", "purple"), cex = 0.58, bty = "n", 
       title.col="black", title="Region", lwd=2)

## 2011 ##
dat.11 <- subset(dat, Epiyear==2011)
dat.11 <- na.omit(dat.11) # smooth.spline does not like NAs so we remove them

plot(smooth.spline(dat.11$Epiweek[dat.11$Region=="Center"], 
                   dat.11$tavg[dat.11$Region=="Center"], df=4), 
     type="l", col="red", xlab="Epidemiology week in 2011", 
     ylab="Ave temperature (Celcius)", 
     main="2011 Average Temperature \n by Week and Region", 
     ylim=c(15, 30), lty=5, lwd=2)
lines(smooth.spline(dat.11$Epiweek[dat.11$Region=="Coastal"], 
                    dat.11$tavg[dat.11$Region=="Coastal"], df=4), 
      type="l", col="blue", lty=4, lwd=2)
lines(smooth.spline(dat.11$Epiweek[dat.11$Region=="Northern"], 
                    dat.11$tavg[dat.11$Region=="Northern"], df=4), 
      type="l", col="green", lty=3, lwd=2)
lines(smooth.spline(dat.11$Epiweek[dat.11$Region=="Southern"], 
                    dat.11$tavg[dat.11$Region=="Southern"], df=4),
      type="l", col="purple", lty=2, lwd=2)
legend("bottomright", c("Center", "Coastal", "Northern", "Southern"), 
       text.col = c("blue", "red", "green", "purple"), lty=c(5,4,3,2), 
       col = c("blue", "red", "green", "purple"), cex = 0.58, bty = "n", 
       title.col="black", title="Region", lwd=2)

## 2012 ##
dat.12 <- subset(dat, Epiyear==2012)
dat.12 <- na.omit(dat.12) # smooth.spline does not like NAs so we remove them

plot(smooth.spline(dat.12$Epiweek[dat.12$Region=="Center"], 
                   dat.12$tavg[dat.12$Region=="Center"], df=4), 
     type="l", col="red", xlab="Epidemiology week in 2012", 
     ylab="Ave temperature (Celcius)", 
     main="2012 Average Temperature \n by Week and Region", 
     ylim=c(15, 30), lty=5, lwd=2)
lines(smooth.spline(dat.12$Epiweek[dat.12$Region=="Coastal"], 
                    dat.12$tavg[dat.12$Region=="Coastal"], df=4), 
      type="l", col="blue", lty=4, lwd=2)
lines(smooth.spline(dat.12$Epiweek[dat.12$Region=="Northern"], 
                    dat.12$tavg[dat.12$Region=="Northern"], df=4), 
      type="l", col="green", lty=3, lwd=2)
lines(smooth.spline(dat.12$Epiweek[dat.12$Region=="Southern"], 
                    dat.12$tavg[dat.12$Region=="Southern"], df=4),
      type="l", col="purple", lty=2, lwd=2)
legend("bottomright", c("Center", "Coastal", "Northern", "Southern"), 
       text.col = c("blue", "red", "green", "purple"), lty=c(5,4,3,2), 
       col = c("blue", "red", "green", "purple"), cex = 0.58, bty = "n", 
       title.col="black", title="Region", lwd=2)

## 2013 ##
dat.13 <- subset(dat, Epiyear==2013)
dat.13 <- na.omit(dat.13) # smooth.spline does not like NAs so we remove them

plot(smooth.spline(dat.13$Epiweek[dat.13$Region=="Center"], 
                   dat.13$tavg[dat.13$Region=="Center"], df=4), 
     type="l", col="red", xlab="Epidemiology week in 2013", 
     ylab="Ave temperature (Celcius)", 
     main="2013 Average Temperature \n by Week and Region", 
     ylim=c(15, 30), lty=5, lwd=2)
lines(smooth.spline(dat.13$Epiweek[dat.13$Region=="Coastal"], 
                    dat.13$tavg[dat.13$Region=="Coastal"], df=4), 
      type="l", col="blue", lty=4, lwd=2)
lines(smooth.spline(dat.13$Epiweek[dat.13$Region=="Northern"], 
                    dat.13$tavg[dat.13$Region=="Northern"], df=4), 
      type="l", col="green", lty=3, lwd=2)
lines(smooth.spline(dat.13$Epiweek[dat.13$Region=="Southern"], 
                    dat.13$tavg[dat.13$Region=="Southern"], df=4),
      type="l", col="purple", lty=2, lwd=2)
legend("bottomright", c("Center", "Coastal", "Northern", "Southern"), 
       text.col = c("blue", "red", "green", "purple"), lty=c(5,4,3,2), 
       col = c("blue", "red", "green", "purple"), cex = 0.58, bty = "n", 
       title.col="black", title="Region", lwd=2)
# all looking same so am just going to do 2014 and 2016 now
# don't plot any 2017 stuff just because 2017 isn't over yet

## 2014 ##
dat.14 <- subset(dat, Epiyear==2014)
dat.14 <- na.omit(dat.14) # smooth.spline does not like NAs so we remove them

plot(smooth.spline(dat.14$Epiweek[dat.14$Region=="Center"], 
                   dat.14$tavg[dat.14$Region=="Center"], df=4), 
     type="l", col="red", xlab="Epidemiology week in 2014", 
     ylab="Ave temperature (Celcius)", 
     main="2014 Average Temperature \n by Week and Region", 
     ylim=c(15, 30), lty=5, lwd=2)
lines(smooth.spline(dat.14$Epiweek[dat.14$Region=="Coastal"], 
                    dat.14$tavg[dat.14$Region=="Coastal"], df=4), 
      type="l", col="blue", lty=4, lwd=2)
lines(smooth.spline(dat.14$Epiweek[dat.14$Region=="Northern"], 
                    dat.14$tavg[dat.14$Region=="Northern"], df=4), 
      type="l", col="green", lty=3, lwd=2)
lines(smooth.spline(dat.14$Epiweek[dat.14$Region=="Southern"], 
                    dat.14$tavg[dat.14$Region=="Southern"], df=4),
      type="l", col="purple", lty=2, lwd=2)
legend("bottomright", c("Center", "Coastal", "Northern", "Southern"), 
       text.col = c("blue", "red", "green", "purple"), lty=c(5,4,3,2), 
       col = c("blue", "red", "green", "purple"), cex = 0.58, bty = "n", 
       title.col="black", title="Region", lwd=2)

## 2016 ##
dat.16 <- subset(dat, Epiyear==2016)
dat.16 <- na.omit(dat.16) # smooth.spline does not like NAs so we remove them

plot(smooth.spline(dat.16$Epiweek, dat.16$normTavg, df=4), 
     type="l", col="red", xlab="Epidemiology week in 2016", 
     ylab="Ave temperature (Celcius)", 
     main="2016 Average Temperature \n by Week and Region", 
     lty=5, lwd=2)
lines(smooth.spline(dat.16$Epiweek, dat.16$normincid, df=4),
      type="l", col="blue", lty = 4, lwd=2)

plot(smooth.spline(dat.16$Epiweek, dat.16$tavg, df=4), 
     type="l", col="red", xlab="Epidemiology week in 2016", 
     ylab="Ave temperature (Celcius)", 
     main="2016 Average Temperature \n by Week and Region", 
     ylim = c(15, 60), lty=5, lwd=2)
lines(smooth.spline(dat.16$Epiweek, dat.16$incid, df=4),
      type="l", col="blue", lty = 4, lwd=2)


lines(smooth.spline(dat.16$Epiweek[dat.16$Region=="Coastal"], 
                    dat.16$tavg[dat.16$Region=="Coastal"], df=4), 
      type="l", col="blue", lty=4, lwd=2)
lines(smooth.spline(dat.16$Epiweek[dat.16$Region=="Northern"], 
                    dat.16$tavg[dat.16$Region=="Northern"], df=4), 
      type="l", col="green", lty=3, lwd=2)
lines(smooth.spline(dat.16$Epiweek[dat.16$Region=="Southern"], 
                    dat.16$tavg[dat.16$Region=="Southern"], df=4),
      type="l", col="purple", lty=2, lwd=2)
legend("bottomright", c("Center", "Coastal", "Northern", "Southern"), 
       text.col = c("blue", "red", "green", "purple"), lty=c(5,4,3,2), 
       col = c("blue", "red", "green", "purple"), cex = 0.58, bty = "n", 
       title.col="black", title="Region", lwd=2)

# repeat these spline plots for the other weather variables (or maybe just rain)
# just do for year 2010 and year 2016 

# 2016 splom plot (looks at pairwise correlations between weather variables)
splom(~dat.16[c("raint", "tavg", "rh", "sd", "psfc")], 
      data=dat.16, varname.cex=.7, cex.axis=.5)
splom(~dat[c("raint", "tavg", "rh", "sd", "psfc")], 
      data=dat, varname.cex=.7, cex.axis=.5)
# looks same as 2016 only just denser because more points

# weather variables over the years
par(mfrow=c(1,1))
boxplot(dat$raint ~ dat$Epiyear, ylim=c(0,20), xlab="Year", ylab="Total weekly rainfall (mm)",
        col="blue", main = "Total Weekly Rainfall Over Years")
boxplot(dat$tavg ~ dat$Epiyear, ylim=c(10,40), xlab="Year", ylab="Ave weekly temperature (C)",
        col="red", main = "Average Weekly Temp (C) Over Years")
boxplot(dat$sd ~ dat$Epiyear, ylim=c(0,30), xlab="Year", 
        ylab="Saturation vapor pressure deficit", col="purple",
        main = "Average Weekly Sat Vap Pres Def Over Years")
boxplot(dat$psfc ~ dat$Epiyear, xlab="Year", ylab="Surface barometric pressure (hPa)", 
        col="yellow", main = "Average Weekly Bar Pressure Over Years")
boxplot(dat$rh ~ dat$Epiyear, xlab="Year", ylab="Relative humidity (%)", col="green",
        main = "Average Weekly Humidity Over Years")

# MAPS #
plot(poly1)

# combining polygon with data
# have to do this for a specific year and week
# try 2016 week 10
dat.16.10 <- subset(dat, Epiyear == 2016 & Epiweek == 10)
rownames(dat.16.10) <- dat.16.10$DISTCODE

# make sure they match
table(dat.16.10$DISTCODE)
table(poly1$DISTCODE)

# now we link polygon file and data
polydat.16.10 <- SpatialPolygonsDataFrame(poly1, dat.16.10)

# basic map for total rainfall in 2016 week 10
spplot(polydat.16.10, "raint", main = "Rainfall Total \n 2016 week 10")

# can make more advanced maps
# different color pallettes to choose from
display.brewer.all() 
# choose color palettes based on weather variable
tempPal <- colorRampPalette(brewer.pal(n = 8, name = "YlOrRd"))(100)
rainPal <- colorRampPalette(brewer.pal(n = 9, name = "YlGnBu"))(100)

# rainfall map 2016 week 10
spplot(polydat.16.10, "raint", col.regions = rainPal, col = "transparent",
       main = "Rainfall Total (mm) \n 2016 week 10")

# temperature map 2016 week 10
spplot(polydat.16.10, "tavg", col.regions = tempPal, col = "transparent",
       main = "Average Temperature (C) \n 2016 week 10")

## repeat these graphs for 2010 week 10 to see the comparison from earliest year
# with last complete year (2016)
dat.10.10 <- subset(dat, Epiyear == 2010 & Epiweek == 10)
rownames(dat.10.10) <- dat.10.10$DISTCODE

# make sure they match
table(dat.16.10$DISTCODE)
table(poly1$DISTCODE)

# now we link polygon file and data
polydat.10.10 <- SpatialPolygonsDataFrame(poly1, dat.10.10)

# rainfall map 2010 week 10
spplot(polydat.10.10, "raint", col.regions = rainPal, col = "transparent",
       main = "Rainfall Total (mm) \n 2010 week 10")

# temperature map 2010 week 10
spplot(polydat.10.10, "tavg", col.regions = tempPal, col = "transparent",
       main = "Average Temperature (C) \n 2010 week 10")



