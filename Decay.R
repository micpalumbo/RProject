###########################################
### Michaela Palumbo
### BIOS 6640 Project
### Inserting Decaying Intervention Effects
###########################################

# load cleaned and merged dataset
datm <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/CleanMerged.csv")
datm$X <- NULL # unnecessary X column
summary(datm)
# why does my merged data only have 131 levels now instead of 142?
# 43492
sum(datm$IRSind) # 35
sum(datm$ITNind) # 114

## creating decay variables ##

## adapted from code provide by Katie in the file ITNdecayExample2.R ##

## create time column to index all weeks measured from 2010 to 2017
datm$time <- (datm$Epiyear-2010)*52 + datm$Epiweek

datm[1:100, c(2, 3, 12, 13, 30)] # checking out some of the data

# number of unique districts (131)
n.distr <- length(unique(datm$DISTCODE)) 

# vector of the unique district codes
unique.distcode <- unique(datm$DISTCODE)

# creating rate of decay function for ITN
# 60% protection at 24 months (96weeks)
0.4/96 # decay rate = 0.004166667
1/0.004166667 # intervention week where will get down to 0% effective = 240

ITNeff <- function(x){
  if(x<0) stop("x cannot be negative")
  if(x>240){
    return(0)
  }else{
    1 - 0.00416667*x
  }
}

ITNeff <- Vectorize(ITNeff, "x")

# creating rate of decay function IRS
# 75% protection at 6 months (24 weeks)
0.25/24 # decay rate = 0.01041667
1/0.01041667 # intervention week where will get down to 0% effective = 96

IRSeff <- function(x){
  if(x<0) stop("x cannot be negative")
  if(x>96){
    return(0)
  }else{
    1 - 0.01041667*x
  }
}

IRSeff <- Vectorize(IRSeff, "x")

## create the decay effect of IRS and ITN variables by looping through distict
# for details explaining refer to comments added to ITNdecayExample2.R

## decay for ITN ##
for(i in 1:n.distr) {
  ind.i <- which(datm$DISTCODE==unique.distcode[i]) 
  ind.ITN <- which(datm[ind.i,]$ITNind==1) 
  if(length(ind.ITN)>0) {
    ind.sort <- sort.list(datm[ind.i,][ind.ITN,]$time) 
    min.t <- min(datm[ind.i,][ind.ITN,][ind.sort,]$time)
    datm[ind.i,][ind.ITN,][ind.sort,]$ITNind <- ITNeff(datm[ind.i,][ind.ITN,][ind.sort,]$time-min.t)
  }
}

## decay for IRS ##
for(i in 1:n.distr) {
  ind.i <- which(datm$DISTCODE==unique.distcode[i]) 
  ind.IRS <- which(datm[ind.i,]$IRSind==1) 
  if(length(ind.IRS)>0) {
    ind.sort <- sort.list(datm[ind.i,][ind.IRS,]$time) 
    min.t <- min(datm[ind.i,][ind.IRS,][ind.sort,]$time)
    datm[ind.i,][ind.IRS,][ind.sort,]$IRSind <- IRSeff(datm[ind.i,][ind.IRS,][ind.sort,]$time-min.t)
  }
}

# save this final dataset
write.csv(datm, "~/Documents/CU AMC Fall 2017/BIOS6640/Project/FinalData.csv")

