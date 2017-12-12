rm(list = ls())

data <- read.csv("ITNcoverage.csv")

x.val <- (0:8)

y.val <- c(100,	93,	74,	50,	28,	12,	4,	1,	0)/100


f.eff <- splinefun(x.val,y.val,method="natural")


efficacy.net <- function(x) {
  if(x<0) stop("x cannot be negative")
  if(x>8) {
    return(0)
  } else {
    f.eff(x)
  }
}
efficacy.net <- Vectorize(efficacy.net,"x")

curve(efficacy.net(x),xlim=c(0,8))

data$time <- (data$yr-2010)*12+data$mon

n.distr <- length(unique(data$DISTCODE))

unique.distcode <- unique(data$DISTCODE) # get a list of all unique districts
for(i in 1:n.distr) {
  ind.i <- which(data$DISTCODE==unique.distcode[i]) # gives row index for all obs in distcode i
  ind.ITN <- which(data[ind.i,]$postITN==1) 
  # above line takes all the observations in the dataset for district i where ITN spraying =1 
  # or where ITN spraying did occur
  # this code takes the index for that observation(s) for district i where ITN happend
  # below code is saying.. 
  # if there is some spraying (ind.ITN >1) at some point for the district then
  # 
  if(length(ind.ITN)>0) {
    ind.sort <- sort.list(data[ind.i,][ind.ITN,]$time) 
    # creates indicator to sort the times for district i that ITN occured in order
    min.t <- min(data[ind.i,][ind.ITN,][ind.sort,]$time)
    # the inner part with the data orders the times so that they are in chronological order
    # the sort indicator did this
    # we then take the min function to get the min (earliest) time that ITN occured
    data[ind.i,][ind.ITN,][ind.sort,]$postITN <- efficacy.net(data[ind.i,][ind.ITN,][ind.sort,]$time-min.t)
    # she then runs her decay function to update the ITN variable (it is currently just 0 and 1)
    # updated for the times for that district that ITN occurred?
    # what she put into her function:
    # the data with square bracketing selects the data for the times at which ITN occured 
    # for district i and they are in chronoligcal order of when they occured
    # the $ subsets out the times (chronological week from time variable)
    # then she subtracts off the min time
    # so the new thing she put in is a list of numbers and they are the times at when spraying
    # began which is now called 0 (starts with 0 because subtract off min) and then
    # each other is how many weeks past the initial ITN
  }
}

functest <- function(x){
  if(x<0) stop("x cannot be negative")
  if(x>240){
    return(0)
  }else{
    1 - 0.00416667*x
  }
}

functest <- Vectorize(functest, "x")
functest(test6)
# this works to make the decaying effect!
length(test)
