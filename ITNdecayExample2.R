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

unique.distcode <- unique(data$DISTCODE)
for(i in 1:n.distr) {
  ind.i <- which(data$DISTCODE==unique.distcode[i])
  ind.ITN <- which(data[ind.i,]$postITN==1)
  if(length(ind.ITN)>0) {
    ind.sort <- sort.list(data[ind.i,][ind.ITN,]$time)
    min.t <- min(data[ind.i,][ind.ITN,][ind.sort,]$time)
    data[ind.i,][ind.ITN,][ind.sort,]$postITN <- efficacy.net(data[ind.i,][ind.ITN,][ind.sort,]$time-min.t)
  }
}



