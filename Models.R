#########################
### Michaela Palumbo
### BIOS 6640 Project
### Running Models
#########################

# load final dataset
dat <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/FinalData.csv")
dat$X <- NULL # unnecessary X column

# looked at lecture 17 notes R script notes on poisson regression models
library(lme4)

 
fit1 <- glmer(cases ~ raint + tavg + Epiweek + decayIRS + decayITN + Region + (1|DISTCODE), 
              data = dat, family = poisson(link = "log"))
summary(fit1)
# this did not converge

# do I need to lag the weather variables like tavg, raint, etc. or is lagging the epiweek
# all we need to do?
# what did Mel mean by offset(population)
# mayb don't include region?