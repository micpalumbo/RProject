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

fit1 <- glmer(cases ~ 1 + raint2 + tavg2 + ITNind + IRSind + Region + (1|DISTCODE), 
              data = dat, offset = log(u5total), family = poisson(link = "log"))
summary(fit1)
# this did not converge

fit2 <- glmer(cases ~ ITNind + IRSind + (1|DISTCODE), 
              data = dat, offset = log(u5total), family = poisson(link = "log"))
summary(fit2)
# this didn't give me a warning when all I included were the decayed intervention variables

# tried lots of different things that didn't converge..
# told to ignore warnings with convergence and just interpret what we get
# so I will explore models with different lags and just choose one based on AIC.. 
# I decided to only use raint and tavg variables from weather data as predictors
# random intercept for district
# (all are very high)

modlag2 <- glmer(cases ~ 1 +raint2 + tavg2 + ITNind + IRSind + (1|DISTCODE), data = dat,
                 offset = log(u5total), family = poisson(link = "log"))
summary(modlag2)
# AIC = 936736.2
# this is the model I chose to interpret

modlag4 <- glmer(cases ~ 1 +raint4 + tavg4 + ITNind + IRSind + (1|DISTCODE), data = dat,
                 offset = log(u5total), family = poisson(link = "log"))
summary(modlag4)
# AIC = 960594.3 (lag 2 model is better than lag 4 according to AIC)

modlag8 <- glmer(cases ~ 1 + raint8 + tavg8 + ITNind + IRSind + (1|DISTCODE), data = dat,
                 offset = log(u5total), family = poisson(link = "log"))
summary(modlag8)
# AIC = 1188584.1 (lag 2 and lag 4 models are better than lag 8 according to AIC)