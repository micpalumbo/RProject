###########################################
### Michaela Palumbo
### BIOS 6640 Project
### Inserting Decaying Intervention Effects
###########################################

# load cleaned and merged dataset
datm <- read.csv("~/Documents/CU AMC Fall 2017/BIOS6640/Project/CleanMerged.csv")
datm$X <- NULL # unnecessary X column
# why does my merged data only have 131 levels now instead of 142?
# 51876 observations?
sum(datm$IRSind) # 35
sum(datm$ITNind) # 141

# uses/modifies code provided by Katie in ITNdecayExample2.R

# create time column
datm$time <- (datm$Epiyear-2010)*12 + datm$Epiweek

# make difference from IRS intervention with current data
datm2 <- NULL

for(i in levels(datm$District)){
  df <- subset(datm, datm$District %in% i)
  timeIRS <- df$time[df$IRSind==1]
  df[, "decayIRS"] <- NA
  
  if(length(timeIRS)==1){
    
    for(j in 1:dim(df)[1]){
      if(df[j,]$time >= timeIRS){
        df[j,]$decayIRS <- df[j,]$time - timeIRS
      }
    }
    
  } else if(length(timeIRS)==2){
    for(j in 1:dim(df)[1]){
      if(df[j,]$time >= timeIRS[1] & df[j,]$time < timeIRS[2]){
        df$decayIRS <- df[j,]$time - timeIRS[1]
      } else if(df[j,]$time >= timeIRS[2]){
        df$decayIRS <- df[j,]$time - timeIRS[2]
      } else{
        
      }
    }
  } else{
    
  }
  datm2 <- rbind(datm2, df)
  
}

sum(datm2$IRS) # still 35
sum(datm2$ITN) # still 141

# make difference from ITN intervention with current data
# change all datm2 to datm3 and all datm to datm2 and all IRS to ITN
datm3 <- NULL

for(i in levels(datm2$District)){
  df <- subset(datm2, datm2$District %in% i)
  timeITN <- df$time[df$ITNind==1]
  df[, "decayITN"] <- NA
  
  if(length(timeITN)==1){
    
    for(j in 1:dim(df)[1]){
      if(df[j,]$time >= timeITN){
        df[j,]$decayITN <- df[j,]$time - timeITN
      }
    }
    
  } else if(length(timeITN)==2){
    for(j in 1:dim(df)[1]){
      if(df[j,]$time >= timeITN[1] & df[j,]$time < timeITN[2]){
        df$decayITN <- df[j,]$time - timeITN[1]
      } else if(df[j,]$time >= timeITN[2]){
        df$decayITN <- df[j,]$time - timeITN[2]
      } else{
        
      }
    }
  } else{
    
  }
  datm3 <- rbind(datm3, df)
  
}

summary(datm3)
# lots of NAs for the decay variables 

# save this final dataset
write.csv(datm3, "~/Documents/CU AMC Fall 2017/BIOS6640/Project/FinalData.csv")

# things to ask Alyssa:
# why she subtracted by 2009 instead of 2010 for her time variable
# different total ITNs
# different total obs
# since we're adding epiweek not month should we still be multiplying by 12?