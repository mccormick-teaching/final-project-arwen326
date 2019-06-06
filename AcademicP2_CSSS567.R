rm(list = ls())


library(RSiena)
library(RSienaTest)
library(tidyverse)

########################### Load the data ########################### 
setwd("~/GoogleDrive/YY/Arwen academic/ArwenAcademicP2")
load("SimulatedData.RData")

########################### Recode Academic into behavior using 20 ranges ########################### 
for(i in 1:22){
  dat.w1 <- datlist.w1[[i]]
  dat.w3 <- datlist.w3[[i]]
  dat.w5 <- datlist.w5[[i]]
  
  # recode academic
  dat.w1 <- dat.w1 %>% 
    mutate(academic = case_when(Aca_raw < 20 ~ 0,
                                Aca_raw >= 21 & Aca_raw < 40 ~ 1,
                                Aca_raw >= 41 & Aca_raw < 60 ~ 2,
                                Aca_raw >= 61 & Aca_raw < 80 ~ 3,
                                Aca_raw >= 80 ~ 4))
  
  dat.w3 <- dat.w3 %>% 
    mutate(academic = case_when(Aca_raw < 20 ~ 0,
                                Aca_raw >= 21 & Aca_raw < 40 ~ 1,
                                Aca_raw >= 41 & Aca_raw < 60 ~ 2,
                                Aca_raw >= 61 & Aca_raw < 80 ~ 3,
                                Aca_raw >= 80 ~ 4))
  
  dat.w5 <- dat.w5 %>% 
    mutate(academic = case_when(Aca_raw < 20 ~ 0,
                                Aca_raw >= 21 & Aca_raw < 40 ~ 1,
                                Aca_raw >= 41 & Aca_raw < 60 ~ 2,
                                Aca_raw >= 61 & Aca_raw < 80 ~ 3,
                                Aca_raw >= 80 ~ 4))
  
  datlist.w1[[i]] <- dat.w1
  datlist.w3[[i]] <- dat.w3
  datlist.w5[[i]] <- dat.w5
}

########################### Calculate indices for different types of norms ########################### 
Des_norms <-  numeric(length(1:22))

# calculate the correlations and means to prepare the norms
for(i in 1:22){
  # load the data
  df <- datlist.w1[[i]]
  
  # calculate the average level of academic for each class
  Des_norms[i] <- mean(df$Aca_raw, na.rm = T)
}

# Indices for each norms
HighDesIdx <- which(Des_norms > mean(Des_norms))
LowDesIdx <- which(Des_norms <= mean(Des_norms)) 

####################################################################################################################
############################# Create SienaBayes Data and Model Object for All Classes ##############################
####################################################################################################################

# Note: repeat this for each separate analysis (e.g., High PP, High SP, etc.)
# list of Siena Data
SienaGroups <- list()

for(i in 1:length(datlist.w3)){
  # extract relevant dataset
  snadat.w1 <- datlist.w1[[i]]
  snadat.w3 <- datlist.w3[[i]]
  snadat.w5 <- datlist.w5[[i]]
  
  # Define dependent variable: friendship relationships
  friend_matrix <- array( c(sociomatrices.w1[[i]], sociomatrices.w3[[i]], sociomatrices.w5[[i]]),
                          dim = c(nrow(sociomatrices.w3[[i]]), nrow(sociomatrices.w3[[i]]), 3))
  
  friend_matrix <- sienaDependent(friend_matrix)
  
  # Define dependent variable: behavior variables
  ## Academic behavior
  Aca <- cbind(snadat.w1$academic, snadat.w3$academic, snadat.w5$academic)
  Aca <- sienaDependent(Aca, type = "behavior", allowOnly=FALSE)
  
  # Define constant covariate:  Gender
  female <- coCovar(snadat.w1$female)
  
  # Create Siena data from all the components
  sdata <- list(sienaDataCreate( friend_matrix, Aca, female))
  
  # Add to list of Siena Data ************* EASY TO FORGET *****
  SienaGroups <- c(SienaGroups, sdata)
}

# create SienaGroup object for all ties
SienaGroupAll <- sienaGroupCreate(SienaGroups)

# create SienaGroup object for all ties
SienaGroupAll <- sienaGroupCreate(SienaGroups)

# Define Effects from the Group objects
GroupEffectsAll <- getEffects(SienaGroupAll)

# Structural effects (random means that this allows a random intercept for each class)
GroupEffectsAll <- setEffect(GroupEffectsAll, density, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, recip, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, transRecTrip, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, transTrip, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, inPop, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, outAct, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, cycle3, random=TRUE)
GroupEffectsAll <- setEffect(GroupEffectsAll, isolateNet, random=TRUE)


# Selection Effects
## Academic
GroupEffectsAll <- setEffect(GroupEffectsAll, egoX, interaction1="Aca", random=FALSE) # ego effect
GroupEffectsAll <- setEffect(GroupEffectsAll, altX, interaction1="Aca", random=FALSE) # alter effect
GroupEffectsAll <- setEffect(GroupEffectsAll, simX, interaction1="Aca", random=FALSE) # similarity effect

## Female
GroupEffectsAll <- setEffect(GroupEffectsAll, egoX, interaction1="female", random=FALSE)
GroupEffectsAll <- setEffect(GroupEffectsAll, altX, interaction1="female", random=FALSE)
GroupEffectsAll <- setEffect(GroupEffectsAll, sameX, interaction1="female", random=FALSE)

# Influence Effects
## Average Similarity Effects
GroupEffectsAll <- setEffect(GroupEffectsAll, avSim, name = "Aca", 
                             interaction1 = "friend_matrix", random=FALSE)

## Covariate Control Effects (control for female)
GroupEffectsAll <- setEffect(GroupEffectsAll, effFrom, name = "Aca", 
                             interaction1 = "female", random=FALSE)

# check the effects
print(GroupEffectsAll, includeRandoms=TRUE)

####################################################################################################################
############################# Create SienaBayes Data and Model Object for HighDes Classes ##############################
##################################################################################################################### 


# list of Siena Data
SienaGroups <- list()

for(i in HighDesIdx){
  # extract relevant dataset
  snadat.w1 <- datlist.w1[[i]]
  snadat.w3 <- datlist.w3[[i]]
  snadat.w5 <- datlist.w5[[i]]
  
  # Define dependent variable: friendship relationships
  friend_matrix <- array( c(sociomatrices.w1[[i]], sociomatrices.w3[[i]], sociomatrices.w5[[i]]),
                          dim = c(nrow(sociomatrices.w3[[i]]), nrow(sociomatrices.w3[[i]]), 3))
  
  friend_matrix <- sienaDependent(friend_matrix)
  
  # Define dependent variable: behavior variables
  ## Academic behavior
  Aca <- cbind(snadat.w1$academic, snadat.w3$academic, snadat.w5$academic)
  Aca <- sienaDependent(Aca, type = "behavior", allowOnly=FALSE)
  
  # Define constant control variable:  Gender
  female <- coCovar(snadat.w1$female)
  
  # Create Siena data from all the components
  sdata <- list(sienaDataCreate( friend_matrix, Aca, female))
  
  # Add to list of Siena Data
  SienaGroups <- c(SienaGroups, sdata)
}

# create SienaGroup object for all ties
SienaGroupHighDes <- sienaGroupCreate(SienaGroups)

# Define Effects from the Group objects
GroupEffectsHighDes <- getEffects(SienaGroupHighDes)

# Structural effects (random means that this allows a random intercept for each class)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, density, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, recip, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, transRecTrip, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, transTrip, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, inPop, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, outAct, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, cycle3, random=TRUE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, isolateNet, random=TRUE)


# Selection Effects
## Academic
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, egoX, interaction1="Aca", random=FALSE) # ego effect
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, altX, interaction1="Aca", random=FALSE) # alter effect
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, simX, interaction1="Aca", random=FALSE) # similarity effect

## Female
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, egoX, interaction1="female", random=FALSE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, altX, interaction1="female", random=FALSE)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, sameX, interaction1="female", random=FALSE)

# Influence Effects
## Average Similarity Effects
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, avSim, name = "Aca", 
                                 interaction1 = "friend_matrix", random=FALSE)

## Covariate Control Effects (control for female)
GroupEffectsHighDes <- setEffect(GroupEffectsHighDes, effFrom, name = "Aca", 
                                 interaction1 = "female", random=FALSE)

# check the effects
print(GroupEffectsHighDes, includeRandoms=TRUE)

####################################################################################################################
############################# Create SienaBayes Data and Model Object for LowDes Classes ##############################
##################################################################################################################### 


# list of Siena Data
SienaGroups <- list()

for(i in LowDesIdx){
  # extract relevant dataset
  snadat.w1 <- datlist.w1[[i]]
  snadat.w3 <- datlist.w3[[i]]
  snadat.w5 <- datlist.w5[[i]]
  
  # Define dependent variable: friendship relationships
  friend_matrix <- array( c(sociomatrices.w1[[i]], sociomatrices.w3[[i]], sociomatrices.w5[[i]]),
                          dim = c(nrow(sociomatrices.w3[[i]]), nrow(sociomatrices.w3[[i]]), 3))
  
  friend_matrix <- sienaDependent(friend_matrix)
  
  # Define dependent variable: behavior variables
  ## Academic behavior
  Aca <- cbind(snadat.w1$academic, snadat.w3$academic, snadat.w5$academic)
  Aca <- sienaDependent(Aca, type = "behavior", allowOnly=FALSE)
  
  # Define constant control variable:  Gender
  female <- coCovar(snadat.w1$female)
  
  # Create Siena data from all the components
  sdata <- list(sienaDataCreate( friend_matrix, Aca, female))
  
  # Add to list of Siena Data
  SienaGroups <- c(SienaGroups, sdata)
}

# create SienaGroup object for all ties
SienaGroupLowDes <- sienaGroupCreate(SienaGroups)

# Define Effects from the Group objects
GroupEffectsLowDes <- getEffects(SienaGroupLowDes)

# Structural effects (random means that this allows a random intercept for each class)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, density, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, recip, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, transRecTrip, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, transTrip, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, inPop, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, outAct, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, cycle3, random=TRUE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, isolateNet, random=TRUE)


# Selection Effects
## Academic
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, egoX, interaction1="Aca", random=FALSE) # ego effect
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, altX, interaction1="Aca", random=FALSE) # alter effect
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, simX, interaction1="Aca", random=FALSE) # similarity effect

## Female
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, egoX, interaction1="female", random=FALSE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, altX, interaction1="female", random=FALSE)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, sameX, interaction1="female", random=FALSE)

# Influence Effects
## Average Similarity Effects
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, avSim, name = "Aca", 
                                interaction1 = "friend_matrix", random=FALSE)

## Covariate Control Effects (control for female)
GroupEffectsLowDes <- setEffect(GroupEffectsLowDes, effFrom, name = "Aca", 
                                interaction1 = "female", random=FALSE)

# check the effects
print(GroupEffectsLowDes, includeRandoms=TRUE)

####################################################################################################################
############################# Run the models ##############################
##################################################################################################################### 

# define algorithm
GroupsAlgo <- sienaAlgorithmCreate(projname = 'SienaGroups', mult=5)

# run model for all classes, high and low descriptive norms
All <- sienaBayes(algo = GroupsAlgo, 
                  data = SienaGroupAll, 
                  effects = GroupEffectsAll,
                  nwarm=100, nmain=1000, nrunMHBatches=20, nImproveMH=100, silentstart=FALSE,
                  nbrNodes = 29, initgainGroupwise = 0, initgainGlobal=0.001)

LowDes <- sienaBayes(algo = GroupsAlgo, 
                     data = SienaGroupLowDes, 
                     effects = GroupEffectsLowDes,
                     nwarm=100, nmain=1000, nrunMHBatches=20, nImproveMH=100, silentstart=FALSE,
                     nbrNodes = 29, initgainGroupwise = 0, initgainGlobal=0.001)

HighDes <- sienaBayes(algo = GroupsAlgo, 
                     data = SienaGroupHighDes, 
                     effects = GroupEffectsHighDes,
                     nwarm=100, nmain=1000, nrunMHBatches=20, nImproveMH=100, silentstart=FALSE,
                     nbrNodes = 29, initgainGroupwise = 0, initgainGlobal=0.001)

####################################################################################################################
############################# Print results ##############################
##################################################################################################################### 
print(All)

print(LowDes)

print(HighDes)
