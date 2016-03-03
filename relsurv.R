# Step 0 - load packages

require(dplyr)
require(relsurv)

#Step 1 - load data

regall <- read.csv("J:/regall.csv") # data address

# Step 2 - construct survival objects

reg_Surv                                                      <- with(regall, Surv(Survival, VitalStatus))
regall$status                                                 <- 0
regall$status[which(regall$CauseOfDeath == "Breast Cancer")]  <- 1
regall$status[which(regall$CauseOfDeath != "Breast Cancer")]  <- 2

