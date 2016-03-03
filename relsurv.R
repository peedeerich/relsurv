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
reg_Surv_BC                                                   <- with(regall, Surv(Survival, status==1))
reg_Surv_OC                                                   <- with(regall, Surv(Survival, status==2))

# Step 3 - load in population background mortality

lifetab <- transrate.hmd("mltper_1x1.txt", "fltper_1x1.txt")
