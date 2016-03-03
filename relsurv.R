# Step 0 - load packages

require(dplyr)
require(relsurv)

#Step 1 - load data

regall <- read.csv("J:/regall.csv") # data address
regall <- regall %>% filter(StageSimple!="0" | is.na(StageSimple))

# Step 2 - construct survival objects

reg_Surv                                                      <- with(regall, Surv(Survival, VitalStatus))
regall$status                                                 <- 0
regall$status[which(regall$CauseOfDeath == "Breast Cancer")]  <- 1
regall$status[which(regall$CauseOfDeath != "Breast Cancer")]  <- 2
reg_Surv_BC                                                   <- with(regall, Surv(Survival, status==1))
reg_Surv_OC                                                   <- with(regall, Surv(Survival, status==2))

# Step 3 - load in population background mortality

lifetab <- transrate.hmd("mltper_1x1.txt", "fltper_1x1.txt")

# Step 4 - relative survival calculations
# use rs.surv (Ederer II method)

# raw survival - KM curve

OS_pop <- survfit(reg_Surv ~ 1)

# population
RS_pop <- rs.surv(reg_Surv ~ 1 + ratetable(age=AgeDiag*365.24, sex="female", year=(DiagYear-1960)*365.24), data=regall, ratetable=lifetab, method="ederer2") # pkg needs age and year in days; year 0 is 1960. see help file for details

# By age group

OS_age <- survfit(reg_Surv ~ AgeCat10Diag,data=regall) #raw

RS_age <- rs.surv(Surv(Survival, VitalStatus) ~ AgeCat10Diag + ratetable(age=AgeDiag*365.24, sex="female", year=(DiagYear-1960)*365.24), data=regall, ratetable=lifetab, method="ederer2") # no idea why but this does not work if you save the survival object first - it needs a function call at lhs of formula

# By stage - our algorithm

OS_stage <- survfit(reg_Surv ~ addNA(StageSimple), data = regall)

RS_stage <- rs.surv(Surv(Survival, VitalStatus) ~ addNA(StageSimple) + ratetable(age = AgeDiag*365.24, sex="female", year = (DiagYear-1960)*365.24), data=regall, ratetable = lifetab, method="ederer2")

# By stage - their algorithm

OS_stageWM <- survfit(reg_Surv ~ StageSimple, data=regall, subset=(!is.na(IntegratedTNM)))

RS_stageWM <- rs.surv(Surv(Survival, VitalStatus) ~ StageSimple + ratetable(age = AgeDiag*365.24, sex="female", year = (DiagYear-1960)*365.24), data=(regall %>% filter(!is.na(IntegratedTNM))), ratetable = lifetab, method="ederer2")