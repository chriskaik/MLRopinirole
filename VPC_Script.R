
# -------------  Initial Simulation from Simulix -----------------

rm(list = ls())
library(mlxR)
library(randomForest)
library(TeachingDemos)
library(dplyr)
library(tidyvpc)
library(caret)
require(stringr)
# load and initialize the API
library(lixoftConnectors) 
initializeLixoftConnectors(software="simulx")
# Get the project
#project <- paste0(getDemoPath(), "/2.models/longitudinal.smlx")
loadProject(projectFile = "C:/Users/christos/Documents/Res. Error/R/MONOLIX/Ropinirol_BAD.smlx")
df_i_f <- NULL
N_samples = 500
# Run the simulation
for (i in 1:N_samples) {
  addGroup(str_c("simulationGroup",i))
}

runSimulation()

df_res <-  getSimulationResults()

df_result <-  df_res[["res"]][["CONC"]]
for (i in 1:N_samples) {
  df_iparams <- df_res[["IndividualParameters"]][[str_c("simulationGroup",i)]]
  df_iparams <- as.data.frame(subset(df_iparams,select = -c(original_id)))
  df_i_f <- rbind(df_i_f,(df_iparams))
}
df_result <- subset(df_result,select = -c(group))
resa <- merge(df_result, df_i_f, by = "id",all.x='TRUE',all.y = 'TRUE')

VPC for Initial Simulation


##---------------------------- DATA LOADING -------------------

res = read.csv("C:\\Users\\christos\\Documents\\Res. Error\\R\\Real_Data\\BAD MODEL\\res_realR_badm.csv")
real_data = read.csv("C:\\Users\\christos\\Documents\\Res. Error\\R\\Real_Data\\Real_Obs.csv")

##---------------------------- DATA ANALYSIS ------------------

sim_data <- data.frame("id" = resa$id ,"dv" = resa$CONC, "time" = resa$time)
Obs_data <- data.frame("id" = real_data$ID ,"dv" = real_data$Y, "time" = real_data$TIME)
Obs_data$time <- as.double(Obs_data$time)
sim_data$time <- as.double(sim_data$time)

##---------------------------- VPC ----------------------------


vpc <- observed(Obs_data, x=time, y=dv) %>%
  simulated(sim_data, y=dv) %>%
  binning(bin=(time)) %>%
  vpcstats(qpred = c(0.1, 0.5, 0.9),conf.level = 0.9
  )
# jpeg(str_c("C:\\Users\\christos\\Documents\\Res. Error\\R\\Real_Data\\VPC_IM1.jpg"),res=300,width = 8,height=6,units='in')
plot(vpc)
# dev.off()



traindata = res
names(traindata)[names(traindata) == 'TIME'] <- 'time'
names(traindata)[names(traindata) == 'KA']   <- 'ka1'
names(traindata)[names(traindata) == 'KA2']  <- 'ka2'
names(traindata)[names(traindata) == 'Y']    <- 'CONC'

##--------------------- MACHINE LEARNING MODEL ----------------

fitcontrol <- trainControl(search="grid")
rfgrid <- expand.grid(.mtry = 6)
rfMod  <- train(IRES~time+ka1+ka2+V+Cl+CONC,
                data = traindata,
                method = "rf",
                trControl = fitcontrol,
                tuneGrid  = rfgrid,
                verbose = FALSE,
                ntree = 400,
)
resa <- subset(resa,select = -c(original_id))
resa$IRES <- 0

pred <- predict(rfMod,newdata = resa)
resa$IRES <- pred
resa_corr = resa
resa_corr$CONC = resa$CONC+resa$IRES
resa_corr[resa[q,2]==0,3] <- 0 
sim_data_corr <- data.frame("id" = resa_corr$id ,"dv" = resa_corr$CONC, "time" = resa_corr$time)
sim_data_corr$time <- as.double(sim_data_corr$time)

##---------------------------- FINAL VPC ----------------------------


vpc <- observed(Obs_data, x=time, y=dv) %>%
  simulated(sim_data_corr, y=dv) %>%
  binning(bin=(time)) %>%
  vpcstats(qpred = c(0.1, 0.5, 0.9),conf.level = 0.9
  )
plot(vpc)


