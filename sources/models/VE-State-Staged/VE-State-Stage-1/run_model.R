#===================
#STAGE 1 run_model.R
#===================
#This run_model.R script runs all of the VE modules that synthesize households for a scenario. 

cat('run_model.R: Stage 1 script entered\n')

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "../VE-State-Stage-1/defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = FALSE,
  DatastoreName = NULL,
  SaveDatastore = TRUE
  )  
cat('run_model.R: initializeModel completed\n')

#Run modules to synthesize households for all years
#--------------------------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWorkers",                  "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictIncome",                   "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
}
 
cat('run_model.R: Stage 1 complete.\n')
