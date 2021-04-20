#===========
#run_model.R
#===========

#This script demonstrates the VisionEval framework for a demonstration of VEScenario model

#Load libraries
#--------------
cat('run_model.R: script entered\n')
library(visioneval)
cat('run_model.R: libraries loaded\n')

planType <- 'callr'
ptm <- proc.time()
#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = FALSE,
  DatastoreName = NULL,
  SaveDatastore = TRUE
)
#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule(
    ModuleName = "RunScenarios",
    PackageName = "VEScenario",
    RunFor = "AllYears",
    RunYear = Year
  )
}
cat('run_model.R: run complete.\n')
proc.time() - ptm

