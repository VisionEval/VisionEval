#===================
#STAGE 3 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model the adopted plans powertrains and fuels scenario

cat('run_model.R: Stage 3 script entered\n')

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "../VE-State-stage-3/defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = TRUE,
  DatastoreName = "../VE-State-stage-2/Datastore",
  SaveDatastore = TRUE
  )  

#Run all demo module for all years
#---------------------------------
# requirePackage("VEHouseholdTravel")
for(Year in getYears()) {
  runModule("CalculateCarbonIntensity",        "VEPowertrainsAndFuels", RunFor = "AllYears",    RunYear = Year)
  runModule("AssignHhVehiclePowertrain",       "VEPowertrainsAndFuels", RunFor = "AllYears",    RunYear = Year)
}

cat('run_model.R: Stage 3 script complete\n')
