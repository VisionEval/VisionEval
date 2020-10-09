#===================
#STAGE 4 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model a travel performance scenario (e.g. pricing)

cat('run_model.R: Stage 4 script entered\n')

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "../VE-State-stage-4/defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = TRUE,
  DatastoreName = "../VE-State-stage-3/Datastore",
  SaveDatastore = TRUE
  )  

#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  for (i in 1:2) {
    runModule("CalculateRoadDvmt",             "VETravelPerformance",   RunFor = "AllYear",    RunYear = Year)
    runModule("CalculateRoadPerformance",      "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
    runModule("CalculateMpgMpkwhAdjustments",  "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
    runModule("AdjustHhVehicleMpgMpkwh",       "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
    runModule("CalculateVehicleOperatingCost", "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
    runModule("BudgetHouseholdDvmt",           "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
    runModule("BalanceRoadCostsAndRevenues",   "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
  }
  runModule("CalculateComEnergyAndEmissions",   "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
  runModule("CalculatePtranEnergyAndEmissions", "VETravelPerformance",   RunFor = "AllYears",    RunYear = Year)
}

cat('run_model.R: Stage 4 script complete\n')
