#===================
#STAGE 1 run_model.R
#===================
#This run_model.R script runs all of the VE modules that synthesize households for a scenario. 

#Load libraries
#--------------
library(visioneval)

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
  SaveDatastore = FALSE
  )  
#Run modules to synthesize households for all years
#--------------------------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWorkers",                  "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictIncome",                   "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
}
 
STAGE 2 of model run to model land use and transportation scenario  (e.g. dense land use, limited parking, high transit service)
#===================
#STAGE 2 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model a land use and transportation scenario

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = TRUE,
  DatastoreName = "<PATH-TO-STAGE-1-SCENARIO-DIRECTORY>/Datastore",
  SaveDatastore = FALSE
  )  

#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule("CreateSimBzones",                 "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("SimulateHousing",                 "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("SimulateEmployment",              "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("Simulate4DMeasures",              "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("SimulateUrbanMixMeasure",         "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("AssignParkingRestrictions",       "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("AssignCarSvcAvailability",        "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("AssignDemandManagement",          "VESimLandUse",          RunFor = "AllYears",    RunYear = Year)
  runModule("SimulateTransitService",          "VESimTransportSupply",  RunFor = "AllYears",    RunYear = Year)
  runModule("SimulateRoadMiles",               "VESimTransportSupply",  RunFor = "AllYears",    RunYear = Year)
  runModule("AssignDrivers",                   "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignVehicleOwnership",          "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignVehicleType",               "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("CreateVehicleTable",              "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("AssignVehicleAge",                "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateVehicleOwnCost",         "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("AdjustVehicleOwnership",          "VEHouseholdVehicles",   RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateHouseholdDvmt",          "VEHouseholdTravel",     RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateAltModeTrips",           "VEHouseholdTravel",     RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateVehicleTrips",           "VEHouseholdTravel",     RunFor = "AllYears",    RunYear = Year)
  runModule("DivertSovTravel",                 "VEHouseholdTravel",     RunFor = "AllYears",    RunYear = Year)
}
STAGE 3 of model run to model adopted plans powertrains and fuels scenario
#===================
#STAGE 3 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model the adopted plans powertrains and fuels scenario

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = TRUE,
  DatastoreName = "<PATH-TO-STAGE-2-SCENARIO-DIRECTORY>/Datastore",
  SaveDatastore = FALSE
  )  

#Run all demo module for all years
#---------------------------------
requirePackage("VEHouseholdTravel")
 
for(Year in getYears()) {
  runModule("CalculateCarbonIntensity",        "VEPowertrainsAndFuelsxAP", RunFor = "AllYears",    RunYear = Year)
  runModule("AssignHhVehiclePowertrain",       "VEPowertrainsAndFuelsxAP", RunFor = "AllYears",    RunYear = Year)
}
 
STAGE 4 of model run to model travel performance scenario (e.g. high tax and carbon pricing scenario)
#===================
#STAGE 4 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model a travel performance scenario (e.g. pricing)

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = TRUE,
  DatastoreName = "<PATH-TO-STAGE-3-SCENARIO-DIRECTORY>/Datastore",
  SaveDatastore = FALSE
  )  

#Run all demo module for all years
#---------------------------------
requirePackage("VEHouseholdTravel")
requirePackage("VEPowertrainsAndFuelsxAP")
 
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
