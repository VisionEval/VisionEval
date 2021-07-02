#===========
#run_model.R
#===========

#This script tests the VisionEval staged model with the first half of the model run

#Load libraries
#--------------
library(visioneval)
writeLog('Running Stage One')

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

#Run first stage modules
#---------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWorkers",                  "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictIncome",                   "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictHousing",                  "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("LocateEmployment",                "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLocTypes",                  "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("Calculate4DMeasures",             "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateUrbanMixMeasure",        "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignParkingRestrictions",       "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignDemandManagement",          "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignCarSvcAvailability",        "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignTransitService",            "VETransportSupply",     RunFor = "AllYears",    RunYear = Year)
  runModule("AssignRoadMiles",                 "VETransportSupply",     RunFor = "AllYears",    RunYear = Year)
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
  runModule("CalculateCarbonIntensity",        "VEPowertrainsAndFuels", RunFor = "AllYears",    RunYear = Year)
  runModule("AssignHhVehiclePowertrain",       "VEPowertrainsAndFuels", RunFor = "AllYears",    RunYear = Year)
}
writeLog('Completed Stage One')
