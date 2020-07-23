#===================
#STAGE 2 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model a land use and transportation scenario

cat('run_model.R: Stage 2 script entered\n')

#Load libraries
#--------------
library(visioneval)

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "../VE-State-stage-2/defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = TRUE,
  DatastoreName = "../VE-State-stage-1/Datastore",
  SaveDatastore = TRUE
  )
cat("run_model.R: Stage 2 Initialization Complete")

#Run core models for land use and travel
#---------------------------------------
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
cat('run_model.R: Stage 2 run complete.\n')

