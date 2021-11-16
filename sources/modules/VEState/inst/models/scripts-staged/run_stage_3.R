#===================
#STAGE 3 run_model.R
#===================
# This run_model.R script runs all of the VE modules to model the adopted plans powertrains and fuels scenario

message('VE-Stage Staged: Stage 3 script entered\n')
#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule("CalculateCarbonIntensity",        "VEPowertrainsAndFuels", RunFor = "AllYears",    RunYear = Year)
  runModule("AssignHhVehiclePowertrain",       "VEPowertrainsAndFuels", RunFor = "AllYears",    RunYear = Year)
}
message('VE-State Staged: Stage 3 complete\n')
