#===================
#STAGE 1 run_model.R
#===================
#This run_model.R script runs all of the VE modules that synthesize households for a scenario. 

message('VE-Stage Staged: Stage 1 script entered\n')
#Run modules to synthesize households for all years
#--------------------------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWorkers",                  "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictIncome",                   "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
}
message('VE-Stage Staged: Stage 1 complete.\n')
