# run_popsyn.R script for staged population

#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictWorkers",                  "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictIncome",                   "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
}
