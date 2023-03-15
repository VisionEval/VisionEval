# run_snap.R script to demonstrate Snapshot module

#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule("CreateHouseholds", "VESimHouseholds", RunFor = "AllYears", RunYear = Year)
  runModule("Snapshot",         "VESnapshot",      Instance="HhSizeSnapshot", RunFor = "AllYears", RunYear = Year)
  runModule("PredictWorkers",   "VESimHouseholds", RunFor = "AllYears", RunYear = Year)
  runModule("AssignLifeCycle",  "VESimHouseholds", RunFor = "AllYears", RunYear = Year)
  runModule("PredictIncome",    "VESimHouseholds", RunFor = "AllYears", RunYear = Year)
}
