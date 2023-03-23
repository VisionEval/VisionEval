# run_model.R for year-based scenario with population already synthesized

#Run all demo module for all years
#---------------------------------
for(Year in getYears()) {
  runModule("PredictHousing",                  "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("LocateEmployment",                "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignLocTypes",                  "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("Calculate4DMeasures",             "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("CalculateUrbanMixMeasure",        "VELandUse",             RunFor = "AllYears",    RunYear = Year)
  runModule("AssignParkingRestrictions",       "VELandUse",             RunFor = "AllYears",    RunYear = Year)
}
