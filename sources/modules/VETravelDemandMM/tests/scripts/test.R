#Test VETravelDemandMM module
library(visioneval)
if (!(require(VETravelDemandMM))) {
  #devtools::install_local(".")
  system("R CMD INSTALL .")
  library(VETravelDemandMM)
}

source("R/VETravelDemandMM.R")

TestDir <- normalizePath(".")
if (!endsWith(TestDir, 'tests'))
  TestDir <- file.path(TestDir, 'tests')


#Test PredictVehicles module
#source("R/PredictVehicles.R")
testModule(
  ModuleName = "PredictVehicles",
  #ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

#Test PredictDrivers module
#source("R/PredictDrivers.R")
testModule(
  ModuleName = "PredictDrivers",
  #ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

# #Test PredictAADVMT module
# #source("R/PredictAADVMT.R")
# testModule(
#   ModuleName = "PredictAADVMT",
#   #ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
# 
# #Test PredictBikePMT module
# #source("R/PredictBikePMT.R")
# testModule(
#   ModuleName = "PredictBikePMT",
#   #ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
# 
# #Test PredictWalkPMT module
# #source("R/PredictWalkPMT.R")
# testModule(
#   ModuleName = "PredictWalkPMT",
#   #ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
# 
# #Test PredictTransitPMT module
# #source("R/PredictTransitPMT.R")
# testModule(
#   ModuleName = "PredictTransitPMT",
#   #ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )

# #Test CalculateHouseholdDvmt module
# #source("R/CalculateHouseholdDvmt.R")
 testModule(
   ModuleName = "CalculateHouseholdDvmt",
  #ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
 )
 
 # #Test CalculateAltModeTrips module
 # #source("R/CalculateAltModeTrips.R")
 testModule(
   ModuleName = "CalculateAltModeTrips",
   #ProjectDir = TestDir,
   ParamDir = "defs",
   LoadDatastore = TRUE,
   SaveDatastore = TRUE,
   DoRun = TRUE
 )
