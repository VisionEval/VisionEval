# Walkthrough script for using LoadModel to incorporate another Datastore

loadModelSetup <- function(log="warn")
{
  logLevel(log)

  baseModel <- openModel("VERSPM-base")
  bare <- openModel("BARE") # run mini-model.R to create the bare model

  # Workflow here could assist with post-mortem debugging
  # Start by copying the original model, but don't grab the results
  #  The point is to have the subsequent model run copy the results
  message("Making demo for LoadModel as 'BARE-load'")
  loadPath <- file.path("models","BARE-load")
  if ( dir.exists(loadPath) ) unlink(loadPath,recursive=TRUE)
  loadModel <- bare$copy("BARE-load",copyResults=FALSE)
  print(loadModel)

  message("Set up load script...")
  loadModelPath <- loadModel$setting("ModelDir",shorten=FALSE)
  runModelFile <- file.path(
    loadModelPath,
    loadModel$setting("ModelScript")
  )
  runModel_vc <- c(
    '',
    'for(Year in getYears()) {',
    'runModule("AssignLifeCycle","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictIncome", "VESimHouseholds", RunFor = "AllYears", RunYear = Year)',
    'runModule("PredictHousing", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    'runModule("LocateEmployment", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    'runModule("AssignLocTypes", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    '}'
  )
  writeLines(runModel_vc,con=runModelFile)

  message("Configure LoadModel")
  # We'll just do it from scratch rather than reading/modifying
  # Region, BaseYear and Years don't change
  runConfig_ls <-  list(
    Model       = "LOAD Model Test",
    Scenario    = "Test LoadModel / LoadDatastore",
    Description = "Add a step onto a different previous model",
    Region      = bare$setting("Region"),
    BaseYear    = bare$setting("BaseYear"),
    Years       = "2038", # new model only does 2038 - but we'll see BaseYear/2010 in results
    LoadModel   = bare$modelPath,
    LoadStage   = names(bare$modelStages)[1] # would default there anyway
  )
  configFile <- file.path(loadModelPath,"visioneval.cnf")
  yaml::write_yaml(runConfig_ls,configFile)

  message("Reload model with LoadDatastore")
  loadModel$configure()

  message("Copy additional inputs")
  base.inputs <- baseModel$setting("InputPath",shorten=FALSE)
  cat("Base Inputs",base.inputs,"\n")
  print(loadModel$setting("LoadDatastoreName"))
  inputs <- loadModel$list(inputs=TRUE,details=c("FILE"))
  required.files <- unique(file.path(base.inputs,inputs[,"FILE"]))
  required.files <- required.files[which(file.exists(required.files))]
  cat("Required Files:\n")
  print(basename(required.files))

  message("Copying additional input files")
  load.inputs <- loadModel$setting("InputPath",shorten=FALSE)

  message("Remove base model inputs - will just have new ones")
  unlink(load.inputs,recursive=TRUE)
  dir.create(load.inputs)
  from <- required.files
  file.copy(from=from, to=load.inputs )

  message("Copy model parameters to 'inputs' - could also be in 'defs')")
  from <- file.path(base.inputs,"model_parameters.json")
  file.copy(from=from,to=load.inputs) # or copy to bare.defs...
  print(dir(load.inputs))

  return( openModel("BARE-load") )
}

#################################
# LOADING ANOTHER MODEL'S RESULTS
#################################

loadModel <- loadModelSetup()
print(loadModel)
viewSetup(loadModel,fromFile=TRUE)
loadModel$run()

# Now notice that the data from the original mini model was copied
# into this Datastore; 
loadModel$dir(results=TRUE,all.files=TRUE)
