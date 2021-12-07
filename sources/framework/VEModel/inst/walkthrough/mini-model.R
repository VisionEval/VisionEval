require(VEModel)

# Define a function to make a mini-model (adapted from VEModel/tests/test.R)

# In the "live walkthrough", we'll be looking at the files after they are
#  created. This code provides a concrete example of what is required to make
#  a runnable model stage.

makeMiniModel <- function(baseModel,log="warn" ) {

  logLevel(log)

  if ( missing(baseModel) || ! "VEModel" %in% class(baseModel) ) {
    stop("Must provide baseModel (opened VEModel; need not have been run)")
  }

  message("Cleaning up previous mini models.")
  models.dir <- file.path(getwd(),"models")
  obsolete <- dir(models.dir,pattern="^bare",ignore.case=TRUE,full.names=TRUE)
  for ( oo in obsolete ) {
    if ( dir.exists(oo) ) unlink(oo,recursive=TRUE)
  }

  bare.dir <- file.path(models.dir,"BARE")
  message("Making mini model in ",bare.dir)

  bare.script <- file.path(bare.dir,visioneval::getRunParameter("ScriptsDir"))
  bare.inputs <- file.path(bare.dir,visioneval::getRunParameter("InputDir"))
  bare.defs   <- file.path(bare.dir,visioneval::getRunParameter("ParamDir"))
  dir.create(bare.dir)
  dir.create(bare.script)
  dir.create(bare.inputs)
  dir.create(bare.defs)
  print( dir(bare.dir,full.names=TRUE) )

  message("Create the model configuration / run parameters")
  runConfig_ls <-  list(
      Model       = "Mini Model Test",
      Scenario    = "MiniModel",
      Description = "Minimal model constructed programmatically",
      Region      = "RVMPO",
      State       = "OR",
      BaseYear    = "2010",
      Years       = c("2010")
    )
  viewSetup(Param_ls=runConfig_ls)

  message("Save the model configuration")
  configFile <- file.path(bare.dir,"visioneval.cnf")
  cat(configFile,"\n")
  yaml::write_yaml(runConfig_ls,configFile)

  message("Make the ModelScript")
  runModelFile <- file.path(bare.script,"run_model.R")
  runModel_vc <- c(
    '', # Don't ask why, but without this line the script gets written wrong...
    'for(Year in getYears()) {',
    'runModule("CreateHouseholds","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictWorkers","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    '}'
  )
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  message("Create model geography (copying from base VERSPM)")
  base.defs <- baseModel$setting("ParamPath",shorten=FALSE)
  from <- file.path(
    base.defs,c(
      "units.csv",
      "deflators.csv",
      "geo.csv"
    )
  )
  file.copy(from=from,to=bare.defs)
  print(bare.defs)
  print(dir(bare.defs,full.names=TRUE))

  message("Open the bootstrapped mini model")
  bare <- openModel(basename(bare.dir))
  print(bare) # Still has no input files
  # But we'll use VEModel functions to tell us what the necessary inputs are.

  message("Copy over input files from baseModel (including model_parameters.json)")
  # Note this strategy won't work if the baseModel has a complex InputPath
  #   (files in stage folders or otherwise distributed)
  # In that case, you probably want to use "insider information" to copy the files using
  #   File Explorer or an equivalent tool
  base.inputs <- unique(baseModel$dir(inputs=TRUE,shorten=FALSE))
  inputs <- bare$list(inputs=TRUE,details=c("FILE"))
  required.files <- unique(file.path(base.inputs,c(inputs[,"FILE"],"model_parameters.json")))
  required.files <- required.files[which(file.exists(required.files))]
  file.copy(from=required.files,to=bare.inputs) # or copy to bare.defs...

  message("Re-open BARE model and review input files")
  bare$configure() # re-open the model after the model configuration, definitions or inputs have changed
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  required.files <- file.path(ifelse(is.na(inputs$INPUTDIR),"",inputs$INPUTDIR),inputs$FILE)
  required.exists <- file.exists(required.files)
  required.files <- sub( runtimeEnvironment()$ve.runtime, "", required.files )
  required.files <- data.frame(EXIST=ifelse(is.na(inputs$INPUTDIR),FALSE,required.exists),FILE=required.files)
  message("Required Files (all should EXIST):\n")
  print(unique(required.files))

  return(bare)
}

# ILLUSTRATE RUN-TIME OPERATIONS

print(vrb)
mini <- makeMiniModel(vrb)

# Tour the mini model structure in File Explorer
shell.exec(mini$modelPath)

# Tour the model using VEModel exploration functions

mini$dir()              # List the summary contents of the model
# TODO 8/27/2021: $dir does not work for staged models (need to handle stage directories)

mini$dir(inputs=TRUE)   # List just the model input directories
mini$dir(inputs=TRUE,all.files=TRUE) # List all the input files...

mini$run("reset")    # throw away existing results and re-run

mini$dir()           # notice presence of results directory

mini$run("save")     # move existing results into an archive and re-run
mini$dir()           # notice presence of archive directory
mini$dir(archive=TRUE) # call out just the archive directories
mini$dir(archive=TRUE,all.files=TRUE) # List all the files in the archive directories

mini$run("continue") # re-run any stage that is not "Run Complete" - does nothing here
mini$run()           # same as vr$run("continue")

