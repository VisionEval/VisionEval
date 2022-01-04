# Load required packages

if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}
if ( ! requireNamespace("jsonlite",quietly=TRUE) ) {
  stop("Missing required package: 'jsonlite'")
}
if ( ! requireNamespace("yaml",quietly=TRUE) ) {
  stop("Missing required package: 'yaml'")
}

# future.callr also loads future
# required for multitasking test
if ( ! requireNamespace("future.callr",quietly=TRUE) ) {
  stop("Missing required package: 'future.callr'")
}

if ( ! "package:VEModel" %in% search() ) {
  # Will be there already if running from test_setup.R inside VEModel package
  message("Loading built VEModel package")
  require("VEModel",quietly=TRUE)
}

logLevel <- function(log="info") {
  visioneval::initLog(Save=FALSE,Threshold=log)
}

testStep <- function(msg) {
  cat("",paste(msg,collapse="\n"),"",sep="\n")
}

stopTest <- function(msg) {
  stop(msg)
}

# TODO throughout: use test_install() as the standard method for creating
# models. ALlow the user to keep earlier test runs (just create a name variant)
# or to blow away what is already there (runs and all).

# Check that we can source run_model.R to run a classic model
test_classic <- function(modelName="VERSPM-Classic",clear=TRUE,log="info") {

  if ( ! missing(log) ) logLevel(log)

  modelPath <- file.path("models",modelName)
  owd <- getwd()
  on.exit(setwd(owd))

  if ( dir.exists(modelPath) && clear ) {
    testStep("Clearing model to start from scratch")
    unlink(modelPath,recursive=TRUE)
  } else {
    testStep("Re-running existing installation")
  }

  if ( ! dir.exists(modelPath) ) {
    testStep(paste("Installing classic VERSPM model from package as",modelName))
    rs <- installModel("VERSPM",variant="classic",installAs=modelName,log=log,confirm=FALSE)
    modelName <- rs$modelName
    rm(rs)  # Don't keep the VEModel around
  }

  testStep(paste("Running",modelName,"by sourcing scripts/run_model.R"))

  setwd(modelPath)
  require(visioneval) # Put it on the search path for GetYears, RunModule, etc
  source("run_model.R")
  detach("package:visioneval") # But leave the namespace loaded

  testStep("Reviewing model status")

  setwd(owd)
  rs <- openModel(modelName)
  cat("Model Status:",rs$printStatus(),"\n")
  return(rs)
}

test_install <- function(modelName="VERSPM",variant="base",installAs="",log="info") {

  if ( ! missing(log) ) logLevel(log)

  if ( ! nzchar(variant) ) variant <- ""
  if ( missing(installAs) || ! nzchar(installAs) ) {
    if ( nzchar(variant) && nzchar(modelName) ) {
      installAs <- paste0("test-",modelName,"-",variant)
    }
  }

  if ( nzchar(modelName) && nzchar(variant) && nzchar(installAs) ) {
    # TODO: create modelExists function? Still want to make people work to delete a model...
    if ( dir.exists(file.path("models",installAs)) ) {
      testStep(paste0("Clearing previous installation at ",installAs))
      unlink(file.path("models",installAs), recursive=TRUE)
    }

    testStep(paste("Installing",modelName,"model from package as",installAs))
    rs <- installModel(modelName,installAs,variant,log=log,confirm=FALSE,overwrite=TRUE)
  } else {
    if ( nzchar(modelName) ) {
      testStep(paste0("Directory of available variants for ",modelName))
    } else {
      testStep("Directory of available models")
    }
    rs <- installModel(modelName=modelName,variant="")
  }
  # NOTE: pkgload bug - print.VEAvailableModels/Variants not recognized for class dispatch...
  if ( "VEAvailableModels" %in% class(rs) ) {
    print.VEAvailableModels(rs)
  } else if ( "VEAvailableVariants" %in% class(rs) ) {
    print.VEAvailableVariants(rs)
  } else {
    print(rs)
  }
  return(invisible(rs))
}

test_all_install <- function(overwrite=FALSE,log="warn") {

  if ( ! missing(log) ) logLevel(log)

  testStep("Installing all models")
  models   <- test_install("")               # List available models by package
  models   <- unique(models$Model)           # Vector of available model names
  variants <- list(                          # List available variants
    "VERSPM"   =test_install("VERSPM",var=""),
    "VERPAT"   =test_install("VERPAT",var=""),
    "VE-State" =test_install("VE-State",var="")
  )
  variants <- lapply(variants,function(v) v$Variant)

  installed <- sapply(
    models,
    function(m) {
      cat("\nInstalling model",m,"\n")
      vars <- variants[[m]]
      mods <- lapply(
        vars,
        function(v) {
          cat("\nInstalling model",m,"variant",v,"\n")
          model <- installModel(m,variant=v,confirm=FALSE,overwrite=overwrite)
          return(model)
        }
      )
      names(mods) <- sapply(mods,function(m)m$modelName)
      return(mods)
    }
  )
  names(installed) <- models
  return(installed)
}

test_flatten <- function(log="info") {
  testStep("run staged model with database path")
  vr <- openModel("VERSPM-pop")
  vr$run(log=log) # use "continue" - won't re-run if already ucla luskin ocnference center
  print(vr)
  testStep("prepare receiving directory")
  ToDir <- normalizePath("testFlatten")
  if ( dir.exists(ToDir) ) unlink(ToDir,recursive=TRUE)
  dir.create(ToDir)
  ms <- tail(vr$modelStages,1)[[1]]
  assign("ModelState_ls",ms$ModelState_ls,envir=visioneval::modelEnvironment(Clear="test_flatten"))
  modelPath <- ms$RunPath
  owd <- setwd(modelPath) # so we can find the datastore to copy
  on.exit(setwd(owd))     # return to original directory even on failure
  print( c(ls(visioneval::modelEnvironment()),modelPath,ToDir) )
  visioneval::copyDatastore(ToDir,Flatten=c(TRUE,TRUE))
  setwd(owd)
}

test_run <- function(modelName="VERSPM-base",baseModel="VERSPM",variant="base",reset=FALSE,log="info") {

  if ( ! missing(log) ) logLevel(log)
  model.dir <- dir("models")
  if ( missing(modelName) || ! nzchar(modelName) ) {
    return(model.dir)
  }
  if ( ! modelName %in% model.dir ) {
    reset <- TRUE
    rs <- test_install(modelName=baseModel,variant=variant,installAs=modelName,log="info")
  }

  if ( ! reset ) {
    testStep(paste("Attempting to re-open existing",modelName))
    rs <- openModel(modelName)
    if ( rs$overallStatus != codeStatus("Run Complete") ) {
      print(rs)
      reset <- TRUE
      message("\nStage is not Complete; Rebuilding model")
    } else {
      message("Returning existing model run")
    }
  }
  if (reset) {
    rs <- openModel(modelName)
    testStep(paste("Running model",rs$modelName))
    rs$run(run="reset",log=log) # clears results directory
  }
  return(rs)
}

# Test model runs through basic model configuration
# oldstyle creates defs/run_parameters.json; if not oldstyle, create visioneval.cnf
# reset forces the base model to rebuild (no special reason to do that; the base model
#  need not have been run).
# modelName is the particular variant of VERSPM to use as a base. The base model MUST
#  be a version of VERSPM since we use its first two modules to create the "Bare" model
# log="warn" will confine to a streamlined list of log messages like what a regular user
#  would see. "info" gives lots of gory details.
test_model <- function(modelName="JRSPM", oldstyle=FALSE, reset=FALSE, log="info", brief=FALSE) {

  if ( ! missing(log) ) logLevel(log)

  cat("*** Test Model Management Functions ***\n")
  options(warn=2) # Make warnings into errors...

  testStep("open (and maybe run) the full test version of VERSPM")
  due.to <- "reset request"
  if ( modelName %in% openModel() ) {
    jr <- openModel(modelName)
    if ( ! jr$overallStatus == codeStatus("Run Complete") ) {
      reset = TRUE
      due.to = paste("status",jr$printStatus())
    }
  } else {
    reset = TRUE
    due.to = paste(modelName,"doesn't exist")
  }
  if ( isTRUE(reset) ) {
    cat("Re-running model due to",due.to,"\n")
    jr <- test_run(modelName=modelName,baseModel="VERSPM",variant="base",reset=TRUE,log=log)
  }
  if (! "VEModel" %in% class(jr) ) {
    return(jr)
  } else print(jr,details=TRUE)

  testStep("Gather base model parameters")
  base.dir <- jr$modelPath
  cat("Base Model directory:\n")
  print(base.dir)

  cat("Base model structural directories - including stages\n")
  for ( stage in jr$modelStages ) {
    cat("Stage:",stage$Name,"\n")
    jrParam_ls <- stage$RunParam_ls
    cat("  ParamPath    :",visioneval::getRunParameter("ParamPath",Param_ls=jrParam_ls),"\n")
    cat(paste("  InputPath    :",visioneval::getRunParameter("InputPath",Param_ls=jrParam_ls),"\n"))
    cat("  InputDir     :",visioneval::getRunParameter("InputDir",Param_ls=jrParam_ls),"\n")
    cat(paste("  DatastorePath:",visioneval::getRunParameter("DatastorePath",Param_ls=jrParam_ls),"\n"))
  }

  testStep("Construct a bare model from scratch, borrowing from base model")
  bare.dir <- file.path("models","BARE")
  if ( dir.exists(bare.dir) ) {
    cat("Blowing away existing bare model.\n")
    unlink(bare.dir,recursive=TRUE)
  }
  dir.create(bare.dir)

  testStep("Create minimal run_model.R")

  # NOTE: VEModel does not require, but will process, the "initializeModel() function
  runModelFile <- file.path(bare.dir,"run_model.R")
  runModel_vc <- c(
    '',
    'for(Year in getYears()) {',
    'runModule("CreateHouseholds","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictWorkers","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    '}'
  )
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  testStep("Set up model directory structure.")

  # Borrow model geography, units, deflators from base model
  bare.defs <- file.path(bare.dir,"defs")
  bare.inputs <- file.path(bare.dir,"inputs")
  dir.create(bare.defs)
  dir.create(bare.inputs)
  print(dir(bare.dir,recursive=TRUE,full.names=TRUE,include.dirs=TRUE))

  testStep(paste0("Create configuration: ",if (oldstyle) "defs/run_parameters.json" else "visioneval.cnf"))

  # Create run_model.R script (two variants)
  # Create model-specific configuration
  # TODO: does the "unboxing" also work for yaml?
  runConfig_ls <-  list(
      Model       = jsonlite::unbox("BARE Model Test"),
      Scenario    = jsonlite::unbox("Test"),
      Description = jsonlite::unbox("Minimal model constructed programmatically"),
      Region      = jsonlite::unbox("RVMPO"),
      BaseYear    = jsonlite::unbox("2010"),
      Years       = c("2010") #, "2038")
    )

  if ( oldstyle ) {
    cat("Old style model setup (defs/run_parameters.json)")
    configFile <- file.path(bare.defs,"run_parameters.json")
    write(jsonlite::toJSON(runConfig_ls, pretty=TRUE),configFile)
    print(bare.defs)
    print(configFile)
    cat(readLines(configFile),sep="\n")
  } else {
    configFile <- file.path(bare.dir,"visioneval.cnf")
    yaml::write_yaml(runConfig_ls,configFile)
    print(bare.dir)
    print(configFile)
    cat(readLines(configFile),sep="\n")
  }

  testStep("Copy other configuration files (geo, units, deflators)")

  base.defs <- jr$setting("ParamPath",shorten=FALSE)
  from <- file.path(base.defs,c("units.csv","deflators.csv","geo.csv"))
  file.copy(from=from,to=bare.defs)
  print(bare.defs)
  print(dir(bare.defs,full.names=TRUE))

  testStep("Open BARE model using defaults (no inputs yet)")
  bare <- openModel("BARE",log=log)

  testStep("List model inputs (only)...")

  # NOTE: though the specs will have an "INPUTDIR" column, it will be "NA"
  #   if the file does not exist on the model's InputPath (which is the case
  #   for the bare model).

  base.inputs <- unique(jr$dir(inputs=TRUE)) # List short names of input paths for each stage
  cat("Base Inputs",base.inputs,"\n")
  base.inputs <- unique(jr$dir(inputs=TRUE,shorten=FALSE)) # Now get the full path name for inputs
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  cat("Input Directories (should be NA - files don't exist yet):\n")
  print( unique(inputs[,"INPUTDIR"]) )
  # INPUTDIR will be NA since files don't exist
  # INPUTDIR is where it actually found the files,
  #   versus INPUTPATH (which is all the places they might be)
  required.files <- unique(file.path(base.inputs,inputs[,"FILE"]))
  required.files <- required.files[which(file.exists(required.files))]
  cat("Required Files:\n")
  print(basename(required.files))

  testStep("Copy model parameters to 'inputs' - could also be in 'defs')")

  # Inputs for sample modules: CreateHouseholds and PredictWorkers
  # Historically, model_parameters.json was in ParamDir; the framework will
  #   look in both ParamDir and InputDir; sample model has it in InputDir
  from <- file.path(base.inputs,"model_parameters.json")
  file.copy(from=from,to=bare.inputs) # or copy to bare.defs...
  print(dir(bare.inputs))

  testStep(paste("Copy required input files from",jr$modelName))

  print(required.files)
  from <- required.files
  file.copy(from=from, to=bare.inputs )
  print(bare.inputs)
  print(dir(bare.inputs,full.names=TRUE))

  testStep("Re-open the bare model")
  bare$configure()

  testStep("List the inputs again: this time showing directory")
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  print(inputs)
  required.files <- file.path(inputs$INPUTDIR,inputs$FILE)
  print(unique(required.files))
  required.files <- data.frame(EXISTS=file.exists(required.files),FILE=required.files)
  cat("Required Files (all should EXIST):\n")
  print(unique(required.files))

  testStep("run the bare model")

  bare$run() # no results yet - it will try to 'continue' then 'reset' if not 'Complete'

  if ( brief ) {
    testStep("Skipping deeper tests")
    return(bare)
  }

  print(bare$dir(results=TRUE))
  cat("Log path for the initial bare model run:\n")
  print(bare$log(shorten=FALSE))

  testStep("run the bare model again with 'save'")

  updateSetup(
    bare$modelStages[[1]],
    Source="test.R/test_model()",
    Scenario="Run with save",
    Description="This run will save prior results"
  )
  bare$run(run="save") # should generate a results archive
  print(bare$dir(root=TRUE,results=TRUE,archive=TRUE))
  cat("Log path should be different from the previous run:\n")
  print(bare$log())

  testStep("run (really DON'T run) the bare model again with 'continue'")

  updateSetup(
    bare$modelStages[[1]],
    Source="test.R/test_model()",
    Scenario="Run with 'continue'",
    Description="This run should not do anything"
  )
  bare$run(run="continue") # examine last run status and don't run if "Complete"
  print(bare$dir(root=TRUE,results=TRUE))
  cat("Log path should be the same as previous run:\n")
  print(bare$log())
  
  testStep("run the bare model with 'reset'")

  updateSetup(
    bare$modelStages[[1]],
    Source="test.R/test_model()",
    Scenario="Run with 'reset'",
    Description="This run should rebuild current results but not change archived list"
  )
  bare$run(run="reset") # Should regenerate just the unarchived results
  cat("Results should still have one saved version plus the current results:\n")
  print(bare$dir(results=TRUE))
  cat("Log path should be new compared to latest run:\n")
  print(bare$log())

  testStep("list all fields in bare model - Inp/Get/Set")
  flds <- bare$list(inputs=TRUE,outputs=TRUE,details=c("INPUTDIR","FILE"))
  flds$INPUTDIR[!is.na(flds$INPUTDIR)] <- basename(flds$INPUTDIR[!is.na(flds$INPUTDIR)]) # Just to keep it from spilling over...
  print(nrow(flds))
  print(flds[sample(nrow(flds),10),])

  testStep("extract model results, show directory")
  br <- bare$results()
  br$extract(prefix="BareTest")

  cat("Directory:\n")
  print(bare$dir(output=TRUE,all.files=TRUE))

  testStep("clear the bare model extracts")
  cat("Interactive clearing:\n")
  bare$clear(force=!interactive())

  testStep("model after clearing outputs...")
  print(bare$dir())

  testStep("clear results as well...")
  bare$clear(force=!interactive(),outputOnly=FALSE) # default is FALSE if no outputs exist - delete results
  print(bare$dir())

  testStep("copy a model (includes results and outputs)")
  cp <- bare$copy("BARE-COPY")
  print(cp)
  print(cp$dir())

  testStep("Forcibly clear results from model copy")
  cp$clear(force=TRUE,outputOnly=FALSE) # forcibly removes outputs and results
  print(cp$dir())

  testStep("Break the run_model.R script in the copy and observe failure")
  runModelFile <- file.path(cp$modelPath,"run_model.R")
  runModel_vc[4] <- 'runModule("BorrowHouseholds","VESimHouseholds",RunFor="AllYears",RunYear=Year)'
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)
  result <- try( cp$run() ) # Should throw error message about missing module...
  print(result)

  testStep("Display log from failed run...")
  logs <- cp$log(shorten=FALSE)
  for ( log in logs ) {
    cat("Log file",log,"\n")
    cat(readLines(log),sep="\n")
  }
  
  testStep("remove model results")
  cat("Directory before...\n")
  print(cp$dir(all.files=TRUE))
  cp$clear(force=TRUE,outputOnly=FALSE,archives=TRUE)
  cat("\nDirectory after...\n")
  print(cp$dir(all.files=TRUE))

  testStep("Delete model in file system")
  print(cp)
  print(dir("models"))
  cat("Unlinking",cp$modelName,"\n")
  unlink(file.path("models",cp$modelName),recursive=TRUE)
  cat("Is",cp$modelName,"still present?\n")
  print(dir("models"))

  testStep("directory still accessible?")
  print(cp$dir())
  rm(cp)

  testStep("return bare model")
  return(bare)
}

test_multicore <- function(model=NULL, log="info", workers=3) {

  logLevel(log)

  testStep("Finding BARE model template")
  if ( is.null(model) ) {
    model <- test_model("JRSPM",brief=TRUE,log=log) # skip the deeper tests, return BARE model
  } else {
    print(model)
  }

  testStep("Copy model...")
  modelPath <- file.path("models","CORE-test")
  if ( dir.exists(modelPath) ) unlink(modelPath,recursive=TRUE)
  coreModel <- model$copy("CORE-test",copyResults=FALSE,log=log)
  print(coreModel)

  testStep("Create CORE-base stage")
  updateSetup(coreModel,inFile=TRUE,drop=c("Scenario","Description"))
  writeSetup(coreModel,overwrite=TRUE)
  owd <- setwd(coreModel$modelPath)
  file.rename(coreModel$setting("InputDir"),"CORE-base")
  stageConfig_ls <-  list(
      Scenario    = jsonlite::unbox("CORE-base"),
      Description = jsonlite::unbox("Base Stage for Multicore Test")
    )
  configFile <- file.path("CORE-base","visioneval.cnf")
  yaml::write_yaml(stageConfig_ls,configFile)
  setwd(owd)
  coreModel$configure(fromFile=TRUE) # Will load CORE-base stage
  print(coreModel)

  testStep("Run the CORE-test model inline")
  coreModel$plan("inline")   # "sequential" maps over to the same thing
  coreModel$run()            # should look identical to test_model run and BARE-test
  # In particular, you should see the Logfile scrolling past "live"
  print(coreModel)

  testStep("Add model stages (just duplicates) to run in parallel")
  # To run in parallel, different stages must have the same "StartFrom"

  # For this test, we'll restructure the CORE-test model so it has three identical
  # stages, each of which just runs the base model over and over. We'll get
  # CORE-base, Stage-1, Stage-2, Stage-3 and Stage-4 as sub-directories of
  # coreModel$modelPath/ResultsDir.
  for ( newstage in 1:4 ) {
    coreModel$addstage(
      Name=paste0("Simultaneous-",newstage),
      Dir=paste0("Stage-",newstage),
      Scenario=paste0("Simultaneous ",newstage),
      Description=paste("Run stage",newstage,"in parallel"),
      StartFrom="CORE-base"
    )
  }
  cat("Show model with new stages\n")
  print(coreModel)

#   testStep("Run model with sequential to check")
#   coreModel$plan("inline")
#   coreModel$run("continue")
# 
#   stopTest("Done checking stage runs")

  logLevel(log=log)

  testStep("Run model with callr")
  cat("Running with callr plan",workers, "workers; should work on any R version\n")
  coreModel$plan("callr",workers=workers)
  coreModel$run("continue") # Use existing Core-base stage run
  print(coreModel)
  # then will run each of the new stages in parallel (asynchronously)

  testStep("Run model with multisession")
  coreResults <- try ( {
    cat("Historically, multisession fails on Windows machines due to firewall restrictions.\n")
    cat("Recent tests on a pretty locked-down Windows 10 suggest it may now work.\n")
    coreModel$plan("multisession",workers=workers)
    coreModel$run("reset") # Will re-run CORE-base stage
    coreModel
  } )
  print(coreResults)
  testStep("Done with multitasking test")

  return(invisible(coreModel))
}

test_load <- function(model=NULL, log="info" ) {
  # Tests the LoadModel functionality (pre-load Datastore and then
  #   execute additional steps from the copied data.all
  testStep("Finding BARE model template")
  if ( is.null(model) ) {
    model <- test_model("JRSPM",brief=TRUE,log=log) # skip the deeper tests
    model$run(log=log)                      # run it anyway (default="continue" does nothing)
  }
  testStep("Copy model...")
  modelPath <- file.path("models","LOAD-test")
  if ( dir.exists(modelPath) ) unlink(modelPath,recursive=TRUE)
  loadModel <- model$copy("LOAD-test",copyResults=FALSE)
  print(loadModel)
  testStep("Set up load script...")
  baseModelPath <- loadModel$setting("ModelDir",shorten=FALSE)
  runModelFile <- file.path(
    baseModelPath,
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
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  testStep("Configure LoadModel")
  # We'll just do it from scratch rather than reading/modifying
  # Region, BaseYear and Years don't change
  runConfig_ls <-  list(
    Model       = jsonlite::unbox("LOAD Model Test"),
    Scenario    = jsonlite::unbox("Test LoadModel / LoadDatastore"),
    Description = jsonlite::unbox("Add a step onto a different previous model"),
    Region      = jsonlite::unbox(model$setting("Region")),
    BaseYear    = jsonlite::unbox(model$setting("BaseYear")),
    Years       = loadModel$setting("Years"),
    LoadModel   = jsonlite::unbox(model$modelPath) # could be any form accepted by openModel
    # Could also set LoadStage (last Reportable stage is used by default)
  )
  configFile <- file.path(baseModelPath,"visioneval.cnf")
  yaml::write_yaml(runConfig_ls,configFile)

  testStep("Reload model with LoadDatastore")
  loadModel <- openModel(basename(loadModel$modelPath),log=log)

  testStep("Copy additional inputs")
  base.model <- openModel("JRSPM")
  base.inputs <- file.path(
    base.model$setting("InputPath",shorten=FALSE),
    base.model$setting("InputDir")
  )
  cat("Base Inputs",base.inputs,"\n")
  inputs <- loadModel$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  required.files <- unique(file.path(base.inputs,inputs[,"FILE"]))
  required.files <- required.files[which(file.exists(required.files))]
  cat("Required Files:\n")
  print(basename(required.files))

  testStep("Copying additional input files")
  bare.inputs <- file.path(
    loadModel$setting("InputPath",shorten=FALSE),
    loadModel$setting("InputDir")
  )
  testStep("Remove base model inputs - will just have new ones")
  unlink(bare.inputs,recursive=TRUE)
  dir.create(bare.inputs)
  from <- required.files
  file.copy(from=from, to=bare.inputs )

  testStep("Copy model parameters to 'inputs' - could also be in 'defs')")
  from <- file.path(base.inputs,"model_parameters.json")
  file.copy(from=from,to=bare.inputs) # or copy to bare.defs...
  print(dir(bare.inputs))

  testStep("Run model, loading datasore")
  loadModel$run("reset",log=log)
  return(loadModel)
}

test_results <- function (log="info") {

  if ( ! missing(log) ) logLevel(log)

  testStep("Manipulate Model Results in Detail")

  testStep("Copy model and get 'results' and 'selection' from empty model...")
  jr <- openModel("JRSPM")
  if ( "COPY" %in% dir("models") ) unlink("models/COPY",recursive=TRUE)
  cp <- jr$copy("COPY")
  cat("Directory before clearing...\n")
  print(cp$dir())
  cp$clear(force=TRUE,outputOnly=FALSE)
  cat("Directory after clearing...\n")
  print(cp$dir())
  cat("Results after clearing... (Generates error)\n")
  rs <- cp$results()
  print(rs)
  cat("Selection after clearing...\n")
  sl <- rs$select()
  print(sl)
  rm(cp)

  testStep("Pull out results and selection from jr (head 12)...")
  cat("Results...\n")
  rs <- jr$results()  # Gets results for final Reportable stage (only)

  # TODO: rs may be a list of VEResults (not just a single object)?
  # Use case is mostly for doing queries over a set of scenarios...
  # Return a list if jr$results(all.stages=TRUE) or jr$results(stages=c(stage1,stage2)) with
  # length(stages)>1 : all reportable stages in that case.
  # An individual stage can also be called out explicitly (and in that case, it does not
  #   need to be Reportable).

  print(rs)
  cat("Selection...\n")
  sl <- rs$select() # Get full field list
  print(head(capture.output(print(sl)),n=12))

  # Do some basic field extraction - list fields
  cat("Groups\n")
  print(sl$groups())
  cat("Tables\n")
  print(sl$tables())
  cat("Fields (random 20)\n")
  fld <- sl$fields()
  print(fld[sample(length(fld),20)])
  
  # Select some subsets by group, table or field name and extract those...
  # Can we easily identify group names, table names, field names and zero in on selecting them?
  testStep("Select some Groups")
  cat("Only the years...\n")
  sl$select( sl$find(Group="Years") )
  print(sl$groups())
  cat("Only the first of the years...\n")
  sl$select( sl$groups()[1] )         # Just the first ones
  print(sl$groups())

  testStep("Select Household and Vehicle Tables")
  sl$select( sl$find(Table=c("Household","Vehicle")) )
  print(sl)
  cat("Print selection with details...\n")
  print(head(capture.output(print(sl,details=TRUE)),n=12))
  
  # Test display units, select speeds, create unit conversion
  testStep("Creating and Writing Display Units...")
  sl$all() # Deselect everything
  un <- rs$list(details=TRUE)[,c("Group","Table","Name","Units")]
  spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
  spd$DisplayUnits <- "MI/HR"
  cat("Writing display_units.csv into ")
  display_units_file <- file.path(
      jr$modelPath,
      visioneval::getRunParameter("ParamDir",Param_ls=jr$RunParam_ls),
      visioneval::getRunParameter("DisplayUnitsFile",Param_ls=jr$RunParam_ls)
    )
  cat(display_units_file,"\n")
  write.csv(spd,file=display_units_file)

  testStep("Selecting speed fields...")
  sl$all() # re-select everything
  sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
  print(sl$fields())

  testStep("Showing currently defined UNITS/DISPLAYUNITS (via sl$results)")
  print(sl$results$units())
  testStep("Showing currently defined UNITS/DISPLAYUNITS (directly from rs)")
  print(rs$units())

  # Clean up the fields to add the geography fields in the Marea Table
  testStep("Adding geography fields to selection...")
  sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
  print(sl$fields())
  print(rs$units())

  testStep("Extracting speed fields using DISPLAY units")
  sl$extract(prefix="DisplayUnits")                 # Using DISPLAY units

  testStep("Exporting speed fields using DATASTORE units")
  sl$export(prefix="Datastore",convertUnits=FALSE)  # Using DATASTORE units

  testStep("Model directory")
  print(jr$dir())

  testStep("Model directory of results")
  print(jr$dir(results=TRUE))
  
  testStep("Model directory of outputs")
  print(jr$dir(outputs=TRUE))
  
  testStep("Interactively clear outputs but leave results")
  jr$clear(outputOnly=TRUE, force=FALSE)

  testStep("Directory after clearing")
  jr$dir()
}

# TODO: make separate functions (not flag) for
#   (1) build/manipulate query object versus
#   (2) run query
test_query <- function(log="info",build.query=TRUE,break.query=TRUE,run.query=TRUE,reset=FALSE) {
  # Process the standard query list for the test model
  # If multiple==TRUE, copy the test model and its results a few times, then submit the
  # list of all the copies to VEQuery. Each column of results will be the same (see
  # test_scenarios for a run that will generate different results in each column).

  testStep("Set up Queries and Run on Model Results")
  testStep("Opening test model and caching its results...")
  jr <- test_run("VERSPM-query",baseModel="VERSPM",variant="pop",log=log,reset=reset)
  rs <- jr$results()

  testStep("Show query directory (may be empty)...")
  print(jr$query())

  if ( ! ( build.query || break.query || run.query ) ) {
    testStep("No query tests requested; returning VERSPM-query model")
    return(jr)
  }

  if ( build.query ) {
    testStep("Create an empty query object and print it...")
    # create a query object
    qry <- jr$query("Test-Query",load=FALSE) # Don't open it if file exists already
    cat("Query valid:",qry$valid(),"\n")
    cat("Print qry$checkResults:"); print(qry$checkResults)
    cat("Print query\n")
    print(qry)

    testStep("Add a query specification formulated as a list element...")
    spec <- list(
      Name = "UrbanHhDvmt",
      Summarize = list(
        Expr = "sum(UrbanHhDvmt)",
        Units = c(
          UrbanHhDvmt = "MI/DAY",
          Marea = ""
        ),
        By = "Marea",
        Table = "Marea"
      ),
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in the urban area"
    )
    qry$add(spec)
    qry$print(details=TRUE)

    testStep("Names of specifications in added query...")
    print(qry$names())    # List names of QuerySpecifications in order
    testStep("Print function for added queries...")
    print(qry)

    testStep("Re-add a query at the beginning of the list")
    print(qry)
    spec <- VEQuerySpec$new(spec)
    spec <- spec$update(Name="UrbanHhDvmt_before")
    cat("Adding spec:\n")
    print(spec)
    qry$add(spec,before=TRUE) # Should be placed at location=1 (first element); existing list after
    cat("Before goes at beginning\n")
    print(qry)
    spec <- VEQuerySpec$new(spec)
    spec <- spec$update(Name="UrbanHhDvmt_loc2")
    qry$add(spec,location=2,before=TRUE) # should put loc2 in between "before" and original
    cat("loc2 goes between 'before' and original\n")
    print(qry)
    spec <- VEQuerySpec$new(spec)
    spec <- spec$update(Name="UrbanHhDvmt_loc45")
    qry$add(spec,location=45) # should put loc45 at the end
    cat("loc45 goes at end\n")
    print(qry)
    spec <- VEQuerySpec$new(spec)
    spec <- spec$update(Name="UrbanHhDvmt_loc0")
    qry$add(spec,location=2,before=TRUE)  # should put loc0 "after" first element: 2nd position
    cat("loc0 goes after first element\n")
    print(qry)

    testStep("Remove test specifications...")
    cat("Removing:\n")
    print( nm <- qry$names()[1:3] )
    qry$remove(nm) # remove by name (bye-bye before,loc2 and loc0)
    print(qry)
    cat("Removing:\n")
    print(c("2",qry$names()[2]))
    qry$remove(2) # remove by position (bye-bye loc45)
    print(qry)
  } else {
    qry <- jr$query("Test-Query",load=FALSE) # Don't open it if file exists already
  }
  if ( build.query && break.query ) {
    testStep("Make a new VEQuery and add various bad specifications to it (not implemented)...")
    # TODO: Throw some additional specific broken queries at it to see if errors are correct.
    # TODO: destroy that object once we're done abusing it.
  }
  if ( ! run.query ) {
    testStep("Not running query - returning it for further exploration")
    return(qry)
  } else {
    # TODO: split into further testing of query construction
    # plus second element to efficiently create a correct query and run it.
    testStep("Build a query from scratch and run it.")
    testStep("Construct bare query specification...")
    spec <- VEQuerySpec$new()
    cat("Bare query is valid (FALSE): ")
    print(spec$valid())   # Should return FALSE
    print(spec)

    testStep("Add spec details to bare query using $update...")
    spec$update(
      Name = "UrbanHhDvmt_MixNbrhd",
      Description = "Daily vehicle miles traveled by households residing in mixed use in the urban area",
      Units = "Miles per day", # Purely advisory...
      Summarize = list(
        Expr = "sum(Dvmt[LocType == 'Urban' & IsUrbanMixNbrhd == '1'])",
        Units = c(
          Dvmt = "MI/DAY",        # Will force to this unit, with conversion if needed
          LocType = "",           # Leaving it blank says use Datastore default
          IsUrbanMixNbrhd = "",
          Marea = ""
        ),
        By = "Marea",
        Table = "Household"
      )
    )
    cat("Updated query is valid (TRUE): ")
    print(spec$valid())   # Should return TRUE
    print(spec)

    testStep("Add updated spec to Query and print...")
    qry$add(spec)
    print(qry)

    testStep("Print again with details...")
    print(qry,details=TRUE)

    testStep("Complete the initial query by adding more 'Summarize' specs...")

    # Just load a list of specifications straight into the query
    spec <- list(
      list(
        Name = "UrbanVanDvmt",
        Summarize = list(
          Expr = "sum(VanDvmt)",
          Units = c(
            VanDvmt = "MI/DAY",
            Marea = ""
          ),
          By = "Marea",
          Table = "Marea"
        ),
        Units = "Miles per day",
        Description = "Daily vehicle miles traveled by on-demand transit vans in the Urban area."
      ),
      list(
        Name = "UrbanComSvcDvmt",
        Summarize = list(
          Expr = "sum(ComSvcUrbanDvmt)",
          Units = c(
            ComSvcUrbanDvmt = "MI/DAY",
            Marea = ""
          ),
          By = "Marea",
          Table = "Marea"
        ),
        Units = "Miles per day",
        Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the urban area"
      ),
      list(
        Name = "UrbanHhDvmt",
        Summarize = list(
          Expr = "sum(UrbanHhDvmt)",
          Units = c(
            UrbanHhDvmt = "MI/DAY",
            Marea = ""
          ),
          By = "Marea",
          Table = "Marea"
        ),
        Units = "Miles per day",
        Description = "Daily vehicle miles traveled by households residing in the urban area"
      )
    )
    qry$add(spec)

    print(qry)
    qry$add(spec,location=1,after=TRUE)
    print(qry)

    testStep("Create a 'Function' query specification...")

    spec <- VEQuerySpec$new()
    spec$update(QuerySpec=list(
        Name = "UrbanLdvDvmt",
        Function = "UrbanHhDvmt + UrbanVanDvmt + UrbanComSvcDvmt",
        Units = "Miles per day",
        Description = "Sum of daily vehicle miles traveled in the urban area"
      )
    )
    cat("Function spec is valid (TRUE):"); print(spec$valid())
    print(spec)

    testStep("Add the Function spec to the query...")

    print(qry)
    qry$add(spec)
    print(qry)

    testStep("Clear test queries, if any...")
    qfiles <- jr$query()

    print(qfiles <- file.path(jr$modelPath,"queries",qfiles))
    unlink(qfiles)
    print(jr$query())

    testStep("Save the query and fix its extension...")

    qry$save() # as Test-Query.VEqry
    cat("Saved values in original query...\n")
    cat("Name; "); print(qry$QuerydName)
    cat("Path: "); print(qry$QueryFile)
    cat("Directory: "); print(qry$QueryDir)
    print(dir(qry$QueryDir))

    if ( build.query ) { # Interior elaborate test of copying/ssving queries
      testStep("Save a copy of the query and fix its extension...")

      qry2 <- qry$copy("Copy-Query.R") # .R will be removed from the name
      qry2$save() # Essentially as "Save As"
      cat("Saved values in renamed query...\n")
      cat("Directory: "); print(qry2$QueryDir)
      cat("Name; "); print(qry2$QueryName)
      cat("Path: "); print(qry2$QueryFile)
      cat("Contents of copied query...\n")
      print(qry2)

      testStep("Model QueryDir contents...")

      cat("Expecting "); print(c("Copy-Query.VEqry","Test-Query.VEqry"))
      print(jr$query())

      testStep("Save a query somewhere else...")
      qfile <- file.path(jr$modelPath,"queries","Dump-Query.R")
      qry2$save(qfile)
      print(jr$query())

      testStep("Save a query without overwriting...")
      actualFile <- qry2$save(overwrite=FALSE)
      qfile <- c(qfile,actualFile)
      print(jr$query())
      unlink(qfile); rm(qry2)

      testStep("Open the query by short name in a different object from the file...")

      runqry <- jr$query("Test-Query")
      cat("Loaded query...\n")
      cat("Directory: "); print(runqry$QueryDir)
      cat("Name; "); print(runqry$QueryName)
      cat("Path: "); print(runqry$QueryFile)
      print(runqry)

      testStep("Open the query again from the file, using full file name...")

      runqry <- jr$query("Test-Query.VEQry")
      cat("Re-Loaded query with name extension...\n")
      cat("Directory: "); print(runqry$QueryDir)
      cat("Name; "); print(runqry$QueryName)
      cat("Path: "); print(runqry$QueryFile)
      print(runqry)
      rm(runqry)
    }

    testStep("Run the query on the model...")
    qry$run(jr) # using original query above

    testStep("Display query results...")
    # TODO: put a class on the query results and push the following into a print method
    rs <- qry$results()
    cat("Number of query results:",length(rs),"\n")
    for ( r in seq(length(rs)) ) {
      cat("Result #",r,"\n")
      result <- rs[[r]]
      cat("  Path:",result$Path)
      cat("  Results:",paste(names(result$Results),collapse=", "),"\n")
      cat("  ModelStage Results:",result$Source$Name,"\n")
    }

    testStep("Extract query results into data.frame")
    df <- query.results <- qry$extract() # Constructs the data.frame of query results
    # TODO: should the data.frame be cached (not just the raw results?)
    print(names(df))
    print(df)

    testStep("Extract query results into .csv file (default name)")
    df <- qry$export(format="csv")
    df <- qry$export() # Does the same thing again, possibly overwriting
    # Each extract creates a new file with a different timestamp, but
    # the timestamps only differ by minutes: figure out how that works.

    testStep("Export query results into explicitly named .csv file")
    qry$export(format="csv",SaveTo=paste0("TestQuery_%timestamp%",qry$Name))

    testStep("Show output files, which will include exports and queries")
    jr$dir(outputs=TRUE,all.files=TRUE)

    testStep("Run the query again on the bare results (should do nothing)...")
    rs <- jr$results()
    qry$run(rs) # Won't re-run if query results are up to date with the scenario runs

    testStep("Force the query to run on the bare results rather than the model...")
    qry$run(rs,Force=TRUE) # Won't re-run if query is up to date

    testStep("Returning extracted query data.frame for further exploration")
    return(df)
  }
}

#TODO: this function is probably obsolete (thought it is useful for describing
#  how to programmatically build scenarios...). Look at test_multicore function
#  and perhaps update that.
test_multiquery <- function(reset=FALSE,log="info") {
  # Merge this with test_scenario
  if ( ! missing(log) ) logLevel(log)
  testStep("Acquiring test model")
  jr <- test_run("VERSPM-query",baseModel="VERSPM",variant="pop",reset=reset)
  print(jr)
  qry <- jr$query("Test-Query")
  print(qry)

  testStep("Query multiple scenarios...")
  # Generate several copies of jr future year
  # Inputs will be sought up the "StartFrom" tree.
  # To customize inputs for Scenario-1 (as an actual scenario),
  #   create InputDir ("inputs") inside stagePath.1 and put in
  #   just the files you want to change. Without such an input,
  #   it just uses the inputs found in earlier stages. For the
  #   purposes of testing the query functionality, it suffices here
  #   to have all the scenarios be the same.
  stagePath.1 <- file.path(jr$modelPath,"Scenario-1")
  if ( ! dir.exists( stagePath.1 ) ) dir.create(stagePath.1)
  jr$addstage(
    Name="Scenario-1",
    Dir="Scenario-1",
    ModelDir=jr$modelPath, # TODO: Why do we need this?
    # TODO: InputPath = NULL, # from one of the category test scenarios
    Scenario="Scenario 1",
    Description="Same as original...",
    StartFrom="stage-pop-future",
    BaseYear=jr$setting("BaseYear",stage="stage-pop-future"),
    Years=jr$setting("Years",stage="stage-pop-future"),
    ModelScript=jr$setting("ModelScript",stage="stage-pop-future")
  )
  stagePath.2 <- file.path(jr$modelPath,"Scenario-2")
  if ( ! dir.exists( stagePath.2 ) ) dir.create(stagePath.2)
  jr$addstage(
    Name="Scenario-2",
    Dir="Scenario-2",
    ModelDir=jr$modelPath,
    # TODO: InputPath = NULL, # from a different category test scenario
    Scenario="Scenario 2",
    Description="Same as original...",
    StartFrom="stage-pop-future",
    BaseYear=jr$setting("BaseYear",stage="stage-pop-future"),
    Years=jr$setting("Years",stage="stage-pop-future"),
    ModelScript=jr$setting("ModelScript",stage="stage-pop-future")
  )

  # Force Reportable on stage-pop-future (auto-detect says no since
  # the other scenarios start from it).
  jr$modelStages[["stage-pop-future"]]$Reportable=TRUE

  # NOTE: without an InputDir or a different InputPath or different Script, this will just re-run
  # the model using all the inputs from stage-pop-future and put the results in this stage's output
  # directory.
  testStep("Running two additional scenarios")
  jr$run() # runs with "continue" - will just do the newly added stages

  # TODO: restructure multi-scenario model to push those scenarios into ModelStages and use
  #   the scenarios StartFrom to establish the default case.

  testStep("Query the model")
  # Make a list of VEResults objects from the VEModel list and run query on it
  qry$run(jr,OutputFile="%queryname%_FromModel_%timestamp%")

  testStep("Query the results list explicitly (should use cached query results)")
  # Make a list of ResultsDir path names (i.e. list of character strings) from the
  # VEResults and query that (Note difference between a character vector - list of
  # model names and a list of character strings, which are the result paths).
  qry$run(jr$results(),OutputRoot=jr$modelResults,OutputFile="%queryname%_FromResultList_%timestamp%.csv")

  testStep("Done with multiple queries")
}

# TODO: once scenario testing is complete, add a test for the visualizer (writing to file
# and also launching with jrc), check that we can do VERPAT all the way (set up the
# queries in the new structure, set up the category scenarios variant, and make sure
# it can run all the way through (model run, query generation on single scenario,
# category scenario generation, visualizer on multiple scenarios)
test_rpat <- function(run=TRUE) {
  testStep("Testing VERPAT as JRPAT")
  verpat <- openModel("JRPAT")
  if ( ! verpat$valid() ) {
    testStep("Installing VERPAT as JRPAT")
    verpat <- installModel("VERPAT",installAs="JRPAT")
  }
  if ( run || ! verpat$results()$valid() ) {
    testStep("Clearing previous extracts")
    verpat$clear(force=TRUE) # outputs only
    testStep("Running JRPAT")
    verpat$run()
  }
  testStep("Extracting JRPAT results...")
  verpat$results()$extract()
}

# Test the setup management functions
test_setup <- function(model=NULL) {
  testStep("Parameter defaults...")
  visioneval::defaultVERunParameters()

  testStep("Runtime setup (none to start)")
  conf.file <- file.path(runtimeEnvironment()$ve.runtime,"visioneval.cnf")
  if ( file.exists(conf.file) ) unlink(conf.file)

  testStep("Load ve.runtime configuration")
  getSetup(reload=TRUE)

  testStep("View initial setup (empty list)")
  viewSetup(fromFile=TRUE)

  testStep("Install test model (based on runtime configuration)...")
  setup <- if ( dir.exists(file.path("models","Setting-Test")) ) {
    openModel("Setting-Test")
  } else {
    installModel("VERSPM","Setting-Test",variant="pop")
  }
  print(setup)

  testStep("Initial model configuration")
  viewSetup(setup)
  cat("Seed setting in model:",setup$setting("Seed"),"\n")
  testStep("Remove Seed parameter from model configuration file")
  updateSetup(setup,inFile=TRUE,drop="Seed")
  writeSetup(setup,overwrite=TRUE)
  testStep("Reload model with Seed parameter from default")
  setup$configure()
  cat("Value of Seed:",setup$setting("Seed",defaults=TRUE),"\n")
  cat("Setting source...\n")
  print(setup$setting("Seed",defaults=TRUE,source=TRUE)) # Show source for Seed parameter: now default
  viewSetup(setup)

  testStep("Update Seed in ve.runtime setup")
  updateSetup(inFile=TRUE,Seed=2.5)

  testStep("View runtime setup with changed Seed")
  viewSetup(fromFile=TRUE)

  testStep("Writing new runtime visioneval.cnf")
  writeSetup() # creates a backup file if setup already exists

  testStep("Runtime setup file now has seed")
  cat("Runtime configuration previously loaded:\n")
  viewSetup(fromFile=FALSE) # Still has old value of Seed in global runtime
  getSetup(reload=TRUE) # force reload of runtime configuration
  cat("Runtime configuration after reloading from file:\n")
  viewSetup(fromFile=FALSE) # Now has reloaded file value in regular runtime

  testStep("Reopen model and see changed setup")
  setup$configure() # Reopen the model from saved configuration
  cat("Seed setting in model:",setup$setting("Seed"),"\n")
  cat("Setting source...\n")
  print(setup$setting("Seed",defaults=TRUE,source=TRUE)) # Show source for Seed parameter: now default
  unlink(conf.file) # Don't leave the runtime visioneval.cnf around
}

test_scenarios <- function(
  useStages=TRUE,
  run=useStages,
  querySpec="VERSPM-scenarios",
  install=FALSE,
  log="info"
) {
  logLevel(log)

  testStep(paste("Checking and installing scenario model"))
  existingModel <- dir.exists(modelPath <- file.path("models","VERSPM-scenario"))
  if ( install || ! existingModel ) {
    if ( install && existingModel ) unlink(modelPath,recursive=TRUE)
    cat( sep="", if ( existingModel) "Existing" else "Installing",":\n", modelPath, "\n" )

    # useStages==TRUE will do the model stage scenario, otherwise combinations
    if ( ! existingModel  ) scenarioVariant <- if (useStages) "scenarios-ms" else "scenarios-cat"
    mod <- installModel("VERSPM-scenario",variant=scenarioVariant,modelName="VERSPM",log=log,confirm=FALSE)
  } else {
    cat(sep="","Using existing model:\n",modelPath,"\n")
    mod <- openModel("VERSPM-scenario")
  }

  if ( run ) {
    testStep("Running model...")
    mod$run()             # does nothing if existing model has "run complete status"
  }
  if ( mod$printStatus() != "Run Complete" ) {
    stopTest("Scenario model is not yet run")
  }

  testStep("Loading scenario query")
  qr <- mod$query(querySpec) # Fails if model has not been run
  qf <- qr$QueryFile
  print(qr)
  cat("QueryFile:",qf,"\n")

  testStep("Running Query")
  qr$run(Force=TRUE)
  print(qr)

  testStep("Examine Query Results")
  qrr <- qr$results()
  print(class(qrr))
  print(length(qrr))

  testStep("Extracting Query Results")
  qrs <- qr$extract()
  print(qrs)

  testStep("Exporting Query Results")
  qr$export()

  testStep("Returning scenario model")
  print(mod,scenarios=TRUE)
  return(invisible(list(
    Model=mod, Query=qr, QueryFile=qf, QueryResults=qrs
  )))
}

test_visual <- function(categories=TRUE,popup=FALSE,reset=FALSE,log="info") {

  # Models need to have been created with test_install...
  modelName <- if ( categories ) "test-VERSPM-scenarios-cat" else "test-VERSPM-scenarios-ms"
  testStep(paste("Opening Model:",modelName))
  mod <- openModel(modelName,log="warn") # presume the model opening is uncomplicated
  print(mod)
  testStep("Running Model")
  mod$run()
  testStep("Opening Query")
  logLevel(log) # Now examine at requested log level
  qr <- mod$query( mod$query() )
  print(qr)
  testStep(paste0("Running Query, Force=",reset))
  results <- qr$run(Force=reset)
  testStep("Extracting Query (prints data.frame)")
  extract <- qr$extract(metadata=FALSE,exportOnly=TRUE)
  print( extract )
  testStep("Building visual data")
  jsonvars <- qr$visual(QueryResults=extract) # pure extraction return jsonvars
  if ( popup ) {
    testStep("Launching jrc visualizer")
    qr$visual(SaveTo=NULL) # save to sub-directory of ResultsDir/OutputDir
  } else {
    testStep("Writing file-system visualizer")
    qr$visual(SaveTo=TRUE) # save to sub-directory of ResultsDir/OutputDir
  }
  invisible( jsonvars )
}
