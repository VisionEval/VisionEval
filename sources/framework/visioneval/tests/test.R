# Test.R
# Comprehensively test visioneval interfaces (basic model runs)
# Also provides working examples of the API

# You should run this from the visioneval package root.
# Make sure your .libPaths includes the built ve-lib (e.g. by
# creating a suitable .REnviron file).
# If you're looking at this file in a built runtime, the individual test_*
# functions are code you can try interactively to test the framework.
# You should NOT run the "rewind" or "setup" functions...
# The test functions are a more detailed and up-to-date version of what the
# walkthrough does: those are the target of test-based development for the
# framework.

# starting in the root of the package, just do this
#  source("tests/test_run.r")
# It will create a temporary folder to use as a runtime
# and list the available test functions.

# TODO: if not using pkgload from source, require visioneval

if ( ! requireNamespace("stringr",quietly=TRUE) ) {
  stop("Missing required package: 'stringr'")
}

testStep <- function(msg) {
  cat("",paste(msg,collapse="\n"),"",sep="\n")
}

test_classic <- function(modelName="Classic",save=TRUE) {
  modelPath <- file.path("models",modelName)
  owd <- getwd()
  on.exit(setwd(owd))

  if ( ! save && dir.exists(modelPath) ) {
    testStep("Clearing runtime environment")
    unlink(modelPath,recursive=TRUE)
  } else {
    testStep("Should save previous Datastore")
    print(list.dirs(modelPath))
  }

  if ( !save || ! dir.exists(modelPath) ) {
    testStep(paste("Copying CLASSIC model from package as",modelName))
    dir.create(modelPath)
    file.copy(dir(file.path(start.dir,"tests","models","TestModel"),full.names=TRUE),modelPath,recursive=TRUE)
  }

  testStep("Run the model...")
  setwd(modelPath)
  message("Directories before:\n")
  for ( f in dir() ) message(f)
  source("run_model.R")
  if ( save) {
    message("Directories after:\n")
    for ( f in list.dirs() ) message(f)
  }

  invisible(NULL)
}

test_load_datastore <- function(modelName="Staged",clear=FALSE) {
  modelPath <- file.path("models",modelName)
  owd <- getwd()
  on.exit(setwd(owd))

  if ( clear && dir.exists(modelPath) ) {
    testStep("Clearing runtime environment")
    unlink(modelPath,recursive=TRUE)
  }
  if ( ! dir.exists(modelPath) ) dir.create(modelPath)

  testStep("Setting up stages...")
  modelPath.1 <- normalizePath(file.path(modelPath,"Stage-1"),winslash="/",mustWork=FALSE)
  modelPath.2 <- normalizePath(file.path(modelPath,"Stage-2"),winslash="/",mustWork=FALSE)

  testStep("Preparing Stage 1...")
  if ( ! dir.exists(modelPath.1) ) {
    dir.create(modelPath.1,recursive=TRUE)
    file.copy(dir(file.path(start.dir,"tests","models","StagedModel","Stage-1"),full.names=TRUE),modelPath.1,recursive=TRUE)
    testStep("Running Stage 1...")
    setwd(modelPath.1)
    source("run_model.R")
  }

  testStep("Preparing Stage 2...")
  if ( dir.exists(modelPath.2) ) unlink(modelPath.2,recursive=TRUE)
  dir.create(modelPath.2)
  file.copy(dir(file.path(start.dir,"tests","models","StagedModel","Stage-2"),full.names=TRUE),modelPath.2,recursive=TRUE)

  testStep("Running Stage 2 (Load Stage 1 Datastore)...")
  setwd(modelPath.2)
  source("run_model.R")

  invisible(NULL)
}

test_deep_copy <- function(modelName="Classic",log="info") {
  testStep("Setting up Datastore to copy (do test_classic first)")
  initLog(Save=FALSE,Threshold=log,Clear=TRUE)
  modelPath <- normalizePath(file.path("models",modelName),winslash="/",mustWork=TRUE)
  ModelStateFile <- getRunParameter("ModelStateFile",Param_ls=list()) # Default "ModelState.Rda"
  envir <- modelEnvironment(Clear="")
  envir$ModelStatePath <- file.path(modelPath,ModelStateFile)
  RunParam_ls <- loadModelState(envir$ModelStatePath,envir=envir)
  ToDir <- file.path(modelPath,"TestDeepCopy")
  testStep("Creating target directory...")
  if ( dir.exists(ToDir) ) {
    unlink(ToDir,recursive=TRUE)
  }
  dir.create(ToDir)
  testStep("Copying Datastore, forcing Flatten")
  owd <- setwd(modelPath) # so we can find the datastore to copy
  on.exit(setwd(owd))     # return to original directory even on failure
  visioneval::copyDatastore(ToDir,Flatten=c(TRUE,TRUE))
  setwd(owd)
  testStep("Resulting directories...")
  print(list.dirs(modelPath,recursive=TRUE))
}

extract_bzone <- function(modelDir,Year) {
  # modelDir is the location that holds the model run Datastore
  # Years is the year to analyze (e.g. 2010 or "2050" - either number
  # or character)
  setwd(modelDir)
  complete <- list()
  Datasets <- c("Household/Bzone","Household/IsUrbanMixNbrhd")
  Datafiles <- file.path("Datastore",Year,paste0(Datasets,".Rda"))
  results <- list()
  my.env <- new.env()
  for ( rs in seq_along(Datasets) ) {
    results[[Datasets[rs]]] <- get(load(Datafiles[rs],envir=my.env),pos=my.env)
  }
  results <- as.data.frame(table(as.data.frame(results)))
  results <- merge(results[results[2]=="0",],results[results[2]=="1",],by="Household.Bzone",suffixes=c(".No",".Yes"))[,-c(2,4)]
  results$PctMixedUse = results[3] / results[2] * 100.0
  return(results)
}
