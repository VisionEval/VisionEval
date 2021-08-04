# Test.R
# Comprehensively test VEModel and related interfaces
# Also provides working examples of the API

# function: pseudo_package
# 
if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}
if ( ! requireNamespace("stringr",quietly=TRUE) ) {
  stop("Missing required package: 'stringr'")
}

# Working directory should be the package base directory
setup <- function(ve.runtime=NULL) {
  # Creates or uses a fresh minimal runtime environment as a sub-directory of "tests"
  # Set VE_RUNTIME to some other location if desired (does not need to have a runtime
  # there yet, and in fact it's better if it doesn't).
  if ( ! is.character(ve.runtime) ) {
    ve.runtime <- Sys.getenv("VE_RUNTIME",unset=NA)
    if ( ! is.na(ve.runtime ) ) {
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- NA
      }
    }
    if ( is.na(ve.runtime) ) {
      ve.runtime <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)[1]
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="tests"),winslash="/",mustWork=FALSE)
        dir.create(ve.runtime)
      }
    }
  }
  ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=TRUE)
  Sys.setenv(VE_RUNTIME=ve.runtime)
  pkgload::load_all()
  if ( ! "ve.env" %in% search() ) {
    message("Setup attaching ve.env")
    ve.env <- attach(name="ve.env",NULL)
  } else {
    ve.env <- as.environment("ve.env")
  }
  ve.env$ve.runtime <- ve.runtime # override default from package load (working directory)
  setwd(ve.env$ve.runtime)

  if ( ! dir.exists("models") ) dir.create("models")
  message("Available Test Functions:")
  print(ls(pattern="^test_",envir=parent.frame(2)))
}

ve.packages <- c(
  "VE2001NHTS",
  "VEPowertrainsAndFuels",
  "VEHouseholdTravel",
  "VEHouseholdVehicles",
  "VELandUse",
  "VEReports",
  "VEScenario",
  "VESimHouseholds",
  "VESimLandUse",
  "VESimLandUseData",
  "VESimTransportSupply",
  "VESyntheticFirms",
  "VETransportSupply",
  "VETransportSupplyUse",
  "VETravelDemandMM",
  "VETravelPerformance",
  "VEModel"
)

takedown <- function() {
  start.dir <- NA
  ve.runtime <- NA
  if ( isNamespaceLoaded("visioneval") && "ve.env" %in% search() ) {
    ve.env <- as.environment("ve.env")
    if ( exists("ve.runtime",envir=ve.env,inherits=FALSE) ) {
      ve.runtime <- ve.env$ve.runtime
    }
    if ( exists("start.dir",envir=ve.env,inherits=FALSE) ) {
      start.dir <- ve.env$start.dir
    }
  }
  if ( "package:visioneval" %in% search() ) {
    detach("package:visioneval")
  }
  if ( "ve.env" %in% search() && ! "start.dir" %in% ls("ve.env") ) {
    detach("ve.env")
    if ( exists("ve.env",inherits=FALSE) ) rm(ve.env)
  }
  for ( p in ve.packages ) {
    if ( isNamespaceLoaded(p) ) unloadNamespace(p)
  }
  unloadNamespace("visioneval")

  if ( ! is.na(start.dir) ) setwd(start.dir)
  if ( ! is.na(ve.runtime) ) {
    message("To remove runtime directory:")
    message("unlink('",ve.runtime,"',recursive=TRUE)")
  }
  loadhistory(".Rhistory") # get rid of rep("n",a.million.times) and other debugging leftovers
}

rewind <- function() {
  if ( ! "ve.env" %in% search() ) {
    message("rewind creating ve.env")
    message(getwd())
    ve.env <- attach(name="ve.env",NULL)
  } else {
    ve.env <- as.environment("ve.env")
  }
  if ( ! "start.dir" %in% ls("ve.env") ) {
    message("setting start.dir")
    ve.env$start.dir <- getwd()
  }
  cat("Rewinding...")
  takedown()
  setup()
}

cleanup <- function() {
  takedown()
  runtimes <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)
  message("Removing:")
  print(runtimes)
  if ( length(runtimes)>0 && isTRUE(askYesNo("Remove runtimes?")) ) unlink(runtimes,recursive=TRUE)
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

# Now set it all up
rewind()
