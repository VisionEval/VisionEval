# Test.R
# Comprehensively test visioneval interfaces (basic model runs)
# Also provides working examples of the API

# You should run this from the visioneval package root (or set start.dir to that location)
# Make sure your .libPaths includes the built ve-lib (e.g. by creating a suitable .REnviron file).

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

start.dir <- system.file(package="visioneval")

test_classic <- function(modelName="Classic",save=TRUE) {
  modelPath <- file.path("models",modelName)
  owd <- getwd()
  on.exit(setwd(owd))

  if ( ! save && dir.exists(modelPath) ) {
    testStep("Clearing runtime environment")
    unlink(modelPath,recursive=TRUE)
  } else {
    testStep("Setting up test model")
    print(list.dirs(modelPath))
  }

  if ( ! save || ! dir.exists(modelPath) ) {
    testStep(paste("Copying CLASSIC model from package as",modelName))
    dir.create(modelPath)
    file.copy(dir(file.path(start.dir,"tests","models","TestModel"),full.names=TRUE),modelPath,recursive=TRUE)
  } else cat("model directory exists or overwriting...\n")

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
  envir$ModelStateFile <- file.path(modelPath,ModelStateFile)
  RunParam_ls <- loadModelState(envir$ModelStateFile,envir=envir)
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

# Not sure what this function is for!
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

setupModelState <- function(items=list(),envir=NULL) {
  if ( is.null(envir) ) envir <- modelEnvironment()
  if ( missing("items") ) {
    return(envir$ModelState_ls)
  }
  if ( ! "ModelState_ls" %in% names(envir) ) {
    envir$ModelState_ls <- items
  } else {
    envir$ModelState_ls[names(items)] <- items
  }
  return( invisible(envir$ModelState_ls) )
}

test_deflate <- function(setupOnly=FALSE) {
  # Construct a pseudo ModelState_ls with Deflators
  Deflators <- data.frame(
    # Just reproduces deflators.csv from sample VERSPM model
    Year = c(
      1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
      2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    ),
    Value = c(
      172.6, 178, 182.4, 183.8, 186.3, 191.1, 196, 201.1, 208.556, 215.389,
      215.647, 218.344, 224.59, 229.779, 235.528, 241.215, 244.19,
      249.426
    )
  )
  setupModelState(list(Deflators=Deflators))
  if ( ! setupOnly ) {
    cat('deflateCurrency(1:10, "1999", "2016")\n')
    print(deflateCurrency(1:10, "1999", "2016"))
    cat('deflateCurrency(1:10, 1999, 2016)\n')
    print(deflateCurrency(1:10, 1999, 2016))
    cat('deflateCurrency(1:10, 2016, 1999)\n')
    print(deflateCurrency(1:10, 2016, 1999))
    cat('deflateCurrency(1:10, 1998, 2016) - 1998 not in table\n')
    print(deflateCurrency(1:10, 1998, 2016))
    cat('deflateCurrency(1:10, 1999, 2017) - future years not in table\n')
    print(deflateCurrency(1:10, 1999, 2017))
  } else {
    cat("Sample Deflators initialized\n")
  }
}

test_convertUnits <- function() {
  Units <- data.frame(
    # Just reproduces deflators.csv from sample VERSPM model
    Type = c(
      "currency", "distance", "area", "mass", "volume", "time", "energy", "people",
      "vehicles", "trips", "households", "employment", "activity"
    ),
    Units = c(
      "USD", "MI", "SQMI", "KG", "GAL", "DAY", "GGE", "PRSN", "VEH", "TRIP", "HH",
      "JOB", "HHJOB"
    )
  )
  setupModelState(list(Units=Units))
  testStep("Basic Units...")
  cat('convertUnits(1:10, "energy", "M", "MI")\n')
  print(convertUnits(1:10, "energy", "M", "MI"))
  cat('convertUnits(1:10, "distance", "meters", "miles")\n')
  print(convertUnits(1:10, "distance", "meters", "miles"))
  cat('convertUnits(1:10, "distance", "M", "miles")\n')
  print(convertUnits(1:10, "distance", "M", "miles"))
  cat('convertUnits(1:10, "double", "revenue-miles", "vehicle-miles")\n')
  print(convertUnits(1:10, "double", "revenue-miles", "vehicle-miles"))
  cat('convertUnits(1:10, "distance", "M", "MI")\n')
  print(convertUnits(1:10, "distance", "M", "MI"))
  cat('convertUnits(1:10, "distance", "M", "M")\n')
  print(convertUnits(1:10, "distance", "M", "M"))
  cat('convertUnits(1:10, "time", "YR", "DAY")\n')
  print(convertUnits(1:10, "time", "YR", "DAY"))
  testStep("Default Target...")
  cat('convertUnits(1:10, "time", "YR")\n')
  print(convertUnits(1:10, "time", "YR"))
  testStep("Compound Units...")
  cat('convertUnits(1:10, "compound", "MI/HR", "TON/MI")\n')
  print(convertUnits(1:10, "compound", "MI/HR", "TON/MI"))
  cat('convertUnits(1:10, "compound", "MI/HR", "KM/SEC")\n')
  print(convertUnits(1:10, "compound", "MI/HR", "KM/SEC"))
  cat('convertUnits(1:10, "compound", "MI/HR", "FT/SEC")\n')
  print(convertUnits(1:10, "compound", "MI/HR", "FT/SEC"))
  cat('convertUnits(1:10, "mass", "MT")\n') # into default unit of mass
  print(convertUnits(1:10, "mass", "MT"))
  cat('convertUnits(1:10, "compound", "GM/MI")\n') # into default mass / distance
  print(convertUnits(1:10, "compound", "GM/MI"))
  cat('convertUnits(1:10, "compound", "MT*KM/YR")\n') # into default mass / distance / time
  print(convertUnits(1:10, "compound", "MT*KM/YR"))
  test_deflate(setupOnly=TRUE)
  testStep("Currencies...")
  cat('convertUnits(1:10, "currency", "USD", Years=(FromYear=2010,ToYear=2008))\n')
  print(convertUnits(1:10, "currency", "USD", Years=list(FromYear=2010,ToYear=2008)))
  cat('deflateCurrency(1:10, "2010", "2008")\n')
  print(deflateCurrency(1:10, "2010", "2008"))
  testStep("Compound Currencies...")
  cat('convertUnits(1:10, "compound", "USD/YR", "USD/YR", Years=(FromYear=2010,ToYear=2008))\n')
  print(convertUnits(1:10, "compound", "USD/YR", "USD/YR", Years=list(FromYear=2010,ToYear=2008)))
}
