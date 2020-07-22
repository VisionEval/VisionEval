# Author: Jeremy Raw

# VisionEval helper tools
# NOTE: these all depend on having set up the VisionEval environment
# See VisionEval.R in the installation runtime (VE-Installer/boilerplate)

tool.contents <- c("ve.model","verpat","verspm","vestate")

requireNamespace("jsonlite")
requireNamespace("R6")

# Function: ve.model.path
# Determine if parameter is a list of locations of run_model.R riles
# First parameter is character vector of directories that may contain
# run_model.R
# Check first whether modelPath contains directories or full paths
# to run_model.R (if the latter, replace with dirnames)
# If modelPath contains directories, each one must have a run_model.R
# If only one directory, and it does not contain run_model.R, look
# for a staged model scenario - so find all the subdirectories and
# verify that each has a run_model.R.

ve.model.path <- function(modelPath=NULL) {
  # Check how modelPath specifies a run_model.R file
  # Does modelPath make sense (absolute or relative to getwd())?
  if ( is.null(modelPath) ) stop("Must provide model path locator.\n")
  if ( ! all( file.exists(modelPath) ) ) {
    modelPath = file.path(ve.runtime,"models",modelPath)
    if ( ! all( dir.exists(modelPath) ) ) {
      stop("Could not locate model directory for [",paste(modelPath,dir.exists(modelPath),sep=":",collapse=","),"]")
    }
  }
  # Figure out if we can use modelPath to find "run_model.R" script(s)
  if ( all ( file.exists(modelPath) & toupper(basename(modelPath))=="run_model.R" ) ) {
    # Provided full path to run_model.R (possibly more than one)
    modelPath <- dirname(modelPath)
  } else if ( ! all( file.exists( file.path(modelPath,"run_model.R") ) ) ) {
    if ( length(modelPath)==1 ) {
      # Check for staged model (must be single root directory)
      subs <- dir(modelPath,full.names=TRUE)
      modelPath <- subs[dir.exists(subs) & file.exists(file.path(subs,"run_model.R"))]
      if ( length(modelPath)==0 ) {
        stop("No run_model.R in [",paste(modelPath,collapse=","),"]")
      }
    } else {
      stop("Could not locate run_model.R for [",paste(modelPath,collapse=","),"]")
    }
  }
  normalizePath(modelPath,winslash="/",mustWork=TRUE)
}

ve.init.model <- function(modelPath=NULL,...) {
  self$modelPath <- ve.model.path(modelPath)
  # Gather defs/run_parameters.json
  if ( file.exists(rpfile <- file.path(self$modelPath[1],"defs","run_parameters.json")) ) {
    self$runParams <- jsonlite::fromJSON(rpfile)
  } else {
    stop("Cannot construct model; missing: ",rpfile)
  }
}

ve.run.model <- function() {
  for ( modelstage in self$modelPath ) {
    message("Running model stage:\n",modelstage)
    owd <- setwd(modelstage)
    source("run_model.R")
    setwd(owd)
  }
}

ve.status.model <- function() {
  step.stat <- sapply(self$modelPath,
    function(p) {
      ms <- file.path(p,"ModelState.Rda")
      ds <- file.path(p,self$runParams$DatastoreName)
      if ( file.exists(ms) && file.exists(ds) ) "Run Complete" else "NOT Run yet"
    }
  )
  names(step.stat) <- basename(self$modelPath)
  cat("Model status:\n")
  print(step.stat)
}

ve.print.model <- function() {
  cat("Model Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
  self$status()
}

# Here is VEModel R6 class

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    modelPath=NULL,
    runParams=NULL,
    initialize=ve.init.model,
    run=ve.run.model,
    print=ve.print.model,
    status=ve.status.model
  ),
)

ve.model <- function(modelPath) {
  return( VEModel$new(modelPath=modelPath) )
}

# The following functions run the command line model versions per the
# Getting Started document.  Optional "scenarios" argument, if TRUE, will
# run the scenarios version of the test models.

verpat <- function(Scenarios=FALSE) {
  if ( ! scenarios ) {
    model <- ve.model("VERPAT")
  } else {
    model <- ve.model("VERPAT_Scenarios")
  }
  model$run()
  model
}

verspm <- function(Scenarios=FALSE,MM=FALSE,VehAdj=FALSE) {
  if ( scenarios ) {
    model <- ve.model("VERSPM_Scenarios")
  } else if ( mm ) {
    model <- ve.model("VERSPM_MM")
  } else if ( withAdj ) {
    model <- ve.model("VERSPM_VehAdj")
  } else {
    model <- ve.model("models/VERSPM")
  }
  model$run()
  model
}

vestate <- function(staged=FALSE) {
  if ( ! staged ) {
    model <- ve.model("VE-State")
  } else {
    model <- ve.model(dir(pattern=".*Stage-\\d",file.path(ve.runtime,"models","VE-State-Staged"),full.names=TRUE))
  }
  model$run()
  model
}
