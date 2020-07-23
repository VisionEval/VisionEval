# Author: Jeremy Raw

# VisionEval helper tools
# NOTE: these all depend on having set up the VisionEval environment
# See VisionEval.R in the installation runtime (VE-Installer/boilerplate)

tool.contents <- c("openModel","verpat","verspm","vestate")

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

ve.init.model <- function(modelPath=NULL,modelName=NULL   ,...) {
  self$modelPath <- ve.model.path(modelPath)
  if ( is.null(modelName) ) {
    self$modelName <- basename(modelPath)
  } else {
    self$modelName <- modelName
  }
  # Gather defs/run_parameters.json
  if ( file.exists(rpfile <- file.path(self$modelPath[1],"defs","run_parameters.json")) ) {
    self$runParams <- jsonlite::fromJSON(rpfile)
  } else {
    stop("Cannot construct model; missing: ",rpfile)
  }
  self$stageCount <- length(self$modelPath)
  self$runStatus <- ifelse (
    file.exists(file.path(self$modelPath,"ModelState.Rda")),
    "Complete",
    "Not Started"
  )
  self$runError <- "Not Run"
}

ve.run.model <- function(verbose=TRUE) {
  pathLength <- length(self$modelPath)
  for ( ms in 1:self$stageCount ) {
    stage <- self$modelPath[ms]
    if ( verbose ) {
      message("Running model stage:")
      message(stage)
    }
    owd <- setwd(stage)
    env.model <- attach(NULL,name="ve.run.model")
    self$runError=""
    tryCatch(
      {
        self$runError <- "Running"
        sys.source("run_model.R",envir=as.environment("ve.run.model"))
        if (verbose) message("Model stage ",stage," complete")
        self$runError <- self$runStatus[ms] <- "Complete"
      },
      error = function(e) {
        message("Model stage ",stage," failed")
        msg <- as.character(e)
        if ( ! nzchar(msg) ) msg <- "Stopped."
        self$runError <- msg
        self$runStatus[ms] <- "Failed"
      },
      finally =
      {
        if ( self$runError == "Running" ) {
          self$runError <- "Failed"
        }
        if ( self$runError == "" ) {
          self$runError <- "Stopped"
        }
        if (verbose) {
          cat("Model Stage:",gsub(ve.runtime,"",stage),"\n")
          if ( self$runError != "Complete" ) cat("Error:",self$runError,"\n")
        }
        setwd(owd)
      }
    )
    if (verbose) {
      cat("Status:",self$runError,"\n")
    }
  }
  return(invisible(self$runError))
}

ve.model.dir <- function(pattern=NULL,recursive=FALSE,shorten="",path=NULL) {
  if ( is.null(path) ) path<-c(1:self$stageCount)
  files <- dir(self$modelPath[path],pattern=pattern,recursive=recursive,full.names=TRUE)
  if ( nzchar(shorten) ) files <- gsub(shorten,"",files)
  return(files)
}

confirm <- function(msg) {
  conf <- askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE
  return(conf)
}

ve.artifacts <- function(path=NULL) {
    mstates <- self$dir(pattern=".*(Previous)*ModelState\\.Rda",path=path)
    mstates <- mstates[!dir.exists(mstates)]
    dstores <- self$dir(pattern="Datastore_*",path=path)
    dstores <- dstores[dir.exists(dstores)]
    logs    <- self$dir(pattern="Log_*.*\\.txt",path=path)
    logs    <- logs[!dir.exists(logs)]
    return(c(mstates,dstores,logs))
}

ve.model.clear <- function(force=FALSE) {
  to.delete <- private$artifacts()
  if ( length(to.delete)>0 ) {
    print(gsub(ve.runtime,"",to.delete))
    if ( force || (force <- confirm("Clear ALL prior model results?")) ) {
      print(gsub(ve.runtime,"",to.delete))
      unlink(to.delete)
    } else {
      cat("Model results NOT cleared.\n")
    }
  } else {
    cat("No prior results to clear.\n")
    force = FALSE
  }
  return(invisible(force))
}

ve.path.prefix <- function(x) {
    x   <-sort(x)                          # sort the vector
    d_x <-strsplit(x[c(1,length(x))],"")   # split the first and last element by character (list of two vectors of single characters)
    pfx <-match(FALSE,do.call("==",d_x))-1 # match the first not common element and back up to last matching one
    if(is.na(pfx)) {                       # all vector elements are the same
      return(x[1])
    } else if (pfx==0) {                   # if there is no matching element, return an empty vector, else return the common part
      return(character(0))
    } else {
      return(substr(x[1],1,pfx))
    }
}

ve.model.duplicate <- function(newName=NULL,newPath=NULL) {
  if ( is.null(newPath) ) {
    if ( self$stageCount>1 ) {
      newPath <- dirname(self$modelPath)
      newPath <- ve.path.prefix(newPath)
      if ( ! dir.exists(newPath) ) newPath <- dirname(newPath) # match might extend into basename
      if ( ! nzchar(newPath) ) {
        newPath <- dirname(self$modelPath[1])
      } else {   # assume there's an embracing directory
        newPath <- dirname(newPath)
      }
    } else {
      newPath <- dirname(self$modelPath[1])
    }
  }
  if ( ! dir.exists(newPath) ) newPath <- dirname(newPath)
  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath)
  for ( p in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(path=p),private$artifacts(path=p))
    copy.to <- file.path(newModelPath,basename(self$modelPath[p]))
  #  dir.create(newModelPath)
    print(copy.to)
    dir.create(copy.to)
    file.copy(copy.from,copy.to,recursive=TRUE)
  }
  return( openModel(newModelPath,newName) )
}

ve.model.status <- function(printStatus=TRUE) {
  if ( printStatus ) print(self$runStatus)
  return(invisible(self$runStatus))
}

ve.print.model <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
  cat("Status:\n")
  self$status()
  return(NULL)
}

ve.model.dump <- function(
                       outputFolder   = "output",
                       includeTables  = character(0),  # default: include all of them
                       excludeTables  = character(0),  # default: don't exclude any
                       overwrite      = FALSE,         # TRUE to destroy outputFolder first if it exists
                       all.tables     = FALSE,         # TRUE will ignore include/exclude and export everything
                       quiet          = FALSE          # TRUE to suppress all 
                       ) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  for ( p in self$modelPath ) {
    ve.export(
      modelStateFile=p,
      outputFolder=outputFolder,
      includeTables=includeTables,
      excludeTables=excludeTables,
      overwrite=overwrite,
      all.tables=all.tables,
      quiet=quiet
    )
  }
}

# Here is VEModel R6 class

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    modelName=NULL,
    modelPath=NULL,
    stageCount=NULL,
    runParams=NULL,
    runStatus=NULL,
    runError=NULL,
    initialize=ve.init.model,
    run=ve.run.model,
    print=ve.print.model,
    status=ve.model.status,
    dir=ve.model.dir,
    clear=ve.model.clear,
    duplicate=ve.model.duplicate,
    dump=ve.model.dump
  ),
  private = list(
    artifacts = ve.artifacts
  )
)

# The following function is the only thing exported
openModel <- ve.model <- function(modelPath,modelName=NULL) {
  return( VEModel$new(modelPath=modelPath,modelName=modelName) )
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
