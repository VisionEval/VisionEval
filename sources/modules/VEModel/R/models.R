# Author: Jeremy Raw

# VEModel Package Code

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

self=private=NULL

# Function: ve.model.path
# Use the modelPath parameter to find run_model.R files
# modelPath is a character vector of directories that may contain run_model.R
#
# 1. Is run_model.R mentioned explicitly in each element of the vector of paths?
#    If so, reduce modelPath to just the elements that contain run_model.R
#        Then replace the vector with the dirnames and return those
#    If not, proceed to step 2
# 2. Does modelPath contain existing directories?
#    Are modelPath absolute (check first one)? If so examine each for run_model.R and if found, return modelPath
#        If not found, examine first-level subdirectories of each listed path, and include the absolute path of all
#          of those that contain run_model.R
#        If run_model.R is not found, throw an error "no model found at modelPath"
#    If modelPath are relative (check first one), try finding run_model.R by normalizing as follows:
#      A. If ve.runtime exists:
#         Look relative to ve.runtime/models if ve.runtime exists
#         Look relative to ve.runtime (directly)
#         If run-model not found directly, also consider first-level sub-directories as with absolute paths
#      B. If ve.runtime does not exist, or run_model.R not found, repeat A. using getwd() instead of ve.runtime
# 3. If directories are still not found and dirname(modelPath) is an empty string, conduct standard model search
#    If dirname(modelPath) is not empty, throw an error "no model found at modelPath"
#    If modelPath contains more than one element, throw an error "no model found on modelPath"
#    Search for models within extdata directory (system.file); but also search in package tree in case we're running
#      the source interactively for testing.
#    If no matching model exists, throw an error "no model found at modelPath"
#    If a matching model exists, if "confirm" is TRUE, conduct a dialog asking if the user wants to install the
#      standard model named in modelPath. Otherwise, use the "skeleton" parameter (TRUE/FALSE) to install either
#      (TRUE) the standard model skeleton files (bare inputs/defs) or (FALSE) the full sample model
#    Then attempt to install the standard model

## Helper
confirmDialog <- function(msg) {
  conf <- utils::askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE # Cancel is the same as No
  return(conf)
}

## Helper
#  return TRUE if modelPath looks like an absolute file path
isAbsolutePath <- function(modelPath) {
  # TODO: may need a more robust regular expression
  any(grepl("^([[:alpha:]]:|/)",modelPath))
}

## Helper
#  Model roots: ve.runtime/models, getwd()/models, ve.runtime, getwd()
getModelRoots <- function(get.root=0) {
  roots <- c( getwd() )
  if ( exists("ve.runtime") ) {
    roots <- c( get("ve.runtime"), roots )
  }
  # VEModelPath is an optional directory in which to seek or put models
  # Hierarchy of places:
  #    VEModelPath (if defined and exists)
  #      if relative, check ve.runtime/VEModelPath and getwd()/VEModelPath
  #    ve.runtime/models (if exists)
  #    getwd()/models (if exists)
  #    ve.runtime
  #    getwd()
  modelPath <- getOption("VEModelPath")
  if ( ! is.null(modelPath) ) {
    modelPath <- modelPath[1]
    if ( ! isAbsolutePath(modelPath) ) {
      test.paths <- normalizePath(file.path(roots,modelPath))
      modelPath <- test.paths[dir.exists(test.paths)]
      if ( length(modelPath)==0 || ! nzchar(modelPath[1]) ) {
        modelPath <- NULL
      } else {
        modelPath <- modelPath[1]
      }
    }
  }
  roots <- c( modelPath, file.path(roots,"models"), roots)
  if ( get.root > length(roots) ) get.root <- 1
  if ( get.root>0 ) return(roots[get.root]) else return(roots)
}

## Helper
#  Get unique file name based on newName in folder newPath
#  NewPath is the directory it should go in, newName is the name to disambiguate
getUniqueName <- function(newPath,newName) {
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  return (newModelPath)
}

## Helper function
#  Look for run_model.R (any case) in root or subdirectories and return full paths
run.model <- "run_model.R"
modelInRoot <- function(root) {
  model.spec <- paste0(run.model,"$")
  paths <- grep(model.spec,ignore.case=TRUE,dir(root,full.names=TRUE,recursive=TRUE),value=TRUE)
  dirname(paths)
}

## Helper function
#  Examine modelPath and return (sub-)directories containing run_model.R
findModel <- function( modelPath=NULL ) {

  # Does modelPath explicitly mention run_model.R?
  if ( is.null(modelPath) ) stop("Must provide modelPath locator.")
  if ( all( runmodel <- grepl(modelPath,"run_model.R",ignore.case=TRUE) ) ) {
    return( normalizePath(dirname(modelPath(runmodel)),winslash="/") )
  }

  # No run_model.R, so we'll presume modelPath describes directories
  # Check for run_model.R in absolute paths
  if ( isAbsolutePath(modelPath) ) {
    # Check recursively for possible stages in subdirectories
    paths <- modelInRoot(modelPath)
    if ( length(paths)>0 ) {
      return(paths)
    } else {
      stop("No run_model.R in [",paste(modelPath,dir.exists(modelPath),sep=":",collapse=","),"]")
    }
  }
  
  # modelPath is a relative path, so check relative to "roots" (VEModelPath, ve.runtime, getwd())
  roots <- getModelRoots()
  for ( root in roots ) {
    paths <- modelInRoot(file.path(root,"models",modelPath))
    if ( length(paths)>0 ) {
      return(paths)
    }
  }

  # No run_model in modelPath relative to "roots" - look for standard model perhaps
  # If we have more than a bare name in modelPath, we've failed
  stop("No run_model.R in [",paste(modelPath,dir.exists(modelPath),sep=":",collapse=","),"]")
}

## Helper
#  Look up a standard model
#  model is bare name of standard model
findStandardModel <- function( model ) {
  standardModels <- system.file("models",package="VEModel")
  if ( is.null(model) || ! nzchar(model) ) return( dir(standardModels) )

  model <- model[1]
  if ( ! nzchar(standardModels) || ! model %in% dir(standardModels) ) {
    standardModels <- getOption("VEStandardModels",default=normalizePath("inst/models"))
  }
  model <- file.path(standardModels,model)
  if ( ! dir.exists(model) ) {
    stop(
      "No standard model called ",model,"\n",
      "installModel() to list available models"
      )
  }
  return(model)
}

## install a standard model either as a template (skeleton==TRUE) or
## a sample (skeleton==FALSE)
#  We're still expecting to distribute with standard models pre-installed
#  Called automatically from findModel, where modelPath must be a bare model name
#  Can install from other locations by calling this function with a more elaborate modelPath

SampleModelDataFormat <- c( templ="Template",samp="Sample" )

installStandardModel <- function( modelName, modelPath, confirm, skeleton ) {
  # Locate and install standard modelName into modelPath
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first available standard root
  #   If modelPath is an existing directory, put modelName into it (conflict-resolved name)
  #   If modelPath does not exist, but dirname(modelPath) exists, create new directory and put the model there
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and they have to try again
  model <- findStandardModel( modelName )
  if ( is.null(modelName) || ! nzchar(modelName) ) return(model) # Vector of available standard model names

  # Set up destination modelPath
  root <- getModelRoots(1)
  if ( missing(modelPath) || is.null(modelPath) ) modelPath <- modelName
  if ( ! isAbsolutePath(modelPath) ) {
    installPath <- normalizePath(file.path(root,modelPath),winslash="/",mustWork=FALSE)
  }
  if ( dir.exists(installPath) ) {
    installPath <- getUniqueName( dirname(installPath), basename(modelPath) )
  }

  # Confirm installation if requested
  install <- TRUE
  skeleton <- if ( skeleton ) "templ" else "samp"
  if ( confirm && interactive() ) {
    msg <- paste0("Install standard model '",modelName,"' (",SampleModelDataFormat[skeleton],") in ",installPath,"?\n")
    install <- confirmDialog(msg)
  }

  if ( ! install ) stop("Model ",modelName," not installed.",call.=FALSE)

  # Now do the installation
  message("Installing ",modelName," from ",model," as ",SampleModelDataFormat[skeleton])
  dir.create(installPath)

  # Locate the model and data source files
  model.path <- file.path(model,"model")
  model.files <- dir(model.path,full.names=TRUE)

  data.path <- file.path(model,skeleton)
  if ( ! dir.exists(data.path) ) stop("No ",SampleModelDataFormat[skeleton]," available for ",modelName)
  data.files <- dir(data.path,full.names=TRUE)

  file.copy(model.files,installPath,recursive=TRUE) # Copy standard model into modelPath
  file.copy(data.files,installPath,recursive=TRUE) # Copy skeleton data into modelPath
  message("Installed ",modelName," in ",installPath)

  return( list(modelName=modelName,modelPath=installPath) )
}

ve.model.copy <- function(newName=NULL,newPath=NULL) {
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
  } else {
    if ( ! dir.exists(newPath) ) newPath <- dirname(newPath)
  }

  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- getUniqueName(newPath,newName)

  get.destination <- if ( self$stageCount == 1 ) {
    function(modelPath,...) modelPath
  } else {
    function(modelPath,basenameStage) file.path(modelPath,basenameStage)
  }
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath,showWarnings=FALSE)
  for ( p in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(path=p),private$artifacts(path=p))
    copy.to <- get.destination(newModelPath,basename(self$modelPath[p]))
    dir.create(copy.to,showWarnings=FALSE)
    file.copy(copy.from,copy.to,recursive=TRUE)
  }
    
  return( openModel(newModelPath,newName) )
}

loadModelState <- function(path) {
  ms.env <- new.env()
  if ( ! grepl("ModelState\\.Rda$",path) ) path <- file.path(path,"ModelState.Rda")
  if ( file.exists(path) ) {
    model.state <- visioneval::readModelState(FileName=path, envir=ms.env)
  } else {
    model.state <- list()
  }
  rm(ms.env)
  return(model.state)
}

# Initialize a VEModel from modelPath/modelName
ve.model.init <- function(modelPath=NULL,modelName=NULL) {

  # Identify the run_model.R root location
  self$modelPath <- findModel(modelPath)

  # The remainder sets up the components for this model management structure
  print(self$modelPath)
  names(self$modelPath) <- basename(self$modelPath)
  if ( is.null(modelName) ) {
    self$modelName <- if ( length(self$modelPath)>1 ) {
       # default modelName for multi-stage model is basename of
       # the directory containing the first stage subdirectory
      basename( dirname(self$modelPath[1]) )
    } else { # For a one-stage model it is the basename of the first path itself
      names(self$modelPath)[1]
    }
  } else {
    self$modelName <- modelName
  }

  # TODO: make this work with the new initializeModel(RunMode="Load") option

  # Gather defs/run_parameters.json
  if ( file.exists(rpfile <- file.path(self$modelPath[1],"defs","run_parameters.json")) ) {
    self$runParams <- jsonlite::fromJSON(rpfile)
  } else {
    stop("Cannot construct model; missing: ",rpfile)
  }
  self$stageCount <- length(self$modelPath)
  private$ModelState <- lapply(
    self$modelPath,
    loadModelState
  )
  if ( length(private$ModelState)>0 && any(unlist(lapply(private$ModelState,length))>0) ) {
    private$outputObject <- VEOutput$new(private$ModelState,self)
  }

  self$runStatus <- sapply(
    simplify=TRUE,
    private$ModelState,
    function(ms) {
      if ( length(ms) > 0 ) {
        ifelse(
          "runStatus" %in% names(ms),
          ms$runStatus,
          "Prior Run"
        )
      } else {
        "Not Run"
      }
    }
  )
  self$status <- self$runStatus[length(self$runStatus)]
}

log.level <- function(level) {
  legal.levels <- c(
    "DEBUG"=futile.logger::DEBUG,
    "ERROR"=futile.logger::ERROR,
    "FATAL"=futile.logger::FATAL,
    "INFO"=futile.logger::INFO,
    "TRACE"=futile.logger::TRACE,
    "WARN"=futile.logger::WARN
  )
  if ( level %in% names(legal.levels) ) {
    return(legal.levels[level])
  } else {
    return(legal.levels["ERROR"])
  }
}

ve.model.run <- function(verbose=TRUE,path=NULL,stage=NULL,log="ERROR") {
  # Unlike .dir the path/stage says where to start - the run will
  # then continue by running that stage then each following stage
  if ( missing(path) ) path <- stage    # Still might be NULL; allow alias
  if ( ! exists("ve.runtime") ) ve.runtime <- getwd()
  pathLength <- length(self$modelPath)
  stageStart <- if ( ! is.null(path) ) path else 1
  for ( ms in stageStart:self$stageCount ) {
    stage <- self$modelPath[ms]
    if ( verbose ) {
      message("Running model stage:")
      message(stage)
    }
    owd <- setwd(stage)
    if ( ! "ve.model" %in% search() ) {
      envir <- attach(NULL,name="ve.model")
    } else {
      envir <- as.environment("ve.model")
    }
    self$status <- ""
    futile.logger::flog.threshold(log.level(log))
    tryCatchLog::tryCatchLog(
      {
        self$status <- "Running"
        sys.source("run_model.R",envir=envir)
        if (verbose) message("Model stage ",stage," complete")
        self$status <- self$runStatus[ms] <- "Complete"
      },
      error = function(e) {
        message("Model stage ",stage," failed")
        msg <- as.character(e)
        if ( ! nzchar(msg) ) msg <- "Stopped."
        self$status <- msg
        self$runStatus[ms] <- "Failed"
      },
      finally =
      {
        if ( self$status == "Running" ) {
          self$status <- "Failed"
        }
        if ( self$status == "" ) {
          self$status <- "Stopped"
        }
        if (verbose) {
          cat("Model Stage:",gsub(get("ve.runtime"),"",stage),"\n")
          if ( self$status != "Complete" ) cat("Error:",self$status,"\n")
        }
        model.state.path <- file.path(self$modelPath[ms],"ModelState.Rda")
        if ( file.exists(model.state.path) ) {
          visioneval::setModelState(
            list(runStatus=self$runStatus[ms]),
            FileName=model.state.path
          )
        }
        setwd(owd)
      }
    )
    if (verbose) {
      cat("Status:",self$status,"\n")
    }
    if ( self$status != "Complete" ) break;
  }
  private$ModelState <- lapply(
    self$modelPath,
    loadModelState
  )
  if ( length(private$ModelState)>0 && all(unlist(lapply(private$ModelState,length))>0) ) {
    private$index()
  }

  return(invisible(self$status))
}

ve.model.dir <- function(pattern=NULL,recursive=FALSE,shorten="",path=NULL,stage=NULL) {
  # path/stage can be a vector of discrete stages (e.g. c(1,3); only
  # those will be inspected.
  if ( missing(path) ) path <- stage
  if ( is.null(path) ) path<-c(1:self$stageCount)
  files <- dir(self$modelPath[path],pattern=pattern,recursive=recursive,full.names=TRUE)
  if ( nzchar(shorten) ) files <- gsub(shorten,"",files)
  return(files)
}

# outputOnly will just report the extraction results
# (not the model run)
ve.artifacts <- function(path=NULL,stage=NULL,outputOnly=FALSE) {
  if ( missing(path) ) path <- stage
  if ( ! outputOnly ) {
    mstates <- self$dir(pattern=".*(Previous)*ModelState\\.Rda",path=path)
    mstates <- mstates[!dir.exists(mstates)]
    dstores <- self$dir(pattern="Datastore_*",path=path)
    dstores <- dstores[dir.exists(dstores)]
    logs    <- self$dir(pattern="Log_*.*\\.txt",path=path)
    logs    <- logs[!dir.exists(logs)]
    artifacts <- c(mstates,dstores,logs)
  } else artifacts <- character(0)
  outputs <- self$dir(pattern="output",path=path)
  return(c(artifacts,outputs))
}

ve.model.clear <- function(force=FALSE,outputOnly=NULL,path=NULL,stage=NULL) {
  if ( missing(path) ) path <- stage
  if ( missing( outputOnly ) ) {
    # If "output" exists in any stage, only offer to clear outputs
    # unless the user manually overrides. Makes it harder to
    # accidentally delete a model run.
    outputOnly = any( dir.exists( file.path(self$modelPath,"output") ) )
  }
  to.delete <- private$artifacts(path=path,outputOnly=outputOnly)
  if ( length(to.delete)>0 ) {
    to.delete <- gsub(paste0("^",getwd(),"/"),"",to.delete)
    print(to.delete)
    if ( interactive() ) {
      choices <- to.delete
      preselect <- if (force || outputOnly ) to.delete else character(0)
      to.delete <- utils::select.list(choices=choices,preselect=preselect,multiple=TRUE,title="Delete Select Outputs")
      force <- length(to.delete)>0
    } else {
      force <- ( force || length(to.delete)>0 )
    }
    if ( force) {
      print(dir(to.delete),recursive=TRUE)
      unlink(to.delete,recursive=TRUE)
      private$ModelState <- lapply(
        self$modelPath,
        function(x) list()
      )
      cat("Model",(if(outputOnly) "outputs" else "results"),"cleared.\n")
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

# Check if a specified attribute belongs to the Datastore row
attributeExist <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(TRUE)
    }
  }
  return(FALSE)
}

# Get a specified attribute for a Datastore row
attributeGet <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(attr_value)
    }
  }
  return(NA)
}

ve.model.status <- function(status) {
  if ( missing(status) ) return(private$runError)
  private$runError <- status
}

ve.model.print <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
  cat("Status:", self$status,"\n")
  self$status
}

selectName <- c(fields="Name",tables="Table",groups="Group")
selectOrder <- c("Name","Table","Group","Stage")
prepSelect <- function(self,what,details=FALSE) {
  # self is a VEModel object
  # what is the character name of the thing we're selecting (groups,tables,fields)
  # details if TRUE pastes all the fields, otherwise
  df <- self[[what]]
  name <- selectName[what]
  if ( details ) {
    show <- selectOrder[ selectOrder %in% names(df) ]
    detail.fields <- show[-grep(name,show)]
    if ( what=="fields" ) {
      list.fields <- self[["list"]](details=TRUE,selected=FALSE)
      df$Description <- substr(list.fields$Description,1,40)
      detail.fields <- c(detail.fields,"Description")
      show <- c(show,"Description")
      }
    detail.function <- function(x) {
      paste(x[name],paste(paste(detail.fields,x[detail.fields],sep=": "),collapse=", "),sep=" | ")
    }
  } else {
    show <- name
    detail.function <- function(x) x
  }
  choices <- apply(df[,show,drop=FALSE], 1, detail.function)
  selected <- choices[which(df$Selected=="Yes")]
  names <- df[,name,drop=TRUE]
  return( list(choices=choices,selected=selected,names=names) )
}

# Create an output object from the model output
ve.model.output <- function(rhs) {
  if ( ! missing(rhs) ) stop("Cannot assign to model output",call.=FALSE)
  if ( is.null(private$outputObject) ) {
    output <- VEOutput$new(private$ModelState,self) # parameters TBD
    if ( output$valid() ) {
      private$outputObject <- output
    } else {
      private$outputObject <- NULL
    }
  }
  if ( is.null(private$outputObject) ) {
    invisible(private$outputObject)
  } else {
    return(private$outputObject)
  }
}

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,
    modelPath=NULL,
    stageCount=NULL,
    runParams=NULL,
    runStatus=NULL,

    # Methods
    initialize=ve.model.init,
    run=ve.model.run,
    print=ve.model.print,                   # provides generic print functionality
    dir=ve.model.dir,
    clear=ve.model.clear,
    copy=ve.model.copy,
    status="Uninitialized"
  ),
  active = list(
    output=ve.model.output                  # Create a VEOutput object (if model is run)
  ),
  private = list(
    # Private Members
    runError=NULL,
    artifacts = ve.artifacts,               # Function may interrogate an existing Output
    outputObject=NULL,                      # VEOutput object for this model
    ModelState=NULL                         # ModelState placeholder
  )
)

#' Open a VisionEval Model
#'
#' @description
#' \code{openModel} opens a VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' @section Model Path and Name:
#'
#' The `modelPath` parameter locates a model object. When a model is opened, a
#'   a relative modelPath will be sought in the user's runtime `models` directory.
#'   An absolute path will be sought only in the user's file system.
#'
#' The `modelName` parameters will specify the name of the model directory within
#'   modelPath.
#'
#' You can set an alternate location for the "models" subdirectory by providing an
#'   a path using, for example, `options(VEModelPath='mymodels')`. Relative paths
#'   will be sought below the VisionEval runtime directory. Absolute paths will
#'   be sought in the user's file system.
#'
#' An error will be raised if a model cannot be found with the indicated
#'   modelPath and modelName.
#'
#' @section Available Models:
#'
#' You can see the available models by providing an empty string for the `modelPath`.
#'   A VEModelList (q.v.) object will be printed as a side-effect, and also returned invisibly.
#'   You can open an installed model from the VEModelList using
#'   square brackets to index the list by position (`VEModelList[1]`) or by name
#'   (`VEModelList['VERSPM']`).
#'
#' @param modelPath Directory containing a VisionEval model; if an empty character string is
#'     provided, prints a list of available models (see details)
#' @param modelName Name displayed for this model; defaults to `basename(modelPath)`
#' @return A VEModel object or a VEModelList of available models if no modelPath or modelName is
#'   provided; see details and `vignette("VEModel")`
#' @export
openModel <- function(modelPath="", modelName = NULL) {
  # TODO: if ( missing(modelPath) || !nzchar(modelPath) ) print dir(ve.runtime/models)
  return( VEModel$new(modelPath = modelPath, modelName = modelName) )
}

#' Install a Standard VisionEval Model
#'
#' @description
#' `installModel` installs a local copy of a standard VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' An error will be raised if a model cannot be found or created with the indicated
#'   modelPath and modelName.
#'
#' You can see the available built-in (standard) models by providing an empty string for the `modelName`.
#'
#' @param modelName Name of a standard model to install; if empty or NULL (default), list
#'   available standard models.
#' @param modelPath Location to place the copy of modelName standard model. Created relative to
#'   ve.runtime/models. If directory does not exist, create it and copy the modelName into it.
#'   If directory does exist, create a unique variant of modelName adjacent to it. If it is NULL
#'   create a unique variant of modelName in ve.runtime/models.
#' @param skeleton if TRUE (default), install just skeleton files for the model; otherwise
#'   install the sample model.
#' @param confirm if TRUE (default) and running interactively, prompt user to confirm, otherwise
#'   just do it.
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, skeleton=FALSE, confirm=!skeleton) {
  model <- installStandardModel(modelName, modelPath, confirm, skeleton)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath, modelName=model$modelName ) )
  } else {
    return( model ) # should be a character vector of available standard models
  }
}