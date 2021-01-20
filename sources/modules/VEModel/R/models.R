# Author: Jeremy Raw

# TODO: get the dir() and clear() functions working on multi-stage models
# Should take a stage parameter on any of those. Shows all the "Results"
# subdirectories for the stages, and all relevant files inside those
# (Datastore, model state).  Also show the outputs folder (if it exists)
# and allow clearing that too.

# VEModel Package Code

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

self=private=NULL # To avoid R6 class "undefined global" errors

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
  any(grepl("^([[:alpha:]]:[\\/]*|[\\/])",modelPath))
}

## Helper
#  Model roots: ve.runtime/models, getwd()/models, ve.runtime, getwd()
getModelRoots <- function(Param_ls,get.root=0) {
  roots <- c( getwd() )
  if ( exists("ve.runtime") ) {
    ve.runtime <- get("ve.runtime")
    if ( ve.runtime != getwd() ) roots <- c( ve.runtime, roots )
  }
  # Hierarchy of roots:
  #    ve.runtime/ModelRoot (if exists)
  #    getwd()/ModelRoot (if exists)
  #    ve.runtime
  #    getwd()
  modelRoot <- file.path(roots,visioneval::getRunParameter("ModelRoot",Param_ls=Param_ls))
  if ( length(modelRoot)>0 ) {
    if ( isAbsolutePath(modelRoot[1]) ) {
      modelRoot <- modelRoot[1]
    } else {
      test.paths <- normalizePath(file.path(roots,modelRoot))
      modelRoot <- test.paths[dir.exists(test.paths)]
      if ( length(modelRoot)==0 || ! nzchar(modelRoot[1]) ) {
        modelRoot <- NULL
      } else {
        modelRoot <- test.paths
      }
    }
  }
  roots <- c( modelRoot, roots)
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
modelInRoot <- function(modelPath,searchScript,searchExact) {
  # Search initially just in modelPath, then in first-level subdirectories
  # Expect modelPath to be normalized
  grepArgs <- list(searchScript,fixed=searchExact,value=TRUE)
  if ( ! searchExact ) grepArgs <- c(grepArgs,list(ignore.case=TRUE))
  dirs <- list(dir(modelPath,full.names=TRUE))
  paths <- do.call("grep",c(grepArgs,dirs))
  if ( length(paths) == 0 ) {
    dirs <- list(dir(list.dirs(modelPath,recursive=FALSE),full.names=TRUE))
    paths <- do.call("grep",c(grepArgs,dirs))
    visioneval::writeLog(dirs,Level="debug")
  }
  if ( length(paths) > 0 ) {
    # All matching searchScript files with path relative to modelPath
    paths <- sub("^\\./","",sub( modelPath,".",paths )) # still includes the file name
    # Must have no more than one run_model.R per subdirectory
    if ( ( any(dupes <- duplicated(dirname(paths))) ) ) {
      Msg <- paste(
        "Model script ",
        if ( searchExact ) "exact search" else "regex search",
        " yielded more than one script file per subdirectory:\n\n",
        paste(paths[ dirname(paths) %in% dirname(paths)[dupes] ],"\n",sep="",collapse="\n"),
        "\nAdjust ModelScript or ModelScriptFile pattern in configuration\n",
        "or remove/rename one of the excess scripts."
      )
      stop(Msg)
    }
  }
  return(paths)
}

## Helper function

## Helper function
# Examine modelPath and return (sub-)directories containing run_model.R
# Param_ls provides a list of configuration values 
# Returns a list with the following elements:
#   isValid : a boolean, TRUE if we found model(s), FALSE if we did not
#   ModelScript : a vector (length of stagePaths) with the actual files found matching
#      the ModelScript RunParameter.
#   modelPath : character vector (length 1); the normalized version of parameter
#   stagePaths : character vector of absolute paths below modelPath that were found
#      to contain files named <runModelName> (per configuration). First part of
#      each path will exactly match modelPath.
findModel <- function( modelPath, Param_ls ) {

  find_ls <- list()
  find_ls$isValid <- FALSE # we'll change our mind later...

  if ( missing(modelPath) || ! is.character(modelPath) ) {
    message("Must provide modelPath locator.")
    return( find_ls )
  }

  # Establish the local name for run_model.R
  # VisionEval.R startup will set ModelScript from site configuration file
  #   located in runtime root (first of .visioneval, VisionEval.ini, VisionEval.cnf)
  #   with same default as here ("run_model.R")
  searchScript <- visioneval::getRunParameter("ModelScript",Param_ls=Param_ls)
  searchExact <- is.na(searchScript)
  if ( searchExact ) {
    searchScript <- visioneval::getRunParameter("ModelScriptFile",Param_ls=Param_ls)
  }

  # check if modelPath contains a runModelName
  findPath <- grep(modelPath,searchScript,fixed=searchExact,ignore.case=TRUE,value=TRUE)
  if ( length(findPath>0) ) {
    modelPath <- findPath[1]
    message("Using explicit script matching ",searchScript,":\n")
    find_ls$modelPath <- normalizePath(dirname(modelPath),winslash="/")
    find_ls$stagePaths <- "." # modelPath root is the only stagePath
    find_ls$stageScripts <- basename(modelPath)
    find_ls$isValid <- TRUE
    message( modelPath )
    return(find_ls)
  }

  # if modelPath is not an absolute path, search for it amongst the "roots"
  if ( ! isAbsolutePath(modelPath) ) {
    roots<-getModelRoots(Param_ls)
    possiblePaths <- file.path(roots,modelPath)
    existing <- dir.exists(possiblePaths)
    if ( ! any(existing) ) {
      message("Failed find to model ",modelPath," in these locations:\n",paste(roots,collapse="\n"))
      find_ls$modelPath <- modelPath
      return(find_ls)
    } else {
      # Use first of the existing paths
      find_ls$modelPath <- normalizePath(possiblePaths[existing][1],winslash="/")
    }
  } else if ( ! dir.exists(modelPath) ) {
    message("Model directory ",modelPath," does not exist")
    return(find_ls)
  } else {
    find_ls$modelPath <- modelPath
  }

  # Attempt to locate runModelName in directory and first-level sub-directories
  # The resulting vector are the stage paths (relative to find_ls$modelPath)
  find_ls$stagePaths <- modelInRoot(find_ls$modelPath,searchScript,searchExact)
  if ( length(find_ls$stagePaths)<=0 ) {
    message("No matching pattern for",searchScript," found in ",find_ls$modelPath)
  } else {
    find_ls$stageScripts <- basename(find_ls$stagePaths)
    find_ls$stagePaths <- dirname(find_ls$stagePaths)
    find_ls$isValid <- TRUE
  }
  
  return( find_ls )
}

## Dir
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

installStandardModel <- function( modelName, modelPath, confirm, skeleton, Param_ls=NULL ) {
  # Locate and install standard modelName into modelPath
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first available standard root
  #   If modelPath is an existing directory, put modelName into it (conflict-resolved name)
  #   If modelPath does not exist, but dirname(modelPath) exists, create new directory and put the model there
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and they have to try again
  model <- findStandardModel( modelName )
  if ( is.null(modelName) || ! nzchar(modelName) ) return(model) # Vector of available standard model names

  # Set up destination modelPath
  if ( ! is.list(Param_ls) ) Param_ls <- list()
  root <- getModelRoots(Param_ls,1)
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
    newPath <- dirname(self$modelPath) # parent of current model
  } else {
    if ( ! dir.exists(newPath) ) {
      newNewPath <- dirname(newPath)
      if ( ! dir.exists(newNewPath) ) {
        stop("Destination path for model copy does not exist:\n",newPath)
      } else newPath <- newNewPath
      if ( is.null( newName ) ) newName <- basename(newPath)
    }
  }

  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- getUniqueName(newPath,newName)
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)

  dir.create(newModelPath,showWarnings=FALSE)
  model.files <- self$dir(root=TRUE)
  copy.subdir <- dirname(model.files)
  unique.dirs <- unique(copy.subdir)
  for ( d in unique.dirs ) {
    copy.from <- file.path(self$modelPath,model.files[copy.subdir==d])
    copy.to <- newModelPath
    if ( d == "." ) { # modelPath
      file.copy( copy.from, newModelPath, recursive=TRUE )
    } else {
      copy.to <- file.path(copy.to,d)
      if ( ! dir.exists(copy.to) ) dir.create(copy.to)
      file.copy( copy.from, copy.to ) # non-recursive in stages
    }
  }
  return( openModel(newModelPath) )
}

# Create a separate function for this purpose to keep consistency between
# loading ModelState files and running Models
ve.model.setupRunEnvironment <- function(
  Owner,
  PreviousState=list(),
  Param_ls=list(),
  RunModel=FALSE,
  ModelDir=getwd(),
  ResultsDir=".",
  InputPath=".",
  ModelScriptFile=NULL,
  LogLevel="info"
) {
  # Set up ve.model environment
  ve.model <- visioneval::modelEnvironment(Clear=Owner)
  ve.model$RunModel <- RunModel
  ve.model$ModelStateStages <- PreviousState; # previously loaded model states
  addParams_ls <- list(
    ResultsDir      = ResultsDir,
    ModelDir        = ModelDir,
    InputPath       = InputPath,
    ModelScriptFile = ModelScriptFile,
    LogLevel        = LogLevel
  )
  addParams_ls <- visioneval::addParameterSource(addParams_ls,paste0("Set up RunParam_ls for ",Owner))
  ve.model$RunParam_ls <- visioneval::mergeParameters(Param_ls=Param_ls,addParams_ls) # addParams_ls will override

  invisible(ve.model$RunParam_ls)
}

ve.model.loadModelState <- function(log="error") {
  # Load all ModelStates for each model stage, using initializeModel with RunModel=FALSE
  # If ModelState exists from a prior run, load that rather than rebuild
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  if ( ! dir.exists(ResultsDir) ) {
    dir.create(ResultsDir,showWarnings=FALSE)
  }
  ModelStateFileName <- visioneval::getRunParameter("ModelStateFileName",Param_ls=self$RunParam_ls)
  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  # Almost always, the BaseInputPath should be "."
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- file.path(self$modelPath,BaseInputPath)
  }
  private$ModelState <- list()
  owd <- setwd(file.path(self$modelPath,ResultsDir))
  on.exit(setwd(owd))
  
  initMsg <- "Loading ModelState"
  if ( self$stageCount>1 ) {
    initMsg <- paste(initMsg,"for Stage ")
  } else {
    visioneval::writeLog(initMsg,Level="info")
  }

  for ( stage in 1:self$stageCount ) {
    stagePath <- self$stagePaths[stage]

    if ( self$stageCount>1 ) {
      visioneval::writeLog(paste(initMsg,stage,":",stagePath),Level="info")
    }

    modelState <- normalizePath(file.path(ResultsDir,stagePath,ModelStateFileName),winslash="/",mustWork=FALSE)
    if ( visioneval::loadModelState(modelState, ( ms.env <- new.env() )) ) {
      # Attempt to load existing ModelState
      private$ModelState[[ basename(stagePath) ]] <- ms.env$ModelState_ls
      if ( "RunStatus" %in% ms.env$ModelState_ls ) {
        self$runStatus[stage] <- ms.env$ModelState_ls$RunStatus
      } else {
        self$runStatus[stage] <- "Prior Run"
      }
    } else {
      # Run the initializeModel function to build in-memory ModelState_ls
      # Needed to inspect model elements (script, inputs, outputs)
      self$runStatus[stage] <- "Not Run"
      stageInput <- file.path(self$modelPath,stagePath)
      scriptFile <- self$stageScripts[stage]
      Param_ls <- ve.model.setupRunEnvironment(
        "VEModel::loadModelState",
        PreviousState=private$ModelState, # previously loaded model states
        Param_ls=self$RunParam_ls,
        RunModel=FALSE,
        ModelDir=stageInput,
        ResultsDir=file.path(ResultsDir,stagePath),
        InputPath=unique(c(stageInput,BaseInputPath)),
        ModelScriptFile=normalizePath(file.path(stageInput,scriptFile),winslash="/"),
        LogLevel=log
      )

      # Parse the model script
      parsedScript <- visioneval::parseModelScript(Param_ls$ModelScriptFile)

      # Execute the initializeModel function from the model script
      initArgs                   <- parsedScript$InitParams_ls; # includes LoadDatastore etc.
      # Naming explicit arguments below (e.g. ModelScriptFile) makes them higher priority than Param_ls
      initArgs$ModelScriptFile   <- Param_ls$ModelScriptFile
      initArgs$ParsedModelScript <- parsedScript
      initArgs$LogLevel          <- log
      private$ModelState[[ toupper(basename(stagePath)) ]] <- do.call(visioneval::initializeModel,args=initArgs)
    }
  }
  if ( length(private$ModelState)!=self$stageCount ) {
    stop(
      visioneval::writeLog(
        c(
          "Failed to load all ModelStates",
          "ModelState list not the same length as stagePaths list: ",
          length(private$ModelState)," ",self$stageCount
        ),
        Level="error"
      )
    )
  }
  invisible(private$ModelState)
}

getRuntimeConfig <- function(ParamDir=NULL) {
  # Function loads configuration from ParamDir/VisionEval.cnf
  # ParamDir defaults to ve.runtime if defined, else getwd()
  # override is a list whose elements may be replaced by this configuration
  # keep is a list whose elements will take precedence over this configuration
  if ( ! is.character(ParamDir) ) {
    ParamDir <- get0("ve.runtime",ifnotfound=getwd())
  }
  return(
    visioneval::loadConfiguration(ParamDir=ParamDir)
  )
}

# Initialize a VEModel from modelPath
ve.model.init <- function(modelPath=NULL,log="error") {
  # Load system and user model configuration
  # Opportunity to override names of ModelState, run_model.R, Datastore, etc.
  # Also to ejstablish standard model directory structure (inputs, results)
  self$RunParam_ls <- getRuntimeConfig()

  # Identify the run_model.R root location(s)
  modelPaths <- findModel(modelPath,self$RunParam_ls)
  if ( ! modelPaths$isValid ) {
    stop("No VisionEval model (",modelPaths$runModelName,") found at ",modelPath)
  }

  # Set up the model name, folder tree and script files
  self$modelPath <- modelPaths$modelPath
  self$modelName <- basename(self$modelPath)
  self$stagePaths <- modelPaths$stagePaths; # named stage names (folders with run_model.R, relative to ModelPath)
  self$stageScripts <- modelPaths$stageScripts; # actual names of ModelScriptFile for each stage
  self$stageCount <- length(self$stagePaths)

  visioneval::initLog(Save=FALSE,Threshold=log)
  initMsg <- paste("Initializing Model",self$modelName)

  # Load the Model State
  private$loadModelState(log=log)

  # TODO: Distinguish between a "virtual" model state and one loaded from disk
  # Move the runStatus setup to loadModelState
  self$status <- self$runStatus[length(self$runStatus)]
  visioneval::writeLogMessage(self$modelPath)
  visioneval::writeLog("Initialization Complete.",Level="info")
  invisible(self$status)
}

default.parameters.table = list(
  ModelRoot       = "models",
  ModelScript     = "run_model\\.R",
  ModelScriptFile = "run_model.R"
)

#GET DEFAULT PARAMETERS
#======================
#' Report useful default values for key parameters
#'
#' \code{defaultVERuntimeParameters} extends \code{visioneval::defaultVERuntimeParameters} to
#' provide additional Parameters and Defaults for VEModel functions that can be accessed '
#' seamlessly. You can call this function directly (e.g. to see what parameters are defined and '
#' defaulted in VEModel). Internally VEModel uses \code{visioneval::defaultVERuntimeParameters},
#' so it doesn't have to remember the place specific defaults are defined.
#'
#' @param Param_ls a list (possibly empty) of already-defined parameters
#' @return a named list for parameters not present in Param_ls containing default values for those
#'   parameters
#' @export
VEPackageRunParameters <- function(Param_ls=list()) {
  defaultParams_ls <- default.parameters.table[
    which( ! names(default.parameters.table) %in% names(Param_ls) )
  ]
  if ( length(defaultParams_ls)>0 ) {
    defaultParams_ls <- visioneval::addParameterSource(defaultParams_ls,"Package VEModel Default")
    Param_ls <- visioneval::mergeParameters(defaultParams_ls,Param_ls) # Param_ls will override
  }
  return(Param_ls)
}

# Run the modelPath (through its stages)
# Find the runModel script
# Work in the Results directory - need to relay locations from here to initializeModel
# Need to set Inputs directory
ve.model.run <- function(stage=NULL,lastStage=NULL,log="info") {
  # stage and lastStage will run a sequence of *exterior* model stages (separate run_model.R in
  # subdirectories)
  # If self$stagePaths has length > 1, stage refers to that *exterior* stage unless stageScript is
  #   also provided (in which case stageScript is the exterior stage, and stage is the interior)
  # Name will match the "stagePath" basename if we have "exterior" stages
  #   (explicit directories with their own run_model.R script)
  # If interior stages, we can match those too
  # Combining exterior and interior stages will yield result directories like
  #   exterior-stage-1/interior-stage-1
  # Where the exterior stage is the location of run_model.R (if subdirectory of
  #   modelPath) and the interior stage is from the "name" parameter of a runStage
  #   directive (just defaulting to Stage-N for the Nth runStage directive).

  if ( is.null(stage) ) {
    stageStart <- 1
    if ( is.null(lastStage) ) {
      lastStage <- self$stageCount
    } else if ( lastStage < stageStart ) {
      lastStage <- stageStart
    }
  } else {
    stageStart <- stage
    if ( is.null(lastStage) ) {
      lastStage <- stageStart
    } else if ( lastStage < stageStart ) {
      lastStage <- stageStart
    }
  }
    
  if ( is.null(lastStage) || lastStage < stageStart ) {
    lastStage <- stageStart
  } else if ( lastStage==0 ) {
    lastStage <- self$stageCount
  } # else it is what it is (hopefully a number > stageStart)

  if ( is.null(lastStage) ) lastStage <- self$stageCount;

  # Set up model constants for running multiple stages
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- normalizePath(file.path(self$modelPath,BaseInputPath),winslash="/",mustWork=FALSE)
  }

  owd <- setwd(file.path(self$modelPath,ResultsDir))
  on.exit(setwd(owd))

  # Set up the model runtime environment
  for ( ms in stageStart:lastStage ) {
    stagePath <- self$stagePaths[ms]
    scriptFile <- self$stageScripts[ms]
    stageInput <- file.path(self$modelPath,stagePath)

    visioneval::initLog(Save=FALSE,Threshold=log)
    initMsg <- paste("Running Model",self$modelName)
    if ( self$stageCount>1 ) {
      visioneval::writeLog(paste(initMsg,"Stage",stage,": ",stagePath),Level="info")
    } else {
      visioneval::writeLog(initMsg,Level="info")
    }

    self$status <- ""
    suppressWarnings (
      # Warnings will not show up interactively
      # However they will get pushed through writeLog
      withCallingHandlers (
        tryCatch (
          {
            self$status <- "Running"

            # Set up run environment
            Param_ls <- ve.model.setupRunEnvironment(
              "VEModel::run",
              PreviousState=private$ModelState, # previously loaded model states
              Param_ls=self$RunParam_ls,
              RunModel=TRUE,
              ModelDir=stageInput,
              ResultsDir=file.path(ResultsDir,stagePath),
              InputPath=unique(c(stageInput,BaseInputPath)),
              ModelScriptFile=normalizePath(file.path(stageInput,scriptFile),winslash="/"),
              LogLevel <- log
            )

            visioneval::writeLog(c("Executing Script File:",Param_ls$ModelScriptFile),Level="info")

            RunDir <- normalizePath(file.path(self$modelPath,Param_ls$ResultsDir),winslash="/",mustWork=FALSE)
            if ( ! dir.exists(RunDir) ) dir.create(RunDir,showWarnings=FALSE,recursive=TRUE)
            setwd(RunDir) # Set working directory each time through each loop
            # Model needs to run in "ResultsDir" (where ModelState and Datastore are written)
            sys.source(Param_ls$ModelScriptFile,envir=new.env())

            visioneval::writeLog(paste0("Model stage ",stagePath," complete"),Level="info")
            self$status <- self$runStatus[ms] <- "Complete"
          },
          error = function(e) {
            if ( self$stageCount>1 ) {
              remark <- paste0("Model stage ",stagePath," failed")
            } else {
              remark <- "Model stage failed"
            }
              
            msg <- c(conditionMessage(e),deparse(conditionCall(e)))
            if ( ! nzchar(msg)[1] ) msg <- "Stopped."
            self$status <- "Error"
            visioneval::writeLog(c(remark,msg),Level="error")
            self$runStatus[ms] <- "Failed"
          },
          finally =
          {
            ve.model <- visioneval::modelEnvironment()
            ve.model$RunModel <- FALSE
            if ( self$status == "Running" ) {
              self$status <- "Failed"
            }
            if ( self$status == "" ) {
              self$status <- "Stopped"
            }
            visioneval::writeLog(
              c(
                if ( self$stageCount>1 ) paste("Model Stage:",stagePath),
                paste("Status:",self$status)
              ),
              Level="info"
            )
            if ( is.list(ve.model$ModelState_ls) && is.list(ve.model$RunParams_ls) ) {
              visioneval::writeLog(c("Current directory saving model state:",getwd()),Level="info")
              visioneval::setModelState(
                list(
                  RunStatus=self$runStatus[ms]
                ),
                Save=file.exists(visioneval::getModelStateFileName()) # expecting to be in ResultsDir
              )
              private$ModelState[[ toupper(basename(stagePath)) ]] <- ve.model$ModelState_ls
            }
            setwd(owd)
          }
        ),
        warning=function(w) {
          # Log the warning then carry forth
          visioneval::writeLog(
            c(
              "Warning from Running Model:",
              conditionMessage(w)
            ),
            Level="warn"
          )
        }
      )
    )
    if ( self$status != "Complete" ) {
      break;
    }
  }

  # Reload the ModelState files for each stage to synchronize with file system
  private$loadModelState(log=log)

  return(invisible(self$status))
}

ve.model.dir <- function(pattern=NULL,stage=NULL,root=FALSE,results=FALSE,outputs=FALSE,inputs=FALSE,shorten) {
  # We're going to search up contents of these directories
  #   self$modelPath
  #   self$modelPath/ResultsDir
  #   self$modelPath/ResultsDir/self$stagePaths[stage]
  #   self$modelPath/InputDir
  #   self$modelPath/self$stagePaths[stage]/InputDir
  #   self$modelPath/OutputDir
  #   self$modelPath/OutputDir/query
  # If none root/results/outputs/inputs is TRUE, then all are TRUE
  #   Otherwise, only report the ones actually asked for
  inputDetails <- if ( ! missing(inputs) ) inputs else FALSE
  if ( all(missing(root),missing(results),missing(outputs),missing(inputs)) ) {
    root <- results <- outputs <- inputs <- TRUE
  }

  if ( missing(shorten) ) shorten <- self$modelPath
  if ( is.null(stage) ) stage<-c(1:self$stageCount)

  if ( inputs ) {
    inputPath <- file.path(
      self$modelPath,c(
        (InputDir <- visioneval::getRunParameter("InputDir",Param_ls=self$RunParam_ls)),
        file.path(self$stagePaths[stage],InputDir)
      )
    )
    inputFiles <- dir(normalizePath(inputPath,winslash="/",mustWork=FALSE),full.names=TRUE)
    if ( inputDetails ) {
      inputFiles <- inputFiles[ ! dir.exists(inputFiles) ] # keep only the files, not subdirectories
    } else {
      # no details: keep directories, not files
      # but also show the input directory names themselves (but only if they exist)
      inputFiles <- normalizePath(c(inputPath,inputFiles),winslash="/",mustWork=FALSE)
      inputFiles <- inputFiles[ dir.exists(inputFiles) ]
    }
  } else inputFiles <- character(0)
  if ( outputs ) {
    outputPath <- file.path(
      self$modelPath,c(
        (OutputDir <- visioneval::getRunParameter("OutputDir",Param_ls=self$RunParam_ls)),
        file.path(self$stagePaths[stage],OutputDir)
      )
    )
    outputFiles <- dir(normalizePath(outputPath,winslash="/",mustWork=FALSE),full.names=TRUE)
    outputFiles <- outputFiles[ ! dir.exists(outputFiles) ] # keep only the files, not subdirectories
  } else outputFiles <- character(0)
  ResultsDir <- normalizePath(
    file.path(self$modelPath,visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)),
    winslash="/",mustWork=FALSE
  )
  ResultsInRoot <- ( root && ResultsDir==self$modelPath )
  if ( results || ResultsInRoot  ) {
    # Handle the old-style case where ResultDir==modelPath
    # ResultsDir is already normalized
    # We're only going to look for known result types ("artifacts")
    resultPath <- c(
      ResultsDir,
      file.path(ResultsDir,self$stagePaths[stage])
    )
    resultPath <- normalizePath(resultPath,winslash="/",mustWork=FALSE)
    mstates <- dir(resultPath,pattern="^ModelState(_[[:digit:]]{4}-.*)*\\.Rda$",full.names=TRUE)
    dstores <- dir(resultPath,pattern="^Datastore(_[[:digit:]]{4}-.*)*$",full.names=TRUE)
    logs    <- dir(resultPath,pattern="Log(_[[:digit:]]{4}-.*)*\\.txt",full.names=TRUE)
    resultFiles <- c(mstates,dstores,logs)
  } else resultFiles <- character(0)
  if ( root ) {
    rootPath <- c(self$modelPath,file.path(self$modelPath,self$stagePaths[stage]))
    rootPath <- unique(normalizePath(rootPath,winslash="/",mustWork=FALSE))
    rootFiles <- dir(rootPath,full.names=TRUE)
    if ( ResultsInRoot ) rootFiles <- setdiff(rootFiles,resultFiles)
    rootFiles <- setdiff(rootFiles, file.path(self$modelPath,self$stagePaths[stage]))
  } else rootFiles <- character(0)

  files <- sort(unique(c(
    # in case nothing was asked for: list(list()[x]) would return NULL, not character(0)
    # So force the type for files by providing an empty string to to add to NULL/nothing
    character(0), 
    unlist(
      list(inputFiles,outputFiles,resultFiles,rootFiles)[c(inputs,outputs,results,root)]
    )
  )))
  if ( nzchar(shorten) ) files <- sub(paste0(shorten,"/"),"",files)
  return(files)
}

ve.model.clear <- function(force=FALSE,outputOnly=NULL,stage=NULL,show=10) {
  # Remove outputs and/or results, either interactivel or in batch
  # 'show' controls maximum number of outputs to display
  # Can limit to outputs in a certain 'stage'
  # outputOnly will show results as well as outputs for deletion;
  #   if outputs exist, we only show those by default
  # force says "just go ahead and delete everything", respecting outputOnly,
  #   so the default is to delete the outputs and leave the results, but if
  #   called a second time, will delete the results.

  to.delete <- self$dir(outputs=TRUE)
  if ( missing( outputOnly ) ) {
    outputOnly <- length(to.delete)>0
  }
  if ( ! isTRUE(outputOnly) ) to.delete <- c(to.delete,self$dir(results=TRUE))
  # note, by default $dir shortens by removing self$modelPath
  # so this deletion will only work if getwd()==self$modelPath
  if ( normalizePath(getwd(),winslash="/") != self$modelPath ) {
    owd <- setwd(self$modelPath)
    on.exit(setwd(owd))
  }

  force <- ( force || ! ( interactive() && length(to.delete)>0 ) )
  if ( ! force && length(to.delete)>0 ) {
    action = "h"
    start = 1
    stop <- min(start+show-1,length(to.delete))
    while ( action != "q" ) {
      print(to.delete[start:stop])
      cat("Enter an item number to delete it, or a selection (2,3,7) or range (1:5)\n")
      if ( action == "h" ) {
        cat("'q' to quit, 'all' to delete all on this screen")
        if ( start>1 )               cat(", 'p' for previous files")
        if ( length(to.delete)>stop ) cat(", 'n' for more files")
        cat("\n")
      }
      cat("Your choice: ")
      response <- tolower(readline())
      if ( grepl("^all$",response) ) {
        response <- paste0(start,":",stop)
      } else {
        if ( grepl("[^hnpq0-9:, ]",response) ) { # if any illegal character, loop back to help
          action = "h"
          next
        }
        response <- sub("^ *","",response)
      }
      if ( grepl("^[0-9:, ]+$",response) ) {
        response <- try ( eval(parse(text=paste("c(",response,")"))) )
        # Look for character command
        candidates <- to.delete[start:stop]
        unlink(candidates[response],recursive=TRUE)
        cat("Deleted:\n",paste(candidates[response],collapse="\n"),"\n")
        to.delete <- self$dir(outputs=TRUE)
        if ( ! isTRUE(outputOnly) ) to.delete <- c(to.delete,self$dir(results=TRUE))
        if ( length(to.delete) > 0 ) {
          start = 1
          stop <- min(start+show-1,length(to.delete))
          action = "h"
        } else {
          cat("No files remaining to delete.\n")
          action = "q"
        }
      } else {
        action <- substr(response[1],1,1)
        if ( action %in% c("n","p") ) {
          if ( action == "n" ) {
            start <- min(start + show,length(to.delete))
            if ( start == length(to.delete) ) start <- max(length(to.delete),1)
            stop <- min(start+show-1,length(to.delete))
          } else if ( action == "p" ) {
            start <- max(start - show,1)
            stop <- min(start+show-1,length(to.delete))
          }
        } else if ( action %in% c("h","q") ) {
          next
        } else action = "h"
      }
    }
  }
  if ( ! force ) cat("Nothing to delete.\n")
  return(invisible(force))
}

ve.model.print <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$RunParam_ls$DatastoreType,"\n")
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

ve.model.output <- function(stage) {
  # Create an output object wrapping the directory that contains the model
  # results for the given stage (or last stage if not given)
  if ( missing(stage) || !is.numeric(stage) ) {
    stage <- self$stageCount
  }
  output <- VEOutput$new(private$ModelState,self,stage) # parameters TBD
  if ( output$valid() ) {
    return(output)
  } else {
    if (stage!=self$stageCount) {
      warning("There is no output for stage ",stage," of this model yet.")
    } else {
      warning("There is no output for this model yet.")
    }
  }
  return(output)
}

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,
    modelPath=NULL,
    stagePaths=NULL,
    stageScripts=NULL,
    stageCount=NULL,
    RunParam_ls=NULL,
    runStatus=NULL,
    status="Uninitialized",

    # Methods
    initialize=ve.model.init,               # initialize a VEModel obje ct
    run=ve.model.run,                       # run a model (or just a subset of stages)
    print=ve.model.print,                   # provides generic print functionality
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    output=ve.model.output                  # Create a VEOutput object (if model is run); option to open a past result
  ),
  private = list(
    # Private Members
    runError=NULL,
    ModelState=NULL,                        # ModelState placeholder
    # Private Methods
    loadModelState=ve.model.loadModelState  # Function to load a model state file
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
#' @param log a character string identifying the log level to be displayed
#' @return A VEModel object or a VEModelList of available models if no modelPath or modelName is
#'   provided; see details and `vignette("VEModel")`
#' @export
openModel <- function(modelPath="",log="error") {
  if ( missing(modelPath) || !nzchar(modelPath) ) {
    Param_ls <- getRuntimeConfig()
    if ( exists("ve.runtime") ) {
      ve.runtime <- get("ve.runtime")
    } else {
      ve.runtime <- getwd()
    }
    return(dir(file.path(ve.runtime,visioneval::getRunParameter("ModelRoot",Param_ls=Param_ls))))
  } else {
    return( VEModel$new(modelPath = modelPath,log=log) )
  }
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
#' @param log a string describing the minimum level to display
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, skeleton=FALSE, confirm=!skeleton, log="error") {
  model <- installStandardModel(modelName, modelPath, confirm, skeleton)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath, log=log ) )
  } else {
    return( model ) # should be a character vector of available standard models
  }
}
