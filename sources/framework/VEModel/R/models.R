# Author: Jeremy Raw
# VEModel Package

#' @include environment.R
#' @import visioneval
NULL

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

##############################################
# VisionEval Model Manager Class and Functions
##############################################

#' VisionEval model manager class and functions
#'
#' The VisionEval model manager (VEModel) provides a simple way to run VisionEval models, and to
#' access the model results. The framework itself contains full support for running a model, so if
#' you have a model, you can still just change into its directory and do
#' \code{source('run_model.R')}. VEModel (and its helpers, VEResult and VEQuery) provide a
#' convenient interface for running a model and exploring its structure and results.
#'
#' Creating a model is still a manual process, so you're usually better off duplicating one of the
#' standard models. The VEModel manager makes that very easy! See \code{vignette('VEModel')} for
#' full instructions, and the "walkthrough" folder with illustrative scripts that is part of the
#' standard installation. A simple introduction is also found on the VisionEval wiki, in the
#' Getting-Started document (that document is also included in runtime installations of VisionEval).
#' 
#' Here are the details of the VEModel manager.
#'
#' @section Usage:
#' \preformatted{mdl <- VEModel$new(modelName)
#'
#' mdl$valid()
#' mdl$run(run="save",stage=NULL,lastStage=NULL,log="warn")
#' mdl$dir(pattern=NULL,stage=NULL,root=FALSE,results=FALSE,outputs=FALSE,inputs=FALSE,all.files=TRUE,shorten=TRUE)
#' mdl$clear(force=FALSE,outputOnly=NULL,stage=NULL,show=10)
#' mdl$list(inputs=FALSE,outputs=FALSE,details=NULL)
#' mdl$log()
#' mdl$set(show="values", src=NULL, namelist=NULL, pattern=NULL,Param_ls=NULL)
#' mdl$setting()
#' mdl$save(FileName="visioneval.cnf")
#' mdl$copy(newName=NULL,newPath=NULL,copyResults=TRUE)
#' mdl$rename(Name=NULL,Scenario=NULL,Description=NULL,Save=TRUE)
#' mdl$results(stage)
#' mdl$findstages(stage)
#' mdl$query(QueryName=NULL,FileName=NULL,load=TRUE)
#' 
#' print(mdl)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{modelName}{The path or basename of a directory containing a VisionEval model setup; if
#'   it's a relative path, the model will be sought in the current directory plus standard places
#'   like getwd()/models. Usually it will just be getwd()/models.}
#'   \item{log}{The error threshold to display within the function (from most detailed to least detailed,
#'   one of the following: \code{c("trace","debug","info","warn","error","fatal")}. The default can
#'   be configured in \code{visioneval.cnf} using the LogLevel parameter.}
#'   \item{run}{Describes how to run the model if a previous run exists. Options are
#'   "save","reset","continue". See "Details".}
#'   \item{stage}{Under development: select a specific RunStep to process by name or sequential number; the default is
#'   to work on the last RunStep, corresponding to the full model results. Specifying a name or number will treat the
#'   model as if it only included results up to that RunStep.}
#'   \item{lastStage}{Under development: RunStep at which to stop processing; run="continue" will resume with the next
#'   stage after the specified one}
#'   \item{pattern}{An R regular expression used to narrow the search of output elements (e.g. to just log files, or
#'   just model state files, or a certain date in archived results)}
#'   \item{stage}{Under development: RunStep for which to display or clear outputs or results. Default is to display the
#'   overall ResultsDir for the model}
#'   \item{results}{Show the model results files and archived results. Directories only for the archives unless all.files=TRUE)}
#'   \item{outputs}{In $dir: Show the model outputs (extracts and queries). Directories only unless all.files=TRUE). In
#'   $list: List output fields descriptions; see Details.}
#'   \item{inputs}{In $dir: Show the model input directories on the InputPath. Directories only unless all.files=TRUE).
#'   In $list: List input fields descriptions; see Details.}
#'   \item{all.files}{Show files, rather than just the directory name for result archives, outputs and inputs.}
#'   \item{shorten}{Remove the runtime directory prefix from the displayed directory and file names. Default is
#'   TRUE}
#'   \item{force}{If TRUE, clear results without interaction. Default is TRUE if not interactive, otherwise FALSE. If
#'   interactive, present a menu of available items and gather user input to determine which to clear.}
#'   \item{outputOnly}{If NULL, offer to delete only outputs (extracts or queries) if any of those exist; otherwise
#'   offer to delete results (like $run with run="reset"). If FALSE, offer to delete any existing outputs and results
#'   (Warning: including archives). If TRUE, offer only to delete existing outputs.}
#'   \item{show}{For $dir: A numeric value of how many results to show in the interactive display when clearing outputs
#'   or results. For $set: A vector containing one or more of "values", "name", "source", "runtime", "defaults" or
#'   "self" to examine the current set of run parameters for the model, after resolving the system and model
#'   configuration files. See Details.}
#'   \item{details}{Include details like UNITS and DESCRIPTION, not just field names, when $list shows model files. See
#'   Details.}
#'   \item{src}{A character string interpreted as a regular expression to match against the parameter source strings. Only
#'   settings from matching sources will be returned. See Details.}
#'   \item{namelist}{A character vector, where each element is interpreseted as the full name of a parameter about
#'   which to return information.}
#'   \item{pattern}{A vector of regular expressions to match against names of parameters - an alternative to
#'   \code{namelist}. Any parameters matching any of the patterns will be returned.}
#'   \item{Param_ls}{A list of run parameters to work on explicitly. In $set, will add these to self$RunParam_ls. In
#'   $results and $resultspath, will allow a different set of parameters to used than self$RunParam_ls. See
#'   \code{runtimeEnvironment()} and Details below.}
#'   \item{FileName}{A name to use to save the current model run parameters if they have been altered dynamically
#'   ($save). Or in $query, the FileName (".VEqry" extension by default) from which to load a query specification; if
#'   NULL, the query FileName will be constructed from the QueryName.}
#'   \item{newName}{The internal name to attach to a copy of this model. Default is to add a numeric disambiguator to
#'   the name of the model being copied.}
#'   \item{newPath}{The directory into which to put the copy of this model. Default (NULL) places
#'   the model in getwd()/models directory, creating "models" if necsssary.}
#'   \item{copyResults}{logical: if TRUE, copy the results, otherwise only copy the model setup. If
#'   the ResultsDir is the same as the root (classic VisionEval), results will always be copied.}
#'   \item{Model}{A character vector with the new model identifier (name) for each stage in the model}
#'   \item{Scenario}{A character vector with the new scenario identifier for each stage in the model}
#'   \item{Description}{A character vector with the new description string for each stage in the model}
#'   \item{Save}{logical; if true save new name etc back to existing ModelState}
#'   \item{QueryName}{The name to use for the Query, which will be used to construct the output directory name when
#'   the query is run. The query file name will be built from the QueryName if FileName is not explicitly provided.}
#'   \item{load}{For a query, if FileName is provided and exists, then attempt to load the file. If it is FALSE, create
#'   a blank query to associate with the FileName. Usually you won't want to touch this parameter.}
#' }
#'
#' @section Details:
#'
#' This section explains the particulars of each operation that can be performed on a VEModel object.
#'
#' \code{$new()} creates a new VEModel object from a file path that locates a \code{run_model.R} script and an optional
#' configuration file. Generally, you'll use \code{openModel()} to create a VEModel object.
#'
#' The "run" argument for $run has three options: "save","reset" and "continue". "save" will archive
#' the Datastore, ModelState and Log file in a sub-folder of ResultsDir. "reset" will delete any
#' existing Datastore, ModelState and Log file and start from scratch (same as running $clear before
#' $run). "continue" is used for debugging and development and will load the existing Datastore and
#' proceed from the first stage or RunStep that is not marked as "Run Complete".
#'
#' @import jsonlite
#' @importFrom R6 R6Class
#' @name VEModel
NULL

# Documentation for VEModelStage
#' VEModelStage class for managing scenarios within a model
#'
#' Documentation yet to come for various functions
#'
#' @name VEModelStage
NULL

self=private=NULL # To avoid R6 class "undefined global" errors

## Helper
# Do a simple command line confirmation prompt
confirmDialog <- function(msg) {
  conf <- utils::askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE # Cancel is the same as No
  return(conf)
}

################################################################################
#                             Model Initialization                             #
################################################################################

## Helper
#  Generate a list of directories that might contain models
#  referring to getwd() and <ModelRoot>
getModelRoots <- function(get.root=0,Param_ls=NULL) {
  roots <- c( getwd() )
  # Hierarchy of roots:
  #    getwd()/<ModelRoot> (if exists)
  #    getwd()
  # Note subtle change in behavior: presume VEModel is running in ve.runtime as the working directory.
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
  if ( get.root > 0 ) return(roots[get.root]) else return(roots)
}

## Helper function
# Cull the InputPath parameter (only those containing InputDir, no duplicates)
cullInputPath <- function(InputPath,modelInputPath=NULL) {
  # Remove any element of InputPath that is "" or "."
  InputPath <- InputPath[ nzchar(InputPath) & InputPath != "." ]

  if ( ! is.null(modelInputPath) ) {
    # modelInputPath is the InputPath associated with the model, ahead of the stages
    # always make sure those elements come last
    writeLog("Reordering modelInputPath",Level="debug")
    writeLog(paste("Model InputPath:",modelInputPath),Level="debug")
    modelPathLocations <- which(InputPath %in% modelInputPath)
    writeLog(paste("Model InputPath present to cull:",paste(modelPathLocations,collapse=",")),Level="debug")
    InputPath <- InputPath[ - modelPathLocations ]
  }

  # Normalize remaining InputPath elements, if any, and remove duplicates
  
  if ( length(InputPath) > 0 ) {
    # Don't cull if there is no stage-specific input path
    writeLog(paste("Culling Input Path:\n",paste(InputPath,collapse="\n")),Level="debug")
    InputPath <- unique(normalizePath(InputPath,winslash="/",mustWork=FALSE))
  }

  if ( ! is.null(modelInputPath) ) {
    InputPath <- c(InputPath,modelInputPath)
  }

  InputPath <- InputPath[dir.exists( InputPath )]
  writeLog(paste("InputPath length after culling:",length(InputPath)),Level="debug")

  return(InputPath)
}

## Helper function
findModel <- function( modelDir, Param_ls=getSetup() ) {

  if ( missing(modelDir) || ! is.character(modelDir) ) {
    writeLog("findModel: Must provide modelDir locator.",Level="warn")
    return( "" ) # An empty list...
  }

  # Establish the model path
  # if modelPath is not an absolute path, search for it amongst the "roots"
  modelPath <- NA
  if ( ! isAbsolutePath(modelDir) ) {
    roots<-getModelRoots(Param_ls=Param_ls)
    possiblePaths <- file.path(roots,modelDir)
    existing <- dir.exists(possiblePaths)
    if ( ! any(existing) ) {
      writeLog(
        paste0("Failed to find model '",modelDir,"' in these locations:\n",paste(roots,collapse="\n")),
        Level="error"
      )
      return("") # empty list
    } else {
      # Use first of the existing paths
      modelPath <- normalizePath(possiblePaths[existing][1],winslash="/")
    }
  } else {
    modelPath <- modelDir
    if ( ! dir.exists(modelPath) ) {
      writeLog(
        paste0("Model directory ",modelPath," does not exist"), Level="error"
      )
      return("")
    }
  }
  return(modelPath)
}

ve.model.torun <- function() {
  # Cascade unrun and out-of-date status from StartFrom stage to those that start from it
}

# configure installs the model parameters (initializing or re-initializing)
# `reloadFile` says to reload self$loadParam_ls from the model configuration in the file system,
#  otherwise use self$RunParam_ls; it is rarely desirable to change the default, which supports
#  in-memory modifications to self$loadedParam_ls
# 
ve.model.configure <- function(modelPath=NULL, reloadFile=FALSE, updateCheck=TRUE) {

  if ( missing(modelPath) || ! is.character(modelPath) ) {
    modelPath <- self$modelPath
  } else {
    self$modelPath <- modelPath;
  }

  # Load any configuration available in modelPath
  Param_ls <- getSetup() # global runtime configuration
  if ( reloadFile || is.null(self$loadedParam_ls) ) {
    self$loadedParam_ls <- visioneval::loadConfiguration(ParamDir=modelPath)
  } # if NOT reloadFile, use existing loadedParam_ls to rebuild (may have in-memory changes)

  modelParam_ls <- visioneval::mergeParameters(Param_ls,self$loadedParam_ls) # override runtime parameters
  if ( "Model" %in% modelParam_ls ) {
    self$modelName <- modelParam_ls$Model
  } else {
    self$modelName <- basename(modelPath) # may be overridden in run parameters
  }

  # Set up ModelDir and ResultsDir
  modelParam_ls <- visioneval::addRunParameter(Param_ls=modelParam_ls,Source="VEModel::findModel",ModelDir=modelPath)
  if ( ! "ResultsDir" %in% names(modelParam_ls) ) {
    # Load default parameter or get from larger runtime environment
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      visioneval::getRunParameter("ResultsDir",Param_ls=Param_ls)
    )
  }

  # Cache the results path (for saving query results that may span multiple stages)
  self$modelResults <- normalizePath(
    file.path(
      self$modelPath,
      resultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=modelParam_ls)
    )
  )
  writeLog(paste("Model Results go in:",self$modelResults),Level="info")

  # Check for LoadModel and LoadStage in model parameters
  if ( "LoadModel" %in% names(modelParam_ls ) ) {
    writeLog("Processing LoadModel directive",Level="info")
    baseModel <- VEModel$new( modelPath=modelParam_ls$LoadModel )
    if ( baseModel$valid() ) {
      writeLog(paste("LoadModel =",baseModel$modelName),Level="info")
      if ( ! "LoadStage" %in% names(modelParam_ls) ) {
        loadStage <- names(baseModel$modelStages)[length(baseModel$modelStages)]
      } else loadStage <- modelParam_ls$LoadStage
      runPath <- baseModel$modelStages[[loadStage]]$RunPath
      modelParam_ls <- visioneval::addRunParameter(
        Param_ls=modelParam_ls,
        Source=attr("Source",modelParam_ls$LoadModel),
        LoadDatastoreName=file.path (
          runPath,
          baseModel$setting("DatastoreName",stage=loadStage)
        ),
        LoadDatastore=TRUE
      )
    } else {
      writeLog(paste("LoadModel present but invalid:",modelParam_ls$LoadModel),Level="warn")
      modelParam_ls <- visioneval::addRunParameter(
        Param_ls=modelParam_ls,
        Source=attr("Source",modelParam_ls$LoadModel),
        LoadDatastore=FALSE
      )
    }
  }

  # Process InputPath for overall model (culling directories for those actually exist)

  # Find ModelDir/InputDir if that exists and make it InputPath
  # If explicit InputPath in modelParam_ls, just use that
  rootInputPath <- normalizePath(
    file.path(modelParam_ls$ModelDir,visioneval::getRunParameter("InputDir",Param_ls=modelParam_ls)),
    winslash="/",mustWork=FALSE
  )
  if ( "InputPath" %in% names(modelParam_ls) ) {
    # expand default InputPath if necessary
    inputPath <- modelParam_ls$InputPath
    if ( ! isAbsolutePath(inputPath) ) {
      inputPath <- normalizePath(
        file.path(
          modelParam_ls$ModelDir,
          inputPath
        )
      )
    }
    inputPath <- c( rootInputPath, inputPath )
  } else {
    inputPath <- rootInputPath
  }
  # cull input path to keep only unique existing directories
  inputPath <- cullInputPath(inputPath)
  if ( length(inputPath) > 0 ) {
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      Source="VEModel::findModel",
      InputPath=inputPath
    )
    writeLog("Input Paths:",Level="info")
    for ( p in modelParam_ls$InputPath ) {
      writeLog(paste("Input Path:",paste("'",p,"'")),Level="info")
    }
  }

  # Locate ParamPath for overall model
  # If it doesn't exist at the level of the model, could still set for individual stages
  #   It makes the most sense to place ParamDir within ModelDir and use the same one
  #   for each model stage (weird things will happen if different stages have different
  #   defs).
  if ( ! "ParamPath" %in% names(modelParam_ls) ) {
    ParamPath <- file.path(
      modelParam_ls$ModelDir,
      visioneval::getRunParameter("ParamDir",Param_ls=modelParam_ls)
    )
    if ( file.exists(ParamPath) ) {
      writeLog(paste("Setting ParamPath to",ParamPath),Level="info")
      modelParam_ls <- visioneval::addRunParameter(
        modelParam_ls,
        Source="VEModel::findModel",
        ParamPath=ParamPath
      )
    } else {
      writeLog("No ParamPath set yet.",Level="info")
    }
  }

  # Locate ModelScriptPath if it has not yet been set
  if ( ! "ModelScriptPath" %in% names(modelParam_ls) ) {
    ScriptName <- visioneval::getRunParameter("ModelScript",Param_ls=modelParam_ls)
    ScriptsDir <- visioneval::getRunParameter("ScriptsDir",Param_ls=modelParam_ls)
    ModelScriptPath <- visioneval::getRunParameter("ModelScriptPath",Default=character(0),Param_ls=modelParam_ls)
    if ( length(ModelScriptPath)>0 && ! nzchar(ModelScriptPath) ) {
      # Not explicitly defined
      writeLog("Searching for ModelScriptPath",Level="info")
      for ( sdir in c(
        file.path(modelParam_ls$ModelDir,ScriptsDir),
        file.path(modelParam_ls$ModelDir)
      ) ) {
        ScriptPath <- file.path(sdir,ScriptName)
        if ( file.exists(ScriptPath) ) {
          ModelScriptPath <- ScriptPath
          break
        }
      }
      if ( length(ModelScriptPath)>0 ) {
        writeLog(paste("Found ModelScriptPath:",ModelScriptPath),Level="info")
      }
    }
  }
  if ( !is.null(ModelScriptPath) && length(ModelScriptPath)>0 ) {
    ModelScriptPath <- ModelScriptPath[1] # just use first found element
    writeLog(paste("Parsing ModelScriptPath:",ModelScriptPath),Level="info")
    if ( nzchar(ModelScriptPath) && file.exists(ModelScriptPath) ) {
      modelParam_ls <- visioneval::addRunParameter(
        modelParam_ls,
        Source="VEModel::findModel",
        ModelScriptPath=ModelScriptPath,
        ParsedScript=visioneval::parseModelScript(ModelScriptPath)
      )
    }
  } else {
    writeLog("No ModelScriptPath; Must set in stages",Level="info")
    modelParam_ls[["ModelScriptPath"]] <- NULL
    # Don't keep ModelScriptPath if there is no ModelScript there!
  }

  # Save the model's RunParam_ls
  self$RunParam_ls <- modelParam_ls

  writeLog("Rebuilding model stages",Level="info")
  self$modelStages <- NULL
  if ( ! "ModelStages" %in% names(modelParam_ls) ) {
    # In general, to avoid errors with random sub-directories becoming stages
    #  it is best to explicitly set ModelStages in the model's main visioneval.cnf
    writeLog("Implicit model stages from directories",Level="info")
    stages <- list.dirs(modelPath,full.names=FALSE,recursive=FALSE)
    structuralDirs <- c(
      self$setting("DatastoreName"),
      self$setting("QueryDir"),
      self$setting("ScriptsDir"),
      self$setting("InputDir"),
      self$setting("ParamDir"),
      self$setting("ScenarioDir"),
      self$setting("ResultsDir"),
      self$setting("DynamicDir"),   # VESnapshot
      self$setting("SnapshotDir")   # VESnapshot
    )
    # TODO: add a hook for other packages (e.g. VESnapshot) to inject structural directories
    stages <- stages[ ! stages %in% structuralDirs ]
    stages <- stages[ grep(paste0("^",self$setting("ArchiveResultsName")),stages,invert=TRUE) ]
    stages <- c(".",stages) # Add model root directory
    writeLog(paste0("Stage directories:\n",paste(stages,collapse=",")),Level="info")
    modelStages <- lapply(stages,
      function(stage) {
        stageParam_ls <- list(
          Dir=stage,           # Relative to modelPath
          Name=stage,          # May be "." for root directory (will change to basename(Path))
          Path=self$modelPath  # Root for stage
        )
        VEModelStage$new(
          Name = stageParam_ls$Name,
          Model = self,
          modelParam_ls = self$RunParam_ls,
          stageParam_ls=stageParam_ls          # Base parameters from model
        )
      }
    )
  } else {
    writeLog("Parsing ModelStages setting from model",Level="info")
    modelStages <- lapply(names(modelParam_ls$ModelStages), # Use pre-defined structures
      # At a minimum, must provide Dir or Config
      function(stage) {
        obj <- modelParam_ls$ModelStages[[stage]] # Get the stageParam_ls structure
        writeLog(paste("Model Stage:",stage),Level="info")
        VEModelStage$new(
          Name=stage,
          Model=self,
          modelParam_ls = self$RunParam_ls,
          stageParam_ls=obj
        )
      }
    )
  }

  # Call the modelStages by their Names
  names(modelStages) <- stageNames <- sapply(modelStages,function(s)s$Name)
  if ( length(stageNames) > 0 ) {
    writeLog(paste("Model Stages:",paste(stageNames,collapse=",")),Level="info")
  }

  # Done with base stages (initialize everything below after scenarios are loaded)
  self$modelStages <- modelStages

  # Load any scenarios from subfolder
  scenarios <- self$scenarios(reloadFile=reloadFile)  # re-create VEModelScenario object from file
  scenarioStages <- scenarios$stages()            # scenario stages may be an empty list

  if ( length(scenarioStages) > 0 ) { # some scenarios are defined
    # It is possible for a model to ONLY have scenarios (if they are "manually" created)
    # Each "scenario" in that case must be a complete model run
    # Usually in such cases, it may be easier just to make them Reportable modelStages
    if ( ! is.list(self$modelStages) ) {
      if ( is.list(scenarioStages) && length(scenarioStages) > 0 ) {
        self$modelStages <- scenarioStages
      } else {
        # If no stages remain, model is invalid
        writeLog("No model stages found!",Level="error")
        return(self)
      }
    } else if ( is.list(scenarioStages) && length(scenarioStages) > 0 ) {
      self$modelStages <- c( self$modelStages, scenarioStages )
    }
  }

  # Link the stages and construct their final RunParam_ls
  writeLog("Initializing Model Stages",Level="info")
  self$modelStages <- self$initstages( self$modelStages, updateCheck=updateCheck )

  self$specSummary <- NULL # regenerate when ve.model.list is next called
  self$updateStatus()

  return(self)
}

# Initialize the model stages, merging parameters and setting runnable status
ve.model.initstages <- function( modelStages, updateCheck=TRUE ) {
  # Loop through modelStages, completing initialization using "StartFrom" stages

  writeLog("Finding runnable stages",Level="info")
  runnableStages <- list()
  for ( stage_seq in seq_along(modelStages) ) {
    stage <- modelStages[[stage_seq]]      # stage is a VEModelStage object
    if ( stage_seq > 1 && isTRUE(stage$loadedParam_ls$LoadDatastore) ) {
      # Only the first stage performs LoadDatastore
      stage$loadedParam_ls <- visioneval::addRunParameter(
        stage$loadedParam_ls,
        Source="VEModel$initstages (rewritten)",
        LoadDatastore = FALSE,
        LoadDatastoreName = NA
      )
    }
    # stage$runnable sets stage$RunParam_ls and determines if everything necessary is present
    if ( stage$runnable(runnableStages, updateCheck=updateCheck) ) {
      writeLog(paste("Stage",stage$Name,"is runnable"),Level="info")
      runnableStages <- c( runnableStages, stage )
      names(runnableStages) <- sapply(runnableStages,function(s)s$Name)
    } else {
      writeLog(paste("Stage",stage$Name,"is NOT runnable"),Level="info")
    }    
  }

  # Forget the modelStages that can't run
  modelStages <- runnableStages
  stageCount <- length(modelStages)
  if ( !is.list(modelStages) || stageCount == 0 ) {
    writeLog("Model has no runnable stages!",Level="error")
    return(modelStages)
  }

  # Set Reportable attribute for the stages
  if ( stageCount == 1 ) {
    # Single stage model will ignore stage$Dir when constructing results or outputs
    if ( is.null(modelStages[[1]]$Reportable) ) {
      # do not set if already explicitly set during ve.stage.init
      modelStages[[1]]$Reportable <- TRUE
    } else {
      writeLog(paste("Single stage",modelStages[[1]]$Name,"Reportable:",modelStages[[1]]$Reportable),Level="info")
    }
  } else {
    # Put names on Stages and identify reportable stages
    # Also fix up scenario Elements (adding to base stage...)
    startFromNames <- unlist(sapply(modelStages,function(s) s$StartFrom))
    if ( length(startFromNames) > 0 ) {
      startFromNames <- unique(startFromNames[ nzchar(startFromNames) ])
    }
    stageNames <- names(modelStages)
    reportable <- ! stageNames %in% startFromNames # default reportable to stages that are not ancestors (will include scenarios)
    for ( r in seq_along(stageNames) ) {
      stage <- modelStages[[r]]
      if ( ! isTRUE(stage$Reportable) ) {
        stage$Reportable <- reportable[r] # Include explicitly reportable stages
      }
      self$fixRunStatus(stage) # check if "Run Complete" is out of date relative to StartFrom
    }
  }

  return( modelStages )
}

# Initialize a VEModel from modelPath
# modelPath may be a full path, and will be expanded into known model directories
#  if it is a relative path.

ve.model.init <- function(modelPath,updateCheck=TRUE) {
  writeLog(paste("Finding",modelPath),Level="info") # for debugging point of failure
  modelPath <- findModel(modelPath) # expand to disk location
  if ( nzchar(modelPath) ) {
    self$configure(modelPath,updateCheck=updateCheck) # actually do the initialization
  } else {
    self$updateStatus() # deliver the bad news on no model path
  }
}

################################################################################
#                      Model Management: copy, rename, dir                     #
################################################################################

# Copy a model to a new directory
# Use the newName to create the model's new directory
# Update directory locations
#   Change the ModelDir in the model's RunParam_ls
#   Update RunPath for each stage in its RunParam_ls and ModelState_ls if copyResults
#   Identify other elements of ModelState and RunParams that will change if the
#     the ModelDir changes (everything built from ModelDir)
ve.model.copy <- function(newName=NULL,newPath=NULL,copyResults=TRUE,copyArchives=FALSE) {
  # Copy the current model to NewName (in ModelDir, unless newPath is also provided)
  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return()
  }
  
  if ( is.null(newPath) ) {
    newPath <- dirname(self$modelPath) # parent of current model
  } else {
    if ( ! dir.exists(newPath) ) {
      newNewPath <- dirname(newPath)
      if ( ! dir.exists(newNewPath) ) {
        stop("Destination path for model copy does not exist:\n",newPath)
      } else newPath <- newNewPath
      if ( is.null( newName ) ) newName <- basename(newPath)
    } # else presume newPath will contain newName
  }

  newPath <- normalizePath(newPath)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- getUniqueName(newPath,newName)
  newModelPath <- normalizePath(newModelPath)

  dir.create(newModelPath,showWarnings=FALSE)
  # check that the directory produces the right results files
  model.files <- self$dir(root=TRUE,inputs=TRUE,results=copyResults,archive=copyArchives,showRootDir=FALSE)
  # message("Files to copy:")
  # print(model.files)
  copy.subdir <- dirname(model.files)
  unique.dirs <- unique(copy.subdir)
  for ( d in unique.dirs ) {
    # Need to copy.date so "out of date" checking has a prayer of working
    copy.from <- file.path(self$modelPath,model.files[copy.subdir==d])
    copy.to <- newModelPath
    # message("Copying to: ",copy.to)
    if ( d == "." ) { # modelPath
      # message("Copying '.', i.e. modelPath to newModelPath:")
      # print(newModelPath)
      file.copy( copy.from, newModelPath, recursive=TRUE, copy.date=TRUE )
    } else {
      # message("Creating target directory to copy into:")
      copy.to <- file.path(copy.to,d)
      # print(copy.to)
      if ( ! dir.exists(copy.to) ) dir.create(copy.to,recursive=TRUE)
      file.copy( copy.from, copy.to, copy.date=TRUE ) # non-recursive in stagesd
    }
  }
  return( VEModel$new(modelPath = newModelPath, updateCheck=FALSE) )
}

# Archive results directory
ve.model.archive <- function(SaveDatastore=TRUE) {
  writeLog("Archiving model results",Level="info")
  failToArchive <- visioneval::archiveResults(
    RunParam_ls=self$RunParam_ls,
    ResultsDir=self$modelResults,
    SaveDatastore=SaveDatastore
  )
  if ( length(failToArchive)>0 ) {
    writeLog(paste0("Failed to archive results (",paste(failToArchive,collapse=","),")"),Level="error")
    return(FALSE)
  }
  return(TRUE)
}

# Provide a listing of key model components (respecting stages, etc.)
# Collapse model results into a single model run entry (though there are typically three pieces:
# the ModelState.Rda, the Datastore, and log*.txt). Find current results, as well as archived
# results from past model runs.
# "shorten" is a logical parameter - if true, strip off the "runtime" part of any paths, so what
#   is shown is relative to the runtime directory.
# Parameters control elements of the display (if all are FALSE, make them all TRUE):
#   root==TRUE   : show "root" elements (config, scripts/run_model.R) plus InputPath and ParamPath
#   archive=TRUE : show archived results directories (these are also found and removed from "root")
#   inputs=TRUE  : show files in "inputs" and "defs" locations for the model
#   results=TRUE : show result sets
#   outputs=TRUE : show "outputs" (extracts and query results)
# if all.files, list inputs and outputs as files, otherwise just directories
ve.model.dir <- function( stage=NULL,shorten=TRUE, showRootDir=TRUE, all.files=FALSE,
  root=FALSE,results=FALSE,outputs=FALSE,inputs=FALSE,scenarios=FALSE,archive=FALSE) {
  # We're going to report contents of these directories
  #   self$modelPath (root)
  #   self$modelPath/ResultsDir (results)
  #   self$modelStages[[stage]]$RunPath (results)
  #   self$modelStages[[stage]]$InputPath (inputs) # if exists
  #     Do the inputs by assembling the entire InputPath and reducing to unique directories
  #     Report for all stages unless specific ones requested
  #     Just list the InputPath directories unless all.files==TRUE
  #   se;f$modelPath/ScenarioDir (scenarios) # if exists
  #   self$modelPath/ParamDir (inputs)
  #   self$modelStages[[stage]]$ParamPath (inputs)
  #     Like the inputs, reduce to a unique set of directories
  #   Outputs are sought within ResultsDir/OutputDir for the model
  #     Subdirectories will be the stage/scenario, files will be specific extracts
  # If none of root/results/outputs/inputs is TRUE, then all are TRUE
  #   Otherwise, only report the ones actually asked for
  validDir <- if ( is.character(self$modelPath) ) dir.exists(self$modelPath) else FALSE
  if ( ! private$p.valid || ! validDir ) {
    return("No model found.")
    private$p.valid <- FALSE
  }

  # show everything if we didn't specify
  if ( all(missing(root),missing(results),missing(outputs),missing(inputs),missing(archive)) ) {
    root <- results <- outputs <- inputs <- archive <- TRUE
  }

  if ( missing(shorten) || shorten ) {
    shorten <- self$modelPath
  } else shorten <- ""
  if ( is.null(stage) ) {
    stages <- self$modelStages
  } else {
    stages <- self$modelStates[stage] # list of named stages
  }
  if ( length(stages)==0 ) {
    writeLog(paste("Invalid stages selected for $dir:",stage,collapse=","),Level="warn")
    inputs <- results <- outputs <- FALSE
  }

  # Develop the input path - unique list of directories from all requested stages
  inputPath <- character(0)
  for ( stg in stages ) {
    # Stage input paths may all be the same at the model level, or individual, or both
    # InputPath includes InputDir
    inputPath <- c(inputPath,self$setting("InputPath",stage=stg$Name,shorten=FALSE))
  }
  inputPath <- unique(inputPath) # use this to avoid copying it with root files
  inputPath <- setdiff(inputPath,self$modelPath)
  if ( inputs ) {
    inputDirs <- normalizePath(inputPath)
    if ( all.files ) {
      inputFiles <- dir(inputDirs,full.names=TRUE)
      inputFiles <- inputFiles[ ! dir.exists(inputFiles) ]
    } else {
      # no details: keep directories, not files
      # Show the input directory names only if they exist
      # inputFiles <- normalizePath(c(inputPath,inputFiles),winslash="/",mustWork=FALSE)
      inputFiles <- inputDirs
    }
  } else inputFiles <- character(0)

  # List scenarios
  scenarioPath <- character(0)
  if ( scenarios ) {
    scenarioPath <- file.path(
      self$modelPath,
      self$setting("ScenarioDir")
    )
    scenarioFiles <- dir(scenarioPath,all.files=all.files,recursive=all.files)
    scenarioDirs <- dir.exists(scenarioFiles)
    if ( all.files ) {
      scenarioFiles <- scenarioFiles[ ! scenarioDirs ] # all the files (only)
    } else {
      scenarioFiles <- scenarioFiles[ scenarioDirs ] # just the subdirectories
    }
  } else {
    scenarioFiles <- character(0)
  }

  # Locate results
  baseResults <- self$modelResults

  # Find RunPath for each stage
  stagePaths <- sapply(stages,function(s) s$RunPath)
  stagePaths <- stagePaths[ !is.na(stagePaths) ]

  # Do the outputs before the results (makes it easier to handle results in root)
  # "OutputDir" is used in VEModel$extract and VEModel$query...
  # Query OutputDir is relative to ModelDir/ResultsDir...
  if ( outputs ) {
    resultPaths <- c(baseResults,stagePaths)
    outputPath <- dir(resultPaths,pattern=self$setting("OutputDir"),full.names=TRUE)
    outputFiles <- dir(outputPath,full.names=TRUE,recursive=all.files)
    outputDirs <- dir.exists(outputFiles)
    if ( all.files ) {
      outputFiles <- outputFiles[ ! outputDirs ]
    }# else {
     # outputFiles <- outputFiles[ outputDirs ]
     # }
    if ( length(outputFiles)==0 ) outputFiles <- outputPath # Show OutputDir as stub
  } else outputFiles <- character(0)

  # Find archiveDirs (and if asked for, archiveFiles)
  # Do this a bit dirfferently from outputs and inputs since there will be a ton of files
  archiveNamePattern <- paste0("^",self$setting("ArchiveResultsName"),"_")
  archiveDirs <- dir(self$modelPath,pattern=archiveNamePattern,full.names=TRUE)
  archiveDirs <- archiveDirs[dir.exists(archiveDirs)] # remove non-directories
  archiveFiles <- archiveDirs
  if ( archive && all.files ) {
    archiveFiles <- dir(archiveDirs,full.names=TRUE,recursive=TRUE)
  } else {
    archiveFiles <- archiveDirs
  }

  ResultsInRoot <- ( root && baseResults==self$modelPath )
  if ( results || ResultsInRoot  ) {
    # Handle the old-style case where ResultsDir==modelPath
    # ResultsDir is already normalized
    # We're only going to look for known result types ("artifacts")
    mstates <- dir(stagePaths,pattern="^ModelState(_[[:digit:]]{4}-.*)*\\.Rda$",full.names=TRUE)
    dstores <- dir(stagePaths,pattern="^Datastore(_[[:digit:]]{4}-.*)*$",full.names=TRUE)
    logs    <- dir(stagePaths,pattern="Log(_[[:digit:]]{4}-.*)+\\.txt",full.names=TRUE)
    qresults<- dir(stagePaths,pattern="^Query_.*.Rda$",full.names=TRUE)
    resultFiles <- c(mstates,dstores,logs,qresults)
  } else resultFiles <- character(0)

  if ( root ) {
    rootPath <- unique(normalizePath(self$modelPath))
    rootFiles <- dir(rootPath,full.names=TRUE)
    if ( ResultsInRoot ) {
      rootFiles <- setdiff(rootFiles,resultFiles) # Drop Log and ModelState
    }
    rootFiles <- setdiff(rootFiles, inputPath)    # need to specify "inputs=TRUE" to get inputPaths...
    rootFiles <- setdiff(rootFiles, baseResults)  # If we want those, they'll arrive via resultFiles
    rootFiles <- setdiff(rootFiles, archiveDirs)
    rootFiles <- setdiff(rootFiles, scenarioPath)

    stageDirs <- file.path(rootPath,sapply(stages,function(s) s$Dir))
    if ( length(stageDirs)==1 && normalizePath(stageDirs) == rootPath ) {
      stageDirs <- character(0)
    }

    if ( all.files ) {
      paramPaths <- self$setting("ParamPath",shorten=FALSE)
      for ( st in stages ) paramPaths <- c(paramPaths,self$setting("ParamPath",stage=st$Name,shorten=FALSE))
      paramPaths <- unique(paramPaths)
      rootFiles <- c( rootFiles, dir(stageDirs,full.names=TRUE) )
      rootFiles <- c( rootFiles, dir(paramPaths,full.names=TRUE) )
      scriptPaths <- file.path(self$modelPath,self$setting("ScriptsDir"))
      for ( st in stages ) scriptPaths <- c(scriptPaths,file.path(st$Path,self$setting("ScriptsDir",stage=st$Name,shorten=FALSE)))
      scriptPaths <- unique(scriptPaths[dir.exists(scriptPaths)])
      rootFiles <- c( rootFiles, dir(scriptPaths,full.names=TRUE) )
      queryPath <- file.path(rootPath,self$setting("QueryDir")) # QueryDir is always already "shortened"
      rootFiles <- c( rootFiles, dir(queryPath,full.names=TRUE) )
    }
  } else rootFiles <- character(0)

  files <- sort(unique(c(
    # in case nothing was asked for: list(list()[x]) would return NULL, not character(0)
    # So force the type for files by providing an empty string to add to NULL/nothing
    character(0), 
    unlist(
      list(
        inputFiles,scenarioFiles,outputFiles,
        resultFiles,rootFiles,archiveFiles) [c(inputs,scenarios,outputs,results,root,archive)]
    )
  )))
  if ( nzchar(shorten) ) {
    shorten <- paste0(shorten,"/")
    files <- sub(shorten,"",files,fixed=TRUE)
    if (showRootDir && length(files)>0 ) files <- c( shorten, files) # if shortening, modelPath is first element
  }
  return(files)
}

# Function to interactively remove prior model runs or extracts
ve.model.clear <- function(force=FALSE,outputOnly=NULL,archives=FALSE,stage=NULL,show=10) {
  # Remove outputs and/or results, either interactively or in batch
  # 'show' controls maximum number of outputs to display for selection
  # Can limit just to outputs or to results in a certain 'stage'
  # outputOnly=FALSE will show/clear results as well as outputs for deletion;
  #   if outputs exist, and outputOnly is NULL, we only show/clear those by default
  # "archives" TRUE will offer to delete results archives
  #   archives FALSE will ignore results archives
  # By default, clear is interactive. If "force=TRUE", then it will
  #   not interact. Instead it will delete all the outputs or archives.
  #   force says "just go ahead and delete everything", respecting outputOnly and archives,
  #   so the default is to delete the outputs and leave the results
  # If you use "force=TRUE" and clear again, nothing will happen uness you also
  #   explicitly set "outputOnly=FALSE" in which case the model results will get deleted
  #   (You probably don't want that!).
  # Result archives are always untouched, unless archives==TRUE, in which case
  #   all of them are considered for deletion just like outputs (that is, unlike
  #   results, the archives are considered "disposable").

  if ( ! private$p.valid ) {
    writeLog(self$printStatus(),Level="error")
    return( invisible(FALSE) )
  }
  
#  to.delete <- self$dir(outputs=TRUE,all.files=TRUE,stage=stage,showRootDir=FALSE)
  to.delete <- self$dir(outputs=TRUE,stage=stage,showRootDir=FALSE)
  if ( is.null( outputOnly ) ) {
    # Can't force delete of results without explicit outputOnly=FALSE
    outputOnly <- ( length(to.delete)>0 || force )
  }

  if ( ! isTRUE(outputOnly) ) to.delete <- c(to.delete,self$dir(results=TRUE,stage=stage,showRootDir=FALSE))

  # Only offer archives to delete if we're looking at all stages
  if ( isTRUE(archives) && is.null(stage) ) to.delete <- c(to.delete,self$dir(archive=TRUE,showRootDir=FALSE))

  # note, by default $dir shortens by removing self$modelPath
  # so deletion will only work if getwd()==self$modelPath
  if ( normalizePath(getwd() ) != self$modelPath ) {
    owd <- setwd(self$modelPath)
    on.exit(setwd(owd))
  }

  if ( ! isTRUE(outputOnly) && ! isTRUE(archives) && is.character(stage) ) {
    # keep only files in subdirectories matching stage$Dir in results
    # TODO: make this work so stages passed by name will have their results selected (only)
    stageDirs <- sapply(self$modelStages[stage],function(s) s$Dir)
    stage <- stage[ stage!="." & stage %in% stageDirs ]
    if ( any(stages) ) {
      stages <- paste("/",stages,"/")
      for ( stg in stages ) {
        to.delete <- to.delete[ grep(stg,to.delete) ] # keep the matching files
      }
    }
  }

  force <- ( force || ! ( interactive() && length(to.delete)>0 ) )
  if ( length(to.delete)>0 ) {
    if ( force ) {
      unlink(to.delete,recursive=TRUE)
    } else if ( ! force && length(to.delete)>0 ) {
      action = "h"
      start = 1
      stop <- if (show>0) min(start+show-1,length(to.delete)) else length(to.delete)
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
            action <- "h"
            next
          }
          response <- sub("^ *","",response)
        }
        if ( grepl("^[0-9:, ]+$",response) ) {
          response <- try ( eval(parse(text=paste("c(",response,")"))) )
          # Look for character command
          if ( is.numeric(response) ) {
            candidates <- to.delete[start:stop]
            unlink(candidates[response],recursive=TRUE)
            cat("Deleted:\n",paste(candidates[response],collapse="\n"),"\n")
          }
          to.delete <- self$dir(outputs=TRUE,showRootDir=FALSE)
          if ( ! isTRUE(outputOnly) ) to.delete <- c(to.delete,self$dir(results=TRUE,showRootDir=FALSE))
          if ( length(to.delete) > 0 ) {
            start <- 1
            stop <- if (show>0) min(start+show-1,length(to.delete)) else length(to.delete)
            action <- "h"
          } else {
            cat("No files remaining to delete.\n")
            action <- "q"
          }
        } else {
          action <- substr(response[1],1,1)
          if ( action %in% c("n","p") ) {
            if ( action == "n" ) {
              start <- min(start + show,length(to.delete))
              if ( start == length(to.delete) ) start <- max(length(to.delete),1)
              stop <- if (show>0) min(start+show-1,length(to.delete)) else length(to.delete)
            } else if ( action == "p" ) {
              start <- max(start - show,1)
              stop <- min(start+show-1,length(to.delete))
            }
          } else if ( action %in% c("h","q") ) {
            next
          } else action <- "h"
        }
      }
    }
  }
  if ( ! force ) cat("Nothing else to delete.\n")
  self$load(onlyExisting=TRUE,reset=TRUE) # Reload model states (now that results may be gone)
  return(invisible(force))
}

################################################################################
# VEModelStage Class
################################################################################

# Load the stage configuration
ve.stage.init <- function(modelParam_ls,Name=NULL,Model=NULL,ScenarioDir=NULL,stageParam_ls=list()) {
  # Name can force the stage name (otherwise use stageParam_ls)
  # Model is the model the stage is attached to
  # ScenarioDir is the root directory to seek the stage folder (default: Model$modelPath)
  # modelParam_ls is the configuration to be used (defaults to Model$loaedParam_ls)
  # stageParam_ls has the following elements (which are explicitly provided or come from
  #  a ModelStages specification in the model parameters):
  #
  #   Name is how we refer to the stage
  #     default is basename(Dir)
  #   Dir is the subdirectory for the stage, holding InputDir/ParamDir
  #     default of NULL becomes "."
  #   Path is the path to directory holding stage-specific InputDir/ParamDir
  #     default is ModelDir/Dir (or Dir itself if absolute path)
  #   Config is alternative path/name for "visioneval.cnf" for stage
  #     default is Path/visioneval.cnf. Relative to Path
  #   Reportable is an optional logical that says whether to include
  #     the stage in query results or exports
  #   StartFrom is an optional name of an earlier stage whose inputs and
  #     and Datastore will be added to this stage's paths
  #
  if ( ! is.character(Name) ) {
    if ( "Name" %in% names(stageParam_ls) ) {
      self$Name <- stageParam_ls$Name
    } else stop("No Name provided for model stage.")
  } else self$Name <- Name
  # Note that modelParam_ls should be provided during model initialization because
  #   RunParam_ls will not yet exist.
  # It can be missing if we're loading a model on the fly and RunParam_ls already exists
  if ( missing(modelParam_ls) || is.null(modelParam_ls) ) {
    modelParam_ls <- Model$RunParam_ls
    if ( is.null(modelParam_ls) ) modelParam_ls <- list()
  }

  if ( is.null(Model) || ! "VEModel" %in% class(Model) ) {
    stop("No VEModel provided to own model stage ",Name)
  } else self$Model <- Model

  readConfig <- if ( self$Name == "." ) {
    # model root directory being inserted as a stage
    # the "stage" configuration has already been read as the overall model configuration
    writeLog("Stage in root directory",Level="info")
    self$Name <- self$Model$modelName
    FALSE
  } else {
    TRUE
  }
  writeLog(paste("Reading config for named stage: ",self$Name,readConfig),Level="info")

  # Initialize model parameters
  ModelName     <- Model$modelName

  # Add name of model as stage name if stage name is "."
  # Also do not reload configuration since the model already has relevant stu

  # Set root in which to seek stages
  # Scenarios will start at a subdirectory of the base model
  if ( ! missing(ScenarioDir) && is.character(ScenarioDir) ) {
    ModelDir <- ScenarioDir
  } else {
    ModelDir <- Model$modelPath # Should already be a normalized path
  }

  # Pull stageParam_ls from ModelStages in modelParam_ls (mostly, we'll send stageParam_ls in as a parameter)
  if ( ( !is.list(stageParam_ls) || length(stageParam_ls)==0 ) && "ModelStages" %in% names(modelParam_ls) ) {
    msp <- modelParam_ls$ModelStages[[self$Name]]
    if ( ! is.null(msp) ) stageParam_ls <- msp
  }

  # Warn if stage is not unique
  if ( Name %in% names(Model$modelStages) ) {
    writeLog(paste0("Model Stage name ",self$Name," is already defined in ",ModelName),Level="info")
    # Not an error, as this object may replace the one already in the Model
    # That condition is trapped when this stage is pushed back into the model
  } else writeLog(paste0("Initializing Model Stage:",self$Name),Level="info")

  # Pull out InputPath from modelParam_ls and re-add it later
  if ( "InputPath" %in% names(modelParam_ls) ) {
    writeLog(paste("Model InputPath:",modelParam_ls$InputPath),Level="debug")
    modelInputPath <- modelParam_ls$InputPath # may be NULL, but more likely set to modelPath/InputDir
    modelParam_ls[["InputPath"]] <- NULL      # remove it from the list
  } else modelInputPath <- NULL
    
  if ( is.null(modelInputPath) ) writeLog("No InputPath from Model",Level="debug")

  # Parse the stageParam_ls (any of these may still be NULL)
  self$Dir               <- stageParam_ls$Dir
  self$Path              <- stageParam_ls$Path
  self$Config            <- stageParam_ls$Config
  self$Reportable        <- stageParam_ls$Reportable
  self$StartFrom         <- stageParam_ls$StartFrom
  self$ScenarioElements  <- stageParam_ls$ScenarioElements # May be null

  # Merge any remaining items defined in stageParam_ls (ModelStages structure) into
  #   stageConfig_ls (which overrides the modelParam_ls and stage Config file (if any))
  if ( length(stageParam_ls) > 0 ) {
    stageConfig_ls <- visioneval::addParameterSource(
      stageParam_ls[ ! names(stageParam_ls) %in% c("Dir","Path","Config","Reportable","StartFrom") ],
      Source = paste0(ModelName,"$modelStages[[",self$Name,"]]")
    )
  } else {
    stageConfig_ls <- list()
  }
  if ( length(stageConfig_ls) > 0 ) {
    writeLog(paste("Stage",self$Name,"stageConfig_ls contains:"),Level="debug")
    writeLog(paste(names(stageConfig_ls),collapse=", "),Level="debug")
    writeLog(paste("Stage Input Path from stageConfig_ls:",stageConfig_ls$InputPath),Level="debug")
    # TODO: stageConfig_ls$InputPath is correct here
  } else {
    writeLog("stageConfig_ls has no additional parameters",Level="debug")
  }

  # Set up self$Path (input location)
  if ( ! is.null(self$Path) ) {
    if ( isAbsolutePath(self$Path) ) {
      if ( ! dir.exists(self$Path) ) {
        writeLog(paste0("Stage path does not exist: ",self$Path),Level="info")
        self$Path <- NULL
      } else self$Path <- normalizePath(ModelDir,self$Path)
      if ( self$Path == ModelDir ) self$Path <- NULL
    }
  }
  if ( is.null(self$Path) ) { # attempt to construct
    if ( is.character(self$Dir) ) {
      self$Path <- normalizePath(file.path(ModelDir,self$Dir))
    } else {
      self$Path <- normalizePath(file.path(ModelDir,self$Name))
    }
    writeLog(paste("Stage path candidate:",self$Path),Level="debug")
    if ( ! dir.exists(self$Path) ) self$Path <- NULL
  }
  if ( ! is.null(self$Path) ) {
    writeLog(paste("Initializing Stage",self$Name,"from",self$Path),Level="debug")
  } else {
    writeLog("Stage path is NULL",Level="debug")
  }
  # self$Path may still be NULL, in which case we need self$Config
  #   or suitable values in stageConfig_ls to specify the required stage inputs
  
  # Set self$Dir (used for input and output)
  if ( is.null(self$Dir) ) {
    self$Dir <- if ( is.character(self$Path) ) basename(self$Path) else self$Name
  }

  ParamDir <- NULL
  if ( is.character(self$Config) ) {
    if ( ! isAbsolutePath(self$Config) ) {
      if ( is.character(self$Path) ) {
        self$Config <- file.path(self$Path,self$Config)
      } else {
        self$Config <- file.path(ModelDir,self$Config)
      }
    }
    if ( dir.exists(self$Config) ) {
      ParamDir <- self$Config
      ParamFile <- NULL
    } else if ( file.exists(self$Config) ) {
      ParamDir <- dirname(self$Config)
      ParamFile <- basename(self$Config)
    } else {
      writeLog(paste0("Could not locate configuration file: ",self$Config),Level="warn")
      ParamDir <- NULL
      ParamFile <- NULL
    }
  } else if ( ! is.null(self$Path) ){
    ParamDir <- self$Path
    ParamFile <- NULL
  } else {
    writeLog("No ParamDir for stage",Level="debug")
  }
  # If ParamDir is defined (and perhaps ParamFile), load the configuration file
  if ( ! is.null(ParamDir) ) {
    if ( readConfig ) {
      writeLog(paste("Loading configuration from",ParamDir),Level="info")
      self$loadedParam_ls <- visioneval::loadConfiguration(ParamDir=ParamDir,ParamFile=ParamFile)
    } else {
      writeLog("Not re-loading configuration for single-stage model with root configuration",Level="info")
      if ( is.null( self$loadedParam_ls ) ) self$loadedParam_ls <- list()
    }
  } else {
    writeLog("No configuration file for stage",Level="debug")
    self$loadedParam_ls <- list()
  }
  if ( length(stageConfig_ls) > 0 ) { # already logged stageConfig_ls
    self$loadedParam_ls <- visioneval::mergeParameters(self$loadedParam_ls,stageConfig_ls)   # parameters in ModelStage or command line override
  }
  if ( length(self$loadedParam_ls) > 0 ) {
    writeLog(paste("Stage",self$Name,"loadedParam_ls pre-merge contains:"),Level="debug")
    writeLog(paste(names(self$loadedParam_ls),collapse=", "),Level="debug")
    self$loadedParam_ls <- visioneval::mergeParameters(modelParam_ls,self$loadedParam_ls) # config file overrides model
  } # else Stage has no configuration - it's probably a single-stage model with no explict stage configuration
  writeLog(paste("Stage",self$Name,"loadedParam_ls contains:"),Level="debug")
  writeLog(paste(names(self$loadedParam_ls),collapse=", "),Level="debug")

  # Stage Output
  self$RunPath <- file.path(Model$modelResults,self$Dir)
  self$RunPath <- normalizePath(self$RunPath)
  writeLog(paste("Stage RunPath:",self$RunPath),Level="debug")

  # Set stage InputPath
  # If InputPath defined explicitly for the stage, look no further
  # Stage input path will be relative to StageDir
  # Otherwise:
  #   Add ModelDir/StageDir/InputDir if it exists
  #   Otherwise, add ModelDir/StageDir if it exists
  if ( "InputPath" %in% names(self$loadedParam_ls) ) {
    stageInput <- self$loadedParam_ls$InputPath
    writeLog(paste(nzchar(stageInput),"Stage InputPath is explicitly set:"),Level="debug")
    writeLog(paste(paste0("'",stageInput,"'"),collapse="\n"),Level="debug")
  } else {
    # Construct stage input path
    writeLog("Constructing stage InputPath",Level="debug")
    if ( is.character(self$Path) && dir.exists(self$Path) ) {
      stageInput <- file.path(self$Path,visioneval::getRunParameter("InputDir",self$loadedParam_ls))
      if ( is.null(stageInput) || ! file.exists(stageInput) ) {
        stageInput <- self$Path
      } else {
        stageInput <- c(stageInput,self$Path)
      }
      writeLog(paste0("Input path for ",self$Name,":"),Level="info")
      writeLog(paste(paste(stageInput,"(",file.exists(stageInput),")"),collapse="\n"),Level="info")
    } else stageInput <- NULL
  }

  # modelInputPath was extracted from modelParam_ls earlier (so it doesn't pre-empt the stage)
  # InputPath construction if no explicit stage InputPath was provided either through LoadedParam_ls
  # (used for explicit non-standard InputPath) or stageConfig_ls (used for category scenarios)
  if ( !is.null(stageInput) && any(file.exists(stageInput)) ) {
    writeLog("Adding stage InputPath to Model InputPath",Level="debug")
    writeLog(paste("Stage InputPath:",stageInput),Level="debug")
    stageInput <- c( stageInput, modelInputPath )
  } else {
    writeLog("No stage InputPath, using Model",Level="debug")
    stageInput <- modelInputPath
  }
  if ( ! is.null(stageInput) ) {
    writeLog(paste("Stage InputPath is",stageInput,collapse="\n"),Level="info")
    self$loadedParam_ls <- visioneval::addRunParameter(
      self$loadedParam_ls,
      Source="VEModelStage$initialize",
      InputPath=stageInput
    )
  } else {
    # Not an error if there is a runnable StartFrom stage with InputPath
    writeLog("No Input Path for Stage",Level="info")
  }
  # Still may have no explicit InputPath for Stage

  # Mark this stage as a scenario
  if ( "IsScenario" %in% names(stageConfig_ls) && stageConfig_ls$IsScenario ) {
    self$IsScenario = TRUE
  }

  # Identify "startFrom" stage (VEModelStage$runnable will complete setup)
  # Can find StartFrom through ModelStages or from the stage configuration file/parameters
  if ( !is.character(self$StartFrom) || length(self$StartFrom)==0 || ! nzchar(self$StartFrom) ) {
    # StartFrom was not set previously from stageParam_ls
    if ( "StartFrom" %in% names(self$loadedParam_ls) ) {
      self$StartFrom <- self$loadedParam_ls$StartFrom
      writeLog(paste("Starting From:",self$StartFrom),Level="debug")
    } else {
      writeLog("No StartFrom in self$loadedParam_ls:",Level="debug")
      writeLog(paste(names(self$loadedParam_ls),collapse=","),Level="debug")
      self$StartFrom <- character(0)
    }
  }
  # Wait for "runnable" setup to unpack StartFrom and to build final InputPath and DatastorePath
}

# Prepare the stage to run in the model context
# Don't do this during stage init because it depends on prior runnable stages
# Runnable is called in the context of overall model initialization/configuration
# reloadFile says to repload any on-disk stage configuration (visioneval.cnf in stage subdirectory)
# if reloadFile is false and stage$loadedParam_ls is not NULL, use existing stage$loadedParam_ls
ve.stage.runnable <- function(priorStages, updateCheck=TRUE) {

  writeLog(paste("Checking stage",self$Name,"is runnable"),Level="info")

  # Re-build the stage's RunParam_ls from the initialized elements
  # self$loadedParam_ls should contain the model RunParam_ls elemenst
  # plus elements loaded from this stage's visioneval.cnf
  self$RunParam_ls <- self$loadedParam_ls

  # dig out the information from the StartFrom stage (which must be NULL or length 0 or 1)
  if ( length(self$StartFrom) > 1 ) {
    stop (
      writeLog(
        paste("StartFrom must specify a single stage. Currently has:"),
        paste(self$StartFrom,collapse=", "),
        Level="error"
      )
    )
  }
  if ( length(self$StartFrom) > 0 && nzchar(self$StartFrom) ) {
    writeLog(paste("StartFrom:",self$StartFrom),Level="info")
    errMessage <- character(0)
    if ( ! self$StartFrom %in% names(priorStages) ) {
      writeLog(paste("priorStages: ",names(priorStages),collapse=", "),Level="info")
      errMessage <- paste("StartFrom stage,",self$StartFrom,"does not run before",self$Name)
    } else if ( ! (priorStages[[self$StartFrom]])$runnable(priorStages) ) {
      errMessage <- paste("StartFrom stage is not Runnable",self$StartFrom)
    }
    if ( length(errMessage)>0 ) {
      stop( writeLog(errMessage,Level="error") )
    }

    startFrom       <- priorStages[[self$StartFrom]]$RunParam_ls
    DatastorePath   <- startFrom$DatastorePath  # Need not exist, NULL if not present
    ParamPath       <- startFrom$ParamPath      # Need not exist, NULL if not present
    if ( ! is.null(ParamPath) ) {
      self$RunParam_ls <- visioneval::addRunParameter(
        self$RunParam_ls,
        Source="VEModelStage$runnable",
        ParamPath=ParamPath
      )
    }
    # There should be an InputPath for the StartFrom stage
    # Prepend the stage's InputPath
    InputPath     <- startFrom$InputPath      # Use this as the base InputPath, may be NULL
    if ( ! is.null(InputPath) ) {
      # self$RunParam_ls$InputPath will be NULL  if not set
      # and thus we'll just get InputPath which may also be NULL
      writeLog("Appending StartFrom InputPath:",Level="debug")
      writeLog(InputPath,Level="debug")
      InputPath <- c( self$RunParam_ls$InputPath, InputPath )
    } else {
      writeLog("No InputPath in StartFrom stage",Level="debug")
      writeLog(paste("contains",paste(names(startFrom),collapse=",")),Level="debug")
      InputPath <- self$RunParam_ls$InputPath
    }
    StartFromScriptPath <- startFrom$ModelScriptPath
  } else {
    writeLog("No StartFrom for stage",Level="debug")
    startFrom     <- list()
    DatastorePath <- character(0)             # Default is to contribute no DatastorePath
    ParamPath <- self$RunParam_ls$ParamPath   # May be NULL
    InputPath <- self$RunParam_ls$InputPath   # May be NULL
    StartFromScriptPath <- NULL
  }

  # Save InputPath into stage run parameters
  if ( ! is.null(InputPath) && length(InputPath)>0 ) {
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$runnable",
      InputPath=cullInputPath( InputPath=InputPath, modelInputPath=self$Model$setting("InputPath",shorten=FALSE,defaults=FALSE) )
    )
    writeLog(paste0("InputPath for ",self$Name,":"),Level="info")
    writeLog(paste(self$RunParam_ls$InputPath,paste0("(",file.exists(self$RunParam_ls$InputPath),")"),collapse="\n"),Level="info")
  } else {
    writeLog(paste("No InputPath for stage",self$Name),Level="debug")
  }

  # Construct DatastorePath, prepending ModelDir/ResultsDir/StageDir as
  #   first element (writable)
  self$RunParam_ls <- visioneval::addRunParameter(
    self$RunParam_ls,
    Source="VEModelStage$runnable",
    DatastorePath=c( self$RunPath, DatastorePath)
  )
  writeLog(paste0("DatastorePath for ",self$Name,": ",self$RunParam_ls$DatastorePath),Level="debug")

  # Underlay run_parameters.json from StagePath/ParamDir/ParamFile if it is out there
  # run_parameters.json is deprecated, and is expected only to provide "descriptive" parameters
  # e.g. Scenario or Description, possibly Years
  # New style model will set those in StageDir/visioneval.cnf
  writeLog("Looking for run_parameters.json",Level="debug")
  self$RunParam_ls <- visioneval::loadParamFile(Param_ls=self$RunParam_ls,ModelDir=self$Path)

  # Set up ModelScriptPath if current stage has different ModelScript from overall model
  ModelDir        <- visioneval::getRunParameter("ModelDir",Param_ls=self$RunParam_ls)
  ScriptName      <- visioneval::getRunParameter("ModelScript",Param_ls=self$RunParam_ls)
  ScriptsDir      <- visioneval::getRunParameter("ScriptsDir",Param_ls=self$RunParam_ls)
  stageModelScriptPath <- character(0)
  for ( scriptDir in unique( c(
    file.path(self$Path,ScriptsDir),
    file.path(self$Path),
    file.path(ModelDir,ScriptsDir),
    file.path(ModelDir)
  ) ) ) {
    ScriptPath <- file.path(scriptDir,ScriptName)
    if ( file.exists(ScriptPath) ) {
      stageModelScriptPath <- normalizePath(ScriptPath,winslash="/",mustWork=FALSE)
      break
    }
  }
  if (
    length(stageModelScriptPath)==0 &&
    ! is.null(StartFromScriptPath) &&
    file.exists(StartFromScriptPath)
  ) {
    stageModelScriptPath <- StartFromScriptPath
    # Applies when each stage is running with its own script (e.g. base population, remainder of
    # base year, full future year, and subsequent scenario stages use full future year with the
    # same script)
  }
  if ( length(stageModelScriptPath)>0 && nzchar(stageModelScriptPath) ) {
    writeLog(paste("Parsing stage ModelScriptFile:",stageModelScriptPath),Level="debug")
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$runnable",
      ModelScriptPath=stageModelScriptPath,
      ParsedScript=visioneval::parseModelScript(stageModelScriptPath)
    )
  }
  writeLog(paste("ModelScriptPath:",self$RunParam_ls$ModelScriptPath),Level="info")

  # Force DatastoreType to be explicit rather than default
  if ( ! "DatastoreType" %in% names(self$RunParam_ls) ) {
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$runnable",
      DatastoreType=visioneval::getRunParameter("DatastoreType",Param_ls=self$RunParam_ls)
    )
  }
  writeLog(paste("DatastoreType:",self$RunParam_ls$DatastoreType),Level="info")

  # Check if stage can run (enough parameters)
  missingParameters <- visioneval::verifyModelParameters(self$RunParam_ls)
  if ( length(missingParameters) != 0 ) {
    writeLog(
      c(
        paste("Candidate stage",self$Name,"is not Runnable"),
        paste("Missing",missingParameters,collapse=", ")
      ),
      Level="warn"
    )
    return(FALSE)
  }

  # Load ModelState.Rda if it already exists (prior run)

  if ( ! self$load(onlyExisting=TRUE,updateCheck=updateCheck) ) self$RunStatus <- codeStatus("Initialized")

  return(TRUE)
}

# Load the model state for the stage
# Create it if onlyExisting=FALSE (that's a time-consuming operation)
# if reset==TRUE, force reload/rebuild of ModelState when model runs
# otherwise, reload is skipped of a ModelState is already loaded.
ve.stage.load <- function(onlyExisting=TRUE,reset=FALSE, updateCheck=TRUE) {
  if ( reset ) self$ModelState_ls <- NULL
  if ( is.null(self$ModelState_ls) ) { # due to reset or not having been initialized in new object
    envir = visioneval::modelEnvironment(Clear="ve.stage.load")
    envir$RunModel <- FALSE
    # We're expecting not to write anything, but we'll set useful directories anyway.
    owd <- if ( dir.exists(self$RunPath) ) {
      setwd(self$RunPath)
    } else setwd(self$loadedParam_ls$ModelDir)
    on.exit(setwd(owd))
    writeLog(paste("Loading stage ModelState_ls for",self$Name,"onlyExisting =",onlyExisting),Level="info")
    ms <- visioneval::loadModel(
      self$RunParam_ls,
      onlyExisting=onlyExisting,
      updateCheck=updateCheck,
      Message=paste("Loading Model Stage",self$Name),
      log="info" # Don't need to see the Loading Model messge
    )

    if ( ! identical(ms,self$ModelState_ls) ) self$ModelState_ls <- ms
    if ( is.list(self$ModelState_ls) && length(self$ModelState_ls)>0 ) {
      if ( is.character(self$ModelState_ls$RunStatus) ) {
        # visioneval::initModelState will set RunStatus to character string ("Initialized")
        self$ModelState_ls$RunStatus <- codeStatus(self$ModelState_ls$RunStatus)
      }
      if ( ! "RunStatus" %in% names(self$ModelState_ls) ) {
        browser(text="why are we here (what's in self$ModelState_ls)?")
        self$ModelState_ls$RunStatus <- codeStatus("Loaded") # Only occurs with old-style model and an old run
      }
      if ( ! isTRUE(attr(self$ModelState_ls,"UpToDate")) && self$ModelState_ls$RunStatus == codeStatus("Run Complete") ) {
        # Internal stage configuration or input/structural files were altered
        self$ModelState_ls$RunStatus <- codeStatus("Out of Date")
      }
      self$RunStatus <- self$ModelState_ls$RunStatus
    } else {
      self$RunStatus <- codeStatus("Uninitialized")
      # Probably a freshly installed model
      # Will be replaced with "Initialized" once the RunParams are processed in ve.stage.load
      if ( ! onlyExisting) {
        writeLog(
          paste0("Unable to build model state for stage ",self$Name,"!"),
          Level="warn"
        )
      }
      return(FALSE) # ModelState unavailable
    }
  } else writeLog("Using pre-loaded ModelState_ls",Level="info")
  return(TRUE) # ModelState available
}

globalVariables(c("runPath", "RunParam_ls")) # attached in function environment when running
run.function <- function() {
  # Parameters (set in function environment - see execution below)
  #   runPath (directory in which to write log/ModelState/Datastore)
  #   RunParam_ls (big list structure with everything needed to run the model)
  #   log is also set in function environment (defaulting to "warn")
  

  #   message("Search path")
  #   message(paste("  ",search(),collapse="\n"))
  #   message("GlobalEnv contents")
  #   message(paste("  ",ls(".GlobalEnv"),collapse="\n"))

  # TODO: clear out runPath if there is a residue there...
  if ( ! dir.exists(runPath) ) dir.create(runPath)

  owd <- setwd(runPath) # may not need inside a future
  on.exit(setwd(owd))

  # Take ownership of ve.model
  ve.model <- visioneval::modelEnvironment(Clear="VEModelStage::run") # add Owner
  stageName <- RunParam_ls$ModelStage # RunParam_ls is set in run.env
  stageName <- if ( ! is.character(stageName) ) { character(0) } else {
    paste(" for stage",paste0("'",stageName,"'"))
  }
  RunStatus <- try (
    {
      # Initialize Log, create new ModelState
      ve.model$RunModel <- TRUE
      visioneval::initLog(Threshold=log,Save=TRUE,envir=ve.model) # Log stage (log set in run.env)
      visioneval::loadModel(RunParam_ls) # Rebuild the ModelState_ls (RunParam_ls set in run.env)
      visioneval::setModelState()        # Save ModelState.Rda
      visioneval::prepareModelRun()      # Initialize Datastore

      # Run the model script
      sys.source(RunParam_ls$ModelScriptPath,envir=new.env())

      # Report completion into model log
      visioneval::writeLog(paste0("Model Run Complete",stageName),Level="warn")
      # Use the following RunStatus if we got this far without error
      codeStatus("Run Complete")
    },
    silent=TRUE
  )

  # Process results (or errors)
  if ( ! is.numeric(RunStatus) ) {
    # Failure: stop trapped while performing run sequence
    msg <- as.character(RunStatus) # try-error (captures "stop" message)
    writeLog(msg,Level="error")
    RunStatus <- structure( codeStatus("Run Failed"), reason=msg ) # append an attribute
    if ( "ModelState_ls" %in% names(visioneval::modelEnvironment()) ) {
      visioneval::setModelState(list(RunStatus=RunStatus),envir=ve.model)
    }
  } else {
    # Success: Assemble the runResults
    visioneval::setModelState(list(RunStatus=RunStatus),envir=ve.model)
  }
  # Reset log to console (in case running in "direct")
  visioneval::saveLog(LogFile="console",envir=ve.model)

  return(RunStatus)
}

ve.stage.running <- function() {
  return (
    ! is.null( self$FutureRun ) && # No process created yet
    ! is.null( self$RunStarted) && # Start time not yet recorded
    ! future::resolved( self$FutureRun )   # not resolved yet
  )
}

tformat <- function(tm) format(tm,"%Y-%m-%d %H:%M:%S")
tdiff <- function(tm0,tm1,digits=3,units="mins") {
  if ( missing(units) && units != "secs" ) {
    # used for display
    sub("mins","minutes",format( difftime(tm1,tm0,units="mins"), digits=digits ))
  } else {
    # used internally for delay interval
    as.double(difftime(tm1,tm0,units="secs"),units="secs")
  }
}

# report whether scenario level is in this stage
# if level is missing, report the named vector of non-zero levels
ve.stage.levels <- function(level) {
  return(character(0))
}

# Return process status
ve.stage.pstatus <- function(start=FALSE) {
  paste0(
    paste(
      if ( is.null(self$RunStarted) ) {
        "Not started"
      } else if ( start ) {
        paste("Started at",tformat(self$RunStarted))
      } else if ( ! is.null(self$RunCompleted) && ! is.null(self$RunStarted) ) {
        paste("Completed in",tdiff(self$RunStarted,self$RunCompleted))
      } else {
        paste("Running for",tdiff(self$RunStarted,Sys.time()),"since",tformat(self$RunStarted))
      }
    ),
    " : ",self$Name
  )
}

# Run the model stage
ve.stage.run <- function(log="warn",UseFuture=TRUE) {
  # Mark local status on the stage
  self$RunStatus <- codeStatus("Running")
  self$RunStarted <- Sys.time()
  self$RunCompleted <- NULL

  # Remove any existing ModelState and run results from this stage
  # Will reload later once the run is complete

  self$ModelState_ls <- NULL
  self$Results       <- NULL

  # Set up run parameters
  run.env <- new.env()
  run.env$runPath <- self$RunPath
  run.env$RunParam_ls <- visioneval::addRunParameter(Param_ls=self$RunParam_ls,Source="Stage Run",ModelStage=self$Name)
  run.env$log=log

  # Create a future (will block if no available processors)
  if ( UseFuture ) {
    # Run in a future (other environment/process)
    # Note that log is not visible
    # Don't log stage name since that will be reported when future is created
    run.future <- run.function # Solve scoping problem when using pkgload...
    self$FutureRun <- future::future(
      {
        environment(run.future) <- run.env
        run.future()
      },
      globals=c("run.future","run.env"),
      seed=visioneval::getRunParameter("Seed",Param_ls=self$RunParam_ls),
      packages="visioneval"
    )
  } else {
    # Run inline
    writeLog(c("\n",paste("Running stage:",self$Name)),Level="warn")
    environment(run.function) <- run.env
    runStatus <- run.function()
    self$completed( runStatus )
  }

  return(self)
}

ve.stage.completed <- function( runStatus=NULL ) {
  self$RunCompleted <- Sys.time()
  if ( !is.null(self$FutureRun) ) {
    runStatus <- future::value(self$FutureRun)
    if ( ! is.numeric(runStatus) ) {
      writeLog(paste("Error running stage",self$Name),Level="error")
      stop( writeLog(runStatus) )
    }
  }
  self$RunStatus <- runStatus
  self$load(onlyExisting=TRUE,updateCheck=FALSE) # Don't need to check since we just ran it
}

# Print a model stage summary
ve.stage.print <- function(details=FALSE,configs=FALSE) {
  cat(if(self$IsScenario) "Scenario" else "Stage",": ",self$Name,sep="")
  if ( details ) {
    startFrom <- if ( length(self$StartFrom)==1 && nzchar(self$StartFrom) ) paste0("StartFrom: ",self$StartFrom)
  } else startFrom <- NULL
  statusText <- printStatus(self$RunStatus)
  reportable <- if ( !is.null(self$Reportable) && self$Reportable ) "Reportable" else NULL
  cat(" (",paste( c(statusText, reportable, startFrom),collapse=", " ),")\n",sep="" )
  if ( configs ) {
    cat("   Configurations:\n")
    cat(paste0("     ",uniqueSources(self$RunParam_ls,shorten=self$Model$modelPath)),sep="\n") # Generates one row for each unique source
  }
}

################################################################################
#                              Model Information                               #
################################################################################

# Helper function:
# parse a table of package/module inputs from assembled AllSpecs_ls
# returns a data.frame summarizing the inputs

# Unique Names in specifications:
# Not all names will be present in all specs
specNames <- c(
  "PACKAGE","MODULE",
  "NAME", "FILE", "TABLE", "GROUP", "TYPE",
  "UNITS", "NAVALUE", "SIZE", "PROHIBIT", "ISELEMENTOF",
  "UNLIKELY", "TOTAL", "DESCRIPTION", "INPUTDIR", "MULTIPLIER",
  "YEAR", "OPTIONAL","SPEC","STAGE"
)
# SPEC is added to what might otherwise be there,
#   and it contains either "Inp", "Get" or "Set"

# stage is the name of the stage containing AllSpecs_ls 
summarizeSpecs <- function(AllSpecs_ls,stage) {
  specFrame <- data.frame()
  dummyRow <- rep(as.character(NA),length(specNames))
  specFrame <- rbind(specFrame,dummyRow) # establish data.frame names
  names(specFrame) <- specNames
  for ( packmod in AllSpecs_ls ) {
    package <- packmod$PackageName
    module <- packmod$ModuleName
    spc <- packmod$Specs;
    for ( spectype in c("Inp","Get","Set") ) {
      if ( ! spectype %in% names(spc) ) next
      addSpecs <- lapply(spc[[spectype]],
        function(x) {
          # Set SPEC type and add other missing names as <NA>
          for ( f in 1:length(x) ) {
            if ( length(x[[f]])>1 || !is.character(x) ) {
              x[[f]] <- paste(x[[f]],collapse=", ")
            }
          }
          x$SPEC <- spectype
          x$PACKAGE <- package
          x$MODULE <- module
          x$STAGE <- stage
          miss <- ! names(x) %in% specNames
          x[miss] <- as.character(NA)
          unlist(x)
        }
      )
      # bind the augmented specs into the data.frame of all specs
      # In principle could use do.call(rbind,addSpecs) but it's too hard to get
      #  the arguments right
      for ( sp in addSpecs ) {
        specFrame <- rbind(specFrame[,specNames],sp[specNames]) # Force name order
      }
    }
  }
  specFrame <- specFrame[!is.na(specFrame$SPEC),] # remove dummyRow
  return(specFrame)
}

# List the model contents
ve.model.list <- function(inputs=FALSE,outputs=FALSE,details=NULL,stage=character(0),reset=FALSE) {
  # "inputs" lists the input files (Inp) by package and module (details = field attributes)
  # "outputs" lists the fields that are Set in the Datastore (details = field attributes)
  # if both are true, we also list the Get elements
  # "details" FALSE lists units and description plus pacakge/module/group/table/name
  # "details" TRUE lists all the field attributes (the full data.frame of specSummary)
  # "details" can also be a character vector of specification field names (to generate a subset of details)
  # If "details" is NULL (the default) it selects a small subset of (hopefully useful) field specifications
  # "stage" is a list of stage names (default = all stages) to appear in the output
  #   The field specifications shown are just the ones accessed in that/those particular stages
  # If you call "list" multiple times, it will use cached results. If you change the model after
  # loading it, reset=TRUE will force a complete reload from the run_model.R script(s).

  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( invisible(data.frame()) )
  }

  # Update specSummary
  if ( reset || is.null(self$specSummary) ) {
    writeLog("Loading model specifications (may take some time)...",Level="warn")
    self$load(onlyExisting=FALSE,reset=TRUE) # Create new model states
    for ( stg in self$modelStages ) {
      writeLog(paste("Loading Stage specs for",stg$Name),Level="info")
      AllSpecs_ls <- stg$ModelState_ls$AllSpecs_ls
      if ( ! is.null( AllSpecs_ls ) ) {
        specFrame <- summarizeSpecs(AllSpecs_ls,stg$Name)
        if ( is.null(self$specSummary) ) {
          self$specSummary <- specFrame
        } else {
          self$specSummary <- rbind(self$specSummary,specFrame)
        }
        writeLog(paste("Number of specs:",nrow(self$specSummary)),Level="info")
      } else {
        writeLog(paste("No specifications for",stg$Name),Level="warn")
      }
    }
    writeLog(paste("Loaded",nrow(self$specSummary),"Specifications"),Level="info")
  } else {
    writeLog("Using existing stage summary",Level="info")
  }

  # which rows to return
  inputRows <- if ( inputs ) which(self$specSummary$SPEC=="Inp") else integer(0)
  outputRows <- if ( outputs ) which(self$specSummary$SPEC=="Set") else integer(0)
  usedRows <- if ( inputs == outputs ) which(self$specSummary$SPEC=="Get") else integer(0)
  listRows <- unique(c(inputRows,outputRows,usedRows))

  # Determine which stages to return
  if ( is.numeric(stage) ) stage <- names(self$modelStages)[stage]
  if ( length(stage) > 0 ) {
    whichStages <- which(self$specSummary$STAGE==stage)
    listRows <- intersect(listRows,whichStages)
  }

  # which fields to return
  listFields <- c("SPEC","STAGE","PACKAGE","MODULE","GROUP","TABLE","NAME")
  addFields <- character(0)
  if ( ! is.null(details) ) {
    if ( is.character(details) ) {
      addFields <- details[which(details %in% names(self$specSummary))]
    }
    if ( is.logical(details) ) {
      if ( details ) {
        addFields <- setdiff(names(self$specSummary),listFields)
      } else {
        addFields <- c("UNITS","DESCRIPTION")
      }
    }
  }
  listFields <- c(listFields, addFields)

  return(self$specSummary[listRows,listFields])
}

# Print a summary of the VEModel, including its run status
ve.model.print <- function(details=FALSE,configs=FALSE,scenarios=FALSE) {
  if ( ! private$p.valid ) {
    cat("Model object is not valid\n")
    return(private$p.valid)
  }
  cat("Model:",self$modelName,"\n")
  if ( details ) {
    cat("Path:","\n")
    cat(self$modelPath,"\n")
    cat("Configurations:","\n")
    cat(paste("  ",uniqueSources(self$RunParam_ls,shorten=self$modelPath)),sep="\n") # Generates one row for each unique source
  }
  cat("Status:", self$printStatus(),"\n")
  scenarios <- self$modelScenarios
  scenarioStages <- sapply( self$modelStages, function(s) s$IsScenario )
  cat("Model Stages:\n")
  for ( s in self$modelStages[ ! scenarioStages ] ) {
    s$print(details,configs)
  }
  scenarioCount <- length(which(scenarioStages))
  if ( scenarioCount > 0 || ! is.null(self$modelScenarios) ) {
    if ( ! details ) {
      cat(scenarioCount,"Scenario stages defined in",sub(self$modelPath,"",self$modelScenarios$scenarioPath),"\n")
    } else {
      cat("Scenario Stages from",sub(self$modelPath,"",self$modelScenarios$scenarioPath),"\n")
      for ( s in self$modelStages[ scenarioStages ] ) {
        s$print(details,configs=FALSE) # don't show configs for scenarios...
      }
    }
  } else if (scenarioCount > 0 && is.null(self$modelScenarios) ) {
    cat("Program error: scenarioCount",scenarioCount,"but modelScenarios is NULL\n")
  } else cat("No scenarios defined.\n")
  return(private$p.valid)
}

# Return the log file location (shorten=FALSE appends full model path)
ve.model.log <- function(shorten=TRUE) {
  # Report log files for each model stage (a vector)
  logs <- character(0)
  for ( s in self$modelStages ) {
    logFile <- s$ModelState_ls$LogFile # may be NULL
    if ( is.null(logFile) ) {
      # See if there's one in the RunPath anyway...
      p <- s$RunPath
      logFile <- dir(p,pattern="//.log$",full.names=TRUE)
    }
    if ( is.character(logFile) && length(logFile)>0 ) {
      if ( ! shorten ) logFile <- normalizePath(file.path(s$RunPath,logFile))
      logs <- c(logs,logFile)
    }
  }
  return(logs)
}

# Ordered Factor for RunStatus levels

StatusLevelCodes <- c(
  "Unknown",             # Catchall; should not appear, but is functionally "Uninitialized"
  "Uninitialized",       # Model or stage exists but has no configuration
  "Run Failed",          # An error prevented the model script from running to completion
  "Out of Date",         # Model or stage changed since current results were generated
  "Load Failed",         # Some configuration error or missing file prevented loading the model
  "No Stages Defined",   # Model configuration does not define any stages
  "Initialized",         # Model stage is ready to run
  "Loaded",              # Model state loaded from results generated by pre-3.0 VisionEval
  "Running",             # Model is running (or crashed) and results are possibly incomplete
  "Run Complete"         # Model stage ran to successful completion
)

# Helper - Decode status and handle "out of bounds"
printStatus <- function(status) {
  if ( ! is.numeric(status) || status < 1 || status > length(StatusLevelCodes) ) {
    status <- codeStatus("Unknown")
  }
  return( StatusLevelCodes[status] )
}

# Turn text representation ("level") into a status code
codeStatus <- function(level) {
  if ( is.character(level) ) {
    level <- which(StatusLevelCodes==level)
  } else {
    level <- integer(0)
  }
  return( 
    if ( length(level)!= 1 ) 1 else level
  )
}

# Return text representation of the status
ve.model.printStatus <- function(status=NULL) {
  if ( is.null(status) ) {
    status <- self$overallStatus
  }
  return( printStatus(status) )
}

# Show "minimum" stage status (see StatusLevelCodes)
ve.model.updateStatus <- function() {
  private$p.valid <- is.list(self$modelStages) && length(self$modelStages)>0
  self$overallStatus <- if ( private$p.valid ) {
    min(sapply(self$modelStages,function(s) s$RunStatus))
  } else {
    codeStatus("No Stages Defined")
  }
}

# Load the model from configuration, respecting existing results
# if onlyExisting, just load existing ModelState
# if reset==TRUE, force rebuild of the ModelState_ls when the model runs
# if outOfDate is a list of numeric indices, mark those stages as "Out of Date"
#   regardless of their internal check
ve.model.load <- function(onlyExisting=TRUE,reset=FALSE, outOfDate=NULL, updateCheck=TRUE) {

  if ( ! self$valid() ) {
    stop(
      writeLog("Model is incomplete and cannot be loaded.",Level="error")
    )
  }

  # Un-cache the specSummary (things may have changed)
  self$specSummary <- NULL

  # Load or Create the ModelState_ls for each stage
  # Includes a copy of the model state for the StartFromStage
  for ( index in seq_along(self$modelStages) ) {
    stage <- self$modelStages[[index]]
    writeLog(paste("Checking stage status",stage$Name),Level="info")

    # Load the current stage model state
    loadStatus <- stage$load(onlyExisting=onlyExisting,reset=reset,updateCheck=updateCheck)

    # Check if complete stage was outdated due to StartFrom stage
    if ( loadStatus && stage$RunStatus == codeStatus("Run Complete") ) {
      self$fixRunStatus(stage, outOfDate) # Check if its StartFrom is out of date or user reset stage
    }
    # Now see if this stage (and its followers) were requested for individual reset
    if ( is.numeric(outOfDate) && index %in% outOfDate  ) {
      # Caution: will cascade to stages that StartFrom this one,
      # so watch out if explicitly re-running base stage of many scenarios...
      writeLog(paste("User requested reset for stage",self$Name),Level="info")
      stage$RunStatus <- codeStatus("Out of Date")
    }
    self$modelStages[[index]] <- stage
  }
  # Update self$overallStatus
  self$updateStatus()
  return(self$printStatus())
}

ve.model.fixRunStatus <- function(stage,outOfDate=integer(0)) {
  writeLog("Checking if previous run is out of date",Level="info")
  if ( # There is a StartFrom stage
    is.character( stage$RunParam_ls$StartFrom ) &&
    length( stage$RunParam_ls$StartFrom )>0 &&
    nzchar(stage$RunParam_ls$StartFrom)
  ) {
    startFrom <- stage$RunParam_ls$StartFrom
    startFromState_ls <- self$modelStages[[startFrom]]$ModelState_ls # may be NULL
    if ( ! is.list(startFromState_ls) ) {
      # No ModelState exists for StartFrom stage (should never happen; should at least get "Initialized")
      writeLog(paste("(internal warning) StartFrom",startFrom,"for stage",self$Name,"has not been built."),Level="warn")
      stage$RunStatus <- codeStatus("Uninitialized")
    } else {
      sfsStatus <- startFromState_ls$RunStatus # may be null
      if ( ! is.null(sfsStatus) ) {
        if ( sfsStatus != codeStatus("Run Complete") ) {
          writeLog(paste("StartFrom stage",startFrom,"for stage",self$Name,"is not Run Complete"),Level="info")
          stage$RunStatus <- sfsStatus # All stages follow their parent's incomplete status
        } else { # StartFrom Stage has run; check if this stage was run before it
          writeLog(paste("Checking LastChanged date",self$Name),Level="info")
          if ( ! "LastChanged" %in% names(startFromState_ls) ) {
            # Shouldn't happen - "Run Complete" implies that something got changed in the
            # the earlier stage's model state
            writeLog(paste("(internal error) StartFrom for",self$Name,"is Run Complete but never changed."),Level="error")
            stage$RunStatus <- codeStatus("Out of Date")
          } else if (
            "LastChanged" %in% names(stage$ModelState_ls)  &&
            stage$ModelState_ls$LastChanged < startFromState_ls$LastChanged
          ) {
            writeLog(paste(stage$Name,"out of date due to LastChanged"),Level="info")
            stage$RunStatus <- codeStatus("Out of Date")
          }
          # If we get this far, Run Complete status for this stage survived!
        }
      } else { # sfsStatus is NULL
        if ( stage$RunStatus != codeStatus("Initialized") ) {
          stage$RunStatus <- codeStatus("Uninitialized")
        }
      }
    }
  }
  return( stage$RunStatus )
}


################################################################################
#                                 Running Models                               #
################################################################################

# TODO: Consult SaveDatastore in self$ModelState_ls
# TODO: Make sure a classic model (where SaveDatastore may be buried rather deep)
#       can still be interpreted properly. We still need to pull out the "initializeModel"
#       parameters...

# Proxy some visioneval functions (things that might get called from
# classic run_model.R without namespace resolution).
getYears <- visioneval::getYears
runModule <- visioneval::runModule
runScript <- visioneval::runScript
requirePackage <- visioneval::requirePackage

# Helper to get multitasking strategy name
futureStrategyName <- function(strategy) {
  return (
    if ( ! is.null(strategy) && "FutureStrategy" %in% class(strategy) ) {
      # Hack from future::print.FutureStrategy
      # Get first class that is not one of the structural ones
      setdiff(class(strategy), c("FutureStrategy", "tweaked", "function"))[1]
    } else {
      "sequential"
    }
  )
}

# Report status of model stages (use model$printStatus for overall)
ve.model.stageStatus <- function(stage=NULL,statusCode=codeStatus("Run Complete"),limit=10) {
  if ( is.null(stage) ) stage <- names(self$modelStages)
  cat(sep="","Model stages with status '",printStatus(statusCode),"' (",statusCode,")\n")
  countdown <- limit
  for ( s in self$modelStages[stage] ) {
    cat(sep="",s$RunStatus==statusCode," (",s$RunStatus,"): ",s$Name,"\n")
    if ( limit>0 && (countdown <- countdown-1) == 0 ) {
      cat("... Stopping display after",limit,"stages (out of ",length(stage),")\n")
      break
    }
  }
}

knownPlans <- c("inline", "sequential", "callr", "multisession")
# "inline" runs the stage by directly calling run.function
# "sequential" gets re-mapped to "inline"
# "callr" spawns a background RTerm to run the process
# "multisession" uses the internal multi-thread run method from future package
# "inline" is the default (and used for unknown plans).
# other strategies can be implemented by directly modifying self$FuturePlan and self$Workers
# deeper hacking may be required if the strategy does not take a "workers" parameter
# or if it needs something else

#' @import parallelly
ve.model.plan <- function(plan="callr",workers=parallelly::availableCores(omit=1)) {
  # options for the plan are described in knownPlans
  if ( ! is.character(plan) || ! plan %in% knownPlans ) plan <- knownPlans[1]
  self$FuturePlan <- plan
  self$Workers <- workers
}

# Run the modelStages
ve.model.run <- function(run="continue",stage=character(0),watch=TRUE,dryrun=FALSE,limit=0,log="warn") {
  # run parameter can be
  #   "continue" (run all steps, starting from first incomplete; "reset" is done on the first
  #      incomplete stage and all subsequent ones, then execution continues)
  #   or "save" in which case we reset, but first save the ResultsDir tree
  #   or "reset" (or "restart") in which case we restart from stage 1, but first clear out ResultsDir (no save)
  #
  # "reset" implies deleting any ModelState or Datastore
  # "continue" will unlink/recreate any stages that are not "Run Complete" (including those that
  #    are out of date due to a change in configuration or inputs files, plus any later complete
  #    stages that may StartFrom one of those).

  # "stage" parameter is ignored if run != "continue"
  # "stage" can be specified either as an index position in self$modelStages or as a named position
  # if "stage" is provided, the stages will be marked as "Out of Date" and the run will start from
  #   the first stage that is not still "Run Complete"
  # "stage" could be a vector of stages (names or numbers) - just those stages will be reset or
  #  re-run, plus any that StartFrom them

  # if "limit" is greater than zero, only run that number of stages, counting the first
  #   incomplete stage as "1".

  # "watch", if TRUE, applies if only one stage is running in a group (no multiprocessing for whatever reason)
  #   It will open a non-blocking connection to the stage log and echo out whatever is written there
  #   before polling again for completion.

  # "dryrun", if TRUE, will not actually run the stages - just report which stages will run,
  # respecting the "run", "stage" and "limit" parameters, plus any existing Run Complete results

  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( invisible(self$overallStatus) )
  }

  # If save, like reset, but forces SaveDatastore to be TRUE
  # If reset, then go back to the first stage and run from there

  # If continue, leave existing ResultsDir, but find the first incomplete stage and reset it,
  #   then continue from there.

  # "continue" will step through the stages looking for the first that is not "Run Complete"
  # SaveDatastore is ignored in that case.
  # That stage will be re-initialized.

  # Set up workingResultsDir for file manipulations (including stage sub-directories)
  workingResultsDir <- self$modelResults

  # Determine which stages need to be-rerun

  # Use the 'stage' parameter name or number
  stageNames <- names(self$modelStages)
  if ( is.character(stage) ) { # pre-process character stages to integers
    stage <- match( stage, stageNames, nomatch=0 ) # 0 indices will be ignored
    stage <- sort( unique( stage[stage>0] ) ) # get rid of those zeroes anyway
  }
  if ( is.numeric(stage) ) {
    stage <- stage[ stage<=length(self$modelStages) ]
  } else {
    stop(
      writeLog("Invalid 'stage' parameter to $run",Level="error")
    )
  }
  # Stages requested manually are passed to self$load and marked as "Out of Date"

  completeStatus <- codeStatus("Run Complete")
  if ( run=="continue" ) {
    self$load(onlyExisting=TRUE,reset=TRUE,outOfDate=stage)
    # self$load includes check for "Out of Date" on each stage, and will also mark
    # any stage passed in the outOfDate list as "Out of Date"

    checkAlreadyRun <- function(s) {
      if ( s$RunStatus == completeStatus &&
        length(s$StartFrom>0) && nzchar(s$StartFrom) &&
        self$modelStages[[s$StartFrom]]$RunStatus != completeStatus
      ) s$RunStatus <- outOfDateStatus else s$RunStatus
    }

    outOfDateStatus <- codeStatus("Out of Date")

    alreadyRun <- completeStatus == sapply( self$modelStages, checkAlreadyRun )

    # Bail out here if everything is done
    if ( all(alreadyRun) ) {
      self$overallStatus <- completeStatus
      writeLog("Model Run Complete",Level="warn")
      return(invisible(self$results()))
    }

    needToRun <- which( ! alreadyRun ) # numeric indices of stages needing to run
    if ( dryrun ) writeLog(c("Not already run:",names(self$modelStages)[needToRun]),Level="warn")
  } else {
    needToRun <- 1:length(self$modelStages)
  }
  toRun <- needToRun[1] # start from first un-run or out-of-date stage
  
  if ( toRun == 1 && run != "save" ) run <- "reset" # If starting over, process SaveDatastore as needed
  writeLog(paste("Starting stage to run:",toRun),Level="info")

  if ( dryrun ) writeLog(c("Need to Run:",names(self$modelStages)[needToRun]),Level="warn")

  # Save existing results if we're restarting or resetting
  SaveDatastore = NULL # ignore any pre-configured value for SaveDatastore
  if ( ! dryrun ) {
    if ( run == "restart" || run=="reset" ) {
      SaveDatastore <- FALSE
      writeLog(paste("Removing previous Results from",workingResultsDir),Level="warn")
      unlink(dir(workingResultsDir,full.names=TRUE),recursive=TRUE)
    } else if ( run == "save" ) {
      SaveDatastore <- TRUE
      run <- "reset"
    } else {
      SaveDatastore <- visioneval::getRunParameter("SaveDatastore",Param_ls=self$RunParam_ls)
    }

    # Archive previous results if SaveDatastore true in RunParam_ls and previous results exist
    if ( run != "continue" && isTRUE(SaveDatastore) && dir.exists(workingResultsDir) ) {
      if ( ! self$archive(SaveDatastore=SaveDatastore) ) {
        writeLog(
          paste0("Failed to save prior results; Continuing anyway..."),
          Level="error"
        )
      }
    }

    # Recreate the results directory in case it is not there after archiving or on first run
    if ( ! dir.exists(workingResultsDir) ) {      
      dir.create(workingResultsDir,showWarnings=FALSE)
      # Later create sub-directories for the stages as necessary
    }
  } else {
    writeLog("Would SAVE previous results",Level="warn")
  }

  # Identify stages to run
  # needToRun constructed above is a list of indices into self$modelStages
  if ( toRun > max(needToRun) ) { # should never happen
    writeLog(paste("Stages:",names(self$modelStages[needToRun]),collapse=","),Level="info")
    msg <- writeLog(paste("Starting stage",toRun,"comes after last stage to run",max(needToRun)),Level="error")
    return( invisible(self$overallStatus) )
  }

  # Optionally limit the number of stages to run...
  # We'll do this by chopping off the needToRun list of Model Stage indices
  if ( limit > 0 ) needToRun <- needToRun[1:limit]

  # Identify the stages to run by their name rather than their position
  runStages <- names(self$modelStages)[ needToRun ]

  # Destroy cached model state in each running stage.
  #   Not doing this introduces a weird bug where previous versions of
  #   script functions will be used from the cached model state rather
  #   than the latest function version in the package (e.g. if the
  #   cached state was from a run that quit in the middle of a
  #   "browser" call, the browser would continue to be called...).
  if ( ! dryrun ) {
    sapply( runStages,function(s) {
      self$modelStages[[s]]$ModelState_ls <- NULL
    } )
  }

  owd <- getwd()
  on.exit(setwd(owd))

  # Establish the LogLevel from the overall environment
  LogLevel <- visioneval::getRunParameter("LogLevel",Default=log,Param_ls=self$RunParam_ls)

  # Organize the stages into run groups (Stages that have identical StartFrom stages)
  # We'll do this by walking through the modelStages in order to preserve the stage order
  RunGroups <- list()
  for ( sn in runStages ) {
    stage <- self$modelStages[[sn]]
    sf <- if ( length(stage$StartFrom)>0 ) stage$StartFrom else "--"
    if ( ! sf %in% names(RunGroups) ) {
      RunGroups[[sf]] <- sn
    } else {
      RunGroups[[sf]] <- c( RunGroups[[sf]], sn )
    }
  }
  if ( dryrun ) writeLog(c("Run Groups:",names(RunGroups)),Level="warn")

  # Don't bother with processing setup if not actually running
  if ( ! dryrun ) {
    # Set up multiprocessing plan if requested
    # Default is just to call run.function directly
    UseFuture <- TRUE
    if ( is.null(self$FuturePlan) ) self$plan("inline")
    if ( self$FuturePlan == "callr" ) {
      oplan <- future::plan(future.callr::callr,workers=self$Workers)
    } else if ( self$FuturePlan == "multisession" ) {
      oplan <- future::plan(future::multisession,workers=self$Workers)
    } else {
      # TODO: extend to allow manually setting self$FuturePlan and self$Workers
      # Do not supply the workers parameter if self$Workers is NULL
      oplan <- NULL
      UseFuture <- FALSE
    }
    if ( ! is.null(oplan) ) {
      writeLog(paste("Processing model stages using",self$FuturePlan),Level="warn")
      on.exit( future::plan(oplan), add=TRUE )
    } else {
      writeLog("Processing model stages inline",Level="warn")
    }

    # Process the run groups (stages in the same RunGroup can run in parallel)
    # Can't start a new run group until the earlier one is done
    if ( UseFuture ) {
      delay <- self$setting("RunPollDelay")           # how long between polls (order of magnitude 2 seconds)
      statusDelay <- self$setting("RunStatusDelay")   # how long between status reports (order of magnitude 1 minute)
    }
  }

  for ( rgn in names(RunGroups) ) {
    runMsg <- if (dryrun) "Would Run" else "Running"
    writeLog(paste(runMsg,"Stages where StartFrom =",rgn),Level="warn")

    # Check if there is anything to do in this RunGroup
    rg <- RunGroups[[rgn]]
    if ( length(rg) == 0 ) {
      # Should never happen:
      # Won't get to running the group if there is not at least one that is incomplete
      writeLog(paste("All stages complete where StartFrom =",rgn),Level="warn")
      next
    } else if ( dryrun ) {
      # skip doing anything dryrun
      for ( ms in rg ) writeLog(paste("Would run stage",ms),Level="warn")
      next
    }

    if ( ! UseFuture ) {
      for ( ms in rg ) { # iterate over names of stages to run
        stg <- self$modelStages[[ms]]
        stg$run(log=LogLevel,UseFuture=FALSE)
        # inline execution will mark stage complete and reload the stage
        writeLog( stg$processStatus(), Level="warn")
        if ( stg$RunStatus != codeStatus("Run Complete") ) {
          stop (
            writeLog(c("Run Failed.",stg$RunStatus),Level="error")
          )
        }
      }
    } else {
      lastStatusReport <- NULL
      runningList <- list()
      for ( ms in rg ) { # iterate over names of stages to run
        # Wait for avaialble processors before attempting to schedule the next stage
        if ( length(runningList) >= future::nbrOfWorkers() ) {
          # Wait for a stage to finish so we can start another one
          writeLog("Waiting for free processor...",Level="warn")
          while ( all(sapply(runningList,function(stg) stg$running())) ) {
            if ( is.null(lastStatusReport) || tdiff(lastStatusReport,Sys.time(),units="secs") > statusDelay ) {
              writeLog(paste(length(runningList),"processes are running"),Level="info")
              sapply(runningList,function(stg) {
                writeLog( stg$processStatus(), Level="warn" )
              })
              lastStatusReport <- Sys.time()
            }
            Sys.sleep(delay)
          }

          # Post-process finished stages
          running <- sapply(runningList,function(stg) stg$running())
          done <- runningList[ which( ! running ) ]
          writeLog(paste("Stages done:",paste(sapply(done,function(stg)stg$Name),collapse=",")),Level="info")
          if ( length(done)>0 ) {
            # Remove stage from running list
            runningList <- runningList[ which(sapply(runningList,function(stg) stg$running())) ]
            sapply( done, function(stg) {
              # Log process completion status to console for stage no longer running
              # Status will be evaluated later be reloading the model stages
              stg$completed()
              writeLog( stg$processStatus(), Level="warn")
            } )
          } else {
            stop( writeLog("Program run error: done processes reported but not found",Level="error") )
          }
          writeLog(paste("Running list now has",length(runningList),"processes"),Level="info")
        }

        queueMsg <- paste("Queuing stage",ms)
        running <- length(runningList)
        if ( running > 0 ) queueMsg <- paste( queueMsg,paste0("(Joining ",running," already running)") )
        writeLog(queueMsg,Level="warn")
        runningList <- c(
          runningList,
          self$modelStages[[ms]]$run(log=LogLevel,UseFuture=TRUE)
        )
      }
      # Finalize last stage(s) in group (keep waiting while any are running)
      writeLog(paste("All run group stages are queued; monitoring processes..."),Level="info")
      watchingLogs <- FALSE
      while ( length(runningList)>0 ) {
        running <- sapply(runningList,function(stg) stg$running())
        done <- runningList[ which( ! running ) ]
        if ( length(done) > 0 ) {
          if ( watchingLogs ) {
            # sapply is overkill since watchingLogs implies done has length==1
            sapply( done, function(stg) {
              stg$watchLogfile(stop=TRUE) # grab end of log file if watching
            } )
            watchingLogs <- FALSE
          }
          # Mark the stage complete
          sapply( done, function(stg) {
            stg$completed()
            writeLog( stg$processStatus(), Level="warn" )
          } )
          # Remove done processes from runningList
          runningList <- runningList[ - which( ! running ) ]
        }
        if ( length(runningList) > 0 ) { # will be zero if we just finished the last stage
          if ( ! watchingLogs && length(runningList)==1 ) {
            writeLog("Watching log file for last stage process",Level="info")
            watchingLogs <- watch
          }
          if ( watchingLogs ) { # only one stage running - watch its logs
            runningList[[1]]$watchLogfile() # There's a delay built into watchLogFile
          } else {
            if ( is.null(lastStatusReport) || tdiff(lastStatusReport,Sys.time(),units="secs") > statusDelay ) {
              writeLog(paste(length(runningList),"processes are running"),Level="info")
              sapply(runningList,function(stg) {
                writeLog( stg$processStatus(), Level="warn" )
              })
              lastStatusReport <- Sys.time()
            }
            Sys.sleep(delay)
          }
        } else {
          writeLog(paste("Stages complete for StartFrom:",rgn),Level="warn")
        }
      }
    }
  }

  # Update overall model status if we did something that might have changed it
  if ( ! dryrun ) {
    self$load(onlyExisting=TRUE,updateCheck=FALSE) # revisit all the run results and update the statuses
  }
  return(self$results())
}

ve.stage.watchlog <- function(stop=FALSE,delay=2) {
  if ( stop ) delay <- 0
  if ( is.null(private$log.to.watch) ) {
    Logfile <- dir(self$RunPath,pattern="^Log_.*txt",full.names=TRUE)
    if ( length(Logfile)==0 ) {
      if ( delay > 0 ) Sys.sleep(delay)
      return() # probably called too soon after launching model
    } else if ( length(Logfile) > 1 ) {
      Logfile <- Logfile[length(Logfile)] # watch the last (most recent) one
    }
    if ( file.exists(Logfile) ) {
      writeLogMessage(paste0(self$Name,": Watching Log file"),Level="warn")
      writeLogMessage(Logfile,Level="info")
      private$log.to.watch <- file(Logfile,blocking=FALSE,open="r")
    }
  }
  if ( ! is.null(private$log.to.watch) ) {
    newlines <- readLines(private$log.to.watch)
    for ( line in newlines ) {
      writeLogMessage(paste0(self$Name,": ",line),Level="warn")
    }
    if ( delay > 0 ) Sys.sleep(delay)
  }
  if ( stop && ! is.null(private$log.to.watch) ) {
    close(private$log.to.watch)
    private$log.to.watch <- NULL
  }
}

################################################################################
#                              Model Configuration                             #
################################################################################

ve.model.scenarios <- function( reloadFile=FALSE  ) {
  if ( is.null(self$modelScenarios) || reloadFile ) {
    self$modelScenarios <- VEModelScenarios$new(baseModel=self,reloadFile=reloadFile)
  }
  if ( is.null(self$modelScenarios) ) {
    writeLog("Programming error: scenarios structure is broken [ve.model.scenarios]",Level="error")
  }
  return( self$modelScenarios )
}

# Function to add a stage to a loaded model. Accepts a stageParam_ls (as we would use for
#   ve.stage.init), which should contain, at a minimum, a "Name" (which must be unique in the model)
#   and a "StartFrom" designating another stage in the current model. The dots resolve to a named
#   list of parameters that are considered to be part of the StageConfig (e.g. InputPath for the
#   stage) - anything that might go into a stage's visioneval.cnf. The combination of stageParam_ls
#   and ... must add up to a runnable stage (errors identified in self$initstages)
ve.model.addstage <- function(Name=NULL,...) {
  # Add additional configuration parameters
  dotParam_ls <- list(...)
  stageParam_ls <- visioneval::addParameterSource(dotParam_ls,"addstage")

  # Merge the stage into the list of modelStages
  stage <- VEModelStage$new(
    Name = Name,
    Model = self,
    stageParam_ls=stageParam_ls
  )
  self$modelStages[[stage$Name]] <- stage
  self$modelStages <- self$initstages(self$modelStages) # existing stages will be skipped

  # Report runnability error...
  if ( ! stage$Name %in% names(self$modelStages) ) {
    writeLog("The stage you tried to add cannot run and will be ignored!",Level="error")
  }

  return(self)
}

# Return values of one or more settings
ve.model.setting <- function(setting=NULL,stage=NULL,defaults=TRUE,shorten=TRUE,source=FALSE) {
  searchParams_ls <- if ( defaults ) {
    visioneval::defaultVERunParameters()
  } else list()

  if ( is.character(stage) ) {
    stage <- stage[1]
    searchParams_ls <- visioneval::mergeParameters(
      searchParams_ls,
      if ( stage == "global" ) getSetup() else self$modelStages[[stage]]$RunParam_ls
    )
  } else {
    searchParams_ls <- visioneval::mergeParameters(searchParams_ls,self$RunParam_ls)
  }
  if ( source ) {
    # Return source
    if ( ! is.character(setting) || ! all(nzchar(setting)) ) setting <- names(searchParams_ls)
    sourceLocations <- sapply( searchParams_ls[setting], function(p) attr(p,"source") )
    values = sapply( searchParams_ls[setting], function(p) sprintf("%s",p) )
    if (shorten) sourceLocations <- sub(paste0(self$modelPath,"/"),"",sourceLocations,fixed=TRUE)
    return(data.frame(Setting=setting,Value=values,Source=sourceLocations))
  } else {
    # Return values for requested settings
    if ( ! is.character(setting) ) {
      return(names(searchParams_ls)) # Report what settings have names (may include defaults or not)
    } else {
      if ( length(setting)==1 && nzchar(setting) ) {
        settings <- searchParams_ls[[setting]] # single setting
      } else {
        if ( length(setting)>1 && all(nzchar(setting)) ) {
          settings <- searchParams_ls[setting]   # list of matching settings
        } else {
          settings <- searchParams_ls # Warning: list is potentially huge!
        }
      }
      if ( shorten ) {
        shorten <- sapply(settings,is.character)
        if ( any(shorten) ) {
          settings[shorten] <- sub(paste0(self$modelPath,"/"),"",settings[shorten],fixed=TRUE)
        }
      }
      return(settings)
    }
  }
}

# Report the model results path (used to creat a VEResults object and
# to retrieve the LogFile)
ve.model.findstages <- function(stage=character(0),Reportable=TRUE) {
  if ( ! private$p.valid ) return( list() )
  stages <- self$modelStages
  if ( ! missing(stage) && length(stage)>0 ) {
    if ( is.integer(stage) ) {
      stages <- stages[ stage ]         # list slice by index
    } else if ( is.character(stage) ) {   # list slice by stage name
      stages <- stages[ names(stages) %in% stage ]
    }
  }
  if ( Reportable && length(stages)>0 ) {
    # NOTE: need length check since sapply returns list() if stages
    # is empty yielding a bad index into stages[]
    stages <- stages[ sapply(stages,function(s) s$Reportable) ] # Only return reportable stages
  }
  return(stages)
}

################################################################################
#                                Model Results                                 #
################################################################################

# create a VEResultsList object (possibly invalid/empty) from the model's Reportable stages. If
# stages are named explicitly, the results for the named stages will be included even if they are
# "StartFrom" stages or are otherwise not reportable.
# This function will return even if the model has not been run, or has incomplete stages: the
# stages with missing results will show up as "Not Run Yet" and will be ignored for export, etc.
ve.model.results <- function(stage=character(0)) {

  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( NULL )
  }

  stages <- self$findstages(stage) # empty vector (default) gets all Reportable stages
  if ( length(stages)==0 ) {
    writeLog(paste("Available stages:",names(self$modelStages),collapse=", "),Level="error")
    stop(
      writeLog(paste("Model stage(s) not found:",stage,collapse=","),Level="error")
    )
  }
  return(VEResultsList$new(stages=stages,model=self))
}

# open a Query object for the model from its QueryDir (or report a list
#  of available queries if no QueryName is provided).
ve.model.query <- function(QueryName=NULL,FileName=NULL,load=TRUE) {
  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( NULL )
  }
  # Get the Query directory for the model
  QueryDir <- visioneval::getRunParameter("QueryDir",Param_ls=self$RunParam_ls)
  if ( all(is.null(c(QueryName,FileName))) ) {
    QueryPath <- file.path(self$modelPath,QueryDir)
    if ( ! dir.exists(QueryPath) ) QueryPath <- self$modelPath;
    queries <- dir(QueryPath,pattern="\\.(VEqry|R)$",ignore.case=TRUE)
    if ( length(queries)==0 ) queries <- "No queries defined"
    return(structure(queries,QueryPath=QueryPath))
  }
  # Let VEquery find the query...
  return(
    VEQuery$new(
      QueryName=QueryName,
      Model=self,                 # Attach this model
      ModelPath=self$modelPath,
      QueryDir=QueryDir,
      FileName=FileName,
      load=load
    )
  )
}

ve.model.exportpath <- function() {
  output.dir <- file.path(self$modelPath,self$setting("ResultsDir"),self$setting("OutputDir"))
}

ve.model.exporter <- function(file=NULL,tag=NULL,connection=NULL,partition=NULL) {
  # If all arguments missing, print a list of available exports
  output.dir <- self$exportPath()
  if ( ! is.character(file) && is.null(tag) ) {
    # Show list of available exports
    return( dir(output.dir,pattern="\\.VEexport$") )
  }
  # If file provided, process that
  if ( is.character(file) ) {
    filepath <- file.path(output.dir,file)[1]
    if ( ! any(grepl("\\.VEexport$",filepath)) ) filepath <- paste0(filepath,".VEexport")
    if ( file.exists(filepath) ) {
      return( VEExporter$new(Model=self,load=filepath) ) # Will report error if can't load
    } else {
      stop("Exporter file does not exist:",filepath)
    }
  }
  if ( ! is.null(tag) || is.character(connection) ) {
    return( VEExporter$new(Model=self,tag=tag,connection=connection,partition=partition) )
  }
  stop("Must provide file, tag, or connection to identify Exporter")
}
  
################################################################################
#                          Exported Helper Functions                           #
################################################################################

#' Open a VisionEval Model
#'
#' @description
#' /code{openModel} opens a VisionEval model and returns a VEModel object (q.v.) through
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
#' If a model with existing results is opened, a check is made to ensure that the model
#' configuration has not been changed. The 'reset' parameter says how to handle existing results.
#' If reset=='continue', then only run stages that are up to date with the model configuration and
#' not "Run Complete". If any stages are out of date, generate an error message and do not open the
#' model.
#' If reset=='save', the full set of model results are archived to a new timestamped results
#' archive folder. Then any stage that is out of date with its model configuration or that is
#' not 'Run Complete' will be reset (results erased).
#' If reset=="reset", any stage that is out of date with its model configuration or that is
#' not 'Run Complete' will be reset (results erased). No previous results will be saved.
#'
#' Note that reset behavior is different from what happens when reset is specified as a model
#' runs. When opening a model, stages will only be reset if they did not previously complete
#' successfully, or if the stage results are out of date compared to the model configuration for
#' that stage (i.e. something was edited to make the stage results inconsistent with the model
#' specification).
#'
#' @param modelPath Directory containing a VisionEval model; if an empty character string is
#'     provided, prints a list of available models (see details)
#' @param updateCheck if TRUE, perform check on existing results to detect changed model
#'     parameters; if FALSE, skip the check. If the specific string "reset" will reset the
#'     LastChanged date for the results to force them to be up to date.
#' @param log a character string identifying the log level to be displayed
#' @return A VEModel object or a VEModelList of available models if no modelPath or modelName is
#'     provided; see details and `vignette("VEModel")`
#' @export
openModel <- function(modelPath="",updateCheck=TRUE,log="error") {
  if ( missing(modelPath) || !nzchar(modelPath) ) {
    modelRoot <- getModelDirectory() # returns absolute path
    return(
      if ( ! dir.exists(modelRoot) ) { # subdirectory of ve.runtime
        paste("No",basename(modelRoot),"directory in current runtime")
      } else {
        dir(modelRoot) # returns list of models in ve.runtime/modelRoot
      }
    )
  } else {
    if ( !is.null(log) ) initLog(Save=FALSE,Threshold=log, envir=new.env())
    return( VEModel$new(modelPath = modelPath,updateCheck=updateCheck) )
  }
}

##########################################################################################
#                      MANAGE "STANDARD MODELS" (VEModel Examples)                       #
##########################################################################################

#' Look up a standard model in the index of available models
#' @param model bare name of standard model (if not provided, list available models)
#' @param variant name of variant with the model (use "" to get list of available variants)
#' @param private if TRUE and showing an index, include private models
#' @return the full path to that model template
#' @export
findStandardModel <- function( model, variant="", private=FALSE ) {

  # COVID-19 Joke
  if ( toupper(variant) %in% c("DELTA","OMICRON") ) return( "Cough, Cough!" )

  if ( missing(model) || is.null(model) || ! nzchar(model)) {
    return( unique(showModelIndex(private=private)[,c("Model","Package")]) )
  }    

  # Locate the model
  modelIndex <- getModelIndex()
  model <- model[1]
  if ( ! model %in% names(modelIndex) ) {
    writeLog(paste("No standard model called ",model),Level="error")
    return( findStandardModel("") ) # recursive call to keep return directory in one place
  }

  # Read the model index to identify variants ("base" should always exist)
  if ( missing(variant) || ! nzchar(variant) || ( ! variant %in% names(modelIndex[[model]]) ) ) {
    if ( nzchar(variant) ) { # not in list of variants
      msg <- writeLog(paste0("Unknown variant '",variant,"' in model '",model,"'"),Level="error")
    }
    index_df <- showModelIndex(private=private)
    return(index_df[index_df$Model==model,])
  }

  # Load the variant configuration
  model_ls <- list()
  model_ls$Name <- model
  model_ls$Variant <- variant
  variantConfig <- modelIndex[[model]][[variant]]

  # Get config file and description
  model_ls$ModelDir <- variantConfig$ModelDir
  model_ls$Description <- variantConfig$description
  configNames <- names(variantConfig)
  if ( "config" %in% configNames ) {
    model_ls$Config <- normalizePath(file.path(model_ls$ModelDir,variantConfig$config))
  } # if no Config, move contents of scripts directory to installPath (hack for classic model)
  configNames <- configNames[ ! configNames %in% c("config","description","ModelDir","stages") ]

  # Get regular files and directories to copy
  # Anything else listed as a key will become a sub-directory
  # modelTo <- c("scripts","inputs","defs","queries","scenarios")
  modelTo <- configNames[ ! configNames %in% c("description","config","stages") ]
  modelFrom <- unlist( variantConfig[modelTo] )

  # Get model stage directories, if any
  modelStages <- unlist(variantConfig$stages)
  if ( is.null(modelStages) ) modelStages <- character(0)
  modelTo <- c(modelTo, modelStages)
  modelFrom <- c(modelFrom, modelStages)

  modelSrc <- normalizePath(
    file.path(model_ls$ModelDir,modelFrom),
    winslash="/",
    mustWork=FALSE
  )
  names(modelSrc) <- modelTo
  modelDirs <- list()
  for ( d in modelTo ) {
    modelDirs[[d]] <- list()
    modelDirs[[d]]$From <- modelSrc[d]
    modelDirs[[d]]$To <- d
  }
  model_ls$Directories <- modelDirs

  writeLog("findStandardModel: Source path:",Level="info")
  writeLog(modelFrom,Level="info")

  return(model_ls) # model structure for installation
}

## install a standard model with data identified by "variant"
# This is the principal way to get hold of a model for learning or testing, though a model
# directory can simply be copied into the runtime "models" directory to make it available.

# modelName is model set to search for ("VERSPM", "VE-State", etc)
# variant is model variant (default is "base", which must be defined for any new model). If variant
#   is "", will list available model variants
# modelPath is path + name into which to install the model (rather than default modelName-variant)
# confirm if FALSE will just attempt the installation without user input
# overwrite if TRUE will destroy any existing model on modelPath and reinstall
# private if TRUE will also list model variants marked "private" (typically test models that
#   contain a limited set of modules, or input data that is broken in some specific way; see
#   VEModel/inst/models for examples)
installStandardModel <- function( modelName, variant="base", modelPath=NULL, confirm=TRUE, overwrite=FALSE, private=FALSE) {
  # Locate and install standard modelName into modelPath
  #   modelName says which standard model to install. If it is missing or empty, return a
  #     list of available models
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first
  #     available root (see VEModel::getModelRoots function)
  #   Otherwise, modelPath is presumed to be the directory into which modelName will be installed
  #     (and basename(modelPath) will become the target model name, disambiguated).
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and
  #     they have to try again.

  model <- findStandardModel( modelName, variant, private=private )
  if ( is.data.frame(model) ) {
    return(model) # Data.frame is a subset of the model index
  } # Otherwise model is a list with details on the model we need to install

  if ( ! "Description" %in% names(model) ) model$Description <- paste(modelName,variant,sep="-")

  # Set up destination modelPath (always create in the first defined root)
  root <- getModelRoots(1)
  writeLog(paste("Root:",root),Level="info")
  if ( missing(modelPath) || is.null(modelPath) ) modelPath <- paste(model$Name,variant,sep="-")
  if ( ! isAbsolutePath(modelPath) ) {
    newModelPath <- file.path(root,modelPath)
    writeLog(paste("Model path:",newModelPath),Level="info")
    installPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  } else installPath <- modelPath
  if ( dir.exists(installPath) ) {
    if ( ! overwrite ) {
      installPath <- getUniqueName( dirname(installPath), basename(installPath) )
    } else {
      unlink(installPath,recursive=TRUE)
    }
  }

  # Confirm installation if requested
  install <- TRUE
  if ( confirm && interactive() ) {
    msg <- paste0(
      "Install '",basename(installPath),"' into ",
      sub( getRuntimeDirectory(),"",installPath ), # shorten runtime directory
      "?"
    )
    install <- confirmDialog(msg)
  }

  if ( ! install ) stop("Model ",modelName," not installed.",call.=FALSE)

  # Create the installation directory
  writeLog(paste("Installing from",model$ModelDir),Level="info")
  writeLog(model$Description,Level="info")
  writeLog(paste("Installing into",installPath),Level="info")
  dir.create(installPath)

  # Process the model directories
  for ( subdir in model$Directories ) {
    from.dir <- subdir$From
    to.dir <- file.path(installPath,subdir$To)
    if ( ! dir.exists(from.dir) ) {
      writeLog(paste0("Searching ",from.dir),Level="info")
      writeLog(msg<-paste0("Could not load variant (",variant,") for ",modelName),Level="error")
      writeLog(c("Directory missing:",from.dir),Level="error")
      stop(msg)
    }
    if ( ! dir.exists(to.dir) ) dir.create(to.dir)
    writeLog(paste0("Copying ",subdir$From,"..."),Level="info")
    copy.files <- dir(from.dir,full.names=TRUE)
    file.copy(copy.files,to.dir,recursive=TRUE) # Copy standard model into modelPath
  }

  # Copy the configuration file
  if ( "Config" %in% names(model) ) {
    writeLog(paste0("Copying ",model$Config," into visioneval.cnf"),Level="info")
    file.copy(model$Config,file.path(installPath,"visioneval.cnf"))
  } else {
    # Hack for classic model - no Config
    # Elevate content "scripts" directory to installPath then remove scriptPath
    if ( dir.exists( scriptPath<-file.path(installPath,"scripts") ) ) {
      writeLog("No config: promoting 'scripts' directory",Level="info")
      file.copy(dir(scriptPath,full.names=TRUE),installPath)
      unlink(scriptPath,recursive=TRUE)
    } else {
      stop( writeLog(paste0("No configuration file available for ",modelName),Level="error") )
    }
  }

  writeLog(paste0("Installed ",modelName," in ",installPath),Level="info")
  return( list(modelName=modelName,modelPath=installPath) )
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
#'   getwd()/models. If directory does not exist, create it and copy the modelName into it.
#'   If directory does exist, create a unique variant of modelName adjacent to it. If it is NULL
#'   create a unique variant of modelName in getwd()/models.
#' @param variant the name of a model variant (staging, sample data, etc) to install; empty string
#'   will list available variants for modelNameb
#' @param confirm if TRUE (default) and running interactively, prompt user to confirm, otherwise
#'   just do it.
#' @param overwrite if TRUE (default = FALSE) then overwrite (recreate) any modelName that already
#'   exists - otherwise modelName is disambiguated as "modelName(1)", "modelName(2)" etc.
#' @param private if TRUE and listing models, include standard model templates marked "private"
#'   (intended for package test models that include subsets of run scripts or deliberately "damaged"
#    input files)
#' @param log a string describing the minimum level to display
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, variant="base", modelPath=NULL, confirm=TRUE, overwrite=FALSE, private=FALSE, log="warn") {
  # Load system model configuration (clear the log status)
  initLog(Save=FALSE,Threshold=log, envir=new.env())
  model <- installStandardModel(modelName, modelPath, confirm=confirm, overwrite=overwrite, private=private, variant=variant)
  if ( ! is.data.frame(model) ) { # returns a list if we found a model
    return( VEModel$new( modelPath=model$modelPath ) )
  } else {
    return( model ) # should be a data.frame of information about standard models
  }
}

################################################################################
#                           VEModel Class Definition                           #
################################################################################

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"
#' @export
VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,                         # Model identifier
    modelPath=NULL,                         # Also as RunParam_ls$ModelDir
    modelStages=NULL,                       # list of VEModelStage objects
    modelScenarios=NULL,                    # VEModelScenarios object, if scenarios configured
    modelResults=NULL,                      # Absolute path == modelPath/ResultsDir
    RunParam_ls=NULL,                       # Run parameters for the model (some constructed by $configure)
    loadedParam_ls=NULL,                    # Loaded parameters (if any) present in model configuration file
    specSummary=NULL,                       # List of inputs, gets and sets from master module spec list  
    overallStatus=codeStatus("Uninitialized"), # Overall model status (worst case from stages)
    FuturePlan=NULL,                        # character string describing multiprocessing plan (see ve.model.plan)
    Workers=1,                              # workers for multiprocessing

    # Methods
    initialize=ve.model.init,               # initialize a VEModel object
    configure=ve.model.configure,           # load or re-load VEModel from its disk location
    initstages=ve.model.initstages,         # Complete stage setup
    addstage=ve.model.addstage,             # Add a stage programmatically
    valid=function() private$p.valid,       # report valid state
    load=ve.model.load,                     # load the model state for each stage (slow - defer until needed)
    fixRunStatus=ve.model.fixRunStatus,     # set each complete stage RunStatus to respect StartFrom
    plan=ve.model.plan,                     # Choose a multiprocessing plan
    run=ve.model.run,                       # run a model (or just a subset of stages)
    print=ve.model.print,                   # provides generic print functionality
    stageStatus=ve.model.stageStatus,       # report status of stages
    printStatus=ve.model.printStatus,       # turn integer status into text representation
    updateStatus=ve.model.updateStatus,     # fill in overall status based on individual stage status
    scenarios=ve.model.scenarios,           # Create a VEModelScenarios object to manage scenarios
    list=ve.model.list,                     # interrogator function (script,inputs,outputs,parameters
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    log=ve.model.log,                       # report the log file path (use e.g. file.show to display it)
    setting=ve.model.setting,               # report the values of parameter settings (for environment, model or stage)
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    archive=ve.model.archive,               # apply framework archive function if results exist
    results=ve.model.results,               # Create a VEResults object (if model is run); option to open a past result
    exportPath=ve.model.exportpath,         # Where to save VEExporters associated with this Model
    exporter=ve.model.exporter,             # Create an exporter for this model's results
    findstages=ve.model.findstages,         # Report the path to the model results for a stage
    query=ve.model.query                    # Create a VEQuery object (or show a list of queries).
  ),
  active = list(                            # Object interface to "set" function; "set" called explicitly has additional options
    settings=function(Param_ls) {
      if ( missing(Param_ls) ) return(self$set()) else return(self$set(Param_ls=Param_ls))
    }),
  private = list(
    # Private Members
    p.valid=FALSE
  )
)

################################################################################
#                         VEModelStage Class Definition                        #
################################################################################

#' @export
VEModelStage <- R6::R6Class(
  "VEModelStage",
  public = list(
    # Public data
    Name = NULL,                    # Name of stage (used e.g. in another stage's StartFrom)
    Model = NULL,                   # VEModel to which this stage belongs
    Dir = NULL,                     # basename of stage subdirectory (for inputs and results)
    Path = NULL,                    # Normalized path to folder holding InputDir and ParamDir
    Config = NULL,                  # File relative to ModelDir (optional); 
    Reportable = NULL,              # If TRUE, include in default result set for extract and queries
    StartFrom = "",                 # Name of another model stage to extend
    RunParam_ls = list(),           # RunParameters to initialize the stage
    loadedParam_ls = NULL,          # Loaded parameters (if any) present in stage configuration file
    ModelState_ls = NULL,           # ModelState constructed from RunParam_ls
    RunPath = NULL,                 # Typically ModelDir/ResultsDir/StageDir
    FutureRun = NULL,               # Future object for running a stage (NULL if run is not happening)
    RunStatus = 1,                  # "Unknown"   # Indicates whether results are available
    RunStarted = NULL,              # Sys.time() when model stage run started
    RunCompleted = NULL,            # Sys.time() when model stage run completed
    Results = NULL,                 # A VEResults object (create on demand)
    ScenarioElements = NULL,        # Scenario descriptor for visualizer
    IsScenario = FALSE,             # Flag TRUE if this stage is a scenario

    # Methods
    initialize=ve.stage.init,       # Create a stage and set its Runnable flag
    runnable=ve.stage.runnable,     # Does the stage have all it needs to run?
    print=ve.stage.print,           # Print stage summary
    load=ve.stage.load,             # Read existing ModelState.Rda if any
    run=ve.stage.run,               # Run the stage (build results)
    completed=ve.stage.completed,   # Gather results of multiprocessing
    running=ve.stage.running,       # Check if stage is still running
    scenariolevel=ve.stage.levels,  # Given a scenario name, return the level that this stage used to alter it (0 if base)
    processStatus=ve.stage.pstatus, # Return string describing stage run process status
    watchLogfile=ve.stage.watchlog  # Helper to watch a logfile as this stage runs in the background
  ),
  private=list(
    log.to.watch = NULL
  )
)
