# Author: Jeremy Raw

# VEModel Package Code

#' @include environment.R
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
#' standard models The VEModel manager makes that very easy! See \code{vignette('VEModel')} for full
#' instructions. A simple introduction is found on the VisionEval wiki, in the Getting-Started
#' document (that document is also included in runtime installations of VisionEval).
#'
#' Here are the details of the VEModel manager.
#'
#' @section Usage:
#' \preformatted{mdl <- VEModel$new(modelName,log="error")
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
#'   like ve.runtime/models. Usually it will just the name of model directory within
#'   ve.runtime/models.}
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
#'   \item{shorten}{Remove the ve.runtime directory prefix from the displayed directory and file names. Default is
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
#'   \item{newPath}{The directory into which to put the copy of this model. Default if NULL is ve.runtime/models
#'   directory.}
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
#' proceed from the first stage or RunStep that is not marked as "complete".
#'
#' @importFrom R6 R6Class
#' @name VEModel
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
#  referring to getwd(), ve.runtime, and <ModelRoot>
getModelRoots <- function(get.root=0,Param_ls=NULL) {
  roots <- c( getwd() )
  if ( exists("ve.runtime") ) {
    ve.runtime <- get("ve.runtime")
    if ( ve.runtime != getwd() ) roots <- c( ve.runtime, roots )
  }
  # Hierarchy of roots:
  #    ve.runtime/<ModelRoot> (if exists)
  #    getwd()/<ModelRoot> (if exists)
  #    ve.runtime
  #    getwd()
  modelRoot <- file.path(roots,visioneval::getRunParameter("ModelRoot",Param_ls=Param_ls))
  if ( length(modelRoot)>0 ) {
    if ( isAbsolutePath(modelRoot[1]) ) {
      modelRoot <- modelRoot[1]
    } else { # happens if ve.runtime is defined but not an absolute pqth (unlikely)
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
cullInputPath <- function(InputPath,InputDir) {
  # Remove any element of InputPath that is "" or "."
  InputPath <- InputPath[ nzchar(InputPath) & InputPath != "." ]

  # Normalize remaining InputPath elements, if any, and remove duplicates
  InputPath <- unique(normalizePath(InputPath,winslash="/",mustWork=FALSE))

  # Remove any element of InputPath that is not an existing directory
  InputDir <- InputDir[1] # backstop in case InputDir accidentally set to a vector
  InputPath <- InputPath[dir.exists( file.path(InputPath,InputDir) )]

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

# configure installs the model parameters (initializing or re-initializing)
ve.model.configure <- function(modelPath=NULL, fromFile=TRUE) {

  if ( missing(modelPath) || ! is.character(modelPath) ) {
    modelPath <- self$modelPath
  }
  
  self$modelPath <- modelPath;

  # Load any configuration available in modelPath (on top of ve.runtime base configuration)
  Param_ls <- getSetup() # runtime configuration
  if ( fromFile || is.null(self$loadedParam_ls) ) {
    self$loadedParam_ls <- visioneval::loadConfiguration(ParamDir=modelPath)
  } # if NOT fromFile, use existing loadedParam_ls to rebuild (may have in-memory changes)

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
      visioneval::getRunParameter("ResultsDir",Param_ls=Param_ls,Source=TRUE)
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
      modelParam_ls[["LoadDatastoreName"]] <- file.path (
        runPath,
        baseModel$setting("DatastoreName",stage=loadStage)
      )
      modelParam_ls$LoadDatastore <- TRUE
    } else {
      writeLog(paste("LoadModel present but invalid:",modelParam_ls$LoadModel),Level="warn")
      modelParam_ls$LoadDatastore <- FALSE
    }
  }

  # Process InputPath for overall model (culling directories for those actually exist)
  if ( ! "InputPath" %in% names(modelParam_ls) ) {
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      Source="VEModel::findModel",
      InputPath=modelParam_ls$ModelDir
    )
  } else {
    # expand default InputPath if necessary
    if ( modelParam_ls$InputPath == "." ) modelParam_ls$InputPath <- modelParam_ls$ModelDir
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      Source="VEModel::findModel",
      InputPath=c( modelParam_ls$ModelDir, modelParam_ls$InputPath )
    )
  }
  # Cull input paths (directory must exist and contain InputDir (default: "inputs")
  modelParam_ls <- visioneval::addRunParameter(
    Param_ls=modelParam_ls,
    Source="VEModel::findModel",
    InputPath=cullInputPath(
      InputPath=modelParam_ls$InputPath,
      InputDir=visioneval::getRunParameter("InputDir",Param_ls=modelParam_ls)
    )
  )
  writeLog("Input Paths:",Level="info")
  for ( p in modelParam_ls$InputPath ) {
    writeLog(paste("Input Path:",p),Level="info")
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

  # Locate model stages

  if ( fromFile || is.null(self$modelStages) ) {
    writeLog("Locating model stages",Level="info")
    if ( ! "ModelStages" %in% names(self$RunParam_ls) ) {
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
        self$setting("ResultsDir")
      )
      stages <- stages[ ! stages %in% structuralDirs ]
      stages <- stages[ grep(paste0("^",self$setting("ArchiveResultsName")),stages,invert=TRUE) ]
      stages <- c(".",stages) # Add model root directory
      writeLog(paste0("Stage directories:\n",paste(stages,collapse=",")),Level="info")
      modelStages <- lapply(stages,
        function(stage) {
          stageParam_ls <- list(
            Dir=stage,                           # Relative to modelPath
            Name=sub("^\\.$",basename(self$modelPath),stage),  # Will only change root directory
            Path=self$modelPath          # Root for stage
          )
          VEModelStage$new(
            Name = stageParam_ls$Name,
            Model = self,
            stageParam_ls=stageParam_ls          # Base parameters from model
          )
        }
      )
    } else {
      writeLog("Parsing ModelStages setting from model",Level="info")
      modelStages <- lapply(names(self$RunParam_ls$ModelStages), # Use pre-defined structures
        # At a minimum, must provide Dir or Config
        function(stage) {
          obj <- self$RunParam_ls$ModelStages[[stage]] # Get the stageParam_ls structure
          writeLog(paste("Model Stage:",stage),Level="info")
          VEModelStage$new(
            Name=stage,
            Model=self,
            stageParam_ls=obj
          )
        }
      )
    }
    # If no stages remain, model is invalid
    if ( !is.list(modelStages) || length(modelStages)==0 ) {
      writeLog("No model stages found!",Level="error")
      return(self)
    }
  } else {
    # all stage edits in memory should be made to stage$RunParam_ls, not stage$loadedParam_ls
    # stages changes mediated through their files should be reloaded with fromFile=TRUE
    writeLog("Existing Model Stages",Level="info")
    modelStages <- self$modelStages
  }

  # Call the modelStages by their Names
  names(modelStages) <- stageNames <- sapply(modelStages,function(s)s$Name)
  if ( length(stageNames) > 0 ) {
    writeLog(paste("Model Stages:",stageNames,collapse=","),Level="info")
  }

  # Save the model RunParam_ls and link the stages
  writeLog("Initializing Model Stages",Level="info")
  self$modelStages <- self$initstages( modelStages )

  # Update the model status
  self$specSummary <- NULL # regenerate when ve.model.list is next called
  self$updateStatus()

  return(self)
}

ve.model.initstages <- function( modelStages ) {
  # Loop through modelStages, completing initialization using "StartFrom" stages
  writeLog("Finding runnable stages",Level="info")
  runnableStages <- list()
  for ( stage_seq in seq_along(modelStages) ) {
    stage <- modelStages[[stage_seq]]      # stage is a VEModelStage object
    if ( stage_seq > 1 && isTRUE(stage$RunParam_ls$LoadDatastore) ) {
      # Only the first stage performs LoadDatastore
      # Consider amending to allow any stage that does not "StartFrom"
      stage$RunParam_ls$LoadDatastore <- FALSE
      stage$RunParam_ls[["LoadDatastoreName"]] <- NULL
    }
    writeLog(paste("Evaluating ",stage$Name),Level="info")
    if ( stage$runnable(runnableStages) ) {   # complete stage initialization
      runnableStages <- c( runnableStages, stage )
      names(runnableStages) <- sapply(runnableStages,function(s)s$Name)
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
    # StageDir will still be used for InputPath
    if ( is.null(modelStages[[1]]$Reportable) ) {
      # do not set if already explicitly set during ve.stage.init
      modelStages[[1]]$Reportable <- TRUE
    } else {
      writeLog(paste("Single stage",modelStages[[1]]$Name,"Reportable:",modelStages[[1]]$Reportable),Level="info")
    }
  } else {
    # Put names on Stages and identify reportable stages
    startFromNames <- unlist(sapply(modelStages,function(s) s$StartFrom))
    startFromNames <- startFromNames[ nzchar(startFromNames) ]
    stageNames <- names(modelStages)
    reportable <- ! stageNames %in% startFromNames
    for ( r in seq_along(stageNames) ) {
      if ( is.null(modelStages[[r]]$Reportable) ) {
        # Can override Reportable in stage configuration (e.g. for base year)
        modelStages[[r]]$Reportable <- reportable[r]
      }
    }
  }
  return( modelStages )
}

# Initialize a VEModel from modelPath
# modelPath may be a full path, and will be expanded into known model directories
#  if it is a relative path.
ve.model.init <- function(modelPath) {

  # Opportunity to override names of ModelState, run_model.R, Datastore, etc.
  # Also to establish standard model directory structure (inputs, results)

  # Identify the run_model.R root location(s)
  # Also, update self$RunParam_ls with model-specific configuration
  writeLog(paste("Finding",modelPath),Level="info")
  modelPath <- findModel(modelPath) # expand to disk location
  if ( nzchar(modelPath) ) {
    self$configure(modelPath)
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
#   Change the ModelDir in the model's RunParams_ls
#   Update RunPath for each stage in its RunParams_ls and ModelState_ls if copyResults
#   Identify other elements of ModelState and RunParams that will change if the
#     the ModelDir changes (everything built from ModelDir)
ve.model.copy <- function(newName=NULL,newPath=NULL,copyResults=TRUE,copyArchives=FALSE) {
  # Copy the current model to NewName (in ModelDir, unless newPath is also provided)
  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( NULL )
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
  model.files <- self$dir(root=TRUE,inputs=TRUE,results=copyResults,archive=copyArchives)
  copy.subdir <- dirname(model.files)
  unique.dirs <- unique(copy.subdir)
  for ( d in unique.dirs ) {
    copy.from <- file.path(self$modelPath,model.files[copy.subdir==d])
    copy.to <- newModelPath
    if ( d == "." ) { # modelPath
      file.copy( copy.from, newModelPath, recursive=TRUE )
    } else {
      copy.to <- file.path(copy.to,d)
      if ( ! dir.exists(copy.to) ) dir.create(copy.to,recursive=TRUE)
      file.copy( copy.from, copy.to ) # non-recursive in stages
    }
  }
  return( openModel(newModelPath) )
}

# Archive results directory
ve.model.archive <- function(SaveDatastore=TRUE) {
  failToArchive <- visioneval::archiveResults(
    RunParam_ls=self$RunParam_ls,
    RunDir=file.path(self$modelPath,self$setting("ResultsDir")),
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
# "shorten" is a logical parameter - if true, strip off the "ve.runtime" part of any paths, so what
#   is shown is relative to ve.runtime.
# Parameters control elements of the display (if all are FALSE, make them all TRUE):
#   root==TRUE   : show "root" elements (config, scripts/run_model.R) plus InputPath and ParamPath
#   archive=TRUE : show archived results directories (these are also found and removed from "root")
#   inputs=TRUE  : show files in "inputs" and "defs" locations for the model
#   results=TRUE : show result sets
#   outputs=TRUE : show "outputs" (extracts and query results)
# if all.files, list inputs and outputs as files, otherwise just directories
ve.model.dir <- function(
  pattern=NULL,stage=NULL,shorten=TRUE, all.files=FALSE,
  root=FALSE,results=FALSE,outputs=FALSE,inputs=FALSE,archive=FALSE) {
  # We're going to report contents of these directories
  #   self$modelPath (root)
  #   self$modelPath/ResultsDir (results)
  #   self$modelStages[[stage]]$RunPath (results)
  #   self$modelPath/InputDir (inputs) # if exists
  #   self$modelStages[[stage]]$InputPath/InputDir (inputs) # if exists
  #     Do the inputs by assembling the entire InputPath and reducing to unique directories
  #   self$modelPath/ParamDir (inputs)
  #   self$modelStages[[stage]]$ParamPath (inputs)
  #     Like the inputs, reduce to a unique set of directories
  #   Outputs are sought within ResultsDir/OutputDir for the model
  #     Subdirectories will be the stage/scenario, files will be specific extracts
  # If none of root/results/outputs/inputs is TRUE, then all are TRUE
  #   Otherwise, only report the ones actually asked for
  validDir <- dir.exists(self$modelPath)
  if ( ! private$p.valid || ! validDir ) {
    return("No model found.")
    private$p.valid <- FALSE
  }

  # 
  if ( all(missing(root),missing(results),missing(outputs),missing(inputs),missing(archive)) ) {
    root <- results <- outputs <- inputs <- archive <- TRUE
  }

  if ( missing(shorten) || shorten ) shorten <- self$modelPath
  if ( is.null(stage) ) {
    stages <- self$modelStages
  } else {
    stages <- self$modelStates[stage] # list of named stages
  }
  if ( length(stages)==0 ) {
    writeLog(paste("Invalid stages selected for $dir:",stage,collapse=","),Level="warn")
    inputs <- results <- outputs <- FALSE
  }

  inputPath <- character(0)
  for ( stg in stages ) {
    # Stage input paths may all be the same at the model level, or individual, or both
    inputPath <- c(inputPath,
      file.path(
        self$setting("InputPath",stage=stg$Name,shorten=FALSE),
        self$setting("InputDir",stage=stg$Name)
      )
    )
  }
  inputPath <- unique(inputPath) # use this to avoid copying it with root files
  if ( inputs ) {
    inputFiles <- dir(normalizePath(inputPath),full.names=TRUE)
    if ( all.files ) {
      inputFiles <- inputFiles[ ! dir.exists(inputFiles) ] # keep only the files, not subdirectories
    } else {
      # no details: keep directories, not files
      # Show the input directory names only if they exist
      inputFiles <- normalizePath(c(inputPath,inputFiles),winslash="/",mustWork=FALSE)
      inputFiles <- inputFiles[ dir.exists(inputFiles) ]
    }
  } else inputFiles <- character(0)

  # Locate results
  baseResults <- normalizePath(
    file.path(
      self$modelPath,
      self$setting("ResultsDir")
    )
  )

  # Do the outputs before the results (makes it easier to handle
  #  results in root)
  # TODO: verify where the "outputs" are. OutputDir needs to be relative to ModelDir/ResultsDir...
  # "OutputDir" is used in VEModel$extract and VEModel$query...
  if ( outputs ) {
    outputPath <- file.path( baseResults,self$setting("OutputDir") )
    outputFiles <- dir(normalizePath(outputPath),full.names=TRUE,recursive=all.files)
    if ( all.files ) {
      outputFiles <- outputFiles[ ! dir.exists(outputFiles) ]
    }
  } else outputFiles <- character(0)

  # Find RunPath for each stage
  stagePaths <- sapply(stages,function(s) s$RunPath)
  stagePaths <- stagePaths[ !is.na(stagePaths) ]

  # Find archiveDirs (and if asked for, archiveFiles)
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
    resultFiles <- c(mstates,dstores,logs)
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

    stageDirs <- file.path(rootPath,sapply(stages,function(s) s$Dir))
    if ( length(stageDirs)>1 || normalizePath(stageDirs) != rootPath ) {
      rootFiles <- setdiff(rootFiles, stageDirs)    # Leave out Stage sub-directories; TODO: is that correct?
      # TODO: probably want to identify the stage directories and show their contents...
    }

    if ( all.files ) {
      paramPaths <- self$setting("ParamPath",shorten=FALSE)
      for ( st in stages ) paramPaths <- c(paramPaths,self$setting("ParamPath",stage=st$Name,shorten=FALSE))
      paramPaths <- unique(paramPaths)
      rootFiles <- c( rootFiles, dir(paramPaths,full.names=TRUE) )
      rootFiles <- c( rootFiles, self$setting("ModelScriptPath",shorten=FALSE) )
      queryPath <- file.path(rootPath,self$setting("QueryDir")) # QueryDir is always "shortened"
      rootFiles <- c( rootFiles, dir(queryPath,full.names=TRUE) )
    }
    # TODO: give better results for model stages...
  } else rootFiles <- character(0)

  files <- sort(unique(c(
    # in case nothing was asked for: list(list()[x]) would return NULL, not character(0)
    # So force the type for files by providing an empty string to add to NULL/nothing
    character(0), 
    unlist(
      list(inputFiles,outputFiles,resultFiles,rootFiles,archiveFiles)[c(inputs,outputs,results,root,archive)]
    )
  )))
  if ( nzchar(shorten) ) files <- sub(paste0(shorten,"/"),"",files,fixed=TRUE)
  return(files)
}

# Function to interactively remove prior model runs or extracts
ve.model.clear <- function(force=FALSE,outputOnly=NULL,archives=FALSE,stage=NULL,show=10) {
  # Remove outputs and/or results, either interactively or in batch
  # 'show' controls maximum number of outputs to display
  # Can limit to outputs or results in a certain 'stage'
  # outputOnly will show results as well as outputs for deletion;
  #   if outputs exist, we only show those by default
  # "archives" TRUE will offer to delete results archives
  #   archives FALSE will ignore results archives
  # force says "just go ahead and delete everything", respecting outputOnly and archives,
  #   so the default is to delete the outputs and leave the results, but if
  #   called a second time, will delete the results.
  # Result archives are always untouched, unless archives==TRUE, in which case
  #   all of them are deleted too.

  if ( ! private$p.valid ) {
    writeLog(self$printStatus(),Level="error")
    return( invisible(FALSE) )
  }
  
  to.delete <- self$dir(outputs=TRUE,stage=stage)
  if ( missing( outputOnly ) ) {
    outputOnly <- length(to.delete)>0
  }
  if ( ! isTRUE(outputOnly) ) to.delete <- c(to.delete,self$dir(results=TRUE))

  # Only offer archives to delete if we're looking at all stages
  if ( isTRUE(archives) && is.null(stage) ) to.delete <- c(to.delete,self$dir(archive=TRUE))

  # note, by default $dir shortens by removing self$modelPath
  # so deletion will only work if getwd()==self$modelPath
  if ( normalizePath(getwd() ) != self$modelPath ) {
    owd <- setwd(self$modelPath)
    on.exit(setwd(owd))
  }

  # "stage" could be any vector of intermediate sub-directory names
  if ( is.character(stage) && ! isTRUE(archives) ) {
    # keep only files in subdirectories matching stage$Dir
    stageDirs <- sapply(self$modelStages,function(s) s$Dir)
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
          if ( is.numeric(response) ) {
            candidates <- to.delete[start:stop]
            unlink(candidates[response],recursive=TRUE)
            cat("Deleted:\n",paste(candidates[response],collapse="\n"),"\n")
          }
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
  }
  if ( ! force ) cat("Nothing else to delete.\n")
  self$load(onlyExisting=TRUE,reset=TRUE) # Reload cached model state
  return(invisible(force))
}

################################################################################
# VEModelStage Class
################################################################################

# Load the stage configuration
ve.stage.init <- function(Name=NULL,Model=NULL,modelParam_ls=NULL,stageParam_ls=list()) {
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
      self$Name <- stageParam_ls$StageName
    } else stop("No Name provided for model stage.")
  } else self$Name <- Name

  if ( is.null(Model) || ! "VEModel" %in% class(Model) ) {
    stop("No VEModel provided to own model stage ",Name)
  } else self$Model <- Model

  # Initialize model parameters
  ModelName     <- Model$modelName
  ModelDir      <- Model$modelPath # Should already be a normalized path
  if ( is.null(modelParam_ls) ) {
    modelParam_ls <- Model$RunParam_ls
    if ( is.null(modelParam_ls) ) modelParam_ls <- list()
  }
  # modelParam_ls may not exist if we are building stages during model initialization...

  # Pull stageParam_ls from ModelStages in Model (could do that manually from outside)
  if ( ( !is.list(stageParam_ls) || length(stageParam_ls)==0 ) && "ModelStages" %in% names(modelParam_ls) ) {
    msp <- modelParam_ls$ModelStages[[self$Name]]
    if ( ! is.null(msp) ) stageParam_ls <- msp
  }

  # Warn if stage is not unique at level "info"
  if ( Name %in% names(Model$modelStages) ) {
    writeLog(paste0("Model name ",self$Name," is already defined in ",ModelName),Level="info")
    # Not an error, as this object may replace the one already in the Model
    # That error is trapped when this stage is pushed back into the model
  } else writeLog(paste0("Initializing Model Stage:",self$Name),Level="info")

  # Parse the stageParam_ls (any of these may still be NULL)
  self$Dir         <- stageParam_ls$Dir
  self$Path        <- stageParam_ls$Path
  self$Config      <- stageParam_ls$Config
  self$Reportable  <- stageParam_ls$Reportable
  selfStartFrom    <- stageParam_ls$StartFrom

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
    writeLog(paste("Stage",self$Name,"stageConfig_ls contains:"),Level="info")
    writeLog(paste(names(stageConfig_ls),collapse=", "),Level="info")
  } else {
    writeLog("stageConfig_ls has no additional parameters",Level="info")
  }

  # Set up self$Path (input location)
  if ( ! is.null(self$Path) ) {
    if ( isAbsolutePath(self$Path) ) {
      if ( ! dir.exists(self$Path) ) {
        writeLog(paste0("Stage input path does not exist: ",self$Path),Level="warn")
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
    if ( ! dir.exists(self$Path) ) self$Path <- NULL
  }
  if ( ! is.null(self$Path) ) {
    writeLog(paste("Initializing Stage",self$Name,"from",self$Path),Level="info")
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
    writeLog("No ParamDir for stage",Level="info")
  }
  # If ParamDir is defined (and perhaps ParamFile), laod the configuration file
  if ( ! is.null(ParamDir) ) {
    writeLog(paste("Loading configuration from",ParamDir),Level="info")
    self$loadedParam_ls <- visioneval::loadConfiguration(ParamDir=ParamDir,ParamFile=ParamFile)
  } else {
    writeLog("No configuration file for stage",Level="info")
    self$loadedParam_ls <- list()
  }
  if ( length(self$loadedParam_ls) > 0 ) {
    writeLog(paste("Stage",self$Name,"loadedParam_ls contains:"),Level="info")
    writeLog(paste(names(self$loadedParam_ls),collapse=", "),Level="info")
    self$RunParam_ls <- visioneval::mergeParameters(modelParam_ls,self$loadedParam_ls) # config file overrides model
  } else {
    self$RunParam_ls <- modelParam_ls
  }
  if ( length(stageConfig_ls) > 0 ) { # already logged stageConfig_ls
    self$RunParam_ls <- visioneval::mergeParameters(self$RunParam_ls,stageConfig_ls)   # parameters in ModelStage or command line override
  }
  writeLog(paste("Stage",self$Name,"RunParam_ls contains:"),Level="info")
  writeLog(paste(names(self$RunParam_ls),collapse=", "),Level="info")

  # Stage Output
  self$RunPath <- file.path(
    ModelDir,
    Model$setting("ResultsDir"),
    self$Dir
  )
  self$RunPath <- normalizePath(self$RunPath)
  writeLog(paste("Stage RunPath:",self$RunPath),Level="info")

  # Set stage InputPath (will be culled later if not useful)
  if (
    ! "InputPath" %in% names(self$RunParam_ls) &&
    is.character(self$Path) &&
    dir.exists(self$Path)
  ) {
    writeLog("Adding stage path to InputPath",Level="info")
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$initialize",
      InputPath=self$Path
    )
  }

  # Identify "startFrom" stage (VEModelStage$runnable will complete setup)
  # Can find StartFrom through ModelStages or from the stage configuration file/parameters
  if (
    ( !is.character(self$StartFrom) || length(self$StartFrom)==0 || ! nzchar(self$StartFrom) ) &&
    "StartFrom" %in% names(self$RunParam_ls)
  ) {
    self$StartFrom <- self$RunParam_ls$StartFrom
    writeLog(paste("Starting From:",self$StartFrom),Level="info")
  } else {
    writeLog("No StartFrom in self$RunParam_ls:",Level="info")
    writeLog(paste(names(self$RunParam_ls),collapse=","),Level="info")
    self$StartFrom <- character(0)
  }

  # Wait for "runnable" setup to unpack StartFrom and to build final InputPath and
  # DatastorePath
}

# Prepare the stage to run in the model context
# Don't do this during init because it depends on prior runnable stages
# Also load its ModelState if the stage has been run
ve.stage.runnable <- function(priorStages) {
  # short circuit if we already verified the stage object
  # Will use short circuit if new stage is being added programmatically to the model
  # (VEModel$addstage)
  if ( ! is.null(private$complete) ) return(private$complete)

  # dig out the information from the StartFrom stage
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
    InputPath     <- startFrom$InputPath      # Use this as the base InputPath
    if ( ! is.null(InputPath) ) {
      InputPath <- c( self$RunParam_ls$InputPath, InputPath )
    } else {
      InputPath <- self$RunParam_ls$InputPath
    }
    StartFromScriptPath <- startFrom$ModelScriptPath
  } else {
    writeLog("No StartFrom for stage",Level="info")
    startFrom     <- list()
    DatastorePath <- character(0)             # Default is to contribute no DatastorePath
    ParamPath <- self$RunParam_ls$ParamPath   # May be NULL
    InputPath <- self$RunParam_ls$InputPath   # May be NULL
    StartFromScriptPath <- NULL
  }

  # Construct InputPath
  self$RunParam_ls <- visioneval::addRunParameter(
    self$RunParam_ls,
    Source="VEModelStage$runnable",
    InputPath=cullInputPath( # remove duplicates
      InputPath=InputPath,
      InputDir=visioneval::getRunParameter("InputDir",Param_ls=self$RunParam_ls)
    )
  )
  writeLog(paste0("InputPath for ",self$Name,": ",self$RunParam_ls$InputPath),Level="info")

  # Construct DatastorePath, prepending ModelDir/ResultsDir/StageDir as
  #   first element (writable)
  self$RunParam_ls <- visioneval::addRunParameter(
    self$RunParam_ls,
    Source="VEModelStage$runnable",
    DatastorePath=c( self$RunPath, DatastorePath)
  )
  writeLog(paste0("DatastorePath for ",self$Name,": ",self$RunParam_ls$DatastorePath),Level="info")

  # Underlay run_parameters.json from StagePath/ParamDir/ParamFile if it is out there
  # run_parameters.json is deprecated, and is expected only to provide "descriptive" parameters
  # e.g. Scenario or Description, possibly Years
  # New style model will set those in StageDir/visioneval.cnf
  writeLog("Looking for run_parameters.json",Level="info")
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
    writeLog(paste("Parsing stage ModelScriptFile:",stageModelScriptPath),Level="info")
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
  writeLog("DatastoreType:",self$RunParam_ls$DatastoreType)

  # Check if stage can run (enough parameters to run visioneval::loadModel and visioneval::prepareModelRun)
  missingParameters <- visioneval::verifyModelParameters(self$RunParam_ls)
  private$complete <- length(missingParameters) == 0
  if ( ! private$complete ) {
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
  if ( ! self$load(onlyExisting=TRUE) ) self$RunStatus <- codeStatus("Initialized")

  return(TRUE)
}

# Load the model state for the stage
# Create it if onlyExisting=FALSE (that's a time-consuming operation)
# if reset==TRUE, 
ve.stage.load <- function(onlyExisting=TRUE,reset=FALSE) {
  if ( reset ) self$ModelState_ls <- NULL
  if ( is.null(self$ModelState_ls) ) {
    envir = visioneval::modelEnvironment(Clear="ve.stage.load")
    envir$RunModel <- FALSE
    # We're expecting not to write anything, but we'll set useful
    #  directories anyway.
    owd <- if ( dir.exists(self$RunPath) ) {
      setwd(self$RunPath)
    } else setwd(self$RunParam_ls$ModelDir)
    on.exit(setwd(owd))
    ms <- visioneval::loadModel(self$RunParam_ls,onlyExisting=onlyExisting,Message=paste("Loading Model",self$modelName))
    if ( is.list(ms) && length(ms)>0 ) { # Stash the ModelState if created successfully
      self$ModelState_ls <- ms
      if ( ! "RunStatus" %in% names(self$ModelState_ls) ) {
        self$ModelState_ls$RunStatus <- codeStatus("Loaded")
      }
      self$RunStatus <- self$ModelState_ls$RunStatus
      return(TRUE)
    } else if ( ! onlyExisting) {
      writeLog(
        paste0("Unable to build model state for stage ",self$Name,"!"),
        Level="warn"
      )
    } else {
      self$RunStatus <- codeStatus("Uninitialized")
    }
  }
  return(FALSE)
}

# Run the model stage
ve.stage.run <- function(log="warn") {
  # Mark local status on the stage
  self$RunStatus <- codeStatus("Running")
  # TODO: add PID below if running in background process

  # Remove any existing ModelState and run results from this stage
  self$ModelState_ls <- NULL
  self$Results       <- NULL

  # Construct and change to working directory
  if ( ! dir.exists(self$RunPath) ) dir.create(self$RunPath)
  owd <- setwd(self$RunPath)
  on.exit(setwd(owd))

  # Take ownership of ve.model
  ve.model <- visioneval::modelEnvironment(Clear="VEModelStage::run") # add Owner

  RunStatus <- try (
    {
      # Initialize Log, create new ModelState
      ve.model$RunModel <- TRUE
      initLog(Threshold=log,Save=TRUE,envir=ve.model) # Log stage
      # Create new ModelState_ls (ignore existing)
      visioneval::loadModel(self$RunParam_ls)
      visioneval::setModelState()                       # Save ModelState.Rda
      visioneval::prepareModelRun()                     # Initialize Datastore

      # Run the model script
      sys.source(self$RunParam_ls$ModelScriptPath,envir=new.env())

      # If we get this far without a "stop", save the ModelState and RunStatus
      RunStatus <- codeStatus("Run Complete")
    },
    silent=TRUE
  )

  # Process results (or errors)
  if ( ! is.numeric(RunStatus) ) {
    # Failure: stop trapped while performing run sequence
    msg <- as.character(RunStatus) # try-error (captures "stop" message)
    writeLog(msg,Level="error") # possibly redundant with interior logging
    self$RunStatus <- codeStatus("Run Failed")
    if ( "ModelState_ls" %in% names(visioneval::modelEnvironment()) ) {
      visioneval::setModelState(list(RunStatus=self$RunStatus),envir=ve.model)
    }
  } else {
    # Success: Assemble the runResults
    self$RunStatus <- RunStatus
    visioneval::setModelState(list(RunStatus=self$RunStatus),envir=ve.model)
    writeLog(printStatus(self$RunStatus),Level="warn")
  }
  visioneval::saveLog(LogFile="console",envir=ve.model)
  self$ModelState_ls <- ve.model$ModelState_ls # The story of the run...
  return(invisible(self$ModelState_ls))
}

# Helper
# List unique sources in a parameter list
uniqueSources <- function(Param_ls) {
  sources <- attr(Param_ls,"source")
  if ( is.null(sources) ) {
    writeLog("'sources' attribute is null in uniqueSources",.traceback(1),Level="info")
    sources <- "NULL"
  } else {
    sources <- unique(sources$Source)
  }
  return(sources)
}

# Print a model stage summary
ve.stage.print <- function(details=FALSE) {
  cat("Stage:",self$Name,"(Reportable:",self$Reportable,")\n")
  cat("   Status:",printStatus(self$RunStatus),"\n")
  if ( details ) {
    if ( length(self$StartFrom)>0 && nzchar(self$StartFrom) ) cat("   Starts from:",self$StartFrom,"\n")
    cat("   Configurations:\n")
    cat(paste0("     ",uniqueSources(self$RunParam_ls)),sep="\n") # Generates one row for each unique source
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
    # TODO: Add Package and Module to specSummary
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
  # if both are true, we also liet the Get elements
  # "details" FALSE lists units and description plus pacakge/module/group/table/name
  # "details" TRUE lists all the field attributes (the full data.frame of specSummary)
  # "stage" is a list of stage names (default = all stages) to appear in the output
  #   The fields shown are just the ones accessed in that particular stage
  # TODO: Include only Reportable stages unless name specified. If a reportable stage is
  #   requested, include the "StartFrom" tree for this stage as well. So augment the
  #   stage list by including "StartFrom" stages for each named stage (or each
  #   Reportable stage by default).

  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( invisible(data.frame()) )
  }

  # Update specSummary
  if ( reset || is.null(self$specSummary) ) {
    writeLog("Loading model specifications (may take some time)...",Level="warn")
    self$load(onlyExisting=FALSE) # Create new model states if they are not present in the file system
    for ( stage in self$modelStages ) {
      writeLog(paste("Loading Stage specs for",stage$Name),Level="info")
      AllSpecs_ls <- stage$ModelState_ls$AllSpecs_ls
      if ( ! is.null( AllSpecs_ls ) ) {
        specFrame <- summarizeSpecs(AllSpecs_ls,stage$Name)
        if ( is.null(self$specSummary) ) {
          self$specSummary <- specFrame
        } else {
          rbind(self$specSummary,specFrame)
        }
      } else {
        writeLog(paste("No specifications for",stage$Name),Level="warn")
      }
    }
    writeLog(paste("Loaded",nrow(self$specSummary),"Specifications"),Level="info")
  }

  # which rows to return
  inputRows <- if ( inputs ) which(self$specSummary$SPEC=="Inp") else integer(0)
  outputRows <- if ( outputs ) which(self$specSummary$SPEC=="Set") else integer(0)
  usedRows <- if ( inputs == outputs ) which(self$specSummary$SPEC=="Get") else integer(0)
  listRows <- unique(c(inputRows,outputRows,usedRows))

  # which fields to return
  listFields <- c("SPEC","PACKAGE","MODULE","GROUP","TABLE","NAME")
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
ve.model.print <- function(details=FALSE) {
  cat("Model:",self$modelName,"\n")
  if ( details ) {
    cat("Path:","\n")
    cat(self$modelPath,"\n")
    cat("Configurations:","\n")
    cat(paste("  ",uniqueSources(self$RunParam_ls)),sep="\n") # Generates one row for each unique source
  }
  cat("Status:", self$printStatus(),"\n")
  if ( private$p.valid ) {
    cat("Model Stages:\n")
    for ( s in self$modelStages ) {
      s$print(details)
    }
  }
  private$p.valid
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
      logFile <- dir(p,pattern="\\.log$",full.names=TRUE)
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
  "Unknown",
  "Uninitialized",
  "Run Failed",
  "Load Failed",
  "No Stages Defined",
  "Initialized",
  "Loaded",
  "Running",
  "Run Complete"
)

# Helper - Decode status and handle "out of bounds"
printStatus <- function(status) {
  if ( ! is.integer(status) || status < 1 || status > length(StatusLevelCodes) ) {
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
    status <- self$status
  }
  return( printStatus(status) )
}

# Show "minimum" stage status (see StatusLevelCodes)
ve.model.updateStatus <- function() {
  private$p.valid <- is.list(self$modelStages) && length(self$modelStages)>0
  self$status <- if ( private$p.valid ) {
    min(sapply(self$modelStages,function(s) s$RunStatus))
  } else {
    codeStatus("No Stages Defined")
  }
}

# Helper function:
#    Key function here is to create an in-memory copy of each stage's ModelState_ls
#      either by loading, or by initializing
#    Also need to handle the StartFrom earlier model stage (where to find the
#      model state that goes into DatastorePath, as well as the model state to
#      interrogate for VERequiredPackages).
#  Resulting structure can be explored for model inputs and outputs, script, etc.

# Two needs:
#   1. Load existing model states if stages have been run before
#   2. Build new model states if not running to parse the model script and check that
#      everything is where it needs to be - unintrusive
# Second step will not be performed if "onlyExisting" is TRUE (the default)

ve.model.load <- function(onlyExisting=TRUE,reset=FALSE) {

  if ( ! self$valid() ) {
    stop(
      writeLog("Model is incomplete and cannot be loaded.",Level="error")
    )
  }

  # Un-cache the specSummary (things may have changed)
  self$specSummary <- NULL

  # Load or Create the ModelState_ls for each stage
  for ( index in seq_along(self$modelStages) ) {
    stage <- self$modelStages[[index]]
    writeLog(paste("Loading stage",stage$Name),Level="info")
    if ( ! is.null( stage$RunParam_ls$StartFrom ) ) {
      startFrom <- stage$RunParam_ls$StartFrom
      startFromState_ls <- self$modelStages[[startFrom]]$ModelState_ls # may be NULL
      if ( ! is.null(startFromState_ls) ) {
        visioneval::addRunParameter(
          stage$RunParam_ls,
          Source="VEModelState::load",
          StartFromModelState=startFromState_ls
        )
      } else if ( ! onlyExisting ) {
        # Expect if we're loading a ModelState that the earlier stage has a
        # ModelState_ls, which might be newly intialized.
        stop(
          writeLog(paste("StartFrom",startFrom,"has no ModelState_ls!"),Level="error")
        )
      }
    }
    stage$load(onlyExisting=onlyExisting,reset=reset)
    self$modelStages[[index]] <- stage
  }
  # Update self$status
  self$updateStatus()
  return(self$printStatus())
}

################################################################################
#                                 Running Models                               #
################################################################################

# TODO: Consult SaveDatastore in self$ModelState_ls
# TODO: Make sure a classic model (where SaveDatastore may be buried rather deep)
#       can still be interpreted properly. We still need to pull out the "initializeModel"
#       parameters...

# Proxy some visioneval functions (things that might get called from
# classic run_model.R without namespace resolution.
getYears <- visioneval::getYears
runModule <- visioneval::runModule
runScript <- visioneval::runScript
requirePackage <- visioneval::requirePackage

# Run the modelStages
ve.model.run <- function(run="continue",stage=NULL,log="warn") {
  # run parameter can be
  #      "continue" (run all steps, starting from first incomplete; "reset" is done on the first
  #      incomplete stage and all subsequent ones, then execution continues)
  #   or "save" in which case we reset, but first save the ResultsDir tree
  #   or "reset" (or "restart") in which case we restart from stage 1, but first clear out ResultsDir (no save)
  #
  # "reset" implies deleting any ModelState or Datastore
  # "continue" will unlink/recreate starting from the first stage that is not "Run Complete"

  # if "stage" is provided, "run" is ignored: the run will reset that stage and subsequent ones
  #   and then do "continue". No saving will occur.
  # "stage" could perhaps be a vector of stages - just those stages will be reset or re-run.
  #   "stage" can be specified either as an index position in self$modelStages or as a named position

  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( invisible(self$status) )
  }
  
  # If save, like reset, but forces SaveDatastore to be TRUE
  # If reset, then go back to the first stage and run from there

  # If continue, leave existing ResultsDir, but find the first incomplete stage and reset it,
  #   then continue from there.

  # "continue" will step through the stages looking for the first that is not "Run Complete"
  # SaveDatastore is ignored in that case.
  # That stage will be re-initialized.

  # TODO: Clear and take ownership of ve.model

  # Set up workingResultsDir for file manipulations (including stage sub-directories)
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  workingResultsDir <- normalizePath(file.path(self$modelPath,ResultsDir),winslash="/",mustWork=FALSE)

  # Determine which stages need to be-rerun
  if ( run=="continue" ) {
    self$load(onlyExisting=TRUE,reset=TRUE) # Open any existing ModelState_ls to see which may be complete
    alreadyRun <- ( sapply( self$modelStages, function(s) s$RunStatus ) == codeStatus("Run Complete") )
    if ( all(alreadyRun) ) {
      self$status <- codeStatus("Run Complete")
      writeLog("Model Run Complete",Level="warn")
      return(invisible(self$printStatus()))
    } else {
      toRun <- which(!alreadyRun)[1] # start from first un-run stage
    }
  } else {
    toRun <- 1 # Start at first stage
  }
  if ( toRun == 1 && run != "save" ) run <- "reset" # If starting over, process SaveDatastore as needed

  # Save existing results if we're restarting or resetting
  SaveDatastore = NULL # ignore any pre-configured value for SaveDatastore
  if ( run == "restart" || run=="reset" ) {
    SaveDatastore <- FALSE
    writeLog(paste("Removing previous Results from",workingResultsDir),Level="info")
    unlink(dir(workingResultsDir,full.names=TRUE),recursive=TRUE)
  } else if ( run == "save" ) {
    SaveDatastore <- TRUE
    run <- "reset"
  } else {
    SaveDatastore <- visioneval::getRunParameter("SaveDatastore",Param_ls=self$RunParam_ls)
  }

  if ( run != "continue" && SaveDatastore && dir.exists(workingResultsDir) ) {
    # Archive previous results if SaveDatastore true in RunParam_ls and previous results exist
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

  # Identify stages to run
  runStages <- names(self$modelStages)
  if ( toRun > length(runStages) ) { # should never happen
    writeLog(paste("Stages:",runStages,collapse=","),Level="info")
    msg <- writeLog(paste("Starting stage",toRun,"comes after last stage",length(runStages)),Level="error")
    return( invisible(self$status) )
  }
  runStages <- runStages[toRun:length(runStages)]

  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- normalizePath(file.path(self$modelPath,BaseInputPath),winslash="/",mustWork=FALSE)
  }

  owd <- getwd()
  on.exit(setwd(owd))

  # Establish the LogLevel from the overall environment
  LogLevel <- visioneval::getRunParameter("LogLevel",Default=log,Param_ls=self$RunParam_ls)

  # Set up the model runtime environment
  for ( ms in runStages ) { # iterate over names of stages to run
    writeLog(paste("Running stage:",ms),Level="warn")
    self$modelStages[[ms]]$run(log=LogLevel)
    if ( self$modelStages[[ms]]$RunStatus != codeStatus("Run Complete") ) {
      msg <- writeLog(paste("Model failed with status",self$printStatus(self$modelStages[[ms]]$RunStatus)),Level="error")
      stop(msg)
    }
  }

  # Update overall model status
  self$updateStatus() # "Worst"/Lowest RunStatus for the overall model

  return(invisible(self$status))
}

################################################################################
#                              Model Configuration                             #
################################################################################

# Function to add a stage to a loaded model. Accepts a stageParam_ls (as we would use for
#   ve.stage.init), which should contain, at a minimum, a "Name" (which must be unique in the model)
#   and a "StartFrom" designating another stage in the current model. The dots resolve to a named
#   list of parameters that are considered to be part of the StageConfig (e.g. InputPath for the
#   stage) - anything that might go into a stage's visioneval.cnf. The combination of stageParam_ls
#   and ... must add up to a runnable stage (errors identified in self$initstages)

# TODO: restructure with explicit stage Name (not from stageParam_ls)
# Then just have stageParam_ls be the entire set of ... (and don't bother with
# stageConfig_ls since the stage.init rolls anything left over from stageParam_ls
# into stageConfig_ls.
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
  if ( ! self$modelStages[[stage$Name]]$runnable() ) {
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
    src_df <- attr(searchParams_ls,"source")
    if ( is.character(setting) ) src_df <- src_df[ setting, ]
    if ( shorten) src_df$Source <- sub(paste0(self$modelPath,"/"),"",src_df$Source,fixed=TRUE)
    return(src_df[,"Source",drop=FALSE])
  } else {
    # Return values
    if ( ! is.character(setting) ) {
      return(names(searchParams_ls))
    } else {
      if ( length(setting)==1 && nzchar(setting) ) {
        settings <- searchParams_ls[[setting]] # single setting
      } else {
        if ( length(setting)>1 && all(nzchar(setting)) ) {
          settings <- searchParams_ls[setting]   # list of matching settings
        } else {
          settings <- searchParams_ls # Warning: potentially huge!
        }
      }
      if ( shorten) settings <- sub(paste0(self$modelPath,"/"),"",settings,fixed=TRUE)
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
  if ( Reportable ) {
    stages <- stages[ sapply(stages,function(s) s$Reportable) ] # Only return reportable stages
  }
  return(stages)
}

################################################################################
#                                Model Results                                 #
################################################################################

# create a VEResults object or list of VEResults objects (possibly invalid/empty) from the model's
# Reportable stages. Provide a vector of stage names or indices to filter the list.
ve.model.results <- function(stage=character(0)) {
  if ( ! private$p.valid ) {
    writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
    return( NULL )
  }
  stages <- self$findstages(stage)
  if ( length(stages)==0 ) {
    writeLog(paste("Available stages:",names(self$modelStages),collapse=", "),Level="error")
    stop(
      writeLog(paste("Model stage(s) not found:",stage,collapse=","),Level="error")
    )
  }
  results <- lapply(
    stages,
    function(stg) VEResults$new(stg$RunPath,ResultsName=stg$Name)
  )
  names(results) <- names(stages)
  valid <- sapply( results, function(r) r$valid() )
  if ( any( ! valid ) ) {
    writeLog(
      paste("No results yet for stage(s): ",names(results)[!valid],collapse=", "),
      Level="warn"
    )
    writeLog("Have you run the model?",Level="warn")
  }
  if ( length(results)>1 ) {
    class(results) <- "VEResultsList" # print function defined below
  } else if ( length(results)==1 ) {
    results <- results[[1]]              # Just return the single VEResults object
#    names(results) <- names(results)[1]  # Carry over the name
  }
  return(results)
}

#' pretty print a list of VEResults objects
#' 
#' @param x a named list of VEResults objects
#' @param ... Other parameters for generic print function
#' @export
print.VEResultsList <- function(x,...) {
  for ( nm in names(x)) {
    print(x[[nm]],name=nm,...)
  }
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
#     cat("QueryDir:"); print(QueryDir)
#     cat("Query Directory:"); print(QueryPath)
#     cat("Query Directory Exists:"); print(dir.exists(QueryPath))
#     cat("Available Queries:\n")
    queries <- dir(QueryPath,pattern="\\.(VEqry|R)$",ignore.case=TRUE)
    if ( length(queries)==0 ) queries <- "No queries defined"
    return(queries)
  }
  # Let VEquery find the query...
  return(
    VEQuery$new(
      QueryName=QueryName,
      ModelPath=self$modelPath,
      QueryDir=QueryDir,
      FileName=FileName,
      load=load
    )
  )
}

################################################################################
#                          Exported Helper Functions                           #
################################################################################

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
    return(
      dir(
        file.path(
          getRuntimeDirectory(),
          visioneval::getRunParameter("ModelRoot")
        )
      )
    )
  } else {
    initLog(Save=FALSE,Threshold=log, envir=new.env())
    return( VEModel$new(modelPath = modelPath) )
  }
}

##########################################################################################
#                      MANAGE "STANDARD MODELS" (VEModel Examples)                       #
##########################################################################################

#' pretty print a list of standard models
#' 
#' @param x a character vector of standard model names
#' @param ... Other parameters for generic print function
#' @export
print.VEAvailableModels <- function(x,...) {
  cat("Available Models:\n")
  print(as.character(x),...)
}

#' pretty print a list of variants for a standard model
#' 
#' @param x a character vector of variant names with a "Model" attribute naming the model
#' @param ... Other parameters for generic print function
#' @export
print.VEAvailableVariants <- function(x,...) {
  model <- attr(x,"Model")
  cat("Available Variants for ",model,":\n",sep="")
  print(as.character(x),...)
}

## Look up a standard model
#  'model' is bare name of standard model
#  returns the full path to that model template
findStandardModel <- function( model, variant="" ) {
  standardModels <- system.file("models",package="VEModel")
#   if ( ! nzchar(standardModels) ) {
#     # The following is for testing purposes...
#     standardModels <- getOption("VEStandardModels",default=normalizePath("inst/models"))
#   }
  # Covid Joke
  if ( toupper(variant)=="DELTA" ) return( "Cough, Cough!" )

  if ( missing(model) || is.null(model) || ! nzchar(model)) {
    available.models <- dir(standardModels)
    class(available.models) <- "VEAvailableModels"
    return( available.models )
  }

  # Locate the model
  model <- model[1]
  model_ls <- list()
  model_ls$Name <- model
  model_ls$ModelDir <- file.path(standardModels,model)
  if ( ! dir.exists(model_ls$ModelDir) ) {
    writeLog(paste("No standard model called ",model),Level="error")
    return( findStandardModel("") ) # recursive call to keep return directory in one place
  }

  # Read the model index to identify variants ("base" should always exist)
  confPath <- file.path(model_ls$ModelDir,"model-index.cnf")
  modelIndex <- try( yaml::yaml.load_file(confPath) )
  if ( ! "variants" %in% names(modelIndex) ) {
    writeLog(paste0("No model variants defined in ",confPath),Level="error")
    writeLog(c(class(modelIndex),names(modelIndex)),Level="error")
    return(c("No model variant:",variant))
  }
  if ( missing(variant) || ! nzchar(variant) || ( ! variant %in% names(modelIndex$variants) ) ) {
    variantMsg <- names(modelIndex$variants)
    attr(variantMsg,"Model") <- model
    class(variantMsg) <- "VEAvailableVariants"
    if ( nzchar(variant) ) { # not in list of variants
      msg <- writeLog(paste0("Unknown variant '",variant,"'"),Level="error")
      print(variantMsg)
      stop(msg)
    }
    return(variantMsg)
  }

  # Load the variant configuration
  model_ls$Variant <- variant
  variantConfig <- modelIndex$variants[[variant]]

  # Get config file and description
  model_ls$Description <- variantConfig$description
  if ( "config" %in% names(variantConfig) ) {
    model_ls$Config <- normalizePath(file.path(model_ls$ModelDir,variantConfig$config))
  } # if no Config, move contents of scripts directory to installPath (hack for classic model)

  # Get standard directories to copy (including stage directories if any)
  modelTo <- c("scripts","inputs","defs","queries")
  modelTo <- modelTo[ modelTo %in% names(variantConfig) ]
  modelFrom <- unlist( variantConfig[modelTo] )
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

  return(model_ls) # model structure for installation
}

## install a standard model with data identified by "variant"
#  We're still expecting to distribute standard models in the runtime (but for now, those will
#   be the "classic" models)
installStandardModel <- function( modelName, modelPath, confirm, overwrite=FALSE, variant="base", log="error" ) {
  # Locate and install standard modelName into modelPath
  #   modelName says which standard model to install. If it is missing or empty, return a
  #     list of available models
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first
  #     available root (see VEModel::getModelRoots function)
  #   Otherwise, modelPath is presumed to be the directory into which modelName will be installed
  #     (and basename(modelPath) will become the target model name, disambiguated).
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and
  #     they have to try again

  model <- findStandardModel( modelName, variant )
  if ( ! is.list(model) ) {
    return(model) # Expecting a character vector of available information about standard models
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
    msg <- paste0("Install standard model '",model$Name,"' into ",installPath,"?\n")
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
      writeLog(msg<-paste0("No variant (",variant,") for ",modelName),Level="error")
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
#'   ve.runtime/models. If directory does not exist, create it and copy the modelName into it.
#'   If directory does exist, create a unique variant of modelName adjacent to it. If it is NULL
#'   create a unique variant of modelName in ve.runtime/models.
#' @param variant the name of a model variant (staging, sample data, etc) to install; empty string
#'   will list available variants for modelName
#' @param confirm if TRUE (default) and running interactively, prompt user to confirm, otherwise
#'   just do it.
#' @param overwrite if TRUE (default = FALSE) then overwrite (recreate) any modelName that already
#'   exists - otherwise modelName is disambiguated as "modelName(1)", "modelName(2)" etc.
#' @param log a string describing the minimum level to display
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, variant="base", confirm=TRUE, overwrite=FALSE, log="info") {
  # Load system model configuration (clear the log status)
  initLog(Save=FALSE,Threshold=log, envir=new.env())
  model <- installStandardModel(modelName, modelPath, confirm=confirm, overwrite=overwrite, variant=variant, log=log)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath ) )
  } else {
    return( model ) # should be a character vector of information about standard models
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
    modelResults=NULL,                      # Absolute path == modelPath/ResultsDir
    RunParam_ls=NULL,                       # Run parameters for the model (some constructed by $configure)
    loadedParam_ls=NULL,                    # Loaded parameters (if any) present in model configuration file
    specSummary=NULL,                       # List of inputs, gets and sets from master module spec list  
    status=codeStatus("Uninitialized"),     # Where are we in opening or running the model?

    # Methods
    initialize=ve.model.init,               # initialize a VEModel object
    configure=ve.model.configure,           # load or re-load VEModel from its disk location
    initstages=ve.model.initstages,         # Complete stage setup
    addstage=ve.model.addstage,             # Add a stage programmatically
    valid=function() private$p.valid,       # report valid state
    load=ve.model.load,                     # load the model state for each stage (slow - defer until needed)
    run=ve.model.run,                       # run a model (or just a subset of stages)
    print=ve.model.print,                   # provides generic print functionality
    printStatus=ve.model.printStatus,       # turn integer status into text representation
    updateStatus=ve.model.updateStatus,     # fill in overall status based on individual stage status
    list=ve.model.list,                     # interrogator function (script,inputs,outputs,parameters
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    log=ve.model.log,                       # report the log file path (use e.g. file.show to display it)
    setting=ve.model.setting,               # report the values of parameter settings (for environment, model or stage)
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    archive=ve.model.archive,               # apply framework archive function if results exist
    results=ve.model.results,               # Create a VEResults object (if model is run); option to open a past result
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
    Name = NULL,                 # Name of stage (used e.g. in another stage's StartFrom)
    Model = NULL,                # VEModel to which this stage belongs
    # May not need to save some of the following (build into RunParam_ls)
    Dir = NULL,                  # basename of stage subdirectory (for inputs and results)
    Path = NULL,                 # Normalized path to folder holding InputDir and ParamDir
    Config = NULL,               # File relative to ModelDir (optional); 
    Reportable = NULL,           # If TRUE, include in default result set for extract and queries
    StartFrom = "",              # Name of another model stage to extend
    RunParam_ls = list(),        # RunParameters to initialize the stage
    loadedParam_ls = NULL,       # Loaded parameters (if any) present in stage configuration file
    ModelState_ls = NULL,        # ModelState constructed from RunParam_ls
    RunPath = NULL,              # Typically ModelDir/ResultsDir/StageDir
    RunStatus = 1, # "Unknown"   # Indicates whether results are available
    Results = NULL,              # A VEResults object (create on demand)
    
    # Methods
    initialize=ve.stage.init,    # Create a stage and set its Runnable flag
    runnable=ve.stage.runnable,  # Does the stage have all it needs to run?
    print=ve.stage.print,        # Print stage summary
    load=ve.stage.load,          # Read existing ModelState.Rda if any
    run=ve.stage.run             # Run the stage (build results)
  ),
  private=list(
    complete = NULL              # set to TRUE/FALSE when self$runnable() is executed
  )
)
