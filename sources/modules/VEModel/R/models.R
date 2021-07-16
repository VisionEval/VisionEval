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
#' mdl$save(FileName="visioneval.cnf")
#' mdl$copy(newName=NULL,newPath=NULL,copyResults=TRUE)
#' mdl$rename(Name=NULL,Scenario=NULL,Description=NULL,Save=TRUE)
#' mdl$results(stage,Param_ls=NULL)
#' mdl$resultspath(stage,Param_ls=NULL)
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
#'   $results and $resultspath, will allow a different set of parameters to used than self$RunParam_ls (which is used
#'   internally to track BaseModel and such). See \code{runtimeEnvironment()} and Details below.}
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
# Add parameters to modelParam_ls from BaseModel
# Do nothing if BaseModel not present in modelParam_ls
addBaseModel <- function(modelParam_ls) {

  existingParams <- names(modelParam_ls)
  if ( ! "BaseModel" %in% existingParams ) {
    return(modelParam_ls)
  }
  baseModelName <- modelParam_ls$BaseModel
  
  if ( nzchar(baseModelName) ) {
    baseStageName <- visioneval::getRunParameter("BaseStage",Default="",Param_ls=modelParam_ls)

    msg <- paste0("Recursively opening BaseModel '",baseModelName,"'")
    if ( nzchar(baseStageName) ) msg <- paste(msg,paste0("at Stage '",baseStageName,"'"))
    visioneval::writeLog(msg,Level="warn")
    
    baseModel <- openModel(baseModelName) # Amounts to a recursive call to initialize
    if ( ! baseModel$valid() ) {
      stop( visioneval::writeLog("BaseModel is not valid!",Level="error") )
    }
    if ( ! nzchar(baseStageName) ) {
      baseStageName <- utils::tail(names(baseModel$modelStages),1)
    }
    baseParam_ls <- baseModel$modelStages[[baseStageName]]$RunParam_ls
    if ( is.na(baseParam_ls) || is.null(baseParam_ls) ) {
      stop (
        visioneval::writeLog(
          paste0("Missing Parameters from BaseModel/BaseStage ",baseModelName,"/",baseStageName),
          Level="error"
        )
      )
    }

    # Only keep the following parameters from the BaseModel/BaseStage which define locations to search
    #  for elements of this model: inputs, defs, run_model.R plus queries and possible datastore path
    keepBase <- which(
      names(baseParam_ls) %in% c(
        "InputPath","ModelScriptPath","ParamPath","QueryPath",
        "RunDstore","DatastorePath" # Used to link to BaseModel Datastore (or copy it)
        )
    )
    baseParam_ls <- baseParam_ls[keepBase]
  } else return(modelParam_ls)

  # Rebuild model default path parameters (copying from base model as available)

  # If DatastorePath is defined in modelParam_ls, do not transfer it from the BaseModel parameters
  # DatastorePath is a vector paths to Datastores in the BaseModel's stages
  # Define it in the model (possibly to an empty string) if not linking to the BaseModel results
  if ( ! "DatastorePath" %in% existingParams ) {
    modelParam_ls$DatastorePath <- baseParam_ls$DatastorePath # Link BaseModel results
    modelParam_ls$LoadDstore <- baseParam_ls$RunDstore        # Supports LoadDatastore
  }
  
  # If ParamDir defined in modelParam_ls: set ParamPath if ModelDir/ParamDir exists
  if ( "ParamDir" %in% existingParams ) {
    paramPath <- file.path(modelParam_ls$ModelDir,modelParam_ls$ParamDir)
    if ( dir.exists(paramPath) ) modelParam_ls$ParamPath <- paramPath
  } else {
    modelParam_ls$ParamPath <- baseParam_ls$ParamPath # Use ParamDir("defs") from BaseModel
  }

  # If InputPath defined in modelParam_ls (even to an empty string), ignore BaseModel
  # Note that ModelDir/InputDir will get added to the path if it exists anyway (no need to define InputPath for that).
  # Individual stage InputPaths will likewise be added to the model's overall InputPath
  if ( ! "InputPath" %in% names(modelParam_ls) ) {
    modelParam_ls$InputPath <- baseParam_ls$InputPath
  }

  # If ScriptDir defined in modelParam_ls (even to "." or ""), or ModelScript defined here, ignore BaseModel
  # Otherwise use BaseModel script
  if ( any(c("ScriptsDir","ModelScript") %in% existingParams) ) {
    modelScriptPath <- file.path(
      modelParam_ls$ModelDir, # Must be defined in modelParam_ls
      # Consider default values for ScriptsDir and ModelScript
      visioneval::getRunParameter("ScriptsDir",Param_ls=modelParam_ls),
      visioneval::getRunParameter("ModelScript",Param_ls=modelParam_ls)
    )
  } else {
    # WARNING: Be careful if BaseModel is horizontally staged (different ModelScript for each stage)
    # You'll only get the script from the BaseStage!!
    # Just define ScriptsDir <- "." to avoid that.
    modelScriptPath <- baseParam_ls$ModelScriptPath
  }

  if ( "QueryPath" %in% existingParams && "QueryPath" %in% names(baseParam_ls) ) {
    modelParam_ls$QueryPath <- c( modelParam_ls$QueryPath, baseParam_ls$QueryPath )
  } else {
    modelParam_ls$QueryPath <- baseParam_ls$QueryPath # May still not exist
  }

  return(modelParam_ls)
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
# Build RunParam_ls for each model stage
# Param_ls is the initial set of runtime parameters (may be an empty list;
#   default gets runtime environment RunParam_ls)
findModel <- function( modelDir, Param_ls=getSetup() ) {

  model_ls <- list() # List of valid model stage structures

  if ( missing(modelDir) || ! is.character(modelDir) ) {
    visioneval::writeLog("findModel: Must provide modelDir locator.",Level="warn")
    return( model_ls ) # An empty list...
  }

  # Establish the model path
  # if modelPath is not an absolute path, search for it amongst the "roots"
  modelPath <- NA
  if ( ! isAbsolutePath(modelDir) ) {
    roots<-getModelRoots(Param_ls=Param_ls)
    possiblePaths <- file.path(roots,modelDir)
    existing <- dir.exists(possiblePaths)
    if ( ! any(existing) ) {
      visioneval::writeLog(
        paste0("Failed to find model '",modelDir,"' in these locations:\n",paste(roots,collapse="\n")),
        Level="error"
      )
      return(model_ls) # empty list
    } else {
      # Use first of the existing paths
      modelPath <- normalizePath(possiblePaths[existing][1],winslash="/")
    }
  } else {
    modelPath <- modelDir
    if ( ! dir.exists(modelPath) ) {
      visioneval::writeLog(
        paste0("Model directory ",modelPath," does not exist"), Level="error"
      )
      return(model_ls)
    }
  }
  model_ls$modelPath <- modelPath;

  # Load any configuration available in modelPath (on top of ve.runtime base configuration)
  modelParam_ls <- visioneval::loadConfiguration(ParamDir=modelPath,override=Param_ls)
  model_ls$RunParam_ls <- modelParam_ls
  if ( "Model" %in% modelParam_ls ) {
    model_ls$modelName <- modelParam_ls$Model
  } else {
    model_ls$modelName <- basename(modelPath) # may be overridden in run parameters
  }

  # Set up ModelDir and ResultsDir
  modelParam_ls$ModelDir <- modelPath;
  if ( ! "ResultsDir" %in% names(modelParam_ls) ) {
    # Load default parameter or get from larger runtime environment
    modelParam_ls$ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls)
  }

  # Check for BaseModel and BaseStage in model parameters
  # If present, load key parameters from the BaseModel
  modelParam_ls <- addBaseModel(modelParam_ls)

  # Process InputPath for overall model (culling directories for those actually exist)
  if ( ! "InputPath" %in% names(modelParam_ls) ) {
    modelParam_ls$InputPath <- modelParam_ls$ModelDir
  } else {
    modelParam_ls$InputPath <- c( modelParam_ls$ModelDir, modelParam_ls$InputPath )
  }
  modelParam_ls$InputPath <- cullInputPath(
    InputPath=modelParam_ls$InputPath,
    InputDir=visioneval::getRunParameter("InputDir",Param_ls=modelParam_ls)
  )

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
    if ( file.exists(ParamPath) ) modelParam_ls$ParamPath <- ParamPath
  }

  # Locate ModelScriptPath for base model if present (stages will possibly override and parse)
  ScriptName <- visioneval::getRunParameter("ModelScript",Param_ls=modelParam_ls)
  ScriptsDir <- visioneval::getRunParameter("ScriptsDir",Param_ls=modelParam_ls)
  ModelScriptPath <- visioneval::getRunParameter("ModelScriptPath",Default=character(0),Param_ls=modelParam_ls)
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
  modelParam_ls$ModelScriptPath <- ModelScriptPath # Possibly unchanged, maybe still empty vector

  # Locate model stages
  visioneval::writeLog("Locating model stages",Level="info")
  if ( ! "ModelStages" %in% names(modelParam_ls) ) {
    # In general, to avoid errors with random sub-directories becoming stages
    #  it is best to explicitly set ModelStages in the model's main visioneval.cnf
    stages <- list.dirs(modelPath,full.names=FALSE,recursive=FALSE)
    structuralDirs <- c(
      visioneval::getRunParameter("DatastoreName",Param_ls=modelParam_ls),
      visioneval::getRunParameter("QueryDir",Param_ls=modelParam_ls),
      visioneval::getRunParameter("ScriptsDir",Param_ls=modelParam_ls),
      visioneval::getRunParameter("InputDir",Param_ls=modelParam_ls),
      visioneval::getRunParameter("ParamDir",Param_ls=modelParam_ls),
      visioneval::getRunParameter("ResultsDir",Param_ls=modelParam_ls)
    )
    stages <- stages[ ! stages %in% structuralDirs ]
    stages <- stages[ grep(paste0("^",visioneval::getRunParameter("ArchiveResultsName",Param_ls=modelParam_ls)),stages,invert=TRUE) ]
    stages <- c(".",stages) # Add model root directory
    visioneval::writeLog(paste0("Stage directories:\n",paste(stages,collapse=",")),Level="info")
    modelStages <- lapply(stages,
      function(stage) {
        list(
          Name=sub("^\\.$","ModelDir",stage),      # Will only change root directory
          Dir=stage,                               # Relative to modelPath
          Path=normalizePath(file.path(modelParam_ls$ModelDir,stage),winslash="/")
        )
      }
    )
    names(modelStages) <- sapply(modelStages,function(s)s$Name)
  } else {
    modelStages <- modelParam_ls$ModelStages
    # TODO: need to set stage$Path
    # TODO: need to read stage$Config into stage$RunParam_ls if Config exists
  }
  if ( !is.list(modelStages) || length(modelStages)==0 ) {
    stop( visioneval::writeLog("No model stages found!",Level="error") )
  }

  # What needs to be in modelStages structure (eventually):
  #   modelStage$Name - Name for "Scenario" run parameter, and also for modelStates list item
  #     At a minimum, to locate sub-directory and visioneval.cnf (containing additional parameters)
  #   modelStage$Dir - StageDir (basename within ModelDir for defs/inputs, and within ResultsDir or
  #     ResultsDir/OutputDir for outputs)
  #   modelStage$Path - Absolute path to stage inputs/defs (== ModelDir/StageDir)
  #   modelStage$Config - Optional: if present, name of visioneval.cnf; sought relative to ModelDir
  #     Config is useful if the stages visioneval.cnf is all that varies among stages
  #   modelStage$StartFrom - modelStage$Name of some earlier modelStage within this Model
  #     RunParam_ls from that stage forms the basis for this stage
  #   modelStage$RunParam_ls - collected elements for stage (must eventually be complete)
  #   modelStage$Runnable - does this stage have everything it needs?
  #   modelStage$Reportable - Is this a "terminal" stage (not a "StartFrom" for any other stage)?
  #   modelStage$ModelState_ls - present after the stage has been loaded or run
  #     Any "StartFrom" stage must have one of these before the stage starting from it can be run.
  #   modelStage$RunStatus - elevated from modelStage$ModelState_ls$RunStatus
  #     Generally "Initialized", "Loaded", "Running", "Run Complete", or "Run Failed"
  #     modelStage$ModelState_ls will perhaps also contain a "LastMessage" if it failed
  #   modelStage$Results - cached results object for this model stage (if Reportable)

  # Loop through modelStages examining ModelDir/StageDir
  for ( stage_seq in seq_along(modelStages) ) {
    # Build modelState$RunParam_ls
    stage <- modelStages[[stage_seq]]

    # Attempt to set up complete parameters for the model stage
    # If not all are present, the stage is not Runnable (warning)

    # Add stage-specific setting (over-riding model base settings)
    stageParam_ls <- list()
    if ( "Config" %in% names(stage) ) {
      ParamFile <- file.path(modelParam_ls$ModelDir,stage$Config)
      if ( file.exists(ParamFile) ) {
        stageParam_ls <- visioneval::loadConfiguration(ParamFile=ParamFile,override=modelParam_ls)
      }
    }
    if ( length(stageParam_ls) == 0 ) {
      stageParam_ls <- visioneval::loadConfiguration(ParamDir=stage$Path,override=modelParam_ls)
    }

    # If startFrom is defined:
    #   Access its modelStages[[startFrom]]$runParam_ls (use startFrom Stage Name to find)
    #   Pick up parameters needed from base/startFrom stage
    #     ParamPath (overlay from startFrom)
    #     DatastorePath (overlay from startFrom)
    #     InputPath (overlay from startFrom)
    if ( "StartFrom" %in% names(stageParam_ls) ) {
      stage$StartFrom <- stageParam_ls$StartFrom # Should be the name of an earlier stage
      startFrom <- modelStages[[stageParam_ls$StartFrom]]
      if ( is.na(startFrom) ) {
        stop(
          visioneval::writeLog(
            paste("StartFrom stage is not (yet) defined:",stageParam_ls$StartFrom),
            Level="error"
          )
        ) 
      }
      startFrom <= startFrom$RunParam_ls
      ParamPath <- startFrom$ParamPath         # Need not exist, NULL if not present
      DatastorePath <- startFrom$DatastorePath # Need not exist, NULL if not present
      InputPath <- startFrom$InputPath         # Use this as the base InputPath
    } else {
      startFrom <- list()
      stage$StartFrom <- character(0)          # No stage to start from
      ParamPath <- modelParam_ls$ParamPath     # ParamPath providing geo.csv etc
      DatastorePath <- character(0)            # Default is contribute no DatastorePath
      InputPath <- modelParam_ls$InputPath     # InputPath to prepend to stage
    }

    # Set base ParamPath if it is already defined (we get another shot below with loadParamFile)
    if ( ! is.null(ParamPath) ) {
      stageParam_ls$ParamPath <- ParamPath
    }

    # Construct InputPath
    if ( ! is.null(InputPath) ) {
      InputPath <- c( stageParam_ls$InputPath, InputPath ) # Base InputPath located above
    } else {
      InputPath <- stage$Path # cull will filter it out again if it's not present
    }
    stageParam_ls$InputPath <- cullInputPath( # remove duplicates
      InputPath=InputPath,
      InputDir=visioneval::getRunParameter("InputDir",Param_ls=stageParam_ls)
    )

    # Construct DatastorePath
    #   If LoadDatastore flag:
    if ( "LoadDatastore" %in% names(stageParam_ls) ) {
      if ( ! "LoadDatastoreName" %in% names(stageParam_ls) ) {
        if ( "DatastorePath" %in% startFrom ) {
          # If StartFrom stage, set LoadDatastoreName (and eventually copy it)
          stageParam_ls$LoadDatastoreName <- startFrom$DatastorePath[1]
        } else {
          stop(
            visioneval::writeLog(
              paste("LoadDatastore requested, but no LoadDatastoreName provided",Level="error")
            )
          )
        }
      }
      DatastorePath <- character(0) # Path doesn't matter any mroe
      # The actual existence of the loaded/linked Datastore will be verified when the model stage runs
    } else {
      DatastorePath <- startFrom$DatastorePath
    }

    # Prepend ModelDir/ResultsDir/StageDir onto front of DatastorePath
    stageDatastorePath <- file.path(
      modelParam_ls$ModelDir,
      visioneval::getRunParameter("ResultsDir",Param_ls=stageParam_ls),
      stage$Dir
    )
    stageDatastorePath <- normalizePath(stageDatastorePath,winslash="/",mustWork=FALSE)
    stageParam_ls$DatastorePath <- c( stageDatastorePath, DatastorePath)

    # Add stage run_parameters.json (and set ParamPath if it is not already present)
    # run_parameters.json is deprecated, and is expected only to provide "descriptive" parameters
    # e.g. Scenario or Description, possibly Years
    # New style model will set those in StageDir/visioneval.cnf
    stageParam_ls <- visioneval::loadParamFile(Param_ls=stageParam_ls,ModelDir=stage$Path)

    # Finally, override the ModelScriptPath if the current stage has a different ModelScript
    # Replace the ModelScript if stage$Path/ScriptsDir/ModelScript exists
    ScriptName      <- visioneval::getRunParameter("ModelScript",Param_ls=stageParam_ls)
    ScriptsDir      <- visioneval::getRunParameter("ScriptsDir",Param_ls=stageParam_ls)
    ModelScriptPath <- visioneval::getRunParameter("ModelScriptPath",Default=character(0),Param_ls=stageParam_ls)
    for ( scriptDir in c(
      file.path(stage$Path,ScriptsDir),
      file.path(stage$Path),
      file.path(modelParam_ls$ModelDir,ScriptsDir),
      file.path(modelParam_ls$ModelDir)
    ) ) {
      ScriptPath <- file.path(scriptDir,ScriptName)
      if ( file.exists(ScriptPath) ) {
        ModelScriptPath <- normalizePath(ScriptPath,winslash="/",mustWork=FALSE)
        break
      }
    }
    # TODO: could set path and parse up at modelParam_ls if there is a script at that level
    #       let the script file and parsed script be propagated down into the Stage
    stageParam_ls$ModelScriptPath <- ModelScriptPath
    stageParam_ls$ParsedScript <- visioneval::parseModelScript(stageParam_ls$ModelScriptPath)
    if ( ! "DatastoreType" %in% names(stageParam_ls) ) {
      # Force DatastoreType to be explicit
      stageParam_ls$DatastoreType <- visioneval::getRunParameter("DatastoreType",Param_ls=stageParam_ls)
    }

    # Check if stage can run (enough parameters to run visioneval::loadModel and visioneval::prepareModelRun)
    missingParameters <- visioneval::verifyModelParameters(stageParam_ls)
    stage$Runnable <- length(missingParameters) == 0
    if ( ! stage$Runnable ) {
      visioneval::writeLog(
        c(
          paste("Candidate stage",stage$Name,"is not Runnable"),
          paste("Missing",missingParameters,collapse=", ")
        ),
        Level="warn"
      )
    }
    # TODO: Check if the ModelStatePath exists and if so, load it

    # return stageParam_ls
    stage$RunParam_ls <- stageParam_ls
    modelStages[[stage_seq]] <- stage
  }

  # Prepare the modelStages (but leave out the ones that can't run)
  modelStages <- modelStages[ sapply(modelStages,function(s) s$Runnable) ]

  # Process set of modelStages
  stageCount <- length(modelStages)

  # Model won't open if there is not at least one runnable stage
  if ( !is.list(modelStages) || stageCount == 0 ) {
    stop(
      visioneval::writeLog("Model has no runnable stages!",Level="error")
    )
  }
  if ( stageCount == 1 ) {
    # Elevate the single stage run parameters into the model parameters
    # Simplifies compatibility with classic single-stage models
    # e.g. ve.model.run looks at modelParam_ls to find SaveDatastore
    # Single stage model will ignore stage$Dir when constructing results or outputs
    # StageDir will still be used for InputPath
    model_ls$modelParam_ls <- modelStages[[1]]$RunParam_ls
  }

  # Put names on Stages and identify reportable stages
  stageNames <- sapply(modelStages,function(s) s$Name)  # Vector of stage names
  names(modelStages) <- stageNames
  startsFrom <- sapply( modelStages,function(s) ifelse("StartFrom" %in% names(s),s$StartFrom,"") )
  reportable <- ! stageNames %in% startsFrom
  for ( r in 1:stageCount ) {
    modelStages[[r]]$Reportable <- reportable[r]
    modelStages[[r]]$RunStatus <- codeStatus("Initialized")
  }
  model_ls$modelStages <- modelStages

  return( model_ls )
}

# Initialize a VEModel from modelPath
# modelPath may be a full path, and will be expanded into known model directories
#  if it is a relative path.
ve.model.init <- function(modelPath, log="error") {

  # Load system model configuration
  visioneval::initLog(Save=FALSE,Threshold=log)

  # Opportunity to override names of ModelState, run_model.R, Datastore, etc.
  # Also to establish standard model directory structure (inputs, results)

  # Identify the run_model.R root location(s)
  # Also, update self$RunParam_ls with model-specific configuration
  visioneval::writeLog(paste("Finding",modelPath),Level="info")
  model_ls <- findModel(modelPath)
  self$modelName <- model_ls$modelName
  self$modelPath <- model_ls$modelPath
  self$modelStages <- model_ls$modelStages
  self$RunParam_ls <- model_ls$modelParam_ls

  # Validity: Do we have a runnable model?
  private$p.valid <- is.list(self$modelStages) && length(self$modelStages)>0
  if ( private$p.valid ) {
    self$status <- codeStatus("Initialized")
  } else {
    self$status <- codeStatus("Incomplete Setup")
  }

  return(FALSE)
}

################################################################################
#                      Model Management: copy, rename, dir                     #
################################################################################

ve.model.copy <- function(newName=NULL,newPath=NULL,copyResults=TRUE) {
  # Copy the current model to NewName (in ModelDir, unless newPath is also provided)
  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$printStatus()),level="error")
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
    }
  }

  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- getUniqueName(newPath,newName)
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)

  dir.create(newModelPath,showWarnings=FALSE)
  model.files <- self$dir(root=TRUE,results=copyResults)
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

ve.model.rename <- function(Model=NULL,Scenario=NULL,Description=NULL,Save=TRUE) {
  # Change description for a model that has already been run under another name
  # Reports, but does not change, the underlying configuration file, so the
  # description changes will be lost if the model is re-run...

  # Need a valid model
  if ( ! private$p.valid ) return(self)

  # Prepare update list
  upd <- list(Model=Model,Scenario=Scenario,Description=Description)
  upd <- upd[!sapply(upd,is.null)]
  if ( length(upd)==0 ) return(self) # Nothing to do

  # Update model name in the object, if Model is in upd
  if ( "Model" %in% names(upd) ) self$modelName <- upd$Model;

  # Update self$ModelState, which is a named list of ModelStates (for each stage)
  # Check self$stageCount and warn if any element of upd is not same length as self$ModelState
  lens <- which(sapply(upd,length)!=self$stageCount)
  if ( length(lens)>0 ) {
    msg <- paste0("Replacement length does not match stage count (",self$stageCount,") for ",paste(names(upd)[lens],collapse=","))
    visioneval::writeLog(msg,Level="error")
    stop(msg)
  }

  # Update ModelState Model/Scenario/Description for each stage
  for ( i in 1:self$stageCount ) {
    # Get the model state for stage i
    ms <- self$ModelState[[i]]
    up <- sapply(upd,function(x)x[i])
    ms[names(up)] <- up;

    # Locate previous source for model description
    prev.src <- (attr(ms$RunParam_ls,"source")[names(up)])[1]
    if ( is.na(prev.src) ) {
      stop(
        visioneval::writeLog(paste("Program bug: No source for",names(up.src),sep=" ",collapse=","),Level="error")
      )
    }
    
    # Log a message stating the original configuration file
    visioneval::writeLog("Overriding model description originally from:",Level="warn")
    visioneval::writeLog(prev.src,Level="warn")

    # Update RunParam_ls in ModelState
    up.src <- visioneval::addParameterSource(up,"Manual Rename")
    ms$RunParam_ls <- visioneval::mergeParameters( ms$RunParam_ls, up.src )
    self$ModelState[[i]] <- ms;

    # Save changes to ModelState
    if ( Save ) {
      up["RunParam_ls"] <- ms$RunParam_ls;
      # locate model state to rewrite
      msfile <- file.path(self$resultspath,visioneval::getModelStateFileName(Param_ls=ms$RunParam_ls))
      # Update model state file if it exists (making the change "permanent")
      # However, we're not updating the original configuration, so the change will be lost
      #  if the model is run again.
      if ( file.exists(msfile) ) {
        visioneval::setModelState(up,Filename=msfile)
        visioneval::writeLog(paste("Saving to ModelState:",msfile),Level="warn")
      } else {
        visioneval::writeLog("No existing model state to update",Level="warn")
      }
    }
  }
  
  return( self ) # Will print model if running interactively
}

# Archive results directory
ve.model.archive <- function(SaveDatastore=TRUE) {
  failToArchive <- archiveResults(
    RunParam_ls=self$RunParam_ls,
    RunDir=file.path(self$modelPath,visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)),
    SaveDatastore=SaveDatastore
  )
  if ( length(failToArchive)>0 ) {
    visioneval::writeLog(paste0("Failed to archive results (",paste(failToArchive,collapse=","),")"),Level="error")
    return(FALSE)
  }
  return(TRUE)
}

# Provide a listing of key model components (respecting stages, BaseModel, etc.)
# Collapse model results into a single model run entry (though there are typically three pieces:
# the ModelState.Rda, the Datastore, and log*.txt). Find current results, as well as archived
# results from past model runs.
# "shorten" is a logical parameter - if true, strip off the "ve.runtime" part of any paths, so what
#   is shown is relative to ve.runtime.
# Parameters control elements of the display (if all are FALSE, make them all TRUE):
#   root==TRUE   : show "root" elements (config, run_model.R)
#   inputs=TRUE  : show files in "inputs" and "defs" locations for the model
#   results=TRUE : show result sets
#   outputs=TRUE : show "outputs" (extracts and query results)
# all.files is a logical parameter indicating whether to print (TRUE) all file name (e.g. the 50-something
#   input file names) or (FALSE) just the model directory(root), input directories
#   (InputPath+InputDir) or date of data set (Current, Timestamp) for results, and for outputs,
#   show the contents of the extract directories rather than just the subdirectory name
# TODO: the all.files functionality is not implemented.
ve.model.dir <- function(pattern=NULL,stage=NULL,root=FALSE,results=FALSE,outputs=FALSE,inputs=FALSE,all.files=TRUE,shorten=TRUE) {
  # We're going to search up contents of these directories
  #   self$modelPath (root)
  #   self$modelPath/ResultsDir (results)
  #   self$modelPath/ResultsDir/self$stagePaths[stage] (results)
  #   self$modelPath/InputDir (inputs)
  #   self$modelPath/self$stagePaths[stage]/InputDir (inputs)
  #   self$modelPath/ParamDir (inputs)
  #   self$modelPath/self$stagePaths[stage]/ParamDir (inputs)
  #   self$modelPath/OutputDir (outputs)
  #   self$modelPath/OutputDir/query (outputs)
  # If none root/results/outputs/inputs is TRUE, then all are TRUE
  #   Otherwise, only report the ones actually asked for
  validDir <- dir.exists(self$modelPath)
  if ( ! private$p.valid || ! validDir ) {
    return("No model found.")
    private$p.valid <- FALSE
  }
  
  inputDetails <- if ( ! missing(inputs) ) inputs else FALSE
  if ( all(missing(root),missing(results),missing(outputs),missing(inputs)) ) {
    root <- results <- outputs <- inputs <- TRUE
  }

  if ( missing(shorten) || shorten ) shorten <- self$modelPath
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
  ResultsDir <- normalizePath(
    file.path(self$modelPath,visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)),
    winslash="/",mustWork=FALSE
  )
  if ( outputs ) {
    outputPath <- file.path(
      ResultsDir,(OutputDir <- visioneval::getRunParameter("OutputDir",Param_ls=self$RunParam_ls))
    )
    outputFiles <- dir(normalizePath(outputPath,winslash="/",mustWork=FALSE),full.names=TRUE,recursive=all.files)
  } else outputFiles <- character(0)
  ResultsInRoot <- ( root && ResultsDir==self$modelPath )
  if ( results || ResultsInRoot  ) {
    # Handle the old-style case where ResultsDir==modelPath
    # ResultsDir is already normalized
    # We're only going to look for known result types ("artifacts")
    resultPath <- c(
      ResultsDir,
      file.path(ResultsDir,self$stagePaths[stage])
    )
    resultPath <- normalizePath(resultPath,winslash="/",mustWork=FALSE)
    mstates <- dir(resultPath,pattern="^ModelState(_[[:digit:]]{4}-.*)*\\.Rda$",full.names=TRUE)
    dstores <- dir(resultPath,pattern="^Datastore(_[[:digit:]]{4}-.*)*$",full.names=TRUE)
    logs    <- dir(resultPath,pattern="Log(_[[:digit:]]{4}-.*)+\\.txt",full.names=TRUE)
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
  if ( nzchar(shorten) ) files <- sub(paste0(shorten,"/"),"",files,fixed=TRUE)
  return(files)
}

# Function to interactively remove prior model runs or extracts
ve.model.clear <- function(force=FALSE,outputOnly=NULL,stage=NULL,show=10) {
  # Remove outputs and/or results, either interactivel or in batch
  # 'show' controls maximum number of outputs to display
  # Can limit to outputs in a certain 'stage'
  # outputOnly will show results as well as outputs for deletion;
  #   if outputs exist, we only show those by default
  # force says "just go ahead and delete everything", respecting outputOnly,
  #   so the default is to delete the outputs and leave the results, but if
  #   called a second time, will delete the results.

  if ( ! private$p.valid ) {
    visioneval::writeLog(self$printStatus(),Level="error")
    return( invisible(FALSE) )
  }
  
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
  return(invisible(force))
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

# List the model contents, using parsedScript and specSummary from ve.model.load
ve.model.list <- function(inputs=FALSE,outputs=FALSE,details=NULL,stage=character(0)) {
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
    visioneval::writeLog(paste0("Invalid model: ",self$printStatus()),level="error")
    return( invisible(data.frame()) )
  }

  # Update specSummary
  if ( is.null(self$specSummary) ) {
    visioneval::writeLog("Loading model specifications (may take some time)...",Level="warn")
    self$load(ifExists=FALSE) # Create new model states if they are not present in the file system
    for ( stage in self$modelStages ) {
      AllSpecs_ls <- stage$ModelState_ls$AllSpecs_ls
      if ( ! is.null( AllSpecs_ls ) ) {
        specFrame <- summarizeSpecs(AllSpecs_ls,stage$Name)
        if ( is.null(self$specSummary) ) {
          self$specSummary <- specFrame
        } else {
          rbind(self$specSummary,specFrame)
        }
      }
    }
  }

  # which rows to return
  print(class(self$specSummary))
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

# Helper
# List unique sources in a parameter list
uniqueSources <- function(stage) {
  sources <- attr(stage$RunParam_ls,"source")
  if ( is.null(sources) ) {
    sources <- "NULL"
  } else {
    sources <- unique(sources$Source)
  }
  return(sources)
}

# Print a summary of a model stage
printModelStage <- function(self,stage,details=FALSE) {
  cat("Stage:",stage$Name,"(Reportable:",stage$Reportable,")\n")
  cat("Status:",self$printStatus(),"\n")
  if ( details ) {
    cat("   Starts from:",stage$StartFrom,"\n")
    cat("   Configurations:\n")
    cat(paste("   ",uniqueSources(stage),"\n")) # Generates one row for each unique source
  }
}

# Print a summary of the VEModel, including its run status
ve.model.print <- function(details=FALSE) {
  cat("Model:",self$modelName,"\n")
  if ( details ) {
    cat("Path:","\n")
    cat(self$modelPath,"\n")
  }
  cat("Status:", self$printStatus(),"\n")
  if ( private$p.valid ) {
    cat("Model Stages:\n")
    for ( s in self$modelStages ) {
      printModelStage(self,s,details)
    }
  }
  private$p.valid
}

ve.model.log <- function() {
  ms <- self$ModelState[[self$stageCount]]
  if ( is.null(ms) ) return("")
  logfile <- file.path(self$resultspath(),ms$LogFile) # log file is saved in ResultsDir/stagePath
  if ( is.null(logfile) || is.na(logfile) ) return("")
  return(logfile)
}

# Ordered Factor for RunStatus levels

StatusLevelCodes <- c(
  "Unknown",
  "Uninitialized",
  "Run Failed",
  "Load Failed",
  "Incomplete Setup",
  "Initialized",
  "Loaded",
  "Running",
  "Run Complete"
)

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
  if ( ! is.integer(status) || status < 1 || status > length(StatusLevelCodes) ) {
    status <- codeStatus("Unknown")
  }
  return( StatusLevelCodes[status] )
}

# Show "minimum" stage status (see StatusLevelCodes)
ve.model.updateStatus <- function() {
  self$status <- min(sapply(self$modelStages,function(s) s$RunStatus))
}

# Helper function:
#    Key function here is to create an in-memory copy of each stage's ModelState_ls
#      either by loading, or by initializing
#    Also need to handle the StartFrom earlier model stage (where to find the
#      model state that goes into DatastorePath, as well as the model state to
#      interrogate for VERequiredPackages).
#  Resulting structure can be explored for model inputs and outputs, script, etc.

X# Two needs:
#   1. Load existing model states if stages have been run before
#   2. Build new model states if not running to parse the model script and check that
#      everything is where it needs to be - unintrusive
# Second step will not be performed if "ifExists" is TRUE (the default)

ve.model.load <- function(runStages=character(0),ifExists=TRUE) {

  if ( ! self$valid() ) {
    stop(
      visioneval::writeLog("Model is incomplete and cannot be loaded.",Level="error")
    )
  }

  # identify stages to load
  if ( length(runStages)>0 ) { # list of names of stages to run
    firstStage <- which( names(self$modelStages) %in% runStages )
    if ( length(firstStage) > 0 ) firstStage <- firstStage[1] else firstStage <- 1
  } else {
    firstStage <- 1
  }
  loadStages <- firstStage:length(self$modelStages)

  # Load or Create the ModelState_ls for each stage if not already loaded
  for ( index in loadStages ) {
    stage <- self$modelStages[[index]]
    if ( ! "ModelState_ls" %in% names(stage) ) {
      envir = new.env()
      envir$RunModel <- FALSE
      envir$Owner <- "ve.model.load"
      ms <- visioneval::loadModel(stage$RunParam_ls,ifExists=ifExists,envir=envir)
      if ( is.list(ms) && length(ms)>0 ) { # Save the ModelState if created successfully
        stage$ModelState_ls <- ms
        stage$RunStatus <- stage$ModelState_ls$RunStatus
        self$modelStages[[index]] <- stage
      } else {
        stage$RunStatus <- NULL
      }
      if ( is.null(stage$RunStatus) ) {
        stage$RunStatus <- codeStatus("Load Failed")
      }
    }
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

# Run the modelStages
ve.model.run <- function(run="continue",stage=NULL,log="warn") {
  # TODO: rework for the new Stage architecture
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
    visioneval::writeLog(paste0("Invalid model: ",self$printStatus()),Level="error")
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
    self$load(RunModel=FALSE,ifExists=TRUE) # Open any existing ModelState_ls to see which may be complete
    alreadyRun <- ( sapply( self$modelStages, function(s) s$RunStatus ) == codeStatus("Run Complete") )
    if ( all(alreadyRun) ) {
      self$status <- codeStatus("Run Complete")
      visioneval::writeLog("Model has been run.",Level="warn")
      return(invisible(self$status))
    } else {
      toRun <- which(!alreadyRun)[1] # start from first un-run stage
    }
  } else {
    toRun <- 1 # Start at first stage
  }
  if ( toRun == 1 ) run <- "reset" # If starting over, process SaveDatastore as needed

  # Save existing results if we're restarting or resetting
  SaveDatastore = NULL # ignore any pre-configured value for SaveDatastore
  if ( run == "restart" || run=="reset" ) {
    SaveDatastore <- FALSE
    visioneval::writeLog(paste("Removing previous Results from",workingResultsDir),Level="info")
    unlink(dir(workingResultsDir,full.names=TRUE),recursive=TRUE)
  } else if ( run == "save" ) {
    SaveDatastore <- TRUE
  } else {
    SaveDatastore <- visioneval::getRunParameter("SaveDatastore",Param_ls=self$RunParam_ls)
  }

  if ( run != "continue" && SaveDatastore && dir.exists(workingResultsDir) ) {
    # Archive previous results if SaveDatastore true in RunParam_ls and previous results exist
    archiveErrors <- archiveResults(RunParam_ls=self$RunParam_ls,RunDir=workingResultsDir)
    if ( length(archiveErrors)>0 ) {
      visioneval::writeLog(
        paste0("Failed to save prior results (",paste(archiveErrors,collapse=","),").\nContinuing anyway..."),
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
    msg <- visioneval::writeLog(paste("Starting stage",toRun,"comes after last stage",length(runStages)),Level="error")
    stop(msg)
  }
  runStages <- runStages[toRun:length(runStages)]

  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- normalizePath(file.path(self$modelPath,BaseInputPath),winslash="/",mustWork=FALSE)
  }

  owd <- getwd()
  on.exit(setwd(owd))

  # If saving Datastore, archive the Datastore
  LogLevel <- getRunParameter("LogLevel",Default=log,Param_ls=self$RunParam_ls)

  # Set up the model runtime environment
  for ( ms in runStages ) { # iterate over names of stages to run
    # Mark local status on the stage
    self$modelStages[[ms]]$RunStatus <- codeStatus("Running")
    # TODO: add PID below if running in background process

    # Remove any existing ModelState and run results from this stage
    stage <- self$modelStages[[ms]]
    stage[["ModelState_ls"]] <- NULL
    stage[["Results"]] <- NULL # Rebuild on demand via ve.model.results

    # Construct and change to working directory
    RunDir <- normalizePath(file.path(workingResultsDir,stage$Dir),winslash="/",mustWork=FALSE)
    if ( ! dir.exists(RunDir) ) dir.create(RunDir)
    setwd(RunDir)

    # Execute the run sequence (may print non-logged warnings...)
    # TODO: turn warnings into log messages (and use the log to echo them to the screen,
    #       not the default function).
    runResults <- try (
      {
        # Take ownership of ve.model
        ve.model <- visioneval::modelEnvironment(Clear="VEModel::run") # add Owner

        # Initialize Log, create new ModelState
        ve.model$RunModel <- TRUE
        visioneval::initLog(Threshold=LogLevel,Save=TRUE) # Log stage
        visioneval::loadModel(stage$RunParam_ls)          # Create new ModelState_ls (ignore existing)
        visioneval::setModelState()                       # Save ModelState.Rda
        visioneval::prepareModelRun()                     # Initialize Datastore

        # Run the model script
        sys.source(stage$ModelScriptPath,envir=new.env())

        # If we get this far without a "stop", save the ModelState and RunStatus
        RunStatus <- codeStatus("Run Complete")
        list(ModelState_ls=ve.model$ModelState_ls,RunStatus=RunStatus)
      },
      silent=TRUE
    )

    # Return to base working directory
    setwd(owd)

    # Process results (or errors)
    if ( ! is.list(runResults) ) {
      # Failure: stop trapped while performing run sequence
      msg <- as.character(runResults) # try-error (captures "stop" message)
      visioneval::writeLog(msg,Level="error") # possibly redundant with interior logging
      RunStatus <- codeStatus("Run Failed")
      self$modelStages[[ms]]$RunStatus <- RunStatus
      if ( "ModelState_ls" %in% visioneval::modelEnvironment() ) {
        visioneval::setModelState(list(RunStatus=RunStatus))
      }
    } else {
      # Success: Assemble the runResults
      self$modelStages[[ms]]$ModelState_ls <- runResults$ModelState_ls # The story of the run...
      self$modelStages[[ms]]$RunStatus <- runResults$RunStatus
      visioneval::setModelState(list(RunStatus=runResults$RunStatus))
    }
  }

  # Update overall model status
  self$updateStatus() # "Worst"/Lowest RunStatus for the overall model

  return(invisible(self$status))
}

################################################################################
#                              Model Configuration                             #
################################################################################

# TODO: clean up the "set" function. Need to be able to do the following:
# Perhaps rename to "Settings"
#   - Report global configuration ("Global")("All Models")
#   - Report model-wide configuration ("Model")("All Stages")
#   - Report stage configuration ("Stages")
# Make interactive changes to a model's settings files (or edit externally in text editor)
# Probe around at the different levels

# Function to inspect the model configuration/setup parameters
# With no arguments, reports name/value pair for values explicitly defined
# The return is the new set of named values (using default or other show/search arguments)
ve.model.set <- function(show="values", src=NULL, namelist=NULL, pattern=NULL,stage=NULL,Param_ls=NULL) {
  # "show" is a character vector describing what to return (default, "values" defined in self$RunParam_ls)
  #   "name" - names of settings (character vector of matching names)
  #   "value" - named list of settings present in self$RunParam_ls (or defaults) (DEFAULT)
  #   "source" - return the source of the parameter (plus name or value if also requested)
  # "show" also describes where to look
  #   "runtime" - use just the list from VEModel::getSetup() ignoring the model-specific settings
  #       (optionally include undefined defaults if "defaults" is also included in "show")
  #   "defaults" - include the list of parameter defaults as if they were defined
  #       (overridden by any that actually are defined)
  #   "self" - (DEFAULT) returns the constructed model specification
  #       (optionally including undefined defaults if "defaults" in "show)
  #       (if "self" and "runtime", use "self")
  #       If model is not valid, falls back to "runtime" as default
  #   If only one of "source", "name", "value":
  #      return a named vector (parameter names; corresponding element)
  #   If both "source" and "value":
  #      return a data.frame (row.names == "name")
  #   If both "source" (or "value") and "name":
  #      return a data.frame with a "name" column plus either source or value (row.names==names)
  #   If all of "source", "name", "value":
  #      return a data.frame of all three (row.names==names)
  # If src is a character vector, find the parameter sources that match any of the elements
  #   as a regular expression.
  # If pattern is a character vector, look for names in the setup that match any of the
  #  vector elements (as regular expressions)
  # If namelist is a character vector, treat each item as a name and return the list of items from
  #  the model setup (but dip into the full hierarchy, unless "src" is also set)
  # If stage is set, look into the configuration for that stage name. If not set, and there is
  #  a ModelStages element in Param_ls, look in all the stages as well (and prepend the stage name
  #  onto the reported setting... How to make that work...

  if ( is.null(Param_ls) ) {
    where.options <- c("defaults","self","runtime")
    if ( ! private$p.valid ) {
      where.options <- where.options[-2] # Can't seek self if invalid
    }
    where.to.look <- where.options %in% show;
    names(where.to.look) <- where.options;
    if ( ! any(where.to.look) ) {
      if ( private$p.valid && "self" %in% names(where.to.look) ) {
        where.to.look["self"] <- TRUE
      } else {
        where.to.look["runtime"] <- TRUE
      }
    }
    where.to.look <- where.options[where.to.look]

    searchParams_ls <- list()
    if ( "defaults" %in% where.to.look ) {
      # defaults, possibly plus self
      if ( "self" %in% where.to.look ) in.self <- self$RunParam_ls else in.self <- list()
      searchParams_ls <-visioneval::mergeParameters(searchParams_ls,visioneval::defaultVERunParameters(in.self))
    }
    if ( "runtime" %in% where.to.look ) { # only self
      searchParams_ls <- visioneval::mergeParameters(searchParams_ls,getSetup())
    } else {
      searchParams_ls <- visioneval::mergeParameters(searchParams_ls,self$RunParam_ls)
    }
  } else {
    searchParams_ls <- Param_ls # search in command line structure
  }

  if ( is.character(src) ) {
    # search for matching patterns in searchParams_ls source attribute
    sources <- attr(searchParams_ls,"source")
    matchsrc <- unique(sapply(src, function(s) sapply(sources,grep,pattern=s)) )
  } else {
    matchsrc <- integer(0)
  }
  if ( is.character(namelist) ) {
    # Slightly faster than using pattern (no "grep" step)
    matchname <- which(names(searchParams_ls) %in% namelist)
  } else {
    matchname <- integer(0)
  }
  if ( is.character(pattern) ) {
    nms <- names(searchParams_ls)
    matchpat <- unique(sapply(src, function(s) sapply(nms,grep,pattern=s)) )
  } else {
    matchpat <- integer(0)
  }
  matching <- unique(c(matchsrc,matchname,matchpat))
  if ( is.null(matching) || length(matching)==0 ) {
    sought <- searchParams_ls
  } else {
    sought <- searchParams_ls[matching]
  }

  # If parameters are provided, override the model's run parameters with them.
  if ( ! is.null(Param_ls) ) {
    self$RunParam_ls[ names(sought) ] <- sought
  }

  return.options <- c("name","value","source")
  what.to.return <- return.options %in% show
  if ( ! any(what.to.return) ) what.to.return <- c(FALSE,TRUE,FALSE) # just values

  results <- data.frame( # columns in same order as return.options
    Parameter = names(sought),
    Value     = sought,
    Source    = attr(sought,"source")
  )
  results <- results[,what.to.return] # logical indexing into columns
  if ( is.data.frame(results) ) {
    row.names(results) <- results$Parameter
  } else {
    names(results) <- names(sought) # One column, so data.frame drops to vector
  }

  return(results)
}

ve.model.save <- function(FileName="visioneval.cnf") {
  # Write self$RunParam_ls into file.path(self$modelPath,FileName) (YAML format).
  # Do not include default values or those in the runtime setup
  # Use the "set" function to limit what is shown.
  # Can provide alternate name (but if it's not part of the known name set, it's 'just for
  # reference')
  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),level="error")
    return( invisible(list()) )
  }

  normalizePath(file.path(self$modelPath,FileName),winslash="/",mustWork=FALSE)
  yaml::write_yaml(self$RunParam_ls,FileName,indent=2)
  invisible(self$RunParam_ls)
}

# Report the model results path (used to creat a VEResults object and
# to retrieve the LogFile
ve.model.resultspath <- function(stage,Param_ls=NULL) {
  if ( missing(stage) || !is.numeric(stage) ) {
    stage <- self$stageCount
  } else if ( length(stage)>1 ) {
    stage <- stage[length(stage)] # use last one listed
  }
  stagePath <- self$stagePaths[stage]
  visioneval::writeLog(paste("Loading Results for Model Stage:",stagePath),Level="info")
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=Param_ls)
  if ( ! dir.exists( file.path(self$modelPath,ResultsDir) ) ) {
    ResultsDir = "."
  } # Old style model run won't have ResultsDir in Param_ls, nor the right default
  resultsPath <- normalizePath(   # may be NULL!
    file.path(self$modelPath,ResultsDir,stagePath),
    winslash="/",
    mustWork=FALSE
  );
  return(resultsPath)
}

################################################################################
#                                Model Results                                 #
################################################################################

# create a VEResults object (possibly invalid/empty) based on the current model
#   run or a particular model stage.
ve.model.results <- function(stage) {
  # Create a results object wrapping the directory that contains the model
  # results for the given stage (or last stage if not given)
  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),level="error")
    return( NULL )
  }

  if ( missing(stage) || !is.numeric(stage) ) {
    stage <- self$stageCount
  } else if ( length(stage)>1 ) {
    stage <- stage[length(stage)] # use last one listed
  }

  Param_ls <- self$ModelState[[stage]]$RunParam_ls
  resultsPath <- self$resultspath(stage,Param_ls=Param_ls)
  results <- VEResults$new(resultsPath)
  if ( ! results$valid() ) {
    private$lastResults <- list()
    if (stage!=self$stageCount) {
      visioneval::writeLog("There are no results for stage ",stage," of this model yet.",Level="warn")
    } else {
      visioneval::writeLog("There are no results for this model yet.",Level="warn")
    }
  } else {
    private$lastResults <- list(
      results=results,
      stage=stage
    )
  }
  return(results)
}

# open a Query object for the model from its QueryDir (or report a list
#  of available queries if no QueryName is provided).
ve.model.query <- function(QueryName=NULL,FileName=NULL,load=TRUE) {
  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),level="error")
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
    queries <- dir(QueryPath,pattern="\\.VEqry$",ignore.case=TRUE)
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
#                           VEModel Class Definition                           #
################################################################################

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,                         # Model identifier
    modelPath=NULL,                         # Also as RunParam_ls$ModelDir
    modelStages=NULL,                       # list of runnable stages
    RunParam_ls=NULL,                       # Run parameters from the model root
    specSummary=NULL,                       # List of inputs, gets and sets from master module spec list  
    status=codeStatus("Uninitialized"),     # Where are we in opening or running the model?

    # Methods
    initialize=ve.model.init,               # initialize a VEModel object
    valid=function() private$p.valid,       # report valid state
    load=ve.model.load,           # load the model state for each stage (slow - defer until needed)
    run=ve.model.run,                       # run a model (or just a subset of stages)
    rename=ve.model.rename,                 # Change model Name, Scenario, Description
    print=ve.model.print,                   # provides generic print functionality
    printStatus=ve.model.printStatus,       # turn integer status into text representation
    updateStatus=ve.model.updateStatus,     # fill in overall status based on individual stage status
    list=ve.model.list,                     # interrogator function (script,inputs,outputs,parameters
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    log=ve.model.log,                       # report the log file path (use e.g. file.show to display it)
    set=ve.model.set,                       # set or list model parameters and their sources
    save=ve.model.save,                     # save changes to the model setup that were created locally (by source)
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    archive=ve.model.archive,               # apply framework archive function if results exist
    results=ve.model.results,               # Create a VEResults object (if model is run); option to open a past result
    resultspath=ve.model.resultspath,       # Report the path to the model results for a stage
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
    return( VEModel$new(modelPath = modelPath,log=log) )
  }
}

##########################################################################################
#                      MANAGE "STANDARD MODELS" (VEModel Examples)                       #
##########################################################################################

## Look up a standard model
#  'model' is bare name of standard model
#  returns the full path to that model template
findStandardModel <- function( model, variant="" ) {
  standardModels <- system.file("models",package="VEModel")
  if ( ! nzchar(standardModels) ) {
    # The following is for testing purposes...
    standardModels <- getOption("VEStandardModels",default=normalizePath("inst/models"))
  }
  if (missing(model) || is.null(model) || ! nzchar(model)) {
    return( c("Available Models:",dir(standardModels)) )
  }

  # Locate the model
  model <- model[1]
  model_ls <- list()
  model_ls$ModelDir <- file.path(standardModels,model)
  if ( ! dir.exists(model_ls$ModelDir) ) {
    visioneval::writeLog(paste("No standard model called ",model),Level="error")
    return( findStandardModel("") ) # recursive call to keep return directory in one place
  }

  # Read the model index to identify variants ("base" should always exist)
  confPath <- file.path(model_ls$ModelDir,"model-index.cnf")
  modelIndex <- try( yaml::yaml.load_file(confPath) )
  if ( ! "variants" %in% names(modelIndex) ) {
    visioneval::writeLog(paste0("No model variants defined in ",confPath),Level="error")
    visioneval::writeLog(c(class(modelIndex),names(modelIndex)),Level="error")
    return(c("No model variant:",variant))
  }
  if ( missing(variant) || ! nzchar(variant) || ( ! variant %in% names(modelIndex$variants) ) ) {
    variantMsg <- c(paste0("Available variants for ",model,":"),names(modelIndex$variants))
    if ( nzchar(variant) ) {
      variantMsg <- c(paste0("Unknown variant '",variant,"'"),variantMsg)
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
installStandardModel <- function( modelName, modelPath, confirm, variant="base", log="error" ) {
  # Locate and install standard modelName into modelPath
  #   modelName says which standard model to install. If it is missing or empty, return a
  #     list of available models
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first
  #     available root (see VEModel::getModelRoots function)
  #   Otherwise, modelPath is presumed to be the directory into which modelName will be installed
  #     (and basename(modelPath) will become the target model name, disambiguated).
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and
  #     they have to try again

  visioneval::initLog(Save=FALSE,Threshold=log)
  
  model <- findStandardModel( modelName, variant )
  if ( ! "Description" %in% names(model) ) model$Description <- paste(modelName,variant,sep="-")
  if ( ! is.list(model) ) {
    return(model) # Expecting a character vector of available information about standard models
  } # Otherwise model is a list with details on the model we need to install

  # Set up destination modelPath (always create in the first defined root)
  root <- getModelRoots(1)
  if ( missing(modelPath) || is.null(modelPath) ) modelPath <- model$Name
  if ( ! isAbsolutePath(modelPath) ) {
    installPath <- normalizePath(file.path(root,modelPath),winslash="/",mustWork=FALSE)
  }
  if ( dir.exists(installPath) ) {
    installPath <- getUniqueName( dirname(installPath), basename(modelPath) )
  }

  # Confirm installation if requested
  install <- TRUE
  if ( confirm && interactive() ) {
    msg <- paste0("Install standard model '",model$Name,"' into ",installPath,"?\n")
    install <- confirmDialog(msg)
  }

  if ( ! install ) stop("Model ",modelName," not installed.",call.=FALSE)

  # Create the installation directory
  visioneval::writeLog(paste("Installing from",model$ModelDir),Level="info")
  visioneval::writeLog(model$Description,Level="info")
  visioneval::writeLog(paste("Installing into",installPath),Level="info")
  dir.create(installPath)

  # Process the model directories
  for ( subdir in model$Directories ) {
    from.dir <- subdir$From
    to.dir <- file.path(installPath,subdir$To)
    if ( ! dir.exists(from.dir) ) {
      visioneval::writeLog(paste0("Searching ",from.dir),Level="info")
      visioneval::writeLog(msg<-paste0("No variant (",variant,") for ",modelName),Level="error")
      visioneval::writeLog(c("Directory missing:",from.dir),Level="error")
      stop(msg)
    }
    if ( ! dir.exists(to.dir) ) dir.create(to.dir)
    visioneval::writeLog(paste0("Copying ",subdir$From,"..."),Level="info")
    copy.files <- dir(from.dir,full.names=TRUE)
    file.copy(copy.files,to.dir,recursive=TRUE) # Copy standard model into modelPath
  }

  # Copy the configuration file
  if ( "Config" %in% model ) {
    file.copy(model$Config,file.path(installPath,"visioneval.cnf"))
  } else {
    # Hack for classic model - no Config
    # Elevate content "scripts" directory to installPath then remove scriptPath
    if ( dir.exists( scriptPath<-file.path(installPath,"scripts") ) ) {
      file.copy(dir(scriptPath,full.names=TRUE),installPath)
      unlink(scriptPath,recursive=TRUE)
    }
  }

  visioneval::writeLog(paste0("Installed ",modelName," in ",installPath),Level="info")
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
#' @param log a string describing the minimum level to display
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, variant="base", confirm=TRUE, log="error") {
  model <- installStandardModel(modelName, modelPath, confirm=confirm, variant=variant, log=log)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath, log=log ) )
  } else {
    return( model ) # should be a character vector of information about standard models
  }
}
