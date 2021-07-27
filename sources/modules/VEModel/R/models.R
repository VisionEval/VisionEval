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
#' mdl$findstage(stage)
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
    
    baseModel <- openModel(baseModelName) # Amounts to a recursive call to initialize (does that work?)
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
    baseParam_ls <- visioneval::extractParameters(
      c(
        "InputPath","ModelScriptPath","ParamPath","QueryPath",
        "RunDstore","DatastorePath" # Used to link to BaseModel Datastore (or copy it)
        ),
      Param_ls=baseParam_ls
    )
  } else return(modelParam_ls) # No BaseModel name provided

  # Rebuild model default path parameters (copying from base model as available)

  # If DatastorePath is defined in modelParam_ls, do not transfer it from the BaseModel parameters
  # DatastorePath is a vector paths to Datastores in the BaseModel's stages
  # Define it in the model (possibly to an empty string) if not linking to the BaseModel results
  if ( ! "DatastorePath" %in% existingParams ) {
    modelParam_ls <- visioneval::addRunParameter(
      modelParam_ls,
      DatastorePath=visioneval::getRunParameter("DatastorePath",Param_ls=baseParam_ls,Source=TRUE),
      LoadDstore=visioneval::getRunParameter("RunDstore",Param_ls=baseParam_ls,Source=TRUE)
    )
  }
  
  # If ParamDir defined in modelParam_ls: set ParamPath if ModelDir/ParamDir exists
  if ( "ParamDir" %in% existingParams ) {
    paramPath <- file.path(modelParam_ls$ModelDir,modelParam_ls$ParamDir)
    if ( dir.exists(paramPath) ) {
      modelParam_ls <- visioneval::addRunParameter(
        modelParam_ls,
        Source="ModelDir + (model)ParamDir",
        ParamPath=paramPath
      )
    }
  } else {
    modelParam_ls <- visioneval::addRunParameter(
      modelParam_ls,
      ParamPath=visioneval::getRunParameter("ParamPath",Param_ls=baseParam_ls,Source=TRUE)
    )
  }

  # If InputPath defined in modelParam_ls (even to an empty string), ignore BaseModel
  # Note that ModelDir/InputDir will get added to the path if it exists anyway (no need to define InputPath for that).
  # Individual stage InputPaths will likewise be added to the model's overall InputPath
  if ( ! "InputPath" %in% names(modelParam_ls) ) {
    visioneval::addRunParameter(
      modelParam_ls,
      InputPath=visioneval::getRunParameter("InputPath",Param_ls=baseParam_ls,Source=TRUE)
    )
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
    modelScriptSrc <- "VEModel::findModel"
  } else {
    # WARNING: Be careful if BaseModel is horizontally staged (different ModelScript for each stage)
    # You'll only get the script from the BaseStage extracted from the BaseModel
    # Just define ScriptsDir <- "." to avoid that.
    modelScriptPath <- visioneval::getRunParameter("ModelScriptPath",Param_ls=baseParam_ls,Source=TRUE)
    modelScriptSrc <- attr(modelScriptPath,"source")
  }
  if ( file.exists(modelScriptPath) ) {
    modelParam_ls <- visioneval::addRunParameter(modelParam_ls,Source=modelScriptSrc,ModelScriptPath=modelScriptPath)
  }

  if ( "QueryPath" %in% existingParams && "QueryPath" %in% names(baseParam_ls) ) {
    queryPath <-c( modelParam_ls$QueryPath, baseParam_ls$QueryPath )
    querySrc <- "Base Model + Model"
  } else {
    queryPath <- visioneval::getRunParameter("QueryPath",Param_ls=baseParam_ls,Source=TRUE) # May still not exist
    querySrc <- attr(queryPath,"source")
  }
  if ( is.character(queryPath) && length(queryPath)>=1 ) {
    modelParam_ls <- visioneval::addRunParameter(
      modelParam_ls,
      Source=querySrc,
      QueryPath=queryPath
    )
  } # May still not have a QueryPath

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

  model_ls <- list() # Model elements built from modelDir

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
  modelParam_ls <- visioneval::addRunParameter(Param_ls=modelParam_ls,Source="VEModel::FindModel",ModelDir=modelPath)
  if ( ! "ResultsDir" %in% names(modelParam_ls) ) {
    # Load default parameter or get from larger runtime environment
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      visioneval::getRunParameter("ResultsDir",Param_ls,Source=TRUE)
    )
  }

  # Check for BaseModel and BaseStage in model parameters
  # If present, load key parameters from the BaseModel
  modelParam_ls <- addBaseModel(modelParam_ls)

  # Process InputPath for overall model (culling directories for those actually exist)
  if ( ! "InputPath" %in% names(modelParam_ls) ) {
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      Source="VEModel::findModel",
      InputPath=modelParam_ls$ModelDir
    )
  } else {
    modelParam_ls <- visioneval::addRunParameter(
      Param_ls=modelParam_ls,
      Source="VEModel::findModel",
      InputPath=c( modelParam_ls$ModelDir, modelParam_ls$InputPath )
    )
  }
  modelParam_ls <- visioneval::addRunParameter(
    Param_ls=modelParam_ls,
    Source="VEModel::findModel",
    InputPath=cullInputPath(
      InputPath=modelParam_ls$InputPath,
      InputDir=visioneval::getRunParameter("InputDir",Param_ls=modelParam_ls)
    )
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
    if ( file.exists(ParamPath) ) {
      visioneval::writeLog(paste("Setting ParamPath to",ParamPath),Level="info")
      modelParam_ls <- visioneval::addRunParameter(
        modelParam_ls,
        Source="VEModel::findModel",
        ParamPath=ParamPath
      )
    }
  }

  # Locate ModelScriptPath if it has not yet been set
  if ( ! "ModelScriptPath" %in% names(modelParam_ls) ) {
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
  }
  if ( nzchar(ModelScriptPath) && file.exists(ModelScriptPath) ) {
    modelParam_ls <- visioneval::addRunParameter(
      modelParam_ls,
      Source="VEModel::findModel",
      ModelScriptPath=ModelScriptPath,
      ParsedScript=visioneval::parseModelScript(ModelScriptPath)
    )
  } else {
    modelParam_ls[["ModelScriptPath"]] <- NULL
    # Don't keep ModelScriptPath if there is no ModelScript there!
    # Search for Base Model may have put in a placeholder
  }

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
        VEModelStage$new(
          Dir=stage,                           # Relative to modelPath
          Name=sub("^\\.$",basename(modelParam_ls$ModelDir),stage),  # Will only change root directory
          Path=modelParam_ls$ModelDir,         # Root for stage
          modelParam_ls=modelParam_ls          # Base parameters from model
        )
      }
    )
  } else {
    modelStages <- lapply(modelParam_ls$ModelStages, # Use pre-defined structures
      # At a minimum, must provide Dir or Config
      function(stage) {
        VEModelStage$new(
          modelParam_ls=modelParam_ls,        # Base parameters from model
          Dir=stage$Dir,                      # Relative to modelPath
          Name=stage$Name,                    # Will only change root directory
          Path=stage$Path,                    # Rarely used, but available
          Config=stage$Config                 # Config may
        )
      }
    )
  }
  # If no stages remain, model is invalid
  if ( !is.list(modelStages) || length(modelStages)==0 ) {
    visioneval::writeLog("No model stages found!",Level="error")
    return( model_ls )
  }

  # Loop through modelStages, completing initialization using "StartFrom" stages
  visioneval::writeLog("Finding runnable stages",Level="info")
  runnableStages <- list()
  for ( stage_seq in seq_along(modelStages) ) {
    # Adjust modelState$RunParam_ls
    stage <- modelStages[[stage_seq]]      # stage is a VEModelStage object
    if ( stage$runnable(runnableStages) ) {   # complete stage initialization
      runnableStages <- c( runnableStages, stage )
    }
  }

  # Forget the modelStages that can't run
  modelStages <- runnableStages
  stageCount <- length(modelStages)
  if ( !is.list(modelStages) || stageCount == 0 ) {
    visioneval::writeLog("Model has no runnable stages!",Level="error")
    return( model_ls )
  }

  # Call the modelStages by their Names
  names(modelStages) <- sapply(modelStages,function(s)s$Name)

  # Set Reportable attribute for the stages
  if ( stageCount == 1 ) {
    # Also elevate the single stage run parameters into the model parameters
    # Simplifies compatibility with classic single-stage models
    # e.g. ve.model.run looks at modelParam_ls to find SaveDatastore
    # Single stage model will ignore stage$Dir when constructing results or outputs
    # StageDir will still be used for InputPath
    model_ls$modelParam_ls <- modelStages[[1]]$RunParam_ls
    modelStages[[1]]$Reportable <- TRUE
  } else {
    # Put names on Stages and identify reportable stages
    startFromNames <- unlist(sapply(modelStages,function(s) s$StartFrom))
    startFromNames <- startFromNames[ nzchar(startFromNames) ]
    stageNames <- names(modelStages)
    reportable <- ! stageNames %in% startFromNames
    for ( r in seq_along(stageNames) ) {
      modelStages[[r]]$Reportable <- reportable[r]
    }
  }
  model_ls$modelStages <- modelStages

  return( model_ls )
}

# Initialize a VEModel from modelPath
# modelPath may be a full path, and will be expanded into known model directories
#  if it is a relative path.
ve.model.init <- function(modelPath, log="error") {

  # Opportunity to override names of ModelState, run_model.R, Datastore, etc.
  # Also to establish standard model directory structure (inputs, results)

  # Identify the run_model.R root location(s)
  # Also, update self$RunParam_ls with model-specific configuration
  visioneval::writeLog(paste("Finding",modelPath),Level="info")
  model_ls <- findModel(modelPath)
  if ( length(model_ls) > 0 ) {
    self$modelName <- model_ls$modelName
    self$modelPath <- model_ls$modelPath
    self$modelStages <- model_ls$modelStages
    self$RunParam_ls <- model_ls$modelParam_ls
  }

  # Validity: Do we have a runnable model?
  self$updateStatus()
}

################################################################################
#                      Model Management: copy, rename, dir                     #
################################################################################

# TODO: Copy a model from one modelPath to another
# Use the newName to create the model's new directory
# Update directory locations
#   Change the ModelDir in the model's RunParams_ls
#   Update RunPath for each stage in its RunParams_ls and ModelState_ls if copyResults
#   Identify other elements of ModelState and RunParams that will change if the
#     the ModelDir changes (everything built from ModelDir)
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
    } # else presume newPath will contain newName
  }

  newPath <- normalizePath(newPath)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- getUniqueName(newPath,newName)
  newModelPath <- normalizePath(newModelPath)

  dir.create(newModelPath,showWarnings=FALSE)
  # check that the directory produces the right results files
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

# Archive results directory
ve.model.archive <- function(SaveDatastore=TRUE) {
  failToArchive <- visioneval::archiveResults(
    RunParam_ls=self$RunParam_ls,
    RunDir=file.path(self$modelPath,self$setting("ResultsDir")),
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
#   root==TRUE   : show "root" elements (config, scripts/run_model.R) plus InputPath and ParamPath
#   inputs=TRUE  : show files in "inputs" and "defs" locations for the model
#   results=TRUE : show result sets
#   outputs=TRUE : show "outputs" (extracts and query results)
# if all.files, list inputs and outputs as files, otherwise just directories
ve.model.dir <- function(pattern=NULL,stage=NULL,root=FALSE,results=FALSE,outputs=FALSE,inputs=FALSE,shorten=TRUE, all.files=FALSE) {
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

  # inputDetails will list the files; otherwise only list InputPath
  if ( all(missing(root),missing(results),missing(outputs),missing(inputs)) ) {
    root <- results <- outputs <- inputs <- TRUE
  }

  if ( missing(shorten) || shorten ) shorten <- self$modelPath
  if ( is.null(stage) ) stage<-names(self$modelStages)

  if ( inputs ) {
    inputPath <- file.path(self$setting("InputPath",shorten=FALSE),self$setting("InputDir"))
    for ( stage in self$modelStages ) {
      inputPath <- c(InputPath,
        file.path(
          self$setting("InputPath",stage=stage,shorten=FALSE),
          self$setting("InputDir",stage=stage)
        )
      )
    }
    inputPath <- unique(inputPath)
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
  if ( outputs ) {
    outputPath <- file.path( baseResults,self$setting("OutputDir") )
    outputFiles <- dir(normalizePath(outputPath),full.names=TRUE,recursive=all.files)
    if ( all.files ) {
      outputFiles <- outputFiles[ ! dir.exists(outputFiles) ]
    }
  } else outputFiles <- character(0)

  stagePaths <- character(0)
  for ( stage in self$modelStages ) {
    stagePaths <- c( stagePaths, self$setting("RunPath",stage=stage) )
  }

  ResultsInRoot <- ( root && baseResults==self$modelPath )
  if ( results || ResultsInRoot  ) {
    # Handle the old-style case where ResultsDir==modelPath
    # ResultsDir is already normalized
    # We're only going to look for known result types ("artifacts")
    ResultsDir <- baseResults

    resultPath <- unique( normalizePath(c(ResultsDir,stagePaths)) )
    mstates <- dir(resultPath,pattern="^ModelState(_[[:digit:]]{4}-.*)*\\.Rda$",full.names=TRUE)
    dstores <- dir(resultPath,pattern="^Datastore(_[[:digit:]]{4}-.*)*$",full.names=TRUE)
    logs    <- dir(resultPath,pattern="Log(_[[:digit:]]{4}-.*)+\\.txt",full.names=TRUE)
    resultFiles <- c(mstates,dstores,logs)
  } else resultFiles <- character(0)

  if ( root ) {
    rootPath <- unique(normalizePath(self$modelPath))
    rootFiles <- dir(rootPath,full.names=TRUE)
    if ( ResultsInRoot ) {
      rootFiles <- setdiff(rootFiles,resultFiles) # Drop Log and ModelState
    }
    rootFiles <- setdiff(rootFiles, stagePaths) # Leave out Stage result sub-directories
  } else rootFiles <- character(0)

  files <- sort(unique(c(
    # in case nothing was asked for: list(list()[x]) would return NULL, not character(0)
    # So force the type for files by providing an empty string to add to NULL/nothing
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
  # so deletion will only work if getwd()==self$modelPath
  if ( normalizePath(getwd() ) != self$modelPath ) {
    owd <- setwd(self$modelPath)
    on.exit(setwd(owd))
  }

  # "stage" could be any vector of intermediate sub-directory names
  if ( is.character(stage) ) {
    # keep only files in subdirectories matching stage$Dir
    stageDirs <- sapply(self$modelStages,function(s) s$Dir)
    stage <- stage[ stage!="." & stage %in% stageDirs ]
    if ( any(stages) ) {
      stages <- paste("/",stages,"/")
      for ( stg in stages ) {
        to.delete <- to.delete[ grep(stg,to.delete) ] # keep the non-matching files
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
  return(invisible(force))
}

################################################################################
# VEModelStage Class
################################################################################

# Load the stage configuration
ve.stage.init <- function(modelParam_ls=list(), Dir=NULL, Name=NULL, Path=NULL, Config=NULL) {
  # modelParam_ls is the base list of parameters from the Model
  # Dir is the subdirectory for the stage, holding InputDir/ParamDir
  #   default of NULL becomes "."
  # ModelDir is where to seek Dir
  #   default of NULL becomes modelParam_ls$ModelDir and if not provided, then "."
  # Name is how we refer to the stage
  #   default is basename(Dir)
  # Path is the path to directory holding InputDir/ParamDir
  #   default is ModelDir/Dir (or Dir itself if absolute path)
  # Config is alternative path/name for "visioneval.cnf" for stage
  #   default is Path/visioneval.cnf
  self$Dir <- if ( is.null(Dir) ) "." else Dir

  if ( "ModelDir" %in% names(modelParam_ls) ) {
    ModelDir <- modelParam_ls$ModelDir
  } else ModelDir <- "."
  ModelDir <- normalizePath(ModelDir,winslash="/")

  # Locate ParamDir, InputDir, ScriptsDir
  if ( is.null(Path) ) {
    self$Path=normalizePath(file.path(ModelDir,Dir),winslash="/")
  } else {
    self$Path=normalizePath(Path,winslash="/")
  }

  # Set Name if not supplied
  self$Name <- if ( is.null(Name) ) basename(self$Path) else Name
  # If self$Dir is ".", Name will be basename of Model

  # no Path for stage - abort
  # NOTE: should rarely happen
  if ( ! dir.exists(self$Path) ) {
    visioneval::writeLog("ModelStage root directory not available",Level="error")
    return(FALSE)
  }

  # Stage Output
  self$RunPath <- file.path(
    ModelDir,
    visioneval::getRunParameter("ResultsDir",Param_ls=modelParam_ls),
    self$Dir
  )
  self$RunPath <- normalizePath(self$RunPath,winslash="/",mustWork=FALSE)

  # Construct stage configuration
  self$RunParam_ls <- list()
  if ( ! is.null(Config)) {
    # Look for Config file, if specified
    if ( ! isAbsolutePath(Config) ) {
      Config <- file.path(self$Path,self$Config)
      self$Config <- normalizePath(Config,winslash="/",mustWork=FALSE)
    }
    if ( is.character(self$Config) && file.exists(self$Config) ) {
      self$RunParam_ls <- visioneval::loadConfiguration(ParamFile=Config,override=modelParam_ls)
    } else self$Config <- NULL
  }
  if ( length(self$RunParam_ls) == 0 ) {
    # if unable to read explicit Config, load "visioneval.cnf" from stage directory
    self$RunParam_ls <- visioneval::loadConfiguration(ParamDir=self$Path,override=modelParam_ls)
  }

  # Default InputPath
  if ( ! "InputPath" %in% names(self$RunParam_ls) ) {
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$initialize",
      InputPath=self$Path
    )
  }

  # Identify "startFrom" stage (VEModelStage$runnable will complete setup)
  if ( "StartFrom" %in% names(self$RunParam_ls) ) {
    self$StartFrom <- self$RunParam_ls$StartFrom
  } else self$StartFrom <- character(0)
}

# Prepare the stage to run in the model context
# Can't do this because it depends on prior stages
# Also load its ModelState if it has been run
ve.stage.runnable <- function(priorStages) {
  # short circuit if we did this already
  if ( ! is.null(private$complete) ) return(private$complete)

  # dig out the information from the StartFrom stage
  if ( length(self$StartFrom) > 0 && nzchar(self$StartFrom) ) {
    errMessage <- character(0)
    if ( ! self$StartFrom %in% names(priorStages) ) {
      errMessage <- paste("StartFrom stage,",self$StartFrom,"does not run before",self$Name)
    } else if ( ! (priorStages[[self$StartFrom]])$runnable() ) {
      errMessage <- paste("StartFrom stage is not Runnable",self$StartFrom)
    }
    if ( length(errMessage)>0 ) {
      stop( visioneval::writeLog(errMessage,Level="error") )
    }

    startFrom     <- priorStages[[self$StartFrom]]$RunParam_ls
    DatastorePath <- startFrom$DatastorePath  # Need not exist, NULL if not present
    ParamPath     <- startFrom$ParamPath      # Need not exist, NULL if not present
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
  } else {
    startFrom     <- list()
    DatastorePath <- character(0)             # Default is to contribute no DatastorePath
    ParamPath <- self$RunParam_ls$ParamPath   # May be NULL
    InputPath <- self$RunParam_ls$InputPath   # May be NULL
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

  # Construct DatastorePath
  #   If LoadDatastore flag:
  if ( "LoadDatastore" %in% names(self$RunParam_ls) ) {
    # Note - if LoadDatastore came from a BaseModel, should have LoadDatastoreName
    if ( ! "LoadDatastoreName" %in% names(self$RunParam_ls) ) {
      if ( "DatastorePath" %in% startFrom ) {
        # If loading Datastore from StartFrom stage
        #   set LoadDatastoreName (and eventually copy it)
        self$RunParam_ls <- visioneval::addRunParameter(
          self$RunParam_ls,
          Source="VEModelStage$runnable",
          LoadDatastoreName=startFrom$DatastorePath[1]
        )
      } else {
        stop(
          visioneval::writeLog(
            paste("LoadDatastore requested, but no LoadDatastoreName provided",Level="error")
          )
        )
      }
    }
    DatastorePath <- character(0) # Path doesn't matter any more
    # Existence of loaded/linked Datastore is verified when the stage runs
  } else {
    DatastorePath <- startFrom$DatastorePath # Virtual Datastore linkage
  }

  # Prepend ModelDir/ResultsDir/StageDir to DatastorePath
  self$RunParam_ls <- visioneval::addRunParameter(
    self$RunParam_ls,
    Source="VEModelStage$runnable",
    DatastorePath=c( self$RunPath, DatastorePath)
  )

  # Underlay run_parameters.json from StagePath/ParamDir/ParamFile if it is out there
  # run_parameters.json is deprecated, and is expected only to provide "descriptive" parameters
  # e.g. Scenario or Description, possibly Years
  # New style model will set those in StageDir/visioneval.cnf
  self$RunParam_ls <- visioneval::loadParamFile(Param_ls=self$RunParam_ls,ModelDir=self$Path)

  # Set up ModelScriptPath if current stage has different ModelScript from overall model
  ScriptName      <- visioneval::getRunParameter("ModelScript",Param_ls=self$RunParam_ls)
  ScriptsDir      <- visioneval::getRunParameter("ScriptsDir",Param_ls=self$RunParam_ls)
  stateModelScriptPath <- character(0)
  for ( scriptDir in c(
    file.path(self$Path,ScriptsDir),
    file.path(self$Path)
  ) ) {
    ScriptPath <- file.path(scriptDir,ScriptName)
    if ( file.exists(ScriptPath) ) {
      stageModelScriptPath <- normalizePath(ScriptPath,winslash="/",mustWork=FALSE)
      break
    }
  }
  if ( nzchar(stageModelScriptPath) ) {
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$runnable",
      ModelScriptPath=stageModelScriptPath,
      ParsedScript=visioneval::parseModelScript(stageModelScriptPath)
    )
  }

  # Force DatastoreType to be explicit rather than default
  if ( ! "DatastoreType" %in% names(self$RunParam_ls) ) {
    self$RunParam_ls <- visioneval::addRunParameter(
      self$RunParam_ls,
      Source="VEModelStage$runnable",
      DatastoreType=visioneval::getRunParameter("DatastoreType",Param_ls=self$RunParam_ls)
    )
  }

  # Check if stage can run (enough parameters to run visioneval::loadModel and visioneval::prepareModelRun)
  missingParameters <- visioneval::verifyModelParameters(self$RunParam_ls)
  private$complete <- length(missingParameters) == 0
  if ( ! private$complete ) {
    visioneval::writeLog(
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
ve.stage.load <- function(onlyExisting=TRUE) {
  if ( is.null(self$ModelState_ls) ) {
    envir = visioneval::modelEnvironment(Clear="ve.stage.load")
    envir$RunModel <- FALSE
    # We're expecting not to write anything, but we'll set useful
    #  directories anyway.
    owd <- if ( dir.exists(self$RunPath) ) {
      setwd(self$RunPath)
    } else setwd(self$RunParam_ls$ModelDir)
    on.exit(setwd(owd))
    ms <- visioneval::loadModel(self$RunParam_ls,onlyExisting=onlyExisting)
    if ( is.list(ms) && length(ms)>0 ) { # Stash the ModelState if created successfully
      self$ModelState_ls <- ms
      if ( ! "RunStatus" %in% names(self$ModelState_ls) ) {
        self$ModelState_ls$RunStatus <- codeStatus("Unknown")
      }
      self$RunStatus <- self$ModelState_ls$RunStatus
      return(TRUE)
    } else {
      visioneval::writeLog(
        paste0("Unable to build model state for stage ",self$Name,"!"),
        Level="warn"
      )
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
      visioneval::initLog(Threshold=log,Save=TRUE,envir=ve.model) # Log stage
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
  visioneval::saveLog(LogFile="console",envir=ve.model)

  # Process results (or errors)
  if ( ! is.numeric(RunStatus) ) {
    # Failure: stop trapped while performing run sequence
    msg <- as.character(RunStatus) # try-error (captures "stop" message)
    visioneval::writeLog(msg,Level="error") # possibly redundant with interior logging
    self$RunStatus <- codeStatus("Run Failed")
    if ( "ModelState_ls" %in% names(visioneval::modelEnvironment()) ) {
      visioneval::setModelState(list(RunStatus=self$RunStatus),envir=ve.model)
    }
  } else {
    # Success: Assemble the runResults
    self$RunStatus <- RunStatus
    visioneval::setModelState(list(RunStatus=self$RunStatus),envir=ve.model)
  }
  self$ModelState_ls <- ve.model$ModelState_ls # The story of the run...
  return(invisible(self$ModelState_ls))
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

# Print a model stage summary
ve.stage.print <- function(details=FALSE) {
  cat("Stage:",self$Name,"(Reportable:",self$Reportable,")\n")
  cat("Status:",printStatus(self$RunStatus),"\n")
  if ( details ) {
    cat("   Starts from:",self$StartFrom,"\n")
    cat("   Configurations:\n")
    cat(paste("   ",uniqueSources(self$RunParam_ls),"\n")) # Generates one row for each unique source
  }
}

VEModelStage <- R6::R6Class(
  "VEModelStage",
  public = list(
    # Public data
    Name = NULL,                 # Name of stage (used e.g. in another stage's StartFrom)
    # May not need to save some of the following (build into RunParam_ls)
    Dir = NULL,                  # 
    Path = NULL,                 # Normalized path to folder holding InputDir and ParamDir
    Config = NULL,               # File relative to ModelDir (optional); 
    StartFrom = "",              # Name of another model stage to extend
    RunParam_ls = list(),        # RunParameters to initialize the stage
    ModelState_ls = NULL,        # ModelState constructed from RunParam_ls
    Reportable = FALSE,          # True if stage is not a starting pont for another
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
    self$load(onlyExisting=FALSE) # Create new model states if they are not present in the file system
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

# Print a summary of the VEModel, including its run status
ve.model.print <- function(details=FALSE) {
  cat("Model:",self$modelName,"\n")
  if ( details ) {
    cat("Path:","\n")
    cat(self$modelPath,"\n")
    cat("Configurations:","\n")
    cat(paste("   ",uniqueSources(self$RunParam_ls),"\n")) # Generates one row for each unique source
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

ve.model.log <- function() {
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

ve.model.load <- function(onlyExisting=TRUE) {

  if ( ! self$valid() ) {
    stop(
      visioneval::writeLog("Model is incomplete and cannot be loaded.",Level="error")
    )
  }

  # Load or Create the ModelState_ls for each stage if not already loaded
  for ( index in seq_along(self$modelStages) ) {
    self$modelStages[[index]]$load(onlyExisting=onlyExisting)
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
    self$load(onlyExisting=TRUE) # Open any existing ModelState_ls to see which may be complete
    alreadyRun <- ( sapply( self$modelStages, function(s) s$RunStatus ) == codeStatus("Run Complete") )
    if ( all(alreadyRun) ) {
      self$status <- codeStatus("Run Complete")
      visioneval::writeLog("Model has been run.",Level="warn")
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
    visioneval::writeLog(paste("Removing previous Results from",workingResultsDir),Level="info")
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
      visioneval::writeLog(
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
    visioneval::writeLog(paste("Stages:",runStages,collapse=","),Level="info")
    msg <- visioneval::writeLog(paste("Starting stage",toRun,"comes after last stage",length(runStages)),Level="error")
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
    self$modelStages[[ms]]$run(log=LogLevel)
  }

  # Update overall model status
  self$updateStatus() # "Worst"/Lowest RunStatus for the overall model

  return(invisible(self$status))
}

################################################################################
#                              Model Configuration                             #
################################################################################

# Make TRANSIENT changes to settings in a model or stage's RunParam_ls (and optionally the stage's
# ModelState_ls). This is a convenience for cloning models and results (see test_query() in
# VEModel/tests/test.r). There is currently no way to save the changes - the model configuration
# files should get updated, and the model should be re-run to make the changes "real".
ve.model.set <- function(stage=NULL,modelState=FALSE,Source="Interactive",Param_ls=list(),...) {
  # Update the RunParam_ls for the model or its stage(s)
  Param_ls <- c(Param_ls,list(...))
  if ( length(Param_ls)>0 ) {
    if ( ! is.character(stage) ) {
      visioneval::writeLog(
        paste0("Changing RunParam_ls elements for model ",self$modelName),
        Level="info"
      )
      self$RunParam_ls <- visioneval::mergeParameters(
        self$RunParam_ls, visioneval::addParameterSource(Param_ls,Source=Source)
      )
    } else {
      visioneval::writeLog(
        paste0("Changing stage RunParam_ls elements for ",paste(stage,collapse=", ")),
        Level="info"
      )
      for ( stgname in stage ) {
        # In the stage, change both the RunParams_ls and (if modelState is TRUE)
        #   also change the ModelState_ls (intended for things like Scenario or
        #   Description). WARNING: you will break things if you reset a deep
        #   structural parameter in the ModelState_ls like DatastoreType. In
        #   general, it's better to change just the RunParams_ls and run the model
        #   again.
        s <- self$modelStages[[stgname]]
        s$RunParam_ls <- visioneval::mergeParameters(
          s$RunParam_ls, visioneval::addParameterSource(Param_ls,Source=Source)
        )
        if ( !is.null(s$ModelState_ls) && modelState ) {
          ms.names <- names(s$ModelState_ls)
          to.change <- ms.names[ ms.names %in% names(Param_ls) ]
          if ( length(to.change) > 0 ) {
            s$ModelState_ls[ ms.names ] <- Param_ls[ ms.names ]
          }
        }
        self$modelStages[[stgname]] <- s
      }
      visioneval::writeLog(paste(paste0(names(Param_ls),"=",as.character(Param_ls)),collapse=","),Level="info")
    }
  }
  return(invisible(Param_ls))
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
    sources <- src_df$Source
    names(sources) <- src_df$Name
    return(sources)
  } else {
    # Return values
    if ( ! is.character(setting) ) {
      return(names(searchParams_ls))
    } else {
      if ( length(setting)<=1 ) {
        settings <- searchParams_ls[[setting]] # single setting
      } else {
        settings <- searchParams_ls[setting]   # list of matching settings
      }
      if ( shorten) settings <- sub(paste0(self$modelPath,"/"),"",settings,fixed=TRUE)
      return(settings)
    }
  }
}

# Report the model results path (used to creat a VEResults object and
# to retrieve the LogFile)
ve.model.findstage <- function(stage=NULL) {
  # TODO: Update to new model stage structure
  if ( ! private$p.valid ) return( NULL )

  if ( missing(stage) || !is.character(stage) ) {
    stage <- tail(names(self$modelStages),1)
  }
  if ( length(stage)>1 ) {
    stage <- names(self$modelStages)[stage[length(stage)]] # use last one listed
  }
  stage <- self$modelStages[[stage]] # get the stage object from the sliced stage list (may be NULL)
  return(stage)
}

################################################################################
#                                Model Results                                 #
################################################################################

# create a VEResults object (possibly invalid/empty) based on the current model
#   run or a particular model stage. Default is the last model stage.
ve.model.results <- function(stage=NULL) {
  # Create a results object wrapping the directory that contains the model
  # results for the given stage (or last stage if not given)
  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),level="error")
    return( NULL )
  }

  stg <- self$findstage(stage)
  if ( is.null(stg) ) stop(
    visioneval::writeLog(paste("Model stage not found:",stage),Level="error")
  )
  results <- VEResults$new(stg$RunPath)
  if ( ! results$valid() ) {
    if (!is.null(stage)) {
      visioneval::writeLog(
        paste("There are no results for stage ",paste(stage,collapse=", ")," of this model yet."),
        Level="warn"
      )
    } else {
      visioneval::writeLog("There are no results for this model yet.",Level="warn")
    }
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
    modelStages=NULL,                       # list of VEModelStage objects
    RunParam_ls=NULL,                       # Run parameters from the model root
    specSummary=NULL,                       # List of inputs, gets and sets from master module spec list  
    status=codeStatus("Uninitialized"),     # Where are we in opening or running the model?

    # Methods
    initialize=ve.model.init,               # initialize a VEModel object
    valid=function() private$p.valid,       # report valid state
    load=ve.model.load,           # load the model state for each stage (slow - defer until needed)
    run=ve.model.run,                       # run a model (or just a subset of stages)
    print=ve.model.print,                   # provides generic print functionality
    printStatus=ve.model.printStatus,       # turn integer status into text representation
    updateStatus=ve.model.updateStatus,     # fill in overall status based on individual stage status
    list=ve.model.list,                     # interrogator function (script,inputs,outputs,parameters
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    log=ve.model.log,                       # report the log file path (use e.g. file.show to display it)
    setting=ve.model.setting,               # report the values of parameter settings (for environment, model or stage)
    set=ve.model.set,                       # change the value of a parameter in a model or stage
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    archive=ve.model.archive,               # apply framework archive function if results exist
    results=ve.model.results,               # Create a VEResults object (if model is run); option to open a past result
    findstage=ve.model.findstage,           # Report the path to the model results for a stage
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
    visioneval::initLog(Console=TRUE,Threshold=log, envir=new.env())
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
  if ( "Config" %in% names(model) ) {
    visioneval::writeLog(paste0("Copying ",model$Config," into visioneval.cnf",Level="info"))
    file.copy(model$Config,file.path(installPath,"visioneval.cnf"))
  } else {
    # Hack for classic model - no Config
    # Elevate content "scripts" directory to installPath then remove scriptPath
    if ( dir.exists( scriptPath<-file.path(installPath,"scripts") ) ) {
      visioneval::writeLog("No config: promoting 'scripts' directory",Level="info")
      file.copy(dir(scriptPath,full.names=TRUE),installPath)
      unlink(scriptPath,recursive=TRUE)
    } else {
      stop( visioneval::writeLog(paste0("No configuration file available for ",modelName),Level="error") )
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
  # Load system model configuration (clear the log status)
  visioneval::initLog(Console=TRUE,Threshold=log, envir=new.env())
  model <- installStandardModel(modelName, modelPath, confirm=confirm, variant=variant, log=log)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath, log=log ) )
  } else {
    return( model ) # should be a character vector of information about standard models
  }
}
