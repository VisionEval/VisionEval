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

## Helper function: getBaseModel
getBaseModel <- function(modelStages) {
  # Locate BaseModel parameter in Param_ls
  # modelStages parameter is a list of model stages already previously defined for this model
  # if it exists, find the BaseModel by deparsing the parameter
  # 
  # Check if BaseModel is defined in Param_ls. If so, interpret it as a location:
  #   If pipe included, first part is model locator, second part is stage within it
  #   Model locator is checked like this:
  #     1. Name of stage already in self$ModelStages (part of what we have built so far)
  #     2. Name of another model that can be opened with VEModel$new
  #   The part after the (optional) pipe is used as an index into the modelStages of the
  #     model that was opened before the pipe. If not provided, use the last terminal
  #     element in the list of model stages from the other model
  # What we need to know about the BaseModel: structural input settings
  #   InputPath, ModelScriptFile, (Base)ModelDir, ParamDir, RunPath (for datastore and
  #     model state).
  #   plus where to find its Datastore - each stage needs to keep its RunPath in
  #   the ModelState file. We can use that to obtain the BaseModel model state
  #   file location (even if it's a stage itself)
  return(Param_ls)
}

## Helper function: getModelStage
# Check if StageDir within modelDir contains a model stage and return a ModelStage structure
## Helper Function
# Examine a directory for a complete model:
#  0. Parameters:
#       modelDir: an absolute path in the file system to inspect for a model stage
#       Param_ls: the current run parameters to be modified here and used to navigate
#       stageDir: if ".", ignore it (use modelDir only). Otherwise this is a relative
#          path for the stage (and its default name). If stageDir is not ".", then also
#          do not allow override of ParamDir if defined (nor DatastoreName, DatastoreType)
#          ResultsDir is also ignored when building stages (always relative to overall ModelDir)
#  1. Load any visioneval.cnf if present (possibly redefining structural directory names)
#       Drop any of the ignore fields if StageDir is not "." (ParamDir, DatastoreName, DatastoreType)
#  1A. Check for BaseModel defined and layer its structural parameters "underneath" visioneval.cnf.
#  2. Establish ParamDir if <ModelDir>/<ParamDir> does not exist
#       Set it to <StageDir>/<ParamDir> if <ModelDir>/<StageDir>/<ParamDir> exists
#       <ModelDir>/<ParamDir> (with ParamDir possibly redefined) must exist for the stage to be valid
#  3. Override ModelScriptFile as <ModelDir>/<StageDir>/<ScriptsDir>/<ModelScript> if that file exists
#       Otherwise leave existing ModelScriptFile
#       ModelScriptFile must be defined for the stage to be valid
#       Parse the ModelScriptFile for initializeModel parameters ("dots")
#  4. Run the Parameter Initialization (including ModelScriptFile$initializeModel arguments)
#       That's visioneval::getModelParameters
#  5. Update InputPath in StageParams if InputDir found specifically in ModelDir/StageDir
#       There must at least one element in InputPath for the stage to be valid
#       All elements of <InputPath>/<InputDir> must exist (but may be empty)
#  6. It's a model stage if:
#       InputPath has at least one element, and all <InputPath>/<InputDir> can be found
#       <ModelDir>/<ParamDir> (with ParamDir possibly redefined) exists in the file system
#       ModelScriptFile can be found
#       Param_ls has Model(Name), Scenario, and Description
#       Param_ls has BaseYear, Years and Region
#  7. Return ModelStage structure (or some error condition)
#     StageModelState
#       Filled in when the stage is loaded (not just found); NA initially; triggers a load
#       when we try to do anything that requires it.
#     StageInputPath
#       All the places to look for InputDir and files we need...
#     StageRunPath
#       The directory in which the stage will run to create its ModelState.Rda, Datastore
#       and Log file (absolute).
#     StageName
#       Becomes name of stage entry in self$modelStages
#       Might come from visioneval.cnf, or from StageDir by default
#     StageScriptPath
#       Might be ModelDir/ModelScript, might be ModelDir/StageDir/ScriptsDir/ModelScript
#     StageParam_ls
#       Param_ls updated with stage's visioneval.cnf - basis for ModelStage for this stage
#       Should get default Model(Name), Scenario, Description if not defined in stage's visioneval.cnf
#       May drop parameters not relevant to running the model or reading its results
#     StageBaseModel
#       A ModelStage structure for the BaseModel (which may itself have a BaseModel, etc.)
#       The chain of run paths is used when setting up the child model Datastore list along
#       with StageParam_ls (to load the relevant ModelState and Datastore).
#
addModelStage <- function(modelStages, modelDir, Param_ls, stageDir=".") {
  # Augment modelStages with new modelStage
  # Use modelStages to locate BaseModels specified by relative path
  return(modelStages)
}

## Helper function
#  Build ModelStages based on contents of modelPath
#  This function works on a modelDir (absolute path) and finds all the stages
# TODO: return the list of model stages found for this model
findModel <- function( modelDir, Param_ls ) {

  find_ls <- list()
  find_ls$isValid <- FALSE # we'll change our mind later...

  if ( missing(modelPath) || ! is.character(modelPath) ) {
    visioneval::writeLog("findModel: Must provide modelPath locator.",Level="warn")
    return( find_ls )
  }

  # TODO: get model Stage in modelDir
  # Examine ModelStages run parameter for subdirectories to consider
  # If no ModelStages and stage in modelDir is incomplete, visit non-structural
  #  sub-directories (based on Param_ls) to add modelStages
  # Otherwise if ModelStages is defined, copy that structure into self$modelStages
  #  and fill it in.  Error if any stage is not complete

  # Establish the local name for run_model.R
  # VisionEval.R startup will set ModelScript from site configuration file
  #   located in runtime root (first of .visioneval, VisionEval.ini, VisionEval.cnf)
  #   with same default as here ("run_model.R")
  searchScript <- visioneval::getRunParameter("ModelScript",Param_ls=Param_ls)

  # check if modelPath contains a runModelName
  findPath <- grep(modelPath,searchScript,ignore.case=TRUE,value=TRUE)
  if ( length(findPath>0) ) {
    modelPath <- findPath[1]
    visioneval::writeLog(paste0("Using explicit script matching ",searchScript,":\n"),Level="debug")
    find_ls$modelPath <- normalizePath(dirname(modelPath),winslash="/")
    find_ls$stagePaths <- "." # modelPath root is the only stagePath
    find_ls$stageScripts <- basename(modelPath)
    find_ls$isValid <- TRUE
    visioneval::writeLog( modelPath, Level="info")
    return(find_ls)
  }

  # if modelPath is not an absolute path, search for it amongst the "roots"
  if ( ! isAbsolutePath(modelPath) ) {
    roots<-getModelRoots()
    possiblePaths <- file.path(roots,modelPath)
    existing <- dir.exists(possiblePaths)
    if ( ! any(existing) ) {
      visioneval::writeLog(
        paste0("Failed to find model ",modelPath," in these locations:\n",paste(roots,collapse="\n")),
        Level="error"
      )
      find_ls$modelPath <- modelPath
      return(find_ls)
    } else {
      # Use first of the existing paths
      find_ls$modelPath <- normalizePath(possiblePaths[existing][1],winslash="/")
    }
  } else if ( ! dir.exists(modelPath) ) {
    visioneval::writeLog(
      paste0("Model directory ",modelPath," does not exist"), Level="error"
    )
    return(find_ls)
  } else {
    find_ls$modelPath <- modelPath
  }

  # Attempt to locate runModelName in directory and first-level sub-directories
  # The stagePaths need to be relative to modelPath
  find_ls$stagePaths <- modelInRoot(find_ls$modelPath,searchScript)
  if ( length(find_ls$stagePaths)<=0 ) {
    visioneval::writeLog(
      paste0("No matching pattern for",searchScript," found in ",find_ls$modelPath),Level="error"
    )
  } else {
    find_ls$stageScripts <- basename(find_ls$stagePaths)
    find_ls$stagePaths <- sub(
      find_ls$modelPath,"",
      dirname(find_ls$stagePaths),
      fixed=TRUE
    )
    find_ls$stagePaths[ ! nzchar(find_ls$stagePaths) ] <- "."
    find_ls$isValid <- TRUE
  }

  # Finally load the model configuration if it exists and update Param_ls
  # see visioneval::readConfigurationFile to learn that parameter files can be
  #  named VisionEval.yml, VisionEval.json, VisionEval.cnf, or VisionEval.ini
  # and their format can be either YAML or JSON. The first name found will be used.
  # If no file is found, find_ls$RunParam_ls will just be the same as Param_ls
  find_ls$RunParam_ls <- visioneval::loadConfiguration(ParamDir=find_ls$modelPath,override=Param_ls)
  ModelDir_ls <- visioneval::addParameterSource(Param_ls=list(ModelDir=find_ls$modelPath),Source="VEModel::initialize")
  find_ls$RunParam_ls <- visioneval::mergeParameters(find_ls$RunParam_ls,ModelDir_ls)

  # Note that when the model is run, further changes to RunParam_ls may come from "dots"
  #  in initializeModel, or from ParamDir/ParamFile (if source'ing a run_model.R; the
  #  new model_setup.yml model configuration will ignore those).
  
  return( find_ls )
}

## Look up a standard model
#  'model' is bare name of standard model
#  returns the full path to that model template
findStandardModel <- function( model ) {
  standardModels <- system.file("models",package="VEModel")
  if ( ! nzchar(standardModels) ) {
    standardModels <- getOption("VEStandardModels",default=normalizePath("inst/models"))
  }
  if (is.null(model) || ! nzchar(model)) {
    return( dir(standardModels) )
  }

  model <- model[1]
  model <- file.path(standardModels,model)
  if ( ! dir.exists(model) ) {
    visioneval::writeLog(paste("No standard model called ",model),Level="error")
    return( findStandardModel("") )
  }
  return(model) # absolute path to standard model matching name
}

## install a standard model with data identified by "skeleton"
#  We're still expecting to distribute with standard models pre-installed
#  Called automatically from findModel, where modelPath must be a bare model name
#  Can install from other locations by calling this function with a more elaborate modelPath

SampleModelDataFormat <- c( sample="samp",template="tpl",test="mini" )

installStandardModel <- function( modelName, modelPath, confirm, skeleton=c("sample","template","test"), Param_ls=NULL, log="error" ) {
  # Locate and install standard modelName into modelPath
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first available standard root
  #   If modelPath is an existing directory, put modelName into it (conflict-resolved name)
  #   If modelPath does not exist, but dirname(modelPath) exists, create new directory and put the model there
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and
  #   they have to try again
  
  visioneval::initLog(Save=FALSE,Threshold=log)
  
  model <- findStandardModel( modelName )
  if ( is.null(modelName) || ! nzchar(modelName) ) {
    return(model) # Expecting a vector of available standard model names
  } # Otherwise model is the path to them model we will install

  # Set up destination modelPath (always put in the first defined root)
  if ( ! is.list(Param_ls) ) Param_ls <- list()
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
  if ( ! is.character(skeleton) ) {
    skeleton <- "sample"
  } else {
    skeleton <- skeleton[1]
    if ( ! skeleton %in% names(SampleModelDataFormat) ) {
      skeleton <- "sample"
    }
  }
  modelData <- SampleModelDataFormat[skeleton]
  if ( confirm && interactive() ) {
    msg <- paste0("Install standard model '",modelName,"' (",modelData,") in ",installPath,"?\n")
    install <- confirmDialog(msg)
  }

  if ( ! install ) stop("Model ",modelName," not installed.",call.=FALSE)

  # Now do the installation
  visioneval::writeLog(paste0("Installing ",modelName," from ",model," as ",modelData),Level="info")
  dir.create(installPath)

  # Locate the model and data source files
  model.path <- file.path(model,"model")
  model.files <- dir(model.path,full.names=TRUE)

  data.path <- file.path(model,modelData)
  if ( ! dir.exists(data.path) ) {
    visioneval::writeLog(msg<-paste("No",skeleton,"data available for",modelName),Level="error")
    visioneval::writeLog(c("Directory missing:",data.path),Level="info")
    stop(msg)
  }
  data.files <- dir(data.path,full.names=TRUE)

  file.copy(model.files,installPath,recursive=TRUE) # Copy standard model into modelPath
  file.copy(data.files,installPath,recursive=TRUE) # Copy skeleton data into modelPath
  visioneval::writeLog(paste0("Installed ",modelName," in ",installPath),Level="info")

  return( list(modelName=modelName,modelPath=installPath) )
}

ve.model.copy <- function(newName=NULL,newPath=NULL,copyResults=TRUE) {
  # Copy the current model to NewName (in ModelDir, unless newPath is also provided)
  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),level="error")
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
  "YEAR", "OPTIONAL","SPEC"
)
# SPEC is added to what might otherwise be there,
#   and it contains either "Inp", "Get" or "Set"

summarizeSpecs <- function(AllSpecs_ls) {
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

# Helper function:
#  Open an existing ModelState file, or create a stripped-down one in memory
#    The role of the in-memory version is to have the parsed model script
#    And to identify the model input data elements and files
#  Will eventually support interrogating the model script and structures (e.g. inputs and
#  outputs) without actually running the model.
ve.model.loadModelState <- function(log="error") {
  # Load all ModelStates for each model stage, using initializeModel with RunModel=FALSE
  # If ModelState exists from a prior run, load that rather than rebuild

  if ( ! dir.exists(self$modelPath) ) {
    private$p.valid <- FALSE
    return( list() )
  }
  
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  workingResultsDir <- file.path(self$modelPath,ResultsDir)
  if ( ! dir.exists(workingResultsDir) ) {
    dir.create(workingResultsDir,showWarnings=FALSE)
  }
  ModelStateFileName <- visioneval::getRunParameter("ModelStateFile",Param_ls=self$RunParam_ls)
  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  # Almost always, the BaseInputPath should be "."
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- file.path(self$modelPath,BaseInputPath)
  }
  self$ModelState <- list()
  owd <- setwd(workingResultsDir)
  on.exit(setwd(owd))
  
  initMsg <- "Loading ModelState"
  if ( self$stageCount>1 ) {
    initMsg <- paste(initMsg,"for Stage ")
  } else {
    visioneval::writeLog(initMsg,Level="debug")
  }

  self$runStatus <- rep("Uninitialized",self$stageCount)
  self$specSummary <- NULL

  for ( stage in 1:self$stageCount ) {
    stagePath <- self$stagePaths[stage]
    stageIndex <- toupper(basename(stagePath))

    if ( self$stageCount>1 ) {
      visioneval::writeLog(paste(initMsg,stage,":",stagePath),Level="debug")
    }
    modelState <- normalizePath(file.path(workingResultsDir,stagePath,ModelStateFileName),winslash="/",mustWork=FALSE)
    visioneval::loadModelState(modelState, ( ms.env <- new.env() ))
    self$ModelState[[ stageIndex ]] <- ms.env$ModelState_ls
    if ( length(self$ModelState[[ stageIndex ]]) > 0 ) {
      # Attempt to load existing ModelState
      if ( "RunStatus" %in% names(ms.env$ModelState_ls) ) {
        self$runStatus[stage] <- ms.env$ModelState_ls$RunStatus
      } else {
        self$runStatus[stage] <- "Unknown Prior Run"
      }
    } else {
      # ModelState file does not yet exist (not run)
      # Run the initializeModel function to build in-memory ModelState_ls
      # Needed to inspect model elements (script, inputs, outputs)
      visioneval::writeLog("Pre-Initializing ModelState",Level="info")
      self$runStatus[stage] <- "Not Run"
      stageInput <- file.path(self$modelPath,stagePath)
      scriptFile <- self$stageScripts[stage]
      Param_ls <- ve.model.setupRunEnvironment(
        Owner="VEModel::loadModelState",
        Param_ls=self$RunParam_ls,
        RunModel=FALSE,
        ModelDir=stageInput,
        ResultsDir=file.path(ResultsDir,stagePath),
        InputPath=unique(c(stageInput,BaseInputPath)),
        ModelScriptFile=normalizePath(file.path(stageInput,scriptFile),winslash="/"),
        Name=self$modelName,
        LogLevel=log
      )

      # Parse the model script
      parsedScript <- visioneval::parseModelScript(Param_ls$ModelScriptFile)

      visioneval::writeLog("InitializeModel...",Level="info")

      # Execute the initializeModel function from the model script with RunModel==FALSE
      # Loads the model configuration elements and builds a ModelState
      # (RunModel==FALSE so working directory is irrelevant).
      initArgs                   <- parsedScript$InitParams_ls; # includes LoadDatastore etc.
      # Naming explicit arguments below (e.g. ModelScriptFile) makes them higher priority than Param_ls
      initArgs$ModelScriptFile   <- Param_ls$ModelScriptFile
      initArgs$LogLevel          <- log
      ms <- do.call(visioneval::initializeModel,args=initArgs)
      self$ModelState[[ stageIndex ]] <- ms
      Param_ls <- ms$RunParam_ls

      visioneval::writeLog("Process Package Specifications...",Level="info")

      # Process required packages and specification list (similar to what is done in
      # initializeModel when actually running a model).
      RequiredPackages <- parsedScript$RequiredPackages
      AlreadyInitialized <- character(0)
      if ( ! is.null(Param_ls$LoadDstoreName) ) { # Loading a base model
        LoadEnv <- new.env()
        LoadDstoreDir <- dirname(Param_ls$LoadDstoreName) # Null if not set during initializeModel
        LoadEnv$ModelState_ls <- self$ModelState[[toupper(basename(LoadDstoreDir))]]
        if ( is.null(LoadEnv$ModelState_ls) ) {
          ModelStateFile <- getRunParameter("ModelStateFile",Param_ls=Param_ls)
          modelStatePath <- file.path(LoadDstoreDir,ModelStateFile) # TODO: Unpack LoadDstoreDir from InitializeModel arguments
          visioneval::loadModelState(modelStatePath,envir=LoadEnv)
        }
        if ( "RequiredVEPackages" %in% names(LoadEnv$ModelState_ls) ) {
          AlreadyInitialized <- LoadEnv$ModelState_ls$RequiredVEPackages
          RequiredPackages <- unique(c(RequiredPackages, AlreadyInitialized))
        }
      }
      visioneval::parseModuleCalls(parsedScript$ModuleCalls_df, AlreadyInitialized, RequiredPackages, Save=FALSE)
    }
    sumspec <- summarizeSpecs(self$ModelState[[ stageIndex ]]$AllSpecs_ls)
    if ( is.null(self$specSummary) ) {
      self$specSummary <- sumspec
    } else {
      self$specSummary <- rbind(self$specSummary,sumspec)
    }
  }

  if ( length(self$ModelState)!=self$stageCount ) {
    self$status <- "Failed to Load"
    visioneval::writeLog(
      c(
        "Failed to load all ModelStates",
        "ModelState list not the same length as stagePaths list: ",
        length(self$ModelState)," ",self$stageCount
      ),
      Level="error"
    )
    self$status <- "No ModelState"
    private$p.valid <- FALSE
  } else {
    self$status <- self$runStatus[length(self$runStatus)]
    private$p.valid <- TRUE
  }
  invisible(self$ModelState)
}

# Initialize a VEModel from modelPath
# modelPath may be a full path, and will be expanded into known model directories
#  if it is a relative path.
ve.model.init <- function(modelPath=NULL,log="error") {
  # Load system model configuration
  visioneval::initLog(Save=FALSE,Threshold=log)
  self$RunParam_ls <- getSetup()

  # Opportunity to override names of ModelState, run_model.R, Datastore, etc.
  # Also to establish standard model directory structure (inputs, results)

  # Identify the run_model.R root location(s)
  # Also, update self$RunParam_ls with model-specific configuration
  modelPaths <- findModel(modelPath,self$RunParam_ls)
  if ( ! modelPaths$isValid ) {
    self$status <- "No Model"
    private$p.valid <- FALSE
  } else {
    # TODO: findModel returns ModelStages structure
    # Set up the model name, folder tree and script files
    #  self$modelPath == ModelDir (an absolute path)
    #  self$modelStages == list of ModelStage structures
    #     StageDir (relative always to ModelDir, and later for ResultsDir)
    #     RunParam_ls for this stage (specifying script, inputs, etc)
    #     StageName (also name of ModelStage list item)
    #     ModelState_ls for this stage will be built or added during Load
    self$RunParam_ls <- modelPaths$RunParam_ls; # Updated from model configuration file, if any
    self$modelPath <- modelPaths$modelPath
    self$modelName <- basename(self$modelPath)
    self$stagePaths <- modelPaths$stagePaths; # named stage names (folders with run_model.R, relative to ModelPath)
    self$stageScripts <- modelPaths$stageScripts; # actual names of ModelScriptFile for each stage
    self$stageCount <- length(self$stagePaths)

    initMsg <- paste("Loading Model",self$modelName)
    visioneval::writeLog(initMsg,Level="info")

    # TODO: skip loading if desired (flag on this function)
    private$loadModelState(log=log) # load model states for model stages

    if ( private$p.valid ) {
      visioneval::writeLog(self$modelPath,Level="info")
      visioneval::writeLog("Model Load Complete.",Level="info")
    } else {
      visioneval::writeLog(c("Model Load Failed",paste(self$modelPath,"\nStatus:",self$status)),Level="error")
    }
  }
  invisible(private$p.valid)
}

# Function to inspect the model configuration/setup parameters
# With no arguments, reports name/value pair for values explicitly defined
# If Param_ls is set, it adds its named components to self$RunParam_ls
# The return is the new set of named values (using default or other show/search arguments)
ve.model.set <- function(show="values", src=NULL, namelist=NULL, pattern=NULL,Param_ls=NULL) {
  # break out the "do something" function (Param_ls provided - set values defined in Param_ls in
  # self$RunParam_ls).
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
    searchParams_ls <- Param_ls; # search in command line structure
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
    names(results) <- names(sought); # One column, so data.frame drops to vector
  }

  return(results)
}

# List the model contents, using parsedScript and specSummary from ve.model.load
ve.model.list <- function(inputs=FALSE,outputs=FALSE,details=NULL) {
  # "inputs" lists the input files (Inp) by package and module (details = field attributes)
  # "outputs" lists the fields that are Set in the Datastore (details = field attributes)
  # if both are true, we also liet the Get elements
  # "details" FALSE lists units and description plus pacakge/module/group/table/name
  # "details" TRUE lists all the field attributes (the full data.frame of specSummary)

  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),level="error")
    return( invisible(data.frame()) )
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

ve.model.archive <- function() {
  # TODO: Rebuild for new stage structure (re-write ResultsDir so we create and archive
  # stage sub-directories)
  # If SaveDatastore is true when a VEModel stage runs and we're resetting or 
  return(NULL);
  ResultsName = getRunParameter("ArchiveResultsName",Param_ls=self$RunParam_ls)
  OutputDir <- getRunParameter("OutputDir",Param_ls=self$RunParam_ls) # May differ in other stages...
  DstoreName <- getRunParameter("DatastoreName",self$RunParam_ls)
  ModelDir <- self$modelPath
  for ( stage in 1:self$stageCount ) {
    ModelStatePath <- dirname(self$resultspath(stage,Param_ls=self$RunParam_ls))

    # TODO: get full pathname of model state (will use
    # dirname(ModelStatePath)) (was ve.model$ModelStatePath)
    # TODO: construct ResultsName
    # TODO: each stage gets archived into ModelDir/ResultsName/stagePath
  
    failToArchive <- archiveResults( ModelDir, DstoreName, ModelStatePath, OutputDir, file.path(stageDir,ResultsName) )
    if ( length(failToArchive)>0 ) {
      visioneval::writeLog(paste0("Failed to archive results (",paste(failToArchive,collapse=","),")"),Level="error")
    }
  }
}

# Run the modelPath (through its stages)
# Find the runModel script
# Work in the Results directory - need to relay locations from here to initializeModel
# TODO: replace the "stages" subdirectories with the more flexible concept of a "BaseModel"
#   which provides Datastore components previously computed (and possibly the entire run_model.R
#   script).
ve.model.run <- function(run="save",stage=NULL,lastStage=NULL,log="warn") {
  # TODO: rework for the new Stage architecture
  # run parameter can be
  #      "continue" (run all steps, starting from first incomplete; "reset" is done on the first
  #      incomplete stage)
  #   or "save" in which case we reset, but first save the ResultsDir tree
  #   or "reset" in which case we restart from stage 1, but first clear out ResultsDir (no save)
  #
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

  if ( ! private$p.valid ) {
    visioneval::writeLog(paste0("Invalid model: ",self$status),Level="error")
    return( invisible(self$status) )
  }
  
  # If reset, then go back to the first stage and run from there
  #   Leaves SaveDatastore untouched
  # If save, like continue, but forces SaveDatastore to be TRUE (below, after copying self$RunParam_ls
  #   into the local stage's RunParam_ls

  # Set up workingResultsDir for file manipulations (including stage sub-directories)
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  workingResultsDir <- file.path(self$modelPath,ResultsDir)

  SaveDatastore = NULL # ignore any pre-configured value for SaveDatastore
  if ( run == "restart" || run=="reset" ) {
    SaveDatastore <- FALSE
    stage <- 1; lastStage <- self$stageCount; # Do it all...
    visioneval::writeLog(paste("Removing previous Results from",workingResultsDir),Level="info")
    unlink(dir(workingResultsDir,full.names=TRUE),recursive=TRUE)
  } else if ( run == "save" ) {
    SaveDatastore <- TRUE
  }

  # Recreate the results directory in case it is not there
  if ( ! dir.exists(workingResultsDir) ) {
    dir.create(workingResultsDir,showWarnings=FALSE)
  }

  if ( ! is.null(SaveDatastore) ) {
    self$set(
      Param_ls=visioneval::addParameterSource(
        Param_ls=list(SaveDatastore=SaveDatastore),
        Source=paste0(self$modelName,"$run()")
      )
    )
  }

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

  # Set up default logging
  visioneval::initLog(Save=FALSE,Threshold=log)

  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- normalizePath(file.path(self$modelPath,BaseInputPath),winslash="/",mustWork=FALSE)
  }

  owd <- setwd(workingResultsDir)
  on.exit(setwd(owd))

  # Set up the model runtime environment
  for ( ms in 1:lastStage ) {
    stagePath <- self$stagePaths[ms]
    scriptFile <- self$stageScripts[ms]
    stageInput <- file.path(self$modelPath,stagePath)
    stageResults <- file.path(workingResultsDir,stagePath)

    initMsg <- paste("Running Model",self$modelName)
    if ( self$stageCount>1 ) {
      visioneval::writeLog(paste(initMsg,"Stage",stage,": ",stagePath),Level="info")
    } else {
      visioneval::writeLog(initMsg,Level="info")
    }

    if ( run == "continue" || ms < stageStart ) {
      if ( self$runStatus[ms] == "Complete" ) {
        visioneval::writeLog(paste("Stage",stage,"is Complete"),Level="warn")
        next
      } else if ( ms < stageStart ) {
        stop(
          visioneval::writeLog("Stage",ms,paste0("(prior to ",stageStart,")"),"is not Complete. Try again",Level="error")
        )
      } # else runStatus is not Complete and ms>=stageStart so just fall through and run this stage
    } # else we will re-run the stage (respecting run="save" or run="reset" with respect to archiving prior results

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
              PreviousState=self$ModelState, # previously loaded model states
              Param_ls=self$RunParam_ls,
              RunModel=TRUE,
              ModelDir=stageInput,
              ResultsDir=file.path(ResultsDir,stagePath),
              InputPath=unique(c(stageInput,BaseInputPath)),
              SaveDatastore=SaveDatastore,
              ModelScriptFile=normalizePath(file.path(stageInput,scriptFile),winslash="/"),
              Name=self$modelName,
              LogLevel <- log
            )

            visioneval::writeLog(c("Executing Script File:",Param_ls$ModelScriptFile),Level="info")

            RunDir <- normalizePath(file.path(self$modelPath,Param_ls$ResultsDir),winslash="/",mustWork=FALSE)
            if ( ! dir.exists(RunDir) ) dir.create(RunDir,showWarnings=FALSE,recursive=TRUE)
            setwd(RunDir) # Set working directory each time through each loop
            # Model needs to run in "RunDir" (where ModelState and Datastore are written)
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
            cat("Traceback:\n")
            for ( t in .traceback(1) ) {
              cat(substr(utils::head(t),1,50),"\n")
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
            if ( is.list(ve.model$ModelState_ls) ) {
              visioneval::writeLog(c("Current directory saving model state:",getwd()),Level="info")
              visioneval::setModelState(
                list(
                  RunStatus=self$runStatus[ms]
                ),
              )
              self$ModelState[[ toupper(basename(stagePath)) ]] <- ve.model$ModelState_ls
            } else {
              visioneval::writeLog("No model state to receive RunStatus",Level="error")
            }
            visioneval::saveLog("console") # Turn off file logging
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

ve.model.log <- function() {
  ms <- self$ModelState[[self$stageCount]]
  if ( is.null(ms) ) return("")
  logfile <- file.path(self$resultspath(),ms$LogFile) # log file is saved in ResultsDir/stagePath
  if ( is.null(logfile) || is.na(logfile) ) return("")
  return(logfile)
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
    visioneval::writeLog(self$status,Level="error")
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

# Print a summary of the VEModel, including its run status
ve.model.print <- function() {
  if ( private$p.valid ) {
    cat("Model:",self$modelName,"\n")
    cat("Path:\n")
    print(self$modelPath)
    cat("Datastore Type:",self$ModelState[[1]]$DatastoreType,"\n")
  } else {
    cat("Model: Invalid\n")
  }
  cat("Status:", self$status,"\n")
  self$status
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
    ModelState=NULL,
    RunParam_ls=NULL,
    specSummary=NULL,                       # List of inputs, gets and sets from master module spec list  
    runStatus=NULL,
    status="Uninitialized",

    # Methods
    initialize=ve.model.init,               # initialize a VEModel object
    valid=function() private$p.valid,         # report valid state
    run=ve.model.run,                       # run a model (or just a subset of stages)
    rename=ve.model.rename,                 # Change model Name, Scenario, Description
    print=ve.model.print,                   # provides generic print functionality
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
    p.valid=FALSE,                          # R6 error: "All items in public, private and active must have unique names"
    runError=NULL,
    lastResults=list(),                     # Cache previous results object
    index=NULL,
    # Private Methods
    loadModelState=ve.model.loadModelState  # Function to load a model state file
  )
)

## EXPORTED HELPER FUNCTIONS
#===========================
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
#' @param skeleton A character string identifying the sample data to install. Options are "sample",
#'   which is a small real-world model, "template" which installs data files containing only their
#'   header row, or "test" which installs a miniature model with just enough data and years to
#'   run the model script (used for testing framework functions).
#' @param confirm if TRUE (default) and running interactively, prompt user to confirm, otherwise
#'   just do it.
#' @param log a string describing the minimum level to display
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, skeleton=c("sample","template","test"), confirm=TRUE, log="error") {
  model <- installStandardModel(modelName, modelPath, confirm, skeleton, log=log)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath, log=log ) )
  } else {
    return( model ) # should be a character vector of available standard models
  }
}
