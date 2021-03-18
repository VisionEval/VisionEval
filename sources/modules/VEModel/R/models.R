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
#' access the model results. The framework itself contains full support for running a model, so
#' if you have a model, you can still just change into its directory and do
#' \code{source('run_model.R')}. VEModel (and its helpers, VEResult and VEQuery) provide a
#' convenient interface for running a model and exploring its structure and results.
#'
#' Creating a model is still a manual process, so you're usually better off duplicating one of the
#' standard models The VEModel manager makes that very easy! See \code{vignette('VEModel')} for full
#' instructions. A simple introduction is found on the VisionEval wiki, in the Getting-Started
#' document (and that document is also included in runtime installations of VisionEval).
#'
#' Here are the details of the VEModel manager.
#'
#' @section Usage:
#' \preformatted{model <- VEModel$new(modelName,log="error")}
#'
#' @section Arguments:
#' \describe{
#'   \item{modelName}{The path or basename of a directory containing a VisionEval model setup; if
#'   it's a relative path, the model will be sought in the current directory plus standard places
#'   like ve.runtime/models}
#' }
#'
#' @section Details:
#'
#' Details are yet to come.
#' scalar.
#'
#' \code{$new()} creates a new VEModel object from a file path that
#' locates a \code{run_model.R} script and an optional configuration file.
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
#  Model roots: ve.runtime/models, getwd()/models, ve.runtime, getwd()
getModelRoots <- function(get.root=0) {
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
  modelRoot <- file.path(roots,visioneval::getRunParameter("ModelRoot"))
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

## Helper function
#  Look for run_model.R (any case) in root or subdirectories and return full paths
#  modelPath is where to look (possibly more than one directory)
#  searchScript is a name or pattern to search for (character string)
#  treat searchScript as an R regular expression.
modelInRoot <- function(modelPath,searchScript) {
  # Search initially just in modelPath, then in first-level subdirectories
  # Expect modelPath to be normalized
  grepArgs <- list(searchScript,value=TRUE,ignore.case=TRUE)
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
        "Model script search yielded more than one file per Model directory:\n\n",
        paste(paths[ dirname(paths) %in% dirname(paths)[dupes] ],"\n",sep="",collapse="\n"),
        "\nAdjust ModelScript pattern in configuration\n",
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
    visioneval::writeLog("findModel: Must provide modelPath locator.",Level="warn")
    return( find_ls )
  }

  # Establish the local name for run_model.R
  # VisionEval.R startup will set ModelScript from site configuration file
  #   located in runtime root (first of .visioneval, VisionEval.ini, VisionEval.cnf)
  #   with same default as here ("run_model.R")
  searchScript <- visioneval::getRunParameter("ModelScript",Param_ls=Param_ls)

  # TODO: add new style model_setup.yml file as an alternative (only in ModelDir)

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

  # TODO: replace stages architecture with new BaseModel / RunStep architecture
  
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

  # Set up destination modelPath
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

ve.model.copy <- function(newName=NULL,newPath=NULL) {
  # TODO: leave behind the "results" and "outputs"
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

# Helper function:
# parse a table of package/module inputs from assembled AllSpecs_ls
# returns a data.frame summarizing the inputs

# Unique Names in specifications:
# Not all names will be present in all specs
specNames <- c(
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
    spc <- packmod$Specs
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
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  workingResultsDir <- file.path(self$modelPath,ResultsDir)
  if ( ! dir.exists(workingResultsDir) ) {
    dir.create(workingResultsDir,showWarnings=FALSE)
  }
  ModelStateFileName <- visioneval::getRunParameter("ModelStateFileName",Param_ls=self$RunParam_ls)
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

  for ( stage in 1:self$stageCount ) {
    stagePath <- self$stagePaths[stage]

    if ( self$stageCount>1 ) {
      visioneval::writeLog(paste(initMsg,stage,":",stagePath),Level="debug")
    }
    modelState <- normalizePath(file.path(workingResultsDir,stagePath,ModelStateFileName),winslash="/",mustWork=FALSE)
    if ( visioneval::loadModelState(modelState, ( ms.env <- new.env() )) ) {
      # Attempt to load existing ModelState
      self$ModelState[[ basename(stagePath) ]] <- ms.env$ModelState_ls
      if ( "RunStatus" %in% ms.env$ModelState_ls ) {
        self$runStatus[stage] <- ms.env$ModelState_ls$RunStatus
      } else {
        self$runStatus[stage] <- "Prior Run"
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
        LogLevel=log
      )
      stageIndex <- toupper(basename(stagePath))

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
      if ( ! is.null(Param_ls$LoadDstoreName) ) {
        LoadEnv <- new.env()
        LoadDstoreDir <- dirname(Param_ls$LoadDstoreName) # Null if not set during initializeModel
        LoadEnv$ModelState_ls <- self$ModelState[[toupper(basename(LoadDstoreDir))]]
        if ( is.null(LoadEnv$ModelState_ls) ) {
          ModelStateFileName <- getRunParameter("ModelStateFileName",Param_ls=Param_ls)
          modelStatePath <- file.path(LoadDstoreDir,ModelStateFileName) # TODO: Unpack LoadDstoreDir from InitializeModel arguments
          loadModelState(modelStatePath,envir=LoadEnv)
        }
        if ( "RequiredVEPackages" %in% names(LoadEnv$ModelState_ls) ) {
          AlreadyInitialized <- LoadEnv$ModelState_ls$RequiredVEPackages
          RequiredPackages <- unique(c(RequiredPackages, AlreadyInitialized))
        }
      }
      allSpecs <- visioneval::parseModuleCalls(parsedScript$ModuleCalls_df, AlreadyInitialized, RequiredPackages, Save=FALSE)
      self$specSummary[[stageIndex]] <- summarizeSpecs(allSpecs)
    }
  }

  if ( length(self$ModelState)!=self$stageCount ) {
    self$status <- "Failed to Load"
    stop(
      visioneval::writeLog(
        c(
          "Failed to load all ModelStates",
          "ModelState list not the same length as stagePaths list: ",
          length(self$ModelState)," ",self$stageCount
        ),
        Level="error"
      )
    )
  } else {
    self$status <- self$runStatus[length(self$runStatus)]
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
    stop(
      visioneval::writeLog(
        c(
          paste0("No VisionEval model (",modelPaths$runModelName,") found at"),
          modelPath
        ), Level="error"
      )
    )
  }

  # Set up the model name, folder tree and script files
  self$RunParam_ls <- modelPaths$RunParam_ls; # Updated from model configuration file, if any
  self$modelPath <- modelPaths$modelPath
  self$modelName <- basename(self$modelPath)
  self$stagePaths <- modelPaths$stagePaths; # named stage names (folders with run_model.R, relative to ModelPath)
  self$stageScripts <- modelPaths$stageScripts; # actual names of ModelScriptFile for each stage
  self$stageCount <- length(self$stagePaths)

  initMsg <- paste("Loading Model",self$modelName)
  visioneval::writeLog(initMsg,Level="info")

  private$loadModelState(log=log) # load bare bones...

  visioneval::writeLogMessage(self$modelPath)
  visioneval::writeLog("Model Load Complete.",Level="info")
  invisible(self$status)
}

# Function to inspect the model configuration/setup parameters
ve.model.setup <- function(show="values", src=NULL, namelist=NULL, pattern=NULL) {
  # "show" is a character vector describing what to return (default, "values" defined in self$RunParam_ls)
  #   "name" - names of settings (character vector of matching names)
  #   "value" - named list of settings present in self$RunParam_ls (or defaults) (DEFAULT)
  #   "source" - return the source of the parameter
  # "show" also describes where to look
  #   "runtime" - use just the list from VEModel::getSetup() (optionally including undefined defaults)
  #   "defaults" - include the list of parameter defaults as if they were defined
  #       (overridden by any that actually are defined)
  #   "self" - returns the model specification (optionally including undefined defaults)
  #   If only one of "source", "name", "value":
  #      return a named vector (parameter names; corresponding element)
  #   If both "source" and "value":
  #      return a data.frame (row.names == "name")
  #   If both "source" (or "value") and "name":
  #      return a data.frame with a "name" column plus either source or value
  #   If all of "source", "name", "value":
  #      return a data.frame of all three
  # If src is a character vector, find the parameter sources that match any of the elements
  #   as a regular expression.
  # If pattern is a character vector, look for names in the setup that match any of the
  #  vector elements (as regular expressions)
  # If namelist is a character vector, treat each item as a name and return the list of items from
  #  the model setup (but dip into the full hierarchy, unless "src" is also set)

  where.options <- c("defaults","self","runtime")
  where.to.look <- where.options %in% show;
  if ( ! any(where.to.look) ) {
    where.to.look <- c(TRUE,TRUE,FALSE) # self + system defaults
  } else if ( where.to.look[3] ) {
    # runtime requested - deselect "self"
    where.to.look[2] <- FALSE
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
  sought <- searchParams_ls[matching]

  return.options <- c("name","value","source")
  what.to.return <- return.options %in% show
  if ( ! any(what.to.return) ) what.to.return <- c(FALSE,TRUE,FALSE) # just values
  how.many.to.return <- length(which(what.to.return))

  results <- data.frame( # columns in same order as return.options
    Parameter = names(sought),
    Value     = sought,
    Source    = attr(sought,"source")
  )
  results <- results[,what.to.return] # logical indexing into columns
  if ( is.data.frame(results) ) {
    row.names(results) <- results$Parameter
  } else {
    names(results) <- results$Parameter; # One column dropped data.frame to vector
  }

  # Don't print results if we used this function to set parameters
  return(results)
}

ve.model.save <- function(FileName="visioneval.cnf") {
  # Write self$RunParam_ls into file.path(self$modelPath,FileName) (YAML format).
  # Do not include default values or those in the runtime setup
  # Use the "setup" function to limit what is shown.
  # Can provide alternate name (but if it's not part of the known name set, it's 'just for reference')
  normalizePath(FileName,winslash="/",mustWork=FALSE)
  yaml::write_yaml(self$RunParam_ls,FileName,indent=2)
  invisible(self$RunParam_ls)
}

# Run the modelPath (through its stages)
# Find the runModel script
# Work in the Results directory - need to relay locations from here to initializeModel
# TODO: replace the "stages" subdirectories with the more flexible concept of a "BaseModel"
#   which provides Datastore components previously computed (and possibly the entire run_model.R
#   script).
ve.model.run <- function(stage=NULL,lastStage=NULL,log="warn") {
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

  # Set up default logging
  visioneval::initLog(Save=FALSE,Threshold=log)

  # Set up model constants for running multiple stages
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=self$RunParam_ls)
  workingResultsDir <- file.path(self$modelPath,ResultsDir)
  if ( ! dir.exists(workingResultsDir) ) {
    dir.create(workingResultsDir,showWarnings=FALSE)
  }

  BaseInputPath <- visioneval::getRunParameter("InputPath",Param_ls=self$RunParam_ls)
  if ( ! isAbsolutePath(BaseInputPath) ) {
    BaseInputPath <- normalizePath(file.path(self$modelPath,BaseInputPath),winslash="/",mustWork=FALSE)
  }

  owd <- setwd(workingResultsDir)
  on.exit(setwd(owd))

  # Set up the model runtime environment
  for ( ms in stageStart:lastStage ) {
    stagePath <- self$stagePaths[ms]
    scriptFile <- self$stageScripts[ms]
    stageInput <- file.path(self$modelPath,stagePath)

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
              PreviousState=self$ModelState, # previously loaded model states
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
              self$ModelState[[ toupper(basename(stagePath)) ]] <- ve.model$ModelState_ls
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
#   show how many independent files there are.
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
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$RunParam_ls$DatastoreType,"\n")
  cat("Status:", self$status,"\n")
  self$status
}

# create a VEResults object (possibly invalid/empty) based on the current model
#   run or a particular model stage.
ve.model.results <- function(stage) {
  # Create a results object wrapping the directory that contains the model
  # results for the given stage (or last stage if not given)
  if ( missing(stage) || !is.numeric(stage) ) {
    stage <- self$stageCount
  } else if ( length(stage)>1 ) {
    stage <- stage[length(stage)] # use last one listed
  }
  stagePath <- self$stagePaths[stage]
  visioneval::writeLog(paste("Loading Results for Model Stage:",stagePath),Level="info")
  Param_ls <- self$ModelState[[stage]]$RunParam_ls
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Param_ls=Param_ls)
  if ( ! dir.exists( file.path(self$modelPath,ResultsDir) ) ) {
    ResultsDir = "."
  } # Old style model run won't have ResultsDir in Param_ls, nor the right default
  resultsPath <- normalizePath(   # may be NULL!
    file.path(self$modelPath,ResultsDir,stagePath),
    winslash="/",
    mustWork=FALSE
  );
  
  results <- VEResults$new(resultsPath,self$modelPath,Param_ls)
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
ve.model.query <- function(QueryName=NULL,FileName=NULL) {
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
    print(queries)
    return(NULL)
  }
  # Let VEquery find the query...
  return(
    VEQuery$new(
      ModelPath=self$modelPath,
      QueryName=QueryName,
      FileName=FileName,
      QueryDir=QueryDir,
      Param_ls=self$RunParam_ls
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
    run=ve.model.run,                       # run a model (or just a subset of stages)
    print=ve.model.print,                   # provides generic print functionality
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    setup=ve.model.setup,                   # set or list model parameters and their sources
    save=ve.model.save,                     # save changes to the model setup that were created locally (by source)
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    results=ve.model.results,               # Create a VEResults object (if model is run); option to open a past result
    query=ve.model.query                    # Create a VEQuery object (or show a list of queries).
  ),
  private = list(
    # Private Members
    runError=NULL,
    lastResults=list(),                      # Cache previous results object
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
