#================
#initialization.R
#================

#This script defines various functions that are invoked to initialize a model
#run. Several of the functions are also invoked when modules are run.

# Get rid of the global function references
utils::globalVariables(c("initTable","writeToTable","ModelState_ls","listDatastore"))


#CREATE R ENVIRONMENT FOR MODEL
#==============================
#' Attach an R environment to the search path for the active model
#'
#' \code{modelEnvironment} a visioneval framework control function that locates
#' the environment for elements of the active model, creating it and placing it
#' on the search path if necessary. That environment contains the ModelState,
#' the Datastore access function aliases and related information. If create is
#' \code{FALSE}, throw an error instead of creating a new environment
#' if it does not already exist. The environment should be emptied and
#' recreated whenever a new model is initialized, either to load or run
#' it.
#'
#' @param Create a logical indicating whether a missing model environment
#' should be created.
#' @return an R environment attached to "ve.model" on the search path.
#' @export
modelEnvironment <- function(Create=TRUE) {
  # export this function since it can be useful in the VEModel
  # package
  if ( ! "ve.model" %in% search() ) {
    if ( ! Create ) stop("Missing ve.model environment.")
    ve.model <- attach(NULL,name="ve.model")
  } else {
    ve.model <- as.environment("ve.model")
  }
  return(ve.model)
}

#GET VISIONEVAL RUN STEPS
#========================
#' Get end user API run mode.
#'
#' \code{getRunMode} a visioneval framework control function
#' that controls how initializeModel and runModule/runScript/Stage
#' operations are processed (see visioneval::initializeModel)
#'
#' The default for RunMode runs all steps.
#'
#' @param RunMode list of values identifying initialization steps.
#' @return vector subset of c("Load","Run")
getRunMode <- function(RunMode=NULL) {
  envir=modelEnvironment()
  runSteps <- c("Load","Run")
  if ( ! is.null(RunMode) && length(RunMode)>0 && all(nzchar(RunMode)) ) {
    mode <- runSteps[ runSteps %in% RunMode ]
    if ( length(mode)>0 ) runSteps <- mode
  } else {
    VEInitializeMode <- get0("RunMode",envir=envir,inherits=FALSE,ifnotfound=NULL)
    if ( !is.null(VEInitializeMode) ) {
      runSteps <- VEInitializeMode
    }
  }
  return ( envir$runSteps <- runSteps )
}

#INITIALIZE MODEL STATE
#======================
#' Initialize model state.
#'
#' \code{initModelStateFile} a visioneval framework control function that loads
#' model run parameters into the model state list in the global workspace and
#' optionally saves it as file.
#'
#' This function creates the model state list and loads model run parameters
#' recorded in the 'parameters.json' file into the model state list. It also
#' optionally saves the model state list in a file (ModelState.Rda).
#'
#' @param Save A logical (default=TRUE) indicating whether the model state file
#'   should be written out.
#' @param ... Additional named parameters that will override VisionEval.cnf or
#'   run_parameters.json. These historically include:
#' Dir [A string identifying the name of the directory where the global
#' parameters, deflator, and default units files are located. The default value
#' is defs.]
#' ParamFile [A string identifying the name of the global parameters file.
#' The default value is run_parameters.json.]
#' DeflatorFile [A string identifying the name of the file which contains
#' deflator values by year (e.g. consumer price index). The default value is
#' deflators.csv.]
#' UnitsFile [A string identifying the name of the file which contains
#' default units for complex data types (e.g. currency, distance, speed, etc.).
#' The default value is units.csv.]

#' @return TRUE if the model state list is created and file is saved, FALSE if
#' the model state file was not saved.
#' @export
#' @import jsonlite
initModelStateFile <- function(Save=TRUE,...) {
  #   Obtain run parameters values RunParam_ls in ve.model environment (if it already exists),
  #   otherwise create a new empty list of run parameters. VEModel API creates ve.model and for
  #   backward compatibility, the modelEnvironment() function will create it if it does not exist.
  #   The VEModel API will set InitialValues from VisionEval.cnf in ve.runtime
  #   VEModel API will override values using VisionEval.cnf in the run_model.R folder
  #   This function furnishes default values for the Dir/ParamDir and ParamFile if they are not
  #   defined already in RunParam_ls (maps Dir into ParamDir) or not in list(...)
  #   This function opens ParamDir/ParamFile and adds run parameters found there
  RunParam_ls <- get0("RunParam_ls",envir=modelEnvironment(),inherits=FALSE,ifnotfound=list())
  dotParams_ls <- list(...)
  # Replace any items in RunParam_ls from items in dotParams_ls
  RunParam_ls[ names(dotParams_ls) ] <- dotParams_ls
  if ( ! "ParamDir" %in% names(RunParam_ls) ) {
    if ( "Dir" %in% names(RunParam_ls) ) {
      RunParam_ls$ParamDir = RunParam_ls$Dir
    }
  }
  ParamLocations <- c("ParamFile","ParamDir")
  defined <- ParamLocations %in% names(RunParam_ls)
  if ( ! all( defined ) ) {
    # Set parameter file default values
    RunParam_ls[ ! defined ] <- list(ParamFile="run_parameters.json",ParamDir="defs")[ ! defined ]
  }

  # Check for existence of run_parameters.json
  # Eventually, we'll make its non-existence NOT an error
  # Though we will still need all the required elements defined somewhere else.
  ParamFilePath <- file.path(RunParam_ls$ParamDir,  RunParam_ls$ParamFile)
  if ( ! file.exists(ParamFilePath) ) {
    Message <- paste("Missing parameter file: ", ParamFilePath)
    stop( writeLog(Message,Level="error") )
  }

  # Override elements not defined in dots with elements from ParamFile_ls
  # May replace elements defined in VisionEval.conf or otherwise in ve.model$RunParam_ls
  # Also can't override ParamLocations (we've already used them so it's too late)
  ParamFile_ls <- jsonlite::fromJSON(ParamFilePath)
  ParamFile_ls <- ParamFile_ls[ names(ParamFile_ls)[ ! ( names(ParamFile_ls) %in% c(ParamLocations, names(dotParams_ls)) ) ] ]
  RunParam_ls[ names(ParamFile_ls) ] <- (ParamFile_ls)

  #   Furnish default values for DeflatorFile, UnitsFile, ModelParamFile and GeoFile if not defined
  #   in list(...). The default for InputDir is backward compatible. The VEModel$run function will
  #   set it to a vector of places to look (effectively a PATH) - when an input file is sought,
  #   the first version of the file encountered on the list of directories will be used.
  DefFiles  <- list( # a named list, not a character vector
    DeflatorFile   = "deflators.csv",
    UnitsFile      = "units.csv",
    GeoFile        = "geo.csv",
    ModelParamFile = "model_parameters.json",
    InputDir       = "inputs"
  )
  missingDefs <- ! names(DefFiles) %in% names(RunParam_ls)
  RunParam_ls[ names(DefFiles)[missingDefs] ] <- DefFiles[missingDefs]
  
  # The required parameters will be the initial elements for the ModelState
  # Other RunParam_ls elements will be placed in ModelState_ls$RunParam_ls
  RequiredParam_ <- c(
    "Model", "Scenario", "Description", "Region", "BaseYear", "Years",
    "DatastoreName", "Seed"
  )
  ParamExists_ <- RequiredParam_ %in% names(RunParam_ls)
  if (any(!ParamExists_)) {
    MissingParam_ <- RequiredParam_[!ParamExists_]
    Message <- c(
      "Missing model run parameters (not set in VisionEval.cnf or run_parameters.json or call to initializeModel):",
      paste(MissingParam_, collapse = ", ")
    )
    stop( writeLog(Message,Level="error") )
  } 
  # Now install the parameters - the required parameters become the foundation for
  # ModelState_ls. Other parameters are placed in newModelState_ls$RunParameters,
  # (including things like ParamDir, UnitsFile, etc.)
  newModelState_ls <- RunParam_ls[RequiredParam_]
  newModelState_ls$RunParam_ls <- RunParam_ls[ ! (names(RunParam_ls) %in% RequiredParam_) ]
  newModelState_ls$LastChanged <- Sys.time()

  DeflatorFilePath <- file.path(RunParam_ls$ParamDir, RunParam_ls$DeflatorFile)
  if ( ! file.exists(DeflatorFilePath) ) {
    stop( writeLog(paste("Could not locate DeflatorFile:",DeflatorFilePath),Level="error") )
  }
  newModelState_ls$Deflators <- read.csv(DeflatorFilePath, as.is = TRUE)

  UnitsFilePath <- file.path(RunParam_ls$ParamDir, RunParam_ls$UnitsFile)
  if ( ! file.exists(UnitsFilePath) ) {
    stop( writeLog(paste("Could not locate DeflatorFile:",UnitsFilePath),Level="error") )
  }
  newModelState_ls$Units <- read.csv(UnitsFilePath, as.is = TRUE)

  envir = modelEnvironment()
  envir$ModelState_ls <- newModelState_ls
  envir$RunParam_ls <- RunParam_ls
  if ( Save) save(ModelState_ls, envir=envir, file = getModelStateFileName())

  return(Save) 
}
#initModelStateFile(ParamDir = "tests/defs")

#LOAD MODEL STATE
#================
#' Load ModelState into ve.model environment
#'
#' \code{loadModelState} a visioneval framework control function that
#' loads the ModelState for a model run into the ve.model environment
#'
#' @param FileName A string identifying the name of the file that contains
#' the ModelState_ls list. The default name is 'ModelState.Rda'.
#' @param envir An environment into which to load ModelState.Rda
#' (default ve.model)
#' @return TRUE if ModelState was loaded, FALSE if it could not be
#  (invisibly)
#' @export
loadModelState <- function(FileName=getModelStateFileName(),envir=NULL) {
  if ( is.null(envir) ) envir = modelEnvironment()
  # TODO: do some error checking on what "envir" is
  # TODO: use a function to find default ModelState FileName
  if ( ! "ModelState_ls" %in% ls(envir) ) {
    if (file.exists(FileName)) {
      load(FileName,envir=envir)
    }
  }
  invisible( "ModelState_ls" %in% ls(envir) )
}

# GET MODEL STATE
#================
#' Get elements from ModelState in ve.model environment (option to Load)
#'
#' \code{getModelState} a visioneval framework control function that
#' gets ModelState elements from ModelState_ls in the ve.model environment
#'
#' Use this faster function inside of modules rather than readModelState, which
#' can load the ModelState.Rda or work with different environments.
#' Note that it just becomes a call to readModelState if any
#' parameters are passed (see \code{readModelState}).
#'
#' @param ... If there are any parameters, this quietly becomes a call to
#' readModelState(...) which can subset, read a different file, load
#' into an environment, etc.
#' @return The model state list
#' @export
getModelState <- function(...) {
  if ( ! missing(...) ) return(readModelState(...))
  envir <- modelEnvironment()
  if ( ! "ModelState_ls" %in% ls(envir) ) stop("getModelState: ModelState is not initialized.")
  return(envir$ModelState_ls)
}

#SET (UPDATE) MODEL STATE
#==================
#' Update model state.
#'
#' \code{setModelState} a visioneval framework control function that updates the
#' list that keeps track of the model state with list of components to update
#' and resaves in the model state file.
#'
#' Key variables that are important for managing the model run are stored in a
#' list (ModelState_ls) that is in the global workspace and saved in the
#' 'ModelState.Rda' file. This function updates  entries in the model state list
#' with a supplied named list of values, and then saves the results in the file.
#'
#' @param ChangeState_ls A named list of components to change in ModelState_ls.
#'   If empty, just Save (and if Save==FALSE, do nothing)
#' @param FileName A string identifying the name of the file in which to save
#' the ModelState_ls list. The default name is 'ModelState.Rda'.
#' @param Save A boolean (default TRUE) saying whether to save the
#' ModelState to its file (otherwise only ModelState_ls in ve.model environment is updated)
#' @return always TRUE
#' @export
setModelState <-
function(ChangeState_ls=list(), FileName = getModelStateFileName(), Save=TRUE) {
  # Get current ModelState (providing FileName will force a "Read")
  changeModelState_ls <- readModelState(FileName=FileName)
  envir=modelEnvironment()

  # Make requested changes, if any
  # (pass an empty list just to Save existing ModelState_Ls)
  if ( (changes <- length(ChangeState_ls)) > 0 ) {
    changeNames <- names(ChangeState_ls)
    for (i in 1:changes) {
      changeModelState_ls[[changeNames[i]]] <- ChangeState_ls[[i]]
    }
    changeModelState_ls$LastChanged <- Sys.time()
    envir$ModelState_ls <- changeModelState_ls
  }

  if ( Save ) {
    result <- try(save("ModelState_ls",envir=envir,file=FileName))
    if ( class(result) == 'try-error' ) {
      Msg <- paste('Could not write ModelState:', FileName)
      writeLog(Msg,Level="error")
      writeLog(result,Level="error")
      stop(Msg,call.=FALSE)
    }
  }
  invisible(TRUE)
}

#GET A VISIONEVAL OPTION FROM CONFIGURATION
#==========================================
#' Fetch a VisionEval option (providing a default)
#'
#' \code{getVEOption} a visioneval framework control function that
#' locates and returns the value for a VisionEval option
#'
#' @param OptionName a character string naming an option
#' @param OptionDefault an R object providing a default value for the
#'   option if it is not set
#' @param SetOption a logical; if TRUE, remember the option in ve.model$VEOptions
#' @return the value of the option (or its default value)
#' @export
getVEOption <- function(OptionName=NULL,OptionDefault=NULL,SetOption=FALSE) {
  # TODO: Look in the following places (use first one found)
  # Existing value in R option at paste0("visioneval.",OptionName)
  # Existing option value from VEOptions in "ve.model" environment
  # VisionEval.ini in RunDirectory/getwd()
  #   (side effect: load any not already set in ve.model$VEOptions)
  # VisionEval.ini in ve.runtime
  #   (side effect: load any not already set in ve.model$VEOptions)
  # OptionDefault

  # TODO: create a setVEOption (where OptionName may be a vector,
  #   in which case OptionDefault must be a named list of the same length)
  #   OptionName[n] is set to OptionDefault[[OptionName]]
  #   Could also just pull OptionName from names(OptionDefault)
  #   Or create a separate function called setVEOption
  # TODO: allow setVEOption to create a VisionEval.ini file

  Option <- OptionDefault
  if ( SetOption) {
    # create ve.model$VEOptions if it doesn't exist
    # assign ve.model$VEOptions[[OptionName]]
  }
  return(Option)
}

#IDENTIFY MODEL STATE FILE
#=========================
#' Get model state file name and attach attribute saying if it exists
#'
#' \code{getModelStateFileName} a visioneval framework control function that
#' reports the (user-configurable model state file name and reports
#' whether the files exists (\code{attribute("exists")})
#'
#' @return a character string containing the standard ModelState (base)name and an
#' attribute \code{"exists"} saying if it exists.
#' @export
getModelStateFileName <- function() {
  # TODO: manage the complete model state path
  envir <- modelEnvironment()
  # use the basename - not legal (yet) to specific any directory
  # other than getwd()
  modelStateName <- get0(
    "ModelStateName", envir=envir, inherits=FALSE,
    ifnotfound={ envir$ModelStateName <- basename(getVEOption("ModelStateName", "ModelState.Rda")) }
  )
  # The exists attribute is fluid
  return(modelStateName)
}

#READ MODEL STATE FILE
#=====================
#' Reads values from model state file (attempting to load if not present)
#'
#' \code{readModelState} a visioneval framework control function that reads
#' components of the file that saves a copy of the model state.
#'
#' The model state is stored in a list (ModelState_ls) that is also saved as a
#' file (ModelState.Rda) whenever the list is updated. This function reads the
#' contents of the ModelState.Rda file.
#'
#' @param Names_ A string vector of the components to extract from the
#' ModelState_ls list.
#' @param FileName A string vector with the full path name of the model state
#' file.
#' @param envir An environment into which to load ModelState.Rda
#' @return A list containing the specified components from the model state file.
#' @export
readModelState <- function(Names_ = "All", FileName=NULL, envir=NULL) {
  # Establish environment
  if ( is.null(envir) ) envir <- modelEnvironment()
  # Load from FileName if we explicitly provide it, or if we do not already
  # have a ModelState_ls in the environment
  if ( !is.null(FileName) || ! exists("ModelState_ls",envir=envir,inherits=FALSE) ) {
    if ( is.null(FileName) ) FileName <- getModelStateFileName()
    if ( ! loadModelState(FileName,envir) ) {
      Msg <- paste0("Could not load ModelState from",FileName)
      writeLog(Msg,Level="error")
      stop(Msg,call.=FALSE)
    }
  }
  State_ls <- get0("ModelState_ls",envir=envir,ifnotfound=list())
  if (Names_[1] == "All") {
    return(State_ls)
  } else {
    return(State_ls[Names_])
  }
}

#RETRIEVE YEARS
#==============
#' Retrieve years
#'
#' \code{getYears} a visioneval framework model user function that reads the
#' Years component from the the model state file.
#'
#' This is a convenience function to make it easier to retrieve the Years
#' component of the model state file which lists all of the specified model run
#' years. If the Years component includes the base year, then the returned
#' vector of years places the base year first in the order. This ordering is
#' important because some modules calculate future year values by pivoting off
#' of base year values so the base year must be run first.
#'
#' @return A character vector of the model run years.
#' @export
getYears <- function() {
  BaseYear <- unlist(readModelState("BaseYear"))
  Years <- unlist(readModelState("Years"))
  if (BaseYear %in% Years) {
    c(BaseYear, Years[!Years %in% BaseYear])
  } else {
    Years
  }
}

# TODO: somewhere north of here, something is not closed properly...

#RETRIEVE DEFAULT UNITS
#======================
#' Retrieve default units for model
#'
#' \code{getUnits} a visioneval framework control function that retrieves the
#' default model units for a vector of complex data types.
#'
#' This is a convenience function to make it easier to retrieve the default
#' units for a complex data type (e.g. distance, volume, speed). The default
#' units are the units used to store the complex data type in the datastore.
#'
#' @param Type_ A string vector identifying the complex data type(s).
#' @return A string vector identifying the default units for the complex data
#' type(s) or NA if any of the type(s) are not defined.
#' @export
getUnits <- function(Type_) {
  Units_df <- getModelState()$Units
  Units_ <- Units_df$Units
  names(Units_) <- Units_df$Type
  Result_ <- Units_[Type_]
  if (any(is.na(Result_))) Result_ <- NA
  Result_
}
#getUnits("Bogus")
#getUnits("currency")
#getUnits("area")

#INITIALIZE RUN LOG
#==================
#' Initialize run log.
#'
#' \code{initLog} a visioneval framework control function that creates a log
#' (text file) that stores messages generated during a model run.
#'
#' This function creates a log file that is a text file which stores messages
#' generated during a model run. The name of the log is 'Log <date> <time>'
#' where '<date>' is the initialization date and '<time>' is the initialization
#' time. The log is initialized with the scenario name, scenario description and
#' the date and time of initialization.
#'
#' @param TimeStamp Force the log message time stamp to this value
#' (default: \code{Sys.time()})
#' @param Threshold Logging threshold to display (see
#' \code{writeLog()} for available levels). Messages below the
#' threshold will be ignored. Default is "info" which shows a lot.
#' "warn" is typical for running a model, and "error" for only the
#' worst.
#' @param Suffix A character string appended to the file name for the log file.
#' For example, if the suffix is 'CreateHouseholds', the log file is named
#' 'Log_CreateHouseholds.txt'. The default value is NULL in which case the
#' suffix is the date and time.
#' @return TRUE if the log is created successfully. It creates a log file in the
#'   working directory and identifies the name of the log file in the
#'   model state file.
#' @export
initLog <- function(TimeStamp = NULL, Threshold="info", Suffix = NULL) {
  # TODO: integrate with running a model from VEModel$run
  # TODO: also ensure that it is backward compatible if just sourcing run_model.R
  # TODO: use ve.env to grab the active logger name

  if (is.null(TimeStamp)) {
    TimeStamp <- as.character(Sys.time())
  }
  LogInitTime <- gsub(" ","_",TimeStamp)

  if (!is.null(Suffix)) {
    LogFile <- paste0("Log_", Suffix, ".txt")
  } else {
    LogFile <- paste0("Log_", gsub(":", "_", LogInitTime), ".txt")
  }
  # Set up logger and threshold in the model environment
  envir=modelEnvironment()
  envir$ve.logger <- "ve.logger"
  if ( ! exists("ve.log.threshold",envir=envir) ) {
    # ve.log.threshold set manually is preserved
    if ( toupper(Threshold) %in% names(log.threshold) ) {
      envir$ve.log.threshold <- log.threshold[toupper(Threshold)]
    }
  }

  # Create and provision the ve.logger
  futile.logger::flog.appender(futile.logger::appender.tee(LogFile),name=envir$ve.logger)
  futile.logger::flog.threshold(envir$ve.log.threshold, name=envir$ve.logger)

  # Log header
  futile.logger::flog.layout( function(level,msg,...) cat(msg,...,"\n"), name=envir$ve.logger )
  # The following are not errors, but give them that log level to ensure they get out
  futile.logger::flog.error("Model Run:",LogInitTime,name=envir$ve.logger)

  # Log standard format
  futile.logger::flog.layout( layout.visioneval,name=envir$ve.logger)

  invisible(list(LogFile=LogFile,ModelStart=TimeStamp))
}
#initLog()

# futile.logger layout for visioneval (adjusted from futile.logger::layout.simple)

prepare_arg <- function(x) {
  if (is.null(x) || length(x) == 0) return(deparse(x))
  return(x)
}

layout.visioneval <- function(level, msg, id='', ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), prepare_arg)
    msg <- do.call(sprintf, c(msg, parsed))
  }
  return( sprintf("[%s] %s : %s\n", substr(names(level),1,1), the.time, msg) )
}

# Internal log level translation table
log.threshold <- c(
    futile.logger::INFO,
    futile.logger::TRACE,
    futile.logger::DEBUG,
    futile.logger::WARN,
    futile.logger::ERROR,
    futile.logger::FATAL
  )
log.function <- list(
    INFO  = futile.logger::flog.info,
    TRACE = futile.logger::flog.trace,
    DEBUG = futile.logger::flog.debug,
    WARN  = futile.logger::flog.warn,
    ERROR = futile.logger::flog.error,
    FATAL = futile.logger::flog.fatal
  )

#WRITE TO LOG
#============
#' Write to log.
#'
#' \code{writeLog} a visioneval framework control function that writes a message
#' to the run log.
#'
#' This function logs messages to the VisionEval log, which will be stdout
#' (the console) if not running a model, and the model log file if a model
#' is running.
#'
#' A character vector may be provided for Msg, in which case each
#' element of the vector will become one line in the log.
#'
#' @param Msg A character vector, whose first element must not be empty.
#' @param Level character string identifying log level
#' @param Logger character string identifying where to send the log message
#' @return TRUE if the message is written to the log successfully.
#' It appends the time and the message text to the run log.
#' @export
writeLog <- function(Msg = "", Level="Info", Logger="") {
  if ( missing(Msg) || ! nzchar(Msg) ) {
    message(
      "writeLog(Msg,Level,Logger): No message supplied\n",
      "Available Log Levels:\n",
      paste(names(log.threshold),collapse=", ")
    )
  } else {
    # empty Logger logs to ROOT (console, unless appender changed)
    # named logger logs to that one, if defined, otherwise ROOT
    # initLog will define ve.logger
    Level <- toupper(Level)
    if ( ! Level %in% names(log.function) ) Level="WARN"
    if ( ! nzchar(Logger) && exists("ve.logger") ) Logger=get("ve.logger") # to avoid global name definition failure
    for ( m in Msg ) {
      log.function[[Level]](m,name=Logger)
    }
  }
  invisible(Msg)
}

#LOAD SAVED DATASTORE
#====================
#' Load saved datastore.
#'
#' \code{loadDatastore} a visioneval framework control function that copies an
#' existing saved datastore and writes information to run environment.
#'
#' This function copies a saved datastore as the working datastore attributes
#' the global list with related geographic information. This function enables
#' scenario variants to be built from a constant set of starting conditions.
#'
#' @param FileToLoad A string identifying the full path name to the saved
#'   datastore. Path name can either be relative to the working directory or
#'   absolute.
#' @param Dir A string identifying the path of the geography definition file (GeoFile),
#'   default to 'defs' relative to the working directory
#' @param GeoFile A string identifying the name of the geography definition file
#'   (see 'readGeography' function) that is consistent with the saved datastore.
#'   The geography definition file must be located in the 'defs' directory.
#' @param SaveDatastore A logical identifying whether an existing datastore
#'   will be saved. It is renamed by appending the system time to the name. The
#'   default value is TRUE.
#' @return TRUE if the datastore is loaded. It copies the saved datastore to
#'   working directory as 'datastore.h5'. If a 'datastore.h5' file already
#'   exists, it first renames that file as 'archive-datastore.h5'. The function
#'   updates information in the model state file regarding the model geography
#'   and the contents of the loaded datastore. If the stored file does not exist
#'   an error is thrown.
#' @export
loadDatastore <- function(FileToLoad, Dir="defs/", GeoFile, SaveDatastore = TRUE) {
  GeoFile <- file.path(Dir, GeoFile)
  G <- getModelState()
  #If data store exists, rename
  DatastoreName <- G$DatastoreName
  if (file.exists(DatastoreName) & SaveDatastore) {
    TimeString <- gsub(" ", "_", as.character(Sys.time()))
    ArchiveDatastoreName <-
      paste0(unlist(strsplit(DatastoreName, "\\."))[1],
            "_", TimeString, ".",
            unlist(strsplit(DatastoreName, "\\."))[2])
    ArchiveDatastoreName <- gsub(":", "-", ArchiveDatastoreName)
    file.copy(DatastoreName, ArchiveDatastoreName)
  }
  if (file.exists(FileToLoad)) {
    file.copy(FileToLoad, DatastoreName)
    Geo_df <- read.csv(GeoFile, colClasses = "character")
    Update_ls <- list()
    Update_ls$BzoneSpecified <- !all(is.na(Geo_df$Bzone))
    Update_ls$CzoneSpecified <- !all(is.na(Geo_df$Czone))
    Update_ls$Geo_df <- Geo_df
    setModelState(Update_ls)
    listDatastore()
  } else {
    Message <- paste("File", FileToLoad, "not found.")
    writeLog(Message,Level="error")
    stop(Message)
  }
  TRUE
}


#READ GEOGRAPHIC SPECIFICATIONS
#==============================
#' Read geographic specifications.
#'
#' \code{readGeography} a visioneval framework control function that reads the
#' geographic specifications file for the model.
#'
#' This function manages the reading and error checking of geographic
#' specifications for the model. It calls the checkGeography function to check
#' for errors in the specifications. The checkGeography function reads in the
#' file and checks for errors. It returns a list of any errors that are found
#' and a data frame containing the geographic specifications. If errors are
#' found, the functions writes the errors to a log file and stops model
#' execution. If there are no errors, the function adds the geographic in the
#' geographic specifications file, the errors are written to the log file and
#' execution stops. If no errors are found, the geographic specifications are
#' added to the model state file.
#'
#' @param Save A logical (default=TRUE) indicating whether the model state
#'   should be saved to the model state file, or just updated in ve.model environment
#' @param ... Additional named arguments may explicitly override ParamDir and GeoFile
#'   (otherwise these will be sought in the ModelState after having been set in
#'   VisionEval.cnf or in run_parameters.json).
#' ParamDir [A string identifying the path to the geographic
#'   specifications file. Note: don't include the final separator in the
#'   path name 'e.g. not defs/'.]
#' GeoFile [A string identifying the name of the geographic
#'   specifications file. This is a csv-formatted text file which contains
#'   columns named 'Azone', 'Bzone', 'Czone', and 'Marea'. The 'Azone' column
#'   must have zone names in all rows. The 'Bzone' and 'Czone' columns can be
#'   unspecified (NA in all rows) or may have have unique names in every row.
#'   The 'Marea' column (referring to metropolitan areas) identifies
#'   metropolitan areas corresponding to the most detailed level of specified
#'   geography (or 'None' no metropolitan area occupies any portion of the
#'   zone.]
#' @return The value TRUE is returned if the function is successful at reading
#'   the file and the specifications are consistent. It stops if there are any
#'   errors in the specifications. All of the identified errors are written to
#'   the run log. A data frame containing the file entries is added to the
#'   model state file as Geo_df'.
#' @export
readGeography <- function(Save=TRUE,...) {
  #Check for errors in the geographic definitions file

  #Read in geographic definitions if file exists, otherwise error
  #--------------------------------------------------------------
  params <- list(...)
  Params_ls < readModelState()$RunParam_ls
  paramNames <- c("ParamDir","GeoFile")
  defined <- paramNames %in% Params_ls
  Params_ls <- Params_ls[paramNames[defined]] # sub-list of relevant parameters (possibly empty)
  if ( length(params) > 0 ) {
    override <- paramNames %in% params
    if ( any(override) ) {
      Params_ls[paramNames[override]] <- params[paramNames[override]]
      defined <- paramNames %in% Params_ls
    }
  }
  if ( ! all(defined) ) {
    Message <- paste("Missing file location elements for readGeograpy: ",paste(paramNames[!defined],collapse=","))
    writeLog(Message,Level="error")
    stop(Message)
  }

  Filepath <- file.path(Params_ls$ParamDir, Params_ls$GeoFile)
  if (file.exists(Filepath)) {
    Geo_df <- read.csv(Filepath, colClasses = "character")
  } else {
    Message <- paste("Missing", Params_ls$GeoFile, "in folder", Params_ls$ParamDir, ".")
    writeLog(Message,Level="error")
    stop(Message)
  }

  CheckResults_ls <- checkGeography(Geo_df)
  #Notify if any errors
  Messages_ <- CheckResults_ls$Messages
  if (length(Messages_) > 0) {
    writeLog(Messages_,Level="error")
    stop(paste0("One or more errors in ", Params_ls$GeoFile, ". See log for details."))
  } else {
    writeLog("Geographical indices successfully read.",Level="info")
  }
  #Update the model state file
  setModelState(CheckResults_ls$Update,Save=Save)
  TRUE
}


#CHECK GEOGRAPHIC SPECIFICATIONS
#===============================
#' Check geographic specifications.
#'
#' \code{checkGeography} a visioneval framework control function that checks
#' geographic specifications file for model.
#'
#' This function reads the file containing geographic specifications for the
#' model and checks the file entries to determine whether they are internally
#' consistent. This function is called by the readGeography function.
#'
#' @param Geo_df A data.frame containing a model geography description
#' @return A list having two components. The first component, 'Messages',
#' contains a string vector of error messages. It has a length of 0 if there are
#' no error messages. The second component, 'Update', is a list of components to
#' update in the model state file. The components of this list include: Geo, a
#' data frame that contains the geographic specifications; BzoneSpecified, a
#' logical identifying whether Bzones are specified; and CzoneSpecified, a
#' logical identifying whether Czones are specified.
#' @export
checkGeography <- function(Geo_df) {
  #Check that file has all required fields and extract field attributes
  #--------------------------------------------------------------------
  FieldNames_ <- c("Azone", "Bzone", "Czone", "Marea")
  if (!(all(names(Geo_df) %in% FieldNames_))) {
    Message <- "'geography.csv' is missing some required fields."
    writeLog(Message,Level="error")
    stop(Message)
  }
  #Check table entries
  #-------------------
  BzoneSpecified <- !all(is.na(Geo_df$Bzone))
  CzoneSpecified <- !all(is.na(Geo_df$Czone))
  Messages_ <- character(0)
  #Determine whether entries are correct if Bzones have not been specified
  if (!BzoneSpecified) {
    if (any(duplicated(Geo_df$Azone))) {
      DupAzone <- unique(Geo_df$Azone[duplicated(Geo_df$Azone)])
      Messages_ <- c(
        Messages_, paste0(
          "Duplicated Azone entries (",
          paste(DupAzone, collapse = ", "),
          ") not allowed when Bzones not specified."
        )
      )
    }
  }
  #Determine whether entries are correct if Bzones have been specified and
  #Czones are unspecified
  if (BzoneSpecified & !CzoneSpecified) {
    #Are Bzones completely specified
    if (any(is.na(Geo_df$Bzone))) {
      Messages_ <- c(Messages_,
                     "Either all Bzone entries must be NA or no Bzone entries must be NA.")
    }
    #Are any Bzone names duplicated
    if (any(duplicated(Geo_df$Bzone))) {
      DupBzone <- unique(Geo_df$Bzone[duplicated(Geo_df$Bzone)])
      Messages_ <- c(Messages_, paste0(
        "Duplicated Bzone entries (",
        paste(DupBzone, collapse = ", "),
        ") not allowed."
      ))
    }
    #Are metropolitan area designations consistent
    AzoneMareas_ <- tapply(Geo_df$Marea, Geo_df$Azone, unique)
    AzoneMareas_ <- lapply(AzoneMareas_, function(x) {
      x[x != "None"]
    })
    if (any(unlist(lapply(AzoneMareas_, length)) > 1)) {
      Messages_ <- c(Messages_,
                     "At least one Azone is assigned more than one Marea.")
    }
  }
  #Determine whether entries are correct if Czones have been specified
  if (CzoneSpecified) {
    #Are Czones completely specified
    if (any(is.na(Geo_df$Czone))) {
      Messages_ <- c(Messages_,
                     "Either all Czone entries must be NA or no Czone entries must be NA.")
    }
    #Are any Czone names duplicated
    if (any(duplicated(Geo_df$Czone))) {
      DupCzone <- unique(Geo_df$Czone[duplicated(Geo_df$Czone)])
      Messages_ <- c(Messages_, paste0(
        "Duplicated Czone entries (",
        paste(DupCzone, collapse = ", "),
        ") not allowed."
      ))
    }
    #Are metropolitan area designations consistent
    AzoneMareas_ <- tapply(Geo_df$Marea, Geo_df$Azone, unique)
    AzoneMareas_ <- lapply(AzoneMareas_, function(x) {
      x[x != "None"]
    })
    if (any(unlist(lapply(AzoneMareas_, length)) > 1)) {
      Messages_ <- c(Messages_,
                     "At least one Azone is assigned more than one Marea.")
    }
  }
  #Return messages and
  Update_ls <- list(Geo_df = Geo_df, BzoneSpecified = BzoneSpecified,
                    CzoneSpecified = CzoneSpecified)
  list(Messages = Messages_, Update = Update_ls)
}


#INITIALIZE DATASTORE GEOGRAPHY
#==============================
#' Initialize datastore geography.
#'
#' \code{initDatastoreGeography} a visioneval framework control function that
#' initializes tables and writes datasets to the datastore which describe
#' geographic relationships of the model.
#'
#' This function writes tables to the datastore for each of the geographic
#' levels. These tables are then used during a model run to store values that
#' are either specified in scenario inputs or that are calculated during a model
#' run. The function populates the tables with cross-references between
#' geographic levels. The function reads the model geography (Geo_df) from the
#' model state file. Upon successful completion, the function calls the
#' listDatastore function to update the datastore listing in the global list.
#'
#' @param GroupNames a character vector of the names of groups to initialize
#' the datastore groups to initialize with geography. The purpose of this
#' parameter is to enable the loading of a datastore in a model run, in which
#' case initialization of geography is only needed for new year groups for the
#' model run. The default value is NULL, which is the case when a datastore
#' is not being loaded.
#' @return The function returns TRUE if the geographic tables and datasets are
#'   sucessfully written to the datastore.
#' @export
initDatastoreGeography <- function(GroupNames = NULL) {
  G <- getModelState()
  #Make lists of zone specifications
  Mareas_ <- unique(G$Geo_df$Marea)
  MareaSpec_ls <- list(MODULE = "visioneval",
                       NAME = "Marea",
                       TABLE = "Marea",
                       TYPE = "character",
                       UNITS = "",
                       NAVALUE = "NA",
                       PROHIBIT = "",
                       ISELEMENTOF = "",
                       SIZE = max(nchar(Mareas_)))
  Azones_ <- unique(G$Geo_df$Azone)
  AzoneSpec_ls <- list(MODULE = "visioneval",
                       NAME = "Azone",
                       TABLE = "Azone",
                       TYPE = "character",
                       UNITS = "",
                       NAVALUE = "NA",
                       PROHIBIT = "",
                       ISELEMENTOF = "",
                       SIZE = max(nchar(Azones_)))
  if(G$BzoneSpecified) {
    Bzones_ <- unique(G$Geo_df$Bzone)
    BzoneSpec_ls <- list(MODULE = "visioneval",
                         NAME = "Bzone",
                         TABLE = "Bzone",
                         TYPE = "character",
                         UNITS = "",
                         NAVALUE = "NA",
                         PROHIBIT = "",
                         ISELEMENTOF = "",
                         SIZE = max(nchar(Bzones_)))
  }
  if(G$CzoneSpecified) {
    Czones_ <- unique(G$Geo_df$Czone)
    CzoneSpec_ls <- list(MODULE = "visioneval",
                         NAME = "Czone",
                         TABLE = "Czone",
                         TYPE = "character",
                         UNITS = "",
                         NAVALUE = "NA",
                         PROHIBIT = "",
                         ISELEMENTOF = "",
                         SIZE = max(nchar(Czones_)))
  }
  #Initialize geography tables and zone datasets
  if (is.null(GroupNames)) GroupNames <- c("Global", G$Years)
  for (GroupName in GroupNames) {
    initTable(Table = "Region", Group = GroupName, Length = 1)
    initTable(Table = "Azone", Group = GroupName, Length = length(Azones_))
    initTable(Table = "Marea", Group = GroupName, Length = length(Mareas_))
    if(G$BzoneSpecified) {
      initTable(Table = "Bzone", Group = GroupName, Length = length(Bzones_))
    }
    if(G$CzoneSpecified) {
      initTable(Table = "Czone", Group = GroupName, Length = length(Czones_))
    }
  }
  rm(GroupName)
  #Add zone names to zone tables
  for (GroupName in GroupNames) {
    if (!G$BzoneSpecified & !G$CzoneSpecified) {
      #Write to Azone table
      writeToTable(G$Geo_df$Azone, AzoneSpec_ls, Group = GroupName, Index = NULL)
      MareaSpec_ls$TABLE = "Azone"
      writeToTable(G$Geo_df$Marea, MareaSpec_ls, Group = GroupName, Index = NULL)
      #Write to Marea table
      MareaSpec_ls$TABLE = "Marea"
      writeToTable(Mareas_, MareaSpec_ls, Group = GroupName, Index = NULL)
    }
    if (G$BzoneSpecified & !G$CzoneSpecified) {
      #Write to Bzone table
      writeToTable(G$Geo_df$Bzone, BzoneSpec_ls, Group = GroupName, Index = NULL)
      AzoneSpec_ls$TABLE = "Bzone"
      writeToTable(G$Geo_df$Azone, AzoneSpec_ls, Group = GroupName, Index = NULL)
      MareaSpec_ls$TABLE = "Bzone"
      writeToTable(G$Geo_df$Marea, MareaSpec_ls, Group = GroupName, Index = NULL)
      #Write to Azone table
      AzoneGeo_df <- G$Geo_df[!duplicated(G$Geo_df$Azone),]
      AzoneSpec_ls$TABLE = "Azone"
      writeToTable(AzoneGeo_df$Azone, AzoneSpec_ls, Group = GroupName, Index = NULL)
      MareaSpec_ls$TABLE = "Azone"
      writeToTable(AzoneGeo_df$Marea, MareaSpec_ls, Group = GroupName, Index = NULL)
      rm(AzoneGeo_df)
      #Write to Marea table
      MareaSpec_ls$TABLE = "Marea"
      writeToTable(Mareas_, MareaSpec_ls, Group = GroupName, Index = NULL)
    }
    if (G$CzoneSpecified) {
      #Write to Czone table
      writeToTable(G$Geo_df$Czone, CzoneSpec_ls, Group = GroupName, Index = NULL)
      BzoneSpec_ls$TABLE = "Czone"
      writeToTable(G$Geo_df$Bzone, BzoneSpec_ls, Group = GroupName, Index = NULL)
      AzoneSpec_ls$TABLE = "Czone"
      writeToTable(G$Geo_df$Azone, AzoneSpec_ls, Group = GroupName, Index = NULL)
      MareaSpec_ls$TABLE = "Czone"
      writeToTable(G$Geo_df$Marea, MareaSpec_ls, Group = GroupName, Index = NULL)
      #Write to Bzone table
      BzoneGeo_df <- G$Geo_df[!duplicated(G$Geo_df$Bzone), c("Azone", "Bzone")]
      BzoneSpec_ls$TABLE = "Bzone"
      writeToTable(BzoneGeo_df$Bzone, BzoneSpec_ls, Group = GroupName, Index = NULL)
      AzoneSpec_ls$TABLE = "Bzone"
      writeToTable(BzoneGeo_df$Azone, AzoneSpec_ls, Group = GroupName, Index = NULL)
      rm(BzoneGeo_df)
      #Write to Azone table
      AzoneGeo_df <- G$Geo_df[!duplicated(G$Geo_df$Azone),]
      AzoneSpec_ls$TABLE = "Azone"
      writeToTable(AzoneGeo_df$Azone, AzoneSpec_ls, Group = GroupName, Index = NULL)
      MareaSpec_ls$TABLE = "Azone"
      writeToTable(AzoneGeo_df$Marea, MareaSpec_ls, Group = GroupName, Index = NULL)
      rm(AzoneGeo_df)
      #Write to Marea table
      MareaSpec_ls$TABLE = "Marea"
      writeToTable(Mareas_, MareaSpec_ls, Group = GroupName, Index = NULL)
    }
  }
  #Write to log that complete
  Message <- "Geography sucessfully added to datastore."
  writeLog(Message,Level="info")
  TRUE
}


#LOAD MODEL PARAMETERS
#=====================
#' Load model global parameters file into datastore.
#'
#' \code{loadModelParameters} a visioneval framework control function reads the
#' 'model_parameters.json' file and stores the contents in the 'Global/Model'
#' group of the datastore.
#'
#' This function reads the 'model_parameters.json' file in the 'defs' directory
#' which contains parameters specific to a model rather than to a module. These
#' area parameters that may be used by any module. Parameters are specified by
#' name, value, and data type. The function creates a 'Model' group in the
#' 'Global' group and stores the values of the appropriate type in the 'Model'
#' group.
#'
#' @return The function returns TRUE if the model parameters file exists and
#' its values are sucessfully written to the datastore.
#' @export
loadModelParameters <- function() {
  G <- getModelState()
  RunParam_ls <- G$RunParam_ls
  ModelParamInfo <- c("ParamDir","ModelParamFile","InputDir")
  missingParams <- ! ModelParamInfo %in% names(RunParam_ls)
  if ( any(missingParams) ) {
    stop(
      writeLog(
        paste(
          "Missing parameter names:",
          paste(ModelParamInfo[missingParams],collapse=",")
        ),
        Level="error"
      )
    )
  }
  writeLog("Loading model parameters file.",Level="info")
  ParamFile <- file.path(RunParam_ls$ParamDir, RunParam_ls$ModelParamFile)
  if (!file.exists(ParamFile)) {
    # Not Found: Try again looking this time in InputDir
    ParamFile <- file.path(RunParam_ls$InputDir, RunParam_ls$ModelParamFile)
    if (!file.exists(ParamFile)) {
      # Still Not Found: Throw an error
      ErrorMsg <- paste0(
        "Model parameters file (",
        ParamFile,
        ") could not be located in ",
        paste(RunParam_ls$ParamDir,RunParam_ls$InputDir,collapse=", ")
      )
      stop( writeLog(ErrorMsg,Level="error") )
    }
  }

  Param_df <- jsonlite::fromJSON(ParamFile)
  Group <- "Global"
  initTable(Table = "Model", Group = "Global", Length = 1)
  for (i in 1:nrow(Param_df)) {
    Type <- Param_df$TYPE[i]
    if (Type == "character") {
      Value <- Param_df$VALUE[i]
    } else {
      Value <- as.numeric(Param_df$VALUE[i])
    }
    Spec_ls <-
      list(
        NAME    = Param_df$NAME[i],
        TABLE   = "Model",
        TYPE    = Type,
        UNITS   = Param_df$UNITS[i],
        NAVALUE = ifelse(Param_df$TYPE[i] == "character", "NA", -9999),
        SIZE    = ifelse(
          Param_df$TYPE[i] == "character",
          nchar(Param_df$VALUE[i]),
          0
        ),
        LENGTH  = 1,
        MODULE  = G$Model
      )
    writeToTable(Value, Spec_ls, Group = "Global", Index = NULL)
  }
}

#PARSE MODEL SCRIPT
#==================
#' Parse model script.
#'
#' \code{parseModelScript} a visioneval framework control function that reads and
#' parses the model script to identify the sequence of module calls and the
#' associated call arguments.
#'
#' This function reads in the model run script and parses the script to
#' identify the sequence of VisionEval modelelement calls. It extracts each call
#' and identifies the values assigned to the function arguments. It creates a
#' list of the calls with their arguments in the order of the calls in the
#' script.
#'
#' The calls include all the legal VisionEval model elements: runModule, runScript, modelStage,
#' initializeModel, and requirePackage. The return from this function is a list of parameters
#' for each element; data.frames for runModule, runScript, modelStage, a list for initializeModel
#' and a vector of package names for requirePackage. See \code{initializeModel} for details on
#' how those return values are used.
#' 
#' @param FilePath A string identifying the model run script file
#' @return A list of parsed parameters for each of the VisionEval model elements found in the script.
#' @export
parseModelScript <- function(FilePath) {
  writeLog("Parsing model script",Level="info")
  if (!file.exists(FilePath)) {
    Msg <- paste0("Specified model script file (", FilePath, ") does not exist.")
    stop( writeLog(Msg,Level="error") )
  }

  Elements <- extractModelElements(parse(FilePath))
  # Shortcut to extract an elementType from the parsed list of VE model elements
  extractElement <- function(elementType) {
    Elements[sapply(
      Elements,
      function(s,seek){seek %in% names(s)},
      seek=elementType
    )]
  }

  ModuleCalls_df <- do.call(
    rbind.data.frame,
    lapply(
      extractElement("runModule"),
      function(x) normalizeElementFields(x$runModule,ModuleCallNames)
    )
  )

  ScriptCalls_df <- do.call(
    rbind.data.frame,
    lapply(
      extractElement("runScript"),
      function(x) normalizeElementFieleds(x$runScript,ScriptCallNames)
    )
  )

  Stages_df <- do.call(
    rbind.data.frame,
    lapply(
      extractElement("Stage"),
      function(x) normalizeElementFields(x$Stage,StageCallNames)
    )
  )
  
  InitParams_ls      <- lapply(extractElement("initializeModel"),function(x)x$initializeModel)[[1]] # Ignore more than one
  RequiredVEPackages <- sapply(extractElement("requirePackage"),function(x)x$requirePackage$Package) # Vector of package names

  return(
    list(
      AllCalls_ls        = Elements,
      ModuleCalls_df     = ModuleCalls_df,
      ScriptCalls_df     = ScriptCalls_df,
      Stages_df          = Stages_df,
      RequiredVEPackages = RequiredVEPackages,
      InitParams_ls      = InitParams_ls
    )
  )
}

# VisionEval Model Elements that we know how to parse
ModelElementNames <- c(
  "runModule",
  "runScript",
  "initializeModel",
  "requirePackage",
  "modelStage"
)
ModelElementsRegex <- paste(ModelElementNames,collapse="|")

ModuleCallNames <- c("ModuleName","PackageName","RunFor")
ScriptCallNames <- c("Module","Specification","RunFor","ModuleType")
StageCallNames  <- c("Name","Sequence")

normalizeElementFields <- function(Elements_ls,NeededNames) {
  missingNames <- setdiff(NeededNames,names(Elements_ls)) # names needed but not found
  for ( name in missingNames ) Elements_ls[[name]] <- NA
  return(Elements_ls) # return a list with all and only NeededNames
}

# Locate VisionEval functions for match.call
normalizeModelElement <- function(ModelElementName,ElementCall) {
  return(
    match.call(
      eval(parse(text=paste0("visioneval::",ModelElementName))),
      ElementCall
    )
  )
}

# screenTypes turns odd R call argument types (like "symbol") into character strings
# the ones named here are things we would like to keep their original type
screenTypes <- function(x) {
  if ( ! mode(x) %in% c("logical","numeric","character","NULL") ) as.character(x) else x
}

# extractModelElements returns a list of length-one named lists
# The inner named lists are named after their model element and their value is a nested list of arguments
# The list of arguments is rectified with match.call so all possible arguments are accounted for
#    Missing or defaulted named arguments will be provided.
#    Additional arguments destined for ... will be expanded to their name or position
extractModelElements <- function(test.expr,depth=0) {
  # use 'grep' to find the calls to VE Model Elements in test.expr
  ve.elements <- grep(ModelElementsRegex,sapply(test.expr,function(s) all.names(s)))

  parsed.calls <- list()
  for ( v in ve.elements ) { # iterate through matching elements
    r <- test.expr[[v]]
    r.char <- as.character(r)
    called <- sub("^visioneval::","",r.char[1]) # handle namespace (optional)
    if ( called %in% ModelElementNames ) {
      # top level call has a VE element: add it to the parse list
      r <- normalizeModelElement(called,r)
      parsed.call <- list(lapply(as.list(r[-1]),screenTypes))
      names(parsed.call) <- called
      attr(parsed.call,"Call") <- deparse(r,width.cutoff=80L)
      parsed.calls[[length(parsed.calls)+1]] <- parsed.call
    } else {
      # call contains other calls that have a VE element (arguments or sub-expressions)
      # handle that by recursing into this function
      for ( deeper.call in Recall(r,depth=depth+1) ) {
        parsed.calls[[length(parsed.calls)+1]] <- deeper.call
      }
    }
  }
  return(parsed.calls)
}
# library(visioneval)
# parsed <- parseModelScript("run_model.R")

parseModelScriptOld <-
  function(FilePath = "Run_Model.R") {
    writeLog("Parsing model script",Level="info")
    if (!file.exists(FilePath)) {
      Msg <-
        paste0("Specified model script file (", FilePath, ") does not exist.")
      writeLog(Msg,Level="error")
      stop(Msg)
    }
    rawScript <- readLines(FilePath) # so we can extract requirePackage calls (see below)

    # remove commented lines from run_model script file (possibly entire lines).
    rawScript <- gsub("^[:space:]*#.*","",rawScript)

    # get required packages
    requirePackages_ <- "requirePackage"
    RequiredVEPackages <- grep(requirePackages_,rawScript,value=TRUE)
    RequiredVEPackages <- sub("[^(]*\\([ \"']*([^\"')]+)[ \"']*\\).*","\\1",RequiredVEPackages)

    Script <- paste(rawScript, collapse = " ")
    RunModuleCalls_ <- unlist(strsplit(Script, "runModule"))[-1]
    if (length(RunModuleCalls_) == 0) {
      Msg <- "Specified script contains no 'runModule' function calls."
      stop(Msg)
    }
    Errors_ <- character(0)
    addError <- function(Error) {
      Errors_ <<- c(Errors_, Error)
    }
    #Utility function to remove spaces from string
    removeSpaces <- function(String) {
      gsub(" ", "", String)
    }
    #Utility function to clean up string
    cleanString <- function(String) {
      ToRemove = c(" ", "\\(", ")", '\\\"')
      for (SubString in ToRemove) {
        String <- gsub(SubString, "", String)
      }
      String
    }
    #Utility function to extract the arguments part of a function call
    extractArgsString <- function(String) {
      substring(String,
                regexpr("\\(", String),
                regexpr("\\)", String))
    }
    #Utility function to extract the value of a named argument
    getNamedArgumentValue <-
      function(ArgString, ArgName) {
        CommaPos <- regexpr(",", ArgString)
        if (CommaPos > 0) {
          ArgValue <-
            substring(ArgString,
                      regexpr("=", ArgString) + 1,
                      CommaPos - 1)
        } else {
          ArgValue <-
            substring(ArgString,
                      regexpr("=", ArgString) + 1,
                      nchar(ArgString))
        }
        cleanString(ArgValue)
      }
    #Utility function to extract the value of an unnamed argument
    getUnnamedArgumentValue <-
      function(ArgString) {
        ArgStringTrim <-
          substring(ArgString,
                    regexpr("\\\"", ArgString) + 1,
                    nchar(ArgString))
        cleanString(substring(ArgStringTrim,
                              1,
                              regexpr("\\\"", ArgStringTrim)))
      }
    #Function to extract the name and value of an argument from a string
    getArg <-
      function(ArgsString, ArgPos) {
        ArgString <- removeSpaces(ArgsString[ArgPos])
        #Define standard argument names
        ArgNames_ <-
          c("ModuleName", "PackageName", "RunFor", "Year")
        #Check whether any of the argument names is in the ArgString
        ArgNameCheck_ <-
          sapply(ArgNames_, function(x) {
            ArgName <- paste0(x, "=")
            regexpr(ArgName, ArgString)
          }) > 0
        if (any(ArgNameCheck_)) {
          ArgName <- names(ArgNameCheck_)[ArgNameCheck_]
          ArgValue <- getNamedArgumentValue(ArgString, ArgName)
        } else {
          ArgName <- ArgNames_[ArgPos]
          ArgValue <- getUnnamedArgumentValue(ArgString)
        }
        list(ArgName = ArgName, ArgValue = ArgValue)
      }
    #Function to extract all the arguments of runModule function call
    getArgs <-
      function(String) {
        PrelimArgs_ <- unlist(strsplit(extractArgsString(String), ","))
        Args_ls <-
          list(ModuleName = NULL,
               PackageName = NULL,
               RunFor = NULL)
        for (i in 1:length(PrelimArgs_)) {
          Arg_ls <- getArg(PrelimArgs_, i)
          Args_ls[[Arg_ls$ArgName]] <- Arg_ls$ArgValue
        }
        unlist(Args_ls)
      }
    #Iterate through RunModuleCalls_ and extract the arguments
    Args_ls <- list()
    for (i in 1:length(RunModuleCalls_)) {
      Args_ <- getArgs(RunModuleCalls_[i])
      #Error if not all mandatory arguments are matched
      if (length(Args_) != 4) {
        Msg <- paste0("runModule call #",
                      i,
                      " has improperly specified arguments.")
        addError(Msg)
      }
      #Add to list
      Args_ls[[i]] <- Args_
    }
    #If there are any errors, print error message
    if (length(Errors_) != 0) {
      writeLog("One or more 'runModule' function calls have errors as follows:",Level="error")
      writeLog(Errors_,Level="error")
      stop(
        "One or more errors in model run script. Must fix before model initialization can be completed."
      )
    }
    ModuleCalls_df <-
      data.frame(do.call(rbind, Args_ls), stringsAsFactors = FALSE)
    writeLog("Success parsing model script",Level="info")
    return(list(ModuleCalls_df = ModuleCalls_df, RequiredVEPackages = RequiredVEPackages))
  }


#CHECK MODULE AVAILABILITY
#=========================
#' Check whether a module required to run a model is present
#'
#' \code{checkModuleExists} a visioneval framework control function that checks
#' whether a module required to run a model is present.
#'
#' This function takes a specified module and package, checks whether the
#' package has been installed and whether the module is in the package. The
#' function returns an error message if the package is not installed or if
#' the module is not present in the package. If the module has been called by
#' another module the value of the 'CalledBy' argument will be used to identify
#' the calling module as well so that the user understands where the call is
#' coming from.
#'
#' @param ModuleName A string identifying the module name.
#' @param PackageName A string identifying the package name.
#' @param InstalledPkgs_ A string vector identifying the names of packages that
#' are installed.
#' @param CalledBy A string vector having two named elements. The value of the
#' 'Module' element is the name of the calling module. The value of the
#' 'Package' element is the name of the package that the calling module is in.
#' @return TRUE if all packages and modules are present and FALSE if not.
#' @export
checkModuleExists <-
  function(ModuleName,
           PackageName,
           InstalledPkgs_ = rownames(installed.packages()),
           CalledBy = NA) {
    ErrorMsg <- character(0)
    PackageMissing <- FALSE
    ModuleMissing <- FALSE
    #Check whether the package is installed and module is present in package
    PackageMissing <- !(PackageName %in% InstalledPkgs_)
    if (!PackageMissing) {
      PkgData_ <- data(package=PackageName)$results[,"Item"]
      PkgModules_ <- PkgData_[grep("Specifications", PkgData_)]
      PkgModules_ <- gsub("Specifications", "", PkgModules_)
      ModuleMissing <- !(ModuleName %in% PkgModules_)
    }
    #Compose error messages if any
    if (PackageMissing) {
      if (all(is.na(CalledBy))) {
        ErrorMsg <-
          paste0("Error in runModule call for module ", ModuleName,
                 " in package ", PackageName, ". Package ", PackageName,
                 " is not installed.")
      } else {
        ErrorMsg <-
          paste0("Error in runModule call for module ", CalledBy["Module"],
                 " in package ", CalledBy["Package"], ". This module calls Module ",
                 ModuleName, " in package ", PackageName, ". Package ", PackageName,
                 " is not installed.")
      }
    }
    if (ModuleMissing) {
      if (all(is.na(CalledBy))) {
        ErrorMsg <-
          paste0("Error in runModule call for module ", ModuleName,
                 " in package ", PackageName, ". Module ", ModuleName,
                 " is not present in package.")
      } else {
        ErrorMsg <-
          paste0("Error in runModule call for module ", CalledBy["Module"],
                 " in package ", CalledBy["Package"], ". This module calls Module ",
                 ModuleName, " in package ", PackageName, ". Module ", ModuleName,
                 " is not present in package.")
      }
    }
    #Return the error message
    ErrorMsg
  }


#GET MODULE SPECIFICATIONS
#=========================
#' Retrieve module specifications from a package
#'
#' \code{getModuleSpecs} a visioneval framework control function that retrieves
#' the specifications list for a module and returns the specifications list.
#'
#' This function loads the specifications for a module in a package. It returns
#' the specifications list.
#'
#' @param ModuleName A string identifying the name of the module.
#' @param PackageName A string identifying the name of the package that the
#' module is in.
#' @return A specifications list that is the same as the specifications list
#' defined for the module in the package.
#' @export
getModuleSpecs <- function(ModuleName, PackageName) {
  eval(parse(text = paste0(PackageName, "::", ModuleName, "Specifications")))
}
# Test_ls <-
#   getModuleSpecs(ModuleName = "CreateBzones", PackageName = "vedemo1")
# rm(Test_ls)


#EXPAND SPECIFICATION
#====================
#' Expand a Inp, Get, or Set specification so that is can be used by other
#' functions to process inputs and to read from or write to the datastore.
#'
#' \code{expandSpec} a visioneval framework control function that takes a Inp,
#' Get, or Set specification and processes it to be in a form that can be used
#' by other functions which use the specification in processing inputs or
#' reading from or writing to the datastore. The parseUnitsSpec function is
#' called to parse the UNITS attribute to extract name, multiplier, and year
#' values. When the specification has multiple values for the NAME attribute,
#' the function creates a specification for each name value.
#'
#' The VisionEval design allows module developers to assign multiple values to
#' the NAME attributes of a Inp, Get, or Set specification where the other
#' attributes for those named datasets (or fields) are the same. This greatly
#' reduces duplication and the potential for error in writing module
#' specifications. However, other functions that check or use the specifications
#' are not capable of handling specifications which have NAME attributes
#' containing multiple values. This function expands a specification with
#' multiple values for a  NAME attribute into multiple specifications, each with
#' a single value for the NAME attribute. In addition, the function calls the
#' parseUnitsSpec function to extract multiplier and year information from the
#' value of the UNITS attribute. See that function for details.
#'
#' @param SpecToExpand_ls A standard specifications list for a specification
#' whose NAME attribute has multiple values.
#' @param ComponentName A string that is the name of the specifications
#' that the specification is a part of (e.g. "Inp", "Get", "Set").
#' @return A list of standard specifications lists which has a component for
#' each value in the NAME attribute of the input specifications list.
#' @export
#Define a function which expands a specification with multiple NAME items
expandSpec <- function(SpecToExpand_ls, ComponentName) {
  SpecToExpand_ls <- parseUnitsSpec(SpecToExpand_ls, ComponentName)
  Names_ <- unlist(SpecToExpand_ls$NAME)
  Descriptions_ <- unlist(SpecToExpand_ls$DESCRIPTION)
  Expanded_ls <- list()
  for (i in 1:length(Names_)) {
    Temp_ls <- SpecToExpand_ls
    Temp_ls$NAME <- Names_[i]
    Temp_ls$DESCRIPTION <- Descriptions_[i]
    Expanded_ls <- c(Expanded_ls, list(Temp_ls))
  }
  Expanded_ls
}


#FILTER INP SPECIFICATIONS BASED ON WHETHER SPECIFICATION IS OPTIONAL
#====================================================================
#' Filters Inp specifications list based on OPTIONAL specification attributes.
#'
#' \code{doProcessInpSpec} a visioneval framework control function that filters
#' out Inp specifications whose OPTIONAL specification attribute is TRUE but the
#' specified input file is not present.
#'
#' An Inp specification component may have an OPTIONAL specification whose value
#' is TRUE. If so, and if the specified input file is present, then the input
#' specification needs to be processed. This function checks whether the
#' OPTIONAL specification is present, whether its value is TRUE, and whether the
#' file exists. If all of these are true, then the input specification needs to
#' be processed. The input specification also needs to be processed if it is
#' not optional. A specification is not optional if the OPTIONAL attribute is
#' not present or if it is present and the value is not TRUE. The function
#' returns a list of all the Inp specifications that meet these criteria.
#'
#' @param InpSpecs_ls A standard specifications list for Inp specifications.
#' @param InputDir A vector of paths in which to seek input files;
#'   the first path containing the named file will be used.
#' @return A list containing the Inp specification components that meet the
#'   criteria of being optional and present or being not optional. If
#'   the file is not optional and not present, throw an error and stop.
#'   The FILE element will be expanded to file.path(InputDir,$FILE) in
#'   the spec that is returned.
#' @export
doProcessInpSpec <- function(InpSpecs_ls, InputDir) {
  #Define function to check an individual specification
  #Return TRUE if missing file is not an error
  checkOptional <- function(SpecToCheck_ls) {
    IsOptional <- FALSE
    if (!is.null(SpecToCheck_ls$OPTIONAL)) {
      if (SpecToCheck_ls$OPTIONAL == TRUE) {
        IsOptional <- TRUE
      }
    }
    IsOptional
  }
  #Return all input specifications that must be processed
  Out_ls <- list()
  j <- 1
  for (i in 1:length(InpSpecs_ls)) {
    Spec_ls <- InpSpecs_ls[[i]]
    File <- file.path(InputDir, Spec_ls$FILE) # might be a vector
    FileExists <- file.exists(File)
    if ( ! any(FileExists) ) {
      if ( checkOptional(Spec_ls) ) {
        next # Do not add to Out_ls; continue to next InpSpec
      } else {
        Spec_ls$INPUTDIR <- NA # Required, but missing; trap later
      }
    } else {
      Spec_ls$INPUTDIR <- InputDir[FileExists][1]
    }
    Out_ls[[j]] <- Spec_ls
    j <- j + 1
  }
  Out_ls
}
#Test code
# setwd("tests")
# source("data/TestOptionalSpecs.R")
# doProcessInpSpec(TestOptionalSpecs$Inp)
# setwd("..")
# rm(TestOptionalSpecs)


#PROCESS MODULE SPECIFICATIONS
#=============================
#' Process module specifications to expand items with multiple names.
#'
#' \code{processModuleSpecs} a visioneval framework control function that
#' processes a full module specifications list, expanding all elements in the
#' Inp, Get, and Set components by parsing the UNITS attributes and duplicating
#' every specification which has multiple values for the NAME attribute.
#'
#' This function process a module specification list. If any of the
#' specifications include multiple listings of data sets (i.e. fields) in a
#' table, this function expands the listing to establish a separate
#' specification for each data set.
#'
#' @param Spec_ls A specifications list.
#' @return A standard specifications list with expansion of the multiple item
#' specifications.
#' @export
processModuleSpecs <- function(Spec_ls) {
  G <- getModelState()
  #Define a function to process a component of a specifications list
  processComponent <- function(Component_ls, ComponentName) {
    Result_ls <- list()
    for (i in 1:length(Component_ls)) {
      Temp_ls <- Component_ls[[i]]
      Result_ls <- c(Result_ls, expandSpec(Temp_ls, ComponentName))
    }
    Result_ls
  }
  #Process the list components and return the results
  Out_ls <- list()
  Out_ls$RunBy <- Spec_ls$RunBy
  if (!is.null(Spec_ls$NewInpTable)) {
    Out_ls$NewInpTable <- Spec_ls$NewInpTable
  }
  if (!is.null(Spec_ls$NewSetTable)) {
    Out_ls$NewSetTable <- Spec_ls$NewSetTable
  }
  if (!is.null(Spec_ls$Inp)) {
    InputDir <- if ( "InputDir" %in% names(G$RunParam_ls) ) {
      (G$RunParam_ls$InputDir)  # May be a vector; first matching file will be used
    } else { # backward compatible
      normalizePath("inputs",winslash="/",mustWork=FALSE)
    }
    FilteredInpSpec_ls <- doProcessInpSpec(Spec_ls$Inp, InputDir)
    if (length(FilteredInpSpec_ls) > 0) {
      Out_ls$Inp <- processComponent(FilteredInpSpec_ls, "Inp")
    }
  }
  if (!is.null(Spec_ls$Get)) {
    Out_ls$Get <- processComponent(Spec_ls$Get, "Get")
  }
  if (!is.null(Spec_ls$Set)) {
    Out_ls$Set <- processComponent(Spec_ls$Set, "Set")
  }
  if (!is.null(Spec_ls$Call)) {
    Out_ls$Call <- Spec_ls$Call
  }
  Out_ls
}


#SIMULATE DATA STORE TRANSACTIONS
#================================
#' Create simulation of datastore transactions.
#'
#' \code{simDataTransactions} a visioneval framework control function that loads
#' all module specifications in order (by run year) and creates a simulated
#' listing of the data which is in the datastore and the requests of data from
#' the datastore and checks whether tables will be present to put datasets in
#' and that datasets will be present that data is to be retrieved from.
#'
#' This function creates a list of the datastore listings for the working
#' datastore and for all datastore references. The list includes a 'Global'
#' component, in which 'Global' references are simulated, components for each
#' model run year, in which 'Year' references are simulated, and if the base
#' year is not one of the run years, a base year component, in which base year
#' references are simulated. For each model run year the function steps through
#' a data frame of module calls as produced by 'parseModelScript', and loads and
#' processes the module specifications in order: adds 'NewInpTable' references,
#' adds 'Inp' dataset references, checks whether references to datasets
#' identified in 'Get' specifications are present, adds 'NewSetTable' references,
#' and adds 'Set' dataset references. The function compiles a vector of error
#' and warning messages. Error messages are made if: 1) a 'NewInpTable' or
#' 'NewSetTable' specification of a module would create a new table for a table
#' that already exists; 2) a dataset identified by a 'Get' specification would
#' not be present in the working datastore or any referenced datastores; 3) the
#' 'Get' specifications for a dataset would not be consistent with the
#' specifications for the dataset in the datastore. The function compiles
#' warnings if a 'Set' specification will cause existing data in the working
#' datastore to be overwritten. The function writes warning and error messages
#' to the log and stops program execution if there are any errors.
#'
#' @param AllSpecs_ls A list containing the processed specifications of all of
#' the modules run by model script in the order that the modules are called with
#' duplicated module calls removed. Information about each module call is a
#' component of the list in the order of the module calls. Each component is
#' composed of 3 components: 'ModuleName' contains the name of the module,
#' 'PackageName' contains the name of the package the module is in, and
#' 'Specs' contains the processed specifications of the module. The 'Get'
#' specification component includes the 'Get' specifications of all modules
#' that are called by the module.
#'
#' @return There is no return value. The function has the side effect of
#' writing messages to the log and stops program execution if there are any
#' errors.
#' @export
simDataTransactions <- function(AllSpecs_ls) {
  G <- getModelState()

  #Initialize errors and warnings vectors
  #--------------------------------------
  Errors_ <- character(0)
  addError <- function(Msg) {
    Errors_ <<- c(Errors_, Msg)
  }
  Warnings_ <- character(0)
  addWarning <- function(Msg) {
    Warnings_ <<- c(Warnings_, Msg)
  }

  #Make a list to store the working datastore and all referenced datastores
  #------------------------------------------------------------------------
  RunYears_ <- getYears()
  BaseYear <- G$BaseYear
  if (BaseYear %in% RunYears_) {
    Years_ <- RunYears_
  } else {
    Years_ <- c(BaseYear, RunYears_)
  }
  Dstores_ls <-
    list(
      Global = list()
    )
  for (Year in Years_) Dstores_ls[[Year]] <- list()

  #Add the working datastore inventory to the datastores list
  #----------------------------------------------------------
  Dstores_ls[["Global"]] <-
    G$Datastore[grep("Global", G$Datastore$group),]
  # for (Year in RunYears_) {
  #   Dstores_ls[[Year]][[G$DatastoreName]] <-
  #     G$Datastore[grep(Year, G$Datastore$group),]
  # }
  getDatastoreYears <- function() {
    DstoreGroups_ls <- strsplit(G$Datastore$group, "/")
    ToKeep_ <- unlist(lapply(DstoreGroups_ls, function(x) length(x) == 2))
    DstoreGroups_ls <- DstoreGroups_ls[ToKeep_]
    DstoreGroups_ <- unique(unlist(lapply(DstoreGroups_ls, function(x) x[2])))
    DstoreGroups_[!(DstoreGroups_ %in% "Global")]
  }
  for (Year in getDatastoreYears()) {
    Dstores_ls[[Year]] <-
      G$Datastore[grep(Year, G$Datastore$group),]
  }

  # #Function to get datastore inventory corresponding to datastore reference
  # #------------------------------------------------------------------------
  # getInventoryRef <- function(DstoreRef) {
  #   SplitRef_ <- unlist(strsplit(DstoreRef, "/"))
  #   RefHead <- paste(SplitRef_[-length(SplitRef_)], collapse = "/")
  #   paste(RefHead, getModelStateFileName(), sep = "/")
  # }
  #
  # #Get datastore inventories for datastore references
  # #--------------------------------------------------
  # if (!is.null(G$DatastoreReferences)) {
  #   RefNames_ <- names(G$DatastoreReferences)
  #   for (Name in RefNames_) {
  #     Refs_ <- G$DatastoreReferences[[Name]]
  #     for (Ref in Refs_) {
  #       if (file.exists(Ref)) {
  #         RefDstore_df <-
  #           readModelState(FileName = getInventoryRef(Ref))$Datastore
  #         RefDstore_df <- RefDstore_df[grep(Name, RefDstore_df$group),]
  #         Dstores_ls[[Name]][[Ref]] <- RefDstore_df
  #         rm(RefDstore_df)
  #
  #       } else {
  #         Msg <-
  #           paste0("The file '", Ref,
  #                  "' included in the 'DatastoreReferences' in the ",
  #                  "'run_parameters.json' file is not present.")
  #         addError(Msg)
  #       }
  #     }
  #   }
  # }

  #Define function to add table reference to datastore inventory
  #-------------------------------------------------------------
  addTableRef <- function(Dstore_df, TableSpec_, IsBaseYear, MakeTableType) {
    Group <- TableSpec_$GROUP
    if (Group == "Year") Group <- Year
    Table <- TableSpec_$TABLE
    #Check if table already exists
    HasTable <- checkTableExistence(Table, Group, Dstore_df)
    #If table exists then possible error, otherwise add reference to table
    if (HasTable) {
      #Is not an error if the group is 'Global' and year is not the base year
      #Because not a conflict between tables created by different modules
      if (Group == "Global" & !IsBaseYear) {
        NewDstore_df <- Dstore_df
        #Otherwise is an error
      } else {
        if (MakeTableType == "Inp") {
          MakeTableSpecName <- "MakeInpTable"
        } else {
          MakeTableSpecName <- "MakeSetTable"
        }
        Msg <-
          paste0("Error: ", MakeTableSpecName, "specification for module '",
                 TableSpec_$MODULE, "' will create a table '", Table,
                 "' that already exists in the working datastore.")
        addError(Msg)
        NewDstore_df <- Dstore_df
      }
    } else {
      NewDstore_df <- data.frame(
        group = c(Dstore_df$group, paste0("/", Group)),
        name = c(Dstore_df$name, Table),
        groupname = c(Dstore_df$groupname, paste0(Group, "/", Table)),
        stringsAsFactors = FALSE
      )
      NewDstore_df$attributes <- c(Dstore_df$attributes, list(TableSpec_))
    }
    NewDstore_df
  }

  #Define function to add dataset reference to datastore inventory
  #---------------------------------------------------------------
  addDatasetRef <- function(Dstore_df, DatasetSpec_, IsBaseYear) {
    Group <- DatasetSpec_$GROUP
    if (Group == "Year") Group <- Year
    Table <- DatasetSpec_$TABLE
    Name <- DatasetSpec_$NAME
    #Check if dataset already exists
    HasDataset <- checkDataset(Name, Table, Group, Dstore_df)
    #If dataset exists then warn and check consistency of specifications
    if (HasDataset) {
      #No need to check if the group is 'Global' and year is not the base year
      #Because not a conflict between datasets created by different modules
      if (Group == "Global" & !IsBaseYear) {
        NewDstore_df <- Dstore_df
        #Otherwise issue a warning and check for consistent data specifications
      } else {
        #Add warning that existing dataset will be overwritten
        Msg <-
          paste0("Module '", Module, "' will overwrite dataset '", Name,
                 "' in table '", Table, "'.")
        addWarning(Msg)
        #Check attributes are consistent
        DstoreDatasetAttr_ls <-
          getDatasetAttr(Name, Table, Group, Dstore_df)
        AttrConsistency_ls <-
          checkSpecConsistency(DatasetSpec_, DstoreDatasetAttr_ls)
        if (length(AttrConsistency_ls$Errors != 0)) {
          addError(AttrConsistency_ls$Errors)
        }
        NewDstore_df <- Dstore_df
      }
    } else {
      NewDstore_df <- data.frame(
        group = c(Dstore_df$group, paste0("/", Group)),
        name = c(Dstore_df$name, Name),
        groupname = c(Dstore_df$groupname, paste0(Group, "/", Table, "/", Name)),
        stringsAsFactors = FALSE
      )
      NewDstore_df$attributes <-
        c(Dstore_df$attributes,
          list(DatasetSpec_[c("NAVALUE", "SIZE", "TYPE", "UNITS")]))
    }
    NewDstore_df
  }

  #Define function to check whether dataset is optional
  #----------------------------------------------------
  isOptional <- function(Spec_ls) {
    if (!is.null(Spec_ls$OPTIONAL)) {
      Spec_ls$OPTIONAL
    } else {
      FALSE
    }
  }

  #Iterate through run years and modules to simulate model run
  #-----------------------------------------------------------
  for (Year in RunYears_) {
    #Iterate through module calls
    for (i in 1:length(AllSpecs_ls)) {
      Module <- AllSpecs_ls[[i]]$ModuleName
      Package <- AllSpecs_ls[[i]]$PackageName
      RunFor <- AllSpecs_ls[[i]]$RunFor
      if (RunFor == "BaseYear" & Year != "BaseYear") break()
      if (RunFor == "NotBaseYear" & Year == "BaseYear") break()
      ModuleSpecs_ls <-
        processModuleSpecs(getModuleSpecs(Module, Package))

      #Add 'Inp' table references to the working datastore inventory
      #-------------------------------------------------------------
      if (!is.null(ModuleSpecs_ls$NewInpTable)) {
        for (j in 1:length(ModuleSpecs_ls$NewInpTable)) {
          Spec_ls <- ModuleSpecs_ls$NewInpTable[[j]]
          Spec_ls$MODULE <- Module
          if (Spec_ls[["GROUP"]] == "Global") {
            RefGroup <- "Global"
          } else {
            RefGroup <- Year
          }
          #Get the datastore inventory for the group
          Dstore_df <- Dstores_ls[[RefGroup]]
          #Add the table reference and check for table add error
          Dstores_ls[[RefGroup]] <-
            addTableRef(Dstore_df, Spec_ls, Year == BaseYear, "Inp")
          rm(Spec_ls, RefGroup, Dstore_df)
        }
        rm(j)
      }

      #Add 'Inp' dataset references to the working datastore inventory
      #---------------------------------------------------------------
      if (!is.null(ModuleSpecs_ls$Inp)) {
        for (j in 1:length(ModuleSpecs_ls$Inp)) {
          Spec_ls <- ModuleSpecs_ls$Inp[[j]]
          Spec_ls$MODULE <- Module
          if (Spec_ls[["GROUP"]] == "Global") {
            RefGroup <- "Global"
          } else {
            RefGroup <- Year
          }
          #Get the datastore inventory for the group
          Dstore_df <- Dstores_ls[[RefGroup]]
          #Add the dataset reference and check for dataset add error
          Dstores_ls[[RefGroup]] <-
            addDatasetRef(Dstore_df, Spec_ls, Year == BaseYear)
          rm(Spec_ls, RefGroup, Dstore_df)
        }
        rm(j)
      }

      #Check for presence of 'Get' dataset references in datastore inventory
      #---------------------------------------------------------------------
      if (!is.null(ModuleSpecs_ls$Get)) {
        for (j in 1:length(ModuleSpecs_ls$Get)) {
          Spec_ls <- ModuleSpecs_ls$Get[[j]]
          Spec_ls$MODULE <- Module
          Group <- Spec_ls[["GROUP"]]
          Table <- Spec_ls[["TABLE"]]
          Name <- Spec_ls[["NAME"]]
          if (Group == "Global") {
            Group <- "Global"
          }
          if (Group == "BaseYear") {
            Group <- G$BaseYear
          }
          if (Group == "Year") {
            Group <- Year
          }
          DatasetFound <- FALSE
          Dstore_df <- Dstores_ls[[Group]]
          DatasetInDstore <- checkDataset(Name, Table, Group, Dstore_df)
          if (!DatasetInDstore) {
            next()
          } else {
            DatasetFound <- TRUE
            DstoreAttr_ <- getDatasetAttr(Name, Table, Group, Dstore_df)
            AttrConsistency_ls <-
              checkSpecConsistency(Spec_ls, DstoreAttr_)
            if (length(AttrConsistency_ls$Errors != 0)) {
              addError(AttrConsistency_ls$Errors)
            }
            rm(DstoreAttr_, AttrConsistency_ls)
          }
          rm(Dstore_df, DatasetInDstore)
          if (!DatasetFound & !isOptional(Spec_ls)) {
            Msg <-
              paste0("Module '", Module,
                     "' has a 'Get' specification for dataset '", Name,
                     "' in table '", Table,
                     "' that will not be present in the working datastore or ",
                     "any referenced datastores when it is needed.")
            addError(Msg)
            stop("CheckError")
          }
        }
      }

      #Add 'Set' table references to the working datastore inventory
      #-------------------------------------------------------------
      if (!is.null(ModuleSpecs_ls$NewSetTable)) {
        for (j in 1:length(ModuleSpecs_ls$NewSetTable)) {
          Spec_ls <- ModuleSpecs_ls$NewSetTable[[j]]
          Spec_ls$MODULE <- Module
          if (Spec_ls[["GROUP"]] == "Global") {
            RefGroup <- "Global"
          } else {
            RefGroup <- Year
          }
          #Get the datastore inventory for the group
          Dstore_df <- Dstores_ls[[RefGroup]]
          #Add the table reference and check for table add error
          Dstores_ls[[RefGroup]] <-
            addTableRef(Dstore_df, Spec_ls, Year == BaseYear, "Set")
          rm(Spec_ls, RefGroup, Dstore_df)
        }
      }

      #Add 'Set' dataset references to the working datastore inventory
      #---------------------------------------------------------------
      if (!is.null(ModuleSpecs_ls$Set)) {
        for (j in 1:length(ModuleSpecs_ls$Set)) {
          Spec_ls <- ModuleSpecs_ls$Set[[j]]
          Spec_ls$MODULE <- Module
          if (Spec_ls[["GROUP"]] == "Global") {
            Group <- "Global"
          } else {
            Group <- Year
          }
          #Get the datastore inventory for the group
          Dstore_df <- Dstores_ls[[Group]]
          Dstores_ls[[Group]] <-
            addDatasetRef(Dstore_df, Spec_ls, Year == BaseYear)
          rm(Spec_ls, Group, Dstore_df)
        }
      }

      rm(Module, Package, ModuleSpecs_ls)
    } #End for loop through module calls
  } #End for loop through years

  writeLog("Simulating model run.",Level="warn")
  if (length(Warnings_) != 0) {
    Msg <-
      paste0("Model run simulation had one or more warnings. ",
             "Datasets will be be overwritten when the model runs. ",
             "Check that this is what it intended. ")
    writeLog(Msg,Level="warn")
    writeLog(Warnings_,Level="warn")
  }
  if (length(Errors_) == 0) {
    writeLog("Model run simulation completed without identifying any errors.",Level="warn")
  } else {
    Msg <-
      paste0("Model run simulation has found one or more errors. ",
             "The following errors must be corrected before the model may be run.")
    writeLog(Msg,Level="error")
    writeLog(Errors_,Level="error")
    stop(Msg, " Check log for details.")
  }
}
