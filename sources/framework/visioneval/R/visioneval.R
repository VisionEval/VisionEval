#============
#visioneval.R
#============

#This script defines the functions that are used to write VisionEval models.

#INITIALIZE MODEL
#================
#' Initialize model.
#'
#' \code{initializeModel} a visioneval framework model user function
#' that initializes a VisionEval model, loading all parameters and inputs, and
#' making checks to ensure that model can run successfully.
#'
#' This function does several things to initialize the model environment and
#' datastore including:
#' 1) Initializing a file that is used to keep track of the state of key model
#' run variables and the datastore;
#' 2) Initializes a log to which messages are written;
#' 3) Creates the datastore and initializes its structure, reads in and checks
#' the geographic specifications and initializes the geography in the datastore,
#' or loads an existing datastore if one has been identifiecdd;
#' 4) Parses the model run script to identify the modules in their order of
#' execution and checks whether all the identified packages are installed and
#' the modules exist in the packages;
#' 5) Checks that all data requested from the datastore will be available when
#' it is requested and that the request specifications match the datastore
#' specifications;
#' 6) Checks all of the model input files to determine whether they they are
#' complete and comply with specifications.
#'
#' @section Run Parameters:
#' Values for the ... parameter are model configurations that are read from VisionEval.cnf
#' in the user's runtime folder, then from VisionEval.cnf in the model folder, then in
#' "defs/run_parameters.json", then from here (the last-found version of the value after
#' checking those places will be used).
#' Parameters that historically have appeared in ... are the following, but ... can include values
#' for any of the parameters that might appear in VisionEval.cnf or in run_parameters.json:
#' ModelScriptFile A string (regular expression) identifying the file that contains
#'   the steps in the model to run (i.e. the calls to \code{runModule})
#' ParamDir (A string identifying the relative or absolute path to the
#'   directory where the parameter and geography definition files are located.
#'   The default value is "defs", relative to the run_model.R location),
#' RunParamFile (A string identifying the name of a JSON-formatted text
#' file that contains parameters needed to identify and manage the model run.
#' The default value is "run_parameters.json"; set this in VisionEval.cnf)
#' GeoFile (A string identifying the name of a text file in comma-separated values
#' format that contains the geographic specifications for the model. The default value is
#' "geo.csv"; set this in VisionEval.cnf)
#' ModelParamFile (A string identifying the name of a JSON-formatted text
#' file that contains global model parameters that are important to a model and
#' may be shared by several modules; set this in VisionEval.cnf. Note also that
#' this file used to be sought in "defs", but will now first be sought in "inputs"
#' with "defs" as the fallback location if not found in "inputs")
#' SaveDatastore A logical identifying whether if an existing datastore
#' should be renamed rather than removed.
#'
#' @param LoadDatastore A logical identifying whether an existing datastore
#' should be loaded.
#' @param DatastoreName A string identifying the full path name of a datastore
#' to load or NULL if an existing datastore in the working directory is to be
#' loaded. If LoadDatastore is FALSE and DatastoreName is provided, LoadDatastore
#' will be set to TRUE with a warning.
#' @param SimulateRun A logical identifying whether the model run should be
#' simulated. see \code{prepareModelRun} for details.
#' @param ... Additional arguments that may be passed as run parameters
#' @return The ModelState_ls that was constructed, invisibly.
#' @export
initializeModel <- function(
  LoadDatastore = FALSE,
  DatastoreName = NULL, # WARNING: different from "DatastoreName" loaded as a run parameter
  SimulateRun = FALSE,
  ...
) {
  # Access the model environment and check for RunModel condition
  ve.model <- modelEnvironment(Clear="") # clear ve.model environment (but don't destroy "Owned" objects)
  if ( all(c("RunParam_ls","ModelState_ls") %in% names(ve.model) ) ) {
    return(invisible(ve.model$ModelState_ls))
  } # If owned objects persist, initialization is happening externally

  # Perform classic model initialization steps if runnign "standalone"

  # Initialize ve.model$ModelState_ls and ve.model$RunParam_ls
  RunParam_ls <- getModelParameters(DotParam_ls=list(...), DatastoreName, LoadDatastore)

  # Archive previous results if SaveDatastore true in RunParam_ls and previous results exist
  archiveErrors <- archiveResults(RunParam_ls=RunParam_ls)
  if ( length(archiveErrors)>0 ) {
    writeLog(paste0("Failed to save prior results (",paste(archiveErrors,collapse=","),").\nContinuing anyway..."),Level="error")
  }

  # Start logger (if not already running)
  # Valid log levels (from least to most important) [ see environment.R ]
  #   trace, debug, info, warn, error, fatal
  LogLevel <- getRunParameter("LogLevel",Param_ls=RunParam_ls)
  initLog(Threshold=LogLevel,Clear=TRUE)

  # Build the model state from RunParam_ls
  # Model will run in the current directory...
  loadModel(RunParam_ls) # RunParam_ls is placed into ve.model$RunParam_ls and builds ve.model$ModelState_ls

  # Running: Save the Log and ModelState files
  saveLog()        # start saving the log for the current results
  setModelState()  # save ve.model$ModelState_ls into file system

  # Run the model (requires ve.model$ModelState_ls and ve.model$RunParam_ls
  prepareModelRun(SimulateRun)

  # Return the resulting model state, invisibly
  invisible(ve.model$ModelState_ls)
}

#LOAD PARAM FILE (run_parameters.json)
#=====================================
#' Load classic run_parameters.json file
#'
#' \code{loadParamFile} a visioneval control function that reads ParamFile (run_parameters.json)
#' and adds it to the Param_ls parameter.
#'
#' @param Param_ls run parameters list to which run_parameters.json will be added (existing values
#' in Param_ls take precedence.
#' @param ModelDir alternative model directory (absolute path)
#' @return Param_ls with missing parameters added from Paramfile
#' @export
loadParamFile <- function(Param_ls=NULL,ModelDir=NULL) {
  if ( ! is.list(Param_ls) ) {
    stop( writeLog("Must supply Param_ls (with $ModelDir) to loadParamFile",Level="error") )
  }
  ParamFile <- getRunParameter("RunParamFile",Param_ls)
  ParamPath <- findRuntimeInputFile(ParamFile,"ParamDir",Param_ls=Param_ls,StopOnError=FALSE)
  if ( ! is.na(ParamPath) ) {
    Param_ls <- loadConfiguration(ParamPath=ParamPath,keep=Param_ls)
    ParamPath <- dirname(ParamPath)
  } else {
    if ( is.null(ModelDir) ) ModelDir <- getRunParameter("ModelDir",Param_ls)
    ParamPath <- file.path(ModelDir,getRunParameter("ParamDir",Default="defs",Param_ls=Param_ls))
    if ( ! dir.exists(ParamPath) ) ParamPath <- NULL
    # The path may exist, but may not have ParamFile within it
  }
  # Make sure RunParam_ls$ParamPath exists (location for Geo.csv, Units.csv, Deflators.csv)
  if ( ! is.null(ParamPath) && ! "ParamPath" %in% names(Param_ls) ) {
    Param_ls <- mergeParameters(
      Param_ls,
      addParamPath_ls <- addParameterSource(list(ParamPath=ParamPath),"loadParamFile()")
    )
  }
  # It will later be an error if ParamPath is not present in Param_ls
  # Error will be trapped when ModelState_ls is constructed seeking GeoFile, UnitsFile, DeflatorsFile
  return(Param_ls)
}

requiredParameters <- c(
  "Model", "Scenario", "Description", "Region",
  "BaseYear", "Years",
#  "DatastoreName", "DatastoreType", "Seed",
#  These will be given default values when we create the ModelState_ls, so don't worry about them here.
  "InputPath","ParamPath","ModelScriptPath","DatastorePath"
)

#VERIFY MODEL PARAMETERS
#=======================
#' Check that a set of run parameters is sufficient to launch a model
#'
#' @param Param_ls The set of parameters to be checked
#' @return A character vector of parmeters that are missing or invalid (empty means everything
#'   required is present)
#' @export
verifyModelParameters <- function(Param_ls) {
  missingParams <- requiredParameters[ ! requiredParameters %in% names(Param_ls) ]
}

#GET MODEL RUN PARAMETERS
#==========================
#' Initialize model run parameters.
#'
#' \code{getModelParameters} a visioneval framework control function that establishes the runtime
#' model environment during model initialization.
#'
#' @param DotParam_ls Function arguments to initializeModel (overridden by stored RunParams in
#' environment; backward compatible).
#' @param DatastoreName A string identifying the full path name of a datastore to load or NULL if
#' an existing datastore in the working directory is to be loaded. If LoadDatastore is FALSE and
#' DatastoreName is provided, LoadDatastore will be set to TRUE with a warning.
#' @param LoadDatastore A logical identifying whether an existing datastore should be loaded
#' (default=FALSE)
#' @return RunParam_ls list of loaded parameters suitable for Loading and Running a model
#' @export
getModelParameters <- function(DotParam_ls=list(),DatastoreName=NULL,LoadDatastore=FALSE) {

  # Incorporate existing run parameters from model environment
  # getModelParameters is typically only called for a classic source("run_model.R") execution
  ve.model <- modelEnvironment()
  Param_ls <- if ( "RunParam_ls" %in% names(ve.model) ) {
    ve.model$RunParam_ls
  } else {
    Param_ls <- list()
  }

  ModelDir <- getRunParameter("ModelDir",Default=".",Param_ls=Param_ls) # Default is working directory
  if ( ! "ModelDir" %in% names(Param_ls) ) {
    Param_ls$ModelDir <- normalizePath(ModelDir,winslash="/",mustWork=FALSE)
  }

  # Propagate explicit arguments into DotParam_ls
  if ( is.character(DatastoreName) ) { # the function parameter, not the Run Parameter
    DotParam_ls[["LoadDatastoreName"]] <- DatastoreName
  }
  if ( ! "LoadDatastore" %in% names(DotParam_ls) ) {
    DotParam_ls[["LoadDatastore"]] <- LoadDatastore;
  }

  # Always keep "Region", "BaseYear" and "Years" from configuration files (not manual override)
  safeList <- c("Region","BaseYear","Years")
  paramNames <- names(Param_ls)
  Param_ls <- Param_ls[ paramNames[ ! paramNames %in% safeList ] ]
  if ( length(Param_ls) > 0 ) {
    DotParam_ls[ names(Param_ls) ] <- Param_ls # Elevate remaining parameters passed as an argument
  }

  # Merge DotParam and configuration file with "safe" parameters from runtime
  DotParam_ls <- addParameterSource(DotParam_ls,"initializeModel(...)")
  RunParam_ls <- loadConfiguration(ParamDir=ModelDir,keep=Param_ls,override=DotParam_ls)

  # Look for defs along InputPath, and run_parameters.json.
  # New style model will already have loaded those from visioneval.cnf in the model root
  # We'll keep the "safe" runtime parameters (see above)
  RunParam_ls <- loadParamFile(Param_ls=RunParam_ls)

  # Clean up some names
  
  if ( ! "ModelScriptPath" %in% names(RunParam_ls) ) {
    if ( "ModelScriptFile" %in% names(RunParam_ls) ) {
      RunParam_ls$ModelScriptPath <- normalizePath(file.path(RunParam_ls$ModelDir,RunParam_ls$ModelScriptFile),winslash="/",mustWork=FALSE)
    }
  }

  # If ResultsDir is not present in RunParam_ls, set it to the current directory (classic version)
  if ( ! "ResultsDir" %in% names(RunParam_ls) ) RunParam_ls[["ResultsDir"]] <- "."

  # Set up default InputPath nd DatastorePath
  if ( ! "InputPath" %in% RunParam_ls ) RunParam_ls$InputPath <- RunParam_ls$ModelDir
  if ( ! "DatastorePath" %in% RunParam_ls ) RunParam_ls$DatastorePath <- RunParam_ls$ResultsDir

  # Check for any other missing parameters
  if ( length( missingParams <- verifyModelParameters(RunParam_ls) ) > 0 ) {
    stop(
      writeLog(paste("Missing Model Parameters:",paste(missingParams,collapse=",")),Level="error")
    )
  }

  return(RunParam_ls)
}

#LOAD MODEL
#==========
#' Load the model state from run parameters
#' \code{loadModel} a visioneval framework control function that loads a VisionEval model without
#' creating any results. This function checks that all required model elements are present in the
#' runtime environment and either loads an existing ModelState_ls (if the model has been previously
#' run and this function is not called in the context of running a new model) or constructs a new
#' ModelState_ls. The function expects the working directory to be the one that contains (or will
#' contain) the ModelState file, the Datastore, and the model run log file.
#'
#' See \code{initializeModel} for details.
#'
#' @param RunParam_ls is returned from \code{getModelParameters}
#' @param Message a character string to report why we're waiting...
#' @param onlyExisting a logical - if TRUE and not modelRunning(), simply load an existing model state.
#'   if FALSE, create a new model state in memory if there is not one already
#' existing model state. Ignored if modelRunning().
#' @param RunDir the directory in which to seek/create the model run
#' @param envir The environment into which to load the ModelState
#' @return The ModelState_ls that was constructed (or NULL if only loading a non-existent one)
#' @export
loadModel <- function(
  RunParam_ls,
  Message = "Initializing Model. This may take some time...",
  onlyExisting = FALSE,
  RunDir = getwd(),
  envir = modelEnvironment()
) {
  # IMPORTANT: Do not write/re-write a model state or log until archiving is done.

  #   Load an existing ModelState and check that the model has everything it needs to run, updating
  #   ModelState as needed. Performs an in-memory "Init" first if no ModelState file exists so the
  #   parsed elements of run_model.R are available for inspection, but it will not save the
  #   ModelState_ls. The ModelState_ls will be saved by the runModel function RunModel is TRUE. This
  #   function will clear the envir environment, load the ModelState, parse the run_model.R
  #   script, check presence of files, check specifications, etc. ModelState_ls remains in the
  #   ve.model environment.

  #==========================
  # SET FLAG IF MODEL RUNNING
  #==========================

  RunModel <- modelRunning(envir=envir)
  owd <- setwd(RunDir)
  on.exit(setwd(owd))

  #===============================
  # GET CURRENT MODEL STATE IF ANY
  #===============================

  ModelStateFile <- getRunParameter("ModelStateFile",Param_ls=RunParam_ls) # Default "ModelState.Rda"

  # Get status of current model state
  # Expect that getwd() contains (or will contain) the ModelState.Rda we're working on
  envir$ModelStatePath <- normalizePath(ModelStateFile,winslash="/",mustWork=FALSE)
  if ( ! grepl("\\.Rda$",envir$ModelStatePath) ) {
    stop(
      writeLog("Configuration Error: ModelStateFile name must have '.Rda' extension",Level="error"),
      call.=FALSE
    )
  }

  #================================
  # ESTABLISH MODEL STATE IN MEMORY
  #================================

  # We'll start saving the model state later
  if ( ! RunModel ) {
    if ( file.exists(envir$ModelStatePath) ) {
      RunParam_ls <- loadModelState(envir$ModelStatePath,envir=envir)
      ms <- envir$ModelState_ls
    } else {
      ms <- list()
    }
    if ( onlyExisting ) {
      # Stop here if we don't want to build a new model state (used in VEModel findModel)
      writeLog("Opened existing ModelState_ls ",Level="info")
      return(ms) # Returns empty list if no ModelState_ls
    } 
  }
  # If we get here we might be running the model, or re-building an in-memory ModelState_ls
  #   for use (e.g.) in reporting the module specifications.

  # Initialize a fresh model state - update later if loading existing datastore
  writeLog("Initializing new ModelState_ls",Level="info")
  RunParam_ls <- initModelState(Save=FALSE,Param_ls=RunParam_ls,envir=envir)

  #==========================================================
  #PARSE RUN SCRIPT FOR MODULE CALLS, CHECK AND COMBINE SPECS
  #==========================================================

  # Set up directories for model run
  ModelDir <- getRunParameter("ModelDir",Param_ls=RunParam_ls)

  # TODO: could do model script parsing earlier (in VEModel).
  # TODO: look in RunParam_ls for ParsedScript
  # ModelScriptPath needs to be the absolute path to the model script
  # It will be provided either in dots, or by the pre-existing RunParam_ls environment (e.g. if
  # built by VEModel). Default is a fixed string, "run_model.R"
  if ( ! "ParsedScript" %in% names(RunParam_ls) ) {
    ModelScriptPath <- getRunParameter("ModelScriptPath",Default=NA,Param_ls=RunParam_ls)
    if ( ! file.exists(ModelScriptPath) ) {
      stop(
        writeLog(
          paste("Unable to locate ModelScript:",ModelScriptPath,sep="\n"),
          Level="error"
        )
      )
    }

    # Parse script and make data frame of modules that are called directly
    parsedScript <- parseModelScript(ModelScriptPath)
  } else {
    parsedScript <- RunParam_ls$ParsedScript
  }
  setModelState(list(ParsedScript_ls=parsedScript),Save=FALSE,envir=envir)

  #==========================================================
  # SAVE RAW MODULE CALLS INTO MODEL STATE FROM PARSED SCRIPT
  #==========================================================

  # Use ModuleCalls_df later in runModule
  setModelState(
    list(ModuleCalls_df=parsedScript$ModuleCalls_df), # Full list of calls for current model state/stage
    Save=FALSE,
    envir=envir
  )

  #===================================================
  # DEVELOP RUN DATASTORE AND LOAD DATASTORE LOCATIONS
  #===================================================

  # Normalized path name of the run datastore from the ModelState, relative to getwd()
  # Note that the DatastoreName in RunParam_ls is decidedly NOT the same as the DatastoreName
  #   in the parameter list to initializeModel; see above for initializing LoadDatastoreName
  #   (the parameter is the path/filename for the *other* Datastore from a previous stage)
  RunDstore <- list()
  RunDstore$Name <- getRunParameter("DatastoreName",Param_ls=RunParam_ls)
  RunDstore$Name <- normalizePath(RunDstore$Name, winslash = "/", mustWork = FALSE)
  RunDstore$Dir  <- dirname(RunDstore$Name)
  RunParam_ls$RunDstore <- RunDstore # for use if this model becomes a VEModel BaseModel
  setModelState(list(RunDstore=RunDstore),Save=FALSE,envir=envir) # for use in this model run

  # Allow explicit function parameter to be overridden from RunParam_ls if defined there
  # May have been changed from function parameter by earlier call to getModelParameters
  LoadDatastore  <- getRunParameter("LoadDatastore",Default=FALSE,Param_ls=RunParam_ls)

  # Set up load datastore parameters if we're loading the Datastore
  LoadDstore <- getRunParameter("LoadDstore",Default=list(),Param_ls=RunParam_ls)
  if ( ! ( is.list(LoadDstore) && all(c("Name","Dir") %in% LoadDstore) ) ) {
    LoadDstore <- list()
    LoadDatastoreName <- getRunParameter("LoadDatastoreName",Default=NA,Param_ls=RunParam_ls)
    if (is.na(LoadDatastoreName) ) {
      if ( LoadDatastore ) {
        # null name means start with the current (existing) Datastore
        # TODO: This use case will be retired (and may never have worked)
        # It's intended to restart a model that is being hacked - the new
        # VEModel framework provides better ways to do that (by explicitly
        # staging the model) - see below
        LoadDstore$Name <- RunDstore$Name
        LoadDstore$Dir <- RunDstore$Dir
      } else { # Nothing to load
        LoadDatastore <- FALSE
      }
    } else {
      LoadDstore$Name <- normalizePath(LoadDatastoreName, winslash = "/", mustWork = FALSE)
      LoadDstore$Dir <- dirname(LoadDstore$Name)
      if ( ! LoadDatastore ) {
        writeLog(
          paste("LoadDatastore is FALSE; Ignoring LoadDatastoreName:",LoadDstore$Name,sep="\n"),
          Level="error"
        )
      }
    }
  }

  # Generate the model loading message
  writeLog(Message,Level="warn")

  #===============================================
  # GET LIST OF PACKAGES FROM PREVIOUS MODEL STATE
  #===============================================

  # Verify that there is a Datastore to load if requested
  LoadEnv <- new.env() # for loaded ModelState
  DstoreConflicts <- character(0)

  # TODO: Use case for loading a previous Datastore on a model that crapped out:
  #       Archive the Datastore first using VEModel convenience function
  #       Then load it with the datastore name in the archive directory as if it were
  #       coming from another model. Doing such a restart implies changing the Model Script.
  #       The changed script can be set at runtime alongside LoadDatastoreName and LoadDatastore
  #       just by providing an alternate ModelScript runtime parameter (e.g. run_model_restart.R)

  # Load the previous model state, if we've asked for LoadDatastore or StartFrom
  if ( LoadDatastore || "StartFromModelState" %in% names(RunParam_ls) ) {
    # In the file system, use the currently configured ModelStateFile
    #  if it's not right, that other model needs to be re-run in
    #  the current environment.
    # TODO: fix up for StartFrom model state
    if ( !is.null(LoadDstore$Dir) ) {
      modelStatePath <- file.path(LoadDstore$Dir,visioneval::getModelStateFileName())
      PreviousRunParam_ls <- loadModelState(modelStatePath,envir=LoadEnv)
      modelStateLoaded <- length(PreviousRunParam_ls)>0
      if ( ! modelStateLoaded ) {
        stop(
          writeLog(
            c(
              "Error loading pre-existing datastore:",
              paste("Prior stage:",LoadDstore$Name),
              paste("Unable to load ModelState:",modelStatePath)
            ),
            Level="error"),
          call.=FALSE
        )
      }
    } else {
      # VEModel will set "StartFromModelState" in a staged model to the previous stage
      #   setup (model state, parameters, run status, etc.)
      # Use pre-loaded ModelState_ls from StartFrom
      startFrom <- getRunParameter("StartFromModelState",Default=list(),Param_ls=RunParam_ls);
      LoadEnv$ModelState_ls <- startFrom$ModelState_ls
    }

    # Save Datastore Directory structure for checking at runtime
    LoadDstore$ModelState_ls <- LoadEnv$ModelState_ls
  }

  # Save the LoadDstore structure for use when model runs
  setModelState(list(LoadDstore=LoadDstore),Save=FALSE,envir=envir)

  # Check for consistency of Load and Save parameters
  # TODO: rework use case for restarting from current Datastore (during debugging for example)
  SaveDatastore <- getRunParameter("SaveDatastore",Param_ls=RunParam_ls)
  if ( RunModel && file.exists(RunDstore$Name) ) {
    if ( ! ( LoadDatastore || SaveDatastore ) ) {
      DstoreConflicts <- c(DstoreConflicts,
        paste(
          "The existing datastore, ",RunDstore$Name," will NOT be overwritten.\n",
          "Set SaveDatastore=TRUE to move the existing results aside.\n",
          "To add to it, set DatastoreName=NULL and set LoadDatastore=TRUE.\n",
          "Or just delete it and try again.\n"
        )
      )
    } else if (
      LoadDatastore &&
      ! SaveDatastore ) {
      DstoreConflicts <- c(DstoreConflicts,
        paste(
          "Loading DatastoreName='",LoadDstore$Name,"'.",
          "Existing datastore ",RunDstore$Name," would be overwritten.",
          "Set SaveDatastore=TRUE to move the existing results aside.\n",
          "Or delete it and try again.\n"
        )
      )
    }
  }

  #=====================================
  #CHECK CONFLICTS WITH LOADED DATASTORE
  #=====================================

  if ( LoadDatastore && length(DstoreConflicts)==0 ) {
    if ( !file.exists(LoadDstore$Name) ) {
      DstoreConflicts <- c(DstoreConflicts,
        paste0(
          "Failed to load existing Datastore ",LoadDstore$Name,".\n",
          "Perhaps the full path name was not specified?\nWas the other model run?\nDoes the output still exist?"
        )
      )
    } else { # loading and file exists
      # Presume that if we're loading the existing one, it's fine!
      # Check whether the datastore to be loaded is consistent with
      #  the current datastore (using its ModelState)

      #Get datastore type from current ModelState
      LoadDstoreType <- (LoadDstore$ModelState_ls$DatastoreType)
      RunDstoreType <- (envir$ModelState_ls$DatastoreType)

      #Check that run and load datastores are of same type
      if ( RunDstoreType != LoadDstoreType) {
        DstoreConflicts <- c(DstoreConflicts, paste(
          "Incompatible datastore types.\n",
          "This Model Run has type: ", RunDstoreType, ".\n",
          "Load Datastore has type: ", LoadDstoreType, "."
        ))
      }

      #Check that geography, units, deflators and base year are consistent
      BadGeography <- !isTRUE(all.equal(envir$ModelState_ls$Geo_df, LoadDstore$ModelState_ls$Geo_df))
      BadUnits <- !isTRUE(all.equal(envir$ModelState_ls$Units, LoadDstore$ModelState_ls$Units))
      BadDeflators <- !isTRUE(all.equal(envir$ModelState_ls$Deflators, LoadDstore$ModelState_ls$Deflators))
      BadBaseYear <- ! (envir$ModelState_ls$BaseYear == LoadDstore$ModelState_ls$BaseYear)

      if ( BadGeography || BadUnits || BadDeflators || BadBaseYear) {
        elements <- paste(
          "Geography"[BadGeography],
          "Units"[BadUnits],
          "Deflators"[BadDeflators],
          "Base Year"[BadBaseYear],
          sep="_"
        )
        elements <- gsub("_",", ",gsub("^_+|_+$","",elements))
        DstoreConflicts <- c(DstoreConflicts,
          paste(
            "Inconsistent ",elements," between Model and Loaded Datastore/Base Model."
          )
        )
      }
    }
  }

  #=============================================
  # REPORT RESULTS OF DATABASE CONSISTENCY CHECK
  #=============================================

  # Abort if we have irresolvable (not just warning) conflicts
  if (length(DstoreConflicts) != 0) {
    writeLog(DstoreConflicts,Level="error")
    stop("Datastore configuration error: See log messages.",call.=FALSE)
  }

  #===========================
  # IDENTIFY REQUIRED PACKAGES
  #===========================

  # Create required package list (cumulative across all run steps)
  # Add from previous ModelState if any (StartFrom, LoadDatastoreName)
  AlreadyInitialized <- character(0) # required (initialized packages) from loaded datastore
  RequiredPackages <- parsedScript$RequiredVEPackages;

  if ( LoadDatastore || "ModelState_ls" %in% names(LoadDstore) ) {
    # Copy over the required packages part of the loaded model state
    # That's all we need so we can parse the current module calls successfully
    if ( "RequiredVEPackages" %in% names(LoadDstore$ModelState_ls) ) {
      AlreadyInitialized <- LoadDstore$ModelState_ls$RequiredVEPackages
      RequiredPackages <- unique(c(RequiredPackages, LoadDstore$ModelState_ls$RequiredVEPackages))
    }
  }

  #======================================================
  # EXTRACT INPUT/OUTPUT SPECIFICATIONS FROM MODULE CALLS
  #======================================================

  writeLog("Parsing Module Calls...",Level="info")
  parseModuleCalls(
    parsedScript$ModuleCalls_df,
    AlreadyInitialized,
    RequiredPackages,
    Save=FALSE
  ) # sets ModelState entries...

  #==============================
  # ENSURE RunParam_ls UP TO DATE
  #==============================

  envir$RunParam_ls <- RunParam_ls
  setModelState(list(RunParam_ls=RunParam_ls),Save=FALSE,envir=envir)

  # If not running model, we've done everything needed from initializeModel. Additional setup
  # operations (parsing model script, examining StartFrom, loading module specifications
  # can be done externally in VEModel, for example)
  return(envir$ModelState_ls)
}

#PREPARE MODEL RUN
#=================
#' Set up Datastore and ModelState_ls to run a model or model stage
#'
#' \code{prepareModelRun} a visioneval framework control function that prepares the Datastore and
#' ModelState_ls for running a VisionEval model, and loads the model's input files into the
#' Datastore.
#'
#' See \code{initializeModel} for other details.
#'
#' @param SimulateRun A logical identifying whether the model run should be
#' simulated: i.e. each step is checked to confirm that data of proper type
#' will be present when the module is called. The Datastore is created but
#' the simulation will happen before anything starts getting loaded, and will
#' report if the run_model.R script does not generate all required datasets
#' plus other oddities in the model_script. Saves time when trying out a new
#' combination of modules; don't bother using with a model that already runs.
#' @param envir The environment in which to see ModelState_ls
#' 
#' @return The ModelState_ls that was constructed, invisibly. It will have been
#' updated with the Datastore inventory, the model run status and a few other things.
#'
#' @export
prepareModelRun <- function(
  SimulateRun = FALSE,
  envir = modelEnvironment()
) {

  if ( ! all( c("RunParam_ls","ModelState_ls") %in% names(envir) ) ) {
    envir$RunModel <- FALSE
    stop(
      writeLog("Model State or Run parameters not created: call loadModel or initializeModel",Level="error")
    )
  }
  writeLog("Preparing Model Run...",Level="info")

  # Establish run environment
  RunParam_ls <- envir$ModelState_ls$RunParam_ls # Both ModelState_ls and RunParam_ls must exist...
  RunModel <- envir$RunModel <- TRUE

  #### IMPORTANT: The working directory must be the place we want to write the ModelState file and
  ####            create the Datastore. The "ResultsDir" (plus RunStep/Scenario)

  # ModelStatePath may be a subdirectory of ResultsDir if the model is staged
  RunDirectory <- envir$ModelState_ls$ModelStatePath

  owd <- setwd(RunDirectory)
  on.exit(setwd(owd))

  #   Initialize the Datastore by loading an existing the "Datastore" (if LoadDatastore is TRUE), or
  #   by creating a new blank Datastore.

  #=============================================
  #ESTABLISH THE DATASTORE INTERACTION FUNCTIONS
  #=============================================
  assignDatastoreFunctions()

  #==================================
  # LOAD OTHER DATASTORE IF SPECIFIED
  #==================================
  # Need to clear existing Datastore, if there is one, and install
  # the Datastore contents from the LoadDatastoreName

  writeLog("Preparing Data Store...",Level="info")

  LoadDatastore  <- getRunParameter("LoadDatastore",Default=FALSE,Param_ls=RunParam_ls)
  if ( ! LoadDatastore ) {
    # not loading, so make a new Datastore
    initDatastore() # Deletes any existing Datastore
    initDatastoreGeography()
    loadModelParameters() # i.e. model_parameters.json - an input file historically located in "defs"
  } else {
    # Grab LoadDstore parameters from the ModelState
    LoadDstore <- envir$ModelState_ls$LoadDstore
    RunDstore <- envir$ModelState_ls$RunDstore;

    loadEnv <- new.env()
    loadEnv$ModelState_ls <- LoadDstore$ModelState_ls

    # Remove existing Datastore if we're not re-using it
    if ( file.exists(RunDstore$Name) ) {
      writeLog(paste0("Removing existing Datastore at ",RunDstore$Name),Level="info")
      unlink(RunDstore$Name,recursive=TRUE)
    }

    # Copy the loaded file/directory Datastore hierarchy
    loadDatastoreCopy <- tempfile(tmpdir=getwd(),pattern="Datastore_")
    dir.create(loadDatastoreCopy)
    copyDatastore(
      ToDir=loadDatastoreCopy,
      envir=loadEnv, # Source Datastore, by way of its ModelState
      Flatten=TRUE
    )
    file.rename(file.path(loadDatastoreCopy,basename(LoadDstore$Name)),RunDstore$Name)
    unlink(loadDatastoreCopy,recursive=TRUE)
    writeLog(paste("Copied previous datastore from:",LoadDstore$Name),Level="info")
    writeLog(paste("Copied datastore            to:",RunDstore$Name),Level="info")

    # Copy datastore inventory for loaded datastore into current ModelState
    # The Datastore listing needs to be ready before we update the Year groups
    listDatastore()

    #=================================================
    # HANDLE USE CASE WHERE SOME YEARS ALREADY DEFINED
    #=================================================
    # TODO: Key feature is to make sure all the geography is present for the new
    #  year. This will probably work for both BaseModel and LoadDatastore variations.

    # Each ModelStage needs to have groups available locally (i.e. materialized in its own
    # Datastore) for the years it is evaluating (even if those years might also be
    # present in an earlier ModelStage).

    # Initialize geography for years not present in datastore
    # Handles model stages where the new stage adds one or more Years
    #  instead of (or in addition to) adding module calls.
    RunYears_ <- envir$ModelState_ls$Years
    LoadYears_ <- LoadDstore$ModelState_ls$Years
    if (!all(RunYears_ == LoadYears_)) {
      NewYears_ <- RunYears_[!(RunYears_ %in% LoadYears_)]
      # NOTE: initDatastore and initDatastoreGeography are performed separately
      # as initDatastore depends on DatastoreType (RD vs H5), whereas
      # initDatastoreGeography will use the assigned functions 
      initDatastore(AppendGroups = NewYears_)
      initDatastoreGeography(GroupNames = NewYears_)
      loadModelParameters(FlagChanges=TRUE)
      # FlagChanges: A bunch of warnings are printed if the model parameters changed
      # TODO: Abandon ship if there are significant changes to the modelp parameters between
      #       this model run and the earlier one that is being loaded.
    }
  }

  #===================
  #CAPTURE RunParam_ls
  #===================
  setModelState(list(RunParam_ls=RunParam_ls),Save=RunModel)
  envir$RunParam_ls <- RunParam_ls # Save the run parameters explicitly as well

  #==================
  #SIMULATE MODEL RUN
  #==================

  # Use this for debugging the run_model.R script to ensure that every "Get" specification
  # has an earlier "Set" specification (with something in the Datastore). Takes a bit of extra time,
  # but saves running the model for a long time only to have it fail. Use this when first trying
  # out a new run_model.R script
  SimulateRun  <- getRunParameter("SimulateRun",Default=SimulateRun,Param_ls=RunParam_ls)
  if (SimulateRun) {
    writeLog("Simulating Model Run...",Level="info")
    simDataTransactions(envir$ModelState_ls$AllSpecs_ls)
  }

  #===============================
  #CHECK AND PROCESS MODULE INPUTS
  #===============================

  writeLog("Adding input files...",Level="info")
  processInputFiles(envir$ModelState_ls$AllSpecs_ls) # Add them to the Datastore

  #============================
  # PRINT MODEL RUN STEP HEADER
  #============================

  # writeLogMessage adds "bare messages" to the "ve.logger"
  # intended as metadata for the model run
  writeLogMessage(
    paste(
      paste("Name:",envir$ModelState_ls$Model),
      paste("Scenario:",envir$ModelState_ls$Scenario),
      paste("Description:",envir$ModelState_ls$Description),
      paste("Timestamp:",envir$ModelState_ls$FirstCreated),
      sep="\n"
    )
  )
  writeLog("Model successfully initialized.",Level="warn")

  #=========================
  # DONE WITH INITIALIZATION
  #=========================

  invisible(envir$ModelState_ls)
}

#===============
#REQUIRE PACKAGE
#===============
#' Require package.
#'
#' \code{requirePackage} a visioneval control function that
#' introduces a package dependency.
#'
#' This function simply returns TRUE. It is used to state a module
#' dependency explicitly to support internal Module calls that do not
#' explicitly name a package.
#'
#' @param Package During parsing, package is added to the list of
#'   packages to be searched for modules. Otherwise ignored.
#' @return TRUE. The function returns TRUE.
#' @export
requirePackage <- function(Package) TRUE

#==========
#RUN MODULE
#==========
#' Run module.
#'
#' \code{runModule} a visioneval framework model user function that runs a module.
#'
#' This function runs a module for a specified year.
#'
#' @param ModuleName A string identifying the name of a module object.
#' @param PackageName A string identifying the name of the package the module is
#'   a part of.
#' @param RunFor A string identifying whether to run the module for all years
#' "AllYears", only the base year "BaseYear", or for all years except the base
#' year "NotBaseYear".
#' @param RunYear A string identifying the run year.
#' @param StopOnErr a logical (default TRUE); if FALSE, report errors
#' and continue anyway
#' @return list returned from module function, with Errors and Warnings as
#'   attributes.
#' @export
runModule <- function(ModuleName, PackageName, RunFor, RunYear, StopOnErr = TRUE) {

  if ( ! modelRunning() ) invisible( list(Errors=character(0),Warnings="Model Not Running") )
  
  #Check whether the module should be run for the current run year
  #---------------------------------------------------------------
  BaseYear <- getModelState()$BaseYear
  if (RunYear == BaseYear & RunFor == "NotBaseYear") {
    return()
  }
  if (RunYear != BaseYear & RunFor == "BaseYear") {
    return()
  }
  #Log and print starting message
  #------------------------------
  ModuleFunction <- paste0(PackageName, "::", ModuleName)
  ModuleSpecs <- paste0(ModuleFunction, "Specifications")
  Msg <- paste0("Start  module '", ModuleFunction, "' for year '", RunYear, "'.")
  writeLog(Msg,Level="warn")
  #Load the package and module
  #---------------------------
  M <- list()
  M$Func <- eval(parse(text = ModuleFunction))
  M$Specs <- processModuleSpecs(eval(parse(text = ModuleSpecs)))
  #Load any modules identified by 'Call' spec if any
  if (is.list(M$Specs$Call)) {
    Call <- list(
      Func = list(),
      Specs = list()
    )
    for (Alias in names(M$Specs$Call)) {
      #Called module function when specified as package::module
      CallFunction <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(CallFunction, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == CallFunction) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          CallFunction <-
            paste(Pkg_df$Package[Pkg_df$Module == CallFunction], CallFunction, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          CallFunction <-
            paste(Pkg_df$Package[Pkg_df$Module == CallFunction], CallFunction, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      CallSpecs <- paste0(CallFunction, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = CallFunction))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = CallSpecs)))
      Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
    }
  }
  #Initialize vectors to store module errors and warnings
  #------------------------------------------------------
  Errors_ <- character(0)
  Warnings_ <- character(0)

  #Run module
  #----------
  if (M$Specs$RunBy == "Region") {
    #Get data from datastore
    L <- getFromDatastore(M$Specs, RunYear = RunYear)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = RunYear)
      }
    }
    #Run module
    if (exists("Call")) {
      R <- M$Func(L, Call$Func)
    } else {
      R <- M$Func(L)
    }
    #Save results in datastore if no errors from module
    if (is.null(R$Errors) ) {
      setInDatastore(R, M$Specs, ModuleName, Year = RunYear, Geo = NULL)
    }
    #Add module errors and warnings if any
    Errors_ <- c(Errors_, R$Errors)
    Warnings_ <- c(Errors_, R$Warnings)
    #Handle errors
    if (!is.null(R$Errors)) {
      writeLog(Errors_,Level="error")
      Msg <-
        paste0("Module ", ModuleFunction, " has reported one or more errors. ",
               "Check log for details.")
      stop(Msg)
    }
    #Handle warnings
    if (!is.null(R$Warnings)) {
      writeLog(Warnings_,Level="error")
      Msg <-
        paste0("Module ", ModuleFunction, " has reported one or more warnings. ",
               "Check log for details.")
      warning(Msg)
      rm(Msg)
    }
  } else {
    #Identify the units of geography to iterate over
    GeoCategory <- M$Specs$RunBy
    #Create the geographic index list
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, RunYear)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, RunYear)
      }
    }
    #Run module for each geographic area
    Geo_ <- readFromTable(GeoCategory, GeoCategory, RunYear)
    for (Geo in Geo_) {
      #Get data from datastore for geographic area
      L <-
        getFromDatastore(M$Specs, RunYear, Geo, GeoIndex_ls)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
            getFromDatastore(Call$Specs[[Alias]], RunYear = RunYear, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
        }
      }
      #Run model for geographic area
      if (exists("Call")) {
        R <- M$Func(L, Call$Func)
      } else {
        R <- M$Func(L)
      }
      #Save results in datastore if no errors from module
      if (is.null(R$Errors)) {
        setInDatastore(R, M$Specs, ModuleName, RunYear, Geo, GeoIndex_ls)
      }
      #Add module errors and warnings if any
      Errors_ <- c(Errors_, R$Errors)
      Warnings_ <- c(Errors_, R$Warnings)
      #Handle errors
      if (!is.null(R$Errors)) {
        writeLog(Errors_,Level="error")
        Msg <-
          paste0("Module ", ModuleFunction, " has reported one or more errors. ",
                 "Check log for details.")
        stop(Msg)
      }
      #Handle warnings
      if (!is.null(R$Warnings)) {
        Msg <-
          paste0("Module ", ModuleFunction, " has reported one or more warnings. ",
                 "Check log for details.")
        writeLog(c(Msg,Warnings_),Level="warn")
      }
    }
  }
  #Log and print ending message
  #----------------------------
  Msg <- paste0("Finish module '", ModuleFunction, "' for year '", RunYear, "'.")
  writeLog(Msg,Level="warn")

  if ( length(Warnings_) > 0 ) {
    writeLog(Warnings_,Level="warn")
  }
  if ( length(Errors_) > 0 ) {
    writeLog(Errors_,Level="error")
    if ( StopOnErr ) stop("runModule call failed") else warning("Not stopping for errors...")
  }

  invisible(
    list(
      Errors = Errors_,
      Warnings = Warnings_
    )
  )
}

#RUN SCRIPT
#=========================================================
#' Run an R function or script file as if it were a module
#'
#' \code{runScript} a visioneval framework module developer function that runs a function in
#' run_model.R as if it were a packaged module.
#'
#' This function runs a function based module for a specified year. The module function can be passed
#' as an R function, than name of an R function, the name of a script file, or as an (exported)
#' module name from a package - the last will reproduce runModule functionality, except that you can
#' provide a revised specification list. If a script file is passed, it is expected to have the same
#' components (at least module function and module specifications) as a package-based module. The
#' script will be run during the "initializeModel" process, so if it has estimation code included,
#' that will be run before the rest of the model.
#'
#' This function does NOT write to the Datastore by default. run_model.R can capture the returned
#' output structure. The function will run "standalone" provided a compatible ModelState and
#' Datastore have been loaded (exist in ve.model). Do VEModel$run to set that up, for example.
#'
#' @param Module An R function or character string function name (or string identifying a
#'   file to source) containing module code
#' @param Specification An R specification list or NULL (default), in which case a list
#'   will be sought using the name 
#' @param RunFor A string identifying whether to run the module for all years
#' "AllYears", only the base year "BaseYear", or for all years except the base
#' year "NotBaseYear".
#' @param RunYear A string identifying the run year.
#' @param writeDatastore A logical indicating whether or not to write the results into
#'   the current Datastore, or just to return them
#' @return (invisible) The list of results returned by the module
#' @export
runScript <- function(Module, Specification=NULL, RunFor, RunYear, writeDatastore = FALSE) {
  #Locate the Module function and Specification
  #  Substitute/Deparse to get the object/name passed as "Module"
  ModuleSource <- substitute(Module)
  ModuleFunc <- (Module)

  if ( ! modelRunning() ) invisible( list(Errors=character(0),Warnings="Model Not Running") )
  
  #Set up failure message
  #----------------------
  badModule <- function(ModuleName,Msg) {
    if ( missing(Msg) ) {
      Msg <- paste0(ModuleName, ": Cound not find module")
    }
    writeLog(Msg,Level="error")
    stop(Msg)
  }

  if ( is.function(ModuleFunc) ) {
    #  If module is a function and specification is a list, just use those
    ModuleName <- deparse(ModuleSource)
  } else if ( is.character(ModuleFunc) ) {
    # see if "ModuleFunc" is really "ModuleName"
    if ( exists(ModuleFunc,parent.frame()) ) {
      ModuleFunc <- get(ModuleFunc,parent.frame())
      if ( ! is.function(ModuleFunc) ) badModule("Not a module function.")
    } else {
      if ( grepl("\\.R$",ModuleFunc) ) {
        ModuleFile <- ModuleFunc
        ModuleName <- basename(sub("\\.R$","",ModuleFile))
        ModuleEnv <- new.env(parent.frame())
        sys.source(ModuleFile,envir=ModuleEnv)
        ModuleFunc <- get0(ModuleName,envir=ModuleEnv)
        if ( ! is.function(ModuleFunc) ) badModule(ModuleName)
        environment(ModuleFunc) <- ModuleEnv
      } else badModule(ModuleName)
    }
  } else badModule(ModuleName)

  # Find the specifications
  if ( is.list(Specification) ) {
    ModuleSpecs <- Specification
  } else if ( is.character(Specification) ) {
    SpecName <- paste0(ModuleName, "Specifications")
    if ( exists(SpecName,environment(ModuleFunc)) ) { # also searches parent.frame
      ModuleSpecs <- get0(SpecName,environment(ModuleFunc))
    } else badModule(ModuleName,"Cannot find module specifications")
  }
  if ( ! is.list(ModuleSpecs) ) badModule(ModuleName,"Specifications not in valid format (list)")

  #Log and print starting message
  #------------------------------
  Msg <-
    paste0("Starting script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg,Level="warn")
  print(Msg)

  #---------------------------------------------------------------
  #Set up the module function and specifications
  #---------------------------------------------------------------
  M <- list()
  M$Func <- ModuleFunc
  M$Specs <- processModuleSpecs(ModuleSpecs)

  # TODO: Factor the following out to support both runModule and runScript

  #Load any modules identified by 'Call' spec if any
  if (is.list(M$Specs$Call)) {
    Call <- list(
      Func = list(),
      Specs = list()
    )
    for (Alias in names(M$Specs$Call)) {
      #Called module function when specified as package::module
      CallFunction <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(CallFunction, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == CallFunction) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          CallFunction <-
            paste(Pkg_df$Package[Pkg_df$Module == CallFunction], CallFunction, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          CallFunction <-
            paste(Pkg_df$Package[Pkg_df$Module == CallFunction], CallFunction, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      CallSpecs <- paste0(CallFunction, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = CallFunction))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = CallSpecs)))
      Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
    }
  }
  #Initialize vectors to store module errors and warnings
  #------------------------------------------------------
  Errors_ <- character(0)
  Warnings_ <- character(0)
  #Run module
  #----------
  R <- list()
  if (M$Specs$RunBy == "Region") {
    #Get data from datastore
    L <- getFromDatastore(M$Specs, RunYear = RunYear)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = RunYear)
      }
    }
    #Run module
    if (exists("Call")) {
      R <- M$Func(L, Call$Func)
    } else {
      R <- M$Func(L)
    }
    #Save results in datastore if no errors from module
    if (writeDatastore && is.null(R$Errors)) {
      setInDatastore(R, M$Specs, ModuleName, Year = RunYear, Geo = NULL)
    }
    #Add module errors and warnings if any
    Errors_ <- c(Errors_, R$Errors)
    Warnings_ <- c(Errors_, R$Warnings)
    #Handle errors
    if (!is.null(R$Errors)) {
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more errors. ",
               "Check log for details.")
      writeLog(c(Msg,Errors_),Level="errors")
    }
    #Handle warnings
    if (!is.null(R$Warnings)) {
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more warnings. ",
               "Check log for details.")
      writeLog(c(Msg,Warnings_),Level="warn")
    }
  } else {
    #Identify the units of geography to iterate over
    GeoCategory <- M$Specs$RunBy
    #Create the geographic index list
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, RunYear)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, RunYear)
      }
    }
    #Run module for each geographic area
    Geo_ <- readFromTable(GeoCategory, GeoCategory, RunYear)
    for (Geo in Geo_) {
      #Get data from datastore for geographic area
      L <-
        getFromDatastore(M$Specs, RunYear, Geo, GeoIndex_ls)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
            getFromDatastore(Call$Specs[[Alias]], RunYear = RunYear, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
        }
      }
      #Run model for geographic area
      if (exists("Call")) {
        R <- M$Func(L, Call$Func)
      } else {
        R <- M$Func(L)
      }
      #Save results in datastore if no errors from module
      if (writeDatastore && is.null(R$Errors)) {
        setInDatastore(R, M$Specs, ModuleName, RunYear, Geo, GeoIndex_ls)
      }
      #Add module errors and warnings if any
      Errors_ <- c(Errors_, R$Errors)
      Warnings_ <- c(Errors_, R$Warnings)
      #Handle errors
      if (!is.null(R$Errors)) {
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more errors. ",
                 "Check log for details.")
        writeLog(c(Msg,Errors_),Level="error")
      }
      #Handle warnings
      if (!is.null(R$Warnings)) {
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more warnings. ",
                 "Check log for details.")
        writeLog(c(Msg,Warnings_),Level="warn")
      }
    }
  }

  #Log and print ending message
  #----------------------------
  Msg <-
    paste0("Finish module '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg,Level="warn")
  print(Msg)

  #Return error and warning messages
  #--------------------------------------------------
  attr(R,"Errors") <- Errors_
  attr(R,"Warnings") <- Warnings_
  return(R)
}
