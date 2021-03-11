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
#' simulated: i.e. each step is checked to confirm that data of proper type
#' will be present when the module is called. JRaw says: Just do it
#' @param ... Additional named arguments that can override run parameters from
#' VisonEval.cnf or from run_parameters.json. See Run Parameters section.
#' @return The ModelState_ls that was constructed, invisibly.
#' @export
initializeModel <-
function(
  LoadDatastore = FALSE,
  DatastoreName = NULL, # WARNING: different from "DatastoreName" loaded as a run parameter
  SimulateRun = FALSE,
  ...
) {
  # Performs the following operations
  #
  #   Regardless of RunModel state will load an existing ModelState and check that the model has
  #   everything it needs to run, updating ModelState as needed. Performs an in-memory "Init" first
  #   if no ModelState file exists so the parsed elements of run_model.R are available for
  #   inspection, but it will not save the ModelState_ls. The ModelState_ls will only be saved when
  #   RunModel is TRUE. This function will clear the ve.model environment, load the ModelState,
  #   parse the run_model.R script, check presence of files, check specifications, etc.
  #   ModelState_ls remains in the ve.model environment.
  #
  #   If RunModel is TRUE, save the ModelState_ls if it was created from scratch. Initialize the
  #   Datastore by loading an existing the "Datastore" (if LoadDatastore is TRUE), or by creating a
  #   new blank Datastore. If SaveDatastore and one already exists, rename it before creating the
  #   new one; otherwise throw an error if Datastore exists. Presumes that ModelState in ve.model is
  #   current. Once initializeModel(RunModel=TRUE,...) completes, runModule / runScript / runStage
  #   steps can be performed.

  #===================================================
  # ESTABLISH MODEL ENVIRONMENT AND RUNTIME PARAMETERS
  #===================================================

  #### IMPORTANT: The working directory must be the place we want to write the ModelState file and
  ####            create the Datastore. The "ResultsDir" (plus RunStep/Scenario)

  # Access the model environment and check for RunModel condition
  ve.model <- modelEnvironment(Clear="") # clear ve.model environment (but don't destroy pre-existing RunParam_ls)
  RunModel <- modelRunning()

  # Check for pre-existing elements in ve.model environment (e.g. from VEModel$run) and RunParam_ls found there
  Param_ls <- addParameterSource(
    get0( "RunParam_ls", envir=ve.model, ifnotfound=list() ),
    "RunParam_ls in modelEnvironment()"
  )

  ModelDir <- getRunParameter("ModelDir",Param_ls=Param_ls) # Default is working directory

  # External environment will override dots (which are vestigial)
  DotParam_ls <- addParameterSource(list(...),"initializeModel(...)")
  if ( is.character(DatastoreName) ) { # the function parameter, not the Run Parameter
    DotParam_ls[["LoadDatastoreName"]] <- DatastoreName
  }
  if ( "Param_ls" %in% DotParam_ls ) {
    DotParam_ls[ names(Param_ls) ] <- Param_ls; # Elevate parameters passed as an argument
  }
  RunParam_ls <- loadConfiguration(ParamDir=ModelDir,keep=Param_ls,override=DotParam_ls)

  # Look for defs along InputPath, and run_parameters.json. Fail if file not found.
  # Note that we can't change parameters listed in run_parameters.json
  # Those represent model setup invariants (e.g. inputs, defs locations)
  ParamPath <- findRuntimeInputFile("run_parameters.json","ParamDir",Param_ls=RunParam_ls)
  RunParam_ls <- loadConfiguration(ParamPath=ParamPath,override=RunParam_ls)

  # ModelScriptFile needs to be the absolute path to the model script
  # It will be provided either in dots, or by the pre-existing RunParam_ls environment (e.g. if
  # built by VEModel). Default is a fixed string, "run_model.R"
  ModelScriptFile <- getRunParameter("ModelScriptFile",Param_ls=RunParam_ls)

  # If ResultsDir is not present in RunParam_ls, set it to the current directory
  if ( ! "ResultsDir" %in% names(RunParam_ls) ) RunParam_ls[["ResultsDir"]] <- "."

  # If the ModelScriptFile is relative, normalize it to ModelDir, which is either
  # working directory.
  if ( ! any(grepl("^([[:alpha:]]:[\\/]|[\\/])",ModelScriptFile)) ) {
    # if relative path, normalize relative to ModelDir (which is probably ".")
    ModelScriptFile <- normalizePath(
      file.path(
        ModelDir,
        ModelScriptFile
      ), winslash="/"
    )
  }

  ModelStateFileName <- getRunParameter("ModelStateFileName",Param_ls=RunParam_ls)

  #=========================================
  # CONFIGURE SAVE MODEL STATE AND DATASTORE
  #=========================================

  # Configure default for SaveDatastore run parameter
  SaveDatastore <- if ( "SaveDatastore" %in% names(RunParam_ls) ) {
    RunParam_ls$SaveDatastore
  } else TRUE # Save by default
  # If SaveDatastore is TRUE, any existing ModelState and Datastore will be renamed rather
  #  than deleted and over-written

  # Timestamp will be placed in new ModelState (and possibly used to rename an old one)
  Timestamp <- gsub(":","-",gsub(" ", "_",Sys.time())) # Suitable for use in file name

  SaveParameters <- list()
  if ( RunModel && SaveDatastore ) {
    # Set up model state transition parameters
    SaveParameters$previousModelState <- NULL
    SaveParameters$previousModelStateName <- ""
    SaveParameters$savedPreviousModelState <- FALSE
    SaveParameters$previousTimestamp <- Timestamp
  }

  #=======================
  # START MODEL RUN LOGGER
  #=======================

  # Valid log levels (from least to most important) [ see environment.R ]
  #   trace, debug, info, warn, error, fatal
  LogLevel <- getRunParameter("LogLevel",Param_ls=RunParam_ls)
  logState <- initLog(Timestamp,Threshold=LogLevel,Save=RunModel) # start/reset the model run logger
  # Save logState parameters later to ModelState after it is created
  # Includes log file name for later cross-checks

  #===============================
  # GET CURRENT MODEL STATE IF ANY
  #===============================

  # Get status of current model state
  # Expect that getwd() contains (or will contain) the ModelState.Rda we're working on
  currentModelStatePath <- normalizePath(ModelStateFileName,winslash="/",mustWork=FALSE)
  if ( ! grepl("\\.Rda$",currentModelStatePath) ) {
    stop(
      writeLog("Configuration Error: ModelStateFileName name must have '.Rda' extension",Level="error"),
      call.=FALSE
    )
  }
  currentModelStateExists <- file.exists(currentModelStatePath)
  SaveParameters$currentModelStatePath <- currentModelStatePath;

  saveModelState <- FALSE # Always start out FALSE; set to TRUE once we're running

  #===================================
  # INSTALL MODEL STATE IN ENVIRONMENT
  #===================================

  if (
    RunModel ||                 # always make a new one if running the model (and save the last one)
    ! currentModelStateExists ) # if just opening a fresh model, initialize one in memory
  {
    if ( RunModel ) writeLog("Initializing Model. This may take a while")
    # TODO: Why does this "take a while"? (it does) There's just not that much processing...

    # If running and the ModelState already exists and we want to SaveDatastore
    #   Set the ModelState aside
    # TODO: This should be done at the level of ResultsDir, and it should sweep up
    #   both the ModelState.Rda and the Datastore, plus any Log files
    # TODO: This should only be done when we Run, and at the same time we save
    #   the Datastore. We could simply not save the current ModelState (Save=FALSE)
    #   and then start saving the ModelState later when we get to the RunModel part.
    if ( currentModelStateExists && RunModel && SaveDatastore ) {
      SaveParameters <- archiveModelState(currentModelStatePath,SaveParameters)
      # unlike archiveDatastore, this MOVES the model state aside
      # it may update SaveParameters$previousTimestamp
    }

    # Build a new ModelState_ls (in modelEnvironment())
    # Loads the units, deflators and model geography
    # Will only save it if running the model, otherwise it's just in memory
    # TODO: there is "overall model init", which can be entirely virtual:
    #   process all the scripts/run steps and load up the input specifications
    # TODO: need a complete document/directory of what gets put into the ModelState
    initModelState(Save=RunModel,Param_ls=RunParam_ls)
  } else { # ModelState file exists and not running model
    # open the existing model state
    loadModelState(currentModelStatePath)
  }

  # ===================================
  # PUT LOG PARAMETERS INTO MODEL STATE
  # ===================================
  
  # We started the log before we had a ModelState into which to save its configuration
  if ( length(logState)>0 ) {
    setModelState(logState,Save=RunModel)
  }

  #===========================================================================
  #PARSE SCRIPT TO MAKE TABLE OF ALL THE MODULE CALLS, CHECK AND COMBINE SPECS
  #===========================================================================

  # Parse script and make data frame of modules that are called directly
  # TODO: this needs to happen "per run step", since each step may have a different script
  parsedScript <- parseModelScript(ModelScriptFile)

  #===================================================
  # DEVELOP RUN DATASTORE AND LOAD DATASTORE LOCATIONS
  #===================================================

  # Normalized path name of the run datastore from the ModelState, relative to getwd()
  # Note that the DatastoreName in RunParam_ls is decidedly NOT the same as the DatastoreName
  #   in the parameter list to initializeModel; see above for initializing LoadDatastoreName
  #   (the parameter is the path/filename for the *other* Datastore from a previous stage)
  RunDstoreName <- normalizePath(RunParam_ls$DatastoreName, winslash = "/", mustWork = FALSE)
  RunDstoreDir <- dirname(RunDstoreName)
  RunDstoreFile <- basename(RunDstoreName)

  # Set up load datastore parameters
  if (is.null(DatastoreName) ) { # DatastoreName here is NOT the RunDstoreName
    if ( LoadDatastore ) {
      # null name means start with the current (existing) Datastore
      LoadDstoreName <- RunDstoreName
      LoadDstoreDir <- RunDstoreDir
    } else { # Nothing to load
      LoadDatastore <- FALSE
      LoadDstoreName <- NULL
      LoadDstoreDir <- NULL
    }
  } else {
    LoadDstoreName <- normalizePath(RunParam_ls$LoadDatastoreName, winslash = "/", mustWork = FALSE)
    LoadDstoreDir <- dirname(LoadDstoreName)
    if ( ! LoadDatastore ) {
      writeLog(
        c(
          paste("DatastoreName:",basename(LoadDstoreName)),
          "Setting LoadDatastore to TRUE"
        ),Level="warn"
      )
      LoadDatastore <- TRUE # must load if there's a name provided
    }
  }

  # Flag if we're trying to (re-)load the existing Datastore
  LoadExistingDatastore <- LoadDatastore && (RunDstoreName == LoadDstoreName) && (RunDstoreDir == LoadDstoreDir)

  #==========================================================
  # SAVE RAW MODULE CALLS INTO MODEL STATE FROM PARSED SCRIPT
  #==========================================================

  setModelState(
    list(ModuleCalls_df=parsedScript$ModuleCalls_df), # Full list of calls for current model state/stage
    Save=RunModel
  )

  #===========================
  # IDENTIFY REQUIRED PACKAGES
  #===========================

  # Create required package list (cumulative across all run steps)
  # Add from previous ModelState if any (BaseModel, LoadDatastore)
  AlreadyInitialized <- character(0) # required (initialized packages) from loaded datastore
  RequiredPackages <- parsedScript$RequiredVEPackages;

  #===============================================
  # GET LIST OF PACKAGES FROM PREVIOUS MODEL STATE
  #===============================================

  # TODO: previous model state will be the one for the BaseModel / BaseRunStep, if configured
  # TODO: alternatively, it is the model state for the LoadDatastoreName
  # TODO: Difference is just how we link/access the previous Datastore

  # Load the previous model state, if available
  LoadEnv <- new.env() # for loaded ModelState
  if ( LoadDatastore ) {
    # Find the other Datastore that will become the basis for this one
    # Start by finding its ModelState
    if ( RunModel && LoadExistingDatastore  ) {
      # LoadExistingDatastore means we're picking up and continuing the last model run
      # If it doesn't exist and we're not running, that's fine 
      if ( nzchar(SaveParameters$previousModelStateName) ) {
        # we moved aside the old model state; now we'll resurrect it
        # into the current model state, OVERWRITING everything we may
        # have initialized earlier.
        modelStateLoaded <- load(SaveParameters$previousModelStateName, envir=LoadEnv)
        if ( modelStateLoaded ) {
          setModelState(LoadEnv$ModelState_ls,Save=RunModel)
        } # else use the current (in memory) ModelState
      } # else use the current (in memory) ModelState
    }
    if ( ! LoadExistingDatastore ) {
      # Seek ModelState from the directory containing the other Datastore
      # In the file system, use the currently configured ModelStateFileName
      #  if it's not right, that other model needs to be re-run in
      #  the current environment.
      modelStatePath <- file.path(LoadDstoreDir,ModelStateFileName)
      modelStateLoaded <- loadModelState(modelStatePath,envir=LoadEnv)
      if ( ! modelStateLoaded && ! RunModel ) {
        # TODO: make sure this unwinds correctly through BaseModel and LoadDatastore
        # If those earlier models do not exist, BaseModel lets us recurse to build earlier
        #  virtual model states (but we should warn that BaseModel has not been run).
        # LoadDatastore requires that the earlier model state exist.
        # TODO: we're getting rid of the current notion of stages in favor of the more
        #  flexible "run steps"
        if ( "ModelStateStages" %in% ls(ve.model) ) {
          LoadEnv$ModelState_ls <- ve.model$ModelStateStages[[toupper(basename(LoadDstoreDir))]]
          if ( ! is.null(LoadEnv$ModelState_ls) ) {
            modelStateLoaded <- TRUE
          }
        }
      }
      if ( ! modelStateLoaded ) {
        stop(
          writeLog(
            c(
              "Error in LoadDatastore:",
              "The prior stage ModelState is missing; run it first.",
              paste("Prior stage:",LoadDstoreName),
              paste("Unable to load ModelState:",modelStatePath)
            ),
            Level="error"),
          call.=FALSE
        )
      }

      # Copy over the required packages part of the previous model state
      # That's all we need so we can parse the current module calls successfully
      if ( "RequiredVEPackages" %in% names(LoadEnv$ModelState_ls) ) {
        AlreadyInitialized <- LoadEnv$ModelState_ls$RequiredVEPackages
        RequiredPackages <- unique(c(RequiredPackages, LoadEnv$ModelState_ls$RequiredVEPackages))
      }
    }
  }

  #======================================================
  # EXTRACT INPUT/OUTPUT SPECIFICATIONS FROM MODULE CALLS
  #======================================================

  writeLog("Parsing Module Calls...",Level="warn")
  AllSpecs_ls <- parseModuleCalls(
    parsedScript$ModuleCalls_df,
    RequiredPackages,
    AlreadyInitialized,
    Save=RunModel
  )
  writeLog("Done parsing Module Calls.",Level="warn")

  #============================
  # RUN THE MODEL, IF REQUESTED
  #============================

  if ( RunModel ) {

    writeLog("Preparing Model Run...",Level="warn")

    #==================
    #SIMULATE MODEL RUN
    #==================

    # TODO: See if this works without RunModel
    # May be some hangups for LoadDatastore workflow, since it might need to know
    #  what is supposed to exist in the loaded datastore. Since we passed the
    #  required packages into parseModuleCalls, AllSpecs_ls probably may have the
    #  specs for the required packages as well.
    if (SimulateRun) {
      simDataTransactions(AllSpecs_ls)
    }

    #==============================================
    # Establish the datastore interaction functions
    #==============================================
    # TODO: This should happen at the highest level (e.g. VEModel)
    # Only needed here for "old style" model setups
    assignDatastoreFunctions(ve.model$ModelState_ls$DatastoreType)

    #=====================================
    #CHECK CONFLICTS WITH LOADED DATASTORE
    #=====================================
    # Sort out LoadDatastore/SaveDatastore parameters, based on what exists
    # Do this step in all run modes, but skip some of the checking if not RunModel
    # The "local" block has side-effects on variables defined outside it;
    # "local" is used to gather the errors into one place via "return" statements

    # Prepare to accumulate errors and warnings
    DstoreConflicts <- character(0)
    InfoMsg <- character(0)

    # Check for consistency of Load and Save parameters
    if ( file.exists(RunDstoreName) ) {
      if ( ! ( LoadDatastore || SaveDatastore ) ) {
        DstoreConflicts <- c(DstoreConflicts,
          paste(
            "The existing datastore, ",RunDstoreName," will NOT be overwritten.\n",
            "Set SaveDatastore=TRUE to move the existing datastore aside.\n",
            "To add to it, set DatastoreName=NULL and set LoadDatastore=TRUE.\n",
            "Or just delete it and try again.\n"
          )
        )
      } else if (
        LoadDatastore &&
        ! SaveDatastore &&
        ! LoadExistingDatastore ) {
        DstoreConflicts <- c(DstoreConflicts,
          paste(
            "Loading DatastoreName='",LoadDstoreName,"'.",
            "Existing datastore ",RunDstoreName," would be overwritten.",
            "Set SaveDatastore=TRUE to move the existing datastore aside.\n",
            "Or delete it and try again.\n"
          )
        )
      }
    } else if ( SaveDatastore ) {
      InfoMsg <- c(
        InfoMsg,
        "Ignored run parameter SaveDatastore=TRUE, as there is no Datastore to save."
      )
      SaveDatastore <- FALSE
    }

    # Verify that there is a Datastore to load
    if ( LoadDatastore && !file.exists(LoadDstoreName) ) {
      DstoreConflicts <- c(DstoreConflicts,
        paste0(
          "Failed to load existing Datastore ",LoadDstoreName,". ",
          "Perhaps the full path name was not specified."
        )
      )
    } else if ( LoadDatastore && ! LoadExistingDatastore ) { # loading and file exists
      # Presume that if we're loading the existing one, it's fine!
      # Check whether the datastore to be loaded is consistent with
      #  the current datastore (using its ModelState)

      #Get datastore type from current ModelState
      LoadDstoreType <- (LoadEnv$ModelState_ls$DatastoreType)
      RunDstoreType <- (ve.model$ModelState_ls$DatastoreType)

      #Check that run and load datastores are of same type
      if ( RunDstoreType != LoadDstoreType) {
        DstoreConflicts <- c(DstoreConflicts, paste(
          "Incompatible datastore types.\n",
          "This Model Run has type: ", RunDstoreType, ".\n",
          "Load Datastore has type: ", LoadDstoreType, "."
        ))
      }

      #Check that geography, units, deflators and base year are consistent
      BadGeography <- !isTRUE(all.equal(ve.model$ModelState_ls$Geo_df, LoadEnv$ModelState_ls$Geo_df))
      BadUnits <- !isTRUE(all.equal(ve.model$ModelState_ls$Units, LoadEnv$ModelState_ls$Units))
      BadDeflators <- !isTRUE(all.equal(ve.model$ModelState_ls$Deflators, LoadEnv$ModelState_ls$Deflators))
      BadBaseYear <- ! (ve.model$ModelState_ls$BaseYear == LoadEnv$ModelState_ls$BaseYear)

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
            "Inconsistent ",elements," between Model Run and Load Datastore."
          )
        )
      }
    }

    #=============================================
    # REPORT RESULTS OF DATABASE CONSISTENCY CHECK
    #=============================================

    if (length(InfoMsg) != 0) {
      writeLog(InfoMsg,Level="warn")
    }

    # Abort if we have irresolvable (not just warning) conflicts
    if (length(DstoreConflicts) != 0) {
      # Restore previous model state if we had archived it earlier
      # TODO: don't actually move the old model state until we move the Datastore below
      # TODO: wrap the moving/archiving into a single function that will handle old style and new
      #   style models.
      if ( nzchar(SaveParameters$previousModelStateName) && SaveParameters$savedPreviousModelState ) {
        unlink(currentModelStatePath)
        if (file.exists(SaveParameters$previousModelStateName)) { # TODO: relative to RunDirectory
          file.rename(SaveParameters$previousModelStateName, basename(currentModelStatePath))
        }
      }
      writeLog(DstoreConflicts,Level="error")
      stop("Datastore configuration error: See log messages.",call.=FALSE)
    }

    #=====================================================
    # SAVE PREVIOUS MODEL STATE AND DATASTORE IF REQUESTED
    #=====================================================

    if (SaveDatastore && file.exists(RunDstoreName)) {
      if ( ! archiveDatastore(
        # unlike archiveModelState, this copies the Datastore plus the ModelState
        RunDstoreName,
        SaveParameters
      ) ) {
        stop(
          writeLog("Failed to save old Datastore!",Level="error"),
          call. = FALSE
        )
      }
    } # If not saving, we'll just blow away the old Datastore

    #==================================
    # LOAD OTHER DATASTORE IF SPECIFIED
    #==================================
    # There are two use cases for LoadDatastore
    #   (1) Loading (continuing) the existing Datastore (LoadDatastore==TRUE
    #       and LoadDstoreName is the same as RunDstoreName
    #   (2) Loading a Datastore from a previous stage (or another run)
    #       Need to clear existing Datastore, if there is one, and install
    #       the Datastore contents from the LoadDatastoreName
    # Additional use case is a BaseModel, but in that case we just pre-load
    #  the model state datastore listing, with DstoreLoc links to BaseModel
    #  final (or specified) run step

    writeLog("Preparing Data Store...",Level="warn")
    if (LoadDatastore) {
      # If not the same as any existing datastore, delete existing datastore
      if ( ! LoadExistingDatastore ) {
        # Remove existing Datastore if we're not re-using it
        if ( file.exists(RunDstoreName) ) {
          unlink(RunDstoreName,recursive=TRUE)
        }
        # Copy the loaded file/directory Datastore hierarchy
        loadDatastoreCopy <- tempfile(tmpdir=getwd(),pattern="Datastore_")
        dir.create(loadDatastoreCopy)
        file.copy(LoadDstoreName, loadDatastoreCopy, recursive = TRUE)
        file.rename(file.path(loadDatastoreCopy,basename(LoadDstoreName)),RunDstoreName)
        unlink(loadDatastoreCopy,recursive=TRUE)
        writeLog(paste("Copied previous datastore from:",LoadDstoreName),Level="info")
        writeLog(paste("Copied datastore            to:",RunDstoreName),Level="info")

        # Copy datastore inventory for loaded datastore into current ModelState
        # TODO: this same step happens for BaseModel, but must rectify DstoreLoc for
        # the loaded Datastore listing. No rectification needed for LoadDatastore (copying)
        # The Datastore listing needs to be ready before we update the Year groups
        setModelState(list(Datastore=LoadEnv$ModelState_ls$Datastore),Save=RunModel)

        #=================================================
        # HANDLE USE CASE WHERE SOME YEARS ALREADY DEFINED
        #=================================================

        # TODO: Key feature is to make sure all the geography is present for the new
        #  year. This will probably work for both BaseModel and LoadDatastore variations.

        # Each RunStep needs to have groups available locally (i.e. materialized in its own
        # Datastore) for the years it is evaluating (even if those years might also be
        # present in earlier RunSteps).

        # Initialize geography for years not present in datastore
        # Handles model stages where the new stage adds one or more Years
        #  instead of (or in addition to) adding module calls.
        RunYears_ <- ve.model$ModelState_ls$Years
        LoadYears_ <- LoadEnv$ModelState_ls$Years
        if (!all(RunYears_ == LoadYears_)) {
          NewYears_ <- RunYears_[!(RunYears_ %in% LoadYears_)]
          # NOTE: initDatastore and initDatastoreGeography are performed separately
          # as initDatastore depends on DatastoreType (RD vs H5), whereas
          # initDatastoreGeography will use the assigned functions 
          initDatastore(AppendGroups = NewYears_)
          initDatastoreGeography(GroupNames = NewYears_)
          loadModelParameters(FlagChanges=TRUE)
          # FlagChanges: A bunch of warnings are printed if the model parameters changed
          # TODO: Probably we should just abandon ship in those cases
        }
      }
      # if we ARE loading the existing Datastore, it's just there and we'll continue
      # all the book-keeping was done when we loaded its ModelState
    } else {
      # not loading, so make a new Datastore
      initDatastore()
      initDatastoreGeography()
      loadModelParameters()
    }

    #===============================
    #CHECK AND PROCESS MODULE INPUTS
    #===============================

    # TODO: AllSpecs_ls should only include modules from the current Run Step
    writeLog("Adding input files...",Level="warn")
    processInputFiles(AllSpecs_ls) # Add them to the Datastore

    #============================
    # PRINT MODEL RUN STEP HEADER
    #============================

    # writeLogMessage adds "bare messages" to the "ve.logger"
    # intended as metadata for the model run
    writeLogMessage(
      paste(
        paste("Name:",ve.model$ModelState_ls$Model),
        paste("Scenario:",ve.model$ModelState_ls$Scenario),
        paste("Description:",ve.model$ModelState_ls$Description),
        paste("Timestamp:",ve.model$ModelState_ls$FirstCreated),
        sep="\n"
      )
    )
  }

  #=========================
  # DONE WITH INITIALIZATION
  #=========================

  if ( RunModel ) writeLog("Model successfully initialized.")
  invisible(ve.model$ModelState_ls)
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
  writeLog(Msg)
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
  writeLog(Msg)

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
