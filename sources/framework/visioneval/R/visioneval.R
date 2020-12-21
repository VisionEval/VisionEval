#============
#visioneval.R
#============

#This script defines the functions that are used to write VisionEval models.

utils::globalVariables(c("initDatastore","Year"))

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
#' @param Param_ls A list of run parameters from the environment. These will
#' be overridden by anything in ... (first) or in ParamDir/RunParamFile
#' (highest priority).
#' @param LoadDatastore A logical identifying whether an existing datastore
#' should be loaded.
#' @param DatastoreName A string identifying the full path name of a datastore
#' to load or NULL if an existing datastore in the working directory is to be
#' loaded.
#' @param SimulateRun A logical identifying whether the model run should be
#' simulated: i.e. each step is checked to confirm that data of proper type
#' will be present when the module is called. JRaw says: Just do it
#' @param LogLevel A character string indicating the threshold for log messages
#' @param ... Additional named arguments that can override run parameters from
#' VisonEval.cnf or from run_parameters.json. See Run Parameters section.
#' @return The ModelState_ls that was constructed, invisibly.
#' @export
initializeModel <-
function(
  Param_ls = NULL,
  LoadDatastore = FALSE,
  DatastoreName = NULL, # WARNING: different from "DatastoreName" loaded as a run parameter
  SimulateRun = FALSE,
  LogLevel = NULL,
  ...
) {
  # TODO: Refactor so we do the following tasks:
  #       1. Load or Create the ModelState (in memory) for this model (if not running)
  #          a. Initialize from Run Parameters
  #          b. Parse the model script
  #          c. Build the model specifications
  #          d. (Optionally) Simulate the Model Run - important for identifying interior stages
  #       2. Initialize for a Model Run
  #          a. Save the previous ModelState and Datastore if requested (starting a run)
  #          b. Save the new ModelState
  #          c. Set up the new Datastore
  #       5. Run the initial model steps
  #          a. Run the Initialize scripts (if any)
  #          b. Load the model Inputs to the Datastore
  #       
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

  # Access the model environment and check for RunModel condition
  ve.model <- modelEnvironment()
  RunModel <- modelRunning()

  # Check for pre-existing ve.model environment (e.g. from VEModel$run) and RunParam_ls found there
  if ( is.null(Param_ls) ) {
    Param_ls <- addParameterSource(
      get0( "RunParam_ls", envir=ve.model, ifnotfound=list() ),
      "RunParam_ls in modelEnvironment()"
   ) 
  } else {
    Param_ls <- addParameterSource(Param_ls, "initializeModel(Param_ls=)")
  }

  # Process configuration files in relevant places
  ModelDir <- getRunParameter("ModelDir",Default=getwd(),Param_ls=Param_ls)

  # Dots will override anything passed from the environment or loaded up from the current directory
  DotParam_ls <- addParameterSource(list(...),"initializeModel(...)")
  RunParam_ls <- loadConfiguration(ParamDir=getwd(),keep=DotParam_ls,override=Param_ls)

  ParamDir <- getRunParameter("ParamDir",Default="defs",Param_ls=RunParam_ls)
  RunParamFile <- getRunParameter("RunParamFile",Default="run_parameters.json",Param_ls=RunParam_ls)
  InputPath <- getRunParameter("InputPath",Default=".",Param_ls=Param_ls)

  ParamPath <- normalizePath(file.path(InputPath,ParamDir),winslash="/",mustWork=FALSE) # might be more than one
  RunParam_ls <- loadConfiguration(ParamDir=ParamPath,ParamFile=RunParamFile,override=RunParam_ls)

  ModelScriptFile <- getRunParameter( # in old-style model, dots will have overridden
    "ModelScriptFile",
    Default=normalizePath(file.path(ModelDir,"run_model.R"),winslash="/"),
    Param_ls=RunParam_ls
  )

  ModelStateFileName <- getRunParameter("ModelStateFileName",Default="ModelState.Rda",Param_ls=RunParam_ls)

  # Configure default for SaveDatastore run parameter
  SaveDatastore <- if ( "SaveDatastore" %in% names(RunParam_ls) ) {
    RunParam_ls$SaveDatastore
  } else TRUE # Save by default

  # Set up model state transition parameters
  # TODO: Only need these if we're running a model (so do it later when we push aside the old Datastore)
  previousModelState <- NULL
  previousModelStateName <- ""
  savedPreviousModelState <- FALSE
  Timestamp <- gsub(":","-",gsub(" ", "_",Sys.time())) # Suitable for use in file name
  previousTimestamp <- (Timestamp) # changed below if a previous ModelState already exists

  # If "Run" step is explicitly invoked, start the current model run log file
  # If not running, all messages are just logged to "ve.logger"
  # (defaulting to the ROOT logger, which appends to the console by
  # default)
  if ( is.null(LogLevel) ) {
    LogLevel <- getRunParameter("LogLevel",Default="error",Param_ls=RunParam_ls)
  }
  logState <- initLog(Timestamp,Threshold=LogLevel,Save=RunModel) # start/reset the model run logger

  # Get status of current model state
  # Expect that getwd() contains (or will contain) the ModelState.Rda we're working on
  currentModelStatePath <- normalizePath(ModelStateFileName,winslash="/",mustWork=FALSE)
  currentModelStateExists <- file.exists(currentModelStatePath)

  # Install ModelState_ls in ve.model environment
  if (
    RunModel || # always make a new one if running the model (and save the last one)
    ! currentModelStateExists ) # if doing Load, initialize one (in memory)
  {
    writeLog("Initializing Model. This may take a while",Level="warn")

    # If running and the ModelState already exists and we want to SaveDatastore, set it aside
    if ( currentModelStateExists && RunModel && SaveDatastore ) {
      # Read the model state to see if it has a "LastChanged" flag
      previousModelState <- readModelState(FileName=currentModelStatePath,envir=new.env())

      # Get timestamp from previous ModelState
      # Use the current time if the ModelState was "blank"
      if ( "LastChanged" %in% previousModelState ) {
        previousTimestamp <- gsub(":","-",gsub(" ", "_",previousModelState$LastChanged))
      }

      # Move the previous model state out of the way
      previousModelStateName <- paste0("ModelState_",previousTimestamp,".Rda")
      savedPreviousModelState <- file.rename(currentModelStatePath, previousModelStateName)
    }

    initModelState(Save=RunModel,Param_ls=RunParam_ls)
    readGeography(Save=RunModel,Param_ls=RunParam_ls) # will already have path/geo file names into ModelState Run Parameters
  } else { # ModelState file exists and doing a pure "Load"
    # open the existing model state
    loadModelState(currentModelStatePath)
  }
  # log got initialized earlier but there may not have been a ModelState
  # Now save log parameters in the ModelState if the model is running
  if ( length(logState)>0 ) {
    setModelState(logState,Save=RunModel)
  }

  # Locate any earlier Datastore and ModelState, if needed

  # Normalized path name of the datastore from the ModelState, relative to getwd()
  # Note that the DatastoreName in RunParam_ls is decidedly NOT the same as the DatastoreName
  #   in the parameter list to initializeModel
  #   (the parameter is the path/filename for the *other* Datastore from a previous stage)
  RunDstoreName <- normalizePath(RunParam_ls$DatastoreName, winslash = "/", mustWork = FALSE)
  RunDstoreDir <- dirname(RunDstoreName)

  #Normalized path name of the datastore to be loaded, or empty string
  if (is.null(DatastoreName) ) { # DatastoreName here is NOT the RunDstoreName
    if ( LoadDatastore ) {
      # null name means start with the current (existing) Datastore
      LoadDstoreName <- RunDstoreName
      LoadDstoreDir <- RunDstoreDir
    } else { # Nothing to load
      LoadDatastore <- FALSE
      LoadDstoreName <- NULL
      LoadStoreDir <- NULL
    }
  } else {
    LoadDstoreName <- normalizePath(DatastoreName, winslash = "/", mustWork = FALSE)
    LoadDstoreDir <- dirname(LoadDstoreName)
    LoadDatastore <- TRUE # must load if there's a name provided
  }

  # Flag if we're trying to (re-)load the existing Datastore
  LoadExistingDatastore <- LoadDatastore && (RunDstoreName == LoadDstoreName) && (RunDstoreDir == LoadDstoreDir)

  #===========================================================================
  #PARSE SCRIPT TO MAKE TABLE OF ALL THE MODULE CALLS, CHECK AND COMBINE SPECS
  #===========================================================================

  #Parse script and make data frame of modules that are called
  #directly
  parsedScript <- getRunParameter(
    "ParsedModelScript",
    Default=parseModelScript(ModelScriptFile),
    Param_ls=RunParam_ls
  )

  # Create required package list, adding from previous ModelState if available
  AlreadyInitialized <- character(0) # required (initialized packages) from loaded datastore
  RequiredPackages <- parsedScript$RequiredVEPackages;

  # Prepare to accumulate errors and warnings
  DstoreConflicts <- character(0)
  InfoMsg <- character(0)

  # Load the previous model state, if available
  # TODO: if ! RunModel look for the model state in the file system,
  # and if not found, then look for LoadedModelState in ve.model, a
  # named list of ModelStates created in memory from earlier stages
  # by VEModel.
  LoadEnv <- new.env() # for loaded ModelState
  if ( LoadDatastore ) {
    # If running and LoadExisting, find it in the file system by
    # loading the "previous" that we had moved out of the way above
    if ( RunModel && LoadExistingDatastore  ) {
      # LoadExistingDatastore means load the one for this model
      # If it doesn't exist and we're not running, that's fine 
      if ( nzchar(previousModelStateName) ) {
        # we moved aside the old model state; now we'll resurrect it
        # into the current model state
        load(previousModelStateName, envir=LoadEnv)
        setModelState(LoadEnv$ModelState_ls,Save=RunModel)
      } # else the current model state already has the necessary contents
        #  probably because it is being built from scratch
    }
    if ( ! LoadExistingDatastore ) {
      # Seek ModelState from the directory containing the other Datastore
      # In the file system, use the currently configured ModelStateFileName
      #  if it's not right, that other model needs to be re-run in
      #  the current environment.
      modelStatePath <- file.path(LoadDstoreDir,ModelStateFileName)
      modelStateLoaded <- loadModelState(modelStatePath,envir=LoadEnv)
      if ( ! modelStateLoaded && ! RunModel ) {
        # If the ModelState is not in the filesystem and we're not
        # running, look for pseudo-ModelStates built by VEModel$initialize
        if ( "ModelStateStages" %in% ls(ve.model) ) {
          LoadEnv$ModelState_ls <- ve.model$ModelStateStages[LoadDstoreDir]
          if ( ! is.null(LoadEnv$ModelState_ls) ) {
            modelStateLoaded <- TRUE
          }
        }
      }
      if ( ! modelStateLoaded ) {
        stop(
          writeLog(
            c(
              "Error in LoadDatastore.",
              "The prior stage is missing; run it first.",
              paste("Prior stage:",LoadDstoreName),
              paste("Unable to load ModelState:",modelStatePath)
            ),
            Level="error"),
          call.=FALSE
        )
      }
    }

    # Copy over the required packages part of the previous model state
    # That's all we need so we can parse the current module calls successfully
    if ( "RequiredVEPackages" %in% names(LoadEnv$ModelState_ls) ) {
      AlreadyInitialized <- LoadEnv$ModelState_ls$RequiredVEPackages
      RequiredPackages <- unique(c(RequiredPackages, LoadEnv$ModelState_ls$RequiredVEPackages))
    }
  }

  setModelState(
    list(ModuleCalls_df=parsedScript$ModuleCalls_df), # Full list of calls
    Save=RunModel
  )

  AllSpecs_ls <- parseModuleCalls(
    parsedScript$ModuleCalls_df,
    RequiredPackages,
    AlreadyInitialized,
    Save=RunModel
  )

  if ( RunModel ) {

    #==================
    #SIMULATE MODEL RUN
    #==================
    # TODO: See if this works without RunModel
    # May be some hangups for LoadDatastore workflow
    if (SimulateRun) {
      simDataTransactions(AllSpecs_ls)
    }

    #----------------------------
    #Set up objects and functions
    #----------------------------
    #Normalized path name of the datastore used in the model run
    RunDstoreName <- normalizePath(ve.model$ModelState_ls$DatastoreName, winslash = "/", mustWork = FALSE)
    RunDstoreDir <- dirname(RunDstoreName)
    RunDstoreFile <- basename(RunDstoreName)

    # Assign the correct datastore interaction functions
    assignDatastoreFunctions(RunParam_ls$DatastoreType)

    #=======================================
    #CHECK CONFLICTS WITH EXISTING DATASTORE
    #=======================================
    # Sort out LoadDatastore/SaveDatastore parameters, based on what exists
    # Do this step in all run modes, but skip some of the checking if not RunModel
    # The "local" block has side-effects on variables defined outside it;
    # "local" is used to gather the errors into one place via "return" statements

    #Error if LoadDatastore and the DatastoreName does not exist
    if ( LoadDatastore && !file.exists(LoadDstoreName) ) {
      DstoreConflicts <- c(DstoreConflicts,
        paste0(
          "Failed to load Datastore ",LoadDstoreName,". ",
          "Perhaps the full path name was not specified."
        )
      )
    }

    if ( LoadDatastore ) {
      # Get values from the other Datastore
      LoadDstoreType <- (LoadEnv$ModelState_ls$DatastoreType)
      #Get datastore type from current ModelState
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
      BadGeography <-
      !all.equal(ve.model$ModelState_ls$Geo_df, LoadEnv$ModelState_ls$Geo_df)
      BadUnits <-
      !all.equal(ve.model$ModelState_ls$Units, LoadEnv$ModelState_ls$Units)
      BadDeflators <-
      !all.equal(ve.model$ModelState_ls$Deflators, LoadEnv$ModelState_ls$Deflators)
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
            "Inconsistent ",elements," between Model Run and LoadDatastore."
          )
        )
      }
    }

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
  
    #---------------------------------------------------------
    # Check loaded ModelState for compatibility
    # Won't do this check if the model is not running
    #---------------------------------------------------------

    #Write information message to log if any
    if (length(InfoMsg) != 0) {
      writeLog(InfoMsg,Level="warn")
    }

    if (length(DstoreConflicts) != 0) {
      # Restore previous model state if we had moved it out of the way
      if ( nzchar(previousModelStateName) && savedPreviousModelState ) {
        unlink(currentModelStatePath)
        if (file.exists(previousModelStateName)) { # TODO: relative to RunDirectory
          file.rename(previousModelStateName, basename(currentModelStatePath))
        }
      }
      writeLog(DstoreConflicts,Level="error")
      stop("Datastore configuration error: See log messages.",call.=FALSE)
    }

    #==============================================================
    # PREPARE THE DATASTORE (SAVE PREVIOUS THEN LOAD OR INITIALIZE)
    #==============================================================

    #--------------------------------------------------------
    #Save previous datastore and model state if SaveDatastore
    #--------------------------------------------------------
    if (SaveDatastore && file.exists(RunDstoreName)) {
      #A previous model state should be associated with this Datastore
      #Create a directory in which to save the datastore
      ArchiveDstoreName <- paste(RunDstoreName, previousTimestamp, sep = "_")
      dir.create(ArchiveDstoreName)
      #Copy the datastore into the directory
      file.copy(RunDstoreName, ArchiveDstoreName, recursive = TRUE)
      #Copy the previous model state file into the directory as well
      file.copy(previousModelStateName,
        file.path(ArchiveDstoreName, getModelStateFileName()))
    }

    #----------------------------------------
    #Load datastore if specified (in "Run" step)
    #----------------------------------------
    # TODO: Rationalize this for the two use cases:
    #   (1) Loading (continuing) the existing Datastore
    #   (2) Loading a Datastore from a previous stage (or another run)
    #       Need to clear existing Datastore, if there is one
    if (LoadDatastore) {
      # If not the same as any existing datastore, delete existing datastore
      if ( ! LoadExistingDatastore ) {
        # Remove existing Datastore
        if ( file.exists(RunDstoreName) ) {
          unlink(RunDstoreName,recursive=TRUE)
        }
        # Copy the loaded file/directory Datastore hierarchy
        # TODO: use virtual link to previous Datastore, rather than copying mega- or gigabytes
        loadDatastoreCopy <- tempfile(tmpdir=getwd(),pattern="Datastore_")
        dir.create(loadDatastoreCopy)
        file.copy(LoadDstoreName, loadDatastoreCopy, recursive = TRUE)
        file.rename(file.path(loadDatastoreCopy,basename(LoadDstoreName)),RunDstoreName)
        unlink(loadDatastoreCopy,recursive=TRUE)
      }

      # Copy datastore inventory for loaded datastore
      # TODO virtual link to previous Datastore
      ve.model$ModelState_ls$Datastore <- LoadEnv$ModelState_ls$Datastore
      setModelState(ve.model$ModelState_ls,Save=RunModel)

      # Initialize geography for years not present in datastore
      # Handles model stages where the new stage adds one or more Years
      RunYears_ <- ve.model$ModelState_ls$Years
      LoadYears_ <- LoadEnv$ModelState_ls$Years
      if (!all(RunYears_ == LoadYears_)) {
        NewYears_ <- RunYears_[!(RunYears_ %in% LoadYears_)]
        initDatastore(AppendGroups = NewYears_)
        initDatastoreGeography(GroupNames = NewYears_)
      }
    } else {
      # not loading, so make a new Datastore
      initDatastore()
      initDatastoreGeography()
      loadModelParameters()
    }

    #===============================
    #CHECK AND PROCESS MODULE INPUTS
    #===============================

    processInputFiles(AllSpecs_ls)

    writeLogMessage(
      paste(
        paste("Name:",ve.model$ModelState_ls$Model),
        paste("Scenario:",ve.model$ModelState_ls$Scenario),
        paste("Description:",ve.model$ModelState_ls$Description),
        sep="\n"
      )
    )
  }

  #Report done with initialization
  #-------------------------------
  writeLog("Model successfully initialized.", Level = "warn")
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
  Msg <-
    paste0("Start  module '", ModuleFunction,
           "' for year '", RunYear, "'.")
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
    L <- getFromDatastore(M$Specs, RunYear = Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
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
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
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
            getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
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
  Msg <-
    paste0("Finish module '", ModuleFunction,
           "' for year '", RunYear, "'.")
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
    L <- getFromDatastore(M$Specs, RunYear = Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
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
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
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
            getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
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
