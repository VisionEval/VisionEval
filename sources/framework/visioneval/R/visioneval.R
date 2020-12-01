#============
#visioneval.R
#============

#This script defines the main functions that implement the VisionEval framework
#and are intended to be exported.

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
#' @param ModelScriptFile A string identifying the path to the file that contains
#'   the steps in the model to run (i.e. the calls to \code{runModule})
#' @param RunDirectory A character string containing a path (relative to \code{getwd()}
#'   or absolute) specifying where to look for ModelState.Rda and the Datastore.
#' @param LoadDatastore A logical identifying whether an existing datastore
#' should be loaded.
#' @param DatastoreName A string identifying the full path name of a datastore
#' to load or NULL if an existing datastore in the working directory is to be
#' loaded.
#' @param SimulateRun A logical identifying whether the model run should be
#' simulated: i.e. each step is checked to confirm that data of proper type
#' will be present when the module is called. JRaw says: Just do it
#' @param RunMode An optional character string specifying the steps to perform
#' during initializeModel. "Load" loads an existing ModelState (or creates a
#' bare one in memory), "Run" performes the traditional operations.
#' See \code{getRunMode} for some other ways to set this parameter.
#' @param ... Additional named arguments that can override run parameters from
#' VisonEval.cnf or from run_parameters.json. See Run Parameters section.
#' @return None. The function prints to the log file messages which identify
#' whether or not there are errors in initialization. It also prints a success
#' message if initialization has been successful.
#' @export
initializeModel <-
function(
  ModelScriptFile = "run_model.R",
  RunDirectory = getwd(),
  LoadDatastore = FALSE,
  DatastoreName = NULL,
  SimulateRun = FALSE,
  RunMode = NULL,
  ...
) {
  # TODO:Walk through the logic (step through it in RStudio)
  
  # NOTE: This function expects to have getwd() be the same directory that the
  # ModelState and Datastore will be created in (the model/scenario location)
  # The actual run_model.R script can be elsewhere

  # Determine which initialization steps to run (supports VEModel end user
  # API). Default is to run all three, reproducing original initializeModel
  # functionality. Set ve.model$runSteps to select runSteps (or modify
  # the initializeModel call).
  # runSteps will also affect any runModule/runScript/Stage calls - if they are
  # executed outside a "Run" step, they will do nothing and return.
  # instructions - they won't run unless
  runSteps <- getRunMode(RunMode)

  # The runSteps do the following:
  #
  #   Empty and replace the ve.model environment.
  #
  #   "Load" will load an existing ModelState and check that the model has everything it needs to
  #   run, updating ModelState as needed. "Load" will perform an in-memory "Init" first if no
  #   ModelState file exists so the parsed elements of run_model.R are available for inspection, but
  #   it will not save the ModelState_ls. The ModelState_ls will only be saved when "Run" is
  #   processed (and it will be loaded again first). This function will clear the ve.model environment,
  #   load the ModelState, parse the run_model.R script, check presence of files, check
  #   specifications, etc. ModelState_ls remains in the ve.model environment. "Load" will be
  #   performed if ModelState is not present in ve.model when we try to "Run". "Load" will also be
  #   performed automatically when a VEModel object is created.
  #
  #   "Run" will initialize the Datastore by loading an existing the "Datastore" (if LoadDatastore
  #   is TRUE), or by creating a new bslank Datastore. It will presume that ModelState in ve.model
  #   is current. Once the "Run" initialization step is complete, the model can be run by
  #   executing runModule / runScript / Stage steps.

  #==============================
  #DETERMINE RUN STEPS TO PERFORM
  #==============================

  # Clear the current ve.model environment (and create if missing)
  ve.model <- modelEnvironment()
  rm( list=ls(envir=ve.model, all.names=TRUE), envir=ve.model )

  # Set up model state transition parameters
  previousModelState <- NULL
  previousModelStateName <- ""
  savedPreviousModelState <- FALSE
  Timestamp <- gsub(":","-",gsub(" ", "_",Sys.time())) # Suitable for use in file name
  previousTimestamp <- (Timestamp) # changed below if a previous ModelState already exists

  # If "Run" step is explicitly invoked, start the current model run log file
  # If not running, all messages are just logged to "ve.logger"
  # (defaulting to the ROOT logger, which appends to the console by default)
  if ( "Run" %in% runSteps ) {
    logState <- initLog(Timestamp) # start/reset the model run logger
  } else {
    if ( ! exists("ve.logger") ) ve.model$ve.logger = "ve.logger"
    logState <- list()
  }

  # Get (just) the name of the current model's model state
  # We expect to find it in getwd() - need to manage the directory better
  # NOTE: if RunDirectory is missing, it will still have getwd() as its default value
  RunDirectory <- if( missing(RunDirectory) ) getVEOption("RunDirectory",RunDirectory)
  currentModelStateName <- getModelStateFileName()
  currentModelStateExists <- file.exists(currentModelStateName) # TODO: relative to RunDirectory
  saveModelState <- ("Run" %in% runSteps)
  # "Load" does not save the model state - it just builds it in memory

  # Install ModelState_ls in ve.model environment
  if (
    saveModelState || # always make a new one if doing Init or Run
    ! currentModelStateExists ) # if doing Load, initialize one (in memory)
  {
    writeLog("Initializing Model. This may take a while",Level="warn")

    # If a ModelState.Rda file exists and this one is to be saved, rename it
    if (currentModelStateExists && saveModelState) {
      # Read the model state to see if it has a "LastChanged" flag
      previousModelState <- readModelState(FileName=currentModelStateName,envir=new.env())

      # Get timestamp from previous ModelState
      # Use the current time if the ModelState was "blank"
      if ( "LastChanged" %in% previousModelState ) {
        previousTimestamp <- gsub(":","-",gsub(" ", "_",previousModelState$LastChanged))
      }

      # Move the previous model state out of the way
      previousModelStateName <- paste0("ModelState_",previousTimestamp,".Rda")
      savedPreviousModelState <- file.rename(currentModelStateName, previousModelStateName)
    }

    # TODO: rationalize the initializeModel arguments versus VisionEval.cnf environment
    # versus RunParamFile. Should verify existence of the run_model.R file at this
    # point (specified as an argument). Probably should actively manage run_model.R
    # directory (for all the relative directors for defs, inputs, etc.). Then
    # initializeModel won't even need to be part of run_model.R, except we'll need to
    # ensure that we have initialized when we get to the first runModule call.x

    # Here are some parameters that might come in through "..." (backward compatible)
    # ParamDir = "defs",
    # RunParamFile = "run_parameters.json",
    # GeoFile = "geo.csv",
    # ModelParamFile = "model_parameters.json",
    # SaveDatastore = TRUE

    initModelStateFile(Save=saveModelState,...)
    readGeography(Save=saveModelState) # will already have put path/geo file names into ModelState Run Parameters
  } else { # ModelState file exists and doing a pure "Load"
    # open the existing model state
    loadModelState(currentModelStateName)
  }
  # log got initialized earlier but there may not have been a ModelState
  # Now save log parameters in the ModelState if the model is running
  if ( length(logState)>0 ) {
    setModelState(logState,Save=saveModelState)
  }

  # Configure default for SaveDatastore run parameter
  SaveDatastore <- if ( "SaveDatastore" %in% names(ve.model$ModelState_ls) ) {
    ve.model$ModelState_ls$SaveDatastore
  } else TRUE

  # Assign the correct datastore interaction functions
  assignDatastoreFunctions(ve.model$ModelState_ls$DatastoreType)

  #=======================================
  #CHECK CONFLICTS WITH EXISTING DATASTORE
  #=======================================
  # Sort out LoadDatastore/SaveDatastore parameters, based on what exists
  # Do this step in all run modes
  # The "local" block has side-effects on variables defined outside it;
  # "local" is used to gather the errors into one place via "return" statements

  # Prepare to accumulate errors and warnings
  DstoreConflicts <- character(0)
  InfoMsg <- character(0)

  #Normalized path name of the datastore from the ModelState (TODO: relative to RunDirectory)
  #TODO: DatastoreName is probably relative, and we need to find it relative to RunDirectory
  RunDstoreName <- normalizePath(ve.model$ModelState_ls$DatastoreName, winslash = "/", mustWork = FALSE)
  RunDstoreDir <- dirname(RunDstoreName)

  #Normalized path name of the datastore to be loaded, or empty string
  if (is.null(DatastoreName) ) {
    if ( LoadDatastore ) {
      # null name means start with the current (existing) Datastore
      LoadDstoreName <- RunDstoreName
      LoadDstoreDir <- RunDstoreDir
    } else {
      LoadDatastore <- FALSE
      LoadDstoreName <- NULL
      LoadStoreDir <- NULL
    }
  } else {
    LoadDstoreName <- normalizePath(DatastoreName, winslash = "/", mustWork = FALSE)
    LoadDstoreDir <- dirname(LoadDstoreName)
    LoadDatastore <- TRUE # must load if there's a name provided
  }

  #Error if LoadDatastore and the DatastoreName does not exist
  if ( LoadDatastore && !file.exists(LoadDstoreName) ) {
    DstoreConflicts <- c(DstoreConflicts,
      paste0(
        "Failed to load Datastore ",LoadDstoreName,". ",
        "Perhaps the full path name was not specified."
      )
    )
  }

  # Flag if we're trying to (re-)load the existing Datastore
  loadExistingDatastore <- LoadDatastore && (RunDstoreName == LoadDstoreName) && (RunDstoreDir == LoadDstoreDir)

  #Error datastore exists and is not being loaded or saved (renamed)
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
      ! loadExistingDatastore ) {
      DstoreConflicts <- c(DstoreConflicts,
        paste(
          "Loading DatastoreName='",LoadDstoreName,"'.",
          "Existing datastore ",RunDstoreName," would be overwritten.",
          "Set SaveDatastore=TRUE to move the existing datastore aside.\n",
          "Or just delete it and try again.\n"
        )
      )
    }
  } else if ( SaveDatastore ) {
    InfoMsg <- c(InfoMsg,
      paste(
        "Run parameter SaveDatastore=TRUE, but there is ",
        "no existing Datastore at ",RunDstoreName,".\n",
        "SaveDatastore will be ignored."
      )
    )
    SaveDatastore <- FALSE
  }

  #---------------------------------------------------------
  # Check loaded ModelState for compatibility
  #---------------------------------------------------------
  if ( LoadDatastore ) {
    LoadEnv <- new.env() # for loaded ModelState
    if ( loadExistingDatastore  ) {
      if ( nzchar(previousModelStateName) ) {
        # we moved aside the old model state; now we'll resurrect it
        # into the current model state
        load(previousModelStateName, envir=LoadEnv)
        setModelState(LoadEnv$ModelState_ls,Save=saveModelState)
      } # else the current model state already has the previous contents
    } else {
      # Load model state from the directory containing the other Datastore
      loadModelStateName <- file.path(LoadDstoreDir,basename(currentModelStateName))
      load(loadModelStateName, envir = LoadEnv)

      # Get values from the other Datastore
      LoadDstoreType <- (LoadEnv$ModelState_ls$DatastoreType)
      #Get datastore type from current ModelState
      RunDstoreType <- (ve.model$ModelState_ls$DatastoreType)

      # Grab the required packages from the model state for LoadDstoreName

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
            "Inconsistent ",elements," between Model Run and Load Datastore."
          )
        )
      }
    }
  }

  #Write information message to log if any
  if (length(InfoMsg) != 0) {
    writeLog(InfoMsg,Level="warn")
  }

  if (length(DstoreConflicts) != 0) {
    writeLog(DstoreConflicts,Level="error")
    # Restore previous model state if we had moved it out of the way
    # Will only happen if performing "Init" or "Run"
    if ( nzchar(previousModelStateName) && savedPreviousModelState ) {
      unlink(currentModelStateName)
      if (file.exists(previousModelStateName)) { # TODO: relative to RunDirectory
        file.rename(previousModelStateName, currentModelStateName)
      }
    }
    stop("Datastore configuration error: See log messages.",call.=FALSE)
  }

  if ( any(c("Load","Run") %in% (runSteps)) )  {
    # Perform the guts of the "Load" run step
    # We should already have ve.model$ModelState_ls in place

    #===========================================================================
    #PARSE SCRIPT TO MAKE TABLE OF ALL THE MODULE CALLS, CHECK AND COMBINE SPECS
    #===========================================================================

    #Parse script and make data frame of modules that are called directly
    parsedScript <- parseModelScript(ModelScriptFile)

    # Create required package list, adding from previous ModelState if available
    AlreadyInitialized_ <- character(0) # required (initialized packages) from loaded datastore
    RequiredPkg_ <- parsedScript$RequiredVEPackages
    if ( LoadDatastore ) {
      # We already set up the model state for the loaded datastore above
      # Copy over the required packages part of the previous model state
      if ( "RequiredVEPackages" %in% names(LoadEnv$ModelState_ls) ) {
        AlreadyInitialized_ <- LoadEnv$ModelState_ls$RequiredVEPackages
        RequiredPkg_ <- unique(c(RequiredPkg_, LoadEnv$ModelState_ls$RequiredVEPackages))
      }
    }
    setModelState(
      list(ModuleCalls_df=parsedScript$ModuleCalls_df), # Full list of calls
      Save=saveModelState
    )

    # All the rest of what we need to do with module calls should have them unique
    ModuleCalls_df <- unique(parsedScript$ModuleCalls_df)

    # Report any required packages that are not also in module calls
    umc <- unique(ModuleCalls_df$PackageName)
    explicitRequired_ <- unique(RequiredPkg_)
    if ( any( not.in.umc <- ! (explicitRequired_ %in% umc) ) ) {
      for ( p in explicitRequired_[not.in.umc] ) {
        writeLog(paste("Package",p,"is required"),Level="warn")
      }
    }
    RequiredPkg_ <- c(umc,explicitRequired_)

    #Get list of installed packages
    #Check that all module packages are in list of installed packages
    InstalledPkgs_ <- rownames(installed.packages())
    MissingPkg_ <- RequiredPkg_[!(RequiredPkg_ %in% InstalledPkgs_)]
    if (length(MissingPkg_ != 0)) {
      Msg <-
      paste0("One or more required packages need to be installed in order ",
        "to run the model. Following are the missing package(s): ",
        paste(MissingPkg_, collapse = ", "), ".")
      writeLog(Msg,Level="error")
      stop(Msg)
    }
    #Check for 'Initialize' module in each package if so add to ModuleCalls_df
    Add_ls <- list()
    # Do not add Initialize if the package was required in an earlier model stage
    # Results of Initialization in that case will already be in the Datastore
    for (Pkg in unique(setdiff(ModuleCalls_df$PackageName,AlreadyInitialized_))) {
      PkgData <- data(package = Pkg)$results[,"Item"]
      if ("InitializeSpecifications" %in% PkgData) {
        Add_df <-
        data.frame(
          ModuleName = "Initialize",
          PackageName = Pkg,
          RunFor = "AllYears",
          Year = "Year"
        )
        Add_ls[[Pkg]] <- Add_df
      }
    }
    #Insert Initialize module entries into ModuleCalls_df
    Pkg_ <- names(Add_ls)
    for (Pkg in Pkg_) {
      Idx <- head(grep(Pkg, ModuleCalls_df$PackageName), 1)
      End <- nrow(ModuleCalls_df)
      ModuleCalls_df <- rbind(
        ModuleCalls_df[1:(Idx - 1),],
        Add_ls[[Pkg]],
        ModuleCalls_df[Idx:End,]
      )
    }

    #Identify all modules and datasets in required packages
    Datasets_df <-
    data.frame(
      do.call(
        rbind,
        lapply(RequiredPkg_, function(x) {
          data(package = x)$results[,c("Package", "Item")]
        })
      ), stringsAsFactors = FALSE
    )
    WhichAreModules_ <- grep("Specifications", Datasets_df$Item)
    ModulesByPackage_df <- Datasets_df[WhichAreModules_,]
    ModulesByPackage_df$Module <-
    gsub("Specifications", "", ModulesByPackage_df$Item)
    ModulesByPackage_df$Item <- NULL
    DatasetsByPackage_df <- Datasets_df[-WhichAreModules_,]
    names(DatasetsByPackage_df) <- c("Package", "Dataset")
    #Save the modules and datasets lists in the model state
    setModelState(
      list (
        ModulesByPackage_df = ModulesByPackage_df,
        DatasetsByPackage_df = DatasetsByPackage_df,
        RequiredVEPackages = RequiredPkg_
      ),
      Save=saveModelState
    )

    #Iterate through each module call and check availability and specifications
    #create combined list of all specifications
    # ModulesByPackage_df lists all modules available in the packages
    # ModuleCalls_df lists only modules that appear in runModule commands
    # AllSpecs_ls is a list of processed specifications
    # We refer to it later to find and load input files, and to simulate a model run
    Errors_ <- character(0)
    AllSpecs_ls <- list()
    for (i in 1:nrow(ModuleCalls_df)) {
      AllSpecs_ls[[i]] <- list()
      ModuleName <- ModuleCalls_df$ModuleName[i]
      AllSpecs_ls[[i]]$ModuleName <- ModuleName
      PackageName <- ModuleCalls_df$PackageName[i]
      AllSpecs_ls[[i]]$PackageName <- PackageName
      AllSpecs_ls[[i]]$RunFor <- ModuleCalls_df$RunFor[i]
      #Check module availability
      Err <- checkModuleExists(ModuleName, PackageName, InstalledPkgs_)
      if (length(Err) > 0) {
        Errors_ <- c(Errors_, Err)
        next()
      }
      #Load and check the module specifications
      Specs_ls <- processModuleSpecs(getModuleSpecs(ModuleName, PackageName))
      Err <- checkModuleSpecs(Specs_ls, ModuleName)
      if (length(Err) > 0) {
        Errors_ <- c(Errors_, Err)
        next()
      } else {
        AllSpecs_ls[[i]]$Specs <- Specs_ls
      }
      #If the 'Call' spec is not null and is a list, check the called module
      if (!is.null(Specs_ls$Call) && is.list(Specs_ls$Call)) {
        #Iterate through module calls
        for (j in 1:length(Specs_ls$Call)) {
          Call_ <- unlist(strsplit(Specs_ls$Call[[j]], "::"))
          #Check module availability
          if (length(Call_) == 2) { # package explicitly specified
            Err <-
            checkModuleExists(
              Call_[2],
              Call_[1],
              InstalledPkgs_,
              c(Module = ModuleName, Package = PackageName))
          } else  {
            if (length(Call_) == 1) { # only module name is provided
              if (! any(Call_ %in% ModulesByPackage_df$Module) ) {
                Err <- c(
                  paste0("Error in runModule call for module ", Call_,"."),
                  "It is not present in any package already identified in the model run script.",
                  "Please add requirePackage(<package-with-module>) to the script."
                )
              } else {
                callPkgs_ <- ModuleCalls_df$PackageName[Call_ %in% ModuleCalls_df$ModuleName]
                if ( length(callPkgs_)==1 ) { # use existing explicit call to module
                  Call_ <- c( callPkgs_, Call_ )
                } else  { # callPkgs_ is probably length 0, but could also have more than 1
                  Pkg <- ModulesByPackage_df$Package[ModulesByPackage_df$Module == Call_]
                  Call_ <- c(unique(Pkg), Call_)
                  if (length(Call_) > 2 ) { # More than one package contains the module
                    callPkgs_ <- Call_[-length(Call_)]
                    callModule_ <- Call_[length(Call_)]
                    testPkgs_ <- callPkgs_[callPkgs_ %in% explicitRequired_]
                    if ( length(testPkgs_) == 0 ) testPkgs_ <- callPkgs_  # No explicit required package
                    if ( length(testPkgs_) > 1 ) { # Could not resolve by explicit required package
                      Msg_ <- paste("Providing module",callModule_,"from package",testPkgs_[1])
                      Warn_ <- c(
                        Msg_,
                        paste("Also present in: ", paste(testPkgs_[2:length(testPkgs_)],collapse=", ")),
                        "Use requirePackage() to force selection."
                      )
                      writeLog(Warn_,Level="warn")
                    } else { # Resolved to exactly one package with the module
                      writeLog(paste("Provided module",callModule_,"from Package",testPkgs_[1]),Level="info")
                    }
                    Call_ <- c(testPkgs_[1],callModule_) # Use the first package found, unless explicit
                  }
                }
              }
            } else {
              Err <- paste("Cannot fathom Call specification:",Specs_ls$Call[[j]])
            }
          }
          if (length(Err) > 0) {
            Errors_ <- c(Errors_, Err)
            next()
          }
          # Load and check the module specifications and add Get specs if
          # there are no specification errors
          for (i in 1:(length(Call_)-1)) { # Code above forces length(Call_) always to be 2 or fail
            CallSpecs_ls <-
            processModuleSpecs(getModuleSpecs(Call_[length(Call_)], Call_[i]))
            Err <- checkModuleSpecs(CallSpecs_ls, Call_[length(Call_)])
            if (length(Err) > 0) {
              Errors_ <- c(Errors_, Err)
              next()
            } else {
              AllSpecs_ls[[i]]$Specs$Get <-
              c(AllSpecs_ls[[i]]$Specs$Get <- Specs_ls$Get)
            }            
          }
        }
      }
    }
    #If any errors, print to log and stop execution
    if (length(Errors_) > 0) {
      Msg <-
      paste0("There are one or more errors in the module calls:\n",
        "package not installed, or module not present in package, ",
        "or errors in module specifications.")
      writeLog(c(Msg,Errors_),Level="error")
      stop(Msg," Check log for details")
    }

    #==================
    #SIMULATE MODEL RUN
    #==================
    if (SimulateRun) {
      simDataTransactions(AllSpecs_ls)
    }
  }

  if ( "Run" %in% runSteps ) {
    # perform the "Run" step

    #==============================================================
    # PREPARE THE DATASTORE (SAVE PREVIOUS THEN LOAD OR INITIALIZE)
    #==============================================================

    #----------------------------
    #Set up objects and functions
    #----------------------------
    #Normalized path name of the datastore used in the model run
    RunDstoreName <- normalizePath(ve.model$ModelState_ls$DatastoreName, winslash = "/", mustWork = FALSE)
    RunDstoreDir <- dirname(RunDstoreName)
    RunDstoreFile <- basename(RunDstoreName)
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
      #Copy the previous model state file into the directory
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
      if ( ! loadExistingDatastore ) {
        # Remove existing Datastore
        if ( file.exists(RunDstoreName) ) {
          unlink(RunDstoreName,recursive=TRUE)
        }
        # Copy the loaded file/directory Datastore hierarchy
        loadDatastoreCopy <- tempfile(tmpdir=getwd(),pattern="Datastore_")
        dir.create(loadDatastoreCopy)
        file.copy(LoadDstoreName, loadDatastoreCopy, recursive = TRUE)
        file.rename(file.path(loadDatastoreCopy,basename(LoadDstoreName)),RunDstoreName)
        unlink(loadDatastoreCopy,recursive=TRUE)
      }

      # Copy datastore inventory for loaded datastore
      ve.model$ModelState_ls$Datastore <- LoadEnv$ModelState_ls$Datastore
      setModelState(ve.model$ModelState_ls,Save=saveModelState)

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
    #Set up a list to store processed inputs for all modules
    ProcessedInputs_ls <- list()
    #Process inputs for all modules and add results to list
    # ORIGINAL VERSION:
      #     for (i in 1:nrow(ModuleCalls_df)) {
      #       Module <- ModuleCalls_df$ModuleName[i]
      #       Package <- ModuleCalls_df$PackageName[i]
      #       EntryName <- paste(Package, Module, sep = "::")
      #       ModuleSpecs_ls <- processModuleSpecs(getModuleSpecs(Module, Package))
      #       #If there are inputs, process them
      #       if (!is.null(ModuleSpecs_ls$Inp)) {
      #         ProcessedInputs_ls[[EntryName]] <- processModuleInputs(ModuleSpecs_ls, Module)
      #         #If module is Initialize process inputs with Initialize function
      #         if (Module == "Initialize") {
      #           if (length(ProcessedInputs_ls[[Module]]$Errors) == 0) {
      #             initFunc <- eval(parse(text = paste(Package, Module, sep = "::")))
      #             InitData_ls <- ProcessedInputs_ls[[EntryName]]
      #             InitializedInputs_ls <- initFunc(InitData_ls)
      #             ProcessedInputs_ls[[EntryName]]$Data <- InitializedInputs_ls$Data
      #             ProcessedInputs_ls[[EntryName]]$Errors <- InitializedInputs_ls$Errors
      #             if (length(InitializedInputs_ls$Warnings > 0)) {
      #               writeLog(InitializedInputs_ls$Warnings,Level="warn")
      #             }
      #           }
      #         }
      #       }
      #     }
    for (Spec in AllSpecs_ls) {
      Module <- Spec$ModuleName
      Package <- Spec$PackageName
      EntryName <- paste(Package, Module, sep = "::")
      ModuleSpecs_ls <- (Spec$Specs)
      #If there are inputs, process them
      if (!is.null(ModuleSpecs_ls$Inp)) {
        ProcessedInputs_ls[[EntryName]] <- processModuleInputs(ModuleSpecs_ls, Module)
        # Process inputs with Initialize function
        if (Module == "Initialize") {
          if (length(ProcessedInputs_ls[[EntryName]]$Errors) == 0) {
            initFunc <- eval(parse(text = paste(Package, Module, sep = "::")))
            InitData_ls <- ProcessedInputs_ls[[EntryName]]
            InitializedInputs_ls <- initFunc(InitData_ls)
            ProcessedInputs_ls[[EntryName]]$Data <- InitializedInputs_ls$Data
            ProcessedInputs_ls[[EntryName]]$Errors <- InitializedInputs_ls$Errors
            if (length(InitializedInputs_ls$Warnings > 0)) {
              writeLog(InitializedInputs_ls$Warnings,Level="warn")
            }
          }
        }
      }
    }
    #Check whether there are any input errors
    InpErrors_ <- unlist(lapply(ProcessedInputs_ls, function (x) {
      x$Errors
    }))
    HasErrors <- length(InpErrors_ != 0)
    if (HasErrors) {
      writeLog(InpErrors_,Level="errors")
      stop("Input files have errors. Check the log for details.")
    }
    rm(InpErrors_)

    #Load model inputs into the datastore
    #------------------------------------
    # Original Process
      #     for (i in 1:nrow(ModuleCalls_df)) {
      #       #Get information on the inputs
      #       Module <- ModuleCalls_df$ModuleName[i]
      #       Package <- ModuleCalls_df$PackageName[i]
      #       EntryName <- paste(Package, Module, sep = "::")
      #       ModuleSpecs_ls <- processModuleSpecs(getModuleSpecs(Module, Package))
      #       #Eliminate writing any new input table to Global group if it already exists
      #       if (!is.null(ModuleSpecs_ls$NewInpTable)) {
      #         NewInpTableSpecs_ls <- ModuleSpecs_ls$NewInpTable
      #         GlobalTableExists_ <- unlist(lapply(NewInpTableSpecs_ls, function(x) {
      #           if (x$GROUP == "Global") {
      #             checkTableExistence(x$TABLE, "Global", ve.model$ModelState_ls$Datastore)
      #           } else {
      #             FALSE
      #           }
      #         }))
      #         if (all(GlobalTableExists_)) {
      #           ModuleSpecs_ls$NewInpTable <- NULL
      #         } else {
      #           ModuleSpecs_ls$NewInpTable <- NewInpTableSpecs_ls[!GlobalTableExists_]
      #         }
      #       }
      #       #Load inputs to datastore
      #       if (!is.null(ModuleSpecs_ls$Inp)) {
      #         inputsToDatastore(ProcessedInputs_ls[[EntryName]], ModuleSpecs_ls, Module)
      #       }
      #     }
    for (Spec in AllSpecs_ls) {
      #Get information on the inputs
      Module <- Spec$ModuleName
      Package <- Spec$PackageName
      EntryName <- paste(Package, Module, sep = "::")
      ModuleSpecs_ls <- (Spec$Specs)
      #Eliminate writing any new input table to Global group if it already exists
      if (!is.null(ModuleSpecs_ls$NewInpTable)) {
        NewInpTableSpecs_ls <- ModuleSpecs_ls$NewInpTable
        GlobalTableExists_ <- unlist(lapply(NewInpTableSpecs_ls, function(x) {
          if (x$GROUP == "Global") {
            checkTableExistence(x$TABLE, "Global", ve.model$ModelState_ls$Datastore)
          } else {
            FALSE
          }
        }))
        if (all(GlobalTableExists_)) {
          ModuleSpecs_ls$NewInpTable <- NULL
        } else {
          ModuleSpecs_ls$NewInpTable <- NewInpTableSpecs_ls[!GlobalTableExists_]
        }
      }
      #Load inputs to datastore
      if (!is.null(ModuleSpecs_ls$Inp)) {
        inputsToDatastore(ProcessedInputs_ls[[EntryName]], ModuleSpecs_ls, Module)
      }
    }
  }

  #Report done with initialization
  #-------------------------------
  if ( saveModelState ) {
    writeLog(
      c(
        paste("Scenario:",ve.model$ModelState_ls$Scenario),
        paste("Description:",ve.model$ModelState_ls$Description)
      ), Level = "warn"
    )
  }
  writeLog("Model successfully initialized.", Level = "warn")
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

  invisible(
    list(
      Errors = Errors_,
      Warnings = Warnings_
    )
  )
  if ( length(Warnings_) > 0 ) {
    writeLog(Warnings_,Level="warn")
  }
  if ( length(Errors_) > 0 ) {
    writeLog(Errors_,Level="error")
    if ( StopOnErr ) stop("runModule call failed") else warning("Not stopping for errors...")
  }
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
