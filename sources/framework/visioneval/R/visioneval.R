#============
#visioneval.R
#============

#This script defines the main functions that implement the VisionEval framework
#and are intended to be exported.

utils::globalVariables(c("initDatastore","Year","ModelState_ls"))

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
#' or loads an existing datastore if one has been identified;
#' 4) Parses the model run script to identify the modules in their order of
#' execution and checks whether all the identified packages are installed and
#' the modules exist in the packages;
#' 5) Checks that all data requested from the datastore will be available when
#' it is requested and that the request specifications match the datastore
#' specifications;
#' 6) Checks all of the model input files to determine whether they they are
#' complete and comply with specifications.
#'
#' @param ModelScriptFile A string identifying the path to the file that contains
#'   the steps in the model to run (i.e. the calls to \code{runModule})
#' @param ParamDir A string identifying the relative or absolute path to the
#'   directory where the parameter and geography definition files are located.
#'   The default value is "defs".
#' @param RunParamFile A string identifying the name of a JSON-formatted text
#' file that contains parameters needed to identify and manage the model run.
#' The default value is "run_parameters.json".
#' @param GeoFile A string identifying the name of a text file in
#' comma-separated values format that contains the geographic specifications
#' for the model. The default value is "geo.csv".
#' @param ModelParamFile A string identifying the name of a JSON-formatted text
#' file that contains global model parameters that are important to a model and
#' may be shared by several modules.
#' @param LoadDatastore A logical identifying whether an existing datastore
#' should be loaded.
#' @param DatastoreName A string identifying the full path name of a datastore
#' to load or NULL if an existing datastore in the working directory is to be
#' loaded.
#' @param SaveDatastore A string identifying whether if an existing datastore
#' in the working directory should be saved rather than removed.
#' @param SimulateRun A logical identifying whether the model run should be
#' simulated: i.e. each step is checked to confirm that data of proper type
#' will be present when the module is called.
#' @return None. The function prints to the log file messages which identify
#' whether or not there are errors in initialization. It also prints a success
#' message if initialization has been successful.
#' @export
initializeModel <-
  function(
    ModelScriptFile = "run_model.R",
    ParamDir = "defs",
    RunParamFile = "run_parameters.json",
    GeoFile = "geo.csv",
    ModelParamFile = "model_parameters.json",
    LoadDatastore = FALSE,
    DatastoreName = NULL,
    SaveDatastore = TRUE,
    SimulateRun = FALSE
  ) {
    
    #====================================================================
    #INITIALIZE MODEL STATE AND LOG FILES, AND ASSIGN DATASTORE FUNCTIONS
    #====================================================================
    #Print introductory message
    Msg <-
      paste0(Sys.time(), " -- Initializing Model. This may take a while.")
    print(Msg)
    #For VE_GUI, check log file location before long running operation begins
    preExistingModelState <- getOption("visioneval.preExistingModelState", NULL)
    #If no preExistingModelState, initialize model state and log
    if (is.null(preExistingModelState)) {
      #If a ModelState.Rda file exists rename
      if (file.exists("ModelState.Rda")) {
        file.rename("ModelState.Rda", "PreviousModelState.Rda")
      }
      #Initialize a new model state file
      initModelStateFile(Dir = ParamDir, ParamFile = RunParamFile)
      initLog()
      writeLog(Msg)
      readGeography(Dir = ParamDir, GeoFile = GeoFile)
      rm(Msg)
      #Otherwise read the preExistingModelState
    } else {
      ModelState_ls <- readModelState()
      writeLog("option visioneval.keepExistingModelState TRUE so skipping initModelStateFile and initLog",
               Print=TRUE)
      setModelState(preExistingModelState)
    }
    #Assign the correct datastore interaction functions
    assignDatastoreFunctions(readModelState()$DatastoreType)
    
    #=======================================
    #CHECK CONFLICTS WITH EXISTING DATASTORE
    #=======================================
    DstoreConflicts <- local({
      #--------------
      #Set up objects
      #--------------
      ErrMsg <- character(0)
      InfoMsg <- character(0)
      G <- getModelState()
      #Normalized path name of the datastore used in the model run
      RunDstoreName <-
        normalizePath(G$DatastoreName, winslash = "/", mustWork = FALSE)
      #Normalized path name of the datastore to be loaded if any
      LoadDstoreName <- ifelse(
        is.null(DatastoreName),
        "",
        normalizePath(DatastoreName, winslash = "/", mustWork = FALSE))
      #Define function to get the directory path

      getDirPath <- function(FilePath) {
        FilePathSplit_ <- unlist(strsplit(FilePath, "/"))
        paste(FilePathSplit_[-length(FilePathSplit_)], collapse = "/")
      }
      #Get path for run datastore and load datastore
      RunDstoreDir <- dirname(RunDstoreName)
      LoadDstoreDir <- ifelse(
        is.null(DatastoreName),
        "",
        dirname(LoadDstoreName)
      )
      #-------------------------------------------------------------------
      #First round of error checks on datastore conflicts, loading, saving
      #-------------------------------------------------------------------
      #Is LoadDatastore FALSE but DatastoreName not NULL
      # JRaw: Why not just force LoadDatastore <- ! is.null(DatastoreName)
      #       (i.e. just have one parameter, the DatastoreName)
      IsLoadConfusion <- !LoadDatastore & !is.null(DatastoreName)
      #Does the datastore identitied in the run parameters already exist?
      IsConflict <- file.exists(RunDstoreName)
      #Is the run datastore the same as the datastore to be loaded?
      ExistingDstoreLoaded <-
        RunDstoreName == LoadDstoreName & RunDstoreDir == LoadDstoreDir
      #Error if not LoadDatastore but DatastoreName not NULL
      if (IsLoadConfusion) {
        ErrMsg <- c(ErrMsg, paste(
          "The value of the 'LoadDatastore' parameter of the 'initializeModel'",
          "call is FALSE, but value of the 'DatastoreName' parameter is not NULL.",
          "The 'DatastoreName' parameter is used to specify the name of",
          "a datastore to be loaded. If the name is not NULL, then the value of",
          "the 'LoadDatastore' parameter must be TRUE."
        ))
      }
      #Error if LoadDatastore and the DatastoreName does not exist
      if (LoadDatastore & !file.exists(LoadDstoreName)) {
        ErrMsg <- c(ErrMsg, paste(
          "The datastore to be loaded identified by the value of the",
          "'DatastoreName' parameter in the 'initializeModel' call does not",
          "exist. Perhaps the full path name was not specified."
        ))
      }
      #Error if existing datastore overwritten and not loaded or saved
      if (IsConflict & !(LoadDatastore | SaveDatastore)) {
        ErrMsg <- c(ErrMsg, paste(
          "An existing datastore will be overwritten when the model is run.",
          "This datastore is not specified to be loaded or saved.",
          "If this datastore is to be loaded then set the value of",
          "the 'LoadDatastore' parameter of the 'initializeModel' call to be",
          "TRUE and set the value of the 'DatastoreName' parameter to be the",
          "name of the existing datastore. If the existing datastore is to be",
          "saved, then set the value of the 'SaveDatastore' parameter to be",
          "TRUE."
        ))
      }
      #Error if datastore overwritten and not saved and load name not same
      if (IsConflict & LoadDatastore & !ExistingDstoreLoaded & !SaveDatastore) {
        ErrMsg <- c(ErrMsg, paste(
          "An existing datastore will be overwritten when the model is run.",
          "Although the value of the 'LoadDatastore' parameter of the",
          "'initializeModel' call is TRUE, the name of the datastore",
          "to be loaded as specified by the value of the 'DatastoreName'",
          "parameter is not the same as the name of the existing datastore.",
          "Perhaps the full path name was not specified."
        ))
      }
      #If errors, restore model state, print error message to log, and stop
      if (length(ErrMsg) != 0) {
        writeLog(ErrMsg)
        return(list(Err = ErrMsg))
      }
      #---------------------------------------------------
      #If LoadDatastore, check existence and compatibility
      #---------------------------------------------------
      #Check if geography, units, deflators, & base year are the same as run
      #parameters
      # JRaw: Should process LoadDatastore all in one place; see below tagged JRaw::LoadDatastore
      if (LoadDatastore) {
        #Run datastore type
        RunDstoreType <- G$DatastoreType
        #Load datastore type
        LoadEnv <- new.env()
        load(file.path(LoadDstoreDir, "ModelState.Rda"), envir = LoadEnv) # Open into ve.model environment
        LoadDstoreType <- LoadEnv$ModelState_ls$DatastoreType
        #Check that run and load datastores are of same type
        if (RunDstoreType != LoadDstoreType) {
          ErrMsg <- c(ErrMsg, paste(
            "In order for a datastore to be loaded, both the model run",
            "datastore and the loaded datastore must be of same type.",
            "Model run datastore is specified as type:", RunDstoreType, ".",
            "Datastore to load is type:", LoadDstoreType, "."
          ))
        }
        #Check that geography, units, and deflators are consistent
        SameGeography <-
          all.equal(ModelState_ls$Geo_df, LoadEnv$ModelState_ls$Geo_df)
        SameUnits <-
          all.equal(ModelState_ls$Units, LoadEnv$ModelState_ls$Units)
        SameDeflators <-
          all.equal(ModelState_ls$Deflators, LoadEnv$ModelState_ls$Deflators)
        SameBaseYear <- ModelState_ls$BaseYear == LoadEnv$ModelState_ls$BaseYear
        if (!(SameGeography & SameUnits & SameDeflators & SameBaseYear)) {
          ErrMsg <- c(ErrMsg, paste(
            "There are inconsistencies in the geography, units, deflators and/or",
            "base year of the specified model run datastore and the datastore",
            "that is to be loaded. If the specified datastore is to be loaded,",
            "the definitions for these values for the model run must be",
            "consistent with the values defined for the model run that produced",
            "the datastore to be loaded."
          ))
        }
      }
      #If errors, restore model state, print error message to log, and stop
      if (length(ErrMsg) != 0) {
        writeLog(ErrMsg)
        return(list(Err = ErrMsg))
      }
      #--------------------------
      #Other informational checks
      #--------------------------
      #If SaveDatastore but indeterminate what datastore to save
      if (!IsConflict & SaveDatastore) {
        InfoMsg <- c(InfoMsg, paste(
          "The value of the SaveDatastore argument is TRUE, but it is",
          "unknown what datastore is to be saved. The purpose of the",
          "'SaveDatastore' parameter is to save a copy of a datastore that",
          "will be overwritten when the model is run. If the name of an",
          "existing datastore is not the same as the name of the datastore",
          "specified in the 'run_parameters.json' file, it will not be",
          "overwritten and there is no need to save the datastore.",
          "When there is no datastore present with the same name as the",
          "datastore specified in the 'run_parameters.json' file, it can't",
          "be determined what the user intends by setting the value of",
          "SaveDatastore equal to TRUE. Therefore it has been ignored."
        ))
      }
      #If conflicting datastore loaded but not specified to be saved
      if (IsConflict & LoadDatastore & !SaveDatastore) {
        #Specify that the datastore will be saved
        SaveDatastore <<- TRUE
        #Notify user
        InfoMsg <- c(InfoMsg, paste(
          "An existing datastore will be overwritten when the model is run.",
          "Because the value of the 'LoadDatastore' parameter of the",
          "'initializeModel' call is TRUE, the existing datastore has been",
          "loaded for the model run. Although the value of the 'SaveDatastore'",
          "parameter is FALSE, a copy of the datastore has nevertheless",
          "been saved because some of the existing datastore contents may be",
          "overwritten when the model is run. Delete the saved datastore",
          "if you do not wish to keep it."
        ))
      }
      #Write information message to log if any
      if (length(InfoMsg) != 0) {
        writeLog(InfoMsg)
        return(list(Err = character(0)))
      }
    })
    if (length(DstoreConflicts$Err) != 0) {
      file.remove("ModelState.Rda")
      if (file.exists("PreviousModelState.Rda")) {
        file.rename("PreviousModelState.Rda", "ModelState.Rda")
      }
      stop(paste(
        "One or more inconsistencies in the specified model initialization",
        "must be corrected. Check log for details."))
    }
    rm(DstoreConflicts)
    
    #============================================================
    #LOAD OR INITIALIZE THE DATASTORE AND SAVE EXISTING DATASTORE
    #============================================================
    local({
      #----------------------------
      #Set up objects and functions
      #----------------------------
      #Get the model state
      G <- getModelState()
      #Define function to load model state file to assigned name
      assignLoadModelState <- function(FileName) {
        TempEnv <- new.env()
        load(FileName, envir = TempEnv)
        TempEnv$ModelState_ls
      }
      #Normalized path name of the datastore used in the model run
      RunDstoreName <-
        normalizePath(G$DatastoreName, winslash = "/", mustWork = FALSE)
      RunDstoreDir <- dirname(RunDstoreName)
      RunDstoreFile <- basename(RunDstoreName)
      #--------------------------------------------------------
      #Save previous datastore and model state if SaveDatastore
      #--------------------------------------------------------
      if (SaveDatastore & file.exists(RunDstoreName)) {
        #Create a directory in which to save the datastore
        TimeString <- gsub(" ", "_", as.character(Sys.time()))
        TimeString <- gsub(":", "-", TimeString)
        ArchiveDstoreName <- paste(RunDstoreName, TimeString, sep = "_")
        dir.create(ArchiveDstoreName)
        #Copy the datastore into the directory
        file.copy(RunDstoreName, ArchiveDstoreName, recursive = TRUE)
        #Copy the previous model state file into the directory
        file.copy("PreviousModelState.Rda",
                  file.path(ArchiveDstoreName, "ModelState.Rda"))
      }
      #---------------------------
      #Load datastore if specified
      #---------------------------
      # JRaw::LoadDatastore
      if (LoadDatastore) {
        #Normalized path name of the datastore to be loaded
        LoadDstoreName <-
          normalizePath(DatastoreName, winslash = "/", mustWork = FALSE)
        #Path of directory where datastore is to be loaded from
        LoadDstoreDir <- dirname(LoadDstoreName)
        #Name of the loaded datastore file
        LoadDstoreFile <- basename(LoadDstoreName)
        #Identify where loaded datastore is relative to run datastore
        SameName <- (LoadDstoreName == RunDstoreName)
        SameDir <- (LoadDstoreDir == RunDstoreDir)
        # Copy and load the model state file for the load datastore
        if (SameDir) {
          file.rename("PreviousModelState.Rda", "LoadModelState.Rda")
        } else {
          LoadModelStateFileName <- file.path(RunDstoreDir, "LoadModelState.Rda")
          file.copy(file.path(LoadDstoreDir, "ModelState.Rda"), LoadModelStateFileName)
        }
        # JRaw: This will fail with undefined variable if SameDir is TRUE
        LoadModelState_ls <- assignLoadModelState(LoadModelStateFileName)
        file.remove(LoadModelStateFileName)
        # Copy load datastore if not same as run datastore
        if (LoadDstoreDir != RunDstoreDir) {
          file.copy(LoadDstoreName, RunDstoreDir, recursive = TRUE)
        }
        # Renames the datastore to be the name specified for the model run
        if (LoadDstoreFile != RunDstoreFile) {
          file.rename(
            file.path(RunDstoreDir, LoadDstoreFile),
            RunDstoreName
          )
        }
        # Copy the datastore inventory to the ModelState_ls
        ModelState_ls$Datastore <- LoadModelState_ls$Datastore
        ModelState_ls$RequiredVEPackages <- if ( "RequiredVEPackages" %in% names(LoadModelState_ls) ) {
          unique(c(LoadModelState_ls$RequiredVEPackages,LoadModelState_ls$ModuleCalls_df$PackageName))
        } else {
          unique(LoadModelState_ls$ModuleCalls_df$PackageName)
        }
        setModelState(ModelState_ls)
        save(ModelState_ls, file = "ModelState.Rda")
        #Initialize geography for years not present in datastore
        RunYears_ <- ModelState_ls$Years
        LoadYears_ <- LoadModelState_ls$Years
        if (!all(RunYears_ == LoadYears_)) {
          NewYears_ <- RunYears_[!(RunYears_ %in% LoadYears_)]
          initDatastore(AppendGroups = NewYears_)
          initDatastoreGeography(GroupNames = NewYears_)
        }
      }
      #-------------------------------------------
      #Initialize datastore if no datastore loaded
      #-------------------------------------------
      if (!LoadDatastore) {
        initDatastore()
        readGeography(Dir = ParamDir, GeoFile = GeoFile)
        initDatastoreGeography()
        loadModelParameters(ModelParamFile = ModelParamFile)
      }
    })
    
    #===========================================================================
    #PARSE SCRIPT TO MAKE TABLE OF ALL THE MODULE CALLS, CHECK AND COMBINE SPECS
    #===========================================================================
    #Parse script and make data frame of modules that are called directly
    # JRaw: changed parseModelScript so it always returns the elements, rather
    #       than secretly stuffing them into the ModelState
    parsedScript <- parseModelScript(ModelScriptFile)

    # JRaw: Create required package list
    RequiredPkg_ <- parsedScript$RequiredVEPackages
    # JRaw: Handle requirements from previously loaded model state
    if ( LoadDatastore ) {
      G <- getModelState()
      if ( "RequiredVEPackages" %in% names(G) ) {
        RequiredPkg_ <- c(RequiredPkg_, G$RequiredVEPackages)
      }
      rm(G)
    }
    setModelState(list(ModuleCalls_df=parsedScript$ModuleCalls_df,RequiredVEPackages=RequiredPkg_))

    # JRaw: not an error to have already saved ModuleCalls_df: ModelState hosts module calls with duplicates
    ModuleCalls_df <- unique(parsedScript$ModuleCalls_df)

    # Report any required packages that are not also in module calls
    umc <- unique(ModuleCalls_df$PackageName)
    explicitRequired_ <- unique(RequiredPkg_)
    if ( any( not.in.umc <- ! (explicitRequired_ %in% umc) ) ) {
      for ( p in explicitRequired_[not.in.umc] ) message(paste("Package",p,"is required"))
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
      stop(Msg)
    }
    #Check for 'Initialize' module in each package if so add to ModuleCalls_df
    Add_ls <- list()
    for (Pkg in unique(ModuleCalls_df$PackageName)) {
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
      rm(Idx, End)
    }
    rm(Pkg, Pkg_, Add_ls)
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
    setModelState(list(ModulesByPackage_df = ModulesByPackage_df,
                       DatasetsByPackage_df = DatasetsByPackage_df))
    rm(Datasets_df, WhichAreModules_)
    #Iterate through each module call and check availability and specifications
    #create combined list of all specifications
    #JRaw: AllSpecs_ls is optionally used to simulate the model run, then discarded
    #      However, parsing the specs as we build AllSpecs_ls does critical error checking
    # ModulesByPackage_df lists all modules available in the packages
    # ModuleCalls_df lists only modules that appear in runModule commands
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
      Specs_ls <-
        processModuleSpecs(getModuleSpecs(ModuleName, PackageName))
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
                      message(Msg_)
                      writeLog(Warn_)
                    } else { # Resolved to exactly one package with the module
                      writeLog(paste("Provided module",callModule_,"from Package",testPkgs_[1]))
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
        paste0("There are one or more errors in the module calls: ",
               "package not installed, or module not present in package, ",
               "or errors in module specifications. ",
               "Check the log for details.")
      writeLog(Errors_)
      stop(Msg)
    }
    
    #==================
    #SIMULATE MODEL RUN
    #==================
    if (SimulateRun) {
      simDataTransactions(AllSpecs_ls)
    }

    #===============================
    #CHECK AND PROCESS MODULE INPUTS
    #===============================
    #Set up a list to store processed inputs for all modules
    ProcessedInputs_ls <- list()
    #Process inputs for all modules and add results to list
    for (i in 1:nrow(ModuleCalls_df)) {
      Module <- ModuleCalls_df$ModuleName[i]
      Package <- ModuleCalls_df$PackageName[i]
      EntryName <- paste(Package, Module, sep = "::")
      ModuleSpecs_ls <-
        processModuleSpecs(getModuleSpecs(Module, Package))
      #If there are inputs, process them
      if (!is.null(ModuleSpecs_ls$Inp)) {
        ProcessedInputs_ls[[EntryName]] <-
          processModuleInputs(ModuleSpecs_ls, Module)
        #If module is Initialize process inputs with Initialize function
        if (Module == "Initialize") {
          if (length(ProcessedInputs_ls[[Module]]$Errors) == 0) {
            initFunc <- eval(parse(text = paste(Package, Module, sep = "::")))
            InitData_ls <- ProcessedInputs_ls[[EntryName]]
            InitializedInputs_ls <- initFunc(InitData_ls)
            ProcessedInputs_ls[[EntryName]]$Data <- InitializedInputs_ls$Data
            ProcessedInputs_ls[[EntryName]]$Errors <- InitializedInputs_ls$Errors
            if (length(InitializedInputs_ls$Warnings > 0)) {
              writeLog(InitializedInputs_ls$Warnings)
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
      writeLog(InpErrors_)
      stop("Input files have errors. Check the log for details.")
    }
    rm(InpErrors_)
    
    #Load model inputs into the datastore
    #------------------------------------
    for (i in 1:nrow(ModuleCalls_df)) {
      #Get information to
      Module <- ModuleCalls_df$ModuleName[i]
      Package <- ModuleCalls_df$PackageName[i]
      EntryName <- paste(Package, Module, sep = "::")
      ModuleSpecs_ls <-
        processModuleSpecs(getModuleSpecs(Module, Package))
      #Eliminate writing any new input table to Global group if it already
      #exists
      if (!is.null(ModuleSpecs_ls$NewInpTable)) {
        NewInpTableSpecs_ls <- ModuleSpecs_ls$NewInpTable
        GlobalTableExists_ <- unlist(lapply(NewInpTableSpecs_ls, function(x) {
          if (x$GROUP == "Global") {
            checkTableExistence(x$TABLE, "Global", ModelState_ls$Datastore)
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
    
    #If no errors print out message
    #------------------------------
    SuccessMsg <-
      paste0(Sys.time(), " -- Model successfully initialized.")
    writeLog(SuccessMsg)
    print(SuccessMsg)
  }

#REQUIRE PACKAGE
#===============
#' Require package.
#'
#' \code{requireModule} a visioneval control function that
#' introduces a package dependency.
#'
#' This function simply returns TRUE. It is used to state a module
#' dependency explicitly to support internal Module calls that do not
#' explicitly name a package.
#'
#' @param Module During parsing, module is added to the list of
#'   packages to be searched for modules. Otherwise ignored.
#' @return TRUE. The function returns TRUE.
#' @export
requirePackage <- function(Module) TRUE

#RUN MODULE
#==========
#' Run module.
#'
#' \code{runModule} a visioneval framework model user function that
#' runs a module.
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
  Msg <-
    paste0(Sys.time(), " -- Starting module script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)
  #Load the package and module
  #---------------------------
  Function <- paste0(PackageName, "::", ModuleName)
  Specs <- paste0(PackageName, "::", ModuleName, "Specifications")
  M <- list()
  M$Func <- eval(parse(text = Function))
  M$Specs <- processModuleSpecs(eval(parse(text = Specs)))
  #Load any modules identified by 'Call' spec if any
  if (is.list(M$Specs$Call)) {
    Call <- list(
      Func = list(),
      Specs = list()
    )
    for (Alias in names(M$Specs$Call)) {
      #Called module function when specified as package::module
      Function <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(Function, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == Function) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      Specs <- paste0(Function, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = Function))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
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
      writeLog(Errors_)
      Msg <-
        paste0("Module Script ", ModuleName, " has reported one or more errors. ",
               "Check log for details.")
      stop(Msg)
    }
    #Handle warnings
    if (!is.null(R$Warnings)) {
      writeLog(Warnings_)
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more warnings. ",
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
        writeLog(Errors_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more errors. ",
                 "Check log for details.")
        stop(Msg)
      }
      #Handle warnings
      if (!is.null(R$Warnings)) {
        writeLog(Warnings_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more warnings. ",
                 "Check log for details.")
        warning(Msg)
        rm(Msg)
      }
    }
  }
  #Log and print ending message
  #----------------------------
  Msg <-
    paste0(Sys.time(), " -- Finish module '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)
  #Return error and warning messages if not StopOnErr
  #--------------------------------------------------
  if (!StopOnErr) {
    list(
      Errors = Errors_,
      Warnings = Warnings_
    )
  }
}

#RUN SCRIPT
#=========================================================
#' Run an R function or script file as if it were a module
#'
#' \code{runScript} a visioneval framework module developer function that
#' runs a function in run_model.R as if it were a packaged module.
#'
#' This function runs a function based module for a specified year.
#' The module function can be passed as an R function, the name of
#' a script file, or as an (exported) module name from a package - the
#' last will reproduce runModule functionality, except that you can
#' provide a revised specification file.
#'
#' This function does NOT write to the Datastore by default.
#'
#' @param Module An R function or character string function name (or string identifying a
#'   file to source) containing module code
#' @param Specification An R specification list or NULL (default), in which case a list
#'   will be sought using the name 
#' @param RunYear A string identifying the run year.
#' @param writeDatastore A logical indicating whether or not to write the results into
#'   the current Datastore, or just to return them
#' @return (invisible) The list of results returned by the module
#' @export
runScript <- function(Module, Specification=NULL, RunYear, writeDatastore = FALSE) {
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
    writeLog(Msg)
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
    paste0(Sys.time(), " -- Starting script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
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
      Function <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(Function, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == Function) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      Specs <- paste0(Function, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = Function))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
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
      writeLog(Errors_)
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more errors. ",
               "Check log for details.")
      warning(Msg)
      rm(Msg)
    }
    #Handle warnings
    if (!is.null(R$Warnings)) {
      writeLog(Warnings_)
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more warnings. ",
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
      if (writeDatastore && is.null(R$Errors)) {
        setInDatastore(R, M$Specs, ModuleName, RunYear, Geo, GeoIndex_ls)
      }
      #Add module errors and warnings if any
      Errors_ <- c(Errors_, R$Errors)
      Warnings_ <- c(Errors_, R$Warnings)
      #Handle errors
      if (!is.null(R$Errors)) {
        writeLog(Errors_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more errors. ",
                 "Check log for details.")
        warning(Msg)
        rm(Msg)
      }
      #Handle warnings
      if (!is.null(R$Warnings)) {
        writeLog(Warnings_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more warnings. ",
                 "Check log for details.")
        warning(Msg)
        rm(Msg)
      }
    }
  }

  #Log and print ending message
  #----------------------------
  Msg <-
    paste0(Sys.time(), " -- Finish module script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)

  #Return error and warning messages
  #--------------------------------------------------
  attr(R,"Errors") <- Errors_
  attr(R,"Warnings") <- Warnings_
  return(R)
}
