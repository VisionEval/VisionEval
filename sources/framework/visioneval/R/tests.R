#========
#module.R
#========

#This script defines functions related to testing modules and other
#parts of the the system

#TEST MODULE
#===========
#' Test module
#'
#' \code{testModule} a visioneval framework module developer function that sets
#' up a test environment and tests a module.
#'
#' This function is used to set up a test environment and test a module to check
#' that it can run successfully in the VisionEval model system. The function
#' sets up the test environment by switching to the tests directory and
#' initializing a model state list, a log file, and a datastore. The user may
#' use an existing datastore rather than initialize a new datastore. The use
#' case for loading an existing datastore is where a package contains several
#' modules that run in sequence. The first module would initialize a datastore
#' and then subsequent modules use the datastore that is modified by testing the
#' previous module. When run this way, it is also necessary to set the
#' SaveDatastore argument equal to TRUE so that the module outputs will be
#' saved to the datastore. The function performs several tests including
#' checking whether the module specifications are written properly, whether
#' the the test inputs are correct and complete and can be loaded into the
#' datastore, whether the datastore contains all the module inputs identified in
#' the Get specifications, whether the module will run, and whether all of the
#' outputs meet the module's Set specifications. The latter check is carried out
#' in large part by the checkModuleOutputs function that is called.
#'
#' @param ModuleName A string identifying the module name.
#' @param Param_ls Parameter configuration (list)
#' @param ... Other parameters (see comments)
#' @return If DoRun is FALSE, the return value is a list containing the module
#'   specifications. If DoRun is TRUE, there is no return value. The function
#'   writes out messages to the console and to the log as the testing proceeds.
#'   These messages include the time when each test starts and when it ends.
#'   When a key test fails, requiring a fix before other tests can be run,
#'   execution stops and an error message is written to the console. Detailed
#'   error messages are also written to the log.
#' @export
testModule <-
function(ModuleName,Param_ls=NULL,...) {
  #            ParamDir = "defs",
  #            RunParamFile = "run_parameters.json",
  #            GeoFile = "geo.csv",
  #            ModelParamFile = "model_parameters.json",
  #            LoadDatastore = FALSE,
  #            SaveDatastore = TRUE,
  #            DoRun = TRUE,
  #            RunFor = "AllYears",
  #            StopOnErr = TRUE,
  #            RequiredPackages = NULL,
  #            TestGeoName = NULL)

  # TODO: make this work with the new parameter setup
  #       the entire thing needs to be rethought...

  #Set working directory to tests and return to main module directory on exit
  #--------------------------------------------------------------------------
  setwd("tests")
  on.exit(setwd("../"))

  if ( ! is.list(Param_ls) ) {
    model.env <- modelEnvironment()
    if ( "RunParam_ls" %in% ls(model.env) ) {
      Param_ls <- model.env$RunParam_ls
    } else {
      Param_ls <- list()
    }
  }

  ParamDir = "defs"
  RunParamFile = "run_parameters.json"
  GeoFile = "geo.csv"
  ModelParamFile = "model_parameters.json"
  LoadDatastore = FALSE
  SaveDatastore = TRUE
  DoRun = TRUE
  RunFor = "AllYears"
  StopOnErr = TRUE
  RequiredPackages = NULL
  TestGeoName = NULL

  defParam_ls <- list(
    ParamDir = "defs",
    RunParamFile = "run_parameters.json",
    GeoFile = "geo.csv",
    ModelParamFile = "model_parameters.json",
    LoadDatastore = FALSE,
    SaveDatastore = TRUE,
    DoRun = TRUE,
    RunFor = "AllYears",
    StopOnErr = TRUE,
    RequiredPackages = NULL,
    TestGeoName = NULL
  )
  missing <- ! names(defParam_ls) %in% names(Param_ls)
  Param_ls[missing] <- defParam_ls[missing]
  f.env <- environment()
  for ( p in names(Param_ls) ) assign(p,Param_ls[p],envir=f.env)

  #Initialize model state and log files
  #------------------------------------
  Msg <- paste0("Testing ", ModuleName, ".")
  initLog(Save=TRUE,Threshold="info")
  initModelState(Save=TRUE,Param_ls=NULL)
  writeLog(Msg,Level="warn")
  rm(Msg)

  #Assign the correct datastore interaction functions
  #--------------------------------------------------
  assignDatastoreFunctions(readModelState()$DatastoreType)

  #Make correspondence tables of modules and datasets to packages
  #--------------------------------------------------------------
  #This supports soft call and dataset references in modules
  RequiredPkg_ <- RequiredPackages
  #If RequiredPkg_ is not NULL make a list of modules and datasets in packages
  if (!is.null(RequiredPkg_)) {
    #Make sure all required packages are present
    InstalledPkgs_ <- rownames(installed.packages())
    MissingPkg_ <- RequiredPkg_[!(RequiredPkg_ %in% InstalledPkgs_)];
    if (length(MissingPkg_ != 0)) {
      Msg <-
      paste0("One or more required packages need to be installed in order ",
        "to run the model. Following are the missing package(s): ",
        paste(MissingPkg_, collapse = ", "), ".")
      stop(Msg)
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
    setModelState(list(ModulesByPackage_df = ModulesByPackage_df,
      DatasetsByPackage_df = DatasetsByPackage_df))
    rm(Datasets_df, WhichAreModules_)
  }

  #Load datastore if specified or initialize new datastore
  #-------------------------------------------------------
  if (LoadDatastore) {
    writeLog("Attempting to load datastore.", Level="warn")
    DatastoreName <- getModelState()$DatastoreName
    if (!file.exists(DatastoreName)) {
      Msg <-
      paste0("LoadDatastore argument is TRUE but the datastore file ",
        "specified in the RunParamFile doesn't exist in the tests ",
        "directory.")
      stop(Msg)
      rm(Msg)
    }
    loadDatastore(
      FileToLoad = DatastoreName,
      SaveDatastore = FALSE
    )
    writeLog("Datastore loaded.", Level="warn")
  } else {
    writeLog("Attempting to initialize datastore.", Level="warn")
    initDatastore()
    readGeography()
    initDatastoreGeography()
    loadModelParameters()
    writeLog("Datastore initialized.", Level="warn")
  }

  #Load module specifications and check whether they are proper
  #------------------------------------------------------------
  loadSpec <- function() {
    SpecsName <- paste0(ModuleName, "Specifications")
    SpecsFileName <- paste0("../data/", SpecsName, ".rda")
    load(SpecsFileName)
    return(processModuleSpecs(get(SpecsName)))
  }
  writeLog("Attempting to load and check specifications.", Level="warn")
  Specs_ls <- loadSpec()
  #Check for errors
  Errors_ <- checkModuleSpecs(Specs_ls, ModuleName)
  if (length(Errors_) != 0) {
    Msg <-
    paste0("Specifications for module '", ModuleName,
      "' have the following errors.")
    writeLog(Msg,Level="error")
    writeLog(Errors_,Level="error")
    Msg <- paste0("Specifications for module '", ModuleName,
      "' have one or more errors. Check the log for details.")
    stop(Msg)
    rm(Msg)
  }
  rm(Errors_)
  writeLog("Module specifications successfully loaded and checked for errors.",
    Level="warn")
  #Check for developer warnings
  DeveloperWarnings_ls <-
  lapply(c(Specs_ls$Inp, Specs_ls$Get, Specs_ls$Set), function(x) {
    attributes(x)$WARN
  })
  DeveloperWarnings_ <-
  unique(unlist(lapply(DeveloperWarnings_ls, function(x) x[!is.null(x)])))
  if (length(DeveloperWarnings_) != 0) {
    writeLog(DeveloperWarnings_,Level="warn")
    Msg <- paste0(
      "Specifications check for module '", ModuleName, "' generated one or ",
      "more warnings. Check log for details."
    )
    warning(Msg)
    rm(DeveloperWarnings_ls, DeveloperWarnings_, Msg)
  }

  #Process, check, and load module inputs
  #--------------------------------------
  if (is.null(Specs_ls$Inp)) {
    writeLog("No inputs to process.", Level="warn")
    # If no inputs and is Initialize module return
    # i.e. all inputs are optional and none are provided
    if (ModuleName == "Initialize") return()
  } else {
    writeLog("Attempting to process, check and load module inputs.",
      Level="warn")
    # Process module inputs
    ProcessedInputs_ls <- processModuleInputs(Specs_ls, ModuleName)
    # Write warnings to log if any
    if (length(ProcessedInputs_ls$Warnings != 0)) {
      writeLog(ProcessedInputs_ls$Warnings,Level="warn")
    }
    # Write errors to log and stop if any errors
    if (length(ProcessedInputs_ls$Errors) != 0)  {
      Msg <- paste0(
        "Input files for module ", ModuleName,
        " have errors. Check the log for details."
      )
      writeLog(ProcessedInputs_ls$Errors,Level="error")
      stop(Msg)
    }
    # If module is NOT Initialize, save the inputs in the datastore
    if (ModuleName != "Initialize") {
      inputsToDatastore(ProcessedInputs_ls, Specs_ls, ModuleName)
      writeLog("Module inputs successfully checked and loaded into datastore.",
        Level="warn")
    } else {
      if (DoRun) {
        # If module IS Initialize, apply the Initialize function
        initFunc <- get("Initialize")
        InitializedInputs_ls <- initFunc(ProcessedInputs_ls)
        # Write warnings to log if any
        if (length(InitializedInputs_ls$Warnings != 0)) {
          writeLog(InitializedInputs_ls$Warnings,Level="warn")
        }
        # Write errors to log and stop if any errors
        if (length(InitializedInputs_ls$Errors) != 0) {
          writeLog(InitializedInputs_ls$Errors,Level="error")
          stop("Errors in Initialize module inputs. Check log for details.")
        }
        # Save inputs to datastore
        inputsToDatastore(InitializedInputs_ls, Specs_ls, ModuleName)
        writeLog("Module inputs successfully checked and loaded into datastore.",
          Level="warn")
        return() # Break out of function because purpose of Initialize is to process inputs.
      } else {
        return(ProcessedInputs_ls)
      }
    }
  }

  #Check whether datastore contains all data items in Get specifications
  #---------------------------------------------------------------------
  writeLog(
    "Checking whether datastore contains all datasets in Get specifications.",
    Level="warn")
  G <- getModelState()
  Get_ls <- Specs_ls$Get
  #Vector to keep track of missing datasets that are specified
  Missing_ <- character(0)
  #Function to check whether dataset is optional
  isOptional <- function(Spec_ls) {
    if (!is.null(Spec_ls$OPTIONAL)) {
      Spec_ls$OPTIONAL
    } else {
      FALSE
    }
  }
  #Vector to keep track of Get specs that need to be removed from list because
  #they are optional and the datasets are not present
  OptSpecToRemove_ <- numeric(0)
  #Check each specification
  for (i in 1:length(Get_ls)) {
    Spec_ls <- Get_ls[[i]]
    if (Spec_ls$GROUP == "Year") {
      for (Year in G$Years) {
        if (RunFor == "NotBaseYear"){
          if(!Year %in% G$BaseYear){
            Present <-
            checkDataset(Spec_ls$NAME, Spec_ls$TABLE, Year, G$Datastore)
            if (!Present) {
              if(isOptional(Spec_ls)) {
                #Identify for removal because optional and not present
                OptSpecToRemove_ <- c(OptSpecToRemove_, i)
              } else {
                #Identify as missing because not optional and not present
                Missing_ <- c(Missing_, attributes(Present))
              }
            }
          }
        } else {
          Present <-
          checkDataset(Spec_ls$NAME, Spec_ls$TABLE, Year, G$Datastore)
          if (!Present) {
            if(isOptional(Spec_ls)) {
              #Identify for removal because optional and not present
              OptSpecToRemove_ <- c(OptSpecToRemove_, i)
            } else {
              #Identify as missing because not optional and not present
              Missing_ <- c(Missing_, attributes(Present))
            }
          }
        }

      }
    }
    if (Spec_ls$GROUP == "BaseYear") {
      Present <-
      checkDataset(Spec_ls$NAME, Spec_ls$TABLE, G$BaseYear, G$Datastore)
      if (!Present) {
        if (isOptional(Spec_ls)) {
          #Identify for removal because optional and not present
          OptSpecToRemove_ <- c(OptSpecToRemove_, i)
        } else {
          #Identify as missing because not optional and not present
          Missing_ <- c(Missing_, attributes(Present))
        }
      }
    }
    if (Spec_ls$GROUP == "Global") {
      Present <-
      checkDataset(Spec_ls$NAME, Spec_ls$TABLE, "Global", G$Datastore)
      if (!Present) {
        if (isOptional(Spec_ls)) {
          #Identify for removal because optional and not present
          OptSpecToRemove_ <- c(OptSpecToRemove_, i)
        } else {
          #Identify as missing because not optional and not present
          Missing_ <- c(Missing_, attributes(Present))
        }
      }
    }
  }
  #If any non-optional datasets are missing, write out error messages and
  #stop execution
  if (length(Missing_) != 0) {
    Msg <-
    paste0("The following datasets identified in the Get specifications ",
      "for module ", ModuleName, " are missing from the datastore.")
    Msg <- paste(c(Msg, Missing_), collapse = "\n")
    writeLog(Msg,Level="error")
    stop(
      paste0("Datastore is missing one or more datasets specified in the ",
        "Get specifications for module ", ModuleName, ". Check the log ",
        "for details.")
    )
    rm(Msg)
  }
  #If any optional datasets are missing, remove the specifications for them so
  #that there will be no errors when data are retrieved from the datastore
  if (length(OptSpecToRemove_) != 0) {
    Specs_ls$Get <- Specs_ls$Get[-OptSpecToRemove_]
  }
  writeLog(
    "Datastore contains all datasets identified in module Get specifications.",
    Level="warn")

  #Run the module and check that results meet specifications
  #---------------------------------------------------------
  #The module is run only if the DoRun argument is TRUE. Otherwise the
  #datastore is initialized, specifications are checked, and a list is
  #returned which contains the specifications list, the data list from the
  #datastore meeting specifications, and a functions list containing any
  #called module functions.

  #Run the module if DoRun is TRUE
  if (DoRun) {
    writeLog(
      "Running module and checking whether outputs meet Set specifications.",
      Level="warn"
    )
    if (SaveDatastore) {
      writeLog("Also saving module outputs to datastore.", Level="warn")
    }
    #Load the module function
    Func <- get(ModuleName)
    #Load any modules identified by 'Call' spec if any
    if (is.list(Specs_ls$Call)) {
      Call <- list(
        Func = list(),
        Specs = list()
      )
      for (Alias in names(Specs_ls$Call)) {
        #Called module function when specified as package::module
        Function <- Specs_ls$Call[[Alias]]
        #Called module function when only module is specified
        if (length(unlist(strsplit(Function, "::"))) == 1) {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
          paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          rm(Pkg_df)
        }
        #Called module specifications
        Specs <- paste0(Function, "Specifications")
        #Assign called module function and specifications for the alias
        Call$Func[[Alias]] <- eval(parse(text = Function))
        Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
        Call$Specs[[Alias]]$RunBy <- Specs_ls$RunBy
      }
    }
    #Run module for each year
    if (RunFor == "AllYears") Years <- getYears()
    if (RunFor == "BaseYear") Years <- G$BaseYear
    if (RunFor == "NotBaseYear") Years <- getYears()[!getYears() %in% G$BaseYear]
    for (Year in Years) {
      ResultsCheck_ <- character(0)
      #If RunBy is 'Region', this code is run
      if (Specs_ls$RunBy == "Region") {
        #Get data from datastore
        L <- getFromDatastore(Specs_ls, RunYear = Year)
        if (exists("Call")) {
          for (Alias in names(Call$Specs)) {
            L[[Alias]] <-
            getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
          }
        }
        #Run module
        if (exists("Call")) {
          R <- Func(L, Call$Func)
        } else {
          R <- Func(L)
        }
        #Check for errors and warnings in module return list
        #Save results in datastore if no errors from module
        if (is.null(R$Errors)) {
          #Check results
          Check_ <-
          checkModuleOutputs(
            Data_ls = R,
            ModuleSpec_ls = Specs_ls,
            ModuleName = ModuleName)
          ResultsCheck_ <- Check_
          #Save results if SaveDatastore and no errors found
          if (SaveDatastore & length(Check_) == 0) {
            setInDatastore(R, Specs_ls, ModuleName, Year, Geo = NULL)
          }
        }
        #Handle warnings
        if (!is.null(R$Warnings)) {
          writeLog(R$Warnings,Level="warn")
          Msg <-
          paste0("Module ", ModuleName, " has reported one or more warnings. ",
            "Check log for details.")
          warning(Msg)
        }
        #Handle errors
        if (!is.null(R$Errors) & StopOnErr) {
          writeLog(R$Errors,Level="warn")
          Msg <-
          paste0("Module ", ModuleName, " has reported one or more errors. ",
            "Check log for details.")
          stop(Msg)
        }
        #Otherwise the following code is run
      } else {
        #Initialize vectors to store module errors and warnings
        Errors_ <- character(0)
        Warnings_ <- character(0)
        #Identify the units of geography to iterate over
        GeoCategory <- Specs_ls$RunBy
        #Create the geographic index list
        GeoIndex_ls <- createGeoIndexList(c(Specs_ls$Get, Specs_ls$Set), GeoCategory, Year)
        if (exists("Call")) {
          for (Alias in names(Call$Specs)) {
            GeoIndex_ls[[Alias]] <-
            createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
          }
        }
        #Run module for each geographic area
        Geo_ <- readFromTable(GeoCategory, GeoCategory, Year)
        for (Geo in Geo_) {
          #Get data from datastore for geographic area
          L <-
          getFromDatastore(Specs_ls, RunYear = Year, Geo = Geo, GeoIndex_ls = GeoIndex_ls)
          if (exists("Call")) {
            for (Alias in names(Call$Specs)) {
              L[[Alias]] <-
              getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo = Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
            }
          }
          #Run model for geographic area
          if (exists("Call")) {
            R <- Func(L, Call$Func)
          } else {
            R <- Func(L)
          }
          #Check for errors and warnings in module return list
          #Save results in datastore if no errors from module
          if (is.null(R$Errors)) {
            #Check results
            Check_ <-
            checkModuleOutputs(
              Data_ls = R,
              ModuleSpec_ls = Specs_ls,
              ModuleName = ModuleName)
            ResultsCheck_ <- c(ResultsCheck_, Check_)
            #Save results if SaveDatastore and no errors found
            if (SaveDatastore & length(Check_) == 0) {
              setInDatastore(R, Specs_ls, ModuleName, Year, Geo = Geo, GeoIndex_ls = GeoIndex_ls)
            }
          }
          #Handle warnings
          if (!is.null(R$Warnings)) {
            writeLog(R$Warnings,Level="warn")
            Msg <-
            paste0("Module ", ModuleName, " has reported one or more warnings. ",
              "Check log for details.")
            warning(Msg)
          }
          #Handle errors
          if (!is.null(R$Errors) & StopOnErr) {
            writeLog(R$Errors,Level="error")
            Msg <-
            paste0("Module ", ModuleName, " has reported one or more errors. ",
              "Check log for details.")
            stop(Msg)
          }
        }
      }
      if (length(ResultsCheck_) != 0) {
        Msg <-
        paste0("Following are inconsistencies between module outputs and the ",
          "module Set specifications:")
        Msg <- paste(c(Msg, ResultsCheck_), collapse = "\n")
        writeLog(Msg,Level="error")
        rm(Msg)
        stop(
          paste0("The outputs for module ", ModuleName, " are inconsistent ",
            "with one or more of the module's Set specifications. ",
            "Check the log for details."))
      }
    }
    writeLog("Module run successfully and outputs meet Set specifications.",
      Level="warn")
    if (SaveDatastore) {
      writeLog("Module outputs saved to datastore.", Level="warn")
    }
    #Print success message if no errors found
    Msg <- paste0("Congratulations. Module ", ModuleName, " passed all tests.")
    writeLog(Msg, Level="warn")
    rm(Msg)

    #Return the specifications, data list, and functions list if DoRun is FALSE
  } else {
    #Load any modules identified by 'Call' spec if any
    if (!is.null(Specs_ls$Call)) {
      Call <- list(
        Func = list(),
        Specs = list()
      )
      for (Alias in names(Specs_ls$Call)) {
        Function <- Specs_ls$Call[[Alias]]
        #Called module function when only module is specified
        if (length(unlist(strsplit(Function, "::"))) == 1) {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
          paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          rm(Pkg_df)
        }
        #Called module specifications
        Specs <- paste0(Function, "Specifications")
        Call$Func[[Alias]] <- eval(parse(text = Function))
        Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
      }
    }
    #Get data from datastore
    if (RunFor == "AllYears") Year <- getYears()[1]
    if (RunFor == "BaseYear") Year <- G$BaseYear
    if (RunFor == "NotBaseYear") Year <- getYears()[!getYears() %in% G$BaseYear][1]
    #Identify the units of geography to iterate over
    GeoCategory <- Specs_ls$RunBy
    #Create the geographic index list
    GeoIndex_ls <- createGeoIndexList(Specs_ls$Get, GeoCategory, Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
        createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
      }
    }
    #Get the data required
    if (GeoCategory == "Region") {
      L <- getFromDatastore(Specs_ls, RunYear = Year, Geo = NULL)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo = NULL)
        }
      }
    } else {
      Geo_ <- readFromTable(GeoCategory, GeoCategory, Year)
      #Check whether the TestGeoName is proper
      if (!is.null(TestGeoName)) {
        if (!(TestGeoName %in% Geo_)) {
          stop(paste0(
            "The 'TestGeoName' value - ", TestGeoName,
            " - is not a recognized name for the ",
            GeoCategory, " geography that this module is specified to be run ",
            "for."
          ))
        }
      }
      #If TestGeoName is NULL get the data for the first name in the list
      if (is.null(TestGeoName)) TestGeoName <- Geo_[1]
      #Get the data
      L <- getFromDatastore(Specs_ls, RunYear = Year, Geo = TestGeoName, GeoIndex_ls = GeoIndex_ls)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo = TestGeoName, GeoIndex_ls = GeoIndex_ls)
        }
      }
    }
    #Return the specifications, data list, and called functions
    if (exists("Call")) {
      return(list(Specs_ls = Specs_ls, L = L, M = Call$Func))
    } else {
      return(list(Specs_ls = Specs_ls, L = L))
    }
  }
}

#LOAD SAVED DATASTORE
#====================
#' Load saved datastore for testing
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
loadDatastore <- function(FileToLoad, SaveDatastore = TRUE) {
  # TODO: This function is apparently only used when testing a module
  # (and it is mighty invasive for that!)
  G <- getModelState()
  #If data store exists, rename
  DatastoreName <- G$DatastoreName
  if (file.exists(DatastoreName) & SaveDatastore) {
    # TODO: blow away the existing one if SaveDatastore is not TRUE
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
    # Note: already checked geography consistency
#     GeoFile <- file.path(Dir, GeoFile)
#     Geo_df <- read.csv(GeoFile, colClasses = "character")
#     Update_ls <- list()
#     Update_ls$BzoneSpecified <- !all(is.na(Geo_df$Bzone))
#     Update_ls$CzoneSpecified <- !all(is.na(Geo_df$Czone))
#     Update_ls$Geo_df <- Geo_df
#     setModelState(Update_ls)
    listDatastore() # Rebuild datastore index
  } else {
    Message <- paste("File", FileToLoad, "not found.")
    writeLog(Message,Level="error")
    stop(Message)
  }
  TRUE
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
#' that are called by the module. See \code{parseModuleCalls}.
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
             "Datasets will be overwritten when the model runs. ",
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
