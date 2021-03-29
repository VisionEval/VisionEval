#================
#initialization.R
#================

#This script defines functions used to set up and manage a model
#run. These are helpers for finding the ModelState and running modules

#INITIALIZE MODEL RUN STATE
#==========================
#' Initialize model run parameters during initialization.
#'
#' \code{getModelParameters} a visioneval framework control function that establishes the runtime
#' model environment during model initialization.
#'
#' @param DotParam_ls Function arguments to initializeModel (overridden by stored RunParams in
#' environment; backward compatible).
#' @param DatastoreName Pass the one relevant explict argument from initializeModel
#' @return The RunParam_ls list of loaded parameters, properly overridden
#' @export
getModelParameters <- function(DotParam_ls=list(),DatastoreName) {

  # Access the model environment and check for RunModel condition
  ve.model <- modelEnvironment(Clear="") # clear ve.model environment (but don't destroy pre-existing RunParam_ls)

  # Check for pre-existing elements in ve.model environment (e.g. from VEModel$run) and RunParam_ls found there
  Param_ls <- addParameterSource(
    get0( "RunParam_ls", envir=ve.model, ifnotfound=list() ),
    "RunParam_ls in modelEnvironment()"
  )

  ModelDir <- getRunParameter("ModelDir",Param_ls=Param_ls) # Default is working directory

  # External environment will override dots (which are vestigial)
  if ( is.character(DatastoreName) ) { # the function parameter, not the Run Parameter
    DotParam_ls[["LoadDatastoreName"]] <- DatastoreName
  }
  if ( "Param_ls" %in% names(DotParam_ls) ) {
    DotParam_ls[ names(Param_ls) ] <- Param_ls; # Elevate parameters passed as an argument
  }
  DotParam_ls <- addParameterSource(DotParam_ls,"initializeModel(...)")
  RunParam_ls <- loadConfiguration(ParamDir=ModelDir,keep=Param_ls,override=DotParam_ls)

  # Look for defs along InputPath, and run_parameters.json.
  # Note that we can't change parameters listed in run_parameters.json
  # Those represent model setup invariants (e.g. inputs, defs locations)
  # New style model will already have loaded those from visioneval.cnf in the model root
  ParamPath <- findRuntimeInputFile("run_parameters.json","ParamDir",Param_ls=RunParam_ls,StopOnError=FALSE)
  if ( ! is.na(ParamPath) ) RunParam_ls <- loadConfiguration(ParamPath=ParamPath,override=RunParam_ls)

  # If ResultsDir is not present in RunParam_ls, set it to the current directory
  if ( ! "ResultsDir" %in% names(RunParam_ls) ) RunParam_ls[["ResultsDir"]] <- "."

  return(RunParam_ls)
}

#INITIALIZE MODEL STATE
#======================
#' Initialize model state.
#'
#' \code{initModelState} a visioneval framework control function that builds creates
#' a new ModelState_ls structure in the model environment, optionally saving it to
#' ModelState.rda.
#'
#' Parameters are can be supplied from run_parameters.json or from the function
#' invocation (the latter mostly for backwards compatibility). The new structure of
#' runtime parameters moves environmental parameters to configuration files outside the
#' model (e.g. DatastoreType) and reserves model specific elements for
#' run_parameters.json (Model, BaseYear, Years). See \code{loadConfiguration}.
#'
#' @param Save A logical (default=TRUE) indicating whether the model state file
#'   should be written out.
#' @param Param_ls A named list of model run parameters
#' @return The updated RunParam_ls with ModelState parameters fleshed out
#' @export
initModelState <- function(Save=TRUE,Param_ls=NULL) {

  # Load model environment (should have RunParam_ls already loaded, furnishing ...)
  model.env <- modelEnvironment()
  if ( ! is.list(Param_ls) ) {
    Param_ls <- model.env$RunParam_ls;
  }

  # The required parameters will be the initial elements for the ModelState
  # Other RunParam_ls elements will be placed in ModelState_ls$RunParam_ls

  # Make sure structural defaults are present in Param_ls
  # These will mostly be set in either the ve.runtime configuration or the model
  # configuration/run_parameters.json
  DefaultValues_ <- defaultVERunParameters(Param_ls)[c( "DatastoreName", "DatastoreType", "Seed" )]
  DefaultValues_ <- addParameterSource(DefaultValues_,"Defaults")
  Param_ls <- mergeParameters(DefaultValues_,Param_ls)

  # Don't need these until running model; Save will always be TRUE if running model
  # In memory model state will not contain these elements, though they may be added after the fact
  RequiredParam_ <- c( "Model", "Scenario", "Description", "Region", "BaseYear", "Years" )
  ParamExists_ <- RequiredParam_ %in% names(Param_ls)
  if (any(!ParamExists_)) {
    MissingParam_ <- RequiredParam_[!ParamExists_]
    Message <- c(
      "Missing model run parameters (not set in VisionEval configuration, run_parameters.json or function call):",
      paste(MissingParam_, collapse = ", ")
    )
    stop( writeLog(Message,Level="error") )
  }

  # Install the parameters that do exist - the required parameters become the foundation for
  # ModelState_ls. Other parameters are placed in newModelState_ls$RunParameters,
  # (including things like ParamDir, UnitsFile, etc.)
  # ModelState version of Param_ls now also includes the required parameters
  newModelState_ls <- Param_ls[c(names(DefaultValues_),RequiredParam_[ParamExists_])]
  newModelState_ls$LastChanged <- Sys.time()
  
  # Also load the complete deflators and units files, which will be accessed later via ModelState_ls
  DeflatorsFile <- getRunParameter("DeflatorsFile",Param_ls=Param_ls)
  DeflatorsFilePath <- findRuntimeInputFile(DeflatorsFile,Dir="ParamDir",Param_ls=Param_ls)
  newModelState_ls$Deflators <- read.csv(DeflatorsFilePath, as.is = TRUE)
  # NOTE: no error-checking on deflators

  UnitsFile <- getRunParameter("UnitsFile",Param_ls=Param_ls)
  UnitsFilePath <- findRuntimeInputFile(UnitsFile,Dir="ParamDir",Param_ls=Param_ls)
  newModelState_ls$Units <- read.csv(UnitsFilePath, as.is = TRUE)
  # NOTE: no error-checking on units

  GeoFile <- getRunParameter("GeoFile",Param_ls=Param_ls)
  GeoFilePath <- findRuntimeInputFile(GeoFile,Dir="ParamDir",Param_ls=Param_ls)
  Geo_df <- read.csv(GeoFilePath, colClasses="character")
  CheckResults_ls <- checkGeography(Geo_df)
  Messages_ <- CheckResults_ls$Messages
  if (length(Messages_) > 0) {
    writeLog(Messages_,Level="error")
    stop(paste0("One or more errors in ", GeoFilePath, ". See log for details."))
  } else {
    writeLog("Geographical indices successfully read.",Level="info")
  }
  newModelState_ls[ names(CheckResults_ls$Update) ] <- CheckResults_ls$Update;

  # Establish the ModelState in model.env (and optionally the ModelStateFilePath)
  newModelState_ls$FirstCreated <- Sys.time() # Timestamp
  newModelState_ls$RunParam_ls <- Param_ls
  model.env$ModelState_ls <- newModelState_ls
  model.env$RunParam_ls <- Param_ls; # Includes all the run Parameters, including "required"

  # Note that the ModelState is saved in the working directory
  # Model is expected to run in the directory that will receive its output.
  #   ModelState.Rda is the model description for this run
  #   Datastore are the model results for this run
  if ( Save) save("ModelState_ls", envir=model.env, file = getModelStateFileName(model.env$RunParam_ls))

  writeLog(paste0("Parameter Names from initModelState:\n",paste(names(model.env$RunParam_ls),collapse=",")),Level="info")

  return(Param_ls) 
}
#initModelState(ParamDir = "tests/defs")

#ARCHIVE MODEL STATE
#===================
#  Probably do not need to export
#' Move an existing model state into a different time-stamped file
#'
#' @param currentModelStatePath the path to the model state that is being archived (moved)
#' @param SaveParameters a named list of parameters, containing at a minimum previousTimestamp.
#' @return a copy of SaveParameters with name of the archived model state (archivedModelStateName)
#'   the updated previousTimestamp, and an updated flag savedPrevousModelState (TRUE if the
#'   previous model state was successfully archived).
#' @export
archiveModelState <- function(currentModelStatePath,SaveParameters) {

  # Get time stamp information from the previous model state if available
  previousModelState <- readModelState(FileName=currentModelStatePath,envir=new.env())

  # Get timestamp from previous ModelState
  # Use the current time if the ModelState was "blank"
  Timestamp <- if ( "FirstCreated" %in% previousModelState ) {
    previousModelState$FirstCreated
  } else if ( "LastChanged" %in% previousModelState ) {
    previousModelState$LastChanged
  } else {
    Timestamp <- NULL
  }
  if ( ! is.null(Timestamp) ) {
    SaveParameters$previousTimestamp <- gsub(":","-",gsub(" ", "_",Timestamp))
  }

  # Move the previous model state out of the way, but keep track of it
  #  as it may be resurrected later if we are re-loading a previous (possibly partial) model run
  SaveParameters$archivedModelStateName <- paste0("ModelState_",SaveParameters$previousTimestamp,".Rda")
  SaveParameters$savedPreviousModelState <- file.rename(currentModelStatePath, SaveParameters$archivedModelStateName)
  return(SaveParameters)
}

#ARCHIVE DATASTORE
#=================
#  Probably do not need to export
#' Move an existing model state into a different time-stamped file
#'
#' @param RunDstoreName the name of the Datastore (file/folder) in the working directory
#' @param SaveParameters a named list of parameters, containing at a minimum previousModelStateName
#'   and previousTimestamp
#' @return TRUE if all files were copied successfully, otherwise FALSE
#' @export
archiveDatastore <- function(RunDstoreName,SaveParameters) {
  # A previous model state should be associated with this Datastore, created by
  #   archiveModelState
  # Create a directory in which to save the datastore
  # Working directory is target of archive
  ArchiveDstoreName <- paste(RunDstoreName, SaveParameters$previousTimestamp, sep = "_")
  dir.create(ArchiveDstoreName)
  # Copy the datastore into the directory
  # Inside the archive folder, the Datastore itself and its model state
  #  have their original names, not timestamped names
  copy.datastore <- file.copy(RunDstoreName, ArchiveDstoreName, recursive = TRUE)
  #Copy the previous model state file into the directory as well
  copy.modelstate <- file.copy(SaveParameters$previousModelStateName,
    file.path(ArchiveDstoreName, getModelStateFileName()))
  invisible(!all(c(copy.datastore,copy.modelstate)))
}

#LOAD MODEL STATE
#================
#' Load ModelState into ve.model environment
#'
#' \code{loadModelState} a visioneval framework control function that
#' loads the ModelState for a model run into the ve.model environment.
#' Will replace any ModelState that is already there, so be cautious to
#' get the environment right, and to make sure that any existing ModelState
#' in memory is not unsaved (shouldn't be a problem inside a model run).
#'
#' @param FileName A string identifying the name of the file that contains
#' the ModelState_ls list. The default is the ModelStateFileName in getwd().
#' @param envir An environment into which to load ModelState.Rda
#' (default ve.model)
#' @return The RunParam_ls from the saved model state (or an empty list if not found)
#' @export
loadModelState <- function(FileName=getModelStateFileName(),envir=NULL) {
  if ( is.null(envir) ) envir = modelEnvironment()
  if (file.exists(FileName)) {
    load(FileName,envir=envir)
  }
  Param_ls <- get0( "RunParam_ls", envir=envir, ifnotfound=list() )
  if ( length(Param_ls) == 0 ) {
    ModelState_ls <- get0( "ModelState_ls", envir=envir, ifnotfound=list() )
    if ( length(ModelState_ls) > 0 ) {
      Param_ls <- ModelState_ls$RunParam_ls
    }
  }
  return ( Param_ls )
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
setModelState <- function(ChangeState_ls=list(), FileName = NULL, Save=TRUE) {
  # Get current ModelState
  ve.model=modelEnvironment()
  currentModelState_ls <- readModelState(FileName=FileName)

  # Make requested changes, if any
  # (pass an empty list just to Save existing ModelState_Ls)
  if ( length(ChangeState_ls) > 0 ) {
    # Replace names in currentModelState_ls with corresponding names in ChangeState_ls
    currentModelState_ls[ names(ChangeState_ls) ] <- ChangeState_ls
    currentModelState_ls$LastChanged <- Sys.time()
    ve.model$ModelState_ls <- currentModelState_ls
  }

  if ( Save ) {
    if ( is.null(FileName) ) FileName <- getModelStateFileName()
    result <- try(save("ModelState_ls",envir=ve.model,file=FileName))
    if ( class(result) == 'try-error' ) {
      Msg <- paste('Could not write ModelState:', FileName)
      writeLog(Msg,Level="error")
      writeLog(result,Level="error")
      stop(Msg,call.=FALSE)
    }
  } else if ( length(ChangeState_ls)==0 ) {
    writeLog("I'm just sitting here watching the wheels go round and round...",Level="trace")
    writeLogMessage(traceback(1),Level="trace")
  }
  invisible(TRUE)
}

#GET MODEL STATE FILENAME
#========================
#' Get model state file name
#'
#' \code{getModelStateFileName} a visioneval framework control function that
#' reports the model state file name for this model (from configuration)
#'
#' @param Param_ls a run parameter list to search (if NULL, the default, load the RunParam_Ls from
#    the modelEnvironment if that exists
#' @return a character string containing the ModelState file base name
#' @export
getModelStateFileName <- function(Param_ls=NULL) {
  # We'll look in envir$RunParam_ls for name and path information
  return(basename(getRunParameter("ModelStateFileName", Param_ls=Param_ls)))
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
  if ( !is.null(FileName) ) {
    if ( ! loadModelState(FileName,envir) ) {
      Msg <- paste("Could not load ModelState from",FileName)
      writeLog(c(Msg,getwd()),Level="error")
      writeLogMessage(.traceback(2))
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

#==================
#PARSE MODULE CALLS
#==================
#' parse the list of module calls from the ModelScriptFile
#'
#' Process the raw list of module calls and their parameters from the ModelScriptFile and expand
#' them into a detailee list of input/output specifications.
#'
#' @param ModuleCalls_df a list of module calls returned from \code{parseModelScript}
#' @param AlreadyInitialized a character vector of names of packages that
#'   would already have been initialized (if loading a pre-existing Datastore)
#' @param RequiredPackages a character vector of packages already required
#' @param Save if FALSE, just update ModelState in memory, otherwise write changes to disk
#' @return A list of all processed module specifications
#' @export
parseModuleCalls <- function( ModuleCalls_df, AlreadyInitialized="", RequiredPackages="", Save=TRUE ) {

  ModuleCalls_df <- unique(ModuleCalls_df)

  # Report any required packages that are not also in module calls
  umc <- unique(ModuleCalls_df$PackageName)
  explicitRequired_ <- unique(RequiredPackages)
  if ( any( not.in.umc <- ! (explicitRequired_ %in% umc) ) ) {
    for ( p in explicitRequired_[not.in.umc] ) {
      writeLog(paste("Package",p,"is required"),Level="info")
    }
  }
  RequiredPackages <- c(umc,explicitRequired_)

  #Get list of installed packages
  #Check that all module packages are in list of installed packages
  InstalledPkgs_ <- rownames(installed.packages())
  MissingPkg_ <- RequiredPackages[!(RequiredPackages %in% InstalledPkgs_)]
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
  for (Pkg in unique(setdiff(ModuleCalls_df$PackageName,AlreadyInitialized))) {
    PkgData <- data(package = Pkg)$results[,"Item"]
    if ("InitializeSpecifications" %in% PkgData) {
      Add_df <-
      data.frame(
        ModuleName = "Initialize",
        PackageName = Pkg,
        RunFor = "AllYears",
        RunYear = "Year"
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
      lapply(RequiredPackages, function(x) {
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
      RequiredVEPackages = RequiredPackages
    ),
    Save=Save
  )

  #Iterate through each module call and check availability and specifications
  #create combined list of all specifications
  # ModulesByPackage_df lists all modules available in the packages
  # ModuleCalls_df lists only modules that appear in runModule commands
  # AllSpecs_ls is a list of processed specifications, which is returned from the function
  Errors_ <- character(0)
  AllSpecs_ls <- list()
  for (i in 1:nrow(ModuleCalls_df)) {
    AllSpecs_ls[[i]] <- list()
    ModuleName <- ModuleCalls_df$ModuleName[i]
    AllSpecs_ls[[i]]$ModuleName <- ModuleName
    PackageName <- ModuleCalls_df$PackageName[i]
    AllSpecs_ls[[i]]$PackageName <- PackageName
    AllSpecs_ls[[i]]$RunFor <- ModuleCalls_df$RunFor[i]
    names(AllSpecs_ls)[i] <- paste0(PackageName,"::",ModuleName)
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

    #If any errors, print to log and stop execution
    if (length(Errors_) > 0) {
      Msg <-
      paste0("There are one or more errors in the module calls:\n",
        "package not installed, or module not present in package, ",
        "or errors in module specifications.")
      writeLog(c(Msg,Errors_),Level="error")
      stop(Msg," Check log for details")
    }
  }
  return(AllSpecs_ls)
}

#DOCUMENT A MODULE
#=================
#' Produces markdown documentation for a module
#'
#' \code{documentModule} a visioneval framework module developer function
#' that creates a vignettes directory if one does not exist and produces
#' module documentation in markdown format which is saved in the vignettes
#' directory.
#'
#' This function produces documentation for a module in markdown format. A
#' 'vignettes' directory is created if it does not exist and the markdown file
#' and any associated resources such as image files are saved in that directory.
#' The function is meant to be called within and at the end of the module
#' script. The documentation is created from a commented block within the
#' module script which is enclosed by the opening tag, <doc>, and the closing
#' tag, </doc>. (Note, these tags must be commented along with all the other
#' text in the block). This commented block may also include tags which identify
#' resources to include within the documentation. These tags identify the
#' type of resource and the name of the resource which is located in the 'data'
#' directory. A colon (:) is used to separate the resource type and resource
#' name identifiers. For example:
#' <txt:DvmtModel_ls$EstimationStats$NonMetroZeroDvmt_GLM$Summary>
#' is a tag which will insert text which is located in a component of the
#' DvmtModel_ls list that is saved as an rdata file in the 'data' directory
#' (i.e. data/DvmtModel_ls.rda). The following 3 resource types are recognized:
#' * txt - a vector of strings which are inserted as lines of text in a code block
#' * fig - a png file which is inserted as an image
#' * tab - a matrix or data frame which is inserted as a table
#' The function also reads in the module specifications and creates
#' tables that document user input files, data the module gets from the
#' datastore, and the data the module produces that is saved in the datastore.
#' This function is intended to be called in the R script which defines the
#' module. It is placed near the end of the script (after the portions of the
#' script which estimate module parameters and define the module specifications)
#' so that it is run when the package is built. It may not properly in other
#' contexts.
#'
#' @param ModuleName A string identifying the name of the module
#' (e.g. 'CalculateHouseholdDvmt')
#' @return None. The function has the side effects of creating a 'vignettes'
#' directory if one does not exist, copying identified 'fig' resources to the
#' 'vignettes' directory, and saving the markdown documentation file to the
#' 'vignettes' directory. The markdown file is named with the module name and
#' has a 'md' suffix.
#' @export
#' @import knitr
documentModule <- function(ModuleName){

  # Do not bother to re-run if in BUILD phase (only processed during
  # documentation/check step).
  if ( toupper(Sys.getenv("VE_BUILD_PHASE","SAVE")) != "SAVE" ) return()

  #Make vignettes directory if doesn't exist
  #-----------------------------------------
  if(!file.exists("inst/module_docs")) dir.create("inst/module_docs")

  #Define function to trim strings
  #-------------------------------
  trimStr <-
  function(String, Delimiters = NULL, Indices = NULL, Characters = NULL) {
    Chars_ <- unlist(strsplit(String, ""))
    if (!is.null(Delimiters)) {
      Start <- which(Chars_ == Delimiters[1]) + 1
      End <- which(Chars_ == Delimiters[2]) - 1
      Result <- paste(Chars_[Start:End], collapse = "")
    }
    if (!is.null(Indices)) {
      Result <- paste(Chars_[-Indices], collapse = "")
    }
    if (!is.null(Characters)) {
      Result <- paste(Chars_[!(Chars_ %in% Characters)], collapse = "")
    }
    Result
  }

  #Define function to split documentation into list
  #------------------------------------------------
  splitDocs <-
  function(Docs_, Idx_) {
    Starts_ <- c(1, Idx_ + 1)
    Ends_ <- c(Idx_ - 1, length(Docs_))
    apply(cbind(Starts_, Ends_), 1, function(x) Docs_[x[1]:x[2]])
  }

  #Define function to process documentation tag
  #--------------------------------------------
  #Returns list identifying documentation type and reference
  processDocTag <- function(DocTag) {
    DocTag <- trimStr(DocTag, Delimiters = c("<", ">"))
    DocTagParts_ <- unlist(strsplit(DocTag, ":"))
    if (length(DocTagParts_) != 2) {
      DocTag_ <- c(Type = "none", Reference = "none")
    } else {
      DocTag_ <- DocTagParts_
      names(DocTag_) <- c("Type", "Reference")
      DocTag_["Type"] <- tolower(DocTag_["Type"])
      if (!(DocTag_["Type"] %in% c("txt", "tab", "fig"))) {
        DocTag_["Type"] <- "none"
      }
    }
    DocTag_
  }

  #Define function to insert Rmarkdown for 'txt' tags
  #--------------------------------------------------
  insertTxtMarkdown <- function(Reference) {
    Object <- unlist(strsplit(Reference, "\\$"))[1]
    File <- paste0("data/", Object, ".rda")
    if (file.exists(File)) {
      load(File)
      if (!is.null(eval(parse(text = Reference)))) {
        Text_ <- eval(parse(text = Reference))
        Markdown_ <- c(
          "```",
          Text_,
          "```"
        )
        rm(list = Object)
      } else {
        return("Error in module documentation tag reference")
      }
    } else {
      return("Error in module documentation tag reference")
    }
    Markdown_
  }

  #Define function to insert Rmarkdown for 'fig' tags
  #--------------------------------------------------
  insertFigMarkdown <- function(Reference) {
    FromFile <- paste0("data/", Reference)
    ToFile <- paste0("inst/module_docs/", Reference)
    if (file.exists(FromFile)) {
      if (file.exists(ToFile)) file.remove(ToFile)
      file.copy(from = FromFile, to = ToFile)
      file.remove(FromFile)
      Markdown_ <- paste0("![", Reference, "](", Reference, ")")
    } else {
      return("Error in module documentation tag reference")
    }
    Markdown_
  }

  #Define function to insert Rmarkdown for 'tab' tags
  #--------------------------------------------------
  insertTabMarkdown <- function(Reference) {
    Object <- unlist(strsplit(Reference, "\\$"))[1]
    File <- paste0("data/", Object, ".rda")
    if (file.exists(File)) {
      load(File)
      if (!is.null(eval(parse(text = Reference)))) {
        Table_df <- eval(parse(text = Reference))
        ColNames <- colnames(Table_df)
        Markdown_ <- c(
          "",
          kable(
            eval(Table_df),
            format = "markdown",
            col.names = ColNames)
        )
        rm(list = Object, Table_df, ColNames)
      } else {
        return("Error in module documentation tag reference")
      }
    } else {
      return("Error in module documentation tag reference")
    }
    Markdown_
  }

  #Locate documentation portion of script and strip off leading comments
  #---------------------------------------------------------------------
  FilePath <- paste0("R/", ModuleName, ".R")
  Text_ <- readLines(FilePath)
  DocStart <- grep("#<doc>", Text_) + 1
  DocEnd <- grep("#</doc>", Text_) - 1
  Docs_ <-
  unlist(lapply(Text_[DocStart:DocEnd], function(x) trimStr(x, Indices = 1)))

  #Insert knitr code for inserting documentation tag information
  #-------------------------------------------------------------
  #Locate statistics documentation tags
  TagIdx_ <- grep("<", Docs_)
  #If one or more tags, split Docs_ and insert tag content
  if (length(TagIdx_) > 0) {
    #Split documentation into list of components before and after tags
    Docs_ls <- splitDocs(Docs_, TagIdx_)
    #Initialize new list into which knitr-processed tags will be inserted
    RevDocs_ls <- list(
      Docs_ls[1]
    )
    #Iterate through tags and insert knitr-processed tags
    #if there are any tags
    for (n in 1:length(TagIdx_)) {
      Idx <- TagIdx_[n]
      DocTag_ <- processDocTag(Docs_[Idx])
      if (DocTag_["Type"] == "none") {
        Markdown_ <- "Error in module documentation tag"
      }
      if (DocTag_["Type"] == "txt") {
        Markdown_ <- insertTxtMarkdown(DocTag_["Reference"])
      }
      if (DocTag_["Type"] == "fig") {
        Markdown_ <- insertFigMarkdown(DocTag_["Reference"])
      }
      if (DocTag_["Type"] == "tab") {
        Markdown_ <- insertTabMarkdown(DocTag_["Reference"])
      }
      RevDocs_ls <- c(
        RevDocs_ls,
        list(Markdown_),
        Docs_ls[n + 1]
      )
    }
  } else {
    RevDocs_ls <- list(
      list(Docs_)
    )
  }

  #Functions to assist in loading and processing module specifications
  #-------------------------------------------------------------------
  #Define function to process specifications
  processModuleSpecs_local <- function(Spec_ls) {
    #Define a function to expand a specification having multiple NAMEs
    expandSpec <- function(SpecToExpand_ls, ComponentName) {
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
      Out_ls$Inp <- processComponent(Spec_ls$Inp, "Inp")
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
  #Define a function to load the specifications
  loadSpecs <- function(ModuleName) {
    ModuleSpecs <- paste0(ModuleName, "Specifications")
    ModuleSpecsFile <- paste0("data/", ModuleSpecs, ".rda")
    if (file.exists(ModuleSpecsFile)) {
      load(ModuleSpecsFile)
      eval(parse(text = ModuleSpecs))
    } else {
      list()
    }
  }
  #Define function to creates a data frame from specifications Inp, Get, or Set
  makeSpecsTable <- function(ModuleName, Component, SpecNames_) {
    Specs_ls <- processModuleSpecs_local(loadSpecs(ModuleName))[[Component]]
    if (Component == "Inp") {
      Specs_ls <- lapply(Specs_ls, function(x) {
        if (!("OPTIONAL" %in% names(x))) {
          x$OPTIONAL <- FALSE
        }
        x
      })
      SpecNames_ <- c(SpecNames_, "OPTIONAL")
    }
    Specs_ls <- lapply(Specs_ls, function(x) x[SpecNames_])
    Specs_ls <- lapply(Specs_ls, function(x) {
      data.frame(lapply(x, function(y) {
        if (length(y) == 1) {
          y
        } else {
          paste(y, collapse = ", ")
        }
      }))
    })
    do.call(rbind, Specs_ls)
  }
  #Define function to break long strings into lines
  wordWrap <- function(WordString, MaxLength) {
    Words_ls <- list(
      "",
      unlist(strsplit(WordString, " "))
    )
    #Define recursive function to peel off first words from string
    getFirstWords <- function(Words_ls) {
      if (length(Words_ls[[2]]) == 0) {
        return(Words_ls[[1]][-1])
      } else {
        RemWords_ <- Words_ls[[2]]
        NumChar_ <- cumsum(nchar(RemWords_))
        NumChar_ <- NumChar_ + 1
        IsFirst_ <- NumChar_ < MaxLength
        AddString <-paste(RemWords_[IsFirst_], collapse = " ")
        RemWords_ <- RemWords_[!IsFirst_]
        getFirstWords(list(
          c(Words_ls[[1]], AddString),
          RemWords_
        ))
      }
    }
    paste(getFirstWords(Words_ls), collapse = "<br>")
  }

  #Insert documentation of user input files
  #----------------------------------------
  #Make a table of Inp specifications
  SpecNames_ <-
  c("NAME", "FILE", "TABLE", "GROUP", "TYPE", "UNITS", "PROHIBIT",
    "ISELEMENTOF", "UNLIKELY", "DESCRIPTION")
  InpSpecs_df <- makeSpecsTable(ModuleName, "Inp", SpecNames_)
  #Make markdown text
  if (!is.null(InpSpecs_df)) {
    InpMarkdown_ <- c(
      "",
      "## User Inputs",
      "The following table(s) document each input file that must be provided in order for the module to run correctly. User input files are comma-separated valued (csv) formatted text files. Each row in the table(s) describes a field (column) in the input file. The table names and their meanings are as follows:",
      "",
      "NAME - The field (column) name in the input file. Note that if the 'TYPE' is 'currency' the field name must be followed by a period and the year that the currency is denominated in. For example if the NAME is 'HHIncomePC' (household per capita income) and the input values are in 2010 dollars, the field name in the file must be 'HHIncomePC.2010'. The framework uses the embedded date information to convert the currency into base year currency amounts. The user may also embed a magnitude indicator if inputs are in thousand, millions, etc. The VisionEval model system design and users guide should be consulted on how to do that.",
      "",
      "TYPE - The data type. The framework uses the type to check units and inputs. The user can generally ignore this, but it is important to know whether the 'TYPE' is 'currency'",
      "",
      "UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.",
      "",
      "PROHIBIT - Values that are prohibited. Values may not meet any of the listed conditions.",
      "",
      "ISELEMENTOF - Categorical values that are permitted. Value must be one of the listed values.",
      "",
      "UNLIKELY - Values that are unlikely. Values that meet any of the listed conditions are permitted but a warning message will be given when the input data are processed.",
      "",
      "DESCRIPTION - A description of the data.",
      ""
    )
    InpSpecs_ls <- split(InpSpecs_df[, names(InpSpecs_df) != "FILE"], InpSpecs_df$FILE)
    FileNames_ <- names(InpSpecs_ls)
    for (fn in FileNames_) {
      InpMarkdown_ <- c(
        InpMarkdown_,
        paste("###", fn)
      )
      IsOptional <- unique(InpSpecs_ls[[fn]]$OPTIONAL)
      if (IsOptional) {
        InpMarkdown_ <- c(
          InpMarkdown_,
          "This input file is OPTIONAL.",
          ""
        )
      }
      SpecsTable_df <-
      InpSpecs_ls[[fn]][, !(names(InpSpecs_ls[[fn]]) %in% c("TABLE", "GROUP", "OPTIONAL"))]
      # SpecsTable_df$DESCRIPTION <-
      #   unname(sapply(as.character(SpecsTable_df$DESCRIPTION), function(x)
      #   {wordWrap(x, 40)}))
      Geo <- as.character(unique(InpSpecs_ls[[fn]]$TABLE))
      HasGeo <- Geo %in% c("Marea", "Azone", "Bzone", "Czone")
      Year <- as.character(unique(InpSpecs_ls[[fn]]$GROUP))
      if (HasGeo) {
        if (Year == "Year") {
          GeoYearDescription <- paste(
            "Must contain a record for each", Geo, "and model run year."
          )
        }
        if (Year == "BaseYear") {
          GeoYearDescription <- paste(
            "Must contain a record for each", Geo, "for the base year only."
          )
        }
        if (Year == "Global") {
          GeoYearDescription <- paste(
            "Must contain a record for each", Geo, "which is applied to all years."
          )
        }
      } else {
        GeoYearDescription <- paste(
          "Must contain a record for each model run year"
        )
      }
      if (Year == "Year") {
        Year_df <- data.frame(
          NAME = "Year",
          TYPE = "",
          UNITS = "",
          PROHIBIT = "",
          ISELEMENTOF = "",
          UNLIKELY = "",
          DESCRIPTION = GeoYearDescription
        )
        SpecsTable_df <- rbind(Year_df, SpecsTable_df)
      }
      if (HasGeo) {
        Geo_df <- data.frame(
          NAME = "Geo",
          TYPE = "",
          UNITS = "",
          PROHIBIT = "",
          ISELEMENTOF = paste0(Geo, "s"),
          UNLIKELY = "",
          DESCRIPTION = GeoYearDescription
        )
        InpMarkdown_ <- c(
          InpMarkdown_,
          kable(rbind(Geo_df, SpecsTable_df))
        )
      } else {
        InpMarkdown_ <- c(
          InpMarkdown_,
          kable(SpecsTable_df)
        )
      }
    }
  } else {
    InpMarkdown_ <- c(
      "",
      "## User Inputs",
      "This module has no user input requirements."
    )
  }
  #Add the markdown text to the documentation list
  RevDocs_ls <- c(
    RevDocs_ls,
    list(InpMarkdown_)
  )

  #Insert documentation of module inputs
  #-------------------------------------
  #Make a table of Get specifications
  SpecNames_ <-
  c("NAME", "TABLE", "GROUP", "TYPE", "UNITS", "PROHIBIT", "ISELEMENTOF")
  GetSpecs_df <- makeSpecsTable(ModuleName, "Get", SpecNames_)
  #Make markdown text
  if (!is.null(GetSpecs_df)) {
    GetMarkdown_ <- c(
      "",
      "## Datasets Used by the Module",
      "The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:",
      "",
      "NAME - The dataset name.",
      "",
      "TABLE - The table in the datastore that the data is retrieved from.",
      "",
      "GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year group. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.",
      "",
      "TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.",
      "",
      "UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.",
      "",
      "PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.",
      "",
      "ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.",
      ""
    )
    SpecsTable_df <- GetSpecs_df
    GetMarkdown_ <- c(
      GetMarkdown_,
      kable(SpecsTable_df)
    )
  } else {
    GetMarkdown_ <- c(
      "",
      "## Datasets Used by the Module",
      "This module uses no datasets that are in the datastore."
    )
  }
  #Add the markdown text to the documentation list
  RevDocs_ls <- c(
    RevDocs_ls,
    list(GetMarkdown_)
  )

  #Insert documentation of module outputs
  #--------------------------------------
  #Make a table of Set specifications
  SpecNames_ <-
  c("NAME", "TABLE", "GROUP", "TYPE", "UNITS", "PROHIBIT", "ISELEMENTOF", "DESCRIPTION")
  SetSpecs_df <- makeSpecsTable(ModuleName, "Set", SpecNames_)
  #Make markdown text
  if (!is.null(SetSpecs_df)) {
    SetMarkdown_ <- c(
      "",
      "## Datasets Produced by the Module",
      "The following table documents each dataset that is placed in the datastore by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:",
      "",
      "NAME - The dataset name.",
      "",
      "TABLE - The table in the datastore that the data is placed in.",
      "",
      "GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.",
      "",
      "TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.",
      "",
      "UNITS - The native units that are created in the datastore. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.",
      "",
      "PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.",
      "",
      "ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.",
      "",
      "DESCRIPTION - A description of the data.",
      ""
    )
    SpecsTable_df <- SetSpecs_df
    # SpecsTable_df$DESCRIPTION <-
    #   unname(sapply(as.character(SpecsTable_df$DESCRIPTION), function(x)
    #     {wordWrap(x, 40)}))
    SetMarkdown_ <- c(
      SetMarkdown_,
      kable(SpecsTable_df)
    )
  } else {
    SetMarkdown_ <- c(
      "",
      "## Datasets Produced by the Module",
      "This module produces no datasets to store in the datastore."
    )
  }
  #Add the markdown text to the documentation list
  RevDocs_ls <- c(
    RevDocs_ls,
    list(SetMarkdown_)
  )

  #Produce markdown file documentation
  #-----------------------------------
  writeLines(unlist(RevDocs_ls), paste0("inst/module_docs/", ModuleName, ".md"))
}

#READ GEOGRAPHIC SPECIFICATIONS
#==============================
#' Read geographic specifications.
#'
#' \code{readGeography} a visioneval framework model developer function that reads the
#' geographic specifications file for the model.
#'
#' This function is not used when running a model: it is only intended for debugging geographic
#' file specifications during model development. See initModelState for identical code that
#' loads the geography along with units and deflators
#'
#' This function manages the reading and error checking of geographic specifications for the model.
#' It calls the checkGeography function to check for errors in the specifications. The
#' checkGeography function reads in the file and checks for errors. That function returns a list of
#' any errors that are found and a data frame containing the geographic specifications. If errors
#' are found, write the errors to a log file and stops model execution. If there are no errors, the
#' function adds the geography in the geographic specifications file, the errors are written to the
#' log file and execution stops. If no errors are found, the geographic specifications are added to
#' the model state file.
#'
#' @param Save A logical (default=TRUE) indicating whether the model state
#'   should be saved to the model state file, or just updated in ve.model environment
#' @param Param_ls a list of parameters
#'   the file and the specifications are consistent. It stops if there are any
#'   errors in the specifications. All of the identified errors are written to
#'   the run log. A data frame containing the file entries is added to the
#'   model state file as Geo_df'.
#' @return The value TRUE is returned if the function is successful at reading
#' @export
readGeography <- function(Save=TRUE,Param_ls=NULL) {
  #Check for errors in the geographic definitions file

  # Load model environment (should have RunParam_ls already loaded, furnishing ...)
  if ( ! is.list(Param_ls) ) {
    model.env <- modelEnvironment()
    Param_ls <- model.env$RunParam_ls
  }

  #Read in geographic definitions if file exists, otherwise error
  #--------------------------------------------------------------
  GeoFile <- getRunParameter("GeoFile",Param_ls=Param_ls)
  GeoFilePath <- findRuntimeInputFile(GeoFile,Dir="ParamDir",Param_ls=Param_ls)
  Geo_df <- read.csv(GeoFilePath, colClasses="character")
  attr(Geo_df,"file") <- GeoFilePath
  CheckResults_ls <- checkGeography(Geo_df)

  #Notify if any errors
  Messages_ <- CheckResults_ls$Messages
  if (length(Messages_) > 0) {
    writeLog(Messages_,Level="error")
    stop(paste0("One or more errors in ", GeoFilePath, ". See log for details."))
  } else {
    writeLog("Geographical indices successfully read.",Level="info")
  }

  #Update the model state file with loaded and checked geography
  setModelState(CheckResults_ls$Update,Save=Save)
  return(TRUE)
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
  missing <- ! (FieldNames_ %in% names(Geo_df))
  if ( any(missing) ) {
    Message <- paste(attr(Geo_df,"file"),"is missing required fields:",paste(FieldNames_[missing],collapse=", "))
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
#' group. If FlagChanges is TRUE, don't do the creation - just report consistency.
#'
#' @param FlagChanges logical (default=FALSE); if TRUE, inspect the model parameters
#'   and see if what is in the Datastore is consistent.
#' @return The function returns TRUE if the model parameters file exists and
#' its values are sucessfully written to the datastore. If FlagChanges, do
#' not write to the Datastore but instead return FALSE if they are different in some
#' way.
#' @export
loadModelParameters <- function(FlagChanges=FALSE) {
  G <- getModelState()
  RunParam_ls <- G$RunParam_ls;
#   Commented out: Default parameters will suffice
#   ModelParamInfo <- c("ParamDir","ModelParamFile")
#   missingParams <- ! ModelParamInfo %in% names(RunParam_ls)
#   if ( any(missingParams) ) {
#     stop(
#       writeLog(
#         paste(
#           "Missing parameter names:",
#           paste(ModelParamInfo[missingParams],collapse=",")
#         ),
#         Level="error"
#       )
#     )
#   }
  writeLog("Loading model parameters file.",Level="info")
  ModelParamFile <- getRunParameter("ModelParamFile",Param_ls=RunParam_ls)
  ParamFile <- findRuntimeInputFile(ModelParamFile,"ParamDir",Param_ls=RunParam_ls,StopOnError=FALSE)
  if ( is.na(ParamFile) ) {
    # Not Found: Try again looking this time in InputDir (element of input path)
    ParamFile <- findRuntimeInputFile(ModelParamFile,"InputDir",Param_ls=RunParam_ls,StopOnError=FALSE)
    if ( is.na(ParamFile) ) {
      # Still Not Found: Throw an error
      ErrorMsg <- paste0(
        "Model parameters file (",
        ParamFile,
        ") could not be located in ",
        paste(RunParam_ls$ParamDir,RunParam_ls$InputDir,collapse=", ")
      )
      stop( writeLog(ErrorMsg,Level="error") )
    } else {
      ParamFile <- ParamFile[1] # may have multiple InputPaths; pick the first found file
    }
  }

  Param_df <- jsonlite::fromJSON(ParamFile)
  if ( ! FlagChanges ) {
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
      result <- writeToTable(Value, Spec_ls, Group = "Global", Index = NULL)
    }
  } else {
    Warnings_ <- character(0)
    for (i in 1:nrow(Param_df)) {
      name <- Param_df$NAME[i]
      item <- try (
        # readFromTable stops if name not found
        # we'll turn that into a warning
        readFromTable(name,Table="Model",Group="Global"),
        silent = TRUE
      )
      if ( class(item)=="try-error" ) {
        Warnings_ <- c(
          Warnings_,
          paste("Previous model parameter",name,"failed to load"),
          as.character(item)
        )
        next
      }
      atts <- attributes(item)
      Type <- Param_df$TYPE[i]
      if ( atts$TYPE != Type ) {
        Warnings_ <- c(Warnings_,
          paste("Model parameter",name," has inconsistent TYPE."),
          paste("Datastore:",atts$TYPE,"versus this model:",Type)
        )
      }
      Value <- if (Type == "character") Param_df$VALUE[i] else as.numeric(Param_df$VALUE[i])
      if ( item != Value ) {
        Warnings_ <- c(Warnings_,
          paste("Model parameter",name," has inconsistent VALUE."),
          paste("Datastore:",Value,"versus this model:",item)
        )
      }
      if ( atts$UNITS != Param_df$UNITS[i] ) {
        Warnings_ <- c(Warnings_,
          paste("Model parameter",name," has inconsistent UNITS."),
          paste("Datastore:",atts$UNITS,"versus this model:",Param_df$UNITS[i])
        )
      }
    }
    if (length(Warnings_) != 0) {
      writeLog(Warnings_,Level="warn")
      result <- FALSE
    } else result <- TRUE
  }
  return(result)
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
  writeLog(c("Parsing model script",FilePath),Level="info")
  if (!file.exists(FilePath)) {
    Msg <- c(
      paste0("Specified model script file does not exist."),
      FilePath
    )
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
      function(x) normalizeElementFields(x$runScript,ScriptCallNames)
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

  writeLog("Done parsing model script",Level="info")
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

#PROCESS INPUT FILES
#===================
#' Locate and Load Input Files to Datastore
#'
#' \code{processInputFiles} will process the model specification list and load
#' any input files that are required
#'
#' @param AllSpecs_ls a list of model specifications (from parseModuleCalls)
#' @return None
#' @export
processInputFiles <- function(AllSpecs_ls) {
  #Set up a list to store processed inputs for all modules
  ve.model <- modelEnvironment()
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
