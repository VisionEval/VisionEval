# Author: Jeremy Raw

# VEModel Package Code

#####################
# RUNTIME ENVIRONMENT
#####################

# The ve.env environment is accessed via VEModel::runtimeEnvironment. It holds the RunParam_ls
# settings from the runtime directory, which is either the current directory or specified in a
# system environment variable VE_RUNTIME; See VEModel::readConfiguration.

ve.env <- new.env()
ve.env$RunParam_ls <- list()

#ACCESS R ENVIRONMENT FOR MODEL RUN
#==================================
#' Access an R environment for the runtime installation.
#'
#' \code{modelEnvironment} returns an environment for tracking default runtime parameters and other
#' runtime needs (e.g. queries). That environment contains the default RunParams_ls structure plus other
#' components needed to manage global state for VEModel classes.
#'
#' @return an R environment "ve.env"
#' @export
runtimeEnvironment <- function() ve.env

# Set up package defaults for VE getRunParameter

default.parameters.table = list(
  ModelRoot           = "models",
  ModelScript         = "run_model\\.R",
  ModelScriptFile     = "run_model.R",
  ConfigDir           = "",  # Holds global display_units.csv (relative to ve.runtime)
  ResultsDir          = "results",
  OutputDir           = "output",
  QueryDir            = "queries",
  DisplayUnitsFile    = "display_units.csv",
  QueryFileName       = "New-Query.VEqry",
  QueryDir            = "queries",
  QueryOutputTemplate = "Measures_%scenario%_%years%_%geography%.csv"
)

#GET DEFAULT PARAMETERS
#======================
#' Hook to add default values for key parameters in the VEModel package
#'
#' \code{VEPackageRunParameters} extends \code{visioneval::defaultVERuntimeParameters} to
#' provide additional Parameters and Defaults for VEModel functions that can be accessed '
#' seamlessly. You can call this function directly (e.g. to see what parameters are defined and '
#' defaulted in VEModel). Internally VEModel uses \code{visioneval::defaultVERuntimeParameters},
#' so it doesn't have to remember the place specific defaults are defined.
#'
#' @param Param_ls a list (possibly empty) of already-defined parameters
#' @return a named list for parameters not present in Param_ls containing default values for those
#'   parameters
#' @export
VEPackageRunParameters <- function(Param_ls=list()) {
  defaultParams_ls <- default.parameters.table[
    which( ! names(default.parameters.table) %in% names(Param_ls) )
  ]
  if ( length(defaultParams_ls)>0 ) {
    defaultParams_ls <- visioneval::addParameterSource(defaultParams_ls,"Package VEModel Default")
    Param_ls <- visioneval::mergeParameters(defaultParams_ls,Param_ls) # Param_ls will override
  }
  return(Param_ls)
}

#LOAD RUNTIME CONFIGURATION
#==========================
#' Load a VisionEval.cnf file from a directory into the runtime environment
#'
#' \code{loadRuntimeConfig} merges a configuration file from a directory into the runtime R
#' environment. With no parameters it will read and merge the file's contents into the system
#' runtime paramters list (so call it after changing the configuration file to bring the in-memory
#' version up to date). See \code{visioneval::loadConfiguration} for more details.
#'
#' @param ParamDir is the directory in which to seek visioneval.cnf
#' @param ParamFile is a specific file to seek (rather than looking for one of the default file
#'   names). Note that an error will be raised if that file is specified but does not exist.
#' @return The updated runtime parameters list (which is also modified in place)
#' @export
loadRuntimeConfig <- function(ParamDir=NULL,ParamFile=NULL) {
  # Function loads configuration from ParamDir/VisionEval.cnf
  # ParamDir defaults to ve.runtime if defined, else getwd()
  # override is a list whose elements may be replaced by this configuration
  # keep is a list whose elements will take precedence over this configuration
  ve.env <- runtimeEnvironment()
  if ( is.null(ve.env$ve.runtime) ) setRuntimeDirectory() # VE_RUNTIME or getwd()
  if ( is.null(ParamDir) ) ParamDir <- ve.env$ve.runtime
  if ( ! exists("RunParam_ls",envir=ve.env,inherits=FALSE) ) ve.env$RunParam_ls <- list()
  ve.env$RunParam_ls <- visioneval::loadConfiguration(ParamDir=ParamDir,ParamFile=ParamFile,override=ve.env$RunParam_ls)
  return( ve.env$RunParam_ls )
}

#GET RUNTIME PARAMETERS
#======================
#' Return runtime base RunParam_ls (loading it if not present)
#'
#' \code{getRuntimeParameters} merges a configuration file from a directory into the runtime R
#' environment. With no parameters it will read and merge the file's contents into the system
#' runtime paramters list (so call it after changing the configuration file to bring the in-memory
#' version up to date). See \code{visioneval::loadConfiguration} for more details.
#'
#' @param paramNames is a character vector of parameter names identifying a subset of runParameters
#'   to retrieve
#' @return A list of defined run parameters (or an empty list if none, or if no paramNames are
#'   found)
#' @export
getRuntimeParameters <- function(paramNames=NULL) {
  RunParams_ls <- if ( is.null(ve.env$RunParam_ls) ) ve.env$RunParam_ls else loadRuntimeConfig()
  if ( is.character(paramNames) ) RunParams_ls <- RunParams_ls[names(RunParams_ls) %in% paramNames]
  return(RunParams_ls)
}

# Set the VE runtime directory; for now, only used internally on package startup
# Could export to allow moving to a different runtime location
#' Set the runtime direcdtory
#'
#' @param Directory a specific directory (relative to getwd()) to use as the runtime
#' @return The directory that has been selected as the runtime
#' @export
setRuntimeDirectory <- function(Directory=NULL) {
  if ( is.null(Directory) ) {
    Directory <- if ( ! exists("ve.runtime",envir=ve.env,inherits=FALSE) ) {
      Sys.getenv("VE_RUNTIME",unset=getwd())
    } else ve.env$ve.runtime
  } else {
    Directory <- normalizePath(Directory,winslash="/",mustWork=FALSE)
    if ( ! dir.exists(Directory) ) {
      Directory <- getwd()
    }
  }
  # returns the working directory from before this call
  ve.env$start.dir <- getwd()
  if ( getwd() != Directory ) {
    setwd(Directory)
  }
  return( ve.env$ve.runtime <- getwd() )
}

# Get the runtime directory (ve.runtime)
#' Get the runtime directory
#' Return the runtime directory established when VEModel package is loaded or by a later call to
#'   \code{setRuntimeDirectory}. If the runtime directory has not yet been set, set it to the
#'   working directory.
#' @return The ve.runtime directory from the package environment, ve.env
#' @export
getRuntimeDirectory <- function() {
  Directory <- ve.env$ve.runtime
  if ( is.null(Directory) ) Directory <- setRuntimeDirectory()
  return(Directory)
}

# INTERNAL FUNCTION TO SET BASIC RUN PARAMETERS
#==============================================
# Set up a Runtime environment consistently by providing
# model-run-specific parameters (see list of parameters)
# Used internally when loading ModelState files and running Models. A specific model's
# configuration may replace these with something else.
# TODO: review the process with the new VEModel::ve.env environment and its RunParam_ls
ve.model.setupRunEnvironment <- function(
  Owner,
  PreviousState=list(),
  Param_ls=list(),
  RunModel=FALSE,
  ModelDir=getwd(),
  ResultsDir=".",       # Should be relative to ModelDir
  InputPath=".",        # Should be relative to ModelDir
  ModelScriptFile=NULL,
  LogLevel="warn"
) {
  # Set up ve.model environment with run parameters
  ve.model <- visioneval::modelEnvironment(Clear=Owner)
  ve.model$RunModel <- RunModel
  ve.model$ModelStateStages <- PreviousState; # previously loaded model states
  addParams_ls <- list(
    ResultsDir      = ResultsDir,
    ModelDir        = ModelDir,
    InputPath       = InputPath,
    ModelScriptFile = ModelScriptFile,
    LogLevel        = LogLevel
  )
  addParams_ls <- visioneval::addParameterSource(addParams_ls,paste0("Set up RunParam_ls for ",Owner))
  ve.model$RunParam_ls <- visioneval::mergeParameters(Param_ls,addParams_ls) # addParams_ls will override

  invisible(ve.model$RunParam_ls)
}

# FUNCTION TO MAKE A UNIQUE FILE NAME
#====================================
#  Get unique file name based on newName in folder newPath
#  NewPath is the directory it should go in, newName is the name to disambiguate
getUniqueName <- function(newPath,newName) {
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  return (newModelPath)
}

