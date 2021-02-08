#================
#environment.R
#================

#This script defines functions used to establish the model
#environment and configuration parameters.

# Experimental approach to "ve.model" environment
ve.model <- new.env()

#ACCESS R ENVIRONMENT FOR MODEL RUN
#==================================
#' Access an R environment for the active model
#'
#' \code{modelEnvironment} a visioneval framework control function that locates the environment for
#' elements of the active model. That environment contains the ModelState_ls structure plus other
#' components needed to keep track of a running model.
#'
#' @param Clear if supplied as a non-empty character string, sets the environment owner and clears
#'   the model environment. If an empty string and no Owner, clear the environment, and if Owner
#'   just clear the Owner.
#' @return an R environment for "ve.model"
#' @export
modelEnvironment <- function(Clear=NULL) {
  # export this function since it can be useful in the VEModel
  # package
  if ( is.character(Clear) ) {
    if ( nzchar(Clear) ) { # optionally set owner flag
      rm(list=ls(ve.model),envir=ve.model)
      ve.model$Owner <- Clear
    } else {
      # suppress ve.model initialization if "owned"
      if ( is.null(ve.model$Owner) ) { # probably called initializeModel from "source"
        rm(list=ls(ve.model),envir=ve.model)
      } else {
        writeLog(paste0("ve.model NOT cleared due to Owner: ",ve.model$Owner),Level="debug")
        rm("Owner",envir=ve.model) # Owner protection is now over.
      }
    }
  }
  return(ve.model)
}

#IS MODEL RUNNING?
#=================
#' Check if model is running
#'
#' \code{modelRunning} returns TRUE if the model is running; otherwise
#' the run_model.R script is executed as a "dry run".
#'
#' @return TRUE if ve.model$RunModel is TRUE, otherwise FALSE
#' @export
modelRunning <- function() {
#   if ( ! "ve.model" %in% search() ) { # have not performed initializeModel
#     return(FALSE)
#   } else {
#     ve.model <- as.environment("ve.model")
    if ( "RunModel" %in% ls(ve.model) ) { # VEModel will set during "open" or "run" model
      return(ve.model$RunModel)
    } else { # The model is running in backward-compatible mode
      return(ve.model$RunModel <- TRUE)
    }
#   }
}

#GET VISIONEVAL RUN PARAMETER
#============================
#'
#' \code{getRunParameter} a visioneval control function to find the current definition for
#' a runtime parameter, using the currently loaded specification. May be called as the
#' configurations are being loaded, so earlier site configuration can help find later
#' locations for additional configuration information (e.g. model run_parameters in a
#' non-standard model directory).
#'
#' Since cascading run parameters sometimes make it hard to determine where a parameter value
#' came from, you can use the logSource=TRUE option to figure out where any particular parameter
#' was defined.  Just run this command from your console after you have run your model (or after
#' it crashed):
#' \code{
#' visioneval::getRunParameter("BadParameter",Default="Default",ve.model$RunParam_ls,logSource=TRUE)
#' }
#' You can also replace ve.model$RunParam_ls with some other list of run parameters (e.g. the
#' RunParam_ls froma ModelState loaded in a VEModel object). To view the entire set of sources for a
#' list of run parameters, just retrieve \code{attr(RunParam_ls,"source")}. see
#' readConfigurationFile for more information on the "source" attribute of run parameters.
#'
#' @param Parameter character vector (length one) naming Parameter to retrieve
#' @param Param_ls a list of run parameters to look within (otherwise RunParam_ls from
#' "ve.model" environment if it exists, and if not just an empty list.
#' @param Default is the value to provide if Parameter is not found in RunParam_ls
#' @param logSource a logical (default=FALSE); if TRUE, write an info-level log message reporting the
#' source for the parameter value
#' @return A parameter value or its default if not set
#' @export
getRunParameter <- function(Parameter,Param_ls=NULL,Default=NA,logSource=FALSE) {
  param.source <- NULL
  defaultParams_ls <- list()
  defaultMissing <- missing(Default)

  # We're going to use ve.model to cache the default parameters
  ve.model <- modelEnvironment()
  defaultParams_ls <- if ( ! is.null(ve.model$VERuntimeDefaults) ) {
    ve.model$VERuntimeDefaults
  } else {
    ve.model$VERuntimeDefaults <- defaultVERunParameters(defaultParams_ls)
  }
  if ( ! is.list(Param_ls) ) { # look only there
    param.source <- "ve.model$RunParam_ls"
    Param_ls <- get0("RunParam_ls",envir=ve.model,ifnotfound=list())
  }
  if ( is.null(attr(Param_ls,"source")) ) {
    if ( is.null(param.source) ) {
      param.source <- paste("Unknown, called from:",as.character(.traceback(2)),collapse=", ")
    }
    Param_ls <- addParameterSource(Param_ls,param.source)
  }
  if ( length(Param_ls)==0 || ! Parameter %in% names(Param_ls) ) {
    if ( logSource ) writeLog(
      paste(Parameter,"using Default, called from:",as.character(.traceback(2)),collapse=", "),
      Level="debug"
    )
    if ( defaultMissing && Parameter %in% names(defaultParams_ls) ) {
      Default <- defaultParams_ls[[Parameter]]
    }
    return( Default )
  } else {
    if ( logSource ) {
      sources <- attr(Param_ls,"source")
      if ( is.null(sources) ) {
        writeLog(
          paste("Unknown source for",Parameter,"at",as.character(.traceback(2)),collapse=", "),
          Level="debug"
        )
      } else {
        writeLog(
          paste("Source for",Parameter,":",sources[Parameter,"Source"]),
          Level="debug"
        )
      }
    }
    return( Param_ls[[Parameter]] )
  }
}

# These are default parameters used by the framework functions
# e.g. in intializeModel or initModelSTate.
default.parameters.table = list(
  Seed = 1,
  LogLevel = "error",
  DatastoreName = "Datastore",
  ModelDir = ".", # subdirectory containing run_model.R (e.g. "script")
  ModelScriptFile = "run_model.R",
  ModelStateFileName = "ModelState.Rda",
  InputPath = ".", # should default to same directory as ModelDir
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  UnitsFile = "units.csv",
  DeflatorsFile = "deflators.csv",
  ModelParamFile = "model_parameters.json",
  InputDir = "inputs",
  ParamDir = "defs",
  ResultsDir = "results",
  OutputDir = "output",
  QueryDir = "queries",
  DatastoreType = "RD",
  SaveDatastore = FALSE
)

#GET DEFAULT PARAMETERS
#======================
#' Report useful default values for key parameters
#'
#' \code{defaultVERuntimeParameters} is a visioneval model developer function takes an (optional,
#' default empty) list of RunParams and reports the default values for any parameter not already in
#' that list. It will look in attached VisionEval packages (names starting with
#' \code{"package:VE..."} for an exported defaultVERunParameters function, and those will be given
#' priority when loading (with the most recently loaded packages having the highest priority).
#' \code{getRunParameter} handles that seamlessly if no inline default is provided.
#'
#' @param Param_ls a list (possibly empty) of already-defined parameters
#' @return a named list for parameters not present in Param_ls containing default values for those
#'   parameters
#' @export
defaultVERunParameters <- function(Param_ls=list()) {
  defaultParams_ls <- list()
  otherVEDefaults <- grep("^package:VE",find("VEPackageRunParameters",mode="function"),value=TRUE)
  if ( length(otherVEDefaults)>0 ) {
    for ( defs in otherVEDefaults[length(otherVEDefaults):1] ) {
      # process in reverse order so most recently loaded packages "win" any collisions
      packageParams_ls <- as.environment(defs)$VEPackageRunParameters(Param_ls)
      defaultParams_ls <- mergeParameters(defaultParams_ls,packageParams_ls)
    }
  }
  Param_ls <- mergeParameters(defaultParams_ls,Param_ls)
  
  # Now add the framework defaults (packages take precedence)
  tableParams_ls <- default.parameters.table[
    which( ! names(default.parameters.table) %in% names(Param_ls) )
  ]
  if ( length(tableParams_ls)>0 ) {
    tableParams_ls <- addParameterSource(tableParams_ls,"VisionEval Framework Default")
    Param_ls <- mergeParameters(tableParams_ls,Param_ls)
  }
  return(Param_ls)
}

#READ CONFIGURATION FILE
#=======================
#' Read a run parameters configuration file.
#'
#' \code{readConfigurationFile} a visioneval control function to load a list of Run Parameters from
#' a configuration file. The files are in YAML or in JSON format.
#'
#' @section Details:
#'
#' If the ParamFile name is not explicitly provided, the following names are sought in this order,
#' and the first one found in ParamDir will be processed. Names are case-insensitive, so if you are
#' running on a case-sensitive file system (e.g. Mac or Linux) and you have names that are only
#' distinguished by capitalization (e.g. visioneval.cnf versus VisionEval.cnf), you will get the
#' first file determined by collation order of the default R \code{dir} function). Possible names
#' include:
#' 
#' \itemize{
#'     \item{VisionEval.yml}
#'     \item{VisionEval.json}
#'     \item{VisionEval.cnf}
#'     \item{VisionEval.ini}
#' }
#'
#' If the file has a \code{.yml} or \code{.json} extension, it will be processed as that type.
#' {VisionEval.cnf} or {.visioneval}, with no type extension, will be processed first as .json, and
#' if that fails, then as .yml. If both fail, a format error is returned with error messages from
#' each failed format type (since we can't always tell what format you were intending to use).
#'
#' The returned list also has a "source" attribute, which is a dataframe with columns "Name" and
#' "Source" and a row for each run parameter that is included in the returned list. See
#' \code{updateParameterSource}. Inspecting this attribute will help debug wrong parameter settings
#' (e.g. if a run parameter is overridden by an unexpected value). If you run the initializeModel
#' function with the RunModel=FALSE option, the ModelState_ls that is generated will contain a
#' complete set of run parameters, and you can inspect their source.
#'
#' @param ParamDir (optional) directory in which to seek a configuration file
#' @param ParamFile (optional) is the name of a file to seek. If it is not found, throw
#' an error.
#' @param mustWork (default FALSE) if TRUE, and ParamFile is provided, throw an error
#'   if ParamDir/ParamFile does not exist. Otherwise, return an empty list if no file is found.
#' @return A list of Run Parameters. If ParamFile is supplied and it does not exist in
#' ParamDir, throw an error. If ParamFile is not supplied and no configuration file is
#' found, return an empty list. Otherwise return the list of parameters found in the file.
#' A 'source' attribute is also constructed if the list is not empty (see Details).
#' @export
readConfigurationFile <- function(ParamDir=NULL,ParamFile=NULL,mustWork=FALSE) {

  # Initialize the parameter list
  ParamFile_ls <- list()

  # ParamFile is ignored if ParamDir is not provided
  if ( is.null(ParamDir) && !is.null(ParamFile) ) ParamFile <- NULL
  
  # Otherwise, check for candidate files and return an empty list if none found in ParamDir
  if ( is.null(ParamFile) ) {
    if ( ! is.null(ParamDir) ) {
      ParamFiles   <- c("visioneval.yml","visioneval.json","visioneval.cnf","visioneval.ini")
      ParamPattern <- sapply(ParamFiles,function(x) paste0("(",x,")"))
      ParamPattern <- paste("^",paste(ParamPattern,collapse="|"),"$",sep="")
      ParamPattern <- gsub("\\.","\\.",ParamPattern) # quote literal periods in file name candidates
      candidates <- dir(ParamDir,pattern=ParamPattern,ignore.case=TRUE) # find any matching files
      candidates <- candidates[ tolower(candidates) %in% ParamFiles ] # figure out which we found
    } else {
      candidates <- character(0)
    }
    if ( length(candidates)==0 ) {
      ParamFile <- NULL
    } else {
      ParamFile <- candidates[1]
    }
  }
  if ( all(!is.null(c(ParamDir,ParamFile))) ) {
    if ( length(ParamDir)>1 ) {
      stop(
        writeLog(
          c(
            "Error: ParamDir has multiple components in readConfigurationFile:\n",
            paste(ParamDir,"\n")
          ),
          Level="error"
        )
      )
    }
    ParamFileExists <- FALSE
    if ( ! is.null(ParamFile) ) {
      ParamFilePath <- unique(file.path(ParamDir,ParamFile))
      ParamFileExists <- file.exists(ParamFilePath)
    } else {
      ParamFilePath <- ParamDir
    }

    if ( ! ParamFileExists ) {
      if ( mustWork ) {
        stop(
          writeLog(
            paste("Could not locate Parameter File:",ParamFilePath),
            Level="error"
          )
        )
      }
      # else fall through with ParamFile_ls an empty list
    } else {
      ParamFile_ls <- withRestarts(
        json = function(path) {
          tryCatch(
            {
              writeLog(c("Trying JSON parameters from",path),Level="info")
              jsonlite::fromJSON(path)
            },
            error = function(e) {
              writeLog("Failed to read JSON file",Level="info")
              writeLog(paste("JSON:",conditionMessage(e)),Level="info")
              return(list())
            },
            warning = function(w) {
              formatWarnings <- writeLog(c("Warning reading JSON:",conditionMessage(w)),Level="info")
            }
          )
        },
        tryCatch(
          {
            writeLog(c("Trying YAML parameters from",ParamFilePath),Level="info")
            yaml::yaml.load_file(ParamFilePath)
          },
          error = function(e) {
            writeLog("Failed to read YAML file; retrying as JSON...",Level="info")
            writeLog(paste("YAML:",conditionMessage(e)),Level="info")
            invokeRestart("json",ParamFilePath)
          },
          warning = function(w) {
            formatWarnings <- writeLog(c("Warning reading YAML:",conditionMessage(w)),Level="info")
          }
        )
      )
      if ( length(ParamFile_ls)==0 ) {
        writeLog(c("No parameters in loaded file:",ParamFilePath),Level="warn")
        writeLog("Make another attempt with log Level='info' for details",Level="warn")
      }
    }
    if ( length(ParamFile_ls)>0 && ! all(nzchar(names(ParamFile_ls))) ) {
      stop( paste(
        collapse="\n",
        writeLog( c(
          paste("Parsing run parameters did not yield a named R list ( class",class(ParamFile_ls),")"),
          paste("Parameter file:",ParamFilePath),
          Level="error"
        ) )
      ) )
    } else {
      writeLog(paste("Successfully read parameters from", ParamFilePath),Level="debug")
    }
  } else writeLog(paste("No parameter file found in",ParamDir),Level="debug") # Just return an empty list
        
  ParamFile_ls <- addParameterSource(ParamFile_ls,paste("File:",ParamFilePath))
  return(ParamFile_ls)
}

#ADD PARAMETER SOURCE ATTRIBUTE
#==============================
#' Add a "source" attribute to a run parameters list
#'
#' \code{addParameterSource} a visioneval framework control function that adds a "source" attribute
#' to a list of run parameters
#'
#' If Param_ls has no parameters in it, this function does nothing
#'
#' @param Param_ls a named list of run parameters
#' @param Source a character string describing the source
#' @return Param_ls, with a "source" data.frame attached (if possible)
#' @export
addParameterSource <- function(Param_ls,Source="Manually added") {
  if ( # Param_ls should be a named list of parameters in it
    is.list(Param_ls) && # must be a list
    (
      length(Param_ls)==0 || # Okay for it to be an empty list
      (
        !is.null(names(Param_ls)) && # But if not empty, everything needs a name
        all(nzchar(names(Param_ls)))
      )
    )
  ) {
    if ( length(Param_ls)>0 ) {
      src.df <- data.frame(Source=Source,Name=names(Param_ls))
      row.names(src.df) <- src.df$Name
    } else {
      src.df <- data.frame()
    }
    attr(Param_ls,"source") <- src.df
  } else {
    # Invalid Param_ls
    writeLog(
      c(
        "Warning: Attempted to add source to invalid parameter list",
        paste("At",as.character(.traceback(2),collapse=", "))
      ),
      Level="warn"
    )
  }
  return(Param_ls)
}

#MERGE PARAMETER LISTS
#=====================
#' Merge configuration parameter lists with priority
#'
#' \code{mergeParameters} a visioneval framework control function that merges
#' two parameter lists, including their "source" attribute (see \code{readConfigurationFile}
#'
#' The list in "Keep_ls" will be added to Param_ls, with duplicated names from keep replacing
#' those in Param_ls.  The "source" attribute of each will be updated.
#'
#' @param Param_ls the list whose parameters will be replace
#' @param Keep_ls the list whose parameters take priority
#' @return a list combining the two arguments
#' @export
mergeParameters <- function(Param_ls,Keep_ls) {

  # Error checking and recovery information
  # Error here mostly means one of the lists was not properly prepared
  #   You should make sure it's a list with all elements named
  #   You should call addParameterSource on it
  if (
    ! is.list(Param_ls) ||
    ( length(Param_ls)>0 &&
      (
        is.null(attr(Param_ls,"source")) ||
        is.null(names(Param_ls)) ||
        any(is.na(names(Param_ls)))
      )
    ) ||
    ! is.list(Keep_ls) ||
    ( length(Keep_ls)>0 &&
      (
        is.null(attr(Keep_ls,"source")) ||
        is.null(names(Keep_ls)) ||
        any(is.na(names(Keep_ls)))
      )
    )
  ) {
    stop(
      "\n",
      paste( collapse="\n",
        writeLog(
          c("mergeParameters has invalid arguments."),
          deparse(.traceback(1)[[1]]),
          Level="error"
        )
      )
    )
  }

  if ( length(Keep_ls)>0 ) {
    keep.source <- attr(Keep_ls,"source")
    param.source <- attr(Param_ls,"source")
    orig.length <- length(Param_ls)
    Param_ls[ names(Keep_ls) ] <- Keep_ls
    if ( orig.length>0 && !is.null(param.source) ) {
      param.source[ row.names(keep.source), ] <- keep.source
    } else {
      param.source <- keep.source
    }
    row.names(param.source) <- param.source$Name
    attr(Param_ls,"source") <- param.source
  }
 return(Param_ls)
}

#LOAD VISIONEVAL CONFIGURATION FILE
#==================================
#' Load model configuration parameters
#'
#' \code{loadConfiguration} a visioneval framework control function that retrieves runtime
#' parameters from a file.
#'
#' @section Details:
#'
#' \code{loadConfiguration} will look in \code{ParamDir} for a configuration file that can be
#' optionally specified in \code{ParamFile}. If it is not specified, the first configuration file
#' found in \code{ParamDir} from among the recognized names will be loaded (see Configuration Files
#' below).
#'
#' Two existing lists of parameters can be provided, \code{override} and \code{keep}.
#' These lists will be merged with any parameters found in the file. If no parameter file is found,
#' and mustWork is FALSE, the two lists will simply be combined as if the file contained an empty
#' parameter list.  Here is the combination algorithm:
#'
#' \enumerate{
#'   \item{ParamDir/ParamFile is loaded if it exists and mustWork==FALSE, otherwise an empty list is
#'     created. See \code{readConfigurationFile} for details on how the file is found and loaded.}
#'   \item{Add list elements from the keep argument (if any); if any of the "keep" elements have the
#'     same name as an element in the parameter file, the "keep" elements take precedence.}
#'   \item{Add list elements from the override argument (if any); if any of the "override" elements
#'     have the same name as an element in the list composed from the parameter file and the "keep"
#'     list, they are ignored}
#' }
#'
#' If all function parameters are defaulted, an empty list is returned. If neither ParamDir nor
#' ParamFile are provided, the "keep" and "override" lists are merged (values from "keep" are used
#' if there are duplicate names in the two lists.
#' 
#' If \code{mustWork} is TRUE (the default if \code{ParamFile} is provided, otherwise defaulting to
#' FALSE) a missing-file error will be raised. Otherwise the input lists will be combined and
#' returned.
#'
#' This function preserves backward compatibility with original VisionEval's rather odd distribution
#' of configuration parameters between run_parameters.json in the model's "defs" directory, the
#' command line parameters passed to initializeModel, and some hardcoded values. It also creates a
#' more complete set of directory specifications for inputs and outputs, so a more informative
#' directory structure can be created. Without any configuration files, source'ing a traditional
#' run_model.R script will do exactly what it always did with respect to filenames, directory
#' locations, etc.
#'
#' Each list passed to \code{loadConfiguration} can have an optional "source" attribute, which is a
#' dataframe with columns "Name" and "Source" and a row for each run parameter that is included in
#' the returned list. This data.frame is sought on the "keep" and "override" lists, and updated to
#' reflect changes made by reading the file and merging the input lists. See
#' \code{updateParameterSource}. Inspecting this attribute will help debug wrong parameter settings
#' (e.g. if a run parameter is overridden by an unexpected value). If you run the initializeModel
#' function with the RunModel=FALSE option, the ModelState_ls that is generated will contain a
#' complete set of run parameters, and you can inspect their source.
#'
#' @param ParamDir Required - a character string (or length-1 vector) identifying the directory in
#'   which to look for configuration files.
#' @param ParamFile The name of a specific configuration file to retrieve. It is an error if this
#'   file does not exist, unless "mustWork" is FALSE. ParamFile is ignored if ParamDir is not
#'   provided.
#' @param ParamPath The path of the file to open. If provided, then ParamDir/ParamFile are ignored
#'   and this path is opened directly.
#' @param keep A named list of run parameters whose values will be added to the items found in
#'   the configuration file (these values take precedence over what is in the file)
#' @param override A named list of run parameters whose values will be added to the items found
#'   in the configuration file (configuration file and keep values take precedence)
#' @param mustWork A logical (default FALSE) which applies when ParamFile is supplied. If TRUE and
#' ParamFile is not found, throw an error. Otherwise a missing file will return an empty list.
#' @return An updated named list of run parameters. The "source" attribute of that list contains
#'   information about what parameter values were changed within this function; see Details
#' @export
loadConfiguration <- function( # if all arguments are defaulted, return an empty list
  ParamDir=NULL,
  ParamFile=NULL,
  ParamPath=NULL, # if provided, use this path and do not construct from ParamDir/ParamFile
  keep=list(),
  override=list(),
  mustWork=FALSE) {

  # Get code origins of keep and override and make "source" attribute if necessary
  # This is a backstop; use addParameterSource for any keep or override parameter
  #   before callling loadConfiguration
  keep.origin <- deparse(substitute(keep))
  if ( is.list(keep) && length(keep)>0 && is.null(attr(keep,"source") ) ) {
    keep <- addParameterSource(keep,keep.origin)
  }
  override.origin <- deparse(substitute(override))
  if ( is.list(override) && length(override)>0 && is.null(attr(override,"source") ) ) {
    override <- addParameterSource(override,override.origin)
  }

  if ( !is.null(ParamPath) ) {
    ParamFile <- basename(ParamPath)
    ParamDir <- dirname(ParamPath)
  }

  # if providing a ParamFile name, default to error if the file does not exist
  if ( missing(mustWork) && !is.null(ParamFile) && !is.null(ParamDir) ) mustWork <- TRUE

  # load the configuration file (empty list returned if mustWork==FALSE and no file found)
  Param_ls <- readConfigurationFile(ParamDir,ParamFile,mustWork) # might be an empty list
  Param_ls <- mergeParameters(Param_ls,keep) # items in keep replace Param_ls
  Param_ls <- mergeParameters(override,Param_ls) # items in Param_ls replace items in override

  return(Param_ls)
}

#FIND RUNTIME INPUT FILE FROM PATH
#=================================
#' Find the first instance of the requested runtime file on InputPath
#'
#' \code{findRuntimeInputFile} is used to model and scenario inputs from the inputPath.
#' The first existing file is returned or NA if none exists. If StopOnError is TRUE (the
#' default), an error message is logged and processing stops.
#'
#' @param File The name of the file to seek (NOT a run parameter)
#' @param Dir The name of a run parameter specifying a directory relative to getwd()
#' @param Param_ls The run parameters in which to seek InputPath and Dir (default
#'   is the one in ve.model environment)
#' @param StopOnError logical, if TRUE stop if no existing file is found, else return NA
#' @return the path of a file existing on InputDir/Dir/File or NA if not found
#' @export
findRuntimeInputFile <- function(File,Dir="InputDir",Param_ls=NULL,StopOnError=TRUE) {
  inputPath <- getRunParameter("InputPath",Param_ls=Param_ls)
  writeLog(paste("Input path raw:",inputPath,collapse=":"),Level="trace")
  searchDir <- getRunParameter(Dir,Param_ls=Param_ls)
  searchInDir <- file.path(inputPath,searchDir) # might yield more than one
  searchInDir <- unique(
    normalizePath(searchInDir,winslash="/",mustWork=FALSE)
  )
  candidates <- character(0)
  if ( any(!is.na(searchInDir)) && !is.na(File) )  {
    searchInDir <- searchInDir[!is.na(searchInDir)]
    candidates <- file.path(searchInDir,File)
    candidates <- candidates[ file.exists(candidates) ]
  }
  if ( StopOnError && ( length(candidates)==0 || is.na(candidates) ) ) {
    writeLog(paste("Input path:",paste(searchInDir,collapse=":")),Level="trace")
    stop( writeLog(paste("Could not locate",File,"in",searchDir),Level="error") )
  }
  return(candidates[1]) # if StopOnError==FALSE, return NA if it does not exist
}

#INITIALIZE LOG
#==============
#' Initialize logging.
#'
#' \code{initLog} a visioneval framework control function that sets up a logging environment and
#' optionally creates a log (text file) that stores messages generated by the framework or packages.
#'
#' This function creates a log file that is a text file which stores messages generated during a
#' model run. The default name of the log is 'Log_<prefix>_<date>_<time>.txt' where '<date>' is the
#' initialization date and '<time>' is the initialization time, and '<prefix>' is the Prefix
#' parameter provided to the function.
#'
#' Logging can be re-initialized at any time to divert subsequent log messages to a different file.
#' If logging to a file is turned off and writeLog is not in an interactive environment
#' (e.g. inside an Rscript), logging to a file will be turned on, with the log file being placed
#' in ve.runtime (if it exists) or the current working directory, with filePrefix set to
#' "VisionEval".
#'
#' @param TimeStamp Force the log message time stamp to this value (default: \code{Sys.time()})
#' @param Threshold Logging threshold to display (see \code{writeLog()} for available levels).
#' Messages below the threshold will be ignored. Default is "info" which shows a lot. "warn" is
#' typical for running a model, and "error" for only the worst.
#' @param Save a logical (default TRUE) indicating whether to save the log file
#' @param Prefix A character string appended to the file name for the log file. For example, if the
#' Prefix is 'CreateHouseholds', the log file is named 'Log_CreateHouseholds_<date>_<time>.txt'. The
#' default value is NULL in which case the Prefix is the date and time.
#' @param Quiet a logical (default=TRUE); if FALSE, write initialization parameters as a log message
#' @return A list containing the name of the constructed log file and the time stamp
#' @import futile.logger
#' @export
initLog <- function(TimeStamp = NULL, Threshold="warn", Save=TRUE, Prefix = NULL, Quiet=TRUE) {

  if (is.null(TimeStamp)) {
    TimeStamp <- as.character(Sys.time())
  }
  LogNameParts <- unlist(strsplit(TimeStamp,split=" "))

  if ( ! is.null(Prefix) ) LogNameParts <- c(Prefix,LogNameParts)

  if ( Save ) {
    LogNameParts <- gsub("[^-[:alnum:]._]","",LogNameParts) # filter illegal filename characters
    LogFile <- paste0(paste(c("Log",LogNameParts),collapse="_"),".txt")
  } else {
    LogFile <- "console"
  }

  # Create and provision the ve.logger
  th <- which( log.threshold %in% toupper(Threshold) )
  if ( length(th)==0 ) Threshold <- "INFO"

  # ve.logger is for all messages from Threshold on up
  #  (so may include more or less than stderr)
  futile.logger::flog.threshold(Threshold, name="ve.logger")
  futile.logger::flog.layout( log.layout.visioneval,name="ve.logger")

  # stderr logger is for WARN, ERROR or FATAL (always)
  futile.logger::flog.threshold("WARN",name="stderr")
  futile.logger::flog.layout( log.layout.visioneval, name="stderr")

  if ( Save ) {
    if ( interactive() ) {
      futile.logger::flog.appender(futile.logger::appender.tee(LogFile),name="ve.logger")
      futile.logger::flog.appender(futile.logger::appender.tee(LogFile),name="stderr")
    } else {
      futile.logger::flog.appender(futile.logger::appender.file(LogFile),name="ve.logger")
      futile.logger::flog.appender(futile.logger::appender.file(LogFile),name="stderr")
    }
  } else {
    futile.logger::flog.appender(futile.logger::appender.console(),name="ve.logger")
    futile.logger::flog.appender(futile.logger::appender.console(),name="stderr")
  }

  if ( ! Quiet && interactive() ) {
    startMsg <- paste("Logging started at",TimeStamp,"for",toupper(Threshold),"to",LogFile)
    writeLogMessage(startMsg)
  }

  invisible(list(LogFile=LogFile,ModelStart=TimeStamp))
}
#initLog()

# futile.logger layout for visioneval (adjusted from futile.logger::layout.simple)

log.appender.errtee <- function(file) {
  function(line) {
    cat(line,sep="",file=stderr())
    cat(line,file=file,append=TRUE,sep="")
  }
}

prepare_arg <- function(x) {
  if (is.null(x) || length(x) == 0) return(deparse(x))
  return(x)
}

log.layout.message <- function(level,msg,...) return(paste(msg,...,"\n",sep=""))

log.layout.simple <- function(level, msg, id='', ... ) {
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), prepare_arg)
    msg <- do.call(sprintf, c(msg, parsed))
  }
  return( sprintf("%s\n", msg) )
}

log.layout.visioneval <- function(level, msg, id='', ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), prepare_arg)
    msg <- do.call(sprintf, c(msg, parsed))
  }
  level <- names(level)[1]
  level <- paste(substring(level, 1, 1), tolower(substring(level, 2)), sep = "")
  log.msg <- sprintf("%s [%s]: %s\n", the.time, level, msg)
  return( log.msg )
}

# Internal log level translation table
log.threshold <- c(
  "TRACE",
  "DEBUG",
  "INFO",
  "WARN",
  "ERROR",
  "FATAL"
)
log.function <- list(
  TRACE = futile.logger::flog.trace,
  DEBUG = futile.logger::flog.debug,
  INFO  = futile.logger::flog.info,
  WARN  = futile.logger::flog.warn,
  ERROR = futile.logger::flog.error,
  FATAL = futile.logger::flog.fatal
)

#WRITE LOG MESSAGE
#================
#' Write an unformatted messge to log (no timestamp, etc.)
#'
#' \code{writeLogMessage} a visioneval framework control function that writes an unformatted message
#' to the run log (without time stamp, etc.)
#'
#' This function logs messages to the VisionEval log, which will be stdout (the console) if not
#' running a model, and the model log file if a model is running.
#'
#' A character vector may be provided for Msg, in which case each element of the vector will become
#' one line in the log.
#'
#' @param Msg A character vector, whose first element must not be empty.
#' @param Logger A string indicating the name of the logger ("ve.logger" or "stderr")
#' @param Level If provided, write the log message to Logger with this Level; otherwise use FATAL,
#'   which doesn't stop anything - it just forces the message out in all cases.
#' @return TRUE if the message is written to the log successfully ("as-is")
#' @export
writeLogMessage <- function(Msg = "", Logger="ve.logger", Level="") {
  if ( missing(Msg) || length(Msg)==0 || ! nzchar(Msg) ) {
    message(
      "writeLogMessage(Msg): No message supplied\n",
    )
  } else {
    on.exit(quote(futile.logger::flog.layout( log.layout.visioneval, name=Logger )))
    futile.logger::flog.layout( log.layout.simple, name=Logger )
    if ( nzchar(Level) ) {
      # This message won't get out if Level is below the Logger threshold
      visioneval::writeLog(Msg,Level=Level,Logger=Logger)
    } else {
      # Otherwise, the message always gets out into the system log
      futile.logger::flog.fatal( Msg, name=Logger ) # "fatals" always get out...
    }
  }
  invisible(Msg)
}

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
#' A character vector may be provided for Msg, in which case each ' element of the vector will
#'   become one line in the log. By default, messages are logged to stderr if WARN or above, or to
#'   ve.logger if below WARN but at or above the current ve.logger threshold. If a Logger is
#'   given explicitly, it will be written to (provided its own threshold is at a sufficient level).
#'
#' @param Msg A character vector, whose first element must not be empty.
#' @param Level character string identifying log level. If missing or invalid a log message will be
#'   generated as a simple message (no log level in the message) at a level guaranteed to get out
#'   ("FATAL" - though no error condition will be generated).
#' @param Logger character string overriding default logger selection
#' @return TRUE if the message is written to the log successfully.
#' It appends the time and the message text to the run log.
#' @export
writeLog <- function(Msg = "", Level="NONE", Logger="") {
  noLevel <- ( missing(Level) || ! (Level <- toupper(Level) ) %in% log.threshold )
  if ( noLevel ) Level <- "FATAL"
  if ( missing(Msg) || length(Msg)==0 || ! nzchar(Msg) ) {
    message(
      "writeLog(Msg,Level,Logger): No message supplied\n",
      "Available Log Levels:\n",
      paste(names(log.threshold),collapse=", ")
    )
  } else {
    # Pick the logger
    if ( ! is.character(Logger) || ! nzchar(Logger) ) {
      Logger <- if ( which(log.threshold==Level) >= which(log.threshold=="WARN") ) "stderr" else "ve.logger"
    }
    # Pick the log format 
    if ( noLevel ) {
      on.exit(quote(futile.logger::flog.layout( log.layout.visioneval, name=Logger )))
      futile.logger::flog.layout( log.layout.simple, name=Logger )
    }
    for ( m in Msg ) {
      log.function[[Level]](m,name=Logger)
    }
  }
  invisible(Msg)
}
