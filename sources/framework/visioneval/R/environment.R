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
#'   just clear the Owner
#' @return an R environment for "ve.model"
#' @export
modelEnvironment <- function(Clear=NULL) {
  # export this function since it can be useful in the VEModel
  # package
  if ( ! missing(Clear) && is.character(Clear) ) {
    if ( nzchar(Clear) ) { # Clear is a non-empty string: set owner flag and remove everything
      rm(list=ls(ve.model),envir=ve.model)
      ve.model$Owner <- Clear
    } else {
      # suppress ve.model initialization if "owned"
      if ( is.null(ve.model$Owner) ) { # probably called initializeModel from "source"
        rm(list=ls(ve.model),envir=ve.model) # clear environment
      } else {
        # pre-existing model environment is retained
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
#' @param envir The environment in which the model is running (default visioneval::ve.model)
#' @return TRUE if ve.model$RunModel is TRUE, otherwise FALSE
#' @export
modelRunning <- function(envir=modelEnvironment()) {
    if ( "RunModel" %in% names(envir) ) { # VEModel or initializeModel will set
      return(envir$RunModel)
    } else { # The model is running in backward-compatible mode
      return(envir$RunModel <- TRUE)
    }
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
#' @param Parameter character vector (length one) naming Parameter to retrieve
#' @param Param_ls a list of run parameters to look within (otherwise RunParam_ls from
#' "ve.model" environment if it exists, and if not just an empty list.
#' @param Default is the value to provide if Parameter is not found in RunParam_ls
#' source for the parameter value
#' @return A parameter value or its default if not set
#' @export
getRunParameter <- function(Parameter,Param_ls=NULL,Default=NA) {
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
  if ( ! is.list(Param_ls) ) { # if provided, look only there, otherwise in ve.model
    Param_ls <- get0("RunParam_ls",envir=ve.model,ifnotfound=list())
    # TODO: add source to any Param_ls elements that do not already have a source
    Param_ls <- addParameterSource(Param_ls,"ve.model$RunParam_ls",onlyMissing=TRUE)
  } else {
    sources <- lapply(Param_ls,function(p) attr(p,"source"))
    updateSources <- which( is.null(sources) )
    if ( length(updateSources) > 0 ) {
      if ( is.null(param.source) ) {
        param.source <- paste("Unknown, called from:",as.character(.traceback(2)),collapse=", ")
      }
      Param_ls <- addParameterSource(Param_ls,param.source,onlyMissing=TRUE)
    }
  }

  if ( length(Param_ls)==0 || ! Parameter %in% names(Param_ls) ) {
    if ( defaultMissing && Parameter %in% names(defaultParams_ls) ) {
      Default <- defaultParams_ls[[Parameter]]
    }
    paramVal <- Default
  } else {
    paramVal <- Param_ls[[Parameter]]
  }
  return( paramVal )
}

# These are default parameters used by the framework functions
# e.g. in intializeModel or initModelSTate.
default.parameters.table = list(
  Seed = 1,
  LogLevel = "error",
  DatastoreName = "Datastore",
  ModelDir = ".", # root directory for the model
  ModelScript = "run_model.R", # default model script name
  ModelStateFile = "ModelState.Rda",
  InputPath = ".", # should default to same directory as ModelDir
  InputDir = "inputs", # directory name within InputPath containing model inputs
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  UnitsFile = "units.csv",
  DeflatorsFile = "deflators.csv",
  ModelParamFile = "model_parameters.json",
  ParamDir = "defs",
  DatastoreType = "RD",
  SaveDatastore = TRUE,           # Whether to archive any existing ResultsDir
  ArchiveResultsName = "Results", # Root name for archived Results directory
  ResultsDir = "."                # To get framework support for VEModel, which overrides
)

#GET DEFAULT PARAMETERS
#======================
#' Report default values for key parameters
#'
#' \code{defaultVERuntimeParameters} is a visioneval model developer function takes an (optional,
#' default empty) list of RunParams and reports the default values for any parameter not already in
#' that list. It will look in attached VisionEval packages (names starting with
#' \code{"package:VE..."} for an exported defaultVERunParameters function, and those will be given
#' priority when loading (with the most recently loaded packages having the highest priority).
#' \code{getRunParameter} handles that seamlessly if no inline default is provided.
#'
#' To get a list of all the defaults (by package - see the Param_ls attribute "Source" - call
#' this function with an empty list or with no parameter at all. To pick out a default value,
#' you can index the resulting list after it is returned,
#' e.g. \code{defaultVERunParameters()[["ResultsDir"]]}
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
#' (e.g. if ag run parameter is overridden by an unexpected value). If you run the initializeModel
#' function with the RunModel=FALSE option, the ModelState_ls that is generated will contain a
#' complete set of run parameters, and you can inspect their source.
#'
#' @param ParamDir (optional) directory in which to seek a configuration file
#' @param ParamFile (optional) is the name of a file to seek. If it is not found, throw
#' an error.
#' @param ParamPath The path of the file to open. If provided, then ParamDir/ParamFile are ignored
#'   and this path is opened directly.
#' @param mustWork (default FALSE) if TRUE, and ParamFile is provided, throw an error
#'   if ParamDir/ParamFile does not exist. Otherwise, return an empty list if no file is found.
#' @return A list of Run Parameters. If ParamFile is supplied and it does not exist in
#' ParamDir, throw an error. If ParamFile is not supplied and no configuration file is
#' found, return an empty list. Otherwise return the list of parameters found in the file.
#' A 'source' attribute is also constructed if the list is not empty (see Details).
#' @export
readConfigurationFile <- function(ParamDir=NULL,ParamFile=NULL,ParamPath=NULL,mustWork=FALSE) {

  # Initialize the parameter list
  ParamFile_ls <- list()

  if ( !is.null(ParamPath) ) {
    # ParamDir and ParamFile are ignored if ParamPath is provided
    ParamFile <- basename(ParamPath)
    ParamDir <- dirname(ParamPath)
    ParamPath <- NULL
  } else {
    # ParamFile is ignored if ParamDir is not provided
    if ( is.null(ParamDir) && !is.null(ParamFile) ) ParamFile <- NULL
  }
  
  # Otherwise, check for candidate files and return an empty list if none found in ParamDir
  if ( is.null(ParamFile) ) {
    if ( ! is.null(ParamDir) ) {
      ParamFiles   <- c("visioneval.yml","visioneval.json","visioneval.cnf","visioneval.ini")
      ParamPattern <- sapply(ParamFiles,function(x) paste0("(",x,")"))
      ParamPattern <- paste("^",paste(ParamPattern,collapse="|"),"$",sep="")
      ParamPattern <- gsub("\\.","\\\\.",ParamPattern) # quote literal periods in file name candidates
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
            "Error: ParamDir is a vector in readConfigurationFile:\n",
            paste(ParamDir,collapse="\n")
          ),
          Level="error"
        )
      )
    }
    ParamFileExists <- FALSE
    if ( is.null(ParamPath) && ! is.null(ParamFile) ) {
      ParamPath <- unique(file.path(ParamDir,ParamFile))
      ParamFileExists <- file.exists(ParamPath)
    }

    writeLog(paste("ParamFileExists:",ParamFileExists),Level="debug")
    if ( ! ParamFileExists ) {
      if ( mustWork ) {
        stop(
          writeLog(
            paste("Could not locate Parameter File:",ParamPath),
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
            writeLog(c("Trying YAML parameters from",ParamPath),Level="info")
            yaml::yaml.load_file(ParamPath)
          },
          error = function(e) {
            writeLog("Failed to read YAML file; retrying as JSON...",Level="info")
            writeLog(paste("YAML:",conditionMessage(e)),Level="info")
            invokeRestart("json",ParamPath)
          },
          warning = function(w) {
            formatWarnings <- writeLog(c("Warning reading YAML:",conditionMessage(w)),Level="info")
          }
        )
      )
      if ( length(ParamFile_ls)==0 ) {
        writeLog(c("No parameters in loaded file:",ParamPath),Level="warn")
        writeLog("Make another attempt with log Level='info' for details",Level="warn")
      }
    }
    if ( length(ParamFile_ls)>0 && ! all(nzchar(names(ParamFile_ls))) ) {
      stop( paste(
        collapse="\n",
        writeLog( c(
          paste("Parsing run parameters did not yield a named R list ( class",class(ParamFile_ls),")"),
          paste("Parameter file:",ParamPath),
          Level="error"
        ) )
      ) )
    } else {
      if ( length(ParamFile_ls)==0 ) {
        writeLog("No parameters read",Level="debug")
      } else {
        writeLog(paste("Successfully read parameters",if(!is.null(ParamPath)) paste("from", paste0("'",ParamPath,"'")) else ""),Level="debug")
      }
    }
  } else writeLog(paste("No parameter file found in",ParamDir),Level="debug") # Just return an empty list

  ParamFile_ls <- addParameterSource(ParamFile_ls,paste("File:",ParamPath))
  attr(ParamFile_ls,"FILE") <- ParamPath # convenience for keeping track of what was loaded
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
#' @param onlyMissing a logicial; if TRUE, assign Source to all items in Param_ls, otherwise just
#'   to those that have no Source
#' @return Param_ls, with a "source" attached to each item
#' @export
addParameterSource <- function(Param_ls,Source="Manually added",onlyMissing=FALSE) {
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
    # Make sure every element of Param_ls has a Source
    Param_ls <- lapply( Param_ls,
      function(p) {
        if ( ! onlyMissing || is.null(attr(p,"source")) ) {
          attr(p,"source") <- Source
        }
        return( p )
      }
    )
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

#ADD RUN PARAMETER
#=================
#' Convenience function for adding or updating a parameter to Param_ls
#'
#' \code{addRunParameter} a visioneval framework control function that adds an arbitrary parameter to a
#' RunParam_ls. If you want to push a list of parameters into \code{Param_ls}, use
#' \code{mergeParameters}. If that other list does not yet have a Source attribute, use
#' addParameterSource first on that list. Or you can force each of the ... items to have a source
#' just by running \code{attr(newParam,"source")<-forceSource} before calling this function.
#'
#' @param Param_ls the list whose parameters will be changed/added to
#' @param Source a character string specifying the source for the changed/added parameters
#' @param ... Named arguments to be added to Param_ls
#' @return The updated Param_ls
#' @export
addRunParameter <- function(Param_ls=list(),Source=NULL,...) {
  newParams_ls <- list(...)
  # if source not supplied, check if first item in newParams_ls has a source and use that
  if ( missing(Source) || is.null(Source) ) {
    item.source <- attr(newParams_ls[[1]],"source")
    if ( ! is.null(item.source) ) Source <- item.source
  }
  if ( is.null(Source) ) Source <- "Interactive"

  # I think ... must have names; we'll drop any items that sneak into ... without names
  if ( !is.null(names(newParams_ls)) ) newParams_ls <- newParams_ls[!is.na(names(newParams_ls))]
  # Add the source to the new parameter list
  newParams_ls <- addParameterSource(newParams_ls,Source)
  # Merge new list into the base parameter list
  return( mergeParameters(Param_ls,newParams_ls) )
}

#MERGE PARAMETER LISTS
#=====================
#' Merge configuration parameter lists with priority
#'
#' \code{mergeParameters} a visioneval framework control function that merges
#' two parameter lists (see \code{readConfigurationFile}
#'
#' The list in "Keep_ls" will be added to Param_ls, with duplicated names from keep replacing
#' those in Param_ls.  The "source" attribute of each will be updated.
#'
#' @param Param_ls the list whose parameters will be replaced
#' @param Keep_ls the list whose parameters take priority
#' @return a list combining the two arguments
#' @export
mergeParameters <- function(Param_ls,Keep_ls) {

  # Error checking and recovery information
  # Error here mostly means one of the lists was not properly prepared
  #   You should make sure it's a list with all elements named
  #   You should call addParameterSource on it

  merge.error <- FALSE
  if ( ! is.list(Param_ls) ) {
    error.reason <- "Param_ls is not a list"
    merge.error <- TRUE
  } else if (
    length(Param_ls)>0 &&
    (
      any( nullSources <- which(sapply(Param_ls,function(p) is.null(attr(p,"source")))) ) ||
      is.null(names(Param_ls)) ||
      any(is.na(names(Param_ls)))
    )
  ) {
    error.reason <- paste(
      "Param_ls",
      "Null Sources:",paste(names(Param_ls)[nullSources],collapse=","),
      "Names Null:",is.null(names(Param_ls)),
      "Names NA:",any(is.na(names(Param_ls)))
    )
    merge.error <- TRUE
  } else if ( ! is.list(Keep_ls) ) {
    error.reason <- "Keep_ls is not a list"
    merge.error <- TRUE
  } else if (
    length(Keep_ls)>0 &&
    (
      any( nullSources <- which(sapply(Keep_ls,function(p) is.null(attr(p,"source")))) ) ||
      is.null(names(Keep_ls)) ||
      any(is.na(names(Keep_ls)))
    )
  ) {
    error.reason <- paste(
      "Keep_ls",
      "Null Sources:",paste(names(Keep_ls)[nullSources],collapse=","),
      "Names Null:",is.null(names(Keep_ls)),
      "Names NA:",any(is.na(names(Keep_ls)))
    )
    merge.error <- TRUE
  }
  if ( merge.error ) {
    stop(
      "\n",
      paste( collapse="\n",
        writeLog(
          c("mergeParameters has invalid arguments.",
            error.reason,
            deparse(.traceback(5))
          ),
          Level="error"
        )
      )
    )
  }

  if ( length(Keep_ls)>0 ) {
    Param_ls[ names(Keep_ls) ] <- Keep_ls
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
  #   before calling loadConfiguration
  keep.origin <- deparse(substitute(keep))
  if (
    is.list(keep) &&
    length(keep)>0 &&
    any( sapply(keep,function(p) is.null(attr(p,"source"))) )
  ) {
    keep <- addParameterSource(keep,keep.origin,onlyMissing=TRUE)
  }
  override.origin <- deparse(substitute(override))
  if ( is.list(override) &&
    length(override)>0 &&
    any( sapply(override,function(p) is.null(attr(p,"source"))) )
  ) {
    override <- addParameterSource(override,override.origin,onlyMissing=TRUE)
  }

  # if providing a ParamFile name, default to error if the file does not exist
  if ( missing(mustWork) && !is.null(ParamFile) && !is.null(ParamDir) ) mustWork <- TRUE

  # load the configuration file (empty list returned if mustWork==FALSE and no file found)
  Param_ls <- readConfigurationFile(ParamDir,ParamFile,ParamPath,mustWork) # might be an empty list
  ParamPath <- attr(Param_ls,"FILE") # absolute path of file that was actually read
  Param_ls <- mergeParameters(Param_ls,keep) # items in keep replace Param_ls
  Param_ls <- mergeParameters(override,Param_ls) # items in Param_ls replace items in override
  if ( is.null(attr(Param_ls,"FILE")) ) attr(Param_ls,"FILE") <- ParamPath # Maintain FILE attribute after merging

  return(Param_ls)
}

# FIND FILE ON PATH
#==================
#' Find the first instance of a file name on a given path.
#'
#' \code{findFileOnPath} is used to locate files that exist in alternative directories. A vector of
#' constructed files is returned or NA if no files exist in the indicated locations. If onlyExists
#' is TRUE, the resulting vector of file paths is reduced to just the vector of files that actually
#' exist, or NA if there are no existing files.
#'
#' @param File The name of the file to seek
#' @param Dir Directory (or directories) in which to seek files
#' @param onlyExists A logical; if TRUE filter the list of files to just those that exist (or NA if
#'   none); otherwise return all candidate names.
#' @return the path(s) of files found on all combinations of Root(s), Dir(s) and File(s)
#' @export
findFileOnPath <- function(File,Dir,onlyExists=TRUE) {
  dir.file <- character(0)
  for ( d in Dir ) for ( f in File ) dir.file <- c(dir.file, file.path(d,f))
  dir.file <- unique(normalizePath(dir.file,winslash="/",mustWork=FALSE))
  if ( onlyExists ) dir.file <- dir.file[ file.exists(dir.file) ]
  return(dir.file) # possibly empty character vector
}

# FIND RUNTIME INPUT FILE FROM PATH
#==================================
#' Find the first instance of the requested runtime file on InputPath
#'
#' \code{findRuntimeInputFile} is used to locate model and scenario inputs from the inputPath. The
#' first existing file is returned or NA if none exists. If StopOnError is TRUE (the default), an
#' error message is logged and processing stops.
#'
#' @param File The name (or a vector of names) of the file to seek (NOT a run parameter)
#' @param Dir The name of a path parameter (InputPath, ParamPath) to search for input files
#' @param Param_ls The run parameters in which to seek value for Dir (yields vector of directory paths)
#' @param StopOnError logical, if TRUE stop if no existing file is found, else return NA
#' @return the path of the first existing Dir/File or NA if not found
#' @export
findRuntimeInputFile <- function(File,Dir="InputPath",Param_ls=NULL,StopOnError=TRUE) {
  inputPath <- getRunParameter(Dir,Param_ls=Param_ls)
  writeLog(paste("Input path raw:",inputPath,collapse=":"),Level="trace")
  candidates <- findFileOnPath( File=File, Dir=inputPath )
  if ( StopOnError && ( length(candidates)==0 || is.na(candidates) ) ) {
    stop( writeLog(paste("Could not locate",File,"on",inputPath,collapse="; "),Level="error") )
  }
  return(candidates[1]) # if StopOnError==FALSE, return NA if no matching File was found
}

#MAKE TIME INTO TIMESTAMP TEXT
#=============================
#' Format a time into text suitable for use in a file name
#'
#' \code{textTimeStamp} will return a string suitable for use in a file name
#' from a standard text version of a time
#'
#' @param TimeStamp a time value (e.g. from Sys.time())
#' @param Prefix An optional prefix to put ahead of the time
#' @return A string representing date and time with all characters sanitized for use in a file or
#'   directory name
#' @export
fileTimeStamp <- function( TimeStamp, Prefix=NULL ) {
  TimeStamp <- gsub(":","-",gsub(" ", "_",TimeStamp))
  LogNameParts <- unlist(strsplit(TimeStamp,split=" "))
  if ( ! is.null(Prefix) ) LogNameParts <- c(Prefix,LogNameParts)
  LogNameParts <- gsub("[^-[:alnum:]._]","",LogNameParts) # filter illegal filename characters
  return( paste(LogNameParts,collapse="_") )
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
#' @param TimeStamp Force the log message time stamp to this value (default: \code{Sys.time()})
#' @param Threshold Logging threshold to display (see \code{writeLog()} for available levels).
#' Messages below the threshold will be ignored. Default is "info" which shows a lot. "warn" is
#' typical for running a model, and "error" for only the worst.
#' @param Save a logical (default TRUE) indicating whether to save the log file
#' @param Prefix A character string appended to the file name for the log file. For example, if the
#' Prefix is 'CreateHouseholds', the log file is named 'Log_CreateHouseholds_<date>_<time>.txt'. The
#' default value is NULL in which case the Prefix is the date and time.
#' @param Clear a logical (default=FALSE); if FALSE, use existing LogStatus (output file and
#'   threshold)
#' @param Quiet a logical (default=TRUE); if FALSE, write initialization parameters as a log message
#' @param envir The enviroment in which to track the log status
#' @return A list containing the name of the constructed log file and the time stamp
#' @import futile.logger
#' @export
initLog <- function(TimeStamp = NULL, Threshold="warn", Save=TRUE, Prefix = NULL,
  Clear=FALSE, Quiet=TRUE, envir=modelEnvironment()) {

  # Don't touch the log if it is already initialized
  ve.model <- envir
  if ( "LogStatus" %in% names(ve.model) ) {
    if ( ! Clear ) {
      return(invisible(ve.model$LogStatus))
    } else {
      rm("LogStatus",envir=ve.model)
    }
  }

  if ( Save ) {
    if (is.null(TimeStamp)) {
      TimeStamp <- Sys.time()
    }
    LogFile <- paste0(fileTimeStamp(TimeStamp,Prefix=c("Log",Prefix)),".txt")
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

  if ( Save ) saveLog(LogFile,envir) # Can always do so later...

  if ( ! Quiet && interactive() ) {
    startMsg <- paste("Logging started at",TimeStamp,"for",toupper(Threshold),"to",LogFile)
    writeLogMessage(startMsg)
  }

  # Save and return the Log status (e.g. to use TimeStamp or start saving later)
  ve.model$LogStatus <- list(LogFile=LogFile,ModelStart=TimeStamp)

  invisible(ve.model$LogStatus)
}
#initLog()

#SAVE LOG
#========
#' Turn log saving on and off.
#'
#' \code{saveLog} a visioneval framework control function that shifts logging to the log file or
#' back just to the console. see \code{initLog}.
#'
#' @param LogFile the file into which to save the log. If the specific value "console", then do
#'   not log to a file. If NULL, look in ve.model environment, and fall back to "console"
#' @param envir The environment holding LogFile and/or ModelState_ls
#' @return TRUE if saving to a file, else FALSE
#' @import futile.logger
#' @export
saveLog <- function(LogFile=NULL,envir=modelEnvironment()) {
  if ( is.null(LogFile) ) {
    ve.model <- envir
    if ( "LogStatus" %in% ls(ve.model) ) LogFile <- ve.model$LogStatus$LogFile
    if ( is.null(LogFile) ) { # if still null, it wasn't in ve.model$LogStatus...
      LogFile <- "console"
    }
  }
  writeLog(paste("Logging to",LogFile),Level="info")
  Save <- LogFile != "console"
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
  return(Save)
}

# futile.logger layout for visioneval (adjusted from futile.logger::layout.simple)

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
  if ( level=="Warn" ) { # Don't put level in "warnings"
    log.msg <- sprintf("%s : %s\n", the.time, msg)
  } else {
    log.msg <- sprintf("%s [%s]: %s\n", the.time, level, msg)
  }
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
      writeLog(Msg,Level=Level,Logger=Logger)
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
      paste(log.threshold,collapse=", ")
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
