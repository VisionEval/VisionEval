#================
#environment.R
#================

#This script defines functions used to establish the model
#environment and configuration parameters.

#CREATE R ENVIRONMENT FOR MODEL
#==============================
#' Attach an R environment to the search path for the active model
#'
#' \code{modelEnvironment} a visioneval framework control function that locates
#' the environment for elements of the active model, creating it and placing it
#' on the search path if necessary. That environment contains the ModelState,
#' the Datastore access function aliases and related information. If create is
#' \code{FALSE}, throw an error instead of creating a new environment
#' if it does not already exist. The environment should be emptied and
#' recreated whenever a new model is initialized, either to load or run
#' it.
#'
#' @param Create a logical indicating whether a missing model environment
#' should be created.
#' @return an R environment attached to "ve.model" on the search path.
#' @export
modelEnvironment <- function(Create=TRUE) {
  # export this function since it can be useful in the VEModel
  # package
  if ( ! "ve.model" %in% search() ) {
    if ( ! Create ) stop("Missing ve.model environment.")
    ve.model <- attach(NULL,name="ve.model")
  } else {
    ve.model <- as.environment("ve.model")
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
  if ( ! "ve.model" %in% search() ) { # have not performed initializeModel
    return(FALSE)
  } else {
    ve.model <- as.environment("ve.model")
    if ( "RunModel" %in% ls(ve.model) ) { # VEModel will set during "open" or "run" model
      return(ve.model$RunModel)
    } else { # The model is running in backward-compatible mode
      return(ve.model$RunModel <- TRUE)
    }
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
#' @param Default is the value to provide if Parameter is not found in RunParam_ls
#' @param Param_ls a list of run parameters to look within (otherwise RunParam_ls from
#' "ve.model" environment if it exists, and if not then in and above
#' \code{parent.frame()})
#' @param logSource a logical (default=FALSE); if TRUE, write an info-level log message reporting the
#' source for the parameter value
#' @return A parameter value or its default if not set
#' @export
getRunParameter <- function(Parameter,Default=NA,Param_ls=NULL,logSource=FALSE) {
  param.source <- NULL
  if ( ! is.list(Param_ls) ) { # look only there
    envir <- if ( "ve.model" %in% search() ) {
      param.source <- "ve.model$RunParam_ls"
      as.environment("ve.model")
    } else {
      param.source <- paste("Calling environment:",as.character(.traceback(2)),collapse=", ")
      parent.frame()
    }
    Param_ls <- get0("RunParam_ls",envir=envir,ifnotfound=list())
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
      Level="info"
    )
    return( Default )
  } else {
    if ( logSource ) {
      sources <- attr(Param_ls,"source")
      if ( is.null(sources) ) {
        writeLog(
          paste("Unknown source for",Parameter,"at",as.character(.traceback(2)),collapse=", "),
          Level="info"
        )
      } else {
        writeLog(
          paste("Source for",Parameter,":",sources[Parameter,"Source"]),
          Level="info"
        )
      }
    }
    return( Param_ls[[Parameter]] )
  }
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
    ParamFileExists <- FALSE
    if ( ! is.null(ParamFile) ) {
      for ( pDir in ParamDir ) { # Might be more than one, depending on InputPath
        ParamFilePath <- unique(file.path(ParamDir,ParamFile))
        ParamFileExists <- file.exists(ParamFilePath)
        if ( length(ParamFilePath)>1 ) {
          for ( i in 1:length(ParamFilePath) ) {
            writeLog(
              paste("ParamFile",i,ParamFilePath[i]),Level="info")
          }
          ParamFilePath <- ParamFilePath[which(ParamFileExists)[1]]
        }
        if ( ParamFileExists ) break
      }
    }
    formatErrors <- character(0)
    formatWarnings <- character(0)

    if ( ! ParamFileExists ) {
      if ( mustWork ) {
        stop(
          writeLog(
            paste("Missing required parameter file:",ParamFilePath),
            Level="error"
          )
        )
      }
      # else fall through with ParamFile_ls an empty list
    } else {
      ParamFile_ls <- withRestarts(
        json = function(path,yaml.msg) {
          tryCatch(
            {
              jsonlite::fromJSON(path)
            },
            error = function(e) {
              formatErrors <- c(formatErrors,yaml.msg,paste("JSON:",conditionMessage(e)))
              list()
            },
            warning = function(w) {
              formatWarnings <- c(formatWarnings,paste("JSON:",conditionMessage(w)))
            }
          )
        },
        tryCatch(
          {
            yaml::yaml.load_file(ParamFilePath)
          },
          error = function(e) formatErrors <- {
            invokeRestart("json",ParamFilePath,paste("YAML:",conditionMessage(e)))
          },
          warning = function(w) formatWarnings <- c(formatWarnings,paste("YAML:",conditionMessage(w)))
        )
      )
      if ( length(formatWarnings)>0 ) {
        writeLog(paste("Warning reading run parameters from ",ParamFilePath))
        writeLog(formatWarnings,Level="warn")
      }
      if ( length(formatErrors)>0 ) {
        stop(
          "\nError reading run parameters from ",ParamFilePath,"\n",
          paste(collapse="\n",writeLog(formatErrors,Level="error"))
        )
      }
    }
  } else writeLog(paste("No parameter file found in",ParamDir),Level="debug")
  if ( length(ParamFile_ls)>0 && ! all(nzchar(names(ParamFile_ls))) ) {
    stop( paste(
      collapse="\n",
      writeLog( c(
        paste("Parsing run parameters did not yield a named R list ( class",class(ParamFile_ls),")"),
        paste("Parameter file:",ParamFilePath),
        Level="error"
      ) )
    ) )
  }
        
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
  if (
    is.list(Param_ls) &&
    (
      length(Param_ls)==0 ||
      (
        !is.null(names(Param_ls)) &&
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
        c("mergeParameters has invalid arguments.",
          "Call Stack:",
          as.character(.traceback(2)) # show calls above call to mergeParameters
        ), Level="error")
    ) )
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
#' @param DefaultDir The default value for the run parameter directory
#' @param Param_ls The run parameters in which to seek InputPath and Dir (default
#'   is the one in ve.model environment)
#' @param StopOnError logical, if TRUE stop if no existing file is found, else return NA
#' @return the path of a file existing on InputDir/Dir/File or NA if not found
#' @export
findRuntimeInputFile <- function(File,Dir="InputDir",DefaultDir="inputs",Param_ls=NULL,StopOnError=TRUE) {
  inputPath <- getRunParameter("InputPath",Default=".",Param_ls=Param_ls)
  writeLog(paste("Input path raw:",inputPath,collapse=":"),Level="trace")
  searchDir <- getRunParameter(Dir,Default=NA,Param_ls=Param_ls)
  searchInDir <- file.path(inputPath,searchDir) # might yield more than one
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
  return(candidates[1]) # if StopOnError==FALSE, return NA
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
#' @return A list containing the name of the constructed log file and the time stamp
#' @import futile.logger
#' @export
initLog <- function(TimeStamp = NULL, Threshold="info", Save=TRUE, Prefix = NULL) {

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

  startMsg <- paste("Logging started at",TimeStamp,"for",toupper(Threshold),"to",LogFile)
  writeLogMessage(startMsg)

  invisible(list(LogFile=LogFile,ModelStart=TimeStamp))
}
#initLog()

# futile.logger layout for visioneval (adjusted from futile.logger::layout.simple)

prepare_arg <- function(x) {
  if (is.null(x) || length(x) == 0) return(deparse(x))
  return(x)
}

log.appender.errtee <- function(file) {
  function(line) {
    cat(line,sep="",file=stderr())
    cat(line,file=file,append=TRUE,sep="")
  }
}

log.layout.message <- function(level,msg,...) return(paste(msg,...,"\n",sep=""))

log.layout.visioneval <- function(level, msg, id='', ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), prepare_arg)
    msg <- do.call(sprintf, c(msg, parsed))
  }
  return( sprintf("[%s] %s : %s\n", substr(names(level),1,1), the.time, msg) )
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
#' @return TRUE if the message is written to the log successfully ("as-is")
#' @export
writeLogMessage <- function(Msg = "", Logger="ve.logger") {
  if ( missing(Msg) || length(Msg)==0 || ! nzchar(Msg) ) {
    message(
      "writeLogMessage(Msg): No message supplied\n",
    )
  } else {
    futile.logger::flog.layout( log.layout.message, name=Logger )
    futile.logger::flog.fatal( Msg, name=Logger ) # "fatals" always get out...
    futile.logger::flog.layout( log.layout.visioneval, name=Logger )
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
#' @param Level character string identifying log level
#' @param Logger character string overriding default logger selection
#' @return TRUE if the message is written to the log successfully.
#' It appends the time and the message text to the run log.
#' @export
writeLog <- function(Msg = "", Level="INFO", Logger="") {
  if ( missing(Msg) || length(Msg)==0 || ! nzchar(Msg) ) {
    message(
      "writeLog(Msg,Level,Logger): No message supplied\n",
      "Available Log Levels:\n",
      paste(names(log.threshold),collapse=", ")
    )
  } else {
    Level <- toupper(Level)
    if ( ! Level %in% names(log.function) ) Level="INFO"
    if ( ! is.character(Logger) || ! nzchar(Logger) ) {
      Logger <- if ( which(log.threshold==Level) >= which(log.threshold=="WARN") ) "stderr" else "ve.logger"
    }
    for ( m in Msg ) {
      log.function[[Level]](m,name=Logger)
    }
  }
  invisible(Msg)
}
