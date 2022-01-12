# Author: Jeremy Raw

# VEModel Package Code

#' @include environment.R
#' @include results.R
#' @import visioneval
NULL

# Documentation for VEQuery
#' VEQuery class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEQuery
NULL

# Documentation for VEQuerySpec
#' VEQuerySpec class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEQuerySpec
NULL

self=private=NULL

# Protocol for building a VEQuery:
#  1. qry <- VEQuery$new() # Parameters forward to "self$attach" if present
#  2. qry.attach() # Modified in place (R6 reference class!) to set up output file characteristics
#  3. qry.load() # Parameters passed forward to "self$attach" if present; If file present, load contents

# If $attach is called with load==TRUE, a matching file must be present
# Need a better way of managing default parameters (e.g. via a model we're querying)
# Some conventions:
#    If FileName is relative, root it against ModelPath/QueryDir or ve.runtime/QueryDir
#      It must be of the form "<relative_path>/<QueryName>.VEqry"
#      Is used only to load. If QueryName not attached, disambiguate basename
#    QueryDir parameter is replaced by getRunParameter if not provided
#    self$QueryDir always contains the absolute path (resolved through ModelPath or ve.runtime)
#    self$QueryFile is built from self$QueryDir, self$QueryName when attached (and always used for Save)
#      No effort is made to reconcile FileName (input only) and self$QueryFile (output only)
#    QueryName replaces the basename of FileName
#    QuerySpec may or may not be accompanied by the others
#    ModelPath can either be a string (a filesystem path, relative to ve.runtime) or a VEModel
#      object; if NULL, it is replaced by ve.runtime. Says where to look for QueryDir.

# TODO: change the ModelPath stuff - there are two stages to the
# query: (1) find the query specification by name within QueryDir and load it

# Default query directory will be relative to the working directory
ve.query.init <- function(
  QueryName=NULL,    # If a consistent set of parameters, carry on to attach
  Model=NULL,        # A VEModel (or something accessible via openModel) to attach the query
  ModelPath=NULL,    # Root for directories (defaults to Model$modelPath or ve.runtime depending on Model)
  QueryDir=NULL,     # relative sub-directory for Queries (could be ".")
  FileName=NULL,     # relative to ModelPath/QueryDir or ve.runtime/QueryDir, or absolute
                     # Forces "load=TRUE"; also fills unattached QueryName; "FromFile"
  OtherQuery=NULL,   # provides fully-formed VEQuery for attach
  QuerySpec=NULL,    # A list of VEQuerySpec's (or another VEQuery) to initialize with
  load=FALSE         # If TRUE, build a FileName as ModelPath/QueryDir/QueryName.VEqry
                     # However, if FileName is provided, use that if absolute, othewise normalize
                     # with ModelPath/QueryDir, attaching ".VEqry" if need be. See ve.query.load
) {
  # Fill in useful filename default
  if ( !is.null(FileName) && is.null(QueryName) ) {
    QueryName <- sub("\\.[^.]*$","",basename(FileName))
  }
  if ( ! is.list(QuerySpec) && ! "VEQuery" %in% class(QuerySpec) ) {
    if ( "VEQuerySpec" %in% class(QuerySpec) ) {
      QuerySpec <- list(QuerySpec) # a single query spec becomes a list of one
    } else if ( !is.null(QuerySpec) ) {
      writeLogMessage("Unrecognized QuerySpec:")
      writeLogMessage(deparse(QuerySpec))
      QuerySpec <- NULL # Invalid query spec
    }
  }
  otherValid <- TRUE
  if ( ! is.null(OtherQuery) ) {
    if ( ! "VEQuery" %in% class(OtherQuery) ) {
      if ( is.list(OtherQuery) && length(OtherQuery)>0 ) {
        if ( ! "VEQuerySpec" %in% class(OtherQuery[[1]]) ) {
          OtherQuery <- list(VEQuerySpec$new(OtherQuery))
        }
        if ( "VEQuerySpec" %in% class(OtherQuery[[1]]) ) {
          QuerySpec <- OtherQuery
          OtherQuery <- NULL
        } else {
          otherValid <- FALSE
        }
      } else {
        otherValid <- FALSE
      }
    } else {
      # OtherQuery is a VEQuery object - clone its specification
      QuerySpec <- OtherQuery$getlist()
    }
  }
  if ( ! otherValid ) {
    stop(writeLogMessage(paste("Could not make a VEQuery from OtherQuery:",class(OtherQuery))))
  }

  # Set up model and model path
  if ( is.null(ModelPath) ) {
    if ( ! is.null(Model) ) {
      ModelPath <- Model$modelPath
    } else {
      ModelPath <- getRuntimeDirectory()
    }
  }

  # Add remaining defaults for saving the query out again
  self$attach(OtherQuery=OtherQuery,ModelPath=ModelPath,QueryDir=QueryDir,QueryName=QueryName)

  # if "load==TRUE" and null QuerySpec/FileName, build the output name and
  #   see if it already exists
  if ( !is.null(QuerySpec) || !is.null(FileName) || load ) {
    self$load(FileName=FileName,QuerySpec=QuerySpec,ModelPath=ModelPath,QueryDir=QueryDir)
  }

  # Evaluate what is present
  self$model(Model) # Requires QueryName; also looks up possibly existing VEQueryResults
  self$check()
  invisible(self$valid())
}

# Build self$QueryFile, which will be used for saving (and perhaps loading if not requested up front)
# Filename will NOT be disambiguated until we get to the save operation
# Just prepare a candidate absolute path to a valid directory
ve.query.attach <- function(OtherQuery=NULL, ModelPath=NULL, QueryDir=NULL, QueryName=NULL) {
  if ( !is.null(OtherQuery) ) {
    self$QueryDir <- OtherQuery$QueryDir
  } else {
    if ( is.null(ModelPath) || ! dir.exists(ModelPath) ) {
      ModelPath <- getRuntimeDirectory()
    }
    if ( is.null(QueryDir) ) {
      QueryDir <- visioneval::getRunParameter("QueryDir")
    }
    QueryDir <- normalizePath(file.path(ModelPath,QueryDir),winslash="/",mustWork=FALSE)
    if ( ! dir.exists(QueryDir) ) {
      QueryDir <- normalizePath(file.path(ModelPath),winslash="/",mustWork=FALSE)
    }
    self$QueryDir  <- QueryDir
  }
  
  if ( is.null(QueryName) ) {
    QueryName <- visioneval::getRunParameter("QueryFileName") # no extension
  }
  if ( grepl("\\.[^.]*$",QueryName) ) {
    QueryName <- sub("\\.[^.]*$","",QueryName) # No extension on QueryName
  }
  QueryFile <- normalizePath(file.path(self$QueryDir,QueryName),winslash="/",mustWork=FALSE)
  QueryFile <- paste0(QueryFile,".VEqry")

  self$QueryName <- QueryName
  self$QueryFile <- QueryFile

  return(self$QueryFile)
}

# Save a .VEqry representation (R dump) of the Query Specification
#  Loading that file will recreate the specification (possibly with
#  different QueryName, QueryDir, etc.) depending on options to the
#  VEQuery$new function.
# overwrite:
#  if TRUE, destroy any identically named file;
#  if FALSE, use serial number disambiguation for existing filename
ve.query.save <- function(saveTo=TRUE,overwrite=TRUE) {
  if ( is.logical(saveTo) ) { # save to stored name
    if ( saveTo ) {
      saveTo <- self$QueryFile;
    } else {
      saveTo <- "" # empty string dumps to console
    }
  }
  if ( ! is.character(saveTo) || ! nzchar(saveTo) ) { # saveTo console if not valid filename
    saveTo = ""
  }
  if ( nzchar(saveTo) && file.exists(saveTo) && ! overwrite ) {
    number <- 1
    actualFile <- saveTo
    while ( file.exists(actualFile) && number < 10 ) {
      # insert, e.g., "-1" in between root filename and extension
      # So MyQuery.VEqry becomes MyQuery-1.VEqry, MyQuery-2.VEqry, etc.
      actualFile <- sub("(-[[:digit:]])*(\\.[^.]+)$",paste0("-",number,"\\2"),actualFile)
      number <- number + 1
    }
    if ( file.exists(actualFile) && number>=10 ) {
      msg <- writeLog(
        c(
          paste("Too many files piled up trying not to overwrite",saveTo),
          "You should save with overwrite=TRUE and/or remove some of them."
        ),
        Level="error"
      )
      stop(msg)
    }
    saveTo <- actualFile
  }
  QuerySpec <- lapply(private$QuerySpec,function(spec) spec$QuerySpec)
  dump("QuerySpec",file=saveTo,control="niceNames") # dump the QuerySpec list as R code
  return(invisible(saveTo))
}

ve.query.clear <- function() {
  invisible( self$remove(1:length(private$QuerySpec)) ) # return what just removed...
}

ve.query.load <- function(FileName=NULL,QuerySpec=NULL,ModelPath=NULL,QueryDir=NULL) {
  if ( ! is.null(QuerySpec) ) {
    self$add(QuerySpec)
  } else {
    if ( is.null(FileName) && !is.null(self$QueryFile) ) {
      FileName <- self$QueryFile
    }
    else if ( is.null(FileName) && !is.null(ModelPath) && !is.null(self$QueryName) ) {
      # build a FileName as ModelPath/QueryDir/QueryName.VEqry
      if ( is.null(QueryDir) ) QueryDir <- ""
      FileName <- self$queryFile <- normalizePath(
        file.path(ModelPath,QueryDir,paste(self$QueryName,".VEqry")),
        winslash="/",mustWork=FALSE
      )
    }
    if ( !is.null(FileName) && file.exists(FileName) ) {
      # Load the query from FileName, commandeering the modelEnvironment
      ve.model <- visioneval::modelEnvironment() # Don't need to clear ve.model
      writeLog(paste("Loading Query:",FileName),Level="info")
      sys.source(self$QueryFile,envir=ve.model)
      self$add(ve.model$QuerySpec) # will interpret the list of lists as a list of VEQuerySpec
    }
  }
  return( self$check() )
}

ve.query.copy <- function(newName=NULL) {
  if ( is.null(newName) ) newName <- paste(self$QueryName,"(Copy)")
  new.query <- VEQuery$new(
    QueryName=newName,
    OtherQuery=self
  )
  return( new.query )
}

ve.query.check <- function(verbose=FALSE) {
  self$names(update=TRUE) # Make sure QuerySpec named list names are consistent
  self$CheckMessages <- character(0)
  query.names <- character(0)
  for ( spec in private$QuerySpec ) {
    if ( ! "VEQuerySpec" %in% class(spec) ) {
      self$CheckMessages <- c(
        self$CheckMessages,
        "QuerySpec contains an unknown type of object:",
        deparse(spec)
      )
      next
    }
    spec$check( Names=query.names ) # names list for validating functions
    query.names <- c( query.names,spec$Name )
    if ( ! spec$valid() ) {
      self$CheckMessages <- c(
        self$CheckMessages,
        paste0("Error checking specification '",spec$Name,"'"),
        spec$CheckMessages )
    }
  }
  return(self)
}

ve.query.valid <- function() {
  # summarize outcome of last check (as a logical)
  return( length(self$CheckMessages)==0 || all(!nzchar(self$CheckMessages)) )
}

ve.query.add <- function(obj,location=0,before=FALSE,after=TRUE) {
  # Really, add or update - if the name(s) of SpecListOrObject is/are already
  #   in QuerySpec, the existing value(s) will be over-written, regardless of location
  # If you use "update", specs not already present will be ignored with a warning.

  # Start by getting the specification list to add
  if ( is.list(obj) && all(sapply(obj,function(o)"VEQuerySpec"%in%class(o))) ) {
    # obj is already a list of VEQuerySpec objects (what we want)
    # But we want to clone it to avoid messing with its contents
    spec <- asSpecList(obj)
  } else {
    qry <- asQuery(obj) # Do deeper type conversions to build a query if needed
    # NOTE: if obj is already a VEQuery, it is returned as is. It is NOT copied.
    if ( ! qry$valid() ) {
      msg <- c("Cannot add to query:",qry$CheckMessages)
      writeLogMessage( c(msg,deparse(obj)) )
      stop(msg)
    }
    spec <- qry$getlist() # Clone the spec list from obj
  }

  # Validate and interpret "location", "before" and "after"
  currentSpec <- self$getlist() # clone existing spec list
  currentNames <- names(currentSpec)
  nameLen <- length(currentNames)

  if ( nameLen == 0 ) { # No existing query spec, so don't need to add
    newSpec <- spec
  } else {
    if ( is.character(location) ) {
      if ( ! location %in% currentNames ) {
        stop(writeLogMessage("Location is not in current QuerySpec"))
      } else {
        location <- which(currentNames %in% location)[1] # first instance of name in specification
      }
    }
    if ( ! is.numeric(location) ) { # invalid or NULL location just appends at the end
      location <- nameLen
      before <- ! ( after <- TRUE ) # before is FALSE, after is TRUE
    } else if ( before ) after <- FALSE # if before, then not after

    location <- if ( location < 1 ) {
      if ( before ) 1 else nameLen
    } else if ( location > nameLen ) {
      nameLen
    } else {
      location
    }

    # adjust list locations to respect before and after
    # location is the first position in namesAfter

    namesBefore <- if ( before && location==1 ) {
      character(0)
    } else if ( before ) { # location > 1
      currentNames[1:(location-1)]
    } else if ( after ) {
      currentNames[1:location]
    } else {
      character(0)
    }

    namesAfter <- if ( after && location==nameLen ) {
      character(0)
    } else if ( before ) {
      currentNames[location:nameLen]
    } else if ( after ) {
      currentNames[(location+1):nameLen]
    } else {
      character(0)
    }

    specNames   <- names(spec)

    namesAlready <- which(namesBefore %in% specNames)
    if ( length(namesAlready) > 0 ) namesBefore <- namesBefore  [ -namesAlready ]
    namesAlready <- which(namesAfter %in% specNames)
    if ( length(namesAlready) > 0 ) namesAfter <- namesAfter  [ -namesAlready ]

    # Then use the names to index the lists and create a new list, replacing the existing

    newSpec <- if ( length(namesBefore)>0 ) currentSpec[namesBefore] else list()
    newSpec <- c( newSpec, spec )
    if ( length(namesAfter)>0 ) {
      newSpec <- c(newSpec,currentSpec[namesAfter])
    }
  }
  private$QuerySpec <- newSpec

  self$check() # probably all we catch here are pre-existing errors and function order problems
  if ( length(self$CheckMessages)>0 ) {
    writeLogMessage("QuerySpec contains errors")
    print(self$CheckMessages) # a named character string
  }
  return(self)
}

ve.query.update <- function(obj) {
  # If it's not already in the QuerySpec, ignore it with a warning
  qry <- VEQuery$new(OtherQuery=obj,QueryName="Temp-Query") # obj is anything we can turn into a VEQuery
  if ( ! qry$valid() ) {
    msg <- writeLogMessage("Invalid VEQuerySpec:")
    writeLogMessage(deparse(obj))
    writeLogMessage(qry$CheckMessages)
    stop(msg)
  }
  q.names <- names(qry$getlist())
  s.names <- names(private$QuerySpec)
  extra.names <- ! q.names %in% s.names
  if ( any( extra.names ) ) {
    writeLogMessage("Warning","Names not in qry (use '$add'):",paste(q.names[extra.names],collapse=", "))
  }
  private$QuerySpec[ q.names ] <- qry$getlist() # replace list items in current spec
  self$check()
  return(self)
}

#' Convert a list of list-formatted specifications to a list of VEQuerySpec objects
#'
#' @param spec a list of lists where the inner lists are (probably) raw specifications. Running
#'   a list of VEQuerySpec's through this function will simply duplicate the specifications (a deep
#'   copy).
#' @return a named list of VEQuerySpec objects created from the raw specifications
#' @export
asSpecList <- function(spec) {
  spec.list <- lapply(spec,function(s) VEQuerySpec$new(s))
  names(spec.list) <- sapply(spec.list,function(s) if ( is.null(name <- s$Name) ) "" else name)
  return(spec.list)
}

#' Convert an R object of various compatible types to a VEQuery
#'
#' This function will convert various kinds of things into a VEQuery that can then be added to
#'   another query or otherwise used. If \code{obj} is already a VEQuery, it is NOT copied, but
#'   just returned as-is. That limits opportunities for infinite recursion since the $add function
#'   is called within VEQuery$new, so we need to be cautious about creating a new VEQuery here.
#'
#' @param obj The object to convert to the query's specifications. Can be a VEQuery, a VEQuerySpec,
#'   or a list of objects where each inner object is already (or is convertible to) a VEQuerySpec
#'   (that last format is how query specifications are manipulated by VEQuery$load or VEQuery$save).
#' @param QueryName The name to use for the returned VEQuery
#' @return A VEQuery constructed from the obj parameter
#' @export
asQuery <- function(obj,QueryName="Temp-Query") {
  if ( ! "VEQuery" %in% class(obj) ) {
    # check if it's a query spec or a list of query specs
    if ( is.list(obj) ) {
      qry.spec <- asSpecList(obj)
      # TODO: may have errors that need to be reported...
      if ( ! all(sapply(qry.spec,function(s) { "VEQuerySpec" %in% class(s) && s$valid() } )) ) {
        # Second, if it wasn't a list of specs, perhaps it is an individual spec
        qry.spec <- list()
        spec <- VEQuerySpec$new(obj) # Attempt to convert unknown list to a single spec
        if ( spec$valid() ) {
          name <- spec$QuerySpec$Name
          loc <- if ( is.null(name) ) 1 else name
          qry.spec[[loc]] <- spec;
        }
      }
    } else if ( "VEQuerySpec" %in% class(obj) ) {
      # Make a bare query spec into a one-element list
      qry.spec <- list(obj)
      names(qry.spec) <- obj$Name
    } else {
      stop("Cannot interpret object as query or specification:\n",deparse(obj))
    }
    qry <- VEQuery$new(QueryName="Temp-Query",OtherQuery=self,QuerySpec=qry.spec)
  } else {
    # obj is already another VEQuery
    qry <- obj
  }
  qry$check()
  return(qry)
}

ve.query.remove <- function(SpecToRemove) {
  # like subset, but remove the elements that match and return them
  extract <- private$QuerySpec[SpecToRemove]
  nm.ext <- names(extract)
  if ( any(is.na(nm.ext) | is.null(nm.ext) | !nzchar(names(extract))) ) {
    stop(writeLogMessage("VEQuery specification list has unnamed elements."))
  }
  private$QuerySpec[nm.ext] <- NULL
  invisible(
    VEQuery$new(
      QueryName="Removed-Specs",
      OtherQuery=self,
      QuerySpec=extract
    )
  )
}

ve.query.assign <- function(obj) {
  # replace the query spec with the other object's query spec
  qry <- asQuery(obj)
  if ( qry$valid() ) {
    private$QuerySpec <- qry$getlist() # just replace what's there with copy of other
  } else {
    msg <- "Cannot assign invalid query"
    writeLogMessage( c(msg,qry$CheckMessages) )
    stop(msg)
  }
  return(self)
}

ve.query.names <- function(update=FALSE) {
  if ( update ) {
    names(private$QuerySpec) <- sapply(
      private$QuerySpec,
      function(QS) if ( is.null(name <- QS$Name) ) "" else name
    )
  }
  return(names(private$QuerySpec))
}

ve.query.subset <- function(SpecToExtract) {
  # Return a new VEQuery that contains the indexed specifications
  subset <- private$QuerySpec[SpecToExtract]
  return(
    VEQuery$new(
      OtherQuery=self,
      QueryName="Subset",
      QuerySpec=subset
    )
  )
}

ve.query.index <- function(SpecToRemove) {
  self$subset(SpecToRemove) # do we need more to implement "["?
}

ve.query.spec <- function(SpecNameOrPosition) {
  # Use a name or number to find and return a VEQuerySpec object
  # Use the add function to put it back into the list with updates
  return( private$QuerySpec[[SpecNameOrPosition]] ) # with all the usual caveats about [[
}

ve.query.print <- function(details=FALSE) {
  # Consider some other elements like the query name, its file (if any), when it was
  # last run (available results); see ve.query.results
  if ( !is.null(self$QueryName) && nzchar(self$QueryName) ) {
    cat("Query Name:",self$QueryName,"\n")
  } else {
    cat("Unnamed Query.\n")
  }
  if ( !is.null(self$QueryDir) && nzchar(self$QueryDir) ) {
    cat("Query Directory:",self$QueryDir,"\n")
  } else {
    cat("No Query Directory.\n")
  }
  if ( !is.null(self$QueryFile) && nzchar(self$QueryFile) ) {
    cat("Query File:",self$QueryFile,"\n")
  } else {
    cat("No file for Query.\n")
  }
  if ( self$valid() ) {
    cat("Valid query with",length(private$QuerySpec),"elements\n")
    if ( length(private$QuerySpec) ) {
      if ( is.list(self$QueryResults) && length(self$QueryResults)>0 ) cat("Results are available.\n")
      print(self$names())
      if ( details ) for ( spec in private$QuerySpec ) print(spec)
    }
  } else if ( length(self$CheckMessages)>0 ) {
    cat("Query specification has errors:\n")
    for ( err in self$CheckMessages ) {
      cat(err,"\n")
    }
  } else if ( length(private$QuerySpec)==0 ) {
    cat("No query specifications.\n")
  } else {
    cat("Uninitialized query.\n")
  }
}

ve.query.getlist <- function(Geography=NULL) {
  ################################
  # Low-level function to get a copy of the specification list to run
  # We'll use this to get the actual list used internally to perform $run
  # The internal VEQUery$QuerySpec is a list of VEQuerySpec objects
  # The framework query function wants a regular R list
  ################################
  
  self$check()
  # Deep copy the current QuerySpec
  newSpec <- lapply(private$QuerySpec,function(s) VEQuerySpec$new(s))
  if ( ! is.null(Geography) ) {
    validity <- list()
    specResults <- character(0)
    for ( test.spec in newSpec ) {
      test.spec <- test.spec$setgeo(Geography)
      validity <- if ( ! test.spec$valid() ) {
        CheckMessages <- c(CheckMessages,
          paste0(test.spec$name(),": ",test.spec$CheckResults," (removed)")
        )
        append(validity,FALSE)
      } else append(validity,TRUE)
    }
    if ( length(CheckMessages)>0 ) {
      newSpec <- newSpec[validity] # Remove any invalid elements from newSpec
      writeLog(paste("Specifications invalid for Geography",Geography,":"),Level="warn")
      writeLog(paste(CheckMessages,collapse="\n"),Level="warn")
    }
  }
  # Make sure list names are up to date
  names(newSpec) <- sapply(newSpec,function(s) if ( is.null(name <- s$Name) ) "" else name)
  return( newSpec )
}

# List of standard Metadata. Can ask for any field from VEQuerySpec
# "Name" is always included to support cbind
defaultMetadata <- c("Units","Description")

# Other available metadata includes:
# Label,Instructions,Metric,DisplayName and XTicks (for the Visualizer)
# Names requested but not present are given a NULL value

# make a data.frame of all (and only) the valid query results
ve.query.extract <- function(Results=NULL, Measures=NULL, Years=NULL,GeoType=NULL,GeoValues=NULL, metadata=TRUE, data=TRUE, exportOnly=FALSE) {
  # "Results" is a list of VEResults (or a VEResultsList) from a VEModel
  # Visit each of the valid results in the Model (or Results list) and add its years as columns
  #  to the resulting data.frame, then return the accumulated result
  # if "metadata" is true, include default metadata in the output table
  # if "metadata" is a character vector, include only the named metadata items (the "Measure" name is
  #   always included whether or not metadata is requested)
  # if "data" is true, include data columns. If false, only generate metadata (used, e.g., by the
  #   visualizer JSON generation function)
  # exportOnly, if TRUE, only includes metrics with the Export attribute set (conventionally to
  #   TRUE, but we're considering only present/mssing)

  Results <- self$results(Results) # generate list of valid VEQueryResults
  if ( length(Results)==0 ) {
    stop(
      writeLog("No query results available; run the query first",Level="error")
    )
  }

  valueList <- lapply(Results,function(r) r$values() )
  valueList <- valueList[ ! sapply(valueList,is.null) ] # Should be no NULLs, but just in case...

  if ( is.character(metadata) ) {
    wantMetadata <- TRUE
  } else {
    wantMetadata <- metadata
    metadata <- if ( wantMetadata ) defaultMetadata else character(0)
  }
  if ( ! data && ! wantMetadata ) {
    wantMetadata <- TRUE
    metadata <- defaultMetadata
  }
  # metadata contains list of names to include from query specification

  # Filter values by Years
  if ( ! is.null(Years) ) {
    Years <- as.character(Years)
    valueList <- lapply(valueList,
      function(v) {
        yrs <- which( names(v) %in% Years )
        if ( length(v[yrs])==0 ) {
          stop(
            writeLog(paste("Years are not available in all query results:",Years,collapse=", "),Level="error")
          )
        }
        v[yrs]
      }
    )
  }

  # Filter list of measure names by Measures parameter (list of names)
  measureNames <- names(private$QuerySpec)
  if ( ! is.character(Measures) ) {
    seekMeasures <- measureNames
  } else {
    seekMeasures <- Measures[ which(Measures %in% measureNames) ]
  }
  if ( length(seekMeasures) == 0 ) {
    stop(
      writeLog(paste("Measures Not Found in Query.",seekMeasures,collapse=", "),Level="error")
    )
  }

  # Filter list of measure names to only those matching GeoType
  if ( is.character(GeoType) && GeoType %in% c("Marea","Azone","Bzone") ) {
    whichGeoMeasures <- which(
      sapply(
        private$QuerySpec[seekMeasures],
        function(m) return( GeoType=="Region" || GeoType %in% m$By )
      )
    )
    if ( length(whichGeoMeasures)==0 ) {
      stop(
        writeLog(paste("Requested GeoType is not found in requested Measures:",GeoType),Level="error")
      )
    }
    seekMeasures <- seekMeasures[ whichGeoMeasures ]
  }

  if ( exportOnly ) {
    whichExport <- sapply( private$QuerySpec[seekMeasures],
      function(m) {
        return( "Export" %in% names(m$QuerySpec) )
      }
    )
    seekMeasures <- seekMeasures[ whichExport ]
  }
    
  # Keep only measures that are being sought
  # Filter the measures using for loops rather than lapply to ensure names stay up to date
  for ( scenario in seq(valueList) ) {
    for ( year in names(valueList[[scenario]]) ) {
      valueList[[scenario]][[year]] <- valueList[[scenario]][[year]][seekMeasures]
    }
  } # TODO: verify that it works if we've accidentally filtered seekMeasures down to nothing

  # Build data.frame with requested measure results (filtered to specific GeoValues), and metadata
  results.df <- NULL
  Scenarios <- character(0)
  ScenarioYears <- character(0)
  ScenarioElements <- list()
  for ( value in valueList ) { # single set of filtered results
    ScenarioName <- attr(value,"ScenarioName")
    Elements <- attr(value,"ScenarioElements")
    for ( year in names(value) ) {
      theseResults <- makeMeasureDataframe(value[[year]],year,GeoValues,data,wantMetadata)
      writeLog(paste("Results for",ScenarioName,"Year",year),Level="info")
      writeLog(paste(theseResults$Measure,collapse=","),Level="info")

      # plus initial columns for first results are metadata if requested
      if ( is.null(results.df) ) {
        if ( wantMetadata && length(metadata>0) ) {
          results.df <- theseResults[,c("Measure",metadata)]
          metadata <- character(0) # only keep the metadata columns this one time
          wantMetadata <- FALSE
        } else {
          # after the first row, jut include the measures (not metadata)
          results.df <- data.frame(Measure=theseResults$Measure)
        }
      }
      if ( data ) {
        # Note: following line may add more than one column to results.df
        results.df[paste(ScenarioName,as.character(year),sep=".")] <- theseResults$Value
        Scenarios <- c( Scenarios, ScenarioName ) # dupes if multiple years for scenarios
        ScenarioYears <- c( ScenarioYears, year )
        ScenarioElements[[length(ScenarioElements)+1]] <- Elements # Scenario metadata
      } else break # Only gathering metadata
    }
    if ( ! data ) break # don't loop if not generating data
  }
  if ( is.null(results.df) ) results.df <- data.frame()
  if ( length(Scenarios)>0 ) {
    ScenarioColumns <- paste(Scenarios,ScenarioYears,sep=".")
    if ( length(Scenarios) != length(results.df[,ScenarioColumns]) || length(Scenarios) != length(ScenarioYears) ) {
      stop(
        writeLog("Scenarios don't match up with number of measure columns (VEModel/query.R circa line 718)",Level="error")
      )
    }
  } else ScenarioColumns <- character(0)
  return(
    structure(results.df,
      Scenarios=Scenarios,
      ScenarioYears=ScenarioYears,
      ScenarioColumns=ScenarioColumns,
      ScenarioElements=ScenarioElements
    )
  )
}

# Visualize query results
ve.query.visual <- function(QueryResults=list(), SaveTo=NULL, overwrite=TRUE) {

  # Get the query results
  if ( missing(QueryResults) ) {
    QueryResults <- self$extract(metadata=FALSE,exportOnly=TRUE)
  }
  if ( length(QueryResults)==0 )  {
    stop(
      writeLog("No query results available; run the query first",Level="error")
    )
  }

  # Set up structural parameters for query results
  measureNames <- QueryResults$Measure
  scenarioElements <- attr(QueryResults,"ScenarioElements") # list of scenario elments present in each set of query results

  if ( length(scenarioElements)==0 ) { # No scenario elements defined - can't run visualizer
    stop(
      writeLog("Query results do not have ScenarioElements - cannot visualize",Level="error")
    )
  }

  scenarioNames <- attr(QueryResults,"ScenarioColumns")
  QueryResults <- QueryResults[,scenarioNames]
  scenarioNames <- as.list(scenarioNames) # to include in VEdata below
  
  VEdata <- list()
  for ( d in 1:length(QueryResults) ) { # index into results columns, plus ScenarioElement attributes
    chk <- VEdata[[length(VEdata)+1]] <- c( Scenario=scenarioNames[[d]], scenarioElements[[d]], as.list(structure(QueryResults[[d]],names=measureNames)) )
  }

  # Do the Category config
  scenarios <- self$Model$scenarios()

  catconfig <- scenarios$categoryConfig()
  scenconfig <- scenarios$scenarioConfig()

  # scentitle
  scentitle <- paste(
    "Results for Query",paste0("'<strong>",self$QueryName,"</strong>'"),
    "on model",paste0("'<strong>",self$Model$modelName,"</strong>'"),
    paste0("(",format(Sys.time(),"%Y/%m/%d at %H:%M"),")")
  )

  # Need to keep the following consistent with the results extraction
  outputconfig <- self$outputConfig()

  jsonvars <- list(
    scentitle=jsonlite::toJSON(scentitle,pretty=TRUE),
    catconfig=jsonlite::toJSON(catconfig,pretty=TRUE),
    scenconfig=jsonlite::toJSON(scenconfig,pretty=TRUE),
    outputconfig=jsonlite::toJSON(outputconfig,pretty=TRUE),
    VEdata=paste("[", # process each result set onto a separate JSON line and present them all as an array
      paste(lapply( VEdata,function(d) jsonlite::toJSON(d,auto_unbox=TRUE) ),collapse=",\n"),
      "]",
      sep="\n"
    )
  )

  buildVisualizer <- ! missing(SaveTo)
  popupVisualizer <- is.null(SaveTo)
  if ( buildVisualizer ) {
    htmlRoot<-system.file("visualizer",package="VEModel")
    if ( popupVisualizer ) {
      # Pop up the visualizer "live" using the jrc package
      jrc::openPage(
        useViewer=FALSE,
        rootDirectory=htmlRoot, # try putting this back to htmlRoot...
        startPage=file.path(htmlRoot,"visualizer.html"),
        browser=getOption("browser")
      )
      # NOTE: jrc will do the toJSON conversion internally - just give it the R objects
      jrc::sendData("scentitle",scentitle)
      jrc::sendData("catconfig",catconfig,keepAsVector=TRUE) # keepAsVector -> otherwise flattens inconveniently
      jrc::sendData("scenconfig",scenconfig)
      jrc::sendData("outputconfig",outputconfig)
      jrc::sendData("VEdata",VEdata)
    } else {
      # construct a visualizer directory
      outputDir <- self$Model$setting("OutputDir")
      if ( ! is.character(SaveTo) ) {
        # Construct timestamped name for visualizer
        SaveTo <- paste0("Visualizer-",self$QueryName)
      }
      SavePath <- normalizePath(file.path(self$Model$modelResults,outputDir,SaveTo))
      if ( dir.exists(SavePath) && overwrite) {
        unlink(SavePath,recursive=TRUE)
      } else if ( ! overwrite ) {
        file.rename(SavePath,paste0(SavePath,"-",format(Sys.time(),"%Y_%m_%d-%H_%M")))
      }
      ve.runtime <- getRuntimeDirectory()
      writeLog(paste0("Creating Visualizer in ",gsub(ve.runtime,"",SavePath)),Level="info")
      if ( ! dir.exists(SavePath) ) dir.create(SavePath,recursive=TRUE) else {
        stop(writeLog("Renaming existing visualizer failed",Level="error"))
      }
      if ( ! dir.exists(SavePath) ) {
        stopMsg <- writeLog("Failed to create visualizer directory:",Level="error")
        writeLog(SavePath,Level="error")
        stop(stopMsg)
      }
      for ( f in dir(htmlRoot,recursive=TRUE) ) {
        dest <- file.path(SavePath,f)
        showPath <- gsub(ve.runtime,"",dirname(dest))
        writeLog(paste("Copying",f,"to",showPath),Level="info")
        if ( ! dir.exists(dirname(dest)) ) {
          writeLog(paste("Creating directory:",showPath))
          dir.create(dirname(dest),recursive=TRUE)
        }
        from <- file.path(htmlRoot,f)
        writeLog(paste("From:",from),Level="info")
        writeLog(paste("  To:",gsub(ve.runtime,"",dest)),Level="info")
        file.copy( from, dest )
      }
      visualizer.js <- file.path(SavePath,"visualizer.js")
      cat(file=visualizer.js,"// Visualizer variables for Query",self$QueryName,"on Model",self$Model$modelName,"\n")
      for ( js in names(jsonvars) ) {
        cat( file=visualizer.js, paste(js,"=",jsonvars[[js]],";\n"), append=TRUE )
      }
      writeLog("Saved visualizer and data to:",Level="warn")
      writeLog(gsub(ve.runtime,"",SavePath),Level="warn")
    }
  }
  return(invisible(jsonvars))
}

ve.query.outputconfig <- function() {
  outputconfig <- lapply(
    private$QuerySpec,
    function(spec) {
      spec$outputConfig() # process Export tag if present
    }
  )
  outputconfig <- structure(
    names=NULL,
    outputconfig[ sapply(outputconfig,function(x) !is.null(x)) ]
  )
}

# Put the results of the query into a csv by default (or a data.frame, or sql, or other
#  tabular receptacle.)
# Option to save the data.frame in some tabular output format (data.frame, csv, sql)
# Export should be able to filter by Measure name, Year of Data (some scenarios will have more than
# one year), and specific ModelStage name (for Results).
# TODO: "query" function on VEModel should be able to limit to certain ModelStages (in which case
# don't consider "Reportable").
ve.query.export <- function(format="csv",OutputDir=NULL,SaveTo=NULL,Results=NULL,Years=NULL,GeoType=NULL,GeoValues=NULL) {
  needOutputDir <- missing(OutputDir) || ! is.null(OutputDir)
  # TODO: Query extract template should be called QueryExportTemplate
  # TODO: The template should probably belong to the ViEIO export format
  # 
  if ( ! is.null(self$Model) ) {
    OutputPath <- self$Model$modelResults # Absolute path to ResultsDir for model
    if ( needOutputDir ) OutputDir <- self$Model$setting("OutputDir")
    QueryExtractFile <- self$Model$setting("QueryExtractTemplate")
  } else if ( !is.null(Results) ) {
    firstResult <-Results$results()[[1]] 
    OutputPath <- firstResult$resultsPath # Results for this scenario
    Param_ls <- firstResult$ModelState()$RunParam_ls
    if ( needOutputDir ) OutputDir <- visioneval::getRunParameter("OutputDir",Param_ls=Param_ls)
    QueryExtractFile <- visioneval::getRunParameter("QueryExtractTemplate",Param_ls=Param_ls)
  } else {
    stop( writeLog("No Query Results to export.",Level="error") )
  }
  OutputPath <- file.path(OutputPath,OutputDir)
  if ( ! dir.exists(OutputPath) ) dir.create(OutputPath)

  if ( format != "csv" ) stop( writeLog("Currently only supporting .csv export",Level="error") )

  if ( missing(Results) || is.null(Results) ) {
    if ( ! is.null(self$Model) ) Results <- self$Model$results()
  } else {
    stop( writeLog("No results to query",Level="error") )
  }

  # Extract results into data.frame
  Results_df <- self$extract(Results=Results,Years,GeoType=GeoType,GeoValues=GeoValues)

  # Then write the data.frame
  if ( ! is.character(SaveTo) ) {
    ExtractFile <- QueryExtractFile
    ExtractFile <- stringr::str_replace(ExtractFile,"%queryname%",self$QueryName)
    ExtractFile <- stringr::str_replace(ExtractFile,"%timestamp%",format(Sys.time(),"%Y_%m_%d-%H_%M"))
    # TODO: support other output file formats (different extension, alternate way to format SaveTo)
    if ( ! grepl("\\.csv$",ExtractFile) ) ExtractFile <- paste0(ExtractFile,".csv")
    SaveTo <- file.path(OutputPath,ExtractFile)
  } else {
    SaveTo <- file.path(OutputPath,SaveTo)
  }
  utils::write.csv(Results_df,file=SaveTo,row.names=FALSE)
  return(invisible(Results_df))
}

# Helper function to locate OutputDir given Results (VEModel or VEResults) for exporting query
# data.frame
# TODO: also use this function to locate the OutputDir/extract file name
exportDir <- function(model=NULL,results=NULL) {
  exportDir <- if ( class(model) == "VEModel" ) {
    file.path(model$modelPath,model$setting("ResultsDir"))
  } else if ( class(results) == "VEResults" ) {
    dirname(dirname(normalizePath(results$resultsPath)))
  } else {
    stop( writeLog("Cannot export: no model or results to locate output directory.",Level="error") )
  }
  return(exportDir)
}

# Find any existing VEQueryResults associated with (model) Results
# The Results argument here is a list of model results (or a VEResultsList)
# The "Results" element generated by doQuery includes:
#   "Timestamp"
#   "Values" (a named list of metrics)
#   "Specifications" (redundant list of specs used to create "Values")
# If results are not available for one of the VEResults, Results element is NULL
ve.query.results <- function(Results=NULL, Reload=FALSE) {
  # Figure out where to look for results
  if ( missing(Results) || is.null(Results) ) {
    if ( "VEModel" %in% class(self$Model) ) {
      Results <- self$Model$results()
      # Output file was set when Model was attached
    } else {
      return( list() ) # empty list if query has not been attached to a model
    }
  } else {
    self$outputfile() # will use self$Model output file if model is available, or global
  }

  if ( "VEResultsList" %in% class(Results) ) {
    # downshift to list of VEResults
    Results <- Results$results()
  }
  if ( ! is.list(Results) && "VEResults" %in% class(Results) ) {
    # Handle pathological case of only one stage with Results
    Results <- list(VEResults)
  }
  if ( Reload || is.null(self$QueryResults) || length(self$QueryResults) < length(Results) ) {
    private$reload( Results ) # pulls up available query results
    if ( length(self$QueryResults) == 0 ) self$QueryResults <- NULL # No valid results
  }
  return(self$QueryResults) # Just return the results that are actually present
}

ve.query.reload <- function( Results ) {
  # Expects a list of VEResults (not VEResultsList)
  # Returns the (possibly invalid) list of VEQueryResults (one for each of Results argument)
  # Also updates self$QueryResults to include only the valid query results
  allQueryResults <- lapply( Results,
    function(r) {
      VEQueryResults$new(Query=self,VEResults=r)
    }
  )
  validResults <- sapply( allQueryResults, function(qr) qr$valid() )
  self$QueryResults <- allQueryResults[ validResults ]
  return(allQueryResults) # use to get status of full list of Results parameter
  # Invalid query results will have NULL Results
}

# Attach a Model and pre-load QueryResults if present
ve.query.model <- function( Model=NULL ) {
  if ( ! is.null(Model) ) {
    self$Model <- Model  # update attached model
    self$outputfile()    # Set file name for query results output
    self$results()       # cache results if available
  }
  return(self$Model)
}

# set or override the query's output filename
# (used for raw query results in the resultsPath).
ve.query.outputfile <- function(OutputFile=NULL) {
  if ( ! is.character(OutputFile) ) {
    if ( ! is.null(self$Model) ) {
      OutputFile <- self$Model$setting("QueryOutputTemplate")
    } else {
      OutputFile =  visioneval::getRunParameter("QueryOutputTemplate")
    }
  }
  OutputFile <- stringr::str_replace(OutputFile,"%queryname%",self$QueryName)
  OutputFile <- stringr::str_replace(OutputFile,"%timestamp%",format(Sys.time(),"%Y_%m_%d-%H_%M"))
  if ( ! grepl("\\.Rda(ta)?$",OutputFile) ) OutputFile <- paste0(OutputFile,".Rda")
  return(  self$QueryResultsFile <- OutputFile )
}

ve.query.run <- function(
  Model      = NULL,  # Attached model on whose results to run (or a VEResultsList)
  Force      = FALSE   # If true, re-run the query for all results even if they are up to date
  )
{
  writeLog(paste("Running query:",self$Name),Level="warn")
  if ( missing(Model) || is.null(Model) ) {
    Model <- self$Model # Use attached model if available
    if ( is.null(Model) ) stop( writeLog("No model results available to run query",Level="error") )
  }
  queryingModel <- FALSE
  if ( "VEModel" %in% class(Model) ) {
    queryingModel <- TRUE
    self$model(Model)
    Results <- Model$results() # Convert model Reportable stages into a VEResults or VEResultsList if more than one
    if ( "VEResultsList" %in% class(Results) ) {
      Results <- Results$results()
    } else if ( "VEResults" %in% class(Results) ) {
      Results <- list(Results)
    } else {
      stop(
        writeLog(paste("Unknown type for Model$results():",class(Results),collapse=","),Level="error")
      )
    }
  } else if ( "VEResultsList" %in% class(Model) ) {
    Results <- Model$results() # Downshift to plain list of VEResults
    if ( class(Results) != "list" || class(Results[[1]])!="VEResults" ) {
      stop( writeLog("Program error: VEResultsList won't convert to list of VEResults",Level="error") )
    }
  } else {
    print(class(Model))
    stop( writeLog(paste0("No results in Model Parameter: ",class(Model)),Level="error") )
  }
  if ( ! is.list(Results) ) {
    stop(
      writeLog(
        paste0("Program Error: list of results is not a list: ",class(Results)),
        Level="error"
      )
    )
  }
  validResults <- sapply(Results,function(r) r$valid())
  if ( !all(validResults) ) {
    for ( result in Results[!validResults] ) {
      writeLog(paste("Model Results",result$Name,"is Invalid; not running query."),Level="warn")
    }
  }
  Results <- Results[validResults]
  if ( length(Results)==0 ) {
    stop( writeLog( "No valid model results to query",Level="error" ) )
  }

  # Check and compile the specifications; abort if not valid
  self$check()

  if ( ! Force && length(Results)>0 ) {
    # Reload cached results (updates self$QueryResults and check for validity)
    # private$reload returns all the Results, whether or not they
    # have query results
    writeLog("Checking for cached query results",Level="warn")
    upToDate <- sapply( private$reload(Results) ,
      function(r) {
        if ( ! "VEQueryResults" %in% class(r) || ! r$valid() ) return(FALSE)
        tempEnv <- new.env()
        load(r$Path,envir=tempEnv)
        Timestamp <- tempEnv$Timestamp # Timestamp when query results were generated
        outOfDate <- ( 
          is.null(Timestamp) ||
          is.null(r$Source$ModelState()$LastChanged) ||
          Timestamp < r$Source$ModelState()$LastChanged
        )
        return( ! outOfDate )
      }
    )
    ResultsToUpdate <- Results[ ! upToDate ]
    if ( all(upToDate) ) {
      writeLog("Query results are all up to date.",Level="info")
    } else {
      writeLog(paste("Query results for",length(ResultsToUpdate),"model results out of",length(Results),"will be updated."),Level="info")
    }
  } else {
    # update everything
    ResultsToUpdate <- Results
    writeLog("Query results will all be updated.",Level="info")
  }
      
  # Run the query on any out-of-date results
  # ResultsToUpdate is a list of VEResults
  if ( (numResults<-length(ResultsToUpdate)) > 0 ) {
    writeLog(paste(paste("Updating",numResults,"Results:"),paste(sapply(ResultsToUpdate,function(x)x$Name),collapse="\n"),sep="\n"),Level="warn")
    doQuery(
      Results=ResultsToUpdate,         # list of VEResults objects for which to generate results
      Specifications=self$getlist(),   # A list of VEQuerySpec
      QueryFile=self$outputfile(),     # File into which to save each query result (in Results$resultsPath)
      Timestamp=Sys.time()             # Compared to ModelState last update to see if Query results are up to date
    )
    # Results of doQuery are written to the QueryFile in Results$resultsPath
    # self$results will reload them
  } else {
    writeLog("No results to update.",Level="info")
  }

  # Update self$QueryResults to the list of VEQueryResults that were processed in this run and
  # return those
  QueryResults <- if ( queryingModel ) self$results() else self$results(Results)
  return( invisible(QueryResults) )
}

# One of these is constructed by VEModel$query or by opening a query specification file
# Perhaps have some S3 generic functions defined...

#' @importFrom R6 R6Class
#' @export
VEQuery <- R6::R6Class(
  "VEQuery",
  public = list(
    # Data
    QueryDir = NULL,                # Default directory from which to load queries (see initialize and load)
    QueryResultsFile = NULL,        # Name of file to hold query results (in each results path)
    QueryFile = NULL,               # Name of file holding VEQuery dump (load/save)
    CheckMessages = c(Init="Not checked"), # Named character vector of check errors from last self$check
    QueryResults = NULL,            # cache of QueryResults (load from QueryResultsFile or re-generate)
    QueryName = NULL,               # Display name for Query (default basename(Query-File)
    Model = NULL,                   # Attached model on which to run query

    # Methods
    # TODO: rethink using "attached model" or "attached ResultsList" variation
    # Running the query will create a data file for each ModelStage/VEResults
    # So it really just works on a list of VEResults in all cases (we can get such a list from the
    # VEModel)
    initialize=ve.query.init,           # initialize a new VEQuery object
    save=ve.query.save,                 # With optional file name prefix (this does an R 'dump' to source)
    attach=ve.query.attach,             # Install consistent QueryName, QueryDir from request
    clear=ve.query.clear,               # Throw away the query specifications
    load=ve.query.load,                 # Using installed file parameters, see if there's a file and load it
    model=ve.query.model,               # Associate this query with a model (to run, or to generate or open results)
    copy=ve.query.copy,                 # Duplicates the query (for further editing) - new query is "unattached"
    check=ve.query.check,               # Make sure all the specs work (including Function order)
    valid=ve.query.valid,               # Just report validation results (run $check first)
    add=ve.query.add,                   # Add a VEQuerySpec (or list of them) to the VEQuery
    update=ve.query.update,             # Update an existing VEQuerySpec in the VEQuery
    remove=ve.query.remove,             # Remove a VEQuerySpec from the VEQuery
    assign=ve.query.assign,             # Assign the QuerySpec from one VEQuery to another
    names=ve.query.names,               # List (or update) names on internal QuerySpec list
    subset=ve.query.subset,             # Return a new VEQuery with a subset (or reordered) list of specs
    `[`=ve.query.index,                 # Alternate notation for subset
    spec=ve.query.spec,                 # Return a single VEQuerySpec from the list
    print=ve.query.print,               # List names of Specs in order, or optionally with details
    getlist=ve.query.getlist,           # Extract he QuerySpec list (possibly filtering geography) for $run)
    results=ve.query.results,           # report results of last run (available stage files)
    extract=ve.query.extract,           # get requested (or all) results into a data.frame
    export=ve.query.export,             # Export query results to .csv or something else (uses $extract)
    outputfile=ve.query.outputfile,     # expand the file name into which to save QueryResults
    run=ve.query.run,                   # results are cached in self$QueryResults
    visual=ve.query.visual,             # visualize query results: same as export(format="visual",...)
    outputConfig=ve.query.outputconfig  # generate outputconfig for visualizer (using "Export" spec)
  ),
  private = list(
    QuerySpec=list(),                   # access via public functions - list of VEQuerySpec objects
    reload=ve.query.reload              # recreate self$QueryResults
  )
)

################################################
#
# Define VEQueryResults

ve.queryresults.init <- function(Query=NULL,VEResults=NULL) {
  # expect VEResuls$resultsPath to be normalized path
  self$Source <- VEResults # "Source" is a VEResults object
  if ( is.null(Query) || is.null(self$Source) ) return()

  self$Path <- file.path(self$Source$resultsPath,Query$QueryResultsFile)
  self$Results <- if ( file.exists(self$Path) ) {
    # "Results" is the named list consisting of output from doQuery
    results.env <- new.env() # where to load query results
    items <- try( load(self$Path,envir=results.env) )
    if ( class(items) != "try-error" && "Values" %in% items ) {
      as.list(results.env) # placed in self$Results
    } else NULL
  } else NULL
}

ve.queryresults.valid <- function() {
  resultsValid <- ( is.list(self$Results) && ! is.null(self$Source) )
  resultsValid <- ( resultsValid && all(c("Timestamp","Values","Specifications") %in% names(self$Results)) )
  return( resultsValid )
}

ve.queryresults.values <- function() {
  if ( self$valid() ) {
    structure(self$Results$Values,
      ScenarioName=self$Source$Name,
      ScenarioElements=self$Source$elements() # expanded list of ScenarioElements
    )
  } else NULL
}

ve.queryresults.print <- function() {
  if ( self$valid() ) {
    cat("Query Results for",self$Source$Name,"\n")
    cat("Generated:",format(self$Results$Timestamp,"%Y/%m/%d at %H:%M"),"\n")
    cat("Measures:",paste(names(self$Results$Specifications),collapse=","),"\n")
  } else cat("No results yet.")
}

VEQueryResults <- R6::R6Class(
  "VEQueryResults",
  public = list(
    # Data
    Results = NULL,  # Query results structure as generated by doQuery
    Source  = NULL,  # VEResults object (model run results) used to generate these (Query) Results
    Path = NULL,     # Path to the file that was loaded into self$Results

    # Methods
    initialize = ve.queryresults.init,
    valid = ve.queryresults.valid,
    values = ve.queryresults.values,
    print = ve.queryresults.print
  )
)

################################################
#
# Define VEQuerySpec

# S3 class wrapping a single query spec
# Could construct an edit function for it...
# Helper function creates an individual spec from parameters
# VEQuerySpec

spec.types <- c("Summarize","Function")
ve.spec.type <- function() {
  self$check()
  if ( self$valid() && is.list( self$QuerySpec ) ) {
    st <- spec.types %in% names(self$QuerySpec)
    if ( any(st) ) return(spec.types[st])
  } else {
    return( "Invalid" )
  }
}

ve.spec.init <- function(other=NULL) {
  # Create from another query spec, or a list object, or bare
  if ( ! is.null(other) ) {
    # A bare VEQuerySpec can be filled in with VEQuerySpec$update
    if ( "VEQuerySpec" %in% class(other) ) {
      # Get the list from the QuerySpec
      self$QuerySpec <- other$QuerySpec # it's just a standard R list
      self$check()
    } else if ( is.list(other) ) {
      # If it's a list, assume it's a list of specifications
      self$QuerySpec <- other
      self$check()
    } else {
      # Can't figure out what to do - an error
      self$QuerySpec <- list()
      self$CheckMessages <- c("Unknown source:",deparse(other))
    }
  }
}

deepPrint <- function(ell,join=" = ",suffix="",newline=TRUE) { # x may be a list
  result <- if ( is.list(ell) || ( ! is.null(names(ell)) && length(ell)>1 ) ) {
    index <- if ( !is.null(names(ell)) ) names(ell) else 1:length(ell)
    if ( newline ) {
      inner <- "\n"
      prefix <- paste(rep("   ",2),collapse="",sep=".")
    } else {
      inner <- " "
      prefix = ""
    }
    inner <- paste(
      paste(
        inner,prefix,
        unlist(lapply( index,m=ell,
          FUN=function(n,m) {
            r <- m[[n]]
            return(paste0(n,join,deepPrint(r,suffix="",join=":",newline=FALSE)))
          })
        ),
        collapse="",
        sep=""
      ),
      sep=""
    )
    paste0(inner,suffix)
  } else ell
  return(result)
}

specRequiredElements <- c(
  # These elements are "required" (except that "Function" and "Summarize" are
  # treated specially - there must be exactly one of them in the QuerySpec)
  "Name", "Description", "Units", "Function", "Summarize", "Require", "RequireNot"
)

specOptionalElements <- c( # Contains "known" elements that generate no warning if present
  # First row are optional for use by the Visualizer:
  "Export"
)

specSummarizeElements <- c(
  "Expr", "Units", "By", "Breaks", "BreakNames", "Table", "Key", "Group"
)

ve.spec.print <- function() {
  # TODO, more class-specific result
  # If function, print its expression
  # If summarize, print its elements (nicely)
  if ( ! self$valid() ) {
    cat("Specification is not valid:\n")
    cat(paste(self$CheckMessages,collapse="\n"),"\n")
  } else {
    spec.print <- function(s,spec) { cat("   ",s,"= "); cat(deepPrint(spec[[s]]),"\n") }
    nm.req <- specRequiredElements[ specRequiredElements %in% names(self$QuerySpec) ]
    dummy <- lapply(nm.req,spec=self$QuerySpec,spec.print)
    nm.opt <- specOptionalElements[ specOptionalElements %in% names(self$QuerySpec) ]
    if ( length(nm.opt) > 0 ) {
      cat("   Optional Spec Elements:\n")
      dummy <- lapply(nm.opt,spec=self$QuerySpec,spec.print)
    }
  }
}

# modeled after helpers in visioneval core package
# extract names from a call or expression
# return "unlisted", which amounts to a character vector
getNames <- function(AST) {
  if ( length(AST)==1 ) {
    if ( is.name(AST) ) deparse(AST) else NULL # return character rendition of R symbol
  } else {
    unlist( lapply(AST, function(x) getNames(x) ) )
  }
}

ve.spec.check <- function(Names=NULL, Clean=TRUE) {
  # Check the query spec and return a corrected version, with errors in self$CheckMessages
  # if Names is a character string, check that a Function spec only refers
  #   to defined names.
  # if Clean is FALSE, leave unknown names behind in the Specification
  #   Mostly for debugging

  # Add a Geography field based on what's in "By" - "Region" by default, otherwise
  # whichever small geography is in the "By" field.

  #   # TODO: While we're doing the following, also extract the query Geography type
  #   #       and set it to Region, Marea, Azone, Bzone as appropriate and set that
  #   #       into the specification for quick reference.
  #   # TODO: move the following computation to the initial processing of Specifications
  #   # and set BreakNames if usingBreaks and BreakNames don't exist.
  #   # Don't do it over and over for each measure.
  #   if ( usingBreaks ) {
  #     TODO: check here or before that explicit BreakNames is same length as Breaks...
  #     if ( "BreakNames" %in% names(sumSpec) ) {
  #       breakNames <- sumSpec$BreakNames[[sumSpec$By[1]]]
  #     } else {
  #       breakNames <- as.character(sumSpec$Breaks[[sumSpec$By[1]]])
  #     }
  #     breakNames <- c("min",breakNames)
  #   }

  self$CheckMessages <- character(0)
  if ( length(self$QuerySpec)==0 ) {
    self$CheckMessages <- "Empty"
    return(self)
  }
  nm.test.spec <- names(self$QuerySpec) # may be NULL
  if ( is.null(nm.test.spec) ) {
    self$CheckMessages <- c(self$CheckMessages,"Specification list elements are unnamed")
  } else {
    if ( "Name" %in% nm.test.spec ) {
      self$Name <- self$QuerySpec$Name
      if ( is.character(Names) && self$Name %in% Names ) {
        self$CheckMessages <- c(self$CheckMessages,paste("Duplicated Specification Name:",self$Name))
      }
    } else {
      self$Name <- "Unnamed"
      self$CheckMessages <- c(self$CheckMessages,paste(self$Name,"Specification"))
    }
    if ( Clean ) {
      self$QuerySpec[ ! nm.test.spec %in% c(specRequiredElements,specOptionalElements) ] <- NULL
    } else {
      have.names <- nm.test.spec %in% c(specRequiredElements,specOptionalElements)
      spec.valid <- length(have.names)>0 && all(have.names)
      if ( ! spec.valid ) {
        self$CheckMessages <- c(
          self$CheckMessages,
          paste("Unknown Spec elements are present:",paste(nm.test.spec[!have.names],collapse=","))
        )
      }
    }

    if ( "Summarize" %in% names(self$QuerySpec) ) { # Functions are checked by VEQuery$check
      nm.test.summarize <- names(self$QuerySpec$Summarize)
      if ( Clean ) { # Remove unknown names from Specification
        self$QuerySpec$Summarize[ ! nm.test.summarize %in% specSummarizeElements ] <- NULL
      } else {
        have.names <- nm.test.summarize %in% specSummarizeElements;
        spec.valid <- length(have.names)>0 && all(have.names)
        if ( ! spec.valid ) {
          self$CheckMessages <- c(
            self$CheckMessages,
            paste("Unrecognized Specification Elements:",paste(nm.test.spec[!have.names],collapse=","))
          )
        }
      }
      checkedSpec <- visioneval::checkQuerySpec(QuerySpec=self$QuerySpec$Summarize)
      if ( length(checkedSpec$Errors)>1 || any(nzchar(checkedSpec$Errors)) ) {
        self$CheckMessages <- c(
          self$CheckMessages,
          msg<-paste("Error(s) in Query Specification:",self$Name,"\n"),
          checkedSpec$Errors
        )
      }
      self$CompiledSpec <- checkedSpec$CompiledSpec
    } else if ( "Function" %in% names(self$QuerySpec) ) {
      if ( is.character(Names) ) {
        # Function dimensions should take care of themselves, though eventually we should have more
        # proactive management of dimensions by looking up the Symbols and their dimensions in the
        # Specifications list. Also perhaps look up the Symbol Geography and make sure they're
        # all compatible (if small geography for one, must be the same for others - or "Region"
        # ok). Then set a "Geography" element for this spec based on Symbols used.
        Symbols <- unique(getNames(str2lang(self$QuerySpec$Function)))
        Symbols <- Symbols [ ! Symbols %in% Names ]
        # Make sure the parent.frame includes names of other specs...
        Symbols <- Symbols [ ! sapply(Symbols,exists,envir=parent.frame()) ]
        if ( length(Symbols)>0 ) {
          self$CheckMessages <- c(
            self$CheckMessages,
            paste("Function refers to undefined names:",Symbols,collapse=", ")
          )
        }
      }
    } else {
      self$CheckMessages <- c(
        self$CheckMessages,
        "Unknown Specification Type (Valid='Summarize' or 'Function')"
      )
    }

    # process Export / visualizer elements (read-only)
    if ( "Export" %in% nm.test.spec ) {
      # Set up plausible defaults for Export tag
      Export <- self$QuerySpec$Export  # NULL if not present - if present, it's a list of visualizer elements
      if ( is.null(Export$Instructions) ) {
        Export$Instructions <- self$QuerySpec$Description
      }
      if ( is.null(Export$DisplayName) ) {
        Export$DisplayName <- self$QuerySpec$Name
      }
      if ( is.null(Export$Metric) ) {
        Export$Metric <- ""
      }
      if ( is.null(Export$XTicks) ) {
        Export$XTicks <- 5
      }
      if ( is.null(Export$Label) ) {
        Export$Label <- Export$DisplayName # either explicit or itself defaulted further
      }
      self$QuerySpec$Export <- Export
    }
  }
  return(self)
}

ve.spec.valid <- function() {
  return ( length(self$CheckMessages)==0 || all(!nzchar(self$CheckMessages)) )
}

ve.spec.copy <- function() {
  return ( VEQuerySpec$new(self) )
}

cleanSpecNames <- function(self)
{
  self$QuerySpec[ ! names(self$QuerySpec) %in% c(specRequiredElements,specOptionalElements) ] <- NULL
  if ( "Summarize" %in% names(self$QuerySpec) ) {
    self$QuerySpec$Summarize[ ! names(self$QuerySpec$Summarize) %in% specSummarizeElements ] <- NULL
  }
  return(self)
}

# TODO: the following is still rather a mess.
# Need a function to build a "Summarize" or "Function" sub-spec
# Then go in and update specific elements
# Perhaps this should be lower priority - just send people back to edit the .VEqry text file.
ve.spec.update <- function(
  # Replaces this QuerySpec from another one
  # See ve.spec.check: can pass a QuerySpec through this with no
  #   override parameters to get the names filtered to just those
  #   that are legal.
  QuerySpec=list(), # a list or another VEQuerySpec
  Name = NULL,
  Description = NULL,
  Units = NULL,
  Require = NULL,
  RequireNot = NULL,
  Function = NULL,
  Summarize = NULL

  # The following are replaced by providing a named list as the Summarize parameter
  #   Expr = NULL, # Relevant to Summarize or Function
  #   Units = NULL, # Rest are Ignored if not Summarize
  #   By = NULL,
  #   Breaks_ls = NULL,
  #   Table = NULL,
  #   Key = NULL,
  #   Group = NULL
) {
  # Then process any overrides for those names
  override <- list(
    Name = Name,
    Description = Description,
    Units = Units,
    Require = Require,
    RequireNot = RequireNot,
    Function = Function,
    Summarize = Summarize
  )
  override <- override[ ! sapply(override,is.null) ]
  if ( length(override)>0 ) {
    QuerySpec[ names(override) ] <- override # replace list elements with named arguments
  }

  # Now evaluate the sub-types (Summarize and Function)
  if ( "Summarize" %in% names(QuerySpec) && is.list(QuerySpec$Summarize) ) {

    # Can't also have "Function"
    if ( "Function" %in% names(QuerySpec) ) QuerySpec["Function"]<-NULL

    # Capture override of summarize elements
    override <- list(
      Expr      = Summarize$Expr,
      Units     = Summarize$Units,
      By        = Summarize$By,
      Breaks_ls = Summarize$Breaks_ls,
      Table     = Summarize$Table,
      Key       = Summarize$Key,
      Group     = Summarize$Group
    )
    override <- override[ ! sapply(override,is.null) ]
    if ( length(override)>0 ) {
      summarize_ls <- QuerySpec$Summarize
      summarize_ls[ names(override) ] <- override # replace list elements
      QuerySpec$Summarize <- summarize_ls
    }
  }
  # Merge the resulting QuerySpec into self$QuerySpec
  self$QuerySpec[ names(QuerySpec) ] <- QuerySpec;

  # Then return the checked version of self
  return( self$check() ) # does the check, then returns "self"
}
    
# data helper
ve.small.geo <- c("Marea","Azone","Bzone")

# TODO: In addition to the robustness elements, this function should just adjust the "By" dimension
# associated with Geography. Region has no geographic "By", otherwise we generate summaries for
# Marea, Azone or Bzone. We need to make sure the geographic breakdown happens appropriately with
# "Breaks" - Geography is first level of breakdown, Breaks are always second level so the
# dimensions can be subsetted and processed appropriately during export.
# TODO: Geography adjust doesn't do Bzone but should - there's no reason not to break things
# out by Bzone (except there may be many of them!).
ve.spec.setgeo <- function(Geography=NULL) {
  # Return a new geography-adjusted VEQuerySpec
  # TODO: this is not especially robust yet...
  # In particular, we're not checking that fields are avalilable at that level
  #   and joining may or may not work right...

  test.spec <- self$copy()
  if ( ! is.null(Geography) && test.spec$type() == "Summarize" ) {
    test.sum <- test.spec$QuerySpec[["Summarize"]] # pull out the sub-list
    if ( Geography["Type"] == "Region" ) {
      # Region: remove Marea or Azone from "By" and "Units", if present
      if ( "By" %in% names(test.sum) ) {
        test.by <- test.sum[["By"]]
        any.geo <- ( test.by %in% ve.small.geo )
        if ( any ( ! any.geo ) ) { # By includes tables other than geography
          # remove geography but leave the rest
          # cat( "In ",test.spec[["Name"]],"By from:",test.by,"to",test.by[!any.geo],"\n" )
          test.sum[["By"]] <- test.by[!any.geo]
        } else if ( all(any.geo) ) {
          # cat( "In ",test.spec[["Name"]],"Removing all from",test.by,"\n" )
          test.sum["By"] <- NULL # Single brackets - remove element entire
        } # else leave "By" untouched.
      }
      if ( "Units" %in% names(test.sum) ) {
        test.units <- test.sum[["Units"]]
        any.geo <- ( names(test.units) %in% ve.small.geo )
        if ( any ( ! any.geo ) ) {
          # remove geography
          test.sum[["Units"]] <- test.units[!any.geo]
        } else if ( all(any.geo) ) { # Only has the geo table in Units
          test.sum["Units"] <- NULL # Single brackets - remove element entire
        } # else leave "Units" untouched
      }
    } else { # Summarizing by geography ("Azone", "Bzone" or "Marea")
      # TODO: Geography "Value" should eventually be screened against model's 'defs/geo.csv'
      # TODO: If "Table" is the same as "By" and not GeographyType, skip that specification
      # with a message (or we could use Require). So any dip into the Marea table or the
      # Azone table only gets processed if we are running for that GeographyType.
      if ( ! Geography["Type"] %in% ve.small.geo ) {
        self$CheckMessages <- paste0("Invalid Geography Type for query specification: ",Geography["Type"])
      }
      geotest <- ( test.sum[["Table"]] %in% ve.small.geo ) # Which Table elements are the small geography
      # Write the following in case more than one Table element is a small geography
      # Mostly, that would probably be a logic error in the query specification
      if ( any( geotest) && any(test.sum[["Table"]][geotest] != Geography["Type"]) ) {
        writeLogMessage(
          paste0(
            "Skipping specification ",test.spec[["Name"]]," due to Table mismatch: ",
            Geography["Type"]," vs. Table ",paste(test.sum[["Table"]][geotest],collapse=", ")
          )
        )
      } else {
        # If Table is not a conflicting ve.small.geo,
        # swap small geography in spec with Geography["Type"]
        if ( Geography["Type"] == "Marea" ) {
          geo.from <- "Azone"
          geo.to <- "Marea"
        } else if ( Geography["Type"] == "Azone" ) {
          geo.from <- "Marea"
          geo.to <- "Azone"
        }
        # Check that "By" and "Units" include geo.from
        # If geo.from BUT NOT geo.to in "By" and "Units", change geo.from to geo.to
        #   if we have both in the spec, don't touch geo.from or geo.to
        # If geo.to not in "By" and "Units", add geo.to to By and geo.to = '' to "Units"
        test.sum.by <- test.sum[["By"]]
        azb <- ( test.sum.by %in% geo.from )
        if ( ! ( geo.to %in% test.sum.by ) ) {
          if ( any(azb) ) {
            test.sum.by[azb] <- geo.to
          } else {
            test.sum.by <- c(test.sum.by,geo.to)
          }
          test.sum[["By"]] <- test.sum.by
        }
        test.sum.units <- test.sum[["Units"]]
        # cat("Spec name:",test.spec[["Name"]],"\n")
        # cat("Units before:",paste(names(test.sum.units),collapse=","),"\n")
        azb <- test.sum.units %in% geo.from
        if ( ! (geo.to %in% names(test.sum.units)) ) {
          if ( any(azb) ) {
            names(test.sum.units)[azb] <- geo.to
          } else {
            test.sum.units[geo.to] <- ""
          }
          test.sum[["Units"]] <- test.sum.units
        }
        # cat("Units after:",paste(names(test.sum.units),collapse=","),"\n")
      }
      test.spec$QuerySpec[["Summarize"]] <- test.sum
    }
  }
  test.spec$valid()
  return(test.spec)
}

ve.spec.outputconfig <- function() {
  # Generate components for the visualizer (using the Export spec)
  Export <- self$QuerySpec$Export
  if ( !is.null(Export) ) {
    list(
      DISPLAYNAME  = Export$DisplayName,
      LABEL        = Export$Label,
      DESCRIPTION  = Export$Description,
      INSTRUCTIONS = Export$Instructions,
      METRIC       = Export$Metric,
      UNIT         = Export$Unit,
      NAME         = self$Name,
      XTICKS       = Export$XTicks
    )
  } else NULL
}

# S3 helper - turn the R6 object into a standard list
as.list.VEQuerySpec <- function(spec) return(spec$QuerySpec)

#' @export
VEQuerySpec <- R6::R6Class(
  "VEQuerySpec",
  public = list(
    # Methods
    initialize = ve.spec.init,      # Create a VEQuerySpec from a list or parmaeters
    print = ve.spec.print,          # Display contents of this spec
    check = ve.spec.check,          # Validate the individual query
    setgeo = ve.spec.setgeo,        # Filter query to indicated geography
    update = ve.spec.update,        # Alter elements of the query spec (from a list or parameters)
    valid = ve.spec.valid,          # See if the spec is valid (sets self$CheckResults)
    type = ve.spec.type,            # return "Summarize" or "Function" or "Invalid"
    copy = ve.spec.copy,            # clone this VEQuerySpec
    outputConfig = ve.spec.outputconfig, # format the specification for use in the visualizer

    # Data elements
    CheckMessages = "Empty",        # message explaining why VEQuerySpec$valid() returned FALSE, or "" if OK
    QuerySpec = list(),             # The actual specification
    Name = "Unnamed",               # Name of the spec, from its QuerySpec
    CompiledSpec = NULL,            # The compiled specification (checked and with derived elements added) see ve.spec.check
    Export = NULL                   # Export specifications for visualizer
  )
)

###########################################################################
# FUNCTION DEFINITIONS - Low-Level helpers
###########################################################################

# TODO: two types of helpers: for running a query; for exporting query results
# through data.frames and other structures.

###########################################################################
# FUNCTION: makeMeasure
#
# Process a measureSpec for a Year
# Return the measure for inclusion in the results
# currentMeasures creates environment in which to evaluate Function specs
makeMeasure <- function(measureSpec,thisYear,QPrep_ls,measureEnv) {

  measureName <- measureSpec$Name
  measureSpec <- measureSpec$QuerySpec

  # Skip or include measures based on presence of required Dataset
  if ( "Require" %in% names(measureSpec) ) {
    if ( ! visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      writeLog(paste(measureName,"(SKIPPED due to Require:)"),Level="warn")
      return( list() )
    }
  } else
  if ( "RequireNot" %in% names(measureSpec) ) {
    if ( visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      writeLog(paste(measureName,"(SKIPPED due to RequireNot:)"),Level="warn")
      return( list() )
    }
  }

  # Compute the measure based on the measureSpec
  if ( "Function" %in% names(measureSpec) ) {
    # Elevate those to individual objects
    measure <- try( eval(parse(text=measureSpec$Function), envir=measureEnv) )
    if ( ! is.numeric(measure) ) {
      writeLog(paste(measureName,"Function measure failed to compute."),Level="error")
      writeLog(as.character(measure),Level="error")
      measure <- as.numeric(NA) # Fall through with measure being scalar NA
    }
    # TODO: Figure out GeoType and GeoValues from Function components
    #   A complex calculation on the parsed function - examining the values used
    measure <- structure(
      measure,
      Units=measureSpec$Units,
      Description=measureSpec$Description
    ) # used during export to filter on Geography
  } else if ( "Summarize" %in% names(measureSpec) ) {
    sumSpec <- measureSpec$Summarize;
    # TODO: pre-process usingBreaks/usingKey once for each measureSpec before we
    # get this deep. BreakNames should already have been prepared.
    usingBreaks <- "Breaks" %in% names(sumSpec) && ! is.null(sumSpec$Breaks)
    byRegion <- ! "By" %in% names(sumSpec) || ( usingBreaks && length(sumSpec$By) == 1 )
    usingKey <- "Key" %in% names(sumSpec) && ! is.null(sumSpec$Key)
    measure <- visioneval::summarizeDatasets(
        Expr = sumSpec$Expr,
        Units = sumSpec$Units,
        By = if ( ! byRegion || usingBreaks ) sumSpec$By else NULL,
        Breaks_ls = if ( usingBreaks) sumSpec$Breaks else NULL,
        Table = sumSpec$Table,
        Key = if ( usingKey ) sumSpec$Key else NULL,
        Group = thisYear,
        QueryPrep_ls = QPrep_ls
      )
    # Create attribute for geographies present in this measure
    if ( length(measure) == 1 || ( usingBreaks && ! is.array(measure) ) ) {
      GeoValues <- "Region"
      GeoType <- "Region"
    } else {
      # length(measure)>1 && ( ! usingBreaks || is.array(measure) )
      GeoType <- sumSpec$By[1] # First By dimension is the GeographyType
      if ( is.array(measure) ) { # Get names from dimnames
        GeoValues <- dimnames(measure)[1] # yields a character vector of all the Geography names
      } else { # Get names from names
        GeoValues <- names(measure)
      }
    }

    # Compute some metric attributes needed by visualizer

    # Add GeoValues as an attribute to the measure
    # GeoType is found in the Spec

    measure <- structure(
      measure,
      Units=measureSpec$Units,
      Description=measureSpec$Description,
      GeoType=GeoType,
      GeoValues=GeoValues,
      Export=measureSpec$Export # visualizer elements...
    ) # used during export to filter on Geography
  } else {
    writeLog(paste(measureName,"Invalid Measure Specification (must be 'Summarize' or 'Function')"),Level="error")
    measure <- as.numeric(NA)
  }

  # Turn the measure into a named list of one, for inclusion in the
  # measureEnv$Values list
  assign(measureName,measure,envir=measureEnv)
  return(measureEnv)
}

#   # Linearize measure elements and names when doing export
#   # TODO: all the names and vector conversion go in the export function
#   if ( ! byRegion && ! usingBreaks ) {
#     # Vector with measure values named for each value of GeoType
#     GeoNames <- names(measure)
#   } else {
#     # We're either doing the region or using breaks or both
#     if ( usingBreaks ) {
#       # TODO: create BreakNames once for the specification at the top
#       # Set up break name vector ("MeasureName.BreakName")
#       # Don't bother with the MeasureName. part - we're just going to save measure
#       # and metadata about how to unpack it.
#       # TODO: all this unpacking (array->vector->series of scalars with names)
#       # should only happen when we export. BreakNames need to be built into the Specification
# 
#       # Apply breakNames to requested results
#       if ( ! byRegion ) {
#         # using breaks and small geography (2-dimensional result array)
#         if ( is.null(dim(measure)) || ! dim(measure)==2 ) {
#           stop(
#             writeLog(
#               Level="error",
#               paste(
#                 "Unsupported: Measure does not have 2 dimensions (Breaks, Geography):",
#                 ifelse(is.null(dim(measure)),"1-D Vector",paste("dim",dim(measure)))
#               )
#             )
#           )
#         }
#         GeoNames <- dimnames(measure)[[2]]
#         measure <- as.vector(measure) # Need to drop to vector if length(GeoValues)>1
#         # Measure is then a set of Break values repeated length(GeoValues) times
#         buildNames <- character(0)
#         for ( nm in GeoNames ) {
#           buildNames <- c(buildNames,paste(breakNames,nm,sep="_"))
#         }
#         if ( length(buildNames) != length(measure) ) {
#           stop(
#             writeLog(
#               Level="error",
#               paste(
#                 "2-Dimensional names not same length as measure:",
#                 length(buildNames),"(expecting",length(measure),")"
#               )
#             )
#           )
#         }
#         names(measure) <- buildNames
#       } else {
#         # Using breaks on Region measure - measure should be a vector of break values
#         if ( length(measure) != length(breakNames) ) {
#           stop(
#             writeLog(
#               Level="error",
#               paste(
#                 "Break names have incorrect length:",
#                 length(breakNames),"(expecting",length(measure),")"
#               )
#             )
#           )
#         }
#         names(measure) <- breakNames
#       }
#     } else {
#       # Region measure, not using breaks (single value)
#       if ( length(measure) != 1 ) {
#         writeLogMessage("Processing measure for Region: ",measureName)
#         stop(writeLogMessage("Program error: expected scalar measure, got vector:",measure))
#       }
#       names(measure) <- measureName
#     }

# Code for writing a data.frame to exported output
#     # Save measure results into a file
#     writeLog(paste("Saving measures to",OutputFileToWrite,"..."),Level="warn")
#     utils::write.csv(Measures_df, row.names = FALSE, file = OutputFileToWrite)

###########################################################################
# FUNCTION: makeMeasureDataframe
#
# Extract the measures made by makeMeasure from measureEnv and put them in a data.frame suitable for
# writing to the output file. This function is an export helper and should draw from the
# result.env$Values list created for each scenario/ModelStage to build the resulting data.frame.
#
# TODO: this is part of the ve.query.extract function, and it works on a series
# of values generated by doQuery
# VEModel ResultsDir if querying a model), and then create one measure .Rdata for
# each VEResults object (fill the environment, then save to .Rdata in the results path)
#
makeMeasureDataframe <- function(Values,Year,GeoValues,wantData=TRUE,wantMetadata=TRUE) {
  # Values is a named list of measures for a single scenario year (scenarios may have more than one year)
  # Specifications provides metadata for each measure
  # If GeoValues provided, only expand Geography measures that are present in GeoValues
  # if data, include actual measure values, otherwise just do Metadata for
  #  each of the seek Measures, based on what is attached to the first value
  #  results.

  outputNames    <- character(0)
  outputMeasures <- numeric(0)
  outputUnits    <- character(0)
  outputDesc     <- character(0)

  wantData <- wantData || ! wantMetadata # return data unless we explicitly only want metaData

  # Values is a named numeric vector of measures
  for ( measureName in names(Values) ) {
    measure      <- Values[[measureName]]
    measureUnits <- attr(measure,"Units")
    measureDesc  <- attr(measure,"Description")
    GeoType      <- attr(measure,"GeoType")
    # measure is a scalar, vector or array of numeric measure values
    if ( ! is.null(GeoType) && GeoType != "Region" ) {
      # We have a geography type, and the names of the vector
      #  are the geography names. If it's an array, the geography
      #  names are in the first dimension
      if ( is.array(measure) ) {
        # it's an array of Geography, Breaks
        geoNames <- dimnames(measure)[[1]]
        geoNames <- geoNames[ which(geoNames) %in% GeoValues ]
        if ( length(geoNames)>0 ) {
          measure  <- measure[geoNames,]
          measureNames <- paste (
            sep=".",
            measureName,
            sapply(
              seq(ncol(measure)),
              function(y) {
                paste(
                  sep=".",
                  sapply(
                    seq(nrow(measure)),
                    function(x) {
                      dimnames(measure)[[1]][x]
                    }
                  ),
                  dimnames(measure)[[2]][y]
                )
              }
            )
          )
          measure <- as.vector(measure)
          if ( length(measureNames) != length(measure) ) {
            stop(
              writeLog(
                paste0(measureName,": Program error: vector=",length(measure)," names=",length(measureNames)),
                Level="error"
              )
            )
          }
        } else {
          measure <- as.numeric(NA)
          measureNames <- measureName
        }
      } else {
        # it's a vector of geographies
        geoNames <- names(measure)
        geoNames <- geoNames[ which(geoNames) %in% GeoValues ]
        if ( length(geoNames)>0 ) {
          measure  <- measure[geoNames]
          measureNames <- paste(sep=".",measureName,names(measure))
        } else {
          measure <- as.numeric(NA)
          measureNames <- measureName
        }
      }
    } else {
      # It's a scalar measure or a vector of break values for the region
      # TODO: this will fail for a Function measure that was based on other measures
      #   that have a non-Region GeoType - need to diagnose Function GeoType in makeMeasure
      if ( length(measure) > 1 ) {
        measureNames <- paste(sep=".",measureName,names(measure))
      } else {
        measureNames <- measureName
      }
    }

    # Assemble vectors to add to resulting data.frame
    outputNames    <- c( outputNames, measureNames )
    outputMeasures <- c( outputMeasures, measure )
    outputUnits    <- c( outputUnits, rep(measureUnits,length(measure)) )
    outputDesc     <- c( outputDesc, rep(measureDesc,length(measure)) )
  }

  # Add the year as a row
  if ( wantData ) {
    outputNames    <- c( "Year", outputNames )
    outputMeasures <- c( as.numeric(Year), outputMeasures )
    outputUnits    <- c( "YR", outputUnits )
    outputDesc     <- c( "Scenario Year", outputDesc )
  }

  # Format output as a data.frame
  if ( wantMetadata && wantData ) {
    Data_df       <- data.frame(
      Measure     = outputNames,
      Units       = outputUnits,
      Description = outputDesc,
      Value       = outputMeasures
    )
  } else if ( wantMetadata ) { # but not data
    Data_df       <- data.frame(
      Measure     = outputNames,
      Units       = outputUnits,
      Description = outputDesc,
    )
  } else if ( wantData ) { # but not metadata
    Data_df <- data.frame(
      Measure=outputNames,
      Value=outputMeasures
    )
  }

  rownames(Data_df) <- NULL
  return(Data_df)
}

############################################################
# PROCESS QUERY SPECIFICATIONS ON DATASTORE
#
###########################################################################
# Process the Specification list
###########################################################################

# doQuery processes a list of VEResults, and generates QueryFile in their Path
# Returns full path of file QueryFile created
doQuery <- function (
  Results,             # a list of VEResult object(s) corresponding to Reportable scenarios
  Specifications,      # validated query specification to process
  QueryFile,           # Name of query file in which to save results
  Timestamp=Sys.time() # Pass as parameter since model calling doQuery will need it too
)
{
  if ( missing(Results) || missing(Specifications) ) {
    writeLog("Program error: Invalid Setup for doQuery function",Level="error")
    return(character(0))
  }
    
  old.wd <- getwd()        # Framework plays fast and loose with working directory
  on.exit(setwd(old.wd))
 
  resultsGenerated <- character(0) # vector of path names for generated query results
  for ( results in Results ) {
    # Results is a list of VEResults objects (with ModelState plus Datastore)

    # Move to results directory
    browser(expr=(!is.environment(results) || !is.character(results$resultsPath)))
    setwd(results$resultsPath)

    # Scenario Name for reporting / OutputFile
    ScenarioName <- results$ModelState()$Scenario;
    writeLog(paste("Building measures for Scenario",ScenarioName),Level="warn")

    # Gather years from the results
    Years <- as.character(results$ModelState()$Years) # should already be character

    # Set up model state and datastore for framework query processing
    # TODO: Qprep_ls is overkill - should revise SummarizeDatasets to use
    #   just the ModelState (multiple Datastores are handled internally through
    #   the virtual Datastore path).
    QPrep_ls <- results$queryprep()

    # Set up result structure for this scenario
    queryEnv <- new.env()
    queryEnv$Specifications <- Specifications
    queryEnv$Timestamp <- Timestamp
    queryEnv$Values <- list()
    
    # Iterate across the Years in the scenario
    for ( thisYear in Years ) {

      # Process measures for thisYear in VEResults
      writeLog(paste("Working on Year",thisYear),Level="warn")
      if ( ! thisYear %in% names(queryEnv$Values) ) {
        queryEnv$Values[[thisYear]] <- list()
      }

      # Iterate over the measures, computing each one
      # Work in an environment so "Function" specs can easily access earlier measure results
      measureEnv <- new.env()
      for ( measureSpec in Specifications ) {
        writeLog(paste("Processing",measureSpec$Name,"..."),Level="info")
        makeMeasure(measureSpec,thisYear,QPrep_ls,measureEnv)
        # makeMeasure attaches GeoType and available GeoValues to each measure
      }
      queryEnv$Values[[thisYear]] <- as.list(measureEnv)
    }
    # Save results to the QueryFile in the VEResults path
    QueryResults <- file.path(results$resultsPath,QueryFile)
    save(list=ls(queryEnv),envir=queryEnv,file=QueryResults)
  }
  return(NULL)
}
