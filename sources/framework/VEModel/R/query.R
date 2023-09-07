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
#' implementation)
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
  if ( ! is.null(QuerySpec) && ! is.list(QuerySpec) && ! "VEQuery" %in% class(QuerySpec) ) {
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
  # If FileName was provided outside the ModelPath, the resulting
  # destination will identify a copy of that source query.
  self$attach(OtherQuery=OtherQuery,ModelPath=ModelPath,QueryDir=QueryDir,QueryName=QueryName)

  # if "load==TRUE" and null QuerySpec/FileName, build the output name and
  #   see if it already exists
  if ( !is.null(QuerySpec) || !is.null(FileName) || load ) {
    writeLogMessage("Loading Query...",Level="info")
    self$load(FileName=FileName,QuerySpec=QuerySpec,ModelPath=ModelPath,QueryDir=QueryDir)
    # in self$load, if QuerySpec is not NULL, no actual load will be
    # attempted... Just sets up the filename and will create a
    # separate version of the query file (with a number suffix on its
    # name).
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
      msg <- writeLogMessage(
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
    if ( is.null(FileName) ) {
      if ( !is.null(self$QueryFile) ) {
        FileName <- self$QueryFile
      }
      else if ( !is.null(ModelPath) && !is.null(self$QueryName) ) {
        # build a FileName as ModelPath/QueryDir/QueryName.VEqry
        if ( is.null(QueryDir) ) QueryDir <- ""
        FileName <- self$QueryFile <- normalizePath(
          file.path(ModelPath,QueryDir,paste(self$QueryName,".VEqry")),
          winslash="/",mustWork=FALSE
        )
      }
    }
    if ( !is.null(FileName) ) {
      writeLogMessage(Level="warn",paste("Loading",FileName))
      if ( file.exists(FileName) ) {
        # Load the query from FileName, creating an environment to hold it
        load.env <- new.env()
        sys.source(FileName,envir=load.env)
        self$add(get(ls(load.env)[1],envir=load.env)) # roundabout load to allow any name for Query object
      } else {
        writeLogMessage(Level="warn",paste("File does not exist:",FileName))
      }
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
    if ( verbose ) msg <- paste("Checking Spec name:",spec$Name)
    spec$check( Names=query.names ) # names list for validating functions
    query.names <- c( query.names,spec$Name )
    if ( ! spec$valid() ) {
      if ( verbose ) msg <- paste(msg,"(INVALID)")
      self$CheckMessages <- c(
        self$CheckMessages,
        paste0("Error checking specification '",spec$Name,"'"),
        spec$CheckMessages )
    } else if ( verbose ) msg <- paste(msg,"(valid)")

  }
  return(self)
}

ve.query.valid <- function(log="info") {
  # summarize outcome of last check (as a logical)
  writeLog(paste("CheckMessages length == 0",length(self$CheckMessages)==0),Level=log)
  if ( length(self$CheckMessages) > 0 ) writeLog(self$CheckMessages,Level=log)
  writeLog(paste("CheckMessages all empty:",all(!nzchar(self$CheckMessages))),Level=log)

  return( length(self$CheckMessages)==0 || all(!nzchar(self$CheckMessages)) )
}

ve.query.add <- function(obj,location=0,before=FALSE,after=TRUE) {
  # Really, add or update - if the name(s) of SpecListOrObject is/are already
  #   in QuerySpec, the existing value(s) will be over-written, regardless of location
  # If you use "update", specs not already present will be ignored with a warning.

  # Start by getting the specification list to add
  if ( is.list(obj) ) {
    spec <- asSpecList(obj)
    # Don't try to recover if the spec has invalid elements
  } else if ( "VEQuery" %in% class(obj) ) {
    spec <- obj$getlist() # Clone the spec list from obj (which is another query)
  } else if ( "VEQuerySpec" %in% class(obj) ) {
    # Make a bare query spec into a one-element list
    spec <- list(obj)
    names(spec) <- obj$Name
  } else {
    message("Cannot interpret object as query or specification:\n",deparse(obj))
    return(self)
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
    writeLogMessage("QuerySpec contains errors",Level="error")
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
      # TODO: may have errors that need to be reported... (relying on $check below...)
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
    qry <- VEQuery$new(QueryName=QueryName,QuerySpec=qry.spec)
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
      if ( is.list(self$QueryResults) && length(self$QueryResults)>0 ) cat("Results are available:\n")
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

ve.query.getlist <- function() {
  ################################
  # Low-level function to get a copy of the specification list to run
  # We'll use this to get the actual list used internally to perform $run
  # The internal VEQUery$QuerySpec is a list of VEQuerySpec objects
  # The framework query function wants a regular R list
  ################################

  self$check()
  # Deep copy the current QuerySpec
  newSpec <- lapply(private$QuerySpec,function(s) VEQuerySpec$new(s))
  names(newSpec) <- sapply(newSpec,function(s) if ( is.null(name <- s$Name) ) "" else name)
  return( newSpec )
}

# List of standard Metadata. Can ask for any field from VEQuerySpec
# "Name" is always included to support cbind
defaultMetadata <- c("Units","Description")

# TODO: In "Wide" format, keep the "By" fields in their own columns: that will be a bit more work if
# different queries have different combinations of By fields, but the logic is already implemented
# for Long format, and we'll want it for any extraction that is intended to be analyzed in a
# database system.

# TODO: Perhaps implement the same "Extractor" logic for queries as for regular data:
# Generate metric vectors for each Scenario/Year and add those to a list of metric columns
# that are then later formatted by the Extractor into "Long" and "Wide". That would simplify
# injecting the By columms, because we could just review all the vectors at the end to figure
# out how to make them conform for purposes of being in a data.frame (or some other output).

# OutputReceiver:
#   Connect to "table location" (identify a directory, open a DBI connection)
#   Create table (given a set of data that will go into it)
#   Append rows to table - Create/Recreate could just be an option on a single "writeToTable"
#   function that the Extractor will process

# Ahead of that, we'll assemble a list of unreconciled vectors, accumulate them in a list Review all
# their column names and make sure all rows are present (with NAs if needed) then just "cbind" them
# together. That's a lot easier if we don't have to preserve the data.frame structure along the way
# (just make a list of conforming vectors, then magically wave the wand over it and *poof* it's a
# data.frame). For long format, we make each row into vectors (and then conform the names at the
# end), and then just "rbind" instead of "cbind".

# "long" and "wide" will build different vectors and assemble them differently.

# The logic will be to generate columns where each one is a set of metrics

# make a data.frame of all (and only) the valid query results
# the results is a single data.frame with attributes
ve.query.extract <- function(
  Results=NULL, Measures=NULL, Years=NULL,
  wantMetadata=TRUE, wantData=TRUE, longScenarios=FALSE, exportOnly=FALSE) {
  # "Results" is a list of VEResults (or a VEResultsList) from a VEModel
  # Visit each of the valid results in the Model (or Results list) and add its years as columns
  #  to the resulting data.frame, then return the accumulated result
  # if "wantMetadata" is TRUE, include default metadata in the output table
  # if "wantMetadata" is a character vector, include only the named metadata items (the "Measure" name is
  #   always included whether or not metadata is requested)
  # if "wantData" is TRUE, include data columns. If false, only generate metadata (used, e.g., by the
  #   visualizer JSON generation function)
  # if "longScenarios" is TRUE, generate one scenario-year-metric per line with requested metaData;
  #   if "longScenarios" is FALSE (default=old style) generate one column per scenario-year and
  #   one row per metric By combination; Merge By fields onto metric name into a single name column;
  #   keep the By columns (if any) as part of default metadata per metric;
  # exportOnly, if TRUE, only includes metrics with the Export attribute set (conventionally to
  #   TRUE, but we're considering only present/missing)

  Results <- self$results(Results) # generate list of valid VEQueryResults
  if ( length(Results)==0 ) {
    writeLogMessage("No query results available; run the query first",Level="error")
    return( data.frame() )
  }

  scenarioList <- lapply(Results,function(r) r$values() )
  scenarioList <- scenarioList[ ! sapply(scenarioList,is.null) ] # Should be no NULLs, but just in case...

  if ( is.character(wantMetadata) ) {
    metadata <- wantMetadata
    wantMetadata <- TRUE
  } else {
    wantMetadata <- wantMetadata || longScenarios
    metadata <- character(0) # use all available metadata
  }
  if ( ! wantData && ! wantMetadata ) {
    wantMetadata <- TRUE
    metadata <- character(0)
  }

  # Filter values by Years
  if ( ! is.null(Years) ) {
    Years <- as.character(Years)
    scenarioList <- lapply(scenarioList,
      function(v) {
        yrs <- which( names(v) %in% Years )
        if ( length(v[yrs])==0 ) {
          stop(
            writeLogMessage(paste("Years are not available in all query results:",Years,collapse=", "),Level="error")
          )
        }
        v[yrs]
      }
    )
  }

  # Filter list of measure names by Measures parameter (list of names)
  measureNames <- Results[[1]]$measures()
  # All results should have the same set of measures...
  if ( any ( sapply(Results,function(m) ! setequal(measureNames,m$measures()) ) ) ) {
    stop("Program error: different measures in the result sets. VEModel::query.r circa line 640")
  }
  if ( exportOnly ) {
    exportMeasures <- sapply(Results[[1]]$values(),function(r) isTRUE(attr(r,"Export")))
    exportNames <- measureNames[which(exportMeasures)]
    if ( length(exportNames) > 0 ) measureNames <- exportNames  # else use all measureNames if none are exported
  }
  seekMeasures <- if ( ! is.character(Measures) ) {
    measureNames
  } else {
    measureNames[ measureNames %in% Measures ]
  }
  if ( length(seekMeasures) == 0 ) {
    stop(
      writeLogMessage(paste("Measures Not Found in Query:",paste(seekMeasures,collapse=", ")),Level="error")
    )
  }

  # Keep only measures that are being sought
  # Filter the measures using for loops rather than lapply to ensure names stay up to date
  for ( scenario in seq(scenarioList) ) {
    for ( year in names(scenarioList[[scenario]]) ) {
      scenarioList[[scenario]][[year]] <- scenarioList[[scenario]][[year]][seekMeasures]
      if ( length(scenarioList[[scenario]][[year]]) < 1 ) {
        message("'Measures' filter eliminated all measures! Available measures:")
        print(measureNames)
        stop("No measures for query.")
      }
    }
  }

  # Build long or wide data.frame with requested measure results and metadata

  results.df <- NULL
  Scenarios <- character(0)
  ScenarioYears <- character(0)
  ScenarioElements <- list()
  longScenarios <- longScenarios && wantData # if only metadata, always do wide format

  for ( scenario in scenarioList ) { # single set of filtered results
    ScenarioName <- attr(scenario,"ScenarioName")
    Elements <- attr(scenario,"ScenarioElements")
    for ( year in names(scenario) ) {
      if ( ! longScenarios ) { # will also use wide format if we're only getting Metadata
        longScenarios <- FALSE # so we don't add extra geography at the end
        # NOTE: removed wantMetadata parameter from makeWideMeasureDataFrame - always need it
        theseResults <- makeWideMeasureDataframe(scenario[[year]],ScenarioName,year,metadata=metadata)
        if ( is.null(results.df) ) { # first set of measures
          if ( wantMetadata ) {
            results.df <- theseResults[,c("Measure",attr(theseResults,"Metadata"))]
          } else {
            # Just put out the measure names if not gathering metadata
            # Data will then be attached below
            results.df <- theseResults[,c("Measure"),drop=FALSE]
          }
          if ( any(row.names(results.df)!=results.df$Measure) ) {
            stop(
              writeLog("Program error: VEModel::query.R circa line 699: bad results row.names",Level="error")
            )
          }
        }
        if ( ! wantData ) break # Only gathering metadata

        # Now attach the data value (expects row.names to contain the expanded Measure name)
        # Need to use merge rather than cbind to ensure that missing values are handled (e.g. if we
        # have BZone breaks but no values reported in certain years)
        byFields <- "Measure"
        metadata <- attr(theseResults,"Metadata")
        if ( !is.null(metadata) && length(metadata)>0 ) byFields <- c(byFields,metadata)
        mergeResults <- theseResults[,byFields,drop=FALSE]
        mergeResults[paste(ScenarioName,year,sep=".")] <- theseResults$Value
        results.df <- merge(results.df,mergeResults,by=byFields,all=TRUE)
        rownames(results.df) <- results.df$Measure
      } else {
        # Long format always produces metadata plus data...
        theseResults <- makeLongMeasureDataframe(scenario[[year]],ScenarioName,year,metadata)
        results.df <- rbind(results.df,theseResults) # Columns should conform...
      }
      Scenarios <- c( Scenarios, ScenarioName ) # dupes if multiple years for scenarios
      ScenarioYears <- c( ScenarioYears, year )
      ScenarioElements[[length(ScenarioElements)+1]] <- Elements # Scenario metadata
    }
    if ( ! wantData ) break # don't loop if not generating data
  }

  # Clean up various failures
  if ( is.null(results.df) ) results.df <- data.frame()

  if ( ! longScenarios ) {
    if ( length(Scenarios)>0 ) {
      ScenarioColumns <- paste(Scenarios,ScenarioYears,sep=".")
      if ( length(Scenarios) != length(results.df[,ScenarioColumns,drop=FALSE]) || length(Scenarios) != length(ScenarioYears) ) {
        stop(
          writeLogMessage("Scenarios don't match up with number of Result columns (VEModel/query.R circa line 732)",Level="error")
        )
      }
    } else {
      ScenarioColumns <- character(0)
    }
    # TODO: (harmless not to do) remove any metadata column that only has NA values (not present in any specification)
  } else {
    ScenarioColumns <- "Scenario" # or whatever we used inside makeLongMeasureDataframe...

    # Add geography tags to query results, if not present.
    Geo_df <- Results[[1]]$Source$ModelState()$Geo_df

    if ( "Bzone" %in% names(results.df) ) {
      # Complete Bzone geography
      extraGeo <- which(! names(Geo_df) %in% c("Marea","Azone","Czone")) # Bzone + extras
      if ( length(extraGeo) > 0 ) {
        extraGeo_df <- Geo_df[,extraGeo,drop=FALSE]
        row.names(extraGeo_df) <- Geo_df$Bzone
      } else extraGeo_df <- data.frame()
      resultNames <- names(results.df)
      for ( extra in names(extraGeo_df) ) {
        if ( ! extra %in% resultNames ) {
          extraValues <- extraGeo_df[results.df$Bzone,extra,drop=FALSE] # will give NA if is.na(Bzone)
          results.df[[extra]] <- extraValues[[extra]]
        } else {
          needExtra <- which(!is.na(results.df$Bzone))
          results.df[needExtra,extra] <- extraGeo_df[results.df$Bzone[needExtra],extra] # will give NA if is.na(Bzone)
        }
      }
      # Add in any missing Azone values
      AzoneLookup <- Geo_df[,"Azone",drop=FALSE]
      row.names(AzoneLookup) <- Geo_df$Bzone
      if ( ! "Azone" %in% resultNames ) {
        results.df$Azone <- AzoneLookup[ results.df$Bzone,"Azone" ]
      } else {
        needAzone <- which( is.na(results.df$Azone) & ! is.na(results.df$Bzone) )
        if ( length(needAzone) > 0 ) {
          results.df$Azone[ needAzone ] <- AzoneLookup[ results.df$Bzone[ needAzone ], "Azone" ]
        }
      }
    }
    if ( "Azone" %in% (resultNames <- names(results.df)) ) {
      MareaLookup <- Geo_df[,c("Azone","Marea"),drop=FALSE]
      MareaLookup <- unique(MareaLookup)
      row.names(MareaLookup) <- MareaLookup$Azone
      MareaLookup <- MareaLookup[,"Marea",drop=FALSE]
      if ( ! "Marea" %in% resultNames ) {
        results.df$Marea <- MareaLookup[ results.df$Azone, "Marea" ]
      } else {
        needMarea <- which( is.na(results.df$Marea) & ! is.na(results.df$Azone) )
        if ( length(needMarea) > 0 ) {
          results.df$Marea[ needMarea ] <- MareaLookup[ results.df$Azone[ needMarea ], "Marea" ]
        }
      }
    }
    if ( ! "Region" %in% names(results.df) ) {
      results.df$Region <- "Region"
    } else {
      results.df$Region[ is.na(results.df$Region) | results.df$Region!="Region" ] <- "Region"
    }

    # Clean up geographies (create look-up data.frames, with row.names = smaller geography and
    # value the larger geography) - for Bzones, have multiple columns and iterate over those.
    # Check if Bzone and ! is.na(Bzone)
    #    Check if extra non-geography fields from Geo_df
    #    If any are present there and not in result, add the field and copy value for each Bzone
    #    If no Azone field, insert
    #    Bzone to Azone where !is.na(Bzone)
    # Check if Azone (including just added) and ! is.na(Azone)
    #    If no Marea field, insert Marea
    #    Azone to Marea where !is.na(Azone)
    # Check if Region and is.na(Region) - if so, replace NA with "Region"
  }

  ExtractName <- self$QueryName
  if ( wantMetadata && ! wantData ) ExtractName <- paste0(ExtractName,"-Metadata")
  return(
    structure(results.df,
      Format=if ( longScenarios ) "Long" else "Wide",
      ExtractName=ExtractName,
      Scenarios=Scenarios,
      ScenarioYears=ScenarioYears,
      ScenarioElements=ScenarioElements,
      ScenarioColumns=ScenarioColumns
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
      writeLogMessage("No query results available; run the query first",Level="error")
    )
  }

  # Set up structural parameters for query results
  measureNames <- QueryResults$Measure
  scenarioElements <- attr(QueryResults,"ScenarioElements") # list of scenario elments present in each set of query results

  if ( length(scenarioElements)==0 ) { # No scenario elements defined - can't run visualizer
    stop(
      writeLogMessage("Query results do not have ScenarioElements - cannot visualize",Level="error")
    )
  }

  scenarioNames <- attr(QueryResults,"ScenarioColumns")
  QueryResults <- QueryResults[,scenarioNames]
  scenarioNames <- as.list(scenarioNames) # to include in VEdata below

  VEdata <- list()
  for ( d in 1:length(QueryResults) ) { # index into results columns, plus ScenarioElement attributes
    chk <- VEdata[[length(VEdata)+1]] <- c(
      Scenario=scenarioNames[[d]],
      scenarioElements[[d]],
      as.list(structure(QueryResults[[d]],names=measureNames))
    )
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
      writeLogMessage(paste0("Creating Visualizer in ",gsub(ve.runtime,"",SavePath)),Level="info")
      if ( ! dir.exists(SavePath) ) dir.create(SavePath,recursive=TRUE) else {
        stop(writeLogMessage("Renaming existing visualizer failed",Level="error"))
      }
      if ( ! dir.exists(SavePath) ) {
        stopMsg <- writeLogMessage("Failed to create visualizer directory:",Level="error")
        writeLogMessage(SavePath,Level="error")
        stop(stopMsg)
      }
      for ( f in dir(htmlRoot,recursive=TRUE) ) {
        dest <- file.path(SavePath,f)
        showPath <- gsub(ve.runtime,"",dirname(dest))
        writeLogMessage(paste("Copying",f,"to",showPath),Level="info")
        if ( ! dir.exists(dirname(dest)) ) {
          writeLogMessage(paste("Creating directory:",showPath))
          dir.create(dirname(dest),recursive=TRUE)
        }
        from <- file.path(htmlRoot,f)
        writeLogMessage(paste("From:",from),Level="info")
        writeLogMessage(paste("  To:",gsub(ve.runtime,"",dest)),Level="info")
        file.copy( from, dest )
      }
      visualizer.js <- file.path(SavePath,"visualizer.js")
      cat(file=visualizer.js,"// Visualizer variables for Query",self$QueryName,"on Model",self$Model$modelName,"\n")
      for ( js in names(jsonvars) ) {
        cat( file=visualizer.js, paste(js,"=",jsonvars[[js]],";\n"), append=TRUE )
      }
      writeLogMessage("Saved visualizer and data to:",Level="warn")
      writeLogMessage(gsub(ve.runtime,"",SavePath),Level="warn")
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

# Put the results of the query into a data.frame or csv (or eventually sql or another tabular receptacle.)
# ... are parameters passed to ve.query.extract (Measures, Years, longScenarios, wantMetadata, wantData, exportOnly

# Any extraction parameters go into ... (e.g. longScenarios)
# If the extract has been pre-generated by an explicit call to extract, pass it in through 'extract='
ve.query.export <- function(
  exporter="csv",         # One of the defined exporter tags; see export.R for details
  connection=NULL,        # Used to instantiate exporter; see export.R and docs for specific exporter drivers
  partition=character(0), # Default partition puts table in connection "root" (only sensible to partition Long format)
  extract=NULL,           # pre-existing extract (called externally)
  ...                     # parameters for extract function, if called here
) {

  Results <- self$Model$results() # else Results <- NULL # implied
  if ( is.null(Results) || ! inherits(Results,"VEResultsList") ) {
    stop( writeLogMessage("No results to query",Level="error") )
  }

  # Set up the exporter (create it externally and pass as a parameter for non-standard locations
  if ( ! inherits(exporter,"VEExporter") ) {
    # User provides externally constructed exporter
    exporter <- self$Model$exporter(tag=exporter,connection=connection,partition=partition)
  }

  # Extract results into data.frame (pass control parameters to self$extract via ...)
  # Alternatively, can extract prior to calling export and just pass the resulting data.frame to export
  if ( missing(extract) || is.null(extract) || !is.data.frame(extract) ) {
    extract <- self$extract(Results=Results,...)
  }

  # Build name for query extract table (mark Long format; Metadata mark is done inside extract -
  # Metadata is always "Wide" so that won't get injected into the name, but "Long" format will)
  ExtractName <- attr(extract,"ExtractName")
  if ( is.null(ExtractName) ) {
    ExtractName <- self$QueryName
    writeLog("Extracted data is missing ExtractName attribute",Level="warn")
  }
  LongWide <- attr(extract,"Format")
  if ( isTRUE(LongWide=="Long") ) ExtractName <- paste0(ExtractName,"_Long")

  ExtractTable <- paste(self$Model$setting("QueryExtractTable"),ExtractName)

  # The partitioning default should get the table into the the "root" of the exporter.
  # For file-system-based exporters like CSV or SQLite, the root will be the model's
  #   "ResultsDir/OutputDir")
  exporter$write(extract,ExtractTable)

  return(invisible(exporter))
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
    stop( writeLogMessage("Cannot export: no model or results to locate output directory.",Level="error") )
  }
  return(exportDir)
}

# Find any existing VEQueryResults associated with (model) Results
# The Results argument here is a list of model results (or a VEResultsList)
# The "Results" element generated by doQuery includes:
#   "Timestamp"
#   "Values" (a data.frame of metrics)
#   "Specifications" (redundant list of specs used to create "Values")
# If results are not available for one of the VEResults, Results element is NULL
ve.query.results <- function(Results=NULL, Reload=FALSE) {
  # Figure out where to look for results
  if ( missing(Results) || is.null(Results) ) {
    if ( "VEModel" %in% class(self$Model) ) {
      Results <- self$Model$results()
      # Output file was set when Model was attached
    } else {
      writeLog("No model from which to retrieve results",Level="info")
      return( list() ) # empty list if query has not been attached to a model
    }
  } else {
    self$outputfile() # will use self$Model output file if model is available, or global
  }
  if ( length(self$getlist())==0 ) {
    writeLog("Query specification is empty: no query results available",Level="error")
    return( list() )
  }

  if ( "VEResultsList" %in% class(Results) ) {
    # downshift to list of VEResults
    Results <- Results$results()
  }
  if ( ! is.list(Results) && "VEResults" %in% class(Results) ) {
    # Handle pathological case of only one stage with Results
    Results <- list(Results)
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
  Model      = NULL,   # Attached model on whose results to run (or a VEResultsList)
  Force      = FALSE,  # If true, re-run the query for all results even if they are up to date
  SaveLog    = FALSE,  # If true, save the query run results in a model root logfile (use with log="info")
  log        = "warn"  # log level to display while running the query
)
{
  if ( ! self$valid() ) {
    msg <- writeLogMessage("Query specification is invalid",Level="error")
    self$valid(log="error")
    stop(msg)
  }
  if ( length(private$QuerySpec)==0 ) {
    writeLog(paste("No specifications defined for",self$QueryName),Level="error")
  }
  visioneval::initLog(Threshold=log,Save=SaveLog,envir=new.env()) # Log level for display in this function

  msg <- writeLogMessage(paste("Running query:",self$QueryName),Level=log)

  if ( missing(Model) || is.null(Model) ) {
    Model <- self$Model # Use attached model if available
    if ( is.null(Model) ) stop( writeLogMessage("No model results available to run query",Level="error") )
  }
  queryingModel <- FALSE # running with attached model
  if ( "VEModel" %in% class(Model) ) {
    queryingModel <- TRUE
    self$model(Model)
    Results <- Model$results() # Convert model Reportable stages into a VEResults or VEResultsList if more than one
    if ( "VEResultsList" %in% class(Results) ) { # downshift to a plain list
      Results <- Results$results()
    } else if ( "VEResults" %in% class(Results) ) { # upshift to a list
      Results <- list(Results)
    } else {
      stop(
        writeLogMessage(paste("Unknown type for Model$results():",class(Results),collapse=","),Level="error")
      )
    }
  } else if ( "VEResultsList" %in% class(Model) ) {
    Results <- Model$results() # Downshift to plain list of VEResults
    if ( ! ("list" %in% class(Results)) || ! ("VEResults" %in% class(Results[[1]])) ) {
      stop( writeLogMessage("Program error: VEResultsList won't convert to list of VEResults",Level="error") )
    }
  } else if ( "VEResults" %in% class(Model) ) {
    Results <- list(Model) # Upshift a single VEResults object to a list of one
    if ( ! ("list" %in% class(Results)) || ! ("VEResults" %in% class(Results[[1]])) ) {
      stop( writeLogMessage("Program error: VEResultsList won't convert to list of VEResults",Level="error") )
    }
  } else {
    print(class(Model))
    stop( writeLogMessage(paste0("No results in Model Parameter: ",paste(class(Model),collapse=",")),Level="error") )
  }
  if ( ! is.list(Results) ) {
    stop(
      writeLogMessage(
        paste0("Program Error: list of results is not a list: ",class(Results)),
        Level="error"
      )
    )
  }
  validResults <- sapply(Results,function(r) r$valid())
  if ( !all(validResults) ) {
    for ( result in Results[!validResults] ) {
      writeLogMessage(paste("Model Results",result$Name,"is Invalid; not running query."),Level=log)
    }
  }
  Results <- Results[validResults]
  if ( length(Results)==0 ) {
    stop( writeLogMessage( "No valid model results to query",Level="error" ) )
  }

  # Check and compile the specifications; abort if not valid
  self$check()

  if ( ! Force && length(Results)>0 && length(private$QuerySpec)>0 ) {
    # Reload cached results (updates self$QueryResults and check for validity)
    # private$reload returns all the Results, whether or not they
    # have query results
    writeLogMessage("Checking for cached query results",Level=log)
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
      writeLogMessage("Query results are all up to date.",Level="info")
    } else {
      writeLogMessage(paste("Query results for",length(ResultsToUpdate),"model results out of",length(Results),"will be updated."),Level="info")
    }
  } else if ( length(private$QuerySpec)==0 ) {
    writeLogMessage("Query contains no valid specifications.",Level="error")
    return(list())
  } else {
    # update everything
    ResultsToUpdate <- Results
    writeLogMessage("Query results will all be updated.",Level="info")
  }

  # Run the query on any out-of-date results
  # ResultsToUpdate is a list of VEResults
  if ( (numResults<-length(ResultsToUpdate)) > 0 ) {
    writeLogMessage(paste(paste("Updating",numResults,"Results:"),paste(sapply(ResultsToUpdate,function(x)x$Name),collapse="\n"),sep="\n"),Level=log)
    doQuery(
      Results=ResultsToUpdate,         # list of VEResults objects for which to generate results
      Specifications=self$getlist(),   # A list of VEQuerySpec
      QueryFile=self$outputfile(),     # File into which to save each query result (in Results$resultsPath)
      Timestamp=Sys.time()             # Compared to ModelState last update to see if Query results are up to date
    )
    # Results of doQuery are written to the QueryFile in Results$resultsPath
    # self$results will reload them
  } else {
    writeLogMessage("No results to update.",Level="error")
  }

  # Update self$QueryResults to the list of VEQueryResults that were processed in this run and
  # return those
  QueryResults <- if ( queryingModel ) self$results(Reload=Force) else self$results(Results)
  return( invisible(QueryResults) )
}

# Quick Specification Generator (sum, mean, length of field by geography)

# Helper functions for quick query
geoList <- c("Region","Marea","Azone","Bzone")
isGeography <- function(Geography) Geography %in% geoList
rectifyTableGeography <- function(Table,Geography) {
  if ( ! isGeography(Geography) ) stop(paste(Geography,"is not a Geography"))
  if ( ! isGeography(Table) ) stop(paste(Table,"is not a Geography"))
  tableIndex <- which(geoList==Table)
  geoIndex <- which(geoList==Geography)
  if ( tableIndex < geoIndex ) return(geoList[tableIndex]) else return(Geography)
}

# Create a quick summary specification and add it to the query
ve.query.quick <- function( Table, Field, SpecName=NULL, Geography=NULL, FUN="sum") {
  # Table is one of the Year Group Tables
  # Field is a field to summarize within that Table
  # Geography is one of c("Region","Marea","Azone","Bzone") and will be made consistent with Table
  # SpecName, if not provided, is constructed from Field and Geography
  # FUN must be one of c("sum","mean","length") - might add others later...

  if ( missing(Table) || ! is.character(Table) ) stop("Missing Table for quick query")
  if ( missing(Field) || ! is.character(Field) ) stop("Missing Field for quick query")
  if ( ! inherits(self$Model,"VEModel") ) {
    stop("Query must be attached to a model for quick query")
  } else {
    model <- self$Model
    fullSpec <- model$list(outputs=TRUE,details=FALSE)
    whichSpecs <- fullSpec$GROUP=="Year" & fullSpec$TABLE==Table & fullSpec$NAME==Field
    modelSpec <- unique(fullSpec[whichSpecs,c("TABLE","NAME","UNITS","DESCRIPTION")])
# Too many inconsistencies across modules where fields are rewritten...
#    if ( nrow(modelSpec) > 1 ) {
#      message("Model specifications are inconsistent across stages:")
#      print(unique(fullSpec[whichSpecs,]))
#      stop("Error locating field for quick query")
#    }
    FieldUnits <- modelSpec$UNITS[1]
    FieldDescription <- modelSpec$DESCRIPTION[1]
  }
  if ( missing(Geography) || is.null(Geography) ) {
    if ( isGeography(Table) ) Geography <- Table else Geography <- "Bzone"
  }
  if ( missing(SpecName) ) SpecName <- paste(Field,Geography,sep="_")
  backstop <- 0
  repeat {
    if ( ! SpecName %in% self$names() ) break
    SpecName <- paste0(SpecName,"x")
    if ( (backstop <- backstop + 1) > 8 ) {
      stop("Spec Name is repeated too many times in quickQuery")
    }
  }

  # Construct Table join and Geography
  Key <- NULL
  TableList <- Table
  if ( Geography != Table ) {
    if ( isGeography(Table) ) {
      # Geopgraphy can't be smaller than Table
      Geography <- rectifyTableGeography(Table,Geography)
    } else if ( Table != "Household" ) {
      # Ignore any geography field already in Vehicle/Worker (just to simplify)
      Key="HhId"
      TableList <- list()
      TableList[[Table]] <- Field
      TableList[["Household"]] <- Geography
    }
  }
  By <- if ( Geography == "Region" ) NULL else Geography
  Units <- character(0)
  Units[Field] <- FieldUnits
  Units[Geography] <- ""
    
  if ( missing(FUN) || ! FUN %in% c("sum","mean","length") ) FUN = "sum"

  # Build the full specification
  newSpec <- list(
    Name        = SpecName,
    Units       = FieldUnits,
    Description = paste(FUN,"over",Geography,"of",FieldDescription),
    Summarize   = list(
      Expr = paste0(FUN,"(",Field,")"),
      Units = Units,
      Table = TableList
    )
  )
  # Add By and Key if required
  if ( ! is.null(By) ) newSpec$Summarize$By <- By
  if ( ! is.null(Key) ) newSpec$Summarize$Key <- Key

  self$add(VEQuerySpec$new(newSpec),location=NULL)
  return(self)
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
    # Running the query will create a data file for each ModelStage/VEResults
    # So it really just works on a list of VEResults in all cases (we can get such a list from the VEModel)
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
    quickSpec=ve.query.quick,           # create a quick QuerySpec to summarize a field by geography
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
  # expect VEResults$resultsPath to be normalized path
  self$Source <- VEResults # "Source" is a VEResults object
  if ( is.null(Query) || is.null(self$Source) || is.null(self$Source$resultsPath) ) return()

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
  resultsValid <- ( resultsValid && all(c("Timestamp","Manifest", "Values","Specifications") %in% names(self$Results)) )
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

ve.queryresults.measures <- function() {
  if ( self$valid() ) self$Results$Manifest else NULL
}  

ve.queryresults.metadata <- function(metadata=specExportElements) {
  if ( ! self$valid() ) return( character(0) )
  metadata <- unique(unlist(sapply( self$Results$Specifications, names )))
  metadata <- metadata[ ! metadata %in% c("Name","Function","Summarize","Export") ]
  return(metadata) # Leave out Name, which we pick up anyway
}

ve.queryresults.print <- function() {
  if ( self$valid() ) {
    cat("Query Results for",self$Source$Name,"\n")
    cat("Generated:",format(self$Results$Timestamp,"%Y/%m/%d at %H:%M"),"\n")
    cat("Measures:",paste(self$measures(),collapse=","),"\n")
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
    metadata = ve.queryresults.metadata,
    measures = ve.queryresults.measures,
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

specMetadataElements <- defaultMetadata

specExportElements <- c( "Export" )

specRequiredElements <- c(
  # These elements are "required" (except that "Function" and "Summarize" are
  # treated specially - there must be exactly one of them in the QuerySpec)
  specMetadataElements, c("Name", "Function", "Summarize", "Require", "RequireNot")
)

specSummarizeElements <- c(
  "Expr", "Units", "By", "Breaks", "BreakNames", "Filter", "Table", "Key", "Group"
)

# Any additional fields in the specification are added to the MetadataNames list

ve.spec.print <- function() {
  # If function, print its expression
  # If summarize, print its elements (nicely)

  if ( ! self$valid() ) {
    cat("Specification",self$Name,"is not valid:\n")
    cat(paste(self$CheckMessages,collapse="\n"),"\n")
  } else {
    cat(self$Name,"\n")
    spec.print <- function(s,spec,suffix="") { cat("   ",s,suffix,"= "); cat(deepPrint(spec[[s]],suffix=suffix),"\n") }
    nm.req <- specRequiredElements[ specRequiredElements %in% names(self$QuerySpec) ]
    nm.metadata <- self$QuerySpec$MetadataNames
    nm.req <- nm.req[ ! nm.req %in% nm.metadata ]
    dummy <- lapply(nm.req,spec=self$QuerySpec,spec.print)
    nm.export <- specExportElements[specExportElements %in% names(self$QuerySpec)]
    if ( length(nm.export) > 0 ) {
      dummy <- lapply(nm.export,spec=self$QuerySpec,spec.print)
    }
    if ( length(nm.metadata) ){ 
      dummy <- lapply(nm.metadata,spec=self$QuerySpec,suffix="(Metadata)",spec.print)
    }
  }
}

ve.spec.check <- function(Names=character(0)) {
  # Check the query spec and return a corrected version, with errors in self$CheckMessages
  # if Names is a character string, check that a Function spec only refers to defined names.

  self$CheckMessages <- character(0)
  if ( length(self$QuerySpec)==0 ) {
    self$CheckMessages <- "Empty"
    return(self)
  }

  # Check that required names are all present
  nm.test.spec <- names(self$QuerySpec) # may be NULL
  if ( is.null(nm.test.spec) ) {
    self$CheckMessages <- c(self$CheckMessages,"Specification list elements are unnamed")
  } else {
    if ( "Name" %in% nm.test.spec ) {
      self$Name <- self$QuerySpec$Name
      if ( length(Names)>0 && self$Name %in% Names ) {
        self$CheckMessages <- c(self$CheckMessages,paste("Duplicated Specification Name:",self$Name))
      }
    } else {
      self$Name <- "Unnamed"
      self$CheckMessages <- c(self$CheckMessages,paste(self$Name,"Specification"))
    }

    # Gather extra metadata names and keep a list of them in the QuerySpec
    extra.names <- ! nm.test.spec %in% c(specRequiredElements,specExportElements,"MetadataNames")
    metadata <- specMetadataElements
    if ( any(extra.names) ) {
      metadata <- c(metadata, nm.test.spec[extra.names])
    }
    self$QuerySpec$MetadataNames <- metadata

    if ( "Summarize" %in% names(self$QuerySpec) ) {
      nm.test.summarize <- names(self$QuerySpec$Summarize)
      have.names <- nm.test.summarize %in% specSummarizeElements;
      spec.valid <- length(have.names)>0 && all(have.names)
      if ( ! spec.valid ) {
        self$CheckMessages <- c(
          self$CheckMessages,
          paste("Unrecognized Summarize Elements:",paste(nm.test.spec[!have.names],collapse=","))
        )
      }
      checkedSpec <- visioneval::checkQuerySpec(QuerySpec=self$QuerySpec$Summarize)
      if ( length(checkedSpec$Errors)>1 || any(nzchar(checkedSpec$Errors)) ) {
        self$CheckMessages <- c(
          self$CheckMessages,
          msg<-paste("Error(s) in Query Summarize Specification:",self$Name,"\n"),
          checkedSpec$Errors
        )
      }
      if ( "Filter" %in% names(self$QuerySpec$Summarize) ) {
        # Confirm that "Filter" names are in "By"
        # Can't easily process values before query runs...
        Filter <- self$QuerySpec$Summarize$Filter
        goodFilter <- names(Filter) %in% self$QuerySpec$Summarize$By
        if ( any( ! goodFilter ) ) {
          self$CheckMessages <- c(
            self$CheckMessages,
            paste("Fields in Filter not listed in By:",paste( names(Filter)[!goodFilter], collapse=", "),"\n")
          )
        }
      }
    } else if ( "Function" %in% names(self$QuerySpec) ) {
      checkSymbols <- checkFunctionSpec(self$Name, self$QuerySpec, Names)
      if ( ! is.character(checkSymbols) || length(checkSymbols)>0 ) {
        checkSymbols <- as.character(checkSymbols) # could be some other kind of error
        self$CheckMessages <- c(
          self$CheckMessages,
          paste("Function probably refers to undefined names:",paste(checkSymbols,collapse=", "))
        )
      }
    } else {
      self$CheckMessages <- c(
        self$CheckMessages,
        "Unknown Specification Type (Valid would be 'Summarize' or 'Function')"
      )
    }

    # process Export / visualizer elements (read-only)
    # make sure all elements are present with suitable defaults
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

# Update function is used to easily change a spec name, or to replace it
# with a new list.
ve.spec.update <- function(
  # Replaces this QuerySpec from another one
  # To edit this spec, use getlist() to get the internal QuerySpec,
  #   then send it back into this function.
  # See ve.spec.check: can pass a QuerySpec through this with no
  #   override parameters to get the names filtered to just those
  #   that are legal.
  QuerySpec=list(), # a list or another VEQuerySpec
  Name = NULL,
  ...               # dots must have names - they will replace same-named elements in self$QuerySpec
) {
  if ( ! is.null(Name) ) {
    self$QuerySpec[["Name"]] <- Name # replace query name if provided
    
  }
  if ( ! missing(QuerySpec) ) {
    if ( inherits(QuerySpec,"VEQuerySpec") ) {
      QuerySpec <- QuerySpec$getlist()
    }
    if ( is.list(QuerySpec) && length(QuerySpec)>0 ) {
      # Merge the resulting QuerySpec into self$QuerySpec
      self$QuerySpec <- QuerySpec
    } else {
      writeLogMessage("Attempted update with invalid QuerySpec:")
      print(QuerySpec)
    }
  }
  if ( ...length() > 0 ) {
    newElements <- list(...)
    newElements <- newElements[ !is.na(names(newElements)) ] # ignore unnamed elements in ...
    self$QuerySpec[ names(newElements) ] <- newElements # replace slice of self$QuerySpect
  }
  # Then return the checked version of self
  return( self$check() ) # does the check, then returns "self"
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
    initialize = ve.spec.init,      # Create a VEQuerySpec from a list or parameters
    print = ve.spec.print,          # Display contents of this spec
    check = ve.spec.check,          # Validate the individual query
    update = ve.spec.update,        # Alter elements of the query spec (from a list or parameters)
    valid = ve.spec.valid,          # See if the spec is valid (sets self$CheckResults)
    type = ve.spec.type,            # return "Summarize" or "Function" or "Invalid"
    copy = ve.spec.copy,            # clone this VEQuerySpec
    outputConfig = ve.spec.outputconfig, # format the specification for use in the visualizer

    # Data elements
    CheckMessages = "Empty",        # message explaining why VEQuerySpec$valid() returned FALSE, or "" if OK
    QuerySpec = list(),             # The actual specification
    Name = "Unnamed"
  )
)

###########################################################################
# FUNCTION DEFINITIONS - Low-Level helpers
###########################################################################

# validGeoTypes <- c("Bzone","Azone","Marea","Region")

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

# FUNCTION: checkFunctionSpec
#
# Check that a Function Spec can be evaluated based on metrics already computed
#
#   measureName is the name of the metric to be computed
#   measureSpec is the function specification
#   Names is a list of names already defined (and available as operands)
# Return the possibly empty vector of undefined names

checkFunctionSpec <- function(measureName, measureSpec, Names) {
  # start by parsing the Function (an expression as a text string)
  Expression <- parse(text=measureSpec$Function)
  # Pull out the Symbols in the expression and check that they are present in envir.
  # Don't diagnose (or process) a Function with no antecedent specs
  Symbols <- unique(getNames(str2lang(measureSpec$Function)))
  # looking at parent.frame will find "R language symbols" like operators,
  # base language functions like "sum", etc. We're interested in those that
  # are left over after ruling out Names and language Symbols
  foundSymbols <- Symbols %in% Names
  notFound <- Symbols [ ! foundSymbols ]
  notFound <- notFound [ ! sapply(notFound,exists,envir=parent.frame()) ]

  if ( length(notFound) > 0 ) {
    notFound <- as.character(notFound)
  }
  return (
    structure(
      notFound,
      Expression=Expression,
      Names=Symbols[ foundSymbols ]
    )
  )
}  

# FUNCTION: evaluateFunctionSpec
#
# Run a Function spec in an environment
#   measureName is the name of the metric to be computed
#   measureSpec is the function specification
#   measureEnv is the environment containing prior specification names
# measureEnv can be an actual environment (requesting evaluation) or a character vector
#   consisting of names defined in earlier query specifications. If envir is an
#   environment, attempt to evaluate the function; otherwise just check that the
#   names are defined and return measureName. Attach diagnostics as appropriate.
# Return the result of checking and evaluation
#' @importFrom stats aggregate
evaluateFunctionSpec <- function(measureName, measureSpec, measureEnv=NULL) {
  environmentError <- if ( ! is.environment(measureEnv) ) {
    writeLog("No measureEnv environment for evaluateFunctionSpec",Level="error")
    TRUE
  } else {
    checkSymbols <- checkFunctionSpec(measureName, measureSpec, names(measureEnv))
    if ( length(checkSymbols)>0 ) {
      writeLog(
        c(
          paste("Function",measureName,"uses undefined names when evaluating Function spec."),
          paste(checkSymbols,collapse=", ")
        ),Level="error"
      )
      TRUE
    } else FALSE
  }
  if ( environmentError ) {
    return(
      structure(
        NA,
        ByFields=NULL,
        measureValid=FALSE
      )
    )
  }

  Names <- attr(checkSymbols,"Names")
  Expression <- attr(checkSymbols,"Expression")

  # Create a data.frame that just has the common denominator By fields for eventual return
  nameFrames <- lapply(Names, function(n) measureEnv[[n,exact=TRUE]]) # don't allow partial matches
  names(nameFrames) <- Names
  byFields <- lapply(nameFrames, function(n) { meNames <- names(n); return(meNames[ meNames != "Measure" ]) })
  commonGeoFields <- c("Bzone","Azone","Marea","Region")
  for ( measureBy in byFields ) {
    smallest <- which( commonGeoFields %in% measureBy )[1] # smallest available geography
    if ( !is.na(smallest) ) {
      if ( smallest > 1 ) commonGeoFields <- commonGeoFields[smallest]
    } else {
      commonGeoFields <- "Region"
      if ( ! "Region" %in% byFields ) byFields <- c(byFields,"Region")
    }
  }
  if ( length(commonGeoFields) > 1 ) stop("Error in VEModel::query.R line 1815; invalid common GeoFields")
  byFields <- byFields[[1]]
  nameFrames <- lapply(Names,function(Name) {
    frame <- nameFrames[[Name]]
    # So veto above if byFields are not the same
    names(frame) <- sub("^Measure$",Name,names(frame))
    frame
  })
  names(nameFrames) <- Names # reinstall names
  measureSet <- nameFrames[[1]]
  if ( length(nameFrames) > 1 ) {
    if ( length(byFields)>1 ) {
      bad <- FALSE
      for ( nm in Names ) {
        missing <- byFields[ ! byFields %in% names(nameFrames[[nm]]) ]
        if ( length(missing) > 1 ) {
          message("Missing byField ",paste(missing,collapse=",")," in ",nm)
          print(names(nameFrames[[nm]]))
          bad <- TRUE
        }
      }
      if ( bad ) stop("Wrong fields")
    }
    for ( frame in nameFrames[2:length(nameFrames)] ) {
      measureSet <- merge(measureSet,frame,by=byFields) # Measure Values have been given names
    } # might have only one frame in measureSet if we're doing (e.g.) an average or sum for all Mareas
  }

  # Now we can evaluate the expression
  measure <- try(
    # "within" adds a column called "Measure" to measureSet
    # the field indexing trims off the fields that are the operands of the Expression
    within( measureSet, Measure <- eval(Expression) )[,c(unique(byFields),"Measure")]
  )

  # Check for error processing the Expression
  measureValid <- if ( is.data.frame(measure) ) TRUE else { # expect it's a try-error
    writeLogMessage(paste(measureName,"Function measure failed to compute. Missing value?"),Level="error")
    writeLogMessage(as.character(measure),Level="error")
    measure <- data.frame(measureSet[byFields],Measure=as.numeric(NA))
    FALSE
  }

  # return the finished measure
  return(
    structure(
      measure,
      Valid=measureValid,
      ByFields=byFields
    )
  )
}

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
      writeLogMessage(paste(measureName,"(SKIPPED due to Require:)"),Level="warn")
      return( character(0) )
    }
  } else
  if ( "RequireNot" %in% names(measureSpec) ) {
    if ( visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      writeLogMessage(paste(measureName,"(SKIPPED due to RequireNot:)"),Level="warn")
      return( character(0) )
    }
  }

  # Compute the measure based on the measureSpec
  if ( "Function" %in% names(measureSpec) ) {
    # Function will compute an expression using Metrics computed earlier
    measure <- evaluateFunctionSpec(measureName, measureSpec, measureEnv)
    measureBy <- attr(measure,"ByFields")
    measureValid <- isTRUE(attr(measure,"Valid")) # NULL works like FALSE
  } else if ( "Summarize" %in% names(measureSpec) ) {
    sumSpec <- measureSpec$Summarize;
    usingBreaks <- "Breaks" %in% names(sumSpec) && ! is.null(sumSpec$Breaks)
    byRegion <- ! "By" %in% names(sumSpec) || ( usingBreaks && length(sumSpec$By) == 1 )
    usingKey <- "Key" %in% names(sumSpec) && ! is.null(sumSpec$Key)
    measureBy <- if ( ! byRegion || usingBreaks ) sumSpec$By else NULL # need this later to pick out fields
    measure <- visioneval::summarizeDatasets(
      Expr = sumSpec$Expr,
      Units = sumSpec$Units,
      By = measureBy,
      Breaks_ls = if ( usingBreaks) sumSpec$Breaks else NULL,
      BreakNames = if ( usingBreaks) sumSpec$BreakNames else NULL,
      Table = sumSpec$Table,
      Key = if ( usingKey ) sumSpec$Key else NULL,
      Group = thisYear,
      QueryPrep_ls = QPrep_ls
    )
    if ( ! all( attr(measure,"ByFields") %in% measureBy ) ) { # Doesn't happen, but just in case
      stop("Program failure visioneval/R/query.R line 1801")
    }
    # measure is a data.frame with a column called "Measure" (the numeric result)
    # and additional columns named after the By Fields (with "Region" at a minimum
    # and a full set of nested geographies if query was run for a smaller geography).

    # Filter measure based on Filter list of values for selectable fields
    if ( "Filter" %in% names(sumSpec) ) {
      filterFields <- names(sumSpec$Filter) # Filter is a list of vectors of values to select for each field
      filter <- rep(TRUE,nrow(measure)) # Start by selecting all the fields
      for ( field in filterFields ) {
        filter <- filter & ( measure[[field]] %in% sumSpec$Filter[[field]] )
      }
      measure <- measure[ which(filter), ]
    }

    # Drop fields that are not part of the "By" (if we use a "Key", we'll get a bunch of
    # other fields potentially from the other table).
    if ( ! is.null(measureBy) ) {
      measure <- measure[,c(measureBy,"Measure")]
    }

    # Mark measure valid if we didn't filter it down to nothing
    measureValid <- is.data.frame(measure) && nrow(measure) > 0
  } else {
    writeLogMessage(paste(measureName,"Invalid Measure Specification (must be 'Summarize' or 'Function')"),Level="error")
    measure <- as.numeric(NA)
    measureValid <- FALSE
  }
  measure <- structure(
    measure,
    ByFields=measureBy,                     # Fields to use for merging scenarios in Wide format
    Valid=measureValid,                     # TRUE if measure is valid (measure values may still be NA), else FALSE
    Export=measureSpec$Export,              # visualizer elements...
    MetadataNames=measureSpec$MetadataNames # Need these for later, but also attach the values of each one below
  )

  # Add any extra metadata names defined in the measure specification
  for ( mname in measureSpec$MetadataNames ) { # a vector of names, including Units and Description at a minimum
    attr(measure,mname) <- measureSpec[[mname]]
  }

  # measure is a single named data.frame with attributes placed in the query result environment
  writeLogMessage(paste("Saving measure:",measureName,"with",nrow(measure),"elements"),Level="info")
  assign(measureName,measure,envir=measureEnv)
  return(measureName)
}

###########################################################################
# FUNCTION: makeWideMeasureDataframe
#
# Extract the measures made by makeMeasure from measureEnv and put them in a data.frame suitable for
# writing to the output file. This function is an export helper and should draw from the
# result.env$Values list created for each scenario/ModelStage to build the resulting data.frame.

makeWideMeasureDataframe <- function(Values,Scenario="",Year=NULL, wantMetadata=TRUE, metadata=character(0)) {
  # Values is a named list of measures for a single scenario year (scenarios may have more than one
  # year)
  # Provide Year to get measure rows for the scenario year
  # WantMetadata will generally be TRUE

  outputNames    <- character(0)
  outputMeasures <- numeric(0)
  outputMetadata <- list()
  outputLength   <- 0 # to facilitate adding new metadata fields from later measures

  filterMetadata <- length(metadata)>0 # if metadata names not provided, show all

  # Values is a named list of data.frames
  for ( measureName in names(Values) ) {

    writeLog(paste("Adding",measureName,"to measure data frame"),Level="info")
    measure      <- Values[[measureName]] # A data.frame with By columns plus Measure
    if ( wantMetadata ) {
      metadataNames <- attr(measure,"MetadataNames") # The ones actually in this Measure
      if ( filterMetadata ) {
        # leave out anything not explicitly requested, otherwise include all metadata
        metadataNames <- metadataNames[ metadataNames %in% metadata ]
      }
      measureMetadata <- lapply(
        metadataNames,
        function(mn) {
          mdata <- attr(measure,mn)
          if ( is.null(mdata) ) mdata <- as.character(NA)
          mdata
        }
      )
      names(measureMetadata) <- metadataNames
    } else {
      metadataNames <- character(0)
      measureMetadata <- list()
    }

    if ( nrow(measure) > 1 ) {
      # NOTE: in wide format, By fields are encoded into the measure name
      byFields <- attr(measure,"ByFields")
      measureNames <- as.character(
        apply(
          measure[,byFields,drop=FALSE],1,
          function(x) paste(c(measureName,x),collapse=".")
        )
      ) # Yields Measure.By1.By2.etc (summarize names)
    } else {
      measureNames <- measureName # single measure row
    }

    # Assemble vectors to add to resulting data.frame
    outputNames    <- c( outputNames, measureNames)
    outputMeasures <- c( outputMeasures, measure$Measure )
    if ( wantMetadata ) {
      outputMetadata <- sapply( simplify=FALSE, USE.NAMES=TRUE,
        metadataNames,
        function(mname) {
          if ( outputLength>0 && ! mname %in% names(outputMetadata) ) {
            outputMetadata[[mname]] <- rep(as.character(NA),outputLength) # WARNING: presuming all metadata is character type
          }
          c( outputMetadata[[mname]], rep(measureMetadata[[mname]],length(measure$Measure)) )
        }
      )
    }
    outputLength <- length(outputMeasures)
  }

  # Add the Year and Scenario as rows, if provided and we're doing the data
  if ( ! is.null(Year)) {
    outputNames    <- c( "Year", outputNames )
    outputMeasures <- c( as.integer(Year), outputMeasures )
    if ( wantMetadata ) {
      outputMetadata <- sapply( simplify=FALSE, USE.NAMES=TRUE,
        metadataNames,
        function(mname) {
          metavalue <- if ( mname=="Units" ) {
            "YR"
          } else if ( mname=="Description" ) {
            "Scenario Year"
          } else as.character(NA)
          c( metavalue, outputMetadata[[mname]] )
        }
      )
    }
  }
  if ( nzchar(Scenario[1]) ) {
    outputNames    <- c( "Scenario", outputNames )
    outputMeasures <- c( Scenario[1], outputMeasures )
    if ( wantMetadata ) {
      outputMetadata <- sapply( simplify=FALSE, USE.NAMES=TRUE,
        metadataNames,
        function(mname) {
          metavalue <- if ( mname=="Units" ) {
            "character"
          } else if ( mname=="Description" ) {
            "Scenario Name"
          } else as.character(NA)
          c( metavalue, outputMetadata[[mname]] )
        }
      )
    }
  }

  # Format output as a data.frame
  # DANGER / TODO: is it a problem that the Value/Measure column may have different types for each
  # row? Worst case we should return a named list whose elements are lists rather than vectors.
  # Then manage tabular output by forcing each inner list element to character

  Data_df <- data.frame(Measure = outputNames)
  if ( length(outputMetadata) > 0 ) Data_df <- cbind(Data_df, outputMetadata) # More than zero metadata fields requested
  Data_df <- cbind(Data_df,Value=outputMeasures)

  rownames(Data_df) <- outputNames # rows are named after measures...
  
  return(
    structure(
      Data_df,
      Metadata=names(outputMetadata)
    )
  )
}

###########################################################################
# FUNCTION: makeLongMeasureDataframe
#
# Extract the measures made by makeMeasure from measureEnv and put them in a long data.frame
# suitable for writing to the output file.
#
# DANGER/TODO: presuming each Value/Measure has the same (probably numeric) type. less of a
# danger in this format than in the Wide format.

makeLongMeasureDataframe <- function(Values,Scenario="",Year=NULL,Metadata=character(0)) {
  # Values is a named list of measure data.frames for a single scenario year (scenarios may have
  # more than one year)
  # Will inject any requested Metadata fields (or all of them if none provided)

  Data_df <- NULL
  measureNames <- names(Values)

  filterMetadata <- length(Metadata)>0
  metadataFields <- character(0)

  for ( measure in seq(Values) ) {
    measureName <- measureNames[[measure]]
    value <- Values[[measure]]

    metadataNames <- attr(value,"MetadataNames")
    if ( is.null(metadataNames) ) {
      metadataNames <- character(0)
      measureMetadata <- list()
    } else {
      measureMetadata <- lapply(
        metadataNames,
        function(mn) {
          mdata <- attr(value,mn)
          if ( is.null(mdata) ) mdata <- as.character(NA)
          mdata
        }
      )
      names(measureMetadata) <- metadataNames
      if ( filterMetadata ) measureMetadata <- measureMetadata[ Metadata ]
    }
    metadataFields <- unique(c(metadataFields,names(measureMetadata)))

    names(value) <- sub("^Measure$","Value",names(value)) # value is a data.frame
    if ( is.null(Data_df) ) {
      # First metric => create new Data_df
      byFieldsData <- names(value)[! names(value) %in% "Value"]
      Add_df <- if ( length(measureMetadata) > 0 ) {
        Meta_df <- data.frame(measureMetadata)
        byFieldsData <- c(byFieldsData,names(Meta_df))
        data.frame(Meta_df,value)
      } else value # Already a data.frame
      Add_df$Measure <- measureName
      Data_df <- Add_df
    } else {
      # Subsequent metrics => conform By columns and append to Data_df
      addNewFields <- names(value)[ ! names(value) %in% c(byFieldsData,"Value") ]
      if ( length(addNewFields) > 0 ) {
        byFieldsData <- c(byFieldsData,addNewFields)
      }

      # Also add any new metadata fields
      Add_df <- if ( length(measureMetadata) > 0 ) {
        Meta_df <- data.frame(measureMetadata)
        addMetaFields <- names(Meta_df)[ ! names(Meta_df) %in% byFieldsData ]
        if( length(addMetaFields) > 0 ) {
          byFieldsData <- c(byFieldsData,addMetaFields)
          addNewFields <- c(addNewFields,addMetaFields)
        }
        data.frame(Meta_df,value)
      } else value # already a data.frame
      for ( field in addNewFields ) Data_df[[field]] <- as.character(NA)

      # Add any metadata or other fields that were present in earlier measures
      # but that may not be in this one.
      addOldFields <- names(Data_df)[!names(Data_df) %in% names(Add_df)]
      for ( field in addOldFields ) Add_df[[field]] <- as.character(NA)

      Add_df$Measure <- measureName

      if (length(Data_df)!=length(Add_df)) {
        cat("Wrong number of columns for",measureName,"\n")
        cat("Data_df c(",paste(names(Data_df),collapse=" "),") =",length(Data_df),"\n")
        cat("Add_df  c(",paste(names(Add_df),collapse=" "),") =",length(Add_df),"\n")
        stop("Program error: Wrong number of columns")
      }
      Data_df <- rbind(Data_df,Add_df)
      # cat("Added",nrow(Add_df),"rows from",measureName,"\n")
    }
    # cat("Output table now has",nrow(Data_df),"rows.\n")
  }
  # cat("Total Rows generated for scenario",Scenario,":",nrow(Data_df),"\n")
  if ( ! is.null(Data_df) ) {
    if ( ! is.null(Year) ) Data_df <- data.frame(Year=Year,Data_df)
    if ( nzchar(Scenario[1]) ) Data_df <- data.frame(Scenario=Scenario[1],Data_df)
  }
  rownames(Data_df) <- NULL # No row.names
  geoFields <- c("Region","Marea","Azone","Bzone")
  columnOrder <- "Measure"
  columnOrder <- c(columnOrder,geoFields[which(geoFields %in% names(Data_df))])
  columnOrder <- if (length(metadataFields)>0) c(columnOrder,metadataFields)
  columnOrder <- c(columnOrder,names(Data_df)[which( ! names(Data_df) %in% columnOrder & names(Data_df) != "Value" )])
  columnOrder <- c(columnOrder,"Value")
  if ( length(columnOrder) != length(Data_df) ) {
    print("Haven't ordered all columns:\n")
    print(columnOrder)
    print(names(Data_df))
    stop("Program error in makeLongMeasureDataframe")
  }
  Data_df <- Data_df[,columnOrder]
  return(Data_df)
}

############################################################
# PROCESS QUERY SPECIFICATIONS ON DATASTORE
#
###########################################################################
# Process the Specification list
###########################################################################

# doQuery processes a list of VEResults, and generates QueryFile in their Path
doQuery <- function (
  # TODO: Results should be a VEResultsList
  # TODO: iterating over Results should use Results$results()
  Results,             # a list of VEResult object(s) corresponding to Reportable scenarios
  Specifications,      # validated query specification to process
  QueryFile,           # Name of query file in which to save results
  Timestamp=Sys.time() # Pass as parameter since model calling doQuery will need it too
)
{
  if ( missing(Results) || missing(Specifications) ) {
    writeLogMessage("Program error: Invalid Setup for doQuery function",Level="error")
    return(character(0))
  }

  old.wd <- getwd()        # Framework plays fast and loose with working directory
  on.exit(setwd(old.wd))

  resultsGenerated <- character(0) # vector of path names for generated query results
  for ( results in Results ) {
    # Results is a list of VEResults objects (with ModelState plus Datastore)

    # Move to results directory
    if ( !is.environment(results) || !is.character(results$resultsPath) ) {
      msg <- writeLog("Results are not in proper format, query.r#2167",Level="error")
      stop(msg)
    }
    setwd(results$resultsPath)

    # Scenario Name for reporting / OutputFile
    ScenarioName <- results$ModelState()$Scenario;
    writeLogMessage(paste("Building measures for Scenario",ScenarioName),Level="warn")

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

    makeManifest = TRUE # only do it for the first year of results; presume later years are the same

    # Iterate across the Years in the scenario
    for ( thisYear in Years ) {

      # Process measures for thisYear in VEResults
      writeLogMessage(paste("Working on Year",thisYear),Level="warn")
      if ( ! thisYear %in% names(queryEnv$Values) ) {
        queryEnv$Values[[thisYear]] <- list()
      }

      # Iterate over the measures, computing each one
      # Work in an environment so "Function" specs can easily access earlier measure results
      measureEnv <- new.env()
      for ( measureSpec in Specifications ) {
        writeLogMessage(paste("Processing",measureSpec$Name,"..."),Level="info")
        manifest <- makeMeasure(measureSpec,thisYear,QPrep_ls,measureEnv)
        # makeMeasure attaches GeoType and available GeoValues to each measure
        if ( makeManifest ) {
          queryEnv$Manifest <- if ( is.null(queryEnv$Manifest) ) {
            manifest
          } else {
            c(queryEnv$Manifest,manifest)
          }
        }
      }
      makeManifest = FALSE # Completed manifest from first year
      queryEnv$Values[[thisYear]] <- as.list(measureEnv)
    }
    # Save results to the QueryFile in the VEResults path
    QueryResults <- file.path(results$resultsPath,QueryFile)
    save(list=ls(queryEnv),envir=queryEnv,file=QueryResults)
  }
  return(NULL)
}
