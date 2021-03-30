# Query.R
#' @include environment.R
#' @include results.R
#' @import visioneval

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
#      object; if NULL, it is replaced by ve.runtime

# Default query directory will be relative to the working directory
ve.query.init <- function(
  QueryName=NULL,    # If a consistent set of parameters, carry on to attach
  ModelPath=NULL,    # Root for directories
  QueryDir=NULL,     # relative sub-directory for Queries (could be ".")
  FileName=NULL,     # relative to ModelPath/QueryDir or ve.runtime/QueryDir, or absolute
                     # Forces "load=TRUE"; also fills unattached QueryName; "FromFile"
  OtherQuery=NULL,   # provides fully-formed QueryDir for attach
  QuerySpec=NULL,    # A list of VEQuerySpec's (or another VEQuery) to initialize with
  # Param_ls=NULL,   # Don't bother - pre-construct in "ModelPath" if coming from a model, otherwise
                     # use runtime environment RunParams_ls
  load=FALSE         # If TRUE, build a FileName as ModelPath/QueryDir/QueryName.VEqry
                     # However, if FileName is provided, use that if absolute, othewise normalize
                     # with ModelPath/QueryDir, attaching ".VEqry" if need be. See ve.query.load
) {
  # Fill in useful filename default
  if ( !is.null(FileName) && is.null(QueryName) ) {
    QueryName <- sub("\\.[^.]*$","",basename(FileName))
  }
  if ( ! is.list(QuerySpec) && ! "VEQuery" %in% class(QuerySpec) ) {
    QuerySpec <- NULL
  }
  otherValid <- TRUE
  if ( ! is.null(OtherQuery) && class(OtherQuery)!="VEQuery" ) {
    if ( is.list(OtherQuery) && length(OtherQuery)>0 ) {
      if ( class(OtherQuery[[1]]) != "VEQuerySpec" ) {
        OtherQuery <- list(VEQuerySpec$new(OtherQuery))
      }
      if ( class(OtherQuery[[1]]) == "VEQuerySpec" ) {
        QuerySpec <- OtherQuery
        OtherQuery <- NULL
      } else {
        otherValid <- FALSE
      }
    } else {
      otherValid <- FALSE
    }
  }
  if ( ! otherValid ) {
    stop(visioneval::writeLogMessage(paste("Could not make a VEQuery from OtherQuery:",class(OtherQuery))))
  }

  # Add remaining defaults for saving the query out again
  self$attach(OtherQuery=OtherQuery,ModelPath=ModelPath,QueryDir=QueryDir,QueryName=QueryName)

  # if "load==TRUE" and null QuerySpec/FileName, build the output name and
  #   see if it already exists
  if ( !is.null(QuerySpec) || !is.null(FileName) || load ) {
    self$load(FileName=FileName,QuerySpec=QuerySpec,ModelPath=ModelPath,QueryDir=QueryDir)
  }

  # Evaluate what is present
  self$check()
  invisible(self$valid())
}

# Build self$QueryFile, which will be used for saving (and perhaps loading if not requested up front)
# Filename will NOT be disambiguated until we get to the save operation
# Just prepare a candidate absolute path to a valid directory
ve.query.attach <- function(ModelPath=NULL, QueryName=NULL, QueryDir=NULL, OtherQuery=NULL) {
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
    if ( ! dir.exists(self$QueryDir) ) {
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
  QueryFile <- normalizePath(file.path(QueryDir,QueryName),winslash="/",mustWork=FALSE)
  QueryFile <- paste0(QueryFile,".VEqry")

  self$QueryName <- QueryName
  self$QueryFile <- QueryFile

  return(self$QueryFile)
}

# TODO: one day allow alternate storage formats
# However, because the existing query spec format uses named vectors,
#  it is difficult to recover full information from JSON or YAML
# overwrite:
#  if TRUE, destroy any identically named file;
#  if FALSE, insert digit disambiguation in filename
ve.query.save <- function(saveTo=TRUE,overwrite=FALSE) {
  # TODO: Disambiguate self$QueryFile if overwrite==FALSE
  if ( is.logical(saveTo) ) { # save to stored name
    if ( saveTo ) {
      saveTo <- self$QueryFile; # TODO: check existence & disambiguate based on overwrite
    } else {
      saveTo <- "" # empty string dumps to console
    }
  } else if ( ! is.character(saveTo) || ! nzchar(saveTo) ) { # saveTo console if not valid filename
    saveTo = ""
  }
  print(self)
  QuerySpec <- lapply(private$QuerySpec,function(spec) spec$QuerySpec) # extract R list from VEQuerySpec object
  dump("QuerySpec",file=saveTo) # dump the QuerySpec list as R
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
      sys.source(self$QueryFile,envir=ve.model)
      self$add(ve.model$QuerySpec)
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
  self$checkResults <- character(0)
  query.names <- character(0)
  for ( spec in self$QuerySpec ) {
    spec$check( Names=query.names ) # names list for validating functions
    query.names <- c( query.names,spec$Name )
    if ( ! spec$valid() ) {
      self$checkResults <- c(
        self$checkResults,
        paste0("Error checking specification '",spec$Name,"'"),
        spec$checkResults )
    }
  }
  return(self)
}

ve.query.valid <- function() {
  # summarize outcome of last check (as a logical)
  return( length(self$checkResults)==0 || all(!nzchar(self$checkResults)) )
}

ve.query.add <- function(obj,location=0,before=FALSE,after=TRUE) {
  # Really, add or update - if the name(s) of SpecListOrObject is/are already
  #   in QuerySpec, the existing value(s) will be over-written, regardless of location
  # If you use "update", specs not already present will be ignored with a warning.

  # Start by getting "obj" in order - make it into a new VEQuery, using standard error checking
  qry <- asQuery(obj)
  if ( ! qry$valid() ) {
    msg <- c("Cannot add to query:",qry$checkResults)
    visioneval::writeLogMessage( c(msg,deparse(obj)) )
    stop(msg)
  }
  # Now validate and interpret "location", "before" and "after"
  currentSpec <- self$getlist() # clone existing spec list
  currentNames <- names(currentSpec)
  nameLen <- length(currentNames)

  spec <- qry$getlist() # get spec list by cloning it
  if ( nameLen == 0 ) { # Already have query specifications
    newSpec <- spec
  } else {
    if ( is.character(location) ) {
      if ( ! location %in% currentNames ) {
        stop(visioneval::writeLogMessage("Location is not in current QuerySpec"))
      } else {
        location <- which(currentNames %in% location)[1] # first instance of name in specification
      }
    }
    if ( ! is.numeric(location) ) { # invalid or NULL location just appends at the end
      location <- nameLen
      before <- ! ( after <- TRUE ) # before is FALSE, after is TRUE
    } else if ( before ) after <- FALSE # if before, then not after

    location <- if ( location < 1 ) {
      1
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
  if ( length(self$checkResults)>0 ) {
    visioneval::writeLogMessage("QuerySpec contains errors")
    print(self$checkResults) # a named character string
  }
  return(self)
}

ve.query.update <- function(obj) {
  # If it's not already in the QuerySpec, ignore it with a warning
  qry <- VEQuery$new(OtherQuery=obj,QueryName="Temp-Query") # obj is anything we can turn into a VEQuery
  if ( ! qry$valid() ) {
    msg <- visioneval::writeLogMessage("Invalid VEQuerySpec:")
    visioneval::writeLogMessage(deparse(obj))
    visioneval::writeLogMessage(qry$checkResults)
    stop(msg)
  }
  q.names <- names(qry$getlist())
  s.names <- names(private$QuerySpec)
  extra.names <- ! q.names %in% s.names
  if ( any( extra.names ) ) {
    visioneval::writeLogMessage("Warning","Names not in qry (use '$add'):",paste(q.names[extra.names],collapse=", "))
  }
  private$QuerySpec[ q.names ] <- qry$getlist() # replace list items in current spec
  self$check()
  return(self)
}

asQuery <- function(obj,QueryName="Temp-Query") {
  if ( ! "VEQuery" %in% class(obj) ) {
    # If it's not a query, assume it's a single object and clone it
    qry.spec <- VEQuerySpec$new(obj)
    name <- qry.spec$QuerySpec$Name
    loc <- if ( is.null(name) ) 1 else name
    qry <- list()
    qry[[loc]] <- qry.spec
    qry <- VEQuery$new(QueryName="Temp-Query",OtherQuery=self,QuerySpec=qry)
  } else {
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
    stop(visioneval::writeLogMessage("VEQuery specification list has unnamed elements."))
  }
  private$QuerySpec[nm.ext] <- NULL
  invisible(
    VEQuery$new(
      QueryName="Removed-Specs",
      OtherQuery=self,
      QuerySpec=private$QuerySpec
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
    visioneval::writeLogMessage( c(msg,qry$checkResults) )
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
      if ( ! is.null(self$QueryResults) ) cat("Results are available.\n")
      print(self$names())
      if ( details ) for ( spec in private$QuerySpec ) print(spec)
    }
  } else if ( length(self$checkResults)>0 ) {
    cat("Query specification has errors:\n")
    for ( err in self$checkResults ) {
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
  newSpec <- lapply(private$QuerySpec,function(s) VEQuerySpec$new(s))
  if ( ! is.null(Geography) ) {
    validity <- list()
    specResults <- character(0)
    for ( test.spec in newSpec ) {
      test.spec <- test.spec$setgeo(Geography)
      validity <- if ( ! test.spec$valid() ) {
        checkResults <- c(checkResults,
          paste0(test.spec$name(),": ",test.spec$CheckResults," (removed)")
        )
        append(validity,FALSE)
      } else append(validity,TRUE)
    }
    if ( length(checkResults)>0 ) {
      newSpec <- newSpec[validity] # Remove any invalid elements from newSpec
      cat("Specifications invalid for Geography",Geography,":\n")
      cat(checkResults,collapse="\n")
    }
  }
  return( newSpec )
}
  
ve.query.results <- function() {
  # Maybe cache the data.frames that were computed in the most recent query run?
  # Keep timestamp data, parameters used for the query?
  # Think a bit about the data management needs.
  # Otherwise, we need to relate this query to the output it
  # generated and return (at least) the file, if not the data.frames
}

# @return A character vector with the names of the .csv files containing the computed measures.
ve.query.run <- function(
  Results, # May be a vector of locations, or a single location
  Geography  ="Region",
  GeoValue   = NULL,   # optional - if Geography is not Region, only compute for this list
  OutputDir  = visioneval::getRunParameter("OutputDir"),
  OutputFile = visioneval::getRunParameter("QueryOutputTemplate"),
  save       = TRUE,  # Send to file
  log        = "error"
  )
{
  if ( missing(Results) ) {
    stop( visioneval::writeLog("No results provided for query",Level="error") )
  }
  if ( ! is.character(Geography) || ! Geography %in% c("Region", "Azone","Marea") )
  {
    visioneval::writeLog("Geography must be one of 'Region','Marea' or 'Azone'",Level="error")
    return(character(0))
  }
  if ( Geography %in% c("Azone","Marea") ) {
    if ( missing(GeoValue) || ! is.character(GeoValue) || length(GeoValue)>1 || ! nzchar(GeoValue) ) {
      visioneval::writeLog("Not supported: Breaking measures by ",Geography,"; including all values",Level="error")
      # TODO: need to assemble proper combinations of By/GeoValues when unpacking results from
      # summarizeDatasets in makeMeasure: we end up with a 2-D matrix, not a vector or scalar, and
      # we need to transform that to a long form with suitable names for each element
      # (Measure-GeoValue-ByLevel). Not hard, just book-keeping (remove matrix dim to get a vector,
      # but understand row/column order and build suitable names, checking length/order of names
      # against original dim)
      # NOTE: the resulting array/matrix has dimension names reflecting the "By" element values;
      # Use those by default (but we can override the break descriptions)
      return(character(0))
    } else {
      visioneval::writeLogMessage("Evaluating measures for this ",Geography,": ",GeoValue)
    }
  } else {
    GeoValue <- "" # Region has no GeoValue
    visioneval::writeLogMessage("Evaluating measures for region")
  }
  Geography <- c(Type=Geography,Value=GeoValue) # prepare to do the query

  # Check and compile the specifications; abort if not valid
  self$check()

  # Now run the query
  outputFiles <- doQuery(
    Results=Results,  # Adapt to work with VEResults
    Geography=Geography,
    Specifications=self$getlist(), # A list of VEQuerySpec
    OutputDir=OutputDir,
    OutputFile=OutputFile, # TODO: compute from VEQuery state
    save=save,
    log=log
  )

  invisible(outputFiles)
}

# Here is the emerging VEQuery R6 class
# One of these is constructed by VEResults$query
# Perhaps have some S3 generic functions defined...

VEQuery <- R6::R6Class(
  "VEQuery",
  public = list(
    # Data
    QueryDir = NULL,                # Default directory from which to load queries (see initialize and load)
    QueryResults = NULL,            # Data Frame holding results of doing the queries
    QueryFile = NULL,               # Name of file holding VEQuery dump (load/save)
    checkResults = "Empty",         # Named character vector of check errors from last self$check
    QueryName = NULL,               # Display name for Query (default basename(Query-File)

    # Methods
    initialize=ve.query.init,       # initialize a new VEQuery object
    save=ve.query.save,             # With optional file name prefix (this does an R 'dump' to source)
    attach=ve.query.attach,         # Install consistent QueryName, QueryDir from request
    clear=ve.query.clear,           # Throw away the query specifications
    load=ve.query.load,             # Using installed file parameters, see if there's a file and load it
    copy=ve.query.copy,             # Duplicates the query (for further editing) - new query is "unattached"
    check=ve.query.check,           # Make sure all the specs work (including Function order)
    valid=ve.query.valid,           # Just report validation results (run $check first)
    add=ve.query.add,               # Add a VEQuerySpec (or list of them) to the VEQuery
    update=ve.query.update,         # Update an existing VEQuerySpec in the VEQuery
    remove=ve.query.remove,         # Remove a VEQuerySpec from the VEQuery
    assign=ve.query.assign,         # Assign the QuerySpec from one VEQuery to another
    names=ve.query.names,           # List (or update) names on internal QuerySpec list
    subset=ve.query.subset,         # Return a new VEQuery with a subset (or reordered) list of specs
    `[`=ve.query.index,             # Alternate notation for subset
    spec=ve.query.spec,             # Return a single VEQuerySpec from the list
    print=ve.query.print,           # List names of Specs in order, or optionally with details
    getlist=ve.query.getlist,       # Extract he QuerySpec list (possibly filtering geography) for $run)
    results=ve.query.results,       # report results of last run
    run=ve.query.run                # Option to save; results are cached in self$QueryResults
  ),
  private = list(
    QuerySpec=list(),               # access via public functions - list of VEQuerySpec objects
    saved=TRUE,                     # Flag whether spec list has unsaved changes
    queryGeo=NULL,                  # Run Parameter from ve.query.run
    queryGeoValue=NULL,             # Run Parameter from ve.query.run
    queryPrep=NULL                  # structure for running with summarizeDatasets code
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
      self$QuerySpec <- other$QuerySpec; # it's just a standard R list
      self$check()
    } else if ( is.list(other) ) {
      # If it's a list, assume it's a list of specifications
      self$QuerySpec <- other
      self$check()
    } else {
      # Can't figure out what to do - an error
      self$QuerySpec <- list()
      self$checkResults <- c("Unknown source:",deparse(other))
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

specOverallElements <- c(
  "Name", "Description", "Units", "Function", "Summarize", "Require", "RequireNot"
)

specSummarizeElements <- c(
  "Expr", "Units", "By", "Breaks_ls", "Table", "Key", "Group"
)

ve.spec.print <- function() {
  # TODO, more class-specific result
  # If function, print its expression
  # If summarize, print its elements (nicely)
  if ( ! self$valid() ) {
    cat("Specification is not valid:\n")
    cat(self$checkResults,collapse="\n")
  } else {
    nm <- specOverallElements[ specOverallElements %in% names(self$QuerySpec) ]
    dummy <- lapply(nm,spec=self$QuerySpec,
      function(s,spec) { cat("   ",s,"= "); cat(deepPrint(spec[[s]]),"\n") }
    )
  }
}

# modeled after helpers in visioneval core package
# extract names from a call or expression
# return "unlisted", which amounts to a character vector
getNames <- function(AST) {
  if ( length(AST)==1 ) {
    if ( is.name(AST) ) deparse(AST) else NULL # return character rendition of R symbol
  } else {
    unlist( lapply(AST, function(x) Recall(x) ) )
  }
}

ve.spec.check <- function(Names=NULL, Clean=TRUE) {
  # Check the query spec and return a corrected version, with errors in self$checkResults
  # if Names is a character string, check that a Function spec only refers
  #   to defined names.
  # if Clean is FALSE, leave unknown names behind in the Specification
  #   Mostly for debugging
  self$checkResults <- character(0)
  if ( length(self$QuerySpec)==0 ) {
    self$checkResults <- "Empty"
    return(self)
  }
  nm.test.spec <- names(self$QuerySpec) # may be NULL
  if ( is.null(nm.test.spec) ) {
    self$checkResults <- c(self$checkResults,"Specification list elements are unnamed")
  } else {
    if ( "Name" %in% nm.test.spec ) {
      self$Name <- self$QuerySpec$Name
      if ( is.character(Names) && self$Name %in% Names ) {
        self$checkResults <- c(self$checkResults,paste("Duplicated Specification Name:",self$Name))
      }
    } else {
      self$Name <- "Unnamed"
      self$checkResults <- c(self$checkResults,paste(self$Name,"Specification"))
    }
    if ( Clean ) {
      self$QuerySpec[ ! nm.test.spec %in% specOverallElements] <- NULL
    } else {
      have.names <- nm.test.spec %in% specOverallElements
      spec.valid <- length(have.names)>0 && all(have.names)
      if ( ! spec.valid ) {
        self$checkResults <- c(
          self$checkResults,
          paste("Unrecognized Specification elements:",paste(nm.test.spec[!have.names],collapse=","))
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
          self$checkResults <- c(
            self$checkResults,
            paste("Unrecognized Specification Elements:",paste(nm.test.spec[!have.names],collapse=","))
          )
        }
      }
      checkedSpec <- visioneval::checkQuerySpec(QuerySpec=self$QuerySpec$Summarize)
      if ( length(checkedSpec$Errors)>1 || any(nzchar(checkedSpec$Errors)) ) {
        self$checkResults <- c(
          self$checkResults,
          msg<-paste("Error(s) in Query Specification:",self$Name,"\n"),
          checkedSpec$Errors
        )
      }
      self$CompiledSpec <- checkedSpec$CompiledSpec
    } else if ( "Function" %in% names(self$QuerySpec) ) {
      if ( is.character(Names) ) {
        # unparse the Function for all symbol types
        Symbols <- unique(getNames(str2lang(self$QuerySpec$Function)))
        Symbols <- Symbols [ ! Symbols %in% Names ]
        Symbols <- Symbols [ ! sapply(Symbols,exists,envir=parent.frame()) ]
        if ( length(Symbols)>0 ) {
          self$checkResults <- c(
            self$checkResults,
            paste("Function refers to undefined names:",Symbols,collapse=", ")
          )
        }
      }
    } else {
      self$checkResults <- c(
        self$checkResults,
        "Unknown Specification Type (Valid='Summarize' or 'Function')"
      )
    }
  }
  return(self)
}

ve.spec.valid <- function() {
  return ( length(self$checkResults)==0 || all(!nzchar(self$checkResults)) )
}

ve.spec.copy <- function() {
  return ( VEQuerySpec$new(self) )
}

cleanSpecNames <- function(self)
{
  self$QuerySpec[ ! names(self$QuerySpec) %in% specOverallElements ] <- NULL
  if ( "Summarize" %in% names(self$QuerySpec) ) {
    self$QuerySpec$Summarize[ ! names(self$QuerySpec$Summarize) %in% specSummarizeElements ] <- NULL
  }
  return(self)
}

# TODO: the following is still a mess.
# Need a function to build a "Summarize" or "Function" sub-spec
# Then go in and update specific elements
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
    QuerySpec[ names(override) ] <- override; # replace list elements with named arguments
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
      summarize_ls[ names(override) ] <- override; # replace list elements
      QuerySpec$Summarize <- summarize_ls
    }
  }
  # Merge the resulting QuerySpec into self$QuerySpec
  self$QuerySpec[ names(QuerySpec) ] <- QuerySpec;

  # Then return the checked version of self
  return( self$check() ) # does the check, then returns "self"
}
    
# data helper
ve.small.geo <- c("Marea","Azone")

ve.spec.setgeo <- function(Geography=NULL) {
  # Return a new, geography adjusted VEQuerySpec

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
    } else { # Summarizing by geography ("Azone" or "Marea")
      # TODO: Geography "Value" should eventually be screened against model's 'defs/geo.csv'
      # TODO: If "Table" is the same as "By" and not GeographyType, skip that specification
      # with a message (or we could use Require). So any dip into the Marea table or the
      # Azone table only gets processed if we are running for that GeographyType.
      if ( ! Geography["Type"] %in% ve.small.geo ) {
        self$checkResults <- paste0("Invalid Geography Type for query specification: ",Geography["Type"])
      }
      geotest <- ( test.sum[["Table"]] %in% ve.small.geo ) # Which Table elements are the small geography
      # Write the following in case more than one Table element is a small geography
      # Mostly, that would probably be a logic error in the query specification
      if ( any( geotest) && any(test.sum[["Table"]][geotest] != Geography["Type"]) ) {
        visioneval::writeLogMessage(
          "Skipping specification ",test.spec[["Name"]],
          " due to Table mismatch: ",
          Geography["Type"]," vs. Table ",paste(test.sum[["Table"]][geotest],collapse=", ")
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

# S3 helper - turn the R6 object into a standard list
as.list.VEQuerySpec <- function(spec) return(spec$QuerySpec)

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

    # Data elements
    checkResults = "Empty",         # message explaining why VEQuerySpec$valid() returned FALSE, or "" if OK
    QuerySpec = list(),             # The actual specification
    Name = "Unnamed",               # Name of the spec, from its QuerySpec
    CompiledSpec = NULL             # The compiled specification (checked and with derived elements added) see ve.spec.check
  )
)

###########################################################################
# FUNCTION DEFINITIONS - helpers
###########################################################################

###########################################################################
# FUNCTION: makeMeasure
#
# Process a measureSpec for a Year and Geography
# Return the measureName, with the side effect that the value(s) of the measure
# are placed into measureEnv (whence they will later be summarized)
#
makeMeasure <- function(measureSpec,thisYear,Geography,QPrep_ls,measureEnv) {
  if ( Geography["Type"] != "Region" ) {
    GeoValue <- Geography["Value"]
    byRegion <- FALSE
  } else {
    GeoValue <- ""
    byRegion <- TRUE # skip processing "By" specifications
    # Yields one less dimension on results from summarizeDatasets
  }
  measureName <- measureSpec$Name

  # Skip or include measures based on presence of required Dataset
  if ( "Require" %in% names(measureSpec) ) {
    if ( ! visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      return(paste(measureName,"(SKIPPED due to Require:)"))
    }
  } else
  if ( "RequireNot" %in% names(measureSpec) ) {
    if ( visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      return(paste(measureName,"(SKIPPED due to RequireNot:)"))
    }
  }

  # Compute the measure based on the measureSpec     
  if ( "Function" %in% names(measureSpec) ) {
    measure <- eval(parse(text=measureSpec$Function), envir=measureEnv)
    names(measure) <- measureName
  } else
  if ( "Summarize" %in% names(measureSpec) ) {
    sumSpec <- measureSpec$Summarize;
    ## TODO: Push the following checks up to evaluate the query spec earlier
    if ( ! byRegion ) {
      if ( ! "By" %in% names(sumSpec) ||
           ! Geography["Type"] %in% sumSpec$By ) {
        stop(
          visioneval::writeLogMessage(
            paste("Script wants Geography Type ",Geography["Type"]," in 'By' but got ",sumSpec$By,"",sep="'")
          )
        )
      }
    }
    usingBreaks <- "Breaks" %in% names(sumSpec) && ! is.null(sumSpec$Breaks)
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
    if ( ! byRegion && ! usingBreaks ) {
      # For now, GeoValue must be present and just a single value
      # TODO: if measure is a vector, its elements will have names of the
      # values that were applied (all value of Geography, but if By was breaks
      # at a regional level, it will be the default break names)
      measure <- measure[GeoValue]  # reduce to scalar value (one geographical unit)
      names(measure) <- measureName
    } else {
      # TODO: base the following on the dimensions of measure and whether or not
      # we have GeoValue. Rows are the first dimension, columns are the second dimension
      # Using as.vector() to flatten the matrix will do (though it removes names)
      # for each column; within column, for each row. Assign the names first,
      # then name the elements - names do not have to be unique, so we can build
      # one dimension at a time.
      if ( ! byRegion ) { # need to reduce to vector for GeoValue
        measure <- measure[,GeoValue]
      }
      if ( usingBreaks ) {
        if ( "BreakNames" %in% names(sumSpec) ) {
          breakNames <- sumSpec$BreakNames[[sumSpec$By[1]]]
        } else {
          breakNames <- as.character(sumSpec$Breaks[[sumSpec$By[1]]])
        }
        names(measure) <- paste(measureName,c("min",breakNames),sep=".")
      } else {
        if ( length(measure) != 1 ) {
          visioneval::writeLogMessage("Processing measure: ",measureName)
          stop(visioneval::writeLogMessage("Program error: expected scalar measure, got vector:",measure))
        }
        names(measure) <- measureName
      }
    }
  } else {
    stop(visioneval::writeLogMessage("Invalid Measure Specification"))
  }
  
  # Stash the measure results in measureEnv
  for ( nm in names(measure) ) {
    msr <- measure[nm]
    attributes(msr) <- list(
      Description = measureSpec$Description
    )
    assign(nm,msr,envir=measureEnv)
  }

  # Return the name(s), for output tracking
  return(names(measure))
}

###########################################################################
# FUNCTION: makeMeasureDataFrame
#
# Extract the measures made by makeMeasure from measureEnv and put them in a
# data.frame suitable for writing to the output file
#
makeMeasureDataFrame <- function(measureEnv) {
  Measures_     <- objects(measureEnv)
  Values_       <- sapply(Measures_, get, envir=measureEnv)
  Units_        <- unname(sapply(Measures_, function(x) attributes(get(x,envir=measureEnv))$Units))
  Description_  <- unname(sapply(Measures_, function(x) attributes(get(x,envir=measureEnv))$Description))
  Data_df       <- data.frame(
    Measure     = Measures_,
    thisYear    = Values_,
    Units       = Units_,
    Description = Description_
  )
  # The following addresses a unique naming standard in original script
  # TODO: make it obsolete
  Data_df$Measure <- gsub("_Ma", "_", Data_df$Measure)
  Data_df$Measure <- gsub("_$", "", Data_df$Measure)
  Data_df$Measure <- gsub("_\\.", ".", Data_df$Measure)
  rownames(Data_df) <- NULL
  return(Data_df)
}

############################################################
# PROCESS QUERY SPECIFICATIONS ON DATASTORE
#
###########################################################################
# Process the Specification list
###########################################################################

doQuery <- function (
  Results, # one (or a list of) VEResult object(s)
  Geography,
  Specifications,
  OutputDir,
  OutputFile,
  save,
  log=""
)
{
  if (
    missing(Results) ||
    missing(Geography) ||
    missing(Specifications) ||
    missing(OutputFile)
  ) {
    visioneval::writeLogMessage("Invalid Setup for doQuery function")
    return(character(0))
  }
    
  # TODO: Update to get the years from the list of VEResults we are processing
  # Years are those defined for the model, less any that are not selected via $groups
  # We will only query groups that are Years from the runParams in each set of results
  # BaseYear will be evaluated only once unless it is unique in another scenario
  # Other years will be selected based on the individual results
  Years <- self$runParams$Years
  groups <- self$groups
  Years <- Years[Years %in% groups$Group[groups$Selected=="Yes"]]
  if ( length(Years)==0 || any(is.na(Years)) ) {
    visioneval::writeLogMessage("Invalid Years specified")
    cat("No years appear to be selected (check VEmodel$groups)\n")
    cat("Model has these Years available:",self$runParams$Years,"\n")
    return(character(0))
  }

  if ( ! is.logical(save) ) save <- TRUE
  if ( ! save ) {
    OutputFiles <- list() # will return list of data.frames
  } else {
    OutputFiles <- character(0) # will return vector of file names
  }
  # Put the Specifications where we can review them against the outputs
  attr(OutputFiles,"Specifications") <- Specifications

  old.wd <- getwd()

  futile.logger::flog.threshold(log)
  tryCatchLog::tryCatchLog(
    {
      for ( scenario in Results ) {
        # scenario is a VEResults object (with ModelState, etc all available)

        # Move to scenario directory
        setwd(scenario$path)

        # Scenario Name for reporting / OutputFile
        scenarioName <- scenario$Name;

        # Confirm what we're working on
        catYears <- paste(Years,collapse=",")
        catGeography <- Geography["Type"]
        if ( Geography["Type"]!="Region" &&
          (
            "Value" %in% Geography &&
            ! any(is.null(Geography["Value"])) &&
            ! any(is.na(Geography["Value"]))
          )
        ) {
          catGeography <- paste(catGeography,"=",paste0("'",Geography["Value"],"'"))
        }
        cat(
          "Building measures for:\n",
          "Scenario:",scenarioName,"\n",
          "Years:",catYears,"\n",
          "Geography:",catGeography,"\n"
        )

        # Build the OutputFile name using the just reported specifications
        OutputFileToWrite <- stringr::str_replace(OutputFile,"%scenario%",scenarioName)
        OutputFileToWrite <- stringr::str_replace(OutputFileToWrite,"%years%",catYears)
        OutputFileToWrite <- stringr::str_replace(OutputFileToWrite,"%geography%",stringr::str_remove_all(catGeography,"[ ']"))
        if ( save ) {
          OutputFileToWrite <- normalizePath(file.path(OutputDir,OutputFileToWrite),mustWork=FALSE)
        } else {
          OutputFileToWrite <- sub("\\.[^.]+$","",OutputFileToWrite) # use this as data.frame name (dropping any file extension)
        }

        # Prepare for datastore queries
        #------------------------------
        QPrep_ls <- scenario$queryprep()

        # Create the name of the data.frame that will collect the results
        Measures_df <- NULL

        # Iterate across the Years in the scenario
        for ( thisYear in Years ) {

          cat("Working on Year",thisYear,"\n")
          results <- new.env()

          # Iterate over the measures
          for ( measureSpec in Specifications ) {
            cat("Processing ",measureSpec$Name,"...",sep="")
            measure <- makeMeasure(measureSpec,thisYear,Geography,QPrep_ls,results)
            if ( length(measure)>1 ) for ( m in measure ) cat( paste0(m,"||") )
            cat("..Processed\n")
          }

          # Add this Year's measures to the output data.frame
          temp <- makeMeasureDataFrame(results)
          if ( is.null(Measures_df) ) {
            Measures_df<-temp[,c("Measure","Units","Description","thisYear")]
          } else {
            Measures_df<-cbind(Measures_df,thisYear=temp$thisYear )
          }
          names(Measures_df)[names(Measures_df)=="thisYear"]<-thisYear
        }

        # Add the measures to the output list
        if ( save ) {
          cat("Saving measures in",basename(dirname(OutputFileToWrite)),"as",basename(OutputFileToWrite),"...")
          utils::write.csv(Measures_df, row.names = FALSE, file = OutputFileToWrite)
          cat("Saved\n")
          OutputFiles <- c(OutputFiles,OutputFileToWrite) # Saving: return list of file names
        } else {
          OutputFiles[OutputFileToWrite] <- (Measures_df) # Not Saving: return list of data.frames
        }
      }
    },
    error=function(e) {
      cat("Error:",geterrmessage(),"\n")
    },
    finally=setwd(old.wd)
  )
  return(OutputFiles)
}
