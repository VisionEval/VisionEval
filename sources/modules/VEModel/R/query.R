# Query.R
#' @include defaults.R
#' @include results.R
#' @import visioneval

self=private=NULL

# VEModel should, when opening query, provide QueryDir for the model
ve.query.init <- function(
  QueryName=NULL,
  FileName=visioneval::getRunParameter("QueryFileName"),
  QuerySpec=NULL,
  OutputDir=visioneval::getRunParameter("OutputDir"),
  QueryDir=visioneval::getRunParameter("QueryDir")
) {
  if ( is.list(QuerySpec) || "VEQuery" %in% class(QuerySpec) ) {
    # Possibly using default QueryDir/FileName
    self$load(FileName=FileName,QueryDir=QueryDir,QuerySpec=QuerySpec,QueryName=QueryName)
  } else {
    FileName <- paste0(QueryName,".VEqry")
    self$load(FileName,QueryDir=QueryDir,QueryName=QueryName)
  } # load sets QueryName   

  self$QueryDir <- QueryDir;
  self$OutputDir <- OutputDir;      # Where to write an output file

  if ( length(self$checkResults)>0 ) {
    visioneval::writeLogMessage("Query specification contains errors")
  } else {
    visioneval::writeLogMessage("Loaded Query")
  }
  self$check()
  invisible(self$valid())
}

# TODO: one day allow alternate storage formats
# However, because the existing query spec format uses named vectors,
#  it is difficult to recover full information from JSON or YAML
ve.query.save <- function(saveTo=TRUE) {
  if ( ! is.character(saveTo) ) {
    saveTo <- file.path(self$QueryDir,self$QueryFile)
  } else if ( nzchar(saveTo) ) {
    saveTo <- file.path(self$QueryDir,saveTo)
  } # else its an empty string, which says dump to console
  dump("QuerySpec",file=saveTo,envir=private) # dump the QuerySpec as R
}

ve.query.load <- function(FileName=NULL,QueryName=NULL,QueryDir=NULL,QuerySpec=NULL) {
  # Load from a list, if provided
  if ( ! is.null(QuerySpec) ) {
    private$QuerySpec <- QuerySpec
    return( self$check() )
  }

  # Otherwise, attempt to load from a file
  if ( is.null(FileName) ) {
    FileName <- visioneval::getRunParameter("QueryFileName")
  }
  if ( is.null(self$QueryFile) || FileName != self$QueryFile ) {
    self$QueryFile <- FileName
  }
  if ( ! is.null(QueryDir) ) self$QueryDir <- QueryDir
  if ( is.null(QueryName) ) {
    QueryName <- sub("\\.[^.]*$","",basename(self$QueryFile))
  }
  if ( is.null(self$QueryName) ) {
    self$QueryName <- QueryName;    # Do not overwrite if self$QueryName already assigned
  }

  sourceFile <- normalizePath(file.path(self$QueryDir,self$FileName),winslash="/",mustWork=FALSE)
  existingFile <- file.exists(sourceFile)
  if ( ! existingFile ) {
    sourceFile <- normalizePath(self$FileName,winslash="/",mustWork=FALSE)
  }
  if ( ! file.exists(sourceFile) ) {
    visioneval::writeLogMessage("New Query:",self$QueryName)
    return( self$check() )
  }

  # Load the query from sourceFile
  ve.model <- visioneval::modelEnvironment() # Don't need to clear ve.model
  sys.source(sourceFile,envir=ve.model)
  private$QuerySpec <- ve.model$QuerySpec
  return( self$check() )
}

ve.query.copy <- function(newName=NULL) {
  if ( is.null(newName) ) newName <- paste(self$QueryName,"(Copy)")
  new.query <- VEQuery$new(
    QuerySpec=self$QuerySpec,
    QueryName=paste(newName),
    FileName=paste0(newName,".VEqry"),
    OutputDir=self$OutputDir,
    QueryDir=self$QueryDir
  )
  return( new.query )
}

ve.query.check <- function(verbose=FALSE) {
  self$names(update=TRUE) # Make sure QuerySpec named list names are consistent
  self$checkResults <- character(0)
  for ( spec in self$QuerySpec ) {
    spec$check()
    self$checkResults <- c( self$checkResults, spec$checkResults )
  }
  # TODO: Can the Function specs be processed based on what was earlier computed
  # Interrogate $checkResults directly or via $valid()
  # Return the validated object (containing messages)
  # Force an update to the names
  return(self)
}

ve.query.valid <- function() {
  # summarize outcome of last check (as a logical)
  return( length(self$checkResults)==0 || ! all(!nzchar(self$checkResults)) )
}

ve.query.add <- function(obj,location=0,before=FALSE,after=TRUE) {
  # Really, add or update - if the name(s) of SpecListOrObject is/are already
  #   in QuerySpec, the existing value(s) will be over-written, regardless of location
  # Default is different from update, which forces all the leftovers to the end.

  # Start by get "obj" in order - make it into another VEQuery, using standard error checking
  qry <- asQueryList(obj)
  if ( ! qry$valid() ) {
    msg <- c("Cannot add to query:",qry$checkResults)
    visioneval::writeLogMessage( c(msg,deparse(obj)) )
    stop(msg)
  }

  # Now validate and interpret "location", "before" and "after"
  currentNames <- names(private$QuerySpec)
  nameLen <- length(currentNames)
  if ( is.character(location) ) {
    if ( ! location %in% currentNames ) {
      stop(visioneval::writeLogMessage("Location is not in current QuerySpec"))
    } else {
      location <- which(currentNames %in% location)[1] # first instance of name in specification
    }
  }
  if ( ! is.numeric(location) ) { # invalid or NULL location just appends at the end
    location <- length(private$QuerySpec)
    before <- ! ( after <- TRUE )
  }
  
  location <- if ( location < 1 ) 1 else if ( location > nameLen ) nameLen else location;

  # adjust list locations to respect before and after
  if ( before ) {
    location <- location - 1
  } else if ( after ) {
    location <- location + 1 # 
  }
  
  if ( location > nameLen ) {
    beforeList <- 1:nameLen
    afterList <- 0
  } else if ( location < 1 ) {
    beforeList <- 0
    afterList <- 1:nameLen
  } else {
    beforeList <- 1:location
    afterList <- (location+1):nameLen
  }

  # Slice the names rather than the lists themselves
  namesBefore <- currentNames [beforeList]
  namesAfter  <- currentNames [ afterList]
  spec <- qry$getlist() # Check and extract specifications
  specNames   <- names(spec)
  namesBefore <- namesBefore  [ - which(namesBefore %in% specNames) ]
  namesAfter  <- namesAfter   [ - which(namesAfter  %in% specNames) ]

  # Then use the names to index the lists and create a new list, replacing the existing
  private$QuerySpec <- c( private$QuerySpec[namesBefore], spec, private$QuerySpec[namesAfter] )

  specNames <- names(private$QuerySpec)
  self$check() # probably all we catch here are pre-existing errors and function order problems
  if ( length(self$checkResults)>0 ) {
    visioneval::writeLogMessage("QuerySpec contains errors")
    print(self$checkResults) # a named character string
  }
  return(self)
}

ve.query.update <- function(obj) {
  # If it's not already in the QuerySpec, ignore it with a warning
  qry <- asQueryList(obj)
  if ( ! qry$valid() ) {
    msg <- visioneval::writeLogMessage("Invalid VEQuerySpec:")
    visioneval::writeLogMessage(deparse(obj))
    visioneval::writeLogMessage(qry$checkResults)
    stop(msg)
  }
  q.names <- names(qry.getlist())
  s.names <- names(private$QuerySpec)
  extra.names <- ! q.names %in% s.names
  if ( any( extra.names ) ) {
    visioneval::writeLogMessage("Warning","Names not in qry (use '$add'):",paste(q.names[extra.names],collapse=", "))
  }
  private$QuerySpec[ q.names ] <- qry.getlist() # replace list items in current spec
  self$check()
  return(self)
}

asQueryList <- function(obj) {
  if ( ! "VEQuery" %in% class(obj) ) {
    # The next step could fail spectacularly, but we'll recover if possible
    qry.spec <- if ( ! "VEQuerySpec" %in% class(obj) ) VEQuerySpec$new(obj) else obj
    name <- qry.spec$QuerySpec$Name
    loc <- if ( is.null(name) ) 1 else name
    qry <- list()
    qry[loc] <- qry.spec
    qry <- VEQuery$new(QuerySpec=qry.spec) # Just default the "environment"
  } else { # it's another VEQuery (or slice thereof)
    qry <- obj
  }
  qry$check()
  return(qry)
}

ve.query.remove <- function(SpecToRemove) {
  # like subset, but remove the elements that match and return them
  extract <- private$QuerySpec[SpecToExtract]
  nm.ext <- names(extract)
  if ( any(is.na(nm.ext) | is.null(nm.ext) | !nzchar(names(extract))) ) {
    stop(visioneval::writeLogMessage("VEQuery specification list has unnamed elements."))
  }
  private$QuerySpec[names(extract)] <- NULL
  invisible(
    VEQuery$new(
      QuerySpec=extract,
      QueryDir=self$QueryDir,
      OutputDir=self$OutputDir
    )
  )
}

ve.query.assign <- function(obj) {
  # replace the query spec with the other object's query spec
  qry <- asQueryList(obj)
  if ( qry.valid() ) {
    private$QuerySpec <- qry.getlist() # just replace what's there
  } else {
    msg <- "Cannot assign invalid query")
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
      QuerySpec=subset,
      QueryDir=self$QueryDir,
      OutputDir=self$OutputDir
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

ve.query.print <- function() {
  # Update for output
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
  cat("Status:", self$status,"\n")
  self$status
}

ve.query.getlist <- function(Geography=NULL) {
  ################################
  # Low-level function to get list to run
  # Original List Sanity Checking (move some to $check)
  # ALSO DOES Geography Processing/Filtering (push down individual spec operations to VEQuerySpec
  # We'll use this to get the actual list used internally to perform $run
  # The internal VEQUery$QuerySpec is a list of VEQuerySpec objects
  # The framework query function wants a regular R list
  ################################
  
  self$check()
  if ( ! is.null(Geography) ) {
    specProcessed <- list()
    specResults <- character(0)
    for ( test.spec in private$QuerySpec ) {
      test.spec <- test.spec$setgeo(Geography)
      if ( ! test.spec$valid() ) {
        checkResults <- c(checkResults,
          paste0(test.spec$name(),": ",test.spec$CheckResults)
        )
      } else {
        specProcessed[[test.spec$name()]] <- test.spec$QuerySpec; # the raw list
      }
    }
    if ( length(specProcessed) == 0 ) {
      checkResults <- c(checkResults,"No valid measure specifications provided.")
    }
    self$checkResults <- checkResults
    return( specProcessed )
  } else {
    return( private$QuerySpec )
  }
}

ve.query.results <- function() {
  # Maybe cache the data.frames that were computed in the most recent query run?
  # Keep timestamp data, parameters used for the query?
  # Think a bit about the data management needs.
}

# @return A character vector with the names of the .csv files containing the computed measures.
ve.query.run <- function(
  Results, # May be a vector of locations, or a single location
  Geography="Region",
  GeoValue=NULL, # optional - if Geography is not Region, only compute for this list
  outputFile = visioneval::getRunParameter("QueryOutputTemplate"),
  saveTo = TRUE, # Send to file
  log = "error"
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

  # Now run the query
  outputFiles <- doQuery(
    Scenarios=Results,  # Adapt to work with VEResults
    Geography=Geography,
    Specifications=self$getlist(), # A list of VEQuerySpec
    outputFile=outputFile, # TODO: compute from VEQuery state
    saveTo=saveTo, #
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
    OutputDir = NULL,               # Where the results will go after run
    QueryResults = NULL,            # Data Frame holding results of doing the queries
    QueryFile = NULL,               # Name of file holding VEQuery dump (load/save)
    checkResults = "Empty",         # Named character vector of check errors from last self$check
    QueryName = NULL,               # Display name for Query (default basename(Query-File)

    # Methods
    initialize=ve.query.init,       # initialize a new VEQuery object
    save=ve.query.save,             # With optional file name prefix (this does an R 'dump' to source)
    load=ve.query.load,             # With a file selection dialog if no name available and interactive()
    copy=ve.query.copy,             # Duplicates the query (for further editing)
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
    results=ve.query.results        # report results of last run
    run=ve.query.run,               # Option to save; results are cached in private$queryResults
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
  if ( self$valid() && is.list( self$QuerySpec ) {
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
    # If building from another object (copy constructor), just
    if ( "VEQuerySpec" %in% class(other) ) {
      self$QuerySpec <- other$QuerySpec
      self$check()
    } else if ( is.list(other) ) {
      self$QuerySpec <- other
      self$check()
    } else {
      self$QuerySpec <- list()
      self$checkResults <- c("Unknown source:",deparse(other))
    }
  }
}

ve.spec.check <- function() {
  self$checkResults <- character(0)
  if ( length(self$QuerySpec)==0 ) {
    self$checkResults <- "Empty"
  }
  nm.test.spec <- names(self$QuerySpec) # may be NULL
  if ( is.null(nm.test.spec) ) {
    checkResults <- c(checkResults,"Specification list unnamed")
  } else {
    if ( "Name" %in% nm.test.spec ) {
      spec.name <- self$QuerySpec$Name
    } else {
      spec.name <- "Unnamed"
      checkResults <- c( checkResults, paste(spec.name,"Specification") )
    }
    have.names <- nm.test.spec %in% c("Name","Units","Description","Function","Summarize","Require","RequireNot")
    # have.names will be logical(0) if nm.test.spec is NULL
    spec.valid <- length(have.names)>0 && all(have.names)
    if ( ! spec.valid ) {
      if ( !all(have.names) ) {
        msg <- "Unknown specification elements: "
      } else {
        msg <- "Unrecognized elements: "
      }
      checkResults <- c( checkResults,
        paste(msg,paste(nm.test.spec[!have.names],collapse=","))
      )
    }
    # TODO: examine "Summarize" and "Function" sub-elements
  }
  return(self)
}

ve.spec.valid <- function() {
  return( length(self$checkResults)==0 || ! all(!nzchar(self$checkResults)) )
}

ve.spec.copy <- function() {
  return ( VEQuerySpec(self) )
}

# data helper
ve.small.geo <- c("Marea","Azone")

ve.spec.setgeo <- function(Geography=NULL) {
  # Return a new, geography adjusted VEQuerySpec

  test.spec <- self$copy()
  if ( ! is.null(Geography) ) && test.spec$type() == "Summarize" ) {
    test.sum <- test.spec$QuerySpec[["Summarize"]] # pull out the sub-list
    if ( Geography["Type"] == "Region" ) {
      # Region: remove Marea or Azone from "By" and "Units", if present
      if ( "By" %in% names(test.sum) ) {
        test.by <- test.sum[["By"]]
        any.geo <- ( test.by %in% small.geo )
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
        any.geo <- ( names(test.units) %in% small.geo )
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
      if ( ! Geography["Type"] %in% small.geo ) {
        self$checkResults <- "Invalid Geography Type for query specification: ",Geography["Type"])
      }
      geotest <- ( test.sum[["Table"]] %in% small.geo ) # Which Table elements are the small geography
      # Write the following in case more than one Table element is a small geography
      # Mostly, that would probably be a logic error in the query specification
      if ( any( geotest) && any(test.sum[["Table"]][geotest] != Geography["Type"]) ) {
        visioneval::writeLogMessage(
          "Skipping specification ",test.spec[["Name"]],
          " due to Table mismatch: ",
          Geography["Type"]," vs. Table ",paste(test.sum[["Table"]][geotest],collapse=", ")
        )
        next  # Skip thist test.spec
      }
      # If Table is not a conflicting small.geo,
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
    check = ve.spec.check,          # Validate the individual query
    setgeo = ve.spec.geo,           # Filter query to indicated geography
    update = ve.spec.update,        # Alter elements of the query spec (from a list or parameters)
    valid = ve.spec.valid,          # See if the spec is valid (sets self$CheckResults)
    type = ve.spec.type,            # return "Summarize" or "Function" or "Invalid"
    copy = ve.spec.copy             # clone this VEQuerySpec

    # Data elements
    CheckResults = "Empty",         # message explaining why VEQuerySpec$valid() returned FALSE, or "" if OK
    QuerySpec = list(),             # The actual specification
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
    sumSpec <- measureSpec$Summarize
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
        Units_ = sumSpec$Units,
        By_ = if ( ! byRegion || usingBreaks ) sumSpec$By else NULL,
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
      Units = measureSpec$Units,
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
  outputFile,
  saveTo,
  log=""
)
{
  if (
    missing(Results) ||
    missing(Geography) ||
    missing(Specifications) ||
    missing(outputFile)
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

  saving <- is.character(saveTo) && length(saveTo)==1 && nzchar(saveTo)[1] && dir.exists(saveTo)
  if ( ! saving ) {
    outputFiles <- list() # will return list of data.frames
  } else {
    outputFiles <- character(0) # will return vector of file names
  }
  # Put the Specifications where we can review them against the outputs
  attr(outputFiles,"Specifications") <- Specifications

  old.wd <- getwd()

  futile.logger::flog.threshold(log)
  tryCatchLog::tryCatchLog(
    {
      for ( scenario in Results ) {
        # scenario is a VEResults object (with ModelState, etc all available)

        # Move to scenario directory
        setwd(scenario$path)

        # Scenario Name for reporting / outputFile
        scenarioName <- basename(scenario)

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

        # Build the outputFile name using the just reported specifications
        outputFileToWrite <- stringr::str_replace(outputFile,"%scenario%",scenarioName)
        outputFileToWrite <- stringr::str_replace(outputFileToWrite,"%years%",catYears)
        outputFileToWrite <- stringr::str_replace(outputFileToWrite,"%geography%",stringr::str_remove_all(catGeography,"[ ']"))
        if ( saving ) {
          outputFileToWrite <- normalizePath(file.path(saveTo,outputFileToWrite),mustWork=FALSE)
        } else {
          outputFileToWrite <- sub("\\.[^.]+$","",outputFileToWrite) # use this as data.frame name (dropping any file extension)
        }

        # Prepare for datastore queries
        #------------------------------
        # TODO: Update for VEResults, which should have everything pre-constructed
        prepForQuery <- function() {
          visioneval::prepareForDatastoreQuery(
            DstoreLocs_ = c("Datastore"),
            DstoreType = DatastoreType
          )
        }
        QPrep_ls <- prepForQuery()

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
        if ( saving ) {
          cat("Saving measures in",basename(dirname(outputFileToWrite)),"as",basename(outputFileToWrite),"...")
          utils::write.csv(Measures_df, row.names = FALSE, file = outputFileToWrite)
          cat("Saved\n")
          outputFiles <- c(outputFiles,outputFileToWrite) # Saving: return list of file names
        } else {
          outputFiles[outputFileToWrite] <- (Measures_df) # Not Saving: return list of data.frames
        }
      }
    },
    error=function(e) {
      cat("Error:",geterrmessage(),"\n")
    },
    finally=setwd(old.wd)
  )
  return(outputFiles)
}
