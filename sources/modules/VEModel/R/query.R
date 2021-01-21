# Query.R
#' @include results.R
self=private=NULL

ve.init.query <- function() { # parameters yet to come - hook to model
  NULL
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
        stop(paste("Script wants Geography Type ",Geography["Type"]," in 'By' but got ",sumSpec$By,"",sep="'"))
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
          message("Processing measure: ",measureName)
          stop("Program error: expected scalar measure, got vector:",measure)
        }
        names(measure) <- measureName
      }
    }
  } else {
    stop("Invalid Measure Specification")
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

###########################################################################
# FUNCTION: ve.output.query
#
# VEModel function to process a query specification against a Datastore
#

#PROCESS QUERY DEFINITIONS AGAINST A DATASTORE
#===========================================================================================

# Comment block is obsolete - Update to R6 documentation (but perhaps simplest eventually to
# construct a "vignette" that documents the fuctions and workflow.

# Process a set of query definitions against the model's final Datastore
#
# \code{ve.output.query} an R6 function for VEModel that processes query specifications
# using the model's Datastore
#
# This function will create a set of output files with summary metrics in it. It will
# compute the specs for all the years in the Datastore, but if one of those years is
# left out of the VEmodel$groups, it will not be processed.
#
# @param Geography a named character vector with elements "Type" (currently supports
# either "Region", "Azone" or "Marea") and "Value" (any valid value for the corresponding Type
# from the model's geo.csv file; for Region, an empty string).
# @param SpecFile is the file name of the file containing the query. Relative path
# interpreted relative to the model path (so you can put it next to run_model.R)
# @param Spec is a list of specifications (already loaded - for one-offs or testing)
# @param outputFile template for generating scenario output file
# @param saveTo sub-directory of model path into which to write output files (defaults to "output" like extract)
# @param log, one of c("WARN","ERROR") for level at which to generate trace details (default "ERROR")
# @return A character vector with the names of the .csv files containing the computed measures.
ve.query.run <- function(
  Geography, # required - although perhaps default to 'Region'?
  GeoValue, # optional - if Geography is not Region, only compute for this list
  Spec, # Can submit a list (like what is contained in Query-Spec.R)
  SpecFile = "Query-Spec.R",
  outputFile = "Measures_%scenario%_%years%_%geography%.csv",
  #   Default is a long-but-informative filename
  saveTo="output", # folder in which to put outputFile (if FALSE or empty string, return list of data.frames)
  log="ERROR"  # set to "WARN" if to get detailed information on warnings as they happen
  )
{
  if ( missing(Geography) ||
    ( ! is.character(Geography) ) ||
    ( ! Geography %in% c("Region", "Azone","Marea") ) )
  {
    message("Geography must be one of 'Region','Marea' or 'Azone'")
    return(character(0))
  }
  if ( Geography %in% c("Azone","Marea") ) {
    if ( missing(GeoValue) || ! is.character(GeoValue) || length(GeoValue)>1 || ! nzchar(GeoValue) ) {
      message("Not supported: Breaking measures by ",Geography,"; including all values")
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
      message("Evaluating measures for each ",Geography,": ",GeoValue)
    }
  } else {
    GeoValue <- "" # Region has no GeoValue
    message("Evaluating measures for region")
  }
  Geography <- c(Type=Geography,Value=GeoValue) # prepare to do the query

  # Get SecenarioRoot and Scenarios using modelPaths
  # TODO: if we processing actual scenarios, we'll set the root a little differently

  # We'll look for the SpecFile in path or dirname(path)
  dataPath <- self$modelPath[self$stageCount]
  ScenarioRoot <- dirname(dataPath) # For now, just the one final stage of the model

  Scenarios <- normalizePath(dataPath,mustWork=FALSE)
  sd <- dir(Scenarios)
  if (
    self$status != "Complete" ||
    length(grep("Datastore",sd))==0 ||
    length(grep("ModelState.Rda",sd))==0
  ) {
    # TODO: this may work with a vector of Scenarios; we can use the grep
    #   results to determine which of Scenarios has model outputs
    # TODO: eventually open the ModelState and determine the run status
    message("Model appears not to have been run yet: ",self$status)
    return(character(0))
  }

  # Compute the saveTo location (if requested) relative to dataPath
  if ( is.logical(saveTo) ) {
    if ( saveTo ) # If somebody says saveTo=TRUE, they mean saveTo="output" (default)
      saveTo = "output" # re-install the default
  } 
  if ( is.character(saveTo) ) { # could be a directory
    if ( ! nzchar(saveTo) ) saveTo <- "output" # empty string replaced by default
    if ( ! ( grepl("^[/\\]",saveTo) | grepl("^.:",saveTo) ) ) { # relative path
      # saveTo is a relative path, so create it relative to dataPath
      saveTo <- normalizePath(file.path(dataPath,saveTo),winslash="/",mustWork=FALSE)
    } # else use saveTo as it is
    if ( ! dir.exists(saveTo) ) dir.create(saveTo,recursive=TRUE,showWarnings=FALSE)
    if ( ! dir.exists(saveTo) ) { # which it won't if saveTo was absolute but made no sense, e.g. missing drive
      message("saveTo directory invalid: ",saveTo)
      saveTo <- FALSE
    } else {
      message("Writing query output into directory: ",saveTo)
    }
  } else {
    if ( ! is.logical(saveTo) ) message("saveTo parameter is invalid: ",saveTo)
    saveTo <- FALSE
  }

  # Years are those defined for the model, less any that are not selected via $groups
  # We will only query groups that are Years from the runParams
  Years <- self$runParams$Years
  groups <- self$groups
  Years <- Years[Years %in% groups$Group[groups$Selected=="Yes"]]
  if ( length(Years)==0 || any(is.na(Years)) ) {
    message("Invalid Years specified")
    cat("No years appear to be selected (check VEmodel$groups)\n")
    cat("Model has these Years available:",self$runParams$Years,"\n")
    return(character(0))
  }

  # Gather the specifications, if they're not supplied via "Spec" parameter
  if ( ! missing(Spec) && ! is.list(Spec) ) {
    PMSpecifications <- Spec
    message("Specifications from existing list")
  } else {
    Spec <- NULL
  }
  if ( is.null(Spec) ) {
    # No pre-manufactured Spec.
    # Read SpecFile if Spec not passed as a parameter
    if ( ! is.character(SpecFile) || ! nzchar(SpecFile[1]) ) {
      message("Invalid SpecFile")
      cat("Provide the name, with optional path, to the file with the Query Specifications.\n")
      cat("SpecFile Path is relative to the model directory.\n")
      return(character(0))
    }
    for ( specName in unique(c(SpecFile,"Query-Spec.R")) ) {
      if ( !file.exists( SpecFile ) ) {
        SpecFile <- file.path(dataPath,SpecFile)
        if ( ! file.exists( SpecFile ) ) {
          SpecFile <- file.path(ScenarioRoot,SpecFile)
        }
      }
      if ( file.exists(SpecFile) ) break
    }
    SpecFile = normalizePath(SpecFile,winslash="/",mustWork=FALSE)
    if ( ! file.exists(SpecFile) ) {
      message("Specification File ",SpecFile," does not exist")
      return(character(0))
    } else {
      specEnv <- new.env()
      sys.source(SpecFile,envir=specEnv)
      specs <- objects(specEnv)
      if ( length(specs) != 1 ) {
        print(specs)
        message("Must define a single specification list in ",SpecFile)
        cat(SpecFile,"contains: ",paste(specs,collapse=", "),"\n")
        return(character(0))
      }
      PMSpecifications <- get(specs,envir=specEnv)
      displaySpec <- if ( exists("ve.runtime") ) {
        sub(get("ve.runtime"),"",SpecFile)
      } else {
        SpecFile
      }
      message(paste("Specification File: ",displaySpec,"",sep="'"))
      rm(specEnv)
    }
  }

  # Superficial sanity check of PMSpecifications (deeper checks within visioneval::summarizeDatasets)
  # Also rewrite the geography (so we can mostly reuse spec files for "Region","Marea", or "Azone"
  # TODO: Probably need better error recovery
  have.names <- character(0)
  spec.valid <- is.list(PMSpecifications)
  specProcessed <- list()
  small.geo <- c("Marea","Azone")
  if ( spec.valid ) {
    for ( test.spec in PMSpecifications ) {
      nm.test.spec <- names(test.spec) # may be NULL
      if ( is.null(nm.test.spec) ) spec.valid <- FALSE
      if ( spec.valid ) {
        have.names <- nm.test.spec %in% c("Name","Units","Description","Function","Summarize","Require","RequireNot")
        # have.names will be logical(0) if nm.test.spec is NULL
        spec.valid <- all(have.names)
        if ( ! spec.valid ) {
          if ( !all(is.na(have.names)) ) {
            message("Unknown specification elements: ",
              paste(nm.test.spec[!have.names],collapse=", ")
            )
          } else {
            message("Unrecognized specification list element:")
            print(test.spec)
          }
          spec.valid <- FALSE
        } else if ( "Summarize" %in% nm.test.spec ) {
          test.sum <- test.spec[["Summarize"]]
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
              message("Invalid Geography Type for query specification: ",Geography["Type"])
              spec.valid <- FALSE
              next
            }
            geotest <- ( test.sum[["Table"]] %in% small.geo ) # Which Table elements are the small geography
            # Write the following in case more than one Table element is a small geography
            # Mostly, that would probably be a logic error in the query specification
            if ( any( geotest) && any(test.sum[["Table"]][geotest] != Geography["Type"]) ) {
              message(
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
#             cat("Spec name:",test.spec[["Name"]],"\n")
#             cat("Units before:",paste(names(test.sum.units),collapse=","),"\n")
            azb <- test.sum.units %in% geo.from
            if ( ! (geo.to %in% names(test.sum.units)) ) {
              if ( any(azb) ) {
                names(test.sum.units)[azb] <- geo.to
              } else {
                test.sum.units[geo.to] <- ""
              }
              test.sum[["Units"]] <- test.sum.units
            }
#             cat("Units after:",paste(names(test.sum.units),collapse=","),"\n")
          }
          test.spec[["Summarize"]] <- test.sum
        }
        if ( spec.valid ) {
          specProcessed[[length(specProcessed)+1]] <- test.spec
        }
      }
      if ( ! spec.valid ) break
    }
    if ( spec.valid) PMSpecifications <- specProcessed
  }
  if ( ! spec.valid ) {
    # Report failing spec name, if it has one...
    if ( !is.na(have.names) && have.names[1] ) cat("In Specification '",test.spec$Name,"\n")
    message("Invalid measure specification.")
    return(character(0))
  }
  if ( length(specProcessed) == 0 ) {
    message("No valid measure specifications provided.")
    return(character(0))
  }

  # Now run the query
  outputFiles <- doQuery(
    Scenarios=Scenarios,
    Years=Years,
    Geography=Geography,
    Specifications=PMSpecifications,
    outputFile=outputFile,
    saveTo=saveTo,
    DatastoreType=self$runParams$DatastoreType,
    log=log
  )

  invisible(outputFiles)
}

############################################################
# PROCESS QUERY SPECIFICATIONS ON DATASTORE
#
###########################################################################
# Process the Specification list
###########################################################################

doQuery <- function (
  Scenarios,
  Years,
  Geography,
  Specifications,
  outputFile,
  saveTo,
  DatastoreType,
  log=""
)
{
  if (
    missing(Scenarios) ||
    missing(Years) ||
    missing(Geography) ||
    missing(Specifications) ||
    missing(outputFile) ||
    missing(DatastoreType)
  ) {
    message("Invalid Setup for doQuery function")
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

  outputFiles <- character(0)
  futile.logger::flog.threshold(log)
  tryCatchLog::tryCatchLog(
    {
      for ( scenario in Scenarios ) {
        # scenario contains a path to a working directory with a Datastore in it

        # Move to scenario directory
        setwd(scenario)

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

ve.query.load <- function() {
  NULL
}

ve.query.save <- function() {
  NULL
}

ve.query.insert <- function() {
  NULL
}

ve.query.check <- function() {
  NULL
}

ve.query.geography <- function() {
  NULL
}

ve.query.geovalue <- function() {
  NULL
}

ve.query.specs <- function() {
  NULL
}

ve.query.search <- function() {
  NULL
}

ve.query.move <- function(from,to="top") {
  # from is a vector of indices into self$querySpec, either integers or names
  # to is an index (integer or name) of length 1, which must not exist in "from"
  #  (moving a slice inside itself is a noop).
  NULL
}

ve.query.copy <- function(newName=NULL,newPath=NULL) {
  NULL
}

# S3 classes and generic functions to support them (used by VEResults)
# These wrap a data.frame (subset from VEResults$modelIndex)
# VEfields
# VEgroups
# VEtables

# S3 class wrapping a single query spec
# Could construct an edit function for it...
# Helper function creates an individual spec from parameters
# VEQuerySpec

# Here is the emerging VEQuery R6 class
# One of these is constructed by VEResults$query
# Perhaps have some S3 generic functions defined...

VEQuery <- R6::R6Class(
  "VEQuery",
  public = list(
    initialize=ve.init.query,
    print=ve.query.print,           # With optional details
    run=ve.query.run,               # Option to save
    copy=ve.query.copy,             # Duplicates the query (for further editing)
    save=ve.query.save,             # With optional file name prefix (this does an R 'dump' to source)
    load=ve.query.load,             # With a file selection dialog if no name available and interactive()
    search=ve.query.search,         # Search for a spec by some feature (returns a list)
    insert=ve.query.insert,         # Add a pre-constructed spec (list element class VEQuerySpec) to querySpec list (before param)
    move=ve.query.move,             # Reorder querySpec using from/to specification
    check=ve.query.check            # Ensure specs are all valid (can use a subset from search)
  ),
  active = list(
    geography=ve.query.geography,
    geovalue=ve.query.geovalue,
    specs=ve.query.specs            # Active interface to querySpec
    ),
  private = list(
    queryGeo=NULL,
    queryGeoValue=NULL,
    queryFile="",
    queryResults=NULL,              # list of data.frames with query results
    querySpec=list(),               # access via active specs
    queryPrep=NULL,                 # structure for running with summarizeDatasets code
    queryRunSpec=NULL,              # specs after filtering for validity against geography, etc.
    outputObject=NULL               # VEResults object, passed through "$new"
  )
)
