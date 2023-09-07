# Results.R
#' @include environment.R
NULL

# Documentation for VEResultsList
#' VEResultsList class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEResultsList
NULL

# Documentation for VEResults
#' VEResults class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEResults
NULL

# Documentation for VESelection
#' VESelection class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VESelection
NULL

self=private=NULL # To avoid global variable warnings

##############################
#
###### Implement VEResultsList
#
##############################

# Initialize a VEResultsList from a model or list of model stages
ve.resultslist.init <- function(stages=NULL,model=NULL) {

  # Set up the model, if provided explicitly
  if ( ! missing(model) && ! is.null(model) && inherits(model,"VEModel") ) self$Model <- model

  # Find model stages from which to get results
  if ( missing(stages) || is.null(stages) || ! all(inherits(stages,"VEModelStage")) ) {
    if ( ! is.null(self$Model) ) stages <- self$Model$findstages() else stages <- NULL
  } else {
    # have a list of stages; check model consistency and set self$Model if not already set
    modelNames <- unique(sapply(stages,function(s) s$Model$modelName))
    if ( length(modelNames)==1 ) {
      if ( is.null(self$Model) ) {
        self$Model <- stages[[1]]$Model
      } else if ( self$Model$modelName != stages[[1]]$Model$modelName ) {
        writeLog(Level="error",paste("Model stages do not come from model:",self$Model$modelName))
        writeLog(Level="error",modelNames)
        stages <- NULL
        self$Model <- NULL
      }
    } else {
      writeLog(Level="error","Model stage results must all come from the same model:")
      writeLog(Level="error",paste(modelNames,collapse=", "))
      stages <- NULL
      self$Model <- NULL # fall through to "invalid"
    }
  }

  # If we have stages, get the results for them
  if ( ! is.null(stages) ) {
    self$Results <- lapply(
      stages,
      function(stg) VEResults$new(stg$RunPath,ResultsName=stg$Name,ModelStage=stg)
    )
    names(self$Results) <- names(stages)
    # print(names(self$Results))
    # for ( r in self$Results ) print(r)
  } else stop("No model stages with results.")
  
  if ( ! is.null(self$Model) && ! is.null(self$Results) ) { # found something
    # Build results index (S/G/T/N plus other metadata) for use in selecting
    self$resultsIndex <- do.call("rbind", lapply( self$Results,function(r) r$list(details=TRUE) ) )
    valid <- sapply( self$Results, function(r) r$valid() )
  } else {
    valid <- FALSE # Didn't find anything...
  }

  # Validation error (stages missing results)
  if ( any( ! valid ) ) {
    if ( ! is.null(self$Results) && length(self$Results)>0 ) {
      msg <- paste("No results yet for stage(s): ",names(self$Results)[!valid],collapse=", ")
    } else {
      msg <- paste0(
        "Could not create result list for ",
        "model=",model,
        "; stages=",stages
      )
    }
    writeLog(
      msg,
      Level="error"
    )
    writeLog("Have you run the model?",Level="warn")
    self$isValid <- FALSE
  } else self$isValid <- TRUE

  if ( self$isValid ) {
    # Initialize selection
    self$selection <- VESelection$new(self)

    # Finally, pull the RunParam_ls and loadedParam_ls out of the model
    # Provides interface for environment.R/getSetup
    self$RunParam_ls <- self$Model$RunParam_ls
    self$loadedParam_ls <- self$Model$loadedParam_ls
  }
}

# export results using the data.frame exporter by default
# "format" parameter follows the rules of VEExporter and you can pass in conversion functions
#   from other packages:  e.g. format=tibble::tibble if you have that library; ... can provide
#   extra parameters to such a function.
ve.resultslist.extract <-  function(
  exporter="data.frame",connection=NULL,partition=NULL,selection=NULL,
  wantMetadata=FALSE,convertUnits=TRUE,
  format="data.frame",...
) {
  export <- self$export(exporter=exporter,
    connection=connection,partition=partition,selection=selection,
    wantMetadata=wantMetadata,convertUnits=convertUnits)
  return(invisible(structure(export$data(format=format,...),Exporter=export)))
} # shortcut to generate a list of data.frames via the export function

# Export results from each set in the results list to an exporter
# If selection is not defined, use the selection embedded in the results list (default=everything)
# The selection needs to come from the same model as the VEResultsList.
# Missing exporter uses the system default exporter ("csv")
# Otherwise it can be a character string (default="csv") or a pre-constructed VEExporter sub-class object
# connection (string or list, as expected by the exporter sub-class) and partition (character
# vector) are passed to exporter factory if exporter is not pre-constructed (see export.R for
# docs). If they're missing, the Model or system or default values will be sought (in that order).
# wantMetadata TRUE will generate a top-level tabel from the selection parameter list
# convertUnits TRUE will use an available display_units.csv conversion table (FALSE use raw Datastore units)
ve.resultslist.export <- function(
  exporter="default",connection=NULL,partition=NULL, # see export.R for docs on exporter, connection, partition
  selection=NULL,     # what we get from selecting a subset using VEResultsList$select() and VESelection operations
  wantMetadata=TRUE,  # Generate a metadata table at the root of the output
  convertUnits=TRUE   # Use "display_units.csv" to convert units for selected fields (otherwise Datastore units)
) {

  # Set up the exporter (defaults to CSV - use $extract to default to list of data.frames)
  if ( ! inherits(exporter,"VEExporter") ) {
    exporter <- self$Model$exporter(tag=exporter,connection=connection,partition=partition)
  }
  # Just ignore connection and partition if we're passing a pre-built exporter

  # Apply a selection if provided, otherwise use entire list of outputs
  # Reduces the resultsIndex to a subset then figures out S/G/T/N from whatever is left
  # Default is everything available in the VEResultsList
  # TODO: make sure list produces the full list...
  if ( ! is.null(selection) ) {
    self$select(selection) # just apply the selection, copying it
  }
  selection <- self$list(details=TRUE) # different from self$select, self$list does not deep-copy the selection

  # partition the selection into stages/scenarios and iterate across just those results
  # Note that the export partitioning is controlled by the partition parameter (or the default
  # for the type of exporter, which can be externally configured in visioneval.cnf), so if
  # all the Household tables (e.g.) are to be gathered into one containing all the scenarios,
  # the exporter will handle that by visiting each stage and appending its Household results
  # to the exporter Household output.
  selected.stages <- unique(selection$Scenario) # names of scenario

  writeLog(paste("Exporting results for model",self$Model$modelName),Level="warn")
  for ( stage in selected.stages ) {
    writeLog(paste("Exporting scenario",stage),Level="warn")
    stage.selection <- selection[selection$Scenario==stage,]
    # Just the elements selected for this stage
    stageResults <- self$Results[[stage]]
    writeLog("Extracting data from Datastore...",Level="warn")
    data <- stageResults$extract( # That's the VERresult$extract (below)
      selection=stage.selection, # keep all the Metadata fields...
      convertUnits=convertUnits
    ) # Generates a list of data.frames, with Units being the converted values
    # data is a named list of Groups, each being a named list of Tables, each of which is a
    # data.frame
    for ( group in names(data) ) {
      writeLog(paste("Exporting Group ",group),Level="warn")
      for ( table in names(data[[group]]) ) {
        writeLog(paste("Exporting Table ",table),Level="warn")
        # Generate input Metadata (fields in this S/G/T) which the exporter will merge with the
        # output metadata (table and field actually written).
        Metadata <- attr(data[[group]],"Metadata")[[table]] # Metadata should be an expeanded data.frame of field descriptors

        # The exporter's partitioning scheme will create tables and merge data.frames as requested
        # To see what got created, print the exporter returned from this function

        # Need to distinguish group=Global versus group=Year=Y
        exporter$write( data[[group]][[table]], Scenario=stage, Group=group, Table=table, Metadata=Metadata )
      }
    }
  }
      
  if ( wantMetadata ) {
    # write the Metadata to the exporter connection using name and location for metadata configured
    # in the connection description (usually a table with a distinctive name in the connection
    # root). The Metadata will identify what was written, and where (in the exporter outputs)
    # it was written to.
    exporter$writeMetadata()
  }

  # Printing the returned exporter will show the list of tables created and connection summary
  # doing exporter$data will enable pulling out a list of tables into data.frames
  return(invisible(exporter))
}

# Produce a bare named list of VEResults objects
# Probably don't ever need this except for the idiom below for getting a subset of results from
# this list. But scenarios can also be selected using the standard selection facility, or they
# can be selected when calling VEModel$results.
ve.resultslist.results <- function(stages=NULL) {
  if ( missing(stages) ) {
    return(self$Results)
  } else {
    return(self$Results[stages])
  }
}

# IDIOM to grab a subset of stages (if not done using model$results):
#    resultsSubset <-
#    VEResultsList$new(results$results()[vector.of.stages]) # vector.of.stages are names or indices

# Print summary of results in the list
ve.resultslist.print <- function(...) {
  cat("Results for Model '",self$Model$modelName,"':\n",sep="")
  cat("Model Status:", self$Model$printStatus(),"\n")
  modelStages <- self$Model$modelStages
  for ( nm in names(self$Results)) { # stage names
    modelStages[[nm]]$print(...)
  }
}

ve.resultslist.list <- function(pattern="", selected=TRUE, details=FALSE, ...) {
  # Show details about model fields
  # selected = TRUE shows just the selected elements
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field names (not groups or tables)
  # details = TRUE returns a data.frame self$resultsIndex (units, description)
  # details = FALSE returns just the "Name" vector from self$resultsIndex

  filter <- if ( missing(selected) || selected ) {
    which( 1:nrow(self$resultsIndex) %in% self$selection$selection )
  } else {
    rep(TRUE,nrow(self$resultsIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$resultsIndex$Name,ignore.case=TRUE )
  }
  if ( missing(details) || ! details ) {
    ret.value <- with( self$resultsIndex[ filter, ,drop=FALSE], paste(Scenario,Group,Table,Name,sep="/") ) # generates a character vector
  } else {
    ret.value <- self$resultsIndex[ filter, ] # Generates a data.frame with all metadata columns
  }
  return(unique(ret.value))
}

# TODO: is this function ever used, or is it still relevant?
# TODO: written to work on an individual VEResults - now should list all selected
# TODO: could we just add the input files and directories to standard metadata?
# We won't necessarily know the input file until after the model is run (otherwise, this function should have been a
#  member of VEModel)
ve.resultslist.inputs <- function( fields=FALSE, module="", filename="" ) {
  # fields=TRUE, show all Fields/Datasets that originated as file inputs (lists all columns within input files)
  # fields=FALSE, just show the module, file, input directory (lists the input files)
  # This is a convenience function for processing the results index. Need to ensure File and
  # InputDir are included in that table.
  if ( ! self$valid() ) stop("Model has not been run yet.")

  if ( ! missing(fields) && fields ) {
    ret.fields <- c("Scenario","Module","Group","Table","Name","File","InputDir","Units","Description")
  } else {
    ret.fields <- c("Scenario","Module","Name","File","InputDir")
  }

  filter <- nzchar(self$resultsIndex$File)
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,self$resultsIndex$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,self$resultsIndex$File)
  }

  ret.value <- unique(self$resultsIndex[ filter, ret.fields ])
  return( ret.value[order(ret.value$InputDir,ret.value$File),] )
}

ve.resultslist.select <- function(selection) {
  return.self <- missing(selection)
  if ( return.self ) {
    selectlist <- self$list()
    self$selection <- VESelection$new(self,selectlist)
    
  } else {
    if ( ! inherits(selection, "VESelection") || self$Model$modelPath != selection$resultsList$Model$modelPath ) {
      # see if we can make a selection (stop inside $new if selection can't be interpreted)
      selection <- VESelection$new(self,selection)
    } else self$selection <- selection
  }
  invisible( self$selection )
}

# List out the units applied to results for this scenario (see addDisplayUnits above)
ve.resultslist.units <- function(selected=TRUE,display=NULL) {
  # if display==TRUE, show DisplayUnits plus Datastore Units
  # if display==FALSE, show only Datastore units
  # if display is NULL (default) merge Display and Datastore and show source
  # selected == FALSE shows units for ALL fields, not just selected
  # Transiently attaches DisplayUnits to field list (transient because user
  #   may be editing the file in this session)
  # Displays a data.frame for the selected (TRUE) or all (FALSE) fields with
  #   Group, Table, Name, DisplayUnits, UnitsSource ("Datastore" or DisplayUnitsFilePath)
  selected <- if ( selected ) self$selection$selection else 1:nrow(self$resultsIndex)
  Units_df <- self$resultsIndex[ selected, c("Scenario","Group","Table","Name","Units") ]
  Units_df$Source <- "Datastore"
  returnFields <- c("Scenario","Group","Table","Name","Units","Source")
  if ( ! is.logical(display) || display ) {
    # Add Display Units if requested
    Units_df <- addDisplayUnits(Units_df,Param_ls=self$RunParam_ls)
    displayUnits <- !is.na(Units_df$DisplayUnits) # find elements where DisplayUnits are available
    Units_df$Source[ displayUnits ] <- basename(Units_df$DisplayUnitsFile[ displayUnits ])
    if ( is.null(display) ) {
      # merge into single Units Column
      Units_df$Units[ displayUnits ] <- Units_df$DisplayUnits[ displayUnits ]
    } else {
      returnFields <- c(returnFields,"DisplayUnits")
    }
  }
  return( Units_df[,returnFields] )
}

# Find fields (as objects) within the current results list
# Note: "Scenarios" correspond to model stages in the model
# We're using the "end user" friendly word for "reportable stages". The resultsList can be
# generated with base stages - see VEModel$results.
ve.resultslist.find <- function(pattern=NULL,Scenario=NULL,Group=NULL,Table=NULL,Name=NULL,select=FALSE) {
  selection <- self$select() # generate a base selection from what is already selected.
  found <-selection$find(pattern=pattern,Scenario=Scenario,Group=Group,Table=Table,Name=Name,as.object=TRUE)
  # without "select=TRUE", found is an independent selection (not bound to results)
  if ( select ) found <- self$select(found) # create a selection from this set of results
  return( found )
}

#' @export
VEResultsList <- R6::R6Class(
  "VEResultsList",
  public = list(
    # public data
    Model          = NULL,
    Results        = NULL,
    isValid        = FALSE,
    selection      = NULL,
    resultsIndex   = NULL,  # consolidated datastore index for all stages
    RunParam_ls    = NULL,  # interface for environment.R/getSetup which is used to find export directories
    loadedParam_ls = NULL,

    # methods
    initialize=ve.resultslist.init,
    results=ve.resultslist.results,  # manipulate the inner list
    print=ve.resultslist.print,      # summary of model results (index)
    export=ve.resultslist.export,    # move the results to an external data storage format
    extract=ve.resultslist.extract,  # generate nested list of data.frames from model results (for further processing in R)
    list=ve.resultslist.list,        # show the consolidated resultsIndex (used by export to develop metadata table)
    select=ve.resultslist.select,    # return the list's selection definition
    find=ve.resultslist.find,        # constructs but does not embed the selection definition
    units=ve.resultslist.units,      # Set units on field list (modifies self$modelIndex) TODO: Move/wrap in VEResultsList
    valid=function() self$isValid    # report state of validity
  )
)

##########################
#
###### Implement VEResults
#
##########################

# VEResults handles the low-level interactions with the ModelState_ls and Datastore for a
# single stage. It automatically does virtual "flattening" by walking back up the StartFrom
# tree. It will generate an index list of all the Group/Table/Name for the specific scenarios
# that will be later used to generate table specifications for export, and also for selecting
# subsets of the state results.

# Create VEResults object (manipulates Datastore/ModelState)
# Extracts tabular data
ve.results.init <- function(ResultsPath,ResultsName=NULL,ModelStage=NULL) {
  # ResultsPath is the normalized path to a directory containing the model results
  #  typically from a Reportable model stage. Expect to find a ModelState.Rda file
  #  and a Datastore in that folder.
  self$resultsPath <- ResultsPath
  self$Name <- if ( !is.character(ResultsName) ) basename(ResultsPath) else ResultsName
  self$index() # Will find model state
  self$RunParam_ls <- self$ModelState()$RunParam_ls
  self$loadedParam_ls <- self$RunParam_ls # establish interface for environment.R/getSetup
  self$modelStage <- ModelStage # may be NULL; only used to get stage elements for category scenarios via the R visualizer
  return(self$valid())
}

# Get tables of data from the Datastore for a specific stage/scenario
# Return a list of groups containing data.frames for each table/name set within the group
ve.results.extract <- function(
  selection,             # data.frame of Scenario/Group/Table/Name/Units/DisplayUnits elements for this stage
  convertUnits=TRUE      # will convert if display units are present; FALSE not to attempt any conversion (use Units from selection)
) {
  if ( ! self$valid() ) {
    bad.results <- if ( ! is.null(self$modelStage) ) self$modelStage$Name else self$resultsPath
    stop("Model Stage contains no results: ",bad.results)
  }
  scenarioName <- if ( is.null(self$modelStage) ) basename(self$resultsPath) else self$modelStage$Name

  if ( convertUnits) {
    selection <- addDisplayUnits(selection,Param_ls=self$RunParam_ls)
  } else {
    selection$DisplayUnits <- selection$Units
  }
  if ( ! "Scenario" %in% names (selection ) ) { # Should be redundant, and Scenario if present should be unique
    selection <- cbind(Scenario=scenarioName,selection)
  }
  extractTables <- unique(selection[,c("Group","Table")])
  extractGroups <- unique(extractTables$Group)

  QueryPrep_ls <- self$queryprep()
  results <- list()

  for ( group in extractGroups ) {
    # Build Tables_ls for readDatastoreTables
    Tables_ls <- list() 
    tables <- extractTables$Table[ extractTables$Group == group ]
    if ( length(tables)==0 ) next # should not happen given how we built extract
    Metadata <- list()
    for ( table in tables ) {
      # get table Metadata
      Metadata[[table]] <- selection[ selection$Group==group & selection$Table==table, ]
      fields <- Metadata[[table]][ , c("Name","DisplayUnits") ]

      # set up unit conversion...
      dispUnits <- fields$DisplayUnits # Will already be fields$Units if not converting
      names(dispUnits) <- fields$Name
      Tables_ls[[table]] <- dispUnits
    }

    # Get a list of data.frames, one for each Table configured in Tables_ls
    Data_ls <- visioneval::readDatastoreTables(Tables_ls, group, QueryPrep_ls)
    if ( ! is.list(Data_ls) ) stop("Data_ls is not a list")

    # Report Missing Tables from readDatastoreTables
    HasMissing_ <- unlist(lapply(Data_ls$Missing, length)) != 0
    if (any(HasMissing_)) {
      WhichMissing_ <- which(HasMissing_)
      Missing_ <- character(0)
      for (i in WhichMissing_) {
        Missing_ <- c(
          Missing_,
          paste0(
            names(Data_ls$Missing)[i], " (",
            paste(Data_ls$Missing[[i]], collapse = ", "), ")"
          )
        )
      }
      msg <- paste("Missing Tables (Datasets):",paste(Missing_, collapse = "\n"),sep="\n")
      stop( writeLog( msg, Level="error" ) )
    }

    # The folloing code exists to handle a bad part of the VERPAT design, where base and future
    # vehicles are loaded into the same table. Fields associated with base and future may
    # have different lengths (each field is only associated with one). This code will
    # be used to split out the base and future into different tables based on matching
    # the lengths of each field column.

    # Handle tables with different lengths of data elements ("multi-tables")
    # readDatastoreTables will have returned a ragged list rather than a data.frame

    if ( ! all(is.df <- sapply(Data_ls$Data,is.data.frame)) ) {
      # Unpack "multi-tables"
      MultiTables <- Data_ls$Data[which(! is.df)] # usually, there's just one of these...
      writeLog(paste("Processing multitables: ",paste(MultiTables,collapse=",")),Level="warn")
      if ( length(MultiTables) > 0 ) {
        for ( multi in names(MultiTables) ) {
          # multi is a list of datasets not made into a data.frame by readDatastoreTables
          multi.data <- MultiTables[[multi]]
          lens <- sapply(multi.data,length) # vector of lengths of columns
          multi.len <- unique(lens)
          for ( dfnum in 1:length(multi.len) ) { # work through unique dataset lengths
            dfname <- paste(multi,multi.len[dfnum],sep="_") # Encode the length onto the table name
            fordf <- which(lens==multi.len[dfnum])
            try.df <- try( data.frame(multi.data[fordf]) )
            if ( ! is.data.frame(try.df) ) {
              msg <- paste("Could not make data.frame from Datastore Table",multi)
              stop( writeLog( msg, Level="error" ) )
            }
            Data_ls$Data[[dfname]] <- try.df
            multiMetadata <- Metadata[[multi]]
            newMetadata <- multiMetadata[multiMetadata$Name %in% names(try.df),] # drop rows that disappeared from try.df
            Metadata[[dfname]] <- newMetadata
          }
        }
        # Remove original tables (they will now have names like "Vehicles.13747" and "Vehicles.15444")
        # That way we can later merge equivalent tables across multiple scenarios depending on the
        # consolidation strategy in VEResultsList$export.
        # TODO: really need to fix the source problem in VERPAT (if anyone ever uses it again...)
        removeTables <- -which(names(Data_ls$Data) %in% names(MultiTables))
        Data_ls$Data <- Data_ls$Data[removeTables]
        Metadata <- Metadata[removeTables]
      }
    }
    # Now visit each of the resulting data.frames and prepend the Scenario, Global and Year columns
    if ( group=="Global" ) {
      Data_ls$Data <- lapply(Data_ls$Data,function(df) {
        if ( any(c("Global","Year") %in% names(df)) ) stop("Program error: Global or Year already present in ",group,"/",table)
        cbind(Global=group,Year=NA,df)
      })
    } else { # group==someYear
      Data_ls$Data <- lapply(Data_ls$Data,function(df) {
        if ( any(c("Global","Year") %in% names(df)) ) stop("Program error: Global or Year already present in ",group,"/",table)
        cbind(Global=NA,Year=group,df)
      })
    }
    Data_ls$Data <- lapply(
      Data_ls$Data,function(df) {
        if ( "Scenario" %in% names(df) ) stop("Program error: Scenario already present in ",group,"/",table)
        cbind(Scenario=scenarioName,df)
      }
    )
    # Make sure Metadata includes added column descriptions
    Metadata <- lapply(names(Metadata),function(tbl) {
      dfm <- Metadata[[tbl]]
      rnames <- names(dfm)
      metaRow <- rep("",length(rnames))
      names(metaRow) <- rnames
      metaRow[c("Scenario","Group","Table")] <- c(scenarioName,group,tbl)
      if ( ! "Global" %in% dfm$Name ) {
        metaRow["Name"] <- "Global"
        metaRow["Description"] <- "Global group indicator"
        dfm <- rbind(dfm,metaRow)
      }
      if ( ! "Year" %in% dfm$Name ) {
        metaRow["Name"] <- "Year"
        metaRow["Description"] <- "Year group indicator"
        dfm <- rbind(dfm,metaRow)
      }
      if ( ! "Scenario" %in% dfm$Name ) {
        metaRow["Name"] <- "Scenario"
        metaRow["Description"] <- "Scenario group indicator"
        dfm <- rbind(dfm,metaRow)
      }
      dfm
    })
    names(Metadata) <- names(Data_ls$Data)

    # Process the table data.frames into results, adding Metadata as an attribute
    results[[ group ]] <- structure(Data_ls$Data,Metadata=Metadata)
  }
  invisible(results) # results will be a named list of groups from the stage results, with each group being
                     # a named list of tables (data.frames) extracted for that group
}

# Check results validity (all files present)
ve.results.valid <- function() {
  valid <- ! is.null(self$RunParam_ls) &&
           dir.exists(self$resultsPath) &&
           !is.null(self$modelIndex) && length(self$modelIndex)>0
  modelStatePath <- file.path(self$resultsPath,visioneval::getRunParameter("ModelStateFile",Param_ls=self$RunParam_ls))
  valid <- valid && all(file.exists(modelStatePath))
  return(valid)
}

# Check if a specified attribute belongs to the Datastore row
attributeExist <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(TRUE)
    }
  }
  return(FALSE)
}

# Get a specified attribute for a Datastore row
attributeGet <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(attr_value)
    }
  }
  return(NA)
}

ve.results.modelstate <- function(ModelState_ls=NULL) {
  if ( ! is.null(ModelState_ls) ) {
    if ( is.null(private$modelStateEnv) ) {
      private$modelStateEnv <- new.env()
    }
    private$modelStateEnv$ModelState_ls <- ModelState_ls
  }
  return (private$modelStateEnv$ModelState_ls)
}

ve.results.list <- function(pattern="", details=FALSE ) {
  # Show details about model fields
  # details = TRUE returns a data.frame self$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from self$modelIndex
  
  if ( ! self$valid() ) stop("Model has not been run yet.")

  filter <- rep(TRUE,nrow(self$modelIndex))

  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$modelIndex$Name,ignore.case=TRUE )
  }

  if ( missing(details) || ! details ) {
    ret.value <- with( self$modelIndex[ filter, ], paste(Group,Table,Name,sep="/") ) # generates a character vector
  } else {
    ret.value <- self$modelIndex[ filter, ] # Generates a data.frame with all columns
  }
  return(unique(ret.value))
}

ve.results.index <- function() {
  # Load model state from self$resultsPath
  # Note that if the model is using a non-standard ModelState name,
  # we might not find it here. ModelState name should be set globally.
  FileName=normalizePath( file.path(
    self$resultsPath, # Should already include ResultsDir
    visioneval::getModelStateFileName()
  ), winslash="/", mustWork=FALSE)
  if ( file.exists(FileName) ) {
    ms <- try(visioneval::readModelState(FileName=FileName,envir=new.env()))
  } else {
    ms <- NULL
  }
  if ( ! is.list(ms) ) {
    self$ModelState(list())
    writeLog(Level="info",paste("No ModelState:",FileName))
    return(list())
  }
  self$ModelState(ms) # save ModelState
  if ( is.null(self$RunParam_ls) && is.list( ms ) ) {
    self$RunParam_ls <- ms$RunParam_ls
  }

  msList <- rev(visioneval::getModelStatePaths(dropFirst=FALSE,envir=private$modelStateEnv))
  Index <- data.frame()
  Inputs <- data.frame()

  if ( length(msList) > 0 ) {
    msFirst <- TRUE
    for ( ms in msList ) {
      dsListing <- ms$ModelState_ls$Datastore
      if ( msFirst ) {
        combinedDatastore <- dsListing
        msFirst <- FALSE
      } else {
        combinedDatastore <- visioneval::mergeDatastoreListings(combinedDatastore,dsListing)
      }
    }
  }

  ds <- combinedDatastore

  Description <- sapply(ds$attributes, attributeGet, "DESCRIPTION",simplify=TRUE) # should yield a character vector
  Module <- sapply(ds$attributes, attributeGet, "MODULE",simplify=TRUE) # should yield a character vector
  Units <- sapply(ds$attributes, attributeGet, "UNITS",simplify=TRUE) # should yield a character vector
  InputDir <- sapply(ds$attributes, attributeGet, "INPUTDIR",simplify=TRUE) # should yield a character vector
  InputDir[ is.na(InputDir) ] <- ""
  File <- sapply(ds$attributes, attributeGet, "FILE",simplify=TRUE) # should yield a character vector
  File[ is.na(File) ] <- ""

  #scenario <- rep(visioneval::getRunParameter("Scenario",Default="Unknown Scenario",Param_ls=self$RunParam_ls),length(Description))
  scenario <- self$Name

  splitGroupTableName <- strsplit(ds$groupname, "/")
  if ( length(Description) != length(splitGroupTableName) ) stop("Inconsistent table<->description correspondence")

  maxLength <- max(unlist(lapply(splitGroupTableName, length)))
  if ( maxLength != 3 ) {
    writeLog(Level="warn",paste0("Model state at ",self$resultsPath," is incomplete (",maxLength,")"))
    return(list())
  }
  splitGroupTableName <- lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x))))
  # splitGroupTableName is a list of character vectors with Group, Table, Name components

  fieldGTN <- do.call(rbind.data.frame,splitGroupTableName)
  names(fieldGTN) <- c("Group","Table","Name")

  # Build parallel data.frame for Inputs (including File parameter)
  # message("Input data frame...")
  Index <- data.frame(
    Group       = fieldGTN$Group,
    Table       = fieldGTN$Table,
    Name        = fieldGTN$Name, # Should be identical to ds$name
    Description = Description,
    Units       = Units,
    # TODO: May need some other specification fields in order to identify variable type for SQL or other export
    Module      = Module,
    Scenario    = scenario,
    File        = File,          # "" if not an Input
    InputDir    = InputDir       # "" if not an Input
  )

  # GroupTableName is now a data.frame with nine columns
  # complete.cases blows away the rows that have any NA values
  # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each column)
  # Reduces the raw Datastore index to just the Fields ("Name"s) in the Datastore
  ccases <- stats::complete.cases(Index[,c("Group","Table","Name")])
  Index <- Index[ccases,]
  self$modelIndex <- Index
  invisible(self$modelIndex)
}

# Helper function to attach DisplayUnits to a list of Group/Table/Name rows in a data.frame
# Need to do this in VEResults since we need access to the model state...
# TODO: Move this to VEResultsList (using Param_ls from Model or first VEResults)
addDisplayUnits <- function(GTN_df,Param_ls) {
  # GTN_df is a data.frame with "Group","Table","Name" rows for each Name/field for which display
  #  units are sought. Always re-open the DisplayUnits file, as it may have changed since the last
  #  run.
  ParamPath <- visioneval::getRunParameter("ParamPath",Param_ls=Param_ls) # location of structural files

  DisplayUnitsFile <- visioneval::getRunParameter("DisplayUnitsFile",Param_ls=Param_ls)
  # Where to look for DisplayUnitsFile...
  # By its name, in ParamPath for model (preferred) or runtime directory (global values)
  DisplayUnitsFile <- c(file.path(c(ParamPath,getRuntimeDirectory()),DisplayUnitsFile))

  existing <- file.exists(DisplayUnitsFile)
  if ( ! any(existing) ) {
    writeLog( Level="info",
      c("Specified DisplayUnits file does not exist (using default units):",paste(DisplayUnitsFile,collapse="; "))
    )
    return( cbind(GTN_df,DisplayUnits=NA,DisplayUnitsFile="None") )
  } else {
    DisplayUnitsFile <- DisplayUnitsFile[existing][1]
  }
#   cat("DisplayUnitsFile:\n")
#   print(DisplayUnitsFile)
  displayUnits <- try(utils::read.csv(DisplayUnitsFile),silent=TRUE)   # May fail for various reasons
  if ( ! "data.frame" %in% class(displayUnits) ) {
    writeLog( Level="warn",
      c(
        "Error reading DisplayUnits file:",
        DisplayUnitsFile,
        paste("Error:",conditionMessage(attr(displayUnits,"condition")))
      )
    )
    return( cbind(GTN_df,DisplayUnits=NA, DisplayUnitsFile="None") )
  }
  displayColumns <- c("Group","Table","Name","DisplayUnits")
  if ( ! all( displayColumns %in% names(displayUnits) ) ) {
    writeLog( Level="warn",
      c("Specified DisplayUnits file does not have correct columns:",DisplayUnitsFile,
        paste("Columns:",names(displayUnits),collapse=", ")
      )
    )
    return( cbind(GTN_df,DisplayUnits=NA, DisplayUnitsFile="None") )
  }
  # Remove existing DisplayUnits, if present, prior to merging
  if ( "DisplayUnits" %in% names(GTN_df) ) GTN_df <- GTN_df[,! grepl("DisplayUnits",names(GTN_df),fixed=TRUE)]
  # Only look at relevant columns in displayUnits when merging
  displayUnits <- try( merge(GTN_df,displayUnits[,displayColumns],by=c("Group","Table","Name"),all.x=TRUE), silent=TRUE )
  if (
    ! "data.frame" %in% class(displayUnits) ||
    ! all( c("Group","Table","Name","DisplayUnits") %in% names(displayUnits) ) # it can have other fields, e.g. original Units
  ) {
    if ( "data.frame" %in% class(displayUnits) ) {
      displayUnits <- paste("Bad Fields - ",names(displayUnits),collapse=", ")
    } else {
      displayUnits <- conditionMessage(attr(displayUnits,"condition"))
    }
    writeLog( Level="warn",
      c(
        "Error reading DisplayUnits file:",
        DisplayUnitsFile,
        paste("Error:",displayUnits)
      )
    )
    return( cbind(GTN_df,DisplayUnits=NA, DisplayUnitsFile="None") )
  }
  # Add displayUnitsFile
  displayUnits$DisplayUnitsFile <- DisplayUnitsFile
  # get here with displayUnits being GTN_df, augmented by matching DisplayUnits
  return(displayUnits) # Minimally includes Group, Table, Name, DisplayUnits, DisplayUnitsFile
}

# Return a named list of ScenarioElements and Levels for this set of
# results (from the ModelStage) - for use with category scenarios.
ve.results.elements <- function() {
  # Get scenario element names plus level values for associated model stage
  # Model stage must have scenario elements to use Visualizer
  if ( is.null(self$modelStage) ) return(list()) # need the modelStage to get elements
  elements <- self$modelStage$ScenarioElements
  if ( !is.character(elements) ) {
    return(list())
  }
  return(as.list(elements)) # converted named vector to named list
}

# Wrapper for visioneval::copyDatastore
# TODO: add a wrapper in VEResultsList that will copy all the model results to another
#  ToDir - VEResultsList will need to manage the directories...
ve.results.copy <- function(ToDir, Flatten=TRUE, DatastoreType=NULL, overwrite=FALSE) {
  if ( missing(ToDir) ) {
    stop(writeLog("Must provide target directory path.",Level="error"))
  }
  if ( ( existing <- dir.exists(ToDir) ) && ! overwrite ) {
    stop(writeLog("ToDir exists: provide another ToDir or set overwrite=TRUE",Level="error"))
  } else if ( existing && overwrite ) {
    unlink(ToDir,recursive=TRUE)
    existing <- FALSE
  }
  if ( ! existing ) dir.create(ToDir)

  owd <- setwd(self$resultsPath) # copyDatastore must work in that directory
  on.exit(setwd(owd))
  success <- visioneval::copyDatastore(
    ToDir=ToDir,
    Flatten=Flatten,
    DatastoreType=DatastoreType,
    envir=self$ModelStateEnv
  )
  if ( ! success ) writeLog("Attempt to copyDatastore Failed!",Level="error")
  invisible(success)
}

# Use this to prepare the VEResults for processing by VEQuery
ve.results.queryprep <- function() {
  visioneval::prepareForDatastoreQuery(
    DstoreLocs_ = file.path(self$resultsPath,self$ModelState()$DatastoreName),
    DstoreType  = self$ModelState()$DatastoreType
  )
}

# Print the VEResults summary (called recurively when printing VEResultsList)
ve.results.print <- function(name="",details=FALSE,...) {
  # Update for output
  if ( details ) {
    cat("VEResults object for",if(nzchar(name)) name else self$Name,":\n")
    print(self$resultsPath,...)
    cat("Output is valid:",self$valid(),"\n")
  } else {
    cat(name,": Results ",if(self$valid()) "NOT ","Available\n",sep="")
  }
}

# Definition of VEResults R6 class
# Constructed within VEResultsList (above)

#' @export
VEResults <- R6::R6Class(
  "VEResults",
  public = list(
    # public data
    Name           = NULL,
    modelStage     = NULL,
    resultsPath    = NULL,
    modelIndex     = NULL,
    RunParam_ls    = NULL,
    loadedParam_ls = NULL,

    # methods
    initialize=ve.results.init,
    index=ve.results.index,          # Index Datastore from ModelState (part of init)
    copy=ve.results.copy,            # Apply visioneval::copyDatastore
    valid=ve.results.valid,          # has the model been run, etc.
    extract=ve.results.extract,      # generate data.frames from model results
    list=ve.results.list,            # show the modelIndex - is this used other than to initialize VEResultsList?
    queryprep=ve.results.queryprep,  # For query or other external access
    print=ve.results.print,          # summary of model results (index)
    elements=ve.results.elements,    # Get scenario elements (named list of scenario levels) for this scenario
    ModelState=ve.results.modelstate # Set/Get the model state for these results
  ),
  private = list(
    modelStateEnv=NULL
  )
)

############################
# VESelection Implementation
############################

# VESelection is managed within VEResultsList, and will specify a subset of available
# Scenarios (Reportable Stages), Groups (Year/Global), Tables, and Names (fields) (S/G/T/N).
# The mechanism uses integer indexes into the table of fields found in the Results
# based on ModelState_ls/Datastore list for each Result set. But character vectors formatted
# as "S/G/T/N" will also be understood, and those are produced for human readability.
# The integer index is easier to use when it comes time actually to select stuff.

# results is a VEResultsList
# selection is pretty much anything that can be recognized as a selection
# (vector of integers, vector of S/G/T/N names, another selection on the same or a different
# set of model results - if same, treat like vector of integers; if different, first generate
# a character vector of S/G/T/N (and only select the ones that are present in this
# selection/resultset).
ve.select.init <- function( results, select=integer(0) ) {
  # default select (integer(0)) selects everything
  # self$selection is just a list of integers pointing to rows
  #  in self$results$modelIndex
  self$resultsList <- results
  rows <- self$parse(select)
  if ( is.null(rows) || any(is.na(rows)) ) {
    self$selection <- as.integer(NA) # no rows selected
  } else if (
    ! is.numeric(rows) ||
    length(rows)==0 ||
    ! min(rows)>0 ||
    max(rows)>nrow(self$resultsList$resultsIndex) ) {
    self$selection <- 1:nrow(self$resultsList$resultsIndex)
  } else {
    self$selection <- rows
  }
}

ve.select.copy <- function(select) VESelection$new(self$resultsList,self$selection)

ve.select.print <- function(details=FALSE) {
  # print the selected fields
  if ( ! self$valid() ) cat("No results: has model been run?\n") else {
    if ( ! details ) {            # just the field names (see below)
      print( self$fields() )
    } else {                      # full data frame of selected model results
      print( self$show() )
    }
  }
}

ve.select.show <- function() { # show the subset of results$modelIndex for this selection
  return( self$resultsList$resultsIndex[ self$selection, ] )
}

ve.select.valid <- function() { return(self$resultsList$valid()) }

ve.select.scenarios <- function() {
  if ( ! self$resultsList$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    # Handle case where a previous selection eliminated all scenarios
    writeLog("No scenarios selected",Level="warn")
    return(character(0))
  }
  idxScenarios <- unique(self$resultsList$resultsIndex[self$selection,c("Scenario"),drop=FALSE])
  scenarios <- sort(idxScenarios$Scenario) # Scenario
  return(scenarios)
}

ve.select.groups <- function() {
  if ( ! self$resultsList$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    writeLog("No groups selected",Level="warn")
    return(character(0))
  }
  idxGroups <- unique(self$resultsList$resultsIndex[self$selection,c("Scenario","Group"),drop=FALSE])
  groups <- sort(paste(idxGroups$Scenario,idxGroups$Group,sep="/")) # Scenario/Group
  return(groups) # Group
}

ve.select.tables <- function(nameOnly=FALSE) {
  if ( ! self$resultsList$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    writeLog("No tables selected",Level="warn")
    return(character(0))
  }
  idxTables <- unique(self$resultsList$resultsIndex[self$selection,c("Scenario","Group","Table")])
  tables <- if ( nameOnly ) {
    unique(idxTables$Table) # pure name
  } else {
    sort(paste(idxTables$Scenario,idxTables$Group,idxTables$Table,sep="/")) # Scenario/Group/Table
  }
  return( tables )
}

ve.select.fields <- function() {
  # extract fields from the index where groups and tables are selected
  if ( ! self$resultsList$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    writeLog("No fields selected",Level="warn")
    return(character(0))
  }
  idxFields <- self$resultsList$resultsIndex[self$selection,c("Scenario","Group","Table","Name")]
  return(sort(paste(idxFields$Scenario,idxFields$Group,idxFields$Table,idxFields$Name,sep="/"))) # Scenario/Group/Table/Name
}

# Here's how to select if we keep the selection as Scenario/Group/Table/Name:
# 
#   Here is a generic solution for this type of problem which is very efficient:
# 
# data.1.ID <- paste(data.1[,1],data.1[,2],data.1[,3],data.1[,4])
# keep.these.ID <- paste(keep.these[,1],keep.these[,2],keep.these[,3],keep.these[,4])
# desired.result <- data.1[data.1.ID %in% keep.these.ID,]

# Internal helper function to make a selection vector out of various other types of objects

ve.select.parse <- function(select) {
  # Though select can be a vector of field names, they need to be the full Scenario/Group/Table/Name field names,
  #  so you should get them from ve.select.find, rather than manually construct them.
  #  Anything mentioned by name in select will be ignored if that combination of S/G/T/N is
  #  not present in the associated results.
  # if select is NA, return NA
  # select can be another VESelection
  #   if it is the same model, just copy its selection
  #   if not the same model, get other selection's VEResults object and parse that
  if ( "VESelection" %in% class(select) ) {
    if ( select$resultsList$Model$modelResults != self$resultsList$Model$modelResults ) {
      # Selection came from a different set of results, so equate them via the fields list
      # Presumes the scenario names are the same.
      # In practice, this will probably never happen
      select <- select$fields()
      # fall through to parse the character strings
    } else {
      return( select$selection )
    }
  }
  # select can be another VEResultsList object
  #   if the other VEResultsList is not from the same model, use its $fields set of names
  #   then parse as a character vector
  #   if it is the same model, just copy its selection
  if ( "VEResults" %in% class(select) ) {
    if ( select$resultsPath != self$resultsList$resultsPath ) {
      select <- select$selection$fields()
    } else {
      return( select$selection$selection )
    }
  }
  # select can be a character vector
  #   split the vector into scenario/group/table/name, providing defaults
  # locate the rows with matching scenario/group/table/name in results$modelIndex
  #   That vector of row indices becomes the selection to act on
  if ( is.character(select) ) {
    build <- integer(0)
    for ( s in select ) {
      t <- unlist(strsplit(s,"/"))
      name <- c( rep(NA,4-length(t)), t )
      if ( is.na(name[4]) || ! nzchar(name[4]) ) next          else field=name[4]
      if ( is.na(name[3]) || ! nzchar(name[3]) ) table <- NULL else table=name[3]
      if ( is.na(name[2]) || ! nzchar(name[2]) ) group <- NULL else group=name[2]
      if ( is.na(name[1]) || ! nzchar(name[1]) ) scenario <- NULL else scenario=name[1]
      fld <- self$find(Name=field,Group=group,Table=table,Scenario=scenario,as.object=FALSE)
      build <- union( build, fld )
    }
    select <- build # should be a vector of integers
  }
  
  # if select is a numeric vector, validate its range and return it
  if ( is.numeric(select) ) {
    if ( length(select)>0 ) {
      if ( any(is.na(select)) ) return( as.integer(NA) )
      if ( ! ( min(select)>0 && max(select)<=nrow(self$resultsList$resultsIndex) ) ) {
        message("Field selection out of range")
        return( as.integer(NA) )
      }
    }
    return( select )
  }
  message("Invalid field selection specification")
  message(deparse(select))
  return( as.integer(NA) )
}

# Select something new (optional) and then return the (possibly updated) self
ve.select.select <- function(select) {
  if ( ! missing(select) ) self$selection <- self$parse(select)
  invisible(self)
}

# NOTE: Strictly speaking, the Datastore table key fields should be recoverable from the module
# specifications, but I haven't found a way to do that comprehensively yet. That's a deficiency
# in the table specification that makes exporting to SQL rudimenary (no indexes or foreign keys)
allTheKeys = c(
  "Marea","Azone","Bzone",
  "HhId","VehId","WkrId"
)

ve.select.addkeys <- function(Scenario=NULL,Group=NULL,Table=NULL,Keys=NULL) {
  # Helper to move key fields across into selection
  # In general, it may be too complex to specify S/G/T - this function should
  #    just select the key fields in any currently selected S/G/T
  # "Scenario" if not specified will be currently selected scenarios
  # "Group" if not specified will be currently selected groups
  # "Table" if not specified will be currently selected tables
  # "Keys" if not specified will be all of them; if provided here,
  # will drop any that are not in the Keys list (so if you give it
  # something that is not a "key", it just ignores it).
  if ( missing(Scenario) ) Scenario <- self$scenarios()
  if ( missing(Group) )    Group <- self$groups()
  if ( missing(Table) )    Table <- self$tables(nameOnly=TRUE) # returns just the Table name(s)
  theKeys <- allTheKeys
  if ( is.character(Keys) ) {
    theKeys <- setdiff(allTheKeys,Keys) # Only include the named ones
  }
  # add the Key fields for selected Group/Table if they're not
  # already there
  theKeys <- self$find(Scenario=Scenario,Group=Group,Table=Table,Name=theKeys)
  self$or( theKeys )
  invisible( self )
}

# Find does NOT alter the object it is called on unless 'select=TRUE'
# It either produces a new VESelection from the matching elements of self$selection (as.object==TRUE)
# OR it produces a vector of matching element indices (as.object==FALSE)
ve.select.find <- function(pattern=NULL,Scenario=NULL,Group=NULL,Table=NULL,Name=NULL,as.object=TRUE,select=FALSE) {
  # if pattern (regexp) given, find names matching pattern (only within the "fields"/Name part)
  # if group or table not specified, look in any group or table
  # return vector of indices for matching rows or (as.object==TRUE) a new VESelection object
  if ( is.character(Group) ) Group <- basename(Group)  # because $groups returns Scenario/Group
  if ( is.character(Table) ) Table <- basename(Table)
  if ( is.character(Name)  ) Name  <- basename(Name)

  searchScenario <- Scenario
  searchGroup <- Group
  searchTable <- Table
  searchName  <- Name
  newSelection <- self$selection
  newSelection <- with( self$resultsList$resultsIndex, {
    if ( !is.null(pattern ) ) {
      fld <- grepl(pattern,Name,ignore.case=TRUE)     # RegEx search for name
    } else if ( !is.null(searchName) ) {
      fld <- Name %in% searchName                     # Exact name match
    } else {
      fld <- rep(TRUE,nrow(self$resultsList$resultsIndex))  # Start with all selected
    }
    if ( !is.null(searchScenario) ) {
      fld <- fld & (Scenario %in% searchScenario)
    }
    if ( !is.null(searchGroup) ) {
      if ( any(searchGroup %in% c("Year","Years","AllYears")) ) {  # shorthand for non-Global group
        group <- Group != "Global"
      } else {
        group <- (Group %in% searchGroup)
      }
      fld <- fld & group
    }
    if ( !is.null(searchTable) ) {
      fld <- fld & (Table %in% searchTable)
    }
    which(fld)
  })
  if ( length(newSelection) == 0 ) newSelection <- as.integer(NA)
  if ( as.object ) {
    if ( select ) {
      self$select(newSelection)
      found <- self
    } else {
      found <- VESelection$new(self$resultsList, select=newSelection)
    }
  } else {
    if ( select ) {
      self$selection <- newSelection
      found <- self$selection
    } else {
      found <- newSelection
    }
  }
  return( if ( select ) invisible(found) else found )
}

# Add another selection to self (add + assign)
# Matching indices will be included
ve.select.add <- function(select) {
  select <- self$parse(select)
  self$selection <- union(self$selection,select)
  invisible(self)
}

# Remove contents of another selection from self (remote + assign)
# Matching indices in select will be removed
ve.select.remove <- function(select) {
  select <- self$parse(select)
  self$selection <- setdiff(self$selection,select)
  invisible(self)
}

# Keep only fields that are in both self and select (logical "and")
# Indices in both will be kept, and those present in only one will be removed
ve.select.and <- function(select) {
  select <- self$parse(select)
  self$selection <- intersect(self$selection,select)
  invisible(self)
}

# 
ve.select.all <- function() {
  self$selection <- 1:nrow(self$resultsList$resultsIndex)
  invisible(self)
}

ve.select.none <- function() {
  self$selection <- integer(NA)
  invisible(self)
}

#' Conversion method to turn a VESelection into a vector of selection indices
#'
#' Mostly used internally.
#'
#' @param x a VESelection object (or something that can be coerced to one)
#' @param ... Additional arguments to support generic signature
#' @return an integer vector of selected fields
#' @export
as.integer.VESelection <- function(x,...) x$selection

# The VESelection R6 class
# This interoperates with VEResultsList to keep track of what subsets of results data

#' @export
VESelection <- R6::R6Class(
  "VESelection",
  public = list(
    # public data
    selection = integer(0),
    resultsList = NULL,  # a VEResultsList object

    # methods
    initialize=ve.select.init, # Initial selection for a results list
    copy=ve.select.copy,       # Create a new selection object with the same results and indices
    print=ve.select.print,     # Print list of fields or (details=TRUE) the subset of results$modelIndex
    show=ve.select.show,       # retrieve the selected subset of resultsList$resultsIndex (data.frame)
    valid=ve.select.valid,     # is the selection valid against resultsList$resultsIndex
    find=ve.select.find,       # general search facility for selecting group/table/name
    parse=ve.select.parse,     # interpret different ways of specifying a selection (number, field descriptor)
    select=ve.select.select,   # assign - set self to other selection value
    add=ve.select.add,         # "union" - indices are included from either selection
    addkeys=ve.select.addkeys, # add keys (e.g. HhID, BZone) for already SELECTED Tables (uses "or")
    remove=ve.select.remove,   # "setdiff" - keep indices that are not in the other selection
    and=ve.select.and,         # "intersection" - keep indices in both selections
    or=ve.select.add,          # alias for "add" (just expressed as a logical operation)
    all=ve.select.all,         # select all indices (resets the selection)
    none=ve.select.none,       # select no indices (empty selection) - usually as the basis for adding in certain ones

    # Field lists (read-only)
    scenarios=ve.select.scenarios,
    stages=ve.select.scenarios,
    groups=ve.select.groups,
    tables=ve.select.tables,
    fields=ve.select.fields
  )
)

#' Open VisionEval results from a directory
#'
#' @description
#' `openResults` opens a directory containing VisionEval model run results and
#'    returns a VEObject instance that can be used to extract the results or
#'    to perform queries. Limited probing occurs to attempt to identify the model
#'    that might contain these results.
#'
#' @details
#' See the VEModel walkthrough and tests, as well as the online VisionEval documentation for
#'   available help and reference materials. The basic use of `openModel` is also described in the
#'   VisionEval Getting-Started document on the VisionEval website (also in the VisionEval
#'   installer). `openResults` is a shortcut function for opening a model and its results from
#'   within the "results" directory. Usually, it's simpler to open the model and call its $results
#'   function; This function cuts out one step and leaps straight to the results.
#'
#' The path provided as a parameter is either a model name (in "models" folder), an absolute path to
#'   a model, or a path to a directory containing ModelState.Rda (in which case the model that
#'   created those results will be sought by working up the directory tree. If no path is provided,
#'   The model search will start in the current directory
#'
#' @param path A relative or absolute path to a directory (default is the working directory)
#'   that will be used to find a VisionEval model and open its results.
#' @return A VEResultsList object giving access to the VisionEval results for the model identified
#'   by `path`
#' @export
openResults <- function(path=NULL) {
  # We're going to look for a model and then open its results
  # normalizePath will handle passing a model name (which will then be sought
  #   in the "models" subdirectory of the VE_RUNTIME).
  if ( missing(path) || is.null(path) ) path <- getwd()
  path <- normalizePath(path,RootDir=getSetup("ModelRoot"),mustWork=TRUE)
  # RootDir is ignored if path is already an absolute path (e.g. from getwd())
  if ( nzchar(dir(path,pattern="(rda|rdata)$",ignore.case=TRUE)[1]) ) {
    # Look for model in parent of results
    searchPath <- dirname(path)
  } else {
    searchPath <- path # Presume we're looking at a model or VE_RUNTIME
  }
  mod <- openModel(searchPath)
  if ( ! mod$valid() ) {
    # If model not found in partent
    searchPath <- dirname(searchPath)
    mod <- openModel(searchPath)
  }
  return(VEResultsList$new(model=mod)) # will return results from mod, or invalid message
}
