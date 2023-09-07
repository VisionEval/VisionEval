# export.R#' @include environment.R
NULL

# Documentation for VEExporter
#' VEExporter class for exporting tabular data from model or query results
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEEXporter
NULL

# Documentation for VEPartition
#' VEPartition class for partitioning tabular data from model or query results
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEPartition
NULL

# Documentation for VEConnection
#' VEConnection class for connecting to an external table database
#'
#' Documentation yet to come for various functions. Inherited classes
#' manage connection defaults (base folders, database name, user/password,
#' timestamp) plus low-level Create/Append/Read operations.
#'
#' @aliases VEConnection.Dataframe VEConnection.CSV VEConnection.DBI
#' @name  VEConnection
NULL

super=self=private=NULL # To avoid global variable warnings for R6 elements

#######################
# Implement VEPartition
#######################

# VEPartition takes a partition description (named character vector of partition actions)
# Basic format is c(Field="action",Field2="action",...)
# Actions are the following:
#   "none" or "merge"  : ignore the fields - just there as documentation...
#   "path" or "folder" : partition data on this element and add its value to the list of "paths"
#   "name"             : partition data on this element and add its value to the list of "names
# Constructor copies the vector (or installs a default), dropping the none/merge elements
#
# Other main function is $partition with this interface:
# $partition(theData,Table)
#    theData is a table with 0 or more of the partition fields in it
#    Table is the base name for the table
#
# Partition returns a list of TableLoc structures that define one or more partitions for the data
#    Range : a numeric vector of rows in this partition
#    Partition : a structure describing the Table:
#       Path : a character vector of path elements describing the Table in the connection (prepended to Table name
#              perhaps as folders)
#       Name : a character vector of path elements describing the Table in the connection (prepended to Table name)
#       Table : root table name

ve.partition.init <- function(partition=character(0)) {
  # "partition" is a character vector of partitioning vectors for possible fields
  # The default partitioning scheme is this:
  if ( missing(partition) || length(partition)==0 || ! is.character(partition) ) {
    self$Partition <- c(Global="name") # Default is to partition with Global in the name
  } else {
    # remove "none" or "merge" elements and keep the rest
    self$Partition <- partition[ grep("(merge)|(none)",partition,invert=TRUE) ]
    # collapse alternate names to standard names
    self$Partition <- gsub("path","folder",self$Partition)
    # get rid of (and warn about) any partition actions that are not defined
    badPartitions <- ! self$Partition %in% c("name","folder") # eventually allow "database"
    if ( any(badPartitions) ) {
      writeLog(paste("Ignoring bad partitions:",paste(self$Partitions[badPartitions],collapse=",")),Level="warn")
      self$Partition <- self$Partition[ ! badPartitions ]
    }
    if ( ! "Global" %in% names(self$Partition) ) self$Partition <- c( c(Global="path"),self$Partition )
  }
}

ve.partition.partition <- function(theData,Table) {
  # theData is a data.frame possible with partitionable columns
  # Table is the root name of the table for its locator
  # returns a list of table locators (lists) for each of the partitions
  if ( missing(theData) || missing(Table) || is.null(theData) || is.null(Table) ) {
    traceback(1)
    stop("Invalid parameters for VEPartition$partition",call.=FALSE)
  }

  # reduce self$Partition to fields present in theData (can't partition what is not there)
  use.partition <- self$Partition[ which( names(self$Partition) %in% names(theData) ) ]
  # reduce partition to ignore any field which only contains NA values
  naFields <- sapply(theData[,names(use.partition)],function(d) all(is.na(d)) )
  if ( any(naFields) ) use.partition <- use.partition[ ! naFields ]

  # now do the same with fields having some NA values, but warn about it
  invalidFields <- sapply(theData[,names(use.partition)],function(d) any(is.na(d)))
  if ( any(invalidFields) ) {
    warning("Export Data to be partitioned has fields with some but not all NA values.")
    warning(paste(names(use.partition)[invalidFields],collapse=",")," will not be partitioned.")
    use.partition <- use.partition[ ! invalidFields ]
  }
  # Abort partitioning if there are no fields to partition
  if ( length(use.partition) == 0 ) return ( self$location(theData,Table) )

  # identify unique values in each of the partition columns 
  partition.values <- lapply( names(use.partition), function(p) unique(theData[[p]]) )
  names(partition.values) <- names(use.partition)
  locations <- expand.grid(partition.values,stringsAsFactors=FALSE)
  # yields a data.frame where columns are the partition fields and rows are the values

  # Build the partitions based on the locations...
  TableLocs <- list()
  locNames <- names(locations)
  pathNames <- names(use.partition)[use.partition=="folder"]
  nameNames <- names(use.partition)[use.partition=="name"]

  for ( loc in 1:nrow(locations) ) {
    thisLocation <- locations[loc,,drop=FALSE] # drop=FALSE in case only one partition field
    Paths <- thisLocation[pathNames]
    if ( length(Paths) > 0 ) { # at least one value in the Paths partition
      names(Paths)<-pathNames;
      Paths[is.na(Paths)]<-"NA" # Turn NA into a valid value; shouldn't have any of these
      Paths <- as.character(Paths)
      names(Paths) <- pathNames
    } else Paths <- character(0)
    Names <- thisLocation[nameNames]
    if ( length(Names) > 0 ) { # at least one value in the Names partition
      names(Names)<-nameNames;
      Names[is.na(Names)] <- "NA" # Turn NA into a valid value; shouldn't have any of these
      Names <- as.character(Names)
      names(Names) <- nameNames
    } else Names <- character(0)
    # Arrive here with Names and Paths both named character vectors, possibly empty

    locFields <- locations[loc,,drop=FALSE]
    locSelection <- rep(TRUE,nrow(theData))
    for ( n in locNames ) {
      nValue <- locFields[[n]]
      locSelection <- locSelection & (theData[[n]] == nValue) # knock off rows not matching this value
    }
    tableLoc <- VETableLocator$new(Paths=Paths,Names=Names,Table=Table,Range=which(locSelection))
    TableLocs[[tableLoc$tableString()]] <- tableLoc
  }
  return(TableLocs)
}

ve.partition.locate <- function(theData,Table) {
  # Creates a TableLoc for an unpartitioned Table
  # Used in VEExporter$write for arbitrary data, including query results and metadata
  tableLoc <- VETableLocator$new(Table=Table,Range=1:nrow(theData))
  locList <- list()
  locList[[ tableLoc$tableString() ]] <- tableLoc
  return( locList)
}

ve.partition.print <- function(...) {
  if ( length(self$Partition) == 0 || ! nzchar(self$Partition[1]) ) {
    cat("No partitioning")
  } else {
    for ( p in 1:length(self$Partition) ) {
      cat(names(self$Partition)[p],":",self$Partition[p],"\n")
    }
  }
}

VEPartition <- R6::R6Class(
  "VEPartition",
  public = list(
    # public data
    Partition = character(0),           # Describes fields to be partitioned into different output tables

    # methods
    initialize=ve.partition.init,     # initialize internal partition
    print=ve.partition.print,         # print the partition
    partition=ve.partition.partition, # partition a data.frame into output tables
    location=ve.partition.locate      # generate a TableLocator from data + Table name only (no partitioning)
  )
)

##########################
# Implement VETableLocator
##########################

# VETableLocator is a structure returned from VEPartition$partition consisting of a Range
# and a Partition (the latter listing out path, name and table elements for building a
# table location in the export destination)

# Eventually work this into R6 documentation format...
# @descriptiom VETableLocator$tableString formats a VETableLocator into a table name string
#
# @details
# VETableLocator$tableString is used in the connection types to convert a TableLoc to whatever is
#   needed to make a table name on the connection. Default format is generic representation used by
#   VEPartition to name the TableLocs it generates
#
# e.g. CSV:
#   loc <- VETableLocator$new( Paths='MyScenario', Names='2019, Table='Household' )
#   csv.table.file <- loc$tableString( tableSep="/")
#   #' MyScenario/2019_Household
#
# e.g. SQL/DBI:
#   sql.table <- loc$tableString( pathSep="_", tableSep="_" )
#   #' MyScenario_2019_Household
#
# e.g. data.frame:
#   df.name <- loc$tableString() # default format
#   2019_Household - Paths (MyScenario)  become nested lists of data.frames
#
# @param pathSep a character string used to separate path elements when building the table name
# @param nameSep a character string used to separate name elements when building the table name
# @param tableSep a character string used to separate path elements from table plus name elements;
#
ve.locator.string <- function(
  pathSep="/", nameSep="_", tableSep=":"
) {
  tableString <- if ( length(self$Names) > 0 ) {
    paste(self$Table,paste(self$Names,collapse=nameSep),sep=nameSep)
  } else self$Table
  if ( length(self$Paths) > 0 ) {
    tableString <- paste(paste(self$Paths,collapse=pathSep),tableString,sep=tableSep)
  }
  return(tableString)
}

# Printing a VETableLocator happens in various places for debugging purposes...
#' @importFrom utils str
ve.locator.print <- function(...) {
  cat("Range: "    ,str(self$range),"\n",sep="")
  cat("Partition: ",self$tableString(),"\n",sep="")
}

# Initialize a VETableLocator
ve.locator.init <- function(Table,Range=integer(0),Paths=character(0),Names=character(0)) {
  self$Paths = Paths
  self$Names = Names
  self$Table = Table
  self$Range = Range
}

ve.locator.append <- function(Name) self$Names <- append(self$Names,Name)

# VETableLocator class definition

VETableLocator <- R6::R6Class(
  "VETableLocator",
  public = list(
    # public data
    Range = integer(0),
    Paths = character(0),
    Names = character(0),
    Table = NULL,

    # methods
    initialize=ve.locator.init,    # Create the table location
    print=ve.locator.print,        # print the location for debugging
    tableString=ve.locator.string, # partition a data.frame into output tables
    append=ve.locator.append       # add a name (e.g. Table Suffix) to the list of location names
  )
)

######################
# Implement VEExporter
######################

# Interface:
#    initialize  ## Individual Exporter classes will receive connection, partition, and
#                ## optional tag and optional VEModel object
#                ## connection is a string or a named list, depending on the requirements
#                ##  of the specific exporter
#                ## partition vector with name of variable and a strategy from among
#                ##  c("none","folder","name")
#                ##  exporter may not support "folder" (e.g. DBI/SQL in which case we
#                ##  should warn and fall back to "name")
#                ## tag parameter is a character string that will be used to look up
#                ##  default connection and partition data in the "Exporters" configuration block
#                ##  in visioneval.cnf
#                ## Model parameter can add default places to search for connection
#                ##  parameters, which will be looked up by the connection tags
#                ## Can load from a file containing connection descriptor, partitions and table locations
#    config      ## Report the partition string and connecton configuration list used to build connection
#    load        ## Read an .VEexport (.Rdata) file with the Exporter elements
#                ##   (called from $initialize for re-opening connection to additional items)
#    save        ## Write a .VEexport (.Rdata) file with the Exporter elements
#    close       ## Close the exporter connection (important for DBI)
#    connection  ## Change/set the exporter connection (can't call if anything has been written)
#    partition   ## Change the exporter partitioning strategy (can't call if anything has
#                ##   already been written) e.g. c(Group=name,Scenario=name,Table=name)
#                ##   CSV Default is folder for Scenario, name for Table, and merge for Group/Scenario
#                ##   SQL Default is merge for Scenario, name for Table, and merge for Group
#                ## Doing folder for SQL might one day allow us to write to a different DBI
#                ##   connection
#    write       ## a data.frame plus partition flags (perhaps passed as ...)
#                ## default for unsupplied parameters is to ignore them (treating them as 'merge"
#                ## and not checking if they are really there)
#    metadata    ## a function to turn self$TableList into a writable data.frame
#    writeMetadata ## write self$metadata() into a table on self$Connection
#    list        ## log of tables that have been built during export
#    print       ## just cat the names from the list
#    data        ## convert table locators (parameters, default=all) to flat list of data.frames
#                ## can also produce other types (e.g. tbl for dbplyr from DBI, data.tables, tibbles)

ve.exporter.init <- function(Model,load=NULL,tag="default",connection=NULL,partition=NULL) {
  # We may be able to live without subclassed Exporters
  # connection is a VEConnection object; if NULL, create a default one
  # partition is a VEPartition object; if NULL, use the default one for tag
  #   To force no partition, pass c() or character(0) - Table name will just get written/appended
  #   as is
  self$Model <- Model
  if ( is.character(load) ) {
    # Presumes load is an existing file
    # Usually we'll get here via VEModel$exporter
    if ( ! file.exists(load) ) stop("Could not find saved exporter to load: ",load,call.=FALSE)
    self$load(load)
    return()
  }
  if ( inherits(tag,"VEExporter") ) {
    self$Configuration <- tag$config()
  } else  if ( ! is.character(tag) ) {
    # No tag or unknown type; let's hope connection and partition have what we need...
    writeLog(Level="warn","Missing or Unknown 'tag' parameter for exporter; using 'default'")
    tag = "default"
  }
  if ( is.character(tag) ) { # didn't already get a fully formed exporter to copy

    # Get Exporters configuration from Model
    # Will have included Global config if present as part of loading the Model
    modelConfig <- Model$setting("Exporters") # will have inherited from global

    # Set the user choice for default exporter (often "sqlite" instead of "csv")
    if ( tag == "default" ) {
      tag <- if ( "Default" %in% names(modelConfig) ) {
        modelConfig$Default
      } else "csv"
    } # Default default is 'csv' if Exporters overridden but no Default tag
    modelConfig$Default <- NULL # Don't need it any more here

    # Find any specification for that configuration
    modelConfig <- if ( tag %in% names(modelConfig) ) modelConfig[[tag]] else list()

    # Default configurations
    defaultConfigs <- defaultExporters()
    defaultConfiguration <- if ( tag %in% names(defaultConfigs) ) {
      defaultConfigs[[tag]]
    } else {
      list(
        Partition = character(0),
        Connection = list()
      )
    }

    # Stash the partition for this exporter
    self$Configuration$Partition <- if ( is.list(partition) || is.character(partition) ) {
      pnames <- names(partition)
      partition <- as.character(partition) # it may be a list if it came from visioneval.cnf
      names(partition) <- pnames
      partition
    } else if ( is.character(modelConfig$Partition) ) {
      modelConfig$Partition
    } else defaultConfiguration$Partition

    # Set up connection (built up the other way from partition since we'll
    #   start with default elements and override them as they are reconfigured)
    adjustConnection <- defaultConfiguration$Connection
    if ( is.list(modelConfig$Connection) && length(modelConfig$Connection)>0 ) {
      adjustConnection[ names(modelConfig$Connection) ] <- modelConfig$Connection
    }
    if ( is.list(connection) ) { # merge parameters over defaults
      adjustConnection[ names(connection) ] <- connection
    }

    # Pull TablePrefix and TableSuffix from connection configuration, if present
    # They will be consumed here by the VEExporter and not passed to VEConnection
    if ( "TablePrefix" %in% names(adjustConnection) ) {
      self$TablePrefix <- adjustConnection$TablePrefix
      adjustConnection$TablePrefix <- NULL # remove it
      self$TablePrefix <- as.character(self$TablePrefix)[[1]] # make sure it is characters!
    } else self$TablePrefix <- character(0)

    if ( "TableSuffix" %in% names(adjustConnection) ) {
      self$TableSuffix <- adjustConnection$TableSuffix
      adjustConnection$TableSuffix <- NULL
      self$TableSuffix <- as.character(self$TableSuffix)[[1]] # make sure it is characters!
    } else self$TableSuffix <- NULL

    if ( "Timestamp" %in% names(adjustConnection) && adjustConnection$Timestamp %in% c("prefix","suffix") ) {
      adjustConnection$startTime <- format(Sys.time(),"%Y%m%d%H%M") # 202308240937
      # Note TimeSeparator is only relevant for TablePrefix or Database Timestamp
      # TableSuffix will use the Names separator for the table location later on
      if ( ! "TimeSeparator" %in% names(adjustConnection) ) adjustConnection$TimeSeparator <- "_"
      if ( adjustConnection$Timestamp == "prefix" ) {
        self$TablePrefix <- paste0(adjustConnection$startTime,adjustConnection$TimeSeparator,self$TablePrefix)
      } else if ( adjustConnection$Timestamp == "suffix" ) {
        if ( isTRUE(nzchar(self$TableSuffix)) ) {
          self$TableSuffix <- paste(self$TableSuffix,adjustConnection$startTime,sep=adjustConnection$TimeSeparator)
        } else {
          self$TableSuffix <- adjustConnection$startTime
        }
      } # else adjustConnection$Timestamp might be "database"
    }
    
    # Stash the connection for this exporter
    self$Configuration$Connection <- adjustConnection
  }

  # either of the following may stop if the Configuration is inadequate
  self$Connection <- makeVEConnection(Model,self$Configuration$Connection) # Returns a VEConnection subclass
  self$Partition <- VEPartition$new(self$Configuration$Partition)
}

# subclasses will do more with Connection and Partition prior to saving them
ve.exporter.write <- function(Data, Table, Scenario=NULL, Group=NULL, Metadata=NULL, overwrite=FALSE ) {
  # Table must be provided as the root name for the Table being written. Table is used to
  #   select the recipient(s) for the partitioned data. The actual Tables written will be
  #   composed of the results of partitioning data into path elements, name elements, the
  #   Table name and built by the connection object
  Table <- paste0(self$TablePrefix,Table) # We'll attach TableSuffix later using the Names location element

  locations <- if ( ! ( is.null(Scenario) && is.null(Group) ) ) {
    # no partitioning unless either Scenario/Group provided
    self$Partition$partition(Data,Table)
  } else {
    self$Partition$location(Data,Table) # creates a TableLocation with the entire range
  }

  # Hack to put the TableSuffix in the right place: make it the final "names" element in the loc
  if ( is.character(self$TableSuffix) ) {
    locations <- lapply( locations, function(loc) {
      # Update the table location to include the TableSuffix as the last Names element.
      loc$append(self$TableSuffix)
      return(loc)
    } )
  }

  # process each location into low-level calls to Connection$writeTable
  for ( loc in locations ) {
    TableName <- self$Connection$nameTable(loc)
    TableFields <- self$Connection$writeTable(Data[loc$Range,],TableName)
    # Save metadata if provided (using connection-specific name format)
    if ( ! is.null(Metadata) ) {
      selector <- Metadata$Name %in% TableFields
      if ( ! is.null(Scenario) ) selector <- selector & Metadata$Scenario == Scenario
      if ( ! is.null(Group) ) selector <- selector & Metadata$Group == Group
      tableMetadata <- Metadata[selector,]
      self$TableList[[TableName]] <- cbind(tableMetadata,DBTable=TableName)
    } else {
      if ( is.null(Scenario) ) Scenario <- NA
      if ( is.null(Group) )    Group    <- NA
      self$TableList[[TableName]] <- data.frame(Name=TableFields,Scenario=Scenario,Group=Group,DBTable=TableName)
    }
  }
}

ve.exporter.list <- function(names=NULL, namesOnly=TRUE) {
  # namesOnly is intended for interactive use to list out the tables in the export (as written)
  # if this function is used internally, set namesOnly to False, or if its really deeply
  #   internal, just access self$TableList (the "metadata")
  if ( is.null(self$TableList) ) { # Nothing has been exported yet
    return("Nothing exported yet")
  }
  allNames <- ( is.null(names) || !is.character(names) || length(names)==0 || ! all(nzchar(names) ) )
  if ( allNames ) {
    names <- names(self$TableList)
    if ( isTRUE(namesOnly) ) return(names)
  }
  # Return a subset of the TableList
  return (
    if ( isTRUE(namesOnly) ) {
      names[names %in% names(self$TableList)] # vector of table locations string
    } else {
      self$TableList[names] # a list of field name vectors.
    }
  )
}

ve.exporter.print <- function(names=NULL,...) {
  cat("Exporter for Model: ",self$Model$modelName,"\n")
  cat("Connection:\n")
  print(self$Connection)
  cat("Partition:\n")
  print(self$Partition)
  tables <- self$list(names=names,...) # could do namesOnly=FALSE in ...
  if ( ! is.null(tables) ) {
    cat("Exported Tables:\n")
    print(tables)
  } else cat("No Tables Exported Yet.\n")
}

#' @import data.table
#' @importFrom utils str
ve.exporter.formatter <- function(Data,format="data.frame",...) {
  if ( ! is.data.frame(Data) && ! is.list(Data) ) {
    message("Data must be data.frame or list of data.frames: ",class(Data))
    stop("Invalid Data output")
  }
  if ( is.list(Data) && ! is.data.frame(Data) ) {
    isList <- all(sapply(Data,function(d)inherits(d,"data.frame")))
    if ( ! isList ) stop("Data must be a data.frame or list of data.frames:",str(Data))
  } else isList <- FALSE
   
  if ( is.function(format) ) {
    return( format(Data,...) ) # Passing either List or data.frame depending on Data
  }
  else if ( ! is.character(format) ) {
    message("Unrecognized data format. Choose default or one of 'data.frame' or 'data.table'")
    message("format may also be a function object that takes a data.frame (or list thereof) as its first parameter")
    message("... here will be passed to that function")
    format <- "data.frame"
  } else if ( isList ) {
    # List has to be passed to a format function, not processed here
    stop("List of data.frames must be passed to a format function, not built-in format")
  }
  if ( format == "data.frame" ) {
    return( as.data.frame(Data) ) # probably already is...
  } else if ( format == "data.table" ) {
    return( data.table::as.data.table(Data) )
  } else {
    message("Returning Data unmodified...")
    return(Data)
  }
}

#' @import stringr
ve.exporter.data <- function(tables=NULL,format="data.frame",formatList=FALSE,...) { # tables: a list of table strings
  # Generate a list of requested table data in the requested format (which will be
  # interpreted by self$Connection as it reads out the data).
  # 'tables' is a subset of table names (a character vector) or 
  # ... is passed to self$formatter, and then on to the formatting function
  # formatList if TRUE will pass the data.frame list to the formatter, and if FALSE will pass each
  # data.frame individually.
  soughtTables <- tables
  if ( ! is.null(tables) ) {
    if ( ! is.character(tables) ) tables <- names(tables)
    if ( ! is.character(tables) || length(tables)<1 ) stop("No tables found matching ",soughtTables)
  }
  tables <- self$list(tables,namesOnly=TRUE) # if is.null(tables), generate all of them
  if ( missing(format) || (is.character(format) && format=="data.frame") || formatList ) {
    exportedData <- lapply( tables, function(t) self$Connection$readTable(t) )
    # NOTE: The gsub is a hack for write_xlsx which has limits on legal characters and won't take
    # care of them itself...
    names(exportedData) <- stringr::str_sub(gsub("[^-_[:alnum:]]","",tables),end=31)
    if ( formatList ) {
      if ( is.function(format) ) {
        # send entire list of data.frames to formatter at once
        exportedData <- self$formatter(Data=exportedData,format=format,...)
      } else {
        message("'formatList' requires 'format' to be a function to process a list of data.frames")
        message("For example, write_xl will write a list of data.frames")
        stop("Invalid output format")
      }
    }
  } else {
    # formatter will process individual data.frames and produce a list of results
    exportedData <- lapply( tables, function(t) self$formatter(self$Connection$readTable(t),format=format,...) )
  }
  if ( length(exportedData) == length(tables) ) names(exportedData) <- tables # write_xl returns workbook name
  return(exportedData)
}

ve.exporter.metadata <- function() {
  # Compile accumulated metadata plus DB TableName (one row per N/TableName), and include
  #  the Units actually written plus the N description. DBTableName is the locator encoded
  #  form (makeTableString with default parameters). Metadata is just intended to understand
  #  what is in TableName.
  # TODO: actually build the full metadata (S/G/T/N/...) while writing tables
  metadatalist <- lapply( names(self$TableList), function(t) {
    metadata <- self$TableList[[t]]
  } )
  # NOTE: the following will will fail if not all metadata tables have the same columns.
  # In practice, shouldn't be a problem: Un-partitioned tables do not end up in the metadatalist,
  # and they are best all written from the same VEResultsList.
  metadatalist <- unique(do.call(rbind,metadatalist))
  return( metadatalist  )
}

ve.exporter.writeMetadata <- function(Table="Metadata") {
  self$write( self$metadata(), Table=Table) # No partitioning
}

# Save Connection, Partition, TableList, and Metadata
ve.exporter.load <- function(filename) { # .VEexport file (.Rdata format)
  # Filename should already be normalized to the model's output location or wherever
  # This function will usually be called via VEModel$exporter(file=...), which will build a good path
  otherExporter <- new.env()
  # force .VEexport extension
  if ( ! any(grepl("\\.VEexport$",filename)) ) filename <- paste0(filename,".VEexport")
  if ( ! file.exists(filename) ) {
    checkFilename <- file.path(self$Model$exportPath(),filename)
    if ( ! file.exists(checkFilename) ) stop("Could not find file: ",filename,call.=FALSE)
    filename <- checkFilename
  }
  load(filename,envir=otherExporter)

  self$TableList     <- otherExporter$TableList
  self$Configuration <- otherExporter$Configuration

  # TODO: the Configuration must include the constructed SQLite Database (and this may be
  # a problem for CSV as well): if there is a Timestamp in the target of the connection.
  # We need the VEConnection to have an interface function for saving and loading (that
  # digs deeper into what was actually configured internally). Use that for CSV and DBI.

  if ( ! is.null(self$Connection) ) self$Connection <- NULL # queue it for garbage collection

  self$Connection <- makeVEConnection(self$Model,self$Configuration$Connection,reopen=TRUE)
  self$Partition  <- VEPartition$new(self$Configuration$Partition)
}

ve.exporter.save <- function(filename) { # .VEexport file (.Rdata)
  filename <- file.path(self$Model$exportPath(),basename(filename))
  if ( ! any(grepl("\\.VEexport$",filename)) ) filename <- paste0(filename,".VEexport")
  Configuration <- self$Configuration # turn these into saveable objects
  Configuration$Connection[["ReopenData"]] <- self$Connection$save() # may be nothing
  TableList <- self$TableList
  save(Configuration,TableList,file=filename)
  message("Saved Exporter to ",filename)
}

VEExporter <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEExporter",
  public = list(
    # public data
    Model      = NULL,      # The Model associated with this exporter (for load/save path, connection building)
    Partition  = NULL,      # default is c(Scenario="merge",Group="merge",Table="name") # probably never want to change Table partition
    Connection = NULL,      # may be named list or character vector or string depending on derived class needs
    Configuration = list(), # Parameters to save for this connection (to rebuild it)
    TableList  = list(),    # Names in this list are table IDs of created tables, elements of the list are lists of the
    # fields that exist in the tables (names). Need to maintain fields so we can
    # make existing table and new written data be conformant during $write (typically will be)
    Metadata   = NULL,      # data.frame of S/G/T/N/Metadata passed into $write (TableList prior to partitioning)
    # Any N row in the Metadata for an S/G/T is a candidate for partitioning

    # methods
    initialize=ve.exporter.init,       # call from subclasses to establish internal connection and partition
    load=ve.exporter.load,             # called from init if load=savedExporter is provided (a file.path)
    save=ve.exporter.save,             # called e.g. by VEResultsList$export when the export is complete
    close=function() self$Connection$close(),

    write=ve.exporter.write,           # write rows onto a table (creating if necessary)
    list=ve.exporter.list,             # list tables that have been created within this exporter
    print=ve.exporter.print,           # print list of table identifiers
    data=ve.exporter.data,             # return list of data.frames corresponding to list of table names (all or some of self$list)
    formatter=ve.exporter.formatter,   # If format is given, use VEExporter$formatter to convert
    metadata=ve.exporter.metadata,     # Assemble the table of metadata for display or writing
    writeMetadata=ve.exporter.writeMetadata, # Write self$Metadata

    # Use Prefix/Suffix to identify different exports
    TablePrefix = NULL,                # if set, send to Table during $writeTable or $readTable
    TableSuffix = NULL                 # if set, send to connection$writeTable or readTable
  ),
  private = list(
    saveFileName = NULL                # File name if exporter has been loaded/saved
    # from $initialize(load=filename) - need not exist
    # Can construct a default file name when initializing, for later use
  )
)

#############################
#
###### Implement VEConnection
#
#############################

# The VEConnection is initialized from a "tag" and named list of connection parameters that are
# interpreted as needed by each driver. Default settings for all of them will yield a valid
# exporter for:
#    data.frames; default is hierarchical names lists of data.frames
#    csv; default is csv in subdirectory of OutputDir named after model
#    dbi; default is SQLite into a file in OutputDir named after model

# ve.connection.missing
ve.connection.missing <- function(dataFields,Table) {
  tableFields <- private$tableFields[[Table]]
  missingTableNames <- ! dataFields %in% tableFields # add these fields as NA to existing Table
  missingDataNames  <- ! tableFields %in% dataFields # add these fields as NA to incoming data source
  missing <- list(Data=character(0),Table=character(0))
  if ( any(missingDataNames) ) {
    missing$Data <- tableFields[missingDataNames]
  }
  if ( any(missingTableNames) ) {
    message("Dataset for '",Table,"' has fields not present in Table:")
    print(dataFields[missingTableNames])
    message("Will push NA into existing table rows in those columns")
    missing$Table <- dataFields[missingTableNames]
  }
  return(missing)
}

# ve.connection.init        <- function(config) {} # initialize the connection from parameters
ve.connection.init <- function(Model,config,reopen=FALSE) {
  # Add Timestamp if it is going to be part of the database name (or the CSV/Parquet folder)
  if ( ! reopen ) {
    if ( "Timestamp" %in% names(config) && isTRUE(config[["Timestamp"]]=="database") ) {
      self$Timestamp <- config[["startTime"]]
      if ( is.null(self$Timestamp) ) self$Timestamp <- format(Sys.time(),"%Y%m%d%H%M") # 202308240937
      self$TimeSeparator <- if ( "TimeSeparator" %in% names(config) ) config[["TimeSeparator"]] else "_"
    } else {
      self$Timestamp <- NULL
      self$TimeSeparator <- NULL
    }
    # derived classes may then use Timestamp and TimeSeparator to create the Database/Folder name
  }
}

# Generic implementation uses derived class functions to do the work
ve.connection.writeTable <- function(Data, Table) {
  # Generic call that uses derived class functions:
  #   tableExists
  #   createTable
  #   appendTable - appends data to table using driver-specific operations
  #   readTable
  # Append Data onto Table
  # Check field names and rewrite table if necessary (Table can be a VETableLocator)
  if ( ! is.character(Table) ) Table <- self$nameTable(Table) # Should result from nameTable call outside
  # In CSV, nameTable will provide the path of the Table as "path1/path2/table_name1_name2
  #   which gets attached to the Model output directory
  if ( ! self$tableExists(Table) ) {
    return( self$createTable(Data,Table) )
  }
  # Conform the columns (ideally, we never end up using this code...)
  missingFields <- self$missingFields(names(Data),Table)
  # How to conform them will depend on the underlying table storate mechanism
  if ( length(missingFields$Data) > 0 ) {
    naValue <- as.list(rep(NA,length(missingFields$Data)))
    names(naValue) <- missingFields$Data
    Data <- cbind(Data,data.frame(naValue))
  }
  if ( length(missingFields$Table) > 0 ) {
    # External data sources will need to read back the table, add fields, recreate (drop/add) table
    naValue <- as.list(rep(NA,length(missingFields$Table)))
    names(naValue) <- missingFields$Table
    oldTable <- self$readTable(Table)
    oldTable <- cbind(oldTable,data.frame(naValue))
    Data <- rbind(oldTable,Data)
    return( self$createTable(Data, Table) ) # Data columns changed; overwrites previous table
  }
  # Append the data and return the field names
  self$appendTable(Data,Table)
  return( self$saveTableFields( Data, Table ) )
}

VEConnection <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEConnection",
  public = list(
    # public data
    Timestamp     = NULL, # pull from config
    TimeSeparator = NULL, # defaults to "_" if Timestamp exists

    # methods (each connecton type will implement its own version of these)
    initialize  = ve.connection.init,           # call from subclasses to establish internal connection and partition
    summary     = function() { "Base VEConnection" }, # simplified text representation of the connection details
    # format and content depends on the connection class
    print       = function(...) cat(self$summary(),"\n"), # Whatever makes sense for the derived class
    raw         = function() {},                # returns CSV folder or DBI connection
    # specific value is class-specific and is enough to re-open and
    # review what is in the connection; e.g. to list actually written
    # tables using DBI::dbListTables(connection$raw())
    # Services provided in base class
    writeTable  = ve.connection.writeTable,     # Base class implements using functions below (dispatches to create/write,
    # after reconciling columns in Data and Table).
    missingFields   = ve.connection.missing,    # Internal helper to find differences between saved table and new data
    tableExists     = function(Table) {         # Used by writeTable to determine if creating or appending
      Table %in% names(private$tableFields)
    },
    saveTableFields = function(Data,Table) {    # Helper to stow updated table field names (in case they were reconciled)
      private$tableFields[[Table]] <- names(Data)
      return( private$tableFields[[Table]] )
    },
    list        = function(nameOnly=TRUE) {
      return( if (nameOnly) {
        names(private$tableFields)
      } else {
        private$tableFields
      } )      # Might eventually want to beef up in derived classes
    },

    # Functions we expect to override in derived classes
    nameTable   = function(TableLoc) TableLoc$tableString(),
    # Turn the TableLoc into an actual table name suitable for creating/writing/reading
    # Exporter will add this name to the TableLoc Metadata
    createTable = function(Data,Table) NULL,    # Create or re-create a Table from scratch (includes append)
    appendTable = function(Data,Table) NULL,    # perform low-level append data operation
    readTable   = function(Table) NULL,         # Read named table into a data.frame
    save        = function() return(list()),    # Return private data for saving/reopening connnection
    open        = function() NULL,              # Reopen the connection (optional for DBI)
    close       = function() NULL               # Close connection (needed for DBI)
    # 'Table' should be a TableLocator string built with nameTable for the Connection

  ),
  private = list(
    tableFields = list()
  )
)

# VEConnection implementations

#######################################
#
###### Implement VEConnection.Dataframe
#
#######################################

ve.connection.df.createTable <- function(Data,Table) {
  if ( ! is.character(Table) ) stop("createTable: Table must come from nameTable")
  private$exportedData[[Table]] <- Data
  return( self$saveTableFields(Data,Table) )
}

ve.connection.df.appendTable <- function(Data,Table) {
  # Columns should already have been reconciled...
  private$exportedData[[Table]] <- rbind(private$exportedData[[Table]],Data)
}

ve.connection.df.readTable <- function(Table) {
  return( private$exportedData[[Table]] )
}

VEConnection.Dataframe <- R6::R6Class(
  # Accumulates data.frames in memory
  # Connection may create a different storage class (e.g. data table / tibble / data.frame or even arrow)
  #   Later, when $data is called, it will produce the desired type automatically
  "VEConnection.Dataframe",
  inherit = VEConnection,
  public = list(
    # methods
    initialize  = function(Model,config,reopen) { super$initialize(Model,config,reopen) },  # No initialization needed locally
    summary     = function() {
      paste( c("Tables:",
        if ( length(private$exportedData)>0 ) names(private$exportedData) else "None written yet"
      ), collapse="\n")
    },
    # print = {} # base class implementation
    raw         = function() { invisible(private$exportedData) },
    # implementing methods
    nameTable   = function(TableLoc) TableLoc$tableString(),
    createTable = ve.connection.df.createTable,
    appendTable = ve.connection.df.appendTable,
    readTable   = ve.connection.df.readTable
  ),
  private = list(
    exportedData = list()
  )
)

#################################
#
###### Implement VEConnection.CSV
#
#################################

#' @import data.table
ve.connection.csv.init      <- function(Model,config,reopen=FALSE) {
  # CSV provides a default name for Directory
  super$initialize(Model,config)
  if ( ! reopen ) {
    if ( ! "Directory" %in% names(config) ) {
      if ( "Database" %in% names(config) ) {
        config[["Directory"]] <- config[["Database"]]
      } else {
        config[["Directory"]] <- "CSV"
      }
    }
    rootDirectory <- file.path(Model$modelPath,Model$setting("ResultsDir"),Model$setting("OutputDir"))
    if ( ! dir.exists( rootDirectory ) ) {
      dir.create(rootDirectory,showWarnings=FALSE) # don't yet have OutputDir
      if ( ! dir.exists( rootDirectory ) ) { # don't have ResultsDir either, which is pathological
        stop("Could not create output directory: ",rootDirectory)
      }
    }
    if ( "Directory" %in% names(config) && is.character(config[["Directory"]]) ) {
      private$Directory <- file.path(rootDirectory,config$Directory)
      defaultDirectory <- private$Directory == rootDirectory # Directory present but empty
      # Force a sub-directory; may want to change that behavior later...
    } else {
      defaultDirectory <- TRUE # default name
    }
    if ( defaultDirectory ) config$Directory <- "CSV"

    config$Directory <- paste(Model$modelName,config$Directory,sep="_")
    config$Directory <- paste0(config$Directory,self$TimeSeparator,self$Timestamp) # Add Timestamp if present
    private$Directory <- normalizePath(file.path(rootDirectory,config$Directory),"/",mustWork=FALSE)
    dir.create(private$Directory,showWarnings=FALSE) # don't do recursive
    if ( ! dir.exists(private$Directory) ) {
      stop("Could not create CSV directory: ",private$Directory)
    }
  } else {
    private$Directory <- config$ReopenData
  }
}

ve.connection.csv.raw       <- function() {
  # Connection specific description
  return( private$Directory )
}

ve.connection.csv.getWriteTable <- function(Table,create=TRUE) {
  tableDir <- dirname(Table) # will produce "." if no path elements
  if ( tableDir != "." ) {
    writeDir <- file.path(private$Directory,tableDir)
    if ( create && ! dir.exists(writeDir) ) dir.create(writeDir,recursive=TRUE)
  } else writeDir <- private$Directory
  return( file.path(writeDir,basename(Table)) )
}

ve.connection.csv.createTable <- function(Data, Table) {
  # Overwrite Table with Data
  # Build table path if necessary
  data.table::fwrite( Data, file=self$getWriteTable(Table), append=FALSE)
  return( self$saveTableFields(Data,Table) )
}

ve.connection.csv.appendTable <- function(Data,Table) {
  data.table::fwrite( Data, file=self$getWriteTable(Table), append=TRUE )
}

ve.connection.csv.readTable <- function(Table) {
  # won't create writeTable
  readTable <- self$getWriteTable(Table,create=FALSE)
  if ( ! file.exists(readTable) ) stop("Table does not exist for Read: ",readTable)
  return ( data.table::fread(file=readTable,data.table=FALSE) )
}

VEConnection.CSV <- R6::R6Class(
  # CSV implementation writes out CSV files using the Partition strategy
  # Depending on how much overlap, could also use this to implement parquet format
  "VEConnection.CSV",
  inherit = VEConnection,
  public = list(
    # methods
    initialize  = ve.connection.csv.init,
    summary     = function() {
      paste( c(paste("CSV Directory:",private$Directory),
        dir(private$Directory)
      ), collapse="\n")
    },
    # implementing methods
    nameTable = function(TableLoc) paste0(TableLoc$tableString(tableSep="/"),".csv"),
    getWriteTable = ve.connection.csv.getWriteTable,
    createTable   = ve.connection.csv.createTable,
    appendTable   = ve.connection.csv.appendTable,
    readTable     = ve.connection.csv.readTable,

    # methods
    save          = function() return(private$Directory)
  ),
  private = list(
    Directory = NULL    # Default to "OutputDir/Export_CSV<TimeSeparator><Timestamp>" in initializer
  )
)

#################################
#
###### Implement VEConnection.DBI
#
#################################

#' @import DBI
#' @import RSQLite
#' @importFrom methods new
ve.connection.dbi.init <- function(Model,config,reopen=FALSE) {
  super$initialize(Model,config)
  # Two avenues here:
  # If we're missing a full DBI configuration, presume SQLite
  # If there is a full DBI configuration, fill in blanks like dbname from outside Database
  # NOTE: the following evades build-time checks on available packages
  if ( ! reopen ) {
    if ( "package" %in% names(config) ) {
      package <- config[["package"]]
      loadError <- try( library(package,character.only=TRUE) )
      if ( inherits(loadError,"try-error") ) {
        stop("Package requested for DBI connection is not installed: ",config[["package"]])
      }
    }
    if ( "drv" %in% names(config) ) {
      if ( is.character(config[["drv"]]) ) {
        # it should be text that can be parsed and executed to create a DBI Driver
        private$DBIDriver <- eval(parse(text=config[["drv"]]))
      } else if ( ! inherits(config[["drv"]],"DBIDriver") ) {
        stop("Could not interpret DBIConfig$drv parameter:",config[["drv"]])
      } else {
        private$DBIDriver <- config[["drv"]]
      }
    } else {
      private$DBIDriver <- RSQLite::SQLite()
    }
    if ( "DBIConfig" %in% names(config) ) {
      # presume DBIConfig has the full and correct set of parameters to call dbConnect
      # We're not going attempt to do anything else to it here, except adjust dbname
      #   if it exists for Timestamp (if it is present, which it probably shouldn't be)
      private$DBIConfig <- config[["DBIConfig"]]
    } else {
      # SQLite will pre-package more stuff
      if ( "Database" %in% names(config) ) {
        dbname <- config[["Database"]]
      } else {
        dbname <- "SQLite"
      }

      # Force default extension if no explicit extension
      # Also inject model name (and timestamp if set up) in appropriate places
      dbname <- strsplit(dbname, "\\.")[[1]] # now a vector with the extension
      if ( length(dbname) == 1 ) dbname <- append(dbname,"sqlite")
      dbname[1] <- paste(Model$modelName,dbname[1],sep="_") # prepend model name
      dbname <- c(paste(dbname[-length(dbname)],collapse="."),dbname[length(dbname)])
      if ( ! is.null(self$Timestamp) ) {
        if ( is.null(self$TimeSeparator) ) self$TimeSeparator="_"
        dbname[1] <- paste(dbname[1],self$Timestamp,sep=self$TimeSeparator)
      }
      dbname <- paste(dbname,collapse=".")

      # Finally, prepend the directories
      # Force the file into ResultsDir/OutputDir
      dbname <- file.path(Model$modelPath,Model$setting("ResultsDir"),Model$setting("OutputDir"),basename(dbname))

      private$DBIConfig <- list(dbname=dbname)

      private$DBIConnector <- new("DBIConnector",
        .drv = private$DBIDriver,
        .conn_args = private$DBIConfig
      )
    }
  } else {
    # rehydrate private$DBIConnector
    private$DBIConnector <- config$ReopenData
  }

  self$open()
}

# The DBI implementation is really simple, once the connection is initialized and created

ve.connection.dbi.open <- function() {
  private$DBIConnection <- DBI::dbConnect(private$DBIConnector)
}

ve.connection.dbi.close = function() {
  if ( ! is.null( private$DBIConnection ) && DBI::dbIsValid(private$DBIConnection) ) {
    DBI::dbDisconnect(private$DBIConnection)
  }
}

ve.connection.dbi.raw         <- function() {
  return(private$DBIConnection)
} # override in specific subclasses

ve.connection.dbi.createTable <- function(Data,Table) {
  DBI::dbWriteTable(private$DBIConnection,Table,Data,overwrite=TRUE) # will trash if it already exists
  return( self$saveTableFields(Data,Table) )
}

ve.connection.dbi.appendTable     <- function(Data,Table) {
  DBI::dbWriteTable(private$DBIConnection,Table,Data,append=TRUE) # will append to existing table (or create, but we will head that off)
}

ve.connection.dbi.readTable   <- function(Table) {
  DBI::dbReadTable(private$DBIConnection,Table)
}

VEConnection.DBI <- R6::R6Class(
  # DBI implementation writes out to DBI connection
  # "folder" partitions are mapped to "name"
  "VEConnection.DBI",
  inherit = VEConnection,
  public = list( 
    # methods
    initialize  = ve.connection.dbi.init,
    raw         = ve.connection.dbi.raw,
    summary     = function() {
      paste( c(paste("Database:\n"),
        DBI:::dbGetConnectArgs(private$DBIConnector)$dbname,
        "Tables:\n",
        DBI::dbListTables(private$DBIConnection)
      ), collapse="\n")
    },
    save        = function() return(private$DBIConnector),
    
    # implementing methods
    nameTable = function(TableLoc) TableLoc$tableString(pathSep="_", tableSep="_"),
    createTable = ve.connection.dbi.createTable,
    appendTable = ve.connection.dbi.appendTable,
    readTable   = ve.connection.dbi.readTable,
    open        = ve.connection.dbi.open,
    close       = ve.connection.dbi.close
  ),
  private = list(
    Database      = NULL,
    DBIDriver     = NULL,
    DBIConfig     = NULL,
    DBIConnector  = NULL,
    DBIConnection = NULL,
    finalize      = function() { self$close() }
    # finalize will be called when the object is garbage collected or when R exits
  )
)

##################
# Exporter factory
##################

defaultExporters <- function() {
  exporters <- list(
    # bare minimum default exporters
    # augment with actual connection details in global or model visioneval.cnf
    csv = list(
      Connection = list(
        driver = "csv",
        Timestamp = "database" # attach it to the folder
      ),
      Partition = c(Scenario="name",Year="name",Global="name") # break out Global
    ),
    sql = list(
      Connection = list(
        driver = "dbi",
        Timestamp = "suffix" # standard sql or dbi puts timestamp on table name
      ),
      Partition = c(Scenario="path",Year="path",Global="path") # break out Global
    ),
    sqlite = list(
      Connection = list(
        driver      = "dbi",
        Timestamp   = "database", # alternate default for sqlite
        Database    = "Database",  # will get SQLite
        DBExtension = ".sqlite"
      ),
      Partition = c(Scenario="path",Year="path",Global="path") # break out Global
    ),
    data.frame = list(
      Connection = list(
        driver = "data.frame"
        # No Timestamp for data.frames
      ),
      Partition = c(Scenario="folder",Global="folder",Year="folder")
    )
  )
  exporters[["dbi"]] <- exporters[["sql"]]
  return(exporters)
}

connectionList <- list(
  csv=list(driverClass = VEConnection.CSV),
  dbi=list(driverClass = VEConnection.DBI),
  data.frame=list(driverClass = VEConnection.Dataframe),
  sql=list(driverClass = VEConnection.DBI),
  sqlite=list(driverClass = VEConnection.DBI)
  # parquet=VEConnection.Parquet  # Apache arrow implementation for parquet
  # excel=VEConnection.Excel      # Native Excel implementation rather than DBI
)
# Instantiate an Explorer
# Still to create: parquet format, others...

###########################################################
#
# Functions to create Exporters (open existing, create new)
#
###########################################################

#' Instantiate a VisionEval Exporter using a lookup name to find the Connection
#'
#' @description
#' `makeExporter` creates a VEExporter object to receive data exported from model results or
#' query results.
#'
#' @details
#' With no parameters or unknown `tag` or `driver`, will return a list of available Exporters.
#'
#' Connection: This string is used to initialize the exporter and will be a base directory (for CSV
#' or SQLite or other file-based output format) or a DBI connection string (identifying the
#' database and credentials). See the specific VEExporter documentation for details on the
#' connection string and any other optional parameters.
#' 
#' Partition Scheme: The partitioning can specify"merge" (just leave the field in the table),
#' "folder" where the table in that scenario or group is placed into a hierarchical table (folders
#' are always constructed first and layered as Scenario/Group/Table), or "name" in which case the
#' Scenario, Group or Table is pasted into the table name. If Table is "folder" and no name is
#' specified on an partition level, the actual table will be created by its name in a subfolder also
#' named after the Table. That's mostly useful if Group or Scenario is also identified by name. In
#' connections that do not support folders, "folder" will silently be replaced by "name". Not every
#' exporter format supports "folder" partitions (e.g. SQL, where flat tables are the order of the
#' day).
#' 
#' The partition scheme `c(Scenario="folder",Group="name",Table="folder)` will create a Scenario
#' folder, Table folders within that, and the name the tables within after the Group plus Table
#' (Table will always be included in the Table name, even if "name" is not the partitioning scheme).
#' If "merge" is specified for everything, it is the same as saying
#' `c("Scenario"="merge",Group="merge",Table="name")` - that is, the Table name will always be
#' forced into the name.
#'
#' NOTE: In keeping with usual DBI protocol, the "sql"/"dbi" connections will NOT create a database.
#'  So if you're hoping to export to an Access, MySQL, or PostgreSQL database, you should create the
#'  database first outside of VisionEval and include database location information in the
#'  `connection` parameter per the syntax of the various drivers. Exceptions are if you are creating
#'  Excel or SQLite output, in which case the interface does support creating output files (but
#'  that's because DBI allows it for those). They will be named after the Model associated with the
#'  VEResultsList or the VEQuery plus a Timestamp.
#'
#' NOTE: A typical call would look like `exporter <- newExporter("csv",Model=myModel)`. In practice,
#'   unless you are playing around with alternative connections to some new database, you won't call
#'   this function directly. Instead, you'll just pass the connection identifier (e.g. "csv") plus
#'   any connection or partition information to the VEResultsList or VEQuery $export function. If
#'   you need to make standard changes for production, add an Exporter block to your global or
#'   model-specific visioneval.cnf and those will be used by default.
#'
#' @param Model a VEmodel object, used by the interior connection to get model-specific settings like the
#'    output directory.
#' @param config a named list of parameters used to build a VEConnection. Default config builds CSV
#'    files in the model output directory. At a minimum, config should contain a "driver" element, which
#'    is the name of the connection specification block (either a built-in default or defined in the model
#'    or global visioneval.cnf).
#' @param reopen if TRUE will not create a new database name (using saved configuration), otherwise
#''   build a new name (not all connection types will care - mostly to avoid Timestamp problems)
#' @return A VEConnection (or derived) object giving access to the VisionEval results in `path`
#' @export
makeVEConnection <- function(Model,config=list(driver="csv"),reopen=FALSE) {
  # Usually called from within VEExportef initialization, which will provide
  #   useful connection defaults
  # Find driver class from config (default is "csv")
  driver <- if ( ! "driver" %in% names(config) ) "csv" else config$driver
  driverClass <- connectionList[[driver]][["driverClass"]]
  if (
    ! inherits( driverClass, "R6ClassGenerator" )
  ) {
    message("Available Drivers:")
    print(names(connectionList))
    stop("Driver type '",driver,"' does not have a VEConnection associated with it",call.=FALSE)
  } else {
    writeLog(paste("Creating Driver for ",driverClass$classname),Level="info")
  }
  # Create new driver object using "config"
  return ( driverClass$new(Model,config,reopen) )
}
