# Use namespaces to bring in library functions
requireNamespace("visioneval")
requireNamespace("tcltk")
requireNamespace("data.table")

# To use this in a visioneval runtime, just do this:
#   import::here(ve.export,"tools/exporter.R")

# ve.export
# Dump a model run Datastore to .csv files in an output folder
#
# Note that it's not a COMPLETE dump - you don't get base years for
# tables that were provided as model run inputs (you do get the
# future year outputs). Some tables get gratuitous name changes.
# Some tables (e.g. 'Vehicle' in VERPAT) embrace columns of
# incompatible lengths.
#
# Parameters:
#   modelStateFile="ModelState.Rda" :
#     Path to ModelState.rda, or of a folder containing ModelState.rda,
#     or to another file in a folder containing ModelState.rda
#   outputFolder="output"
#     Path to folder in which to put the .csv files
#     NOTE: will be deleted if overwrite=TRUE
#   includeTables=character(0)
#     Character vector of table names to include from the Datastore
#     character(0) or "" will include all
#   excludeTables=c("Vehicle")
#     Character vector of table names to exclude from the Datastore
#     'Model' will always be excluded
#     'Vehicle' from VERPAT includes incompatible numbers of rows
#   overwrite=TRUE
#     Blow away the outputFolder if it already exists
#   quiet=FALSE
#     If FALSE, dish up some progress messages and extra debugging data
#     If TRUE, only report errors

ve.export <- function (modelStateFile = "ModelState.Rda",
                       outputFolder   = "output",
                       includeTables  = character(0),  # default: all of them
                       excludeTables  = c("Vehicle"),  # Vehicle doesn't work for VERPAT
                       overwrite      = FALSE,         # TRUE to destroy outputFolder first if it exists
                       quiet          = FALSE          # TRUE to suppress all 
                       ) {
  owd <- getwd()
  # Do the actual export
  msf <- getModelStateFile(modelStateFile,quiet=quiet)
  idx <- indexModelState(msf,quiet=quiet)
  odt <- outputData(idx,quiet=quiet)
  exportData(  outputFolder  = outputFolder,
               includeTables = includeTables,
               excludeTables = excludeTables,
               overwrite     = overwrite,
               quiet         = quiet,
               odt
             )
  if ( exists("ModelState_ls",envir=.GlobalEnv) ) rm(ModelState_ls,pos=.GlobalEnv) # Hack to support framework
  setwd(owd)
}

# Locate Model State File
# See: https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
getModelStateFile <- function(modelState,quiet=FALSE) {
  if ( dir.exists(modelState) ) {
    model.dir <- modelState
  } else if ( file.exists(modelState) ) {
    model.dir <- dirname(modelState)
  } else {
    model.dir <- "."
  }
  model.dir <- normalizePath(model.dir)
  modelState <- file.path(model.dir,"ModelState.Rda") # visioneval says you don't get to change this name
  if ( interactive() ) {
    while ( ! file.exists(modelState) ) {
      cat("ModelState.Rda not found in",model.dir,"\n")
      cont <- readline("Pick a different directory? ")
      if ( substr(cont,1,1) %in% c("N","n","F","f") ) break
      model.dir <- tcltk::tk_choose.dir(default=model.dir,caption = "Select model run directory" )
      modelState<-file.path(model.dir,"ModelState.Rda")
    }
  }
  if ( ! file.exists(modelState) ) modelState <- ""

  # Status messaging and context check
  if ( nchar(modelState)==0 ) stop("No Model State found.")
  if ( ! quiet ) cat("Model State File:",modelState,"\n")

  # Move to location of Model State file (the model run root directory)
  output.dir <- dirname(modelState)
  setwd(output.dir)

  modelState
}

# Create a function to check if a specified attribute
# belongs to the Datastore row
attributeExist <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(TRUE)
    }
  }
  FALSE
}

indexModelState <- function(modelState,quiet=FALSE) {
  # Check that there is a model state
  if ( nchar(modelState)==0 || !file.exists(modelState) ) stop("No Model State File to index\n")

  # Load Model State, and through it, the Datastore and its access functions
  load(modelState,.GlobalEnv) # Horrendous hack to keep the framework happy by stuffing ModelState_ls into .GlobalEnv
  Datastore <- ModelState_ls$Datastore
  DatastoreType <- ModelState_ls$DatastoreType
  BaseYear <- ModelState_ls$BaseYear

  # NOTE: Datastore element of ModelState is a data.frame.
  #       The attributes column contains a list for each row
  # Datastore elements with a "FILE" attribute are inputs; we want the outputs
  # the non-FILE elements are creations living in the Datastore (i.e. not inputs => outputs)
  InputIndex <- sapply(Datastore$attributes, attributeExist, "FILE")
  splitGroupTableName <- strsplit(Datastore[!InputIndex, "groupname"], "/")
  maxLength <- max(unlist(lapply(splitGroupTableName, length)))
  # NOTE: the "length" in this case is the maximum number of elements composing the groupname, probably 3

  # Using 'do.call' turns each element of the splitGroupTableName list into one argument for rbind.data.frame
  # By contrast, calling rbind.data.frame(splitGroupTableName) simply converts the list (a single argument) into a
  # data.frame (so each element becomes one column) Explanation: https://www.stat.berkeley.edu/~s133/Docall.html
  GroupTableName <- data.frame()
  GroupTableName <- do.call(rbind.data.frame, lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x)))))
  colnames(GroupTableName) <- c("Group", "Table", "Name")

  # GroupTableName is now a data.frame with three columns
  # complete.cases blows away the rows that have any NA values
  # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each column)
  GroupTableName <- GroupTableName[complete.cases(GroupTableName),]
  if ( ! quiet ) cat(nrow(GroupTableName),"rows in Group/Table/Name data.frame\n")
  # returns the basis for later output
  list(GroupTableName=GroupTableName,Type=DatastoreType, BaseYear=BaseYear)
}

outputData <- function(Output,quiet=FALSE) {
  # Pick appropriate data extractor
  # JR Note on visioneval Datastore handling: OMG. Quel Cauchemar! Please let us refactor this!
  readFromTable <- function(x) {
    if ( Output$Type == "H5" ) {
      visioneval::readFromTableH5(Name = x[3], Table = x[2], Group = x[1], ReadAttr = TRUE)
    } else {
      visioneval::readFromTableRD(Name = x[3], Table = x[2], Group = x[1], ReadAttr = TRUE)
    }
  }

  # Extract the data
  if ( ! quiet ) cat("Extracting data...")
  Data<-apply(Output$GroupTableName, 1, readFromTable)
  Attr<-lapply(Data, function(x) attr(x, "UNITS"))
  if ( ! quiet ) cat("Done\n")
  c(Output,list(Data=Data,Attr=Attr))
}

# NOTE: The following function creates a (standard) data.frame
makeDataFrame <- function(Table, Output){
  OutputAllYr <- data.frame()

  for ( year in getYears()){
    # Looping over each year's worth of data in the given table
    # OutputIndex is a vector of booleans true for each row in GroupTableName that
    # contains data for the sought table in the sought year
    OutputIndex <- Output$GroupTableName$Table %in% Table & Output$GroupTableName$Group %in% year
    OutputRows  <- Output$Data[OutputIndex] # trim OutputData down to the elements in Table/year

    if ( Table %in% c('Azone', 'Bzone', 'Marea') ){
      orn <- paste0(Output$GroupTableName$Name[OutputIndex], "_",
                              Output$Attr[OutputIndex], "_")
    } else if ( Table %in% c('FuelType', 'IncomeGroup') ){
      orn <- paste0(Output$GroupTableName$Name[OutputIndex])
    } else {
      orn <- paste0(Output$GroupTableName$Name[OutputIndex])
    }
    names(OutputRows) <- orn
    OutputRows <- data.frame(OutputRows, stringsAsFactors = FALSE)

    # Do not output base year except for the Azone, Bzone and Marea Tables
    if( Table %in% c('Azone', 'Bzone', 'Marea') || year != Output$BaseYear ){
      OutputRows$Year <- year
      OutputAllYr <- data.table::rbindlist(list(OutputAllYr, OutputRows), fill = TRUE)
    }
  }
  OutputAllYr
}

exportData <- function(Output,
                       outputFolder="output",
                       includeTables=character(0),
                       excludeTables=character(0),
                       overwrite=FALSE,
                       quiet=TRUE) {

  # (Re-)Create the output directory (relative to getwd())
  if ( dir.exists(outputFolder) || file.exists(outputFolder) ) {
    if ( overwrite ) {
      unlink(outputFolder,recursive=TRUE,force=TRUE)
    } else {
      cat("Output folder exists:",outputFolder,"\n")
      stop("Must specify 'overwrite=TRUE' if output folder already exists.")
    }
  }
  dir.create(outputFolder)

  # Get the list of tables
  Tables <- unique(as.character(Output$GroupTableName$Table))
  if ( length(includeTables)>0 && nchar(includeTables[1])>0 ) {
    Tables <- intersect(Tables,includeTables)
  } else if ( length(excludeTables)>0 && nchar(excludeTables[1])>0 ) {
    Tables <- setdiff(Tables,unique(c(excludeTables,"Model")))
  } else {
    Tables <- setdiff(Tables,"Model")
  }
  if ( ! quiet ) {
    cat("Include Tables:\n")
    print(includeTables)
    cat("Exclude Tables:\n")
    print(excludeTables)
    cat("Exporting Tables:\n")
    print(Tables)
  }

  for ( tbl in Tables ){
    if ( ! quiet ) cat('Exporting', tbl,'...')
    OutDf <- makeDataFrame(tbl, Output)
    if ( tbl == "IncomeGroup" ) tbl <- "JobAccessibility"

    filename <- file.path(outputFolder, paste0(tbl, ".csv"))
    data.table::fwrite(OutDf, file=filename)
    if ( ! quiet ) cat("Done (as",basename(filename),")\n")
  }
}
