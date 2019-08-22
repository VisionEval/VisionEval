# Load the visioneval library to read data
library(visioneval)
require(tcltk)
requireNamespace("data.table")

# Helper function to find the directory with ModelState.Rda
# See: https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
choose.directory = function(start.in=getwd(),caption = 'Select model run directory') {
  tk_choose.dir(default=start.in,caption = caption)
# The Windows dialog annoyingly won't start in folders that it
# unilaterally deems "out of bounds"
#   if ( interactive() ) {
#     if (.Platform$OS.type == "windows") {
#       choose.dir(default=start.in,caption = caption)
#     } else {
#       tk_choose.dir(default=start.in,caption = caption)
#     }
#   }
}

# Locate Model State File

getModelStateFile <- function(modelState="ModelState.Rda") {
  model.dir = getwd()
  while ( ! file.exists(modelState) ) {
    cat("ModelState.Rda not found in",model.dir,"\n")
    cont <- readline("Pick a different directory? ")
    if ( substr(cont,1,1) %in% c("N","n","F","f") ) break
    model.dir = choose.directory(start.in=model.dir)
    modelState<-file.path(model.dir,"ModelState.Rda")
  }
  if ( ! file.exists(modelState) ) modelState <- ""
  return(modelState)
}

# NOTE: an "attribute" is a list element (with the value
# of the variable (the list) being one of its elements.

# Create a function to check if a specified attributes
# belongs to the variable
attributeExist <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(TRUE)
    }
  }
  return(FALSE)
}

# NOTE: The following function creates a (standard) data.frame
makeDataFrame <- function(Table, GroupTableName, OutputData, OutputAttr){
  OutputAllYr <- data.frame()

  for ( year in getYears()){
    # Looping over each year's worth of data in the given table
    # OutputIndex is a vector of booleans true for each row in GroupTableName that
    # contains data for the sought table in the sought year
    OutputIndex <- GroupTableName$Table %in% Table & GroupTableName$Group %in% year
    Output <- OutputData[OutputIndex] # trim OutputData down to the elements in Table/year

    if ( Table %in% c('Azone', 'Bzone', 'Marea') ){
      names(Output) <- paste0(GroupTableName$Name[OutputIndex], "_",
                              OutputAttr[OutputIndex], "_")
    } else if ( Table %in% c('FuelType', 'IncomeGroup') ){
      names(Output) <- paste0(GroupTableName$Name[OutputIndex]) # NOTE: why use paste0 at all?
    } else {
      next
    }

    # NOTE: what is the native structure of "Output"?
    Output <- data.frame(Output, stringsAsFactors = FALSE)
    # NOTE: want to use the logical or-operator (||) not the bitwise one
    if( Table %in% c('Azone', 'Bzone', 'Marea') | year != ModelState_ls$BaseYear ){
      Output$Year <- year
      # NOTE: Is there a more efficient way to do this?
      OutputAllYr <- data.table::rbindlist(list(OutputAllYr, Output), fill = TRUE)
    }
  }
  return(OutputAllYr)
}

modelStateFile <- getModelStateFile()
cat("Model State File:",modelStateFile,"\n")

if ( nchar(modelStateFile)==0 ) stop("No Model State to export.")

output.dir <- dirname(modelStateFile)
owd <- setwd(output.dir)

# Locate Model State, and through it, the Datastore and its access functions
ModelState_ls <- readModelState(FileName=modelStateFile)
Datastore <- ModelState_ls$Datastore
assignDatastoreFunctions(ModelState_ls$DatastoreType) # set up datastore interface

# Datastore elements with a "FILE" attribute were inputs
# the non-FILE elements are creations living in the Datastore (i.e. not inputs => outputs)
InputIndex <- sapply(Datastore$attributes, attributeExist, "FILE")

# Identify Groups and table names
splitGroupTableName <- strsplit(Datastore[!InputIndex, "groupname"], "/")
maxLength <- max(unlist(lapply(splitGroupTableName, length)))
# NOTE: the "length" in this case is the maximum number of elements composing the groupname

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

# Write all the outputs by table
# Read the data from the tables
OutputData <- apply(GroupTableName, 1, function(x) readFromTable(Name = x[3], Table = x[2], Group = x[1], ReadAttr = TRUE))
OutputAttr <- lapply(OutputData, function(x) attr(x, "UNITS"))

# Organize output by "Tables"
Tables <- unique(as.character(GroupTableName$Table))

# Dump the output
for ( tbl in Tables ) {
  OutDf <- makeDataFrame(tbl, GroupTableName, OutputData, OutputAttr)
}

dontDoIt <- function() {

  # Dump the output
  for ( tbl in Tables ) {
      OutDf <- makeDataFrame(tbl, GroupTableName, OutputData, OutputAttr)
  }

  # NOTE: need more flexible control (and a better name) for the output
  # folder that will hold the .csv files
  if(!dir.exists("output")){
    dir.create("output")
  } else {
    system("rm -rf output")
    dir.create("output")
  }

  # NOTE: Need to make the list of tables configurable (or read from the
  # model state.  Need to structure an export utility (CLI). optparse
  # package is our friend.
  # Ideally also have a Datastore inspector (GUI) - that should be in
  # VEGUI (which needs deeper surgery)
  for ( tbl in c("Azone", "Bzone", "Marea", "FuelType", "IncomeGroup") ){
    cat('Writing out', tbl, '\n')
    OutDf <- makeDataFrame(tbl, GroupTableName, OutputData, OutputAttr)

    # NOTE: why change the table name?
    if ( tbl == "IncomeGroup" ) tbl <- "JobAccessibility"
    filename <- file.path("output", paste0(tbl, ".csv"))
    fwrite(OutDf, file = filename)
  }

}