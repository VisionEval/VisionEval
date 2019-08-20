# Load the visioneval library to read data
library(visioneval)
library(data.table) # NOTE: why is this even being used?

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
    # NOTE: need to comprehend the following, which seems perverse
    OutputIndex <- GroupTableName$Table %in% Table & GroupTableName$Group %in% year
    Output <- OutputData[OutputIndex]

    # NOTE: need to parameterize the Tables of interest
    if ( Table %in% c('Azone', 'Bzone', 'Marea') ){
      names(Output) <- paste0(GroupTableName$Name[OutputIndex], "_",
                              OutputAttr[OutputIndex], "_")
    } else if ( Table %in% c('FuelType', 'IncomeGroup') ){
      names(Output) <- paste0(GroupTableName$Name[OutputIndex]) # NOTE: shy use paste0 at all?
    } else {
      stop(Table, 'not found')
    }

    # NOTE: what is the native structure of "Output"?
    Output <- data.frame(Output, stringsAsFactors = FALSE)
    # NOTE: want to use the logical or-operator (||) not the bitwise one
    if( Table %in% c('Azone', 'Bzone', 'Marea') | year != ModelState_ls$BaseYear ){
      Output$Year <- year
      # NOTE: Is there a more efficient way to do this?
      OutputAllYr <- rbindlist(list(OutputAllYr, Output), fill = TRUE)
    }
  }
  return(OutputAllYr)
}

# Get the model state
# NOTE: usig a framework function to extract model_state.rda, and from
# that, the Datastore
ModelState_ls <- readModelState()
Datastore <- ModelState_ls$Datastore

# Collect the output of all the modules
# NOTE: identify the FILE attrbute of Datastore
# NOTE: only application of attributeExist - could streamline?
InputIndex <- sapply(Datastore$attributes, attributeExist, "FILE")

# NOTE: all other attributes besides the FILE are groups within the
# datastore. Datastore itself appears to be a two-dimensional
# structure...
splitGroupTableName <- strsplit(Datastore[!InputIndex, "groupname"], "/")
maxLength <- max(unlist(lapply(splitGroupTableName, length)))

# NOTE: why use do.call on a static function, rather than just call the
# function? That happens a lot (too much). 
GroupTableName <- do.call(rbind.data.frame, lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x)))))
colnames(GroupTableName) <- c("Group", "Table", "Name")
# NOTE: research the complete.cases function
GroupTableName <- GroupTableName[complete.cases(GroupTableName),]

OutputData <- apply(GroupTableName, 1, function(x) readFromTableRD(Name = x[3], Table = x[2], Group = x[1], ReadAttr = TRUE))
OutputAttr <- lapply(OutputData, function(x) attr(x, "UNITS"))

# Write all the outputs by table

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





