# Load the visioneval library to read data
requireNamespace("visioneval")
requireNamespace("tcltk")
requireNamespace("data.table")

DatastoreType="RD"
ve.export <- function (outputFolder="output",
                       includeTables=character(0), # default: all of them
                       excludeTables=c("Vehicle") # Vehicle doesn't work for VERPAT
                       ) {
  #For RPAT includeTables=c("Azone", "Bzone", "Marea", "FuelType", "IncomeGroup","Business","Household")

  owd <- getwd()
  # Do the actual export
  msf <- getModelStateFile()
  idx <- indexModelState(msf)
  odt <- outputData(idx)
  exportData(  outputFolder  = outputFolder,
               includeTables = includeTables,
               excludeTables = excludeTables,
               odt
             )
  if ( exists("ModelState_ls",envir=.GlobalEnv) ) rm(ModelState_ls,pos=.GlobalEnv) # Hack to support framework
  setwd(owd)
}

#################################################################
# Define interior functions
# (it would be nice just to stuff these into some namespace)

# Helper function to find the directory with ModelState.Rda
# See: https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
choose.directory = function(start.in=getwd(),caption = 'Select model run directory') {
  tcltk::tk_choose.dir(default=start.in,caption = caption)
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
getModelStateFile <- function() {
  model.dir <- getwd()
  modelState <- "ModelState.Rda" # visioneval says you don't get to change this name
  while ( ! file.exists(modelState) ) {
    cat("ModelState.Rda not found in",model.dir,"\n")
    cont <- readline("Pick a different directory? ")
    if ( substr(cont,1,1) %in% c("N","n","F","f") ) break
    model.dir <- choose.directory(start.in=model.dir)
    modelState<-file.path(model.dir,"ModelState.Rda")
  }
  if ( ! file.exists(modelState) ) {
    modelState <- ""
  } else {
    modelState <- normalizePath(modelState)
  }

  # Status messaging and context check
  cat("Model State File:",modelState,"\n")
  if ( nchar(modelState)==0 ) stop("No Model State selected.")

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

indexModelState <- function(modelState) {
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
  # returns the basis for later output
  list(GroupTableName=GroupTableName,Type=DatastoreType, BaseYear=BaseYear)
}

outputData <- function(Output) {
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
  cat("Extracting data...")
  Data<-apply(Output$GroupTableName, 1, readFromTable)
  Attr<-lapply(Data, function(x) attr(x, "UNITS"))
  cat("Done\n")
  c(Output,list(Data=Data,Attr=Attr))
}

exportData <- function(Output,
                       outputFolder="output",
                       includeTables=character(0),
                       excludeTables=character(0) ) {

  # (Re-)Create the output directory (relative to getwd())
  if ( dir.exists(outputFolder) || file.exists(outputFolder) )
    unlink(outputFolder,recursive=TRUE,force=TRUE)
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
  cat("Include Tables:\n")
  print(includeTables)
  cat("Exclude Tables:\n")
  print(excludeTables)
  cat("Exporting Tables:\n")
  print(Tables)

  for ( tbl in Tables ){
    cat('Exporting', tbl,'...')
    OutDf <- makeDataFrame(tbl, Output)
    if ( tbl == "IncomeGroup" ) tbl <- "JobAccessibility"

    filename <- file.path(outputFolder, paste0(tbl, ".csv"))
    data.table::fwrite(OutDf, file=filename)
    cat("Done (as",basename(filename),")\n")
  }
}
# End of interior functions
#################################################################
