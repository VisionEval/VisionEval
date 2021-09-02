#===========
#datastore.R
#===========

#Functions for interacting with the datastore. There are 6 core functions which
#enable interaction with alternative datastore structures: initDatastore,
#initTable, initDataset, readFromTable, writeToTable, listDatastore. At present,
#two alternatives are supported: an HDF5 file datastore and a datastore that
#uses R data files. The names of these key functions has appended a suffix
#corresponding to the datastore type which is declared in the model run
#parameters. The initializeModel then make the appropriate assignments to the
#the basic function names depending on the declared datastore type. In addition
#to these core functions which depend on the datastore, there are several
#functions which call the core functions to move data to and from the datastore
#in order to run modules.

#Here is what each of these generic functions does:
#
#initDatastore
#   Prepares a new Datastore
#
#initTable
#   Make a Table space for a Dataset.
#   
#initDataset
#   Creates an empty Dataset
#   This only appears to be used in its H5 version (so may not need
#   to export it)
#
#readFromTable
#   Reads a dataset from a table.
#
#writeToTable
#   Writes a dataset to a table
#
#listDatastore
#  This function is principally used internally, but may be called at
#  the top level without parameters to build a datastore index into
#  ModelState. In the RD version, it also builds a file with the
#  Datastore table of contents in it. In the H5 version, the H5
#  Datastore furnishes its own index.

# GENERIC DATASTORE ACCESS FUNCTIONS
#-----------------------------------
#' Initialize Datastore
#'
#' initDatastore creates a datastore with starting structure.
#'
#' This function creates a new datastore for the model run. Alternately, if the value of the
#' AppendGroups parameter is not NULL the function will add the group or groups identified by this
#' parameter to an existing datastore.
#'
#' @param AppendGroups a character string identifying the names of groups to add to an existing
#'   datastore. The default value is NULL. If the value is NULL, a new datastore will be created. If
#'   an existing datastore has the same name as that defined for the model run, it will be deleted.
#'   The datastore will have a 'Global' group established in it as well as a group for each year
#'   identified in the model run years. If append is a character vector of group names, the groups
#'   identified in the character string will be added to the datastore.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if datastore initialization is successful. Calls the listDatastore function which
#'   adds a listing of the datastore contents to the model state file.
#' @export
initDatastore <- function(AppendGroups=NULL,envir=modelEnvironment()) {
  envir$initDatastore(AppendGroups,envir=envir)
}

#' Initialize Table
#'
#' This function initializes a table in the datastore.
#'
#' @param Table a string identifying the name of the table to initialize.
#' @param Group a string representation of the name of the top-level
#' subdirectory the table is to be created in (i.e. either 'Global' or the name
#' of the year).
#' @param Length a number identifying the table length.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return The value TRUE is returned if the function is successful at creating
#'   the table. In addition, the listDatastore function is run to update the
#'   inventory in the model state file. The function stops if the group in which
#'   the table is to be placed does not exist in the datastore and a message is
#'   written to the log.
#' @export
initTable <- function(Table, Group, Length, envir=modelEnvironment()) {
  envir$initTable(Table, Group, Length, envir=envir)
}

#' Initialize dataset.
#'
#' This function initializes a dataset.
#'
#' @param Spec_ls a list containing the standard module specifications
#'   described in the model system design documentation.
#' @param Group a string representation of the name of the top-level
#' subdirectory the table is to be created in (i.e. either 'Global' or the name
#' of the year).
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if dataset is successfully initialized. If the identified table
#' does not exist, the function throws an error.
#' @export
initDataset <- function(Spec_ls, Group, envir=modelEnvironment()) {
  envir$initDataset(Spec_ls, Group, envir=envir)
}

#' Read from datastore table.
#'
#' \code{readFromTable} reads a dataset from a datastore table, dispatching through
#' the ModelState environment to linked Datastores from other model stages.
#'
#' @param Name A string identifying the name of the dataset to be read from.
#' @param Table A string identifying the complete name of the table where the
#' dataset is located.
#' @param Group a string representation of the name of the datastore group the
#' data is to be read from.
#' @param Index A numeric vector identifying the positions the data is to be
#' written to. NULL if the entire dataset is to be read.
#' @param ReadAttr A logical identifying whether to return the attributes of
#' the stored dataset. The default value is FALSE.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#' @return A vector of the same type stored in the datastore and specified in
#' the TYPE attribute.
#' @export
readFromTable <- function(Name, Table, Group, Index = NULL, ReadAttr = TRUE, envir=modelEnvironment()) {

  G <- getModelState(envir)

  # Attempt to read the Table from current Datastore using its associated function
  table <- envir$readFromTable(Name, Table, Group, Index, ReadAttr, envir=envir)
  if ( length(table)==1 && is.null(attributes(table)) && length(G$DatastorePath)>1 ) {
    # table might legitimately be NA of length 1. Attributes are non-null if it is "for real"
    # If failed to find, then try upstream Datastore locations
    # DatastorePath holds absolute path to Datastore
    msPaths <- getModelStatePaths(envir=envir) # Does not return first element (currently writable Datastore)

    # Iterate over additional ModelStates in prior model stages
    # ms.env contains an upstream ModelState_ls
    for ( ms.env in msPaths ) {
      # Use the read function associated with the upstream Datastore
      table <- ms.env$readFromTable(Name, Table, Group, Index, ReadAttr, envir=ms.env)
      if ( ! is.null(attributes(table)) ) {
        writeLog(
          paste0(
            "Found ",file.path(Name,Table,Group)," in ",
            file.path(ms.env$ModelState_ls$ModelStatePath, ms.env$ModelState_ls$DatastoreName)
          ), Level="trace"
        )
        break
      }
    }
    if ( is.null(attributes(table)) ) {
      writeLog(
        paste0(file.path(Name,Table,Group)," ",class(table)," (",length(table),") not found on DatastorePath"),
        Level="info"
      )
    } else {
      # Only "trace" - caller will report at LogLevel "info"
      writeLog(
        paste0(file.path(Name,Table,Group)," ",class(table)," (",length(table),") not found in current Datastore"),
        Level="trace"
      )
    }
  } else {
    writeLog(
      paste0(file.path(Name,Table,Group)," ",class(table)," (",length(table),") returned from current Datastore"),
      Level="trace"
    )
  }
  return(table)
}

#' Write to a datastore table.
#'
#' Writes data to an RData (RD) type datastore table and initializes
#' dataset if needed.
#'
#' This function writes a dataset file. It initializes the dataset if the dataset does not exist.
#' Enables data to be written to specific location indexes in the dataset.
#'
#' @param Data_ A vector of data to be written.
#' @param Spec_ls a list containing the standard module 'Set' specifications
#'   described in the model system design documentation.
#' @param Group a string representation of the name of the datastore group the
#' data is to be written to.
#' @param Index A numeric vector identifying the positions the data is to be
#'   written to.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if data is sucessfully written.
#' @export
writeToTable  <- function(Data_, Spec_ls, Group, Index = NULL, envir=modelEnvironment()) {
  envir$writeToTable(Data_, Spec_ls, Group, Index, envir=envir)
}

#' List datastore contents.
#'
#' Lists the contents of a datastore.
#'
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if the listing is successfully read from the datastore.
#' @export
listDatastore <- function(envir=NULL) {
  if ( is.null(envir) ) envir <- ve.model
  envir$listDatastore(envir=envir)
}

DatastoreFunctionNames <-
    c("initDatastore", "initTable", "initDataset", "readFromTable",
      "writeToTable", "listDatastore")

#REPORT ALLOWED DATASTORE TYPES
#==============================
#' Return a list of valid Datastore types
#'
#' \code{getAllowedDstoreTypes} a visioneval framework control function that
#' returns the list of supported Datastore types.
#' 
#' @return a character vector of valid Datastore types
#' @export
getAllowedDstoreTypes <- function() c("RD", "H5")

#ASSIGN DATASTORE INTERACTION FUNCTIONS
#======================================
#' Assign datastore interaction functions
#'
#' \code{assignDatastoreFunctions} a visioneval framework control function that
#' assigns the values of the functions for interacting with the datastore to the
#' functions for the declared datastore type.
#'
#' The visioneval framework can work with different types of datastores. For
#' example a datastore which stores datasets in an HDF5 file or a datastore
#' which stores datasets as RData files in a directory hierarchy. This function
#' reads the 'DatastoreType' parameter from the model state file and then
#' assigns the common datastore interaction functions the values of the
#' functions for the declared datastore type.
#'
#' @param DstoreType A string identifying the datastore type (get from envir$ModelState_ls if NULL
#' @param FunctionName A character vector identifyin a single function to find for the indicated
#'   DstoreType
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return If FunctionName is provided and found, the function. Otherwise, invisibly
#'   return the list of all functions
#' @export
assignDatastoreFunctions <- function(DstoreType=NULL, FunctionName=NULL, envir=modelEnvironment()) {

  if ( is.null(DstoreType) ) DstoreType <- envir$ModelState_ls$DatastoreType

  if ( ! DstoreType %in% getAllowedDstoreTypes()) {
    Msg <- paste0("Unknown 'DatastoreType': ", DstoreType)
    writeLog(c(Msg,"\nRecognized Types:",paste(getAllowedDstoreTypes(), collapse = ", ")),Level="error")
    stop(Msg)
  } else {
    DstoreFuncs_ <- lapply(
      paste0(DatastoreFunctionNames,DstoreType),
      function(x) return(
        if ( exists(x,envir=envir) ) {
          get(x,envir=envir) # make a list of function objects
        } else {
          NA
        }
      )
    )
    names(DstoreFuncs_) <- DatastoreFunctionNames
  }
  if ( ! is.null(FunctionName) ) {
    if ( ! FunctionName %in% DatastoreFunctionNames ) {
      Msg <- paste0("Unknown Datastore Function: ", FunctionName)
      writeLog(c(Msg,"\nRecognized Functions:",paste(DatastoreFunctionNames, collapse = ", ")),Level="error")
      stop(Msg)
    } else {
      return(DstoreFuncs_[[FunctionName]])
    }
  } else {
    invisible( lapply(DatastoreFunctionNames,function(n) assign(n,DstoreFuncs_[[n]],envir=envir)) )
  }
}

###############################################################################
#                                                                             #
#              IMPLEMENTATION OF DATASTORE USING RDATA FILES                  #
#                                                                             #
###############################################################################

#LIST DATASTORE CONTENTS
#=======================
#' List datastore contents for an RData (RD) type datastore.
#'
#' \code{listDatastoreRD} a visioneval framework datastore connection function
#' that lists the contents of an RData (RD) type datastore.
#'
#' This function writes a listing of the contents of a datastore for
#'   an RData (RD) type ' datastore.
#'
#' @param DataListing_ls a list containing named elements describing a new data
#' item being added to the datastore listing and the model state file. The list
#' components are:
#' group - the name of the group (path) the item is being added to;
#' name - the name of the data item (directory or dataset);
#' groupname - the full path to the data item relative to the Datastore root;
#' attributes - a list containing the named attributes of the data item.
#' @param ModelStateFile is the path to an alternate ModelState
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return the datastore listing (invisibly) after writing to the model state file.
#' @export
listDatastoreRD <- function(DataListing_ls = NULL, ModelStateFile = NULL, envir=modelEnvironment()) {
  #Load the model state file
  G <- if ( ! is.null(ModelStateFile) ) {
    readModelState(FileName=ModelStateFile,envir=envir)
  } else {
    getModelState(envir)
  }

  #If no Datastore component, get from DatastoreListing.RData
  listingPath <- file.path(G$ModelStatePath, G$DatastoreName, "DatastoreListing.Rda")
  if (is.null(G$Datastore)) {
    if ( ! file.exists(listingPath) ) {
      # TODO: if DatastoreListing.Rda - rebuild from Datastore file system...
      stop(
        writeLog(paste("listDatastoreRD: Missing",listingPath),Level="error")
      )
    } else {
      loadEnv <- new.env()
      load(listingPath,envir=loadEnv)
      Datastore_df <- data.frame(
        group = loadEnv$DatastoreListing_ls$group,
        name = loadEnv$DatastoreListing_ls$name,
        groupname = loadEnv$DatastoreListing_ls$groupname,
        stringsAsFactors = FALSE
      )
      Datastore_df$attributes <- loadEnv$DatastoreListing_ls$attributes
    }
  } else {
    Datastore_df <- G$Datastore
  }

  #Update the datastore listing
  if (!is.null(DataListing_ls)) {
    NewDatastore_df <-
      rbind(
        Datastore_df[,c("group", "name", "groupname")],
        DataListing_ls[c("group", "name", "groupname")])
    NewDatastore_df$attributes <-
      c(Datastore_df$attributes, list(DataListing_ls$attributes))
  } else {
    NewDatastore_df <- Datastore_df
  }

  #Update model state and datastore listing
  setModelState(list(Datastore = NewDatastore_df),envir=envir)
  DatastoreListing_ls <- as.list(NewDatastore_df)
  save(DatastoreListing_ls, file = file.path(listingPath))
  invisible(DatastoreListing_ls)
}

#INITIALIZE DATASTORE
#====================
#' Initialize Datastore for an RData (RD) type datastore.
#'
#' \code{initDatastoreRD} a visioneval framework datastore connection function
#' that creates a datastore with starting structure for an RData (RD) type
#' datastore.
#'
#' This function creates the datastore for the model run with the initial
#' structure for an RData (RD) type datastore. Alternately, if the value of
#' the AppendGroups parameter is not NULL the function will add the group or
#' groups identified by this parameter to an existing datastore.
#'
#' @param AppendGroups a character string identifying the names of groups to add
#'   to an existing datastore. The default value is NULL. If the value is NULL,
#'   a new datastore will be created. If an existing datastore has the same name
#'   as that defined for the model run, it will be deleted. The datastore will
#'   have a 'Global' group established in it as well as a group for each year
#'   identified in the model run years. If append is a character vector of group
#'   names, the groups identified in the character string will be added to the
#'   datastore.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if datastore initialization is successful. Calls the
#' listDatastore function which adds a listing of the datastore contents to the
#' model state file.
#' @import stats utils
#' @export
initDatastoreRD <- function(AppendGroups = NULL, envir=modelEnvironment()) {

  G <- getModelState(envir)
  DatastoreName <- G$DatastoreName;
  dsPath <- file.path(G$ModelStatePath,DatastoreName)

  # If 'AppendGroups' is NULL initialize a new datastore
  if (is.null(AppendGroups)) {
    #If datastore exists, delete
    if (dir.exists(dsPath)) {
      unlink(dsPath,recursive=TRUE)
    }
    #Create datastore
    dir.create(dsPath)
    #Initialize the DatastoreListing
    Datastore_df <-
      data.frame(
        group = "/",
        name = "",
        groupname = "",
        attributes = NA,
        stringsAsFactors = FALSE)
    Datastore_df$attributes <- as.list(Datastore_df$attributes)
    setModelState(list(Datastore = Datastore_df),envir=envir)
    #Create global group which stores data that is constant for all geography and
    #all years
    dir.create(file.path(dsPath, "Global"))
    listDatastoreRD(
      list(group = "/", name = "Global", groupname = "Global",
           attributes = list(NA)),
      envir=envir
    )
    #Create groups for years
    Years <- getYears(envir=envir)
    for (year in Years) {
      YearGroup <- year
      dir.create(file.path(dsPath, YearGroup))
      listDatastoreRD(
        list(group = "/", name = YearGroup, groupname = YearGroup,
             attributes = list(NA)),
        envir=envir
      )
    }
    #If 'AppendGroups' is not NULL add listed groups to existing datastore
  } else {
    #If the datastore exists add the groups
    if (file.exists(dsPath)) {
      #Identify existing groups in the datastore
      DstoreGroups_ <- local({
        DstoreGroups_ls <- strsplit(G$Datastore$group, "/")
        ToKeep_ <- unlist(lapply(DstoreGroups_ls, function(x) length(x) == 2))
        DstoreGroups_ls <- DstoreGroups_ls[ToKeep_]
        DstoreGroups_ <- unique(unlist(lapply(DstoreGroups_ls, function(x) x[2])))
      })
      #Add groups listed in 'AppendGroups' if none are present in datastore
      if (!any(AppendGroups %in% DstoreGroups_)) {
        for (Grp in AppendGroups) {
          dir.create(file.path(dsPath, Grp))
          listDatastoreRD(
            list(group = "/", name = Grp, groupname = Grp,
                 attributes = list(NA)),
            envir=envir
          )
        }
      } else {
        #Error if any ups listed in 'AppendGroups' are present in datastore
        DupGrp <- AppendGroups[AppendGroups %in% DstoreGroups_]
        stop(paste(
          "The following groups listed in the 'AppendGroups' argument are",
          "present in the datastore:",
          paste(DupGrp, collapse = ", "), ".",
          "The names of these groups must be removed from the 'AppendGroups'",
          "argument."
        ))
      }
    } else {
      #Error if the datastore does not exist
      stop(paste(
        "The datastore -", DatastoreName, "- identified in the model",
        "'run_parameters.json' file does not exist.",
        "In order for a datastore to be initialized with appended groups,",
        "this datastore must be present."
      ))
    }
  }
  TRUE
}
#initDatastoreRD()


#INITIALIZE TABLE IN DATASTORE
#=============================
#' Initialize table in an RData (RD) type datastore.
#'
#' \code{initTableRD} a visioneval framework datastore connection function
#' initializes a table in an RData (RD) type datastore.
#'
#' This function initializes a table in an RData (RD) type datastore.
#'
#' @param Table a string identifying the name of the table to initialize.
#' @param Group a string representation of the name of the top-level
#' subdirectory the table is to be created in (i.e. either 'Global' or the name
#' of the year).
#' @param Length a number identifying the number of rows the table is allowed to have
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return The value TRUE is returned if the function is successful at creating
#'   the table. In addition, the listDatastore function is run to update the
#'   inventory in the model state file. The function stops if the group in which
#'   the table is to be placed does not exist in the datastore and a message is
#'   written to the log.
#' @export
initTableRD <- function(Table, Group, Length, envir=modelEnvironment()) {
  G <- getModelState(envir)
  DatastoreName <- G$DatastoreName;
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)

  #Create a directory for the table
  # TODO: warn (lightly) if Table exists and do not update anything
  result <- dir.create(file.path(dsPath, Group, Table))
  #Update the datastore listing and model state
  listDatastoreRD(
    list(group = paste0("/", Group), name = Table,
         groupname = paste(Group, Table, sep = "/"),
         attributes = list(LENGTH = Length)
    ),
    envir=envir
  )
  TRUE
}
#initTableRD("Azone", "2010", 3)


#INITIALIZE A DATASET IN A TABLE
#===============================
#' Initialize dataset in an RData (RD) type datastore table.
#'
#' \code{initDatasetRD} a visioneval framework datastore connection function
#' initializes a dataset in an RData (RD) type datastore table.
#'
#' This function initializes a dataset in an RData (RD) type datastore table.
#'
#' @param Spec_ls a list containing the standard module specifications
#'   described in the model system design documentation.
#' @param Group a string representation of the name of the top-level
#' subdirectory the dataset is to be created in (i.e. either 'Global' or the name
#' of the year).
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if dataset is successfully initialized. If the identified group or table
#' does not exist, the function throws an error.
#' @export
initDatasetRD <- function(Spec_ls, Group, envir=modelEnvironment()) {
  G <- getModelState(envir)
  DatastoreName <- G$DatastoreName;
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)

  Table <- paste(Group, Spec_ls$TABLE, sep = "/")
  Name <- Spec_ls$NAME
  DatasetName <- paste(Table, Name, sep = "/")
  #Check whether the table exists and throw error if it does not
  TableExists <- Table %in% G$Datastore$groupname
  if (!TableExists) {
    Msg <- paste0("Specified table - ", Table, " - doesn't exist. ",
                  "The table must be initialized before the dataset can ",
                  "be initialized.")
    writeLog(Msg,Level="error")
    stop(Msg)
  }
  #Get the table length
  Length <- G$Datastore$attributes[G$Datastore$groupname == Table][[1]]$LENGTH
  #Create an initialized dataset
  Dataset <-
    switch(Types()[[Spec_ls$TYPE]]$mode,
           character = character(Length),
           double = numeric(Length),
           integer = integer(Length),
           logical = logical(Length))
  attributes(Dataset) <- Spec_ls
  #Save the initialized dataset
  DatasetName <- paste(Spec_ls$NAME, "Rda", sep = ".")
  save(Dataset, file = file.path(dsPath, Table, DatasetName))
  #Update the datastore listing and model state
  listDatastoreRD(
    list(group = paste0("/", Table), name = Spec_ls$NAME,
         groupname = paste(Table, Spec_ls$NAME, sep = "/"),
         attributes = Spec_ls
    ),
    envir=envir
  )
  TRUE
}
#source("data/test_spec.R")
#initDatasetRD(TestSpec_ls, "2010")


#READ FROM TABLE
#===============
#' Read from an RData (RD) type datastore table.
#'
#' \code{readFromTableRD} a visioneval framework datastore connection function
#' that reads a dataset from an RData (RD) type datastore table.
#'
#' This function reads a dataset from an RData (RD) type datastore table.
#'
#' @param Name A string identifying the name of the dataset to be read from.
#' @param Table A string identifying the complete name of the table where the
#' dataset is located.
#' @param Group a string representation of the name of the datastore group the
#' data is to be read from.
#' @param Index A numeric vector identifying the positions the data is to be
#' read from. NULL if the entire dataset is to be read.
#' @param ReadAttr A logical identifying whether to return the attributes of
#' the stored dataset. The default value is TRUE (return the attributes)
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return A vector of the same type stored in the datastore and specified in
#' the TYPE attribute.
#' @export
readFromTableRD <- function(Name, Table, Group, Index = NULL, ReadAttr = TRUE, envir=modelEnvironment()) {
  # This function does NOT process the DatastorePath - the overall interface does that
  G <- getModelState(envir)
  DatastoreName <- G$DatastoreName;
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)
  
  #Check that dataset exists to read from and if so get path to dataset
  DatasetExists <- checkDataset(Name, Table, Group, G$Datastore) # do not pass envir - path already checked
  if (DatasetExists) {
    FileName <- paste(Name, "Rda", sep = ".")
    DatasetPath <- file.path(dsPath, Group, Table, FileName)
  } else {
    return(NA)
  }
  #Load the dataset
  if ( ! file.exists(DatasetPath) ) return(NA)
  loadEnv <- new.env()
  load(DatasetPath,envir=loadEnv)
  Dataset <- loadEnv$Dataset
  if ( is.null(Dataset) ) {
    msg <- writeLog(paste("Failed to load",DatasetPath),Level="error")
    stop(msg)
  }

  #Convert NA values
  # This happens with H5, but not RD (where we can save suitable NA values directly)
  # NAValue <- as.vector(attributes(Dataset)$NAVALUE)
  # Dataset[Dataset == NAValue] <- NA

  #If there is an Index, check, and use to subset the dataset
  if (!is.null(Index)) {
    #Save the attributes
    Attr_ls <- attributes(Dataset)
    TableAttr_ <-
      unlist(G$Datastore$attributes[G$Datastore$groupname == file.path(Group, Table)])
    AllowedLength <- TableAttr_["LENGTH"]
    if (any(Index > AllowedLength)) {
      Message <-
        c(
          paste0(
            "One or more specified indices for reading data from ",
            paste(Group,Table,sep="/"), " exceed ", AllowedLength
          ),
          paste("Indices:",Index[Index>AllowedLength],collapse=", ")
        )
      writeLog(Message,Level="error")
      stop(Message)
    } else {
      # Re-apply attributes to the requested subset
      Dataset <- Dataset[Index]
      attributes(Dataset) <- Attr_ls
    }
  }
  #Return results
  # WARNING: Non-null attributes are how the standard read table wrapper detects
  #   whether a Dataset exists in the Datastore Group/Table. Only set ReadAttr=FALSE when
  #   reaching for unadorned data vector (i.e. you're sure it exists, or don't care if it
  #   doesn't)
  if (!ReadAttr) {
    attributes(Dataset) <- NULL
  }
  Dataset
}
#readFromTableRD("Azone", "Azone", "2010")
#readFromTableRD("Azone", "Azone", "2010", Index = 1:2)


#WRITE TO TABLE
#==============
#' Write to an RData (RD) type datastore table.
#'
#' \code{writeToTableRD} a visioneval framework datastore connection function
#' that writes data to an RData (RD) type datastore table and initializes
#' dataset if needed.
#'
#' This function writes a dataset file to an RData (RD) type datastore table. It
#' initializes the dataset if the dataset does not exist. Enables data to be
#' written to specific location indexes in the dataset.
#'
#' @param Data_ A vector of data to be written.
#' @param Spec_ls a list containing the standard module 'Set' specifications
#'   described in the model system design documentation.
#' @param Group a string representation of the name of the datastore group the
#' data is to be written to.
#' @param Index A numeric vector identifying the positions the data is to be
#'   written to.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if data is sucessfully written.
#' @export
writeToTableRD <- function(Data_, Spec_ls, Group, Index = NULL, envir=modelEnvironment()) {
  G <- getModelState(envir)
  dsPath <- file.path(G$ModelStatePath, G$DatastoreName)
  Name <- Spec_ls$NAME
  Table <- Spec_ls$TABLE
  #Check that dataset exists to write to and attempt to create if not
  #Do NOT pass envir to checkDataset here...
  DatasetExists <- checkDataset(Name, Table, Group, G$Datastore)
  if ( DatasetExists ) {
    # check for actual file, in case dataset listing is obsolete
    # It is fine to re-write a missing file
    DatasetName <- paste0(Name, ".Rda")
    DatasetPath <- file.path(dsPath, Group, Table, DatasetName)
    DatasetExists <- file.exists(DatasetPath)
  }

  if (!DatasetExists) {
    # Add to Datastore listing
    GroupName <- file.path(Group, Spec_ls$TABLE)
    Length <- G$Datastore$attributes[G$Datastore$groupname == GroupName][[1]]$LENGTH
    if ( !is.numeric(Length) ) {
      msg <- writeLog(paste("WriteTableRD: Table",GroupName,"has not been created in the current Datastore"),Level="error")
      stop(msg)
    }
    Dataset <-
      switch(Types()[[Spec_ls$TYPE]]$mode,
             character = character(Length),
             double = numeric(Length),
             integer = integer(Length),
             logical = logical(Length))
    Attr_ls <- Spec_ls
    listDatastoreRD(
      list(group = paste0("/", GroupName), name = Name,
           groupname = paste(GroupName, Name, sep = "/"),
           attributes = Spec_ls
      ),
      envir=envir
    )
  } else {
    Dataset <- readFromTableRD(Name, Table, Group, ReadAttr = TRUE, envir=envir)
    Attr_ls <- attributes(Dataset)
  }

  #Modify the loaded dataset
  if (is.null(Index)) {
    Dataset <- Data_
  } else {
    if (any(Index > length(Dataset))) {
      Message <-
        paste0(
          "One or more specified indicies for writing data to ",
          file.path(Group, Table, Name), " exceed the length of the dataset."
        )
      writeLog(Message,Level="error")
      stop(Message)
    } else {
      Dataset[Index] <- Data_
    }
  }
  #Save the dataset
  attributes(Dataset) <- Attr_ls
  DatasetName <- paste0(Name, ".Rda")
  save(Dataset, file = file.path(dsPath, Group, Table, DatasetName))
  TRUE
}

#readFromTableRD("Azone", "Azone", "2010")
#NewVals_ <- paste0("A", 1:3)
#writeToTableRD(NewVals_, TestSpec_ls, "2010", Index = NULL)
#readFromTableRD("Azone", "Azone", "2010")
#Write an uninitialized dataset
#initTableRD("Bzone", "2010", 10)
#TestSpec_ls$NAME <- "Bzone"
#TestSpec_ls$TABLE <- "Bzone"
#NewVals_ <- paste0("B", 1:10)
#writeToTableRD(NewVals_, TestSpec_ls, "2010")
#readFromTableRD("Bzone", "Bzone", "2010")
#TestSpec_ls$NAME <- "Azone"
#NewVals_ <- c("A1", "A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A3")
#writeToTableRD(NewVals_, TestSpec_ls, "2010")
#readFromTableRD("Azone", "Bzone", "2010")


###############################################################################
#                                                                             #
#              IMPLEMENTATION OF DATASTORE USING HDF5 FILES                   #
#                                                                             #
###############################################################################

#LIST DATASTORE CONTENTS
#=======================
#' List datastore contents for an HDF5 (H5) type datastore.
#'
#' \code{listDatastoreH5} a visioneval framework datastore connection function
#' that lists the contents of an HDF5 (H5) type datastore.
#'
#' This function lists the contents of a datastore for an HDF5 (H5) type
#' datastore.
#'
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return the datastore listing (invisibly) after writing to the model state file.
#' @export
#' @import rhdf5
listDatastoreH5 <- function(envir=modelEnvironment()) {
  G <- getModelState(envir)
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)

  H5File <- H5Fopen(dsPath)
  DS_df <- h5ls(H5File, all = TRUE)
  DS_df$groupname <- paste(DS_df$group, DS_df$name, sep = "/")
  DS_df$groupname <- gsub("^/+", "", DS_df$groupname)
  DS_df <-
    DS_df[, c("group", "name", "groupname", "otype", "num_attrs", "dclass", "dtype")]
  Attr_ls <- list()
  for (i in 1:nrow(DS_df)) {
    if (DS_df$num_attrs[i] == 0) {
      Attr_ls[[i]] <- NA
    } else {
      Item <- paste(DS_df$group[i], DS_df$name[i], sep = "/")
      Attr_ls[[i]] <- h5readAttributes(H5File, Item)
    }
  }
  DS_df$attributes <- Attr_ls
  H5Fclose(H5File)
  AttrToWrite_ <- c("group", "name", "groupname", "attributes")
  dsListing <- DS_df[, AttrToWrite_]
  setModelState(list(Datastore = dsListing),envir=envir)
  invisible(dsListing)
}


#INITIALIZE DATASTORE
#====================
#' Initialize Datastore for an HDF5 (H5) type datastore.
#'
#' \code{initDatastoreH5} a visioneval framework datastore connection function
#' that creates datastore with starting structure for an HDF5 (H5) type
#' datastore.
#'
#' This function creates the datastore for the model run with the initial
#' structure for an HDF5 (H5) type datastore. Alternately, if the value of
#' the AppendGroups parameter is not NULL the function will add the group or
#' groups identified by this parameter to an existing datastore.
#'
#' @param AppendGroups a character string identifying the names of groups to add
#'   to an existing datastore. The default value is NULL. If the value is NULL,
#'   a new datastore will be created. If an existing datastore has the same name
#'   as that defined for the model run, it will be deleted. The datastore will
#'   have a 'Global' group established in it as well as a group for each year
#'   identified in the model run years. If append is a character vector of group
#'   names, the groups identified in the character string will be added to the
#'   datastore.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if datastore initialization is successful. Calls the
#' listDatastore function which adds a listing of the datastore contents to the
#' model state file.
#' @export
#' @import rhdf5
initDatastoreH5 <- function(AppendGroups = NULL, envir=modelEnvironment()) {
  G <- getModelState(envir)
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)
  #If 'AppendGroups' is NULL initialize a new datastore
  if (is.null(AppendGroups)) {
    #If data store exists, delete
    if (file.exists(dsPath)) {
      file.remove(dsPath)
    }
    #Create data store file
    H5File <- H5Fcreate(dsPath)
    #Create global group which stores data that is constant for all geography and
    #all years
    h5createGroup(H5File, "Global")
    #Create groups for years
    for ( year in as.character(getYears(envir=envir)) ) {
      YearGroup <- year
      h5createGroup(H5File, YearGroup)
    }
    H5Fclose(H5File)
    #If 'AppendGroups' is not NULL add listed groups to existing datastore
  } else {
    #If the datastore exists add the groups
    if (file.exists(dsPath)) {
      #Identify existing groups in the datastore
      DstoreGroups_ <- local({
        DstoreGroups_ls <- strsplit(G$Datastore$group, "/")
        ToKeep_ <- unlist(lapply(DstoreGroups_ls, function(x) length(x) == 2))
        DstoreGroups_ls <- DstoreGroups_ls[ToKeep_]
        DstoreGroups_ <- unique(unlist(lapply(DstoreGroups_ls, function(x) x[2])))
      })
      #Add groups listed in 'AppendGroups' if none are present in datastore
      if (!any(AppendGroups %in% DstoreGroups_)) {
        for (Grp in AppendGroups) {
          h5createGroup(dsPath, Grp)
        }
        #Error if groups listed in 'AppendGroups' are present in datastore
      } else {
        DupGrp <- AppendGroups[AppendGroups %in% DstoreGroups_]
        stop(paste(
          "The following groups listed in the 'AppendGroups' argument are",
          "present in the datastore:",
          paste(DupGrp, collapse = ", "), ".",
          "The names of these groups must be removed from the 'AppendGroups'",
          "argument."
        ))
      }
      #Error if the datastore does not exist
    } else {
      stop(paste(
        "The datastore -", G$DatastoreName, "- identified in the model",
        "'run_parameters.json' file does not exist.",
        "In order for a datastore to be initialized with appended groups,",
        "this datastore must be present."
      ))
    }
  }
  listDatastoreH5(envir=envir)
  return(TRUE)
}


#INITIALIZE TABLE IN DATASTORE
#=============================
#' Initialize table in an HDF5 (H5) type datastore.
#'
#' \code{initTableH5} a visioneval framework datastore connection function that
#' initializes a table in an HDF5 (H5) type datastore.
#'
#' This function initializes a table in an HDF5 (H5) type datastore.
#'
#' @param Table a string identifying the name of the table to initialize.
#' @param Group a string representation of the name of the group the table is to
#' be created in.
#' @param Length a number identifying the table length.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return The value TRUE is returned if the function is successful at creating
#'   the table. In addition, the listDatastore function is run to update the
#'   inventory in the model state file. The function will create any
#'   missing Group as it creates the Table.
#' @export
#' @import rhdf5
initTableH5 <- function(Table, Group, Length, envir=envir) {
  G <- getModelState(envir=envir)
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)
  NewTable <- paste(Group, Table, sep = "/")
  H5File <- H5Fopen(dsPath)
  h5createGroup(H5File, NewTable)
  H5Group <- H5Gopen(H5File, NewTable)
  h5writeAttribute(Length, H5Group, "LENGTH")
  H5Gclose(H5Group)
  H5Fclose(H5File)
  listDatastoreH5(envir=envir)
  TRUE
}


#INITIALIZE A DATASET IN A TABLE
#===============================
#' Initialize dataset in an HDF5 (H5) type datastore table.
#'
#' \code{initDatasetH5} a visioneval framework datastore connection function
#' that initializes a dataset in an HDF5 (H5) type datastore table.
#'
#' This function initializes a dataset in an HDF5 (H5) type datastore table.
#'
#' @param Spec_ls a list containing the standard module 'Set' specifications
#'   described in the model system design documentation.
#' @param Group a string representation of the name of the group the table is to
#' be created in.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if dataset is successfully initialized. If the dataset already
#' exists the function throws an error and writes an error message to the log.
#' Updates the model state file.
#' @export
#' @import rhdf5
initDatasetH5 <- function(Spec_ls, Group, envir=modelEnvironment()) {
  G <- getModelState(envir)
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)

  Table <- paste(Group, Spec_ls$TABLE, sep = "/")
  Name <- Spec_ls$NAME
  DatasetName <- paste(Table, Name, sep = "/")

  #Read SIZE specification or throw error if doesn't exist
  if (!is.null(Spec_ls$SIZE)) {
    Size <- Spec_ls$SIZE
  } else {
    Message <- paste0("SIZE specification for dataset (", Name,
                      ") is not present.")
    writeLog(Message,Level="Error")
    stop(Message)
  }
  #Create the dataset
  Length <- unlist(h5readAttributes(dsPath, Table))["LENGTH"]
  Chunk <- ifelse(Length > 1000, 100, 1)
  StorageMode <- Types()[[Spec_ls$TYPE]]$mode
  H5File <- H5Fopen(dsPath)
  h5createDataset(
    H5File, DatasetName, dims = Length,
    storage.mode = StorageMode, size = Size + 1,
    chunk = Chunk, level = 7
  )
  H5Data <- H5Dopen(H5File, DatasetName)
  h5writeAttribute(Spec_ls$MODULE, H5Data, "MODULE")
  h5writeAttribute(Spec_ls$NAVALUE, H5Data, "NAVALUE")
  h5writeAttribute(Spec_ls$UNITS, H5Data, "UNITS")
  h5writeAttribute(Size, H5Data, "SIZE")
  h5writeAttribute(Spec_ls$TYPE, H5Data, "TYPE")
  if (!is.null(Spec_ls$PROHIBIT)) {
    h5writeAttribute(Spec_ls$PROHIBIT, H5Data, "PROHIBIT")
  }
  if (!is.null(Spec_ls$ISELEMENTOF)) {
    h5writeAttribute(Spec_ls$ISELEMENTOF, H5Data, "ISELEMENTOF")
  }
  if (!is.null(Spec_ls$DESCRIPTION)) {
    h5writeAttribute(Spec_ls$DESCRIPTION, H5Data, "DESCRIPTION")
  }
  H5Dclose(H5Data)
  H5Fclose(H5File)
  #Update datastore inventory
  listDatastoreH5(envir=envir)
  TRUE
}


#READ FROM TABLE
#===============
#' Read from an HDF5 (H5) type datastore table.
#'
#' \code{readFromTableH5} a visioneval framework datastore connection function
#' that reads a dataset from an HDF5 (H5) type datastore table.
#'
#' This function reads a dataset from an HDF5 (H5) type datastore table.
#'
#' @param Name A string identifying the name of the dataset to be read from.
#' @param Table A string identifying the complete name of the table where the
#'   dataset is located.
#' @param Group a string representation of the name of the datastore group the
#' data is to be read from.
#' @param Index A numeric vector identifying the positions the data is to be
#'   written to. NULL if the entire dataset is to be read.
#' @param ReadAttr A logical identifying whether to return the attributes of
#' the stored dataset. The default value is TRUE.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return A vector of the same type stored in the datastore and specified in
#'   the TYPE attribute.
#' @export
#' @import rhdf5
readFromTableH5 <- function(Name, Table, Group, Index = NULL, ReadAttr = TRUE, envir=envir) {
  #Expects to find the Datastore in the working directory
  #Load the model state file
  G <- getModelState(envir)

  #If DstoreLoc is NULL get the name of the datastore from the model state
  # NOTE: if called from the framework dispatcher function, DstoreLoc will always be NULL
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)

  #Check that dataset exists to read from
  #Do NOT pass envir to checkDataset - we're looking just within this Datastore
  DatasetExists <- checkDataset(Name, Table, Group, G$Datastore)
  if (DatasetExists) {
    DatasetName <- file.path(Group, Table, Name)
  } else {
    Message <-
      paste("Dataset", Name, "in table", Table, "in group", Group, "doesn't exist.")
    stop(Message)
  }
  #If there is an Index, check that it is in bounds
  if (!is.null(Index)) {
    TableAttr_ <-
      unlist(G$Datastore$attributes[G$Datastore$groupname == file.path(Group, Table)])
    AllowedLength <- TableAttr_["LENGTH"]
    if (any(Index > AllowedLength)) {
      Message <-
        paste0(
          "One or more specified indicies for reading data from ",
          Table, " exceed ", AllowedLength
        )
      writeLog(Message,Level="error")
      stop(Message)
    }
  }
  #Read data
  if (is.null(Index)) {
    Data_ <- h5read(dsPath, DatasetName, read.attributes = ReadAttr)
  } else {
    Data_ <-
      h5read(dsPath, DatasetName, index = list(Index), read.attributes = ReadAttr)
  }
  #Convert NA values
  NAValue <- as.vector(attributes(Data_)$NAVALUE)
  Data_[Data_ == NAValue] <- NA
  #Return results
  if (ReadAttr) {
    #If single value array, convert to vector but preserve attributes
    if (all(dim(Data_) == 1)) dim(Data_) <- NULL
  } else {
    #Remove attributes
    Data_ <- as.vector(Data_)
  }
  return(Data_)
}

#WRITE TO TABLE
#==============
#' Write to an HDF5 (H5) type datastore table.
#'
#' \code{writeToTableH5} a visioneval framework datastore connection function
#' that writes data to an HDF5 type datastore table and initializes
#' dataset if needed.
#'
#' This function writes a dataset file to an hdf5 (H5) type datastore table. It
#' initializes the dataset if the dataset does not exist. Enables data to be
#' written to specific location indexes in the dataset.
#'
#' @param Data_ A vector of data to be written.
#' @param Spec_ls a list containing the standard module 'Set' specifications
#'   described in the model system design documentation.
#' @param Group a string representation of the name of the datastore group the
#' data is to be written to.
#' @param Index A numeric vector identifying the positions the data is to be
#'   written to.
#' @param envir An R environment with assigned Datastore functions and a ModelState_ls
#'   (see assignDatastoreFunctions)
#' @return TRUE if data is sucessfully written. Updates model state file.
#' @export
#' @import rhdf5
writeToTableH5 <- function(Data_, Spec_ls, Group, Index = NULL, envir=modelEnvironment()) {
  G <- getModelState(envir)
  dsPath <- file.path(G$ModelStatePath,G$DatastoreName)
  Name <- Spec_ls$NAME
  Table <- Spec_ls$TABLE

  #Check that dataset exists to write to and attempt to create if not
  #Do NOT pass envir to checkDataset - we're just looking in this Datastore
  DatasetExists <- checkDataset(Name, Table, Group, G$Datastore)
  if (!DatasetExists) {
    initDatasetH5(Spec_ls, Group)
  }
  #Write the dataset
  if (is.null(Data_)) {
    Message <-
      paste0(
        "writeToTable passed NULL Data_ "
      )
    writeLog(Message,Level="error")
    stop(Message)
  }
  Data_[is.na(Data_)] <- Spec_ls$NAVALUE
  DatasetName <- file.path(Group, Table, Name)
  if (is.null(Index)) {
    h5write(Data_, file = dsPath, name = DatasetName)
  } else {
    h5write(Data_, file = dsPath, name = DatasetName, index = list(Index))
  }
  #Update datastore inventory
  listDatastoreH5(envir=envir)
  TRUE
}

###############################################################################
#                                                                             #
#                 COMMON DATASTORE INTERACTION FUNCTIONS                      #
#                                                                             #
###############################################################################

# GET DATASTORE PATH FROM ModelState_ls
#======================================
#' Build and return ModelState_ls objects on virtual Datastore path
#'
#' TODO: Full description of \code{getModelStatePaths}
#'
#' @param dropFirst if TRUE, do not return first (current writable) Datastore 
#' @param envir environment containing the model state and cached path list
#' @return A list of ModelState_ls objects to search for DatasetName's
#' @export
getModelStatePaths <- function(dropFirst=TRUE,envir=modelEnvironment()) {
  if ( is.null(envir$ModelStateList) ) { # Check for ModelState cache from earlier stages
    paths <- envir$ModelState_ls$DatastorePath
    if ( dropFirst ) paths <- paths[-1]
    if ( length(paths) > 0 ) {
      writeLog(c("Datastore Paths:",paths),Level="trace")
      msList <- lapply(paths, # Create environments that describe the source Datastore/ModelState
        function(dstore) {
          path <- file.path(dstore,getModelStateFileName())
          if ( file.exists(path) ) {
            G.env <- new.env()
            loadModelState(FileName=path,envir=G.env)
            if ( length(G.env$ModelState_ls) > 0 ) {
              assignDatastoreFunctions(envir=G.env) # Each datastore can have a different type
            }
            return(G.env)
          } else return(NULL)
        }
      )
      if ( any(bad <- sapply(msList,is.null)) ) {
        stop( writeLog(
          c("Could not open ModelState for paths:",paths[bad]),
          Level="error"
        ))
      }
      envir$ModelStateList <- msList
    }
  } else {
    writeLog("Using cached DatastorePath/ModelStates",Level="trace")
  }

  return(envir$ModelStateList)
}

#LOCATE DATASET
#==============
#' Find virtual Datastore containing Dataset
#'
#' @param DatasetName Group/Table/Name string describing Dataset to locate
#' @param DstoreListing_df a dataframe which lists the contents of the datastore
#'   as contained in the model state file (ignored if envir is supplied)
#' @param envir Alternate source for ModelState_ls / Model State Paths
#' @return a DatastoreListing_df which contains the Group/Table/Name (NULL if not found)
#' @export
findDataset <- function(DatasetName, DstoreListing_df=NULL, envir=modelEnvironment()) {
  if ( is.null(DstoreListing_df) ) {
    G <- getModelState(envir=envir)
    msPaths <- getModelStatePaths(envir=envir) # Does not return current model state, might be zero length
    if ( length(msPaths) > 0 ) {
      basePath <- list(dsPath=G$Datastore)
      dsPaths <- lapply(msPaths,function(m) m$ModelState_ls$Datastore)
      dsPaths <- c(basePath,dsPaths) # Look into current model state first
    } else {
      dsPaths <- list(dsPath=G$Datastore)
    }
  } else {
    dsPaths <- list(dsPath=DstoreListing_df)
  }

  FoundIn_df <- NULL
  for ( ds in dsPaths ) {
    if (
        class(DatasetName)!= "character" ||
        class(ds$groupname)!="character" ||
        length(ds$groupname) == 0 || length(DatasetName)==0
      ) {
      writeLog("Failed to find path element; entering debug browser (Q to quit)",Level="error")
      browser()
    }
    if ( DatasetName %in% ds$groupname ) {
      FoundIn_df <- ds
      break
    }
  }
  if ( is.null(FoundIn_df) ) writeLog(paste("Could not find",DatasetName),Level="trace")
  return(FoundIn_df)
}

#CHECK DATASET EXISTENCE
#=======================
#' Check dataset existence
#'
#' \code{checkDataset} a visioneval framework control function that checks
#' whether a dataset exists in the datastore and returns a TRUE or FALSE value
#' with an attribute of the full path to where the dataset should be located in
#' the datastore.
#'
#' This function checks whether a dataset exists. The dataset is identified by
#' its name and the table and group names it is in. If the dataset is not in the
#' datastore listing, an error is thrown. If it is located in the datastore, the
#' full path name to the dataset is returned.
#'
#' @param Name a string identifying the dataset name.
#' @param Table a string identifying the table the dataset is a part of.
#' @param Group a string or numeric representation of the group the table is a
#' part of.
#' @param DstoreListing_df a dataframe which lists the contents of the datastore
#'   as contained in the model state file.
#' @param envir Alternate source for ModelState_ls / Model State Paths
#' @return A logical identifying whether the dataset is in the datastore. It has
#' an attribute that is a string of the full path to where the dataset should be
#' in the datastore.
#' @export
checkDataset <- function(Name, Table, Group, DstoreListing_df=NULL, envir=modelEnvironment()) {

  Name <- as.character(Name)
  Table <- as.character(Table)
  Group <- as.character(Group)
  DatasetName <- file.path(Group, Table, Name)

  DstoreListing_df <- findDataset(DatasetName, DstoreListing_df, envir)
  DatasetExists <- ! is.null(DstoreListing_df)
  attributes(DatasetExists) <- list(DatasetName=DatasetName,DstoreListing_df=DstoreListing_df)
  DatasetExists
}

#GET ATTRIBUTES OF A DATASET
#===========================
#' Get attributes of a dataset
#'
#' \code{getDatasetAttr} a visioneval framework control function that retrieves
#' the attributes for a dataset in the datastore.
#'
#' This function extracts the listed attributes for a specific dataset from the
#' datastore listing.
#'
#' @param Name a string identifying the dataset name.
#' @param Table a string identifying the table the dataset is a part of.
#' @param Group a string or numeric representation of the group the table is a
#' part of.
#' @param DstoreListing_df a dataframe which lists the contents of the datastore
#'   as contained in the model state file.
#' @param envir Alternate source for ModelState_ls / Model State Paths
#' @return A named list of the dataset attributes.
#' @export
getDatasetAttr <- function(Name=NULL, Table=NULL, Group=NULL, DstoreListing_df=NULL, envir=modelEnvironment()) {
  # Warning: should call checkDataset first if the missing Dataset error needs to
  #   be accepted.

  if ( missing(Table) || is.null(Table) || missing(Group) || is.null(Group) ) {
    stop(writeLog("getDatasetAttr: must provide Group and Table",Level="error"))
  }
  if ( missing(Name) || is.null(Name) ) {
    DatasetName <- file.path(Group, Table)
  } else {
    DatasetName <- file.path(Group, Table, Name)
  }
  DstoreListing_df <- findDataset(DatasetName, DstoreListing_df, envir)
  if ( !is.null(DstoreListing_df) ) {
    DatasetIdx <- which(DstoreListing_df$groupname == DatasetName)
    if ( length(DatasetIdx)>1 ) {
      stop(
        writeLog(
          Level="error",
          paste(
            "Dataset",DatasetName,"appears more than once in listing",
            paste(DatasetIdx,collapse=",")
          )
        )
      )
    }
    return(DstoreListing_df$attributes[[DatasetIdx]])
  } else {
    stop(
      writeLog(paste("datastore.R circa #1338: Dataset",DatasetName,"does not exist for attributes."),Level="error")
    )
  }
}


#CHECK WHETHER TABLE EXISTS
#==========================
#' Check whether table exists in the datastore
#'
#' \code{checkTableExistence} a visioneval framework control function that
#' checks whether a table is present in the datastore.
#'
#' This function checks whether a table is present in the datastore.
#'
#' @param Table a string identifying the table.
#' @param Group a string or numeric representation of the group the table is a
#' part of.
#' @param DstoreListing_df a dataframe which lists the contents of the datastore
#'   as contained in the model state file.
#' @param envir Alternate source for ModelState_ls / Model State Paths
#' @return A logical identifying whether a table is present in the datastore.
#' @export
checkTableExistence <- function(Table, Group, DstoreListing_df=NULL,envir=modelEnvironment()) {
  DatasetName <- file.path(Group, Table)
  DstoreListing_df <- findDataset(DatasetName, DstoreListing_df, envir)
  return ( ! is.null(DstoreListing_df) )
}

#CREATE A DATASTORE INDEX LIST
#=============================
#' Create a list of geographic indices for all tables in a datastore.
#'
#' \code{createGeoIndexList} a visioneval framework control function that
#' creates a list containing the geographic indices for tables in the operating
#' datastore for identified tables.
#'
#' This function takes a 'Get' or 'Set' specifications list for a module and the
#' 'RunBy' specification and returns a list which has a component for each table
#' identified in the specifications. Each component includes all geographic
#' datasets for the table.
#'
#' @param Specs_ls A 'Get' or 'Set' specifications list for a module.
#' @param RunBy The value of the RunBy specification for a module.
#' @param RunYear A string identifying the model year that is being run.
#' @param envir An environment from which to extract G / ModelState_ls
#' @return A list that contains a component for each table identified in the
#' specifications in which each component includes all the geographic datasets
#' for the table represented by the component.
#' @export
createGeoIndexList <- function(Specs_ls, RunBy, RunYear,envir=modelEnvironment()) {
  G <- getModelState(envir=envir)
  #Make data frame of all tables and groups
  TablesToIndex_df <-
  do.call(
    rbind,
    lapply(Specs_ls, function(x) {
      data.frame(Table = x$TABLE, Group = x$GROUP, stringsAsFactors = FALSE)
    })
  )
  #Replace Group name with appropriate year if necessary
  TablesToIndex_df$Group[TablesToIndex_df$Group == "Year"] <- RunYear
  TablesToIndex_df$Group[TablesToIndex_df$Group == "BaseYear"] <- G$BaseYear
  #Remove duplicates
  TablesToIndex_df <- unique(TablesToIndex_df)
  #Don't create and index for any global tables that are not geographic
  DoCreateIndex_ <-
  !(
    TablesToIndex_df$Group == "Global" &
    !(TablesToIndex_df$Table %in% c("Marea", "Azone", "Bzone", "Czone"))
  )
  TablesToIndex_df <- TablesToIndex_df[DoCreateIndex_,]
  #Turn data frame in list by group
  TablesToIndex_ls <- split(TablesToIndex_df$Table, TablesToIndex_df$Group)
  TablesToIndex_ls <- lapply(TablesToIndex_ls, function(x) {
    if (!(RunBy %in% x)) x <- c(RunBy, x)
    x
  })
  #Iterate through groups and tables and create indexes
  Index_ls <- list()
  for (nm in names(TablesToIndex_ls)) {
    Index_ls[[nm]] <- list()
    for(tab in TablesToIndex_ls[[nm]]) {
      Index_ls[[nm]][[tab]] <- list()
      if (!(tab %in% c("Marea", "Region"))) {
        Index_ls[[nm]][[tab]]$Marea <- readFromTable("Marea", tab, nm, envir=envir)
        Index_ls[[nm]][[tab]]$Azone <- readFromTable("Azone", tab, nm, envir=envir)
      } else {
        if (tab == "Marea") {
          Index_ls[[nm]]$Marea$Marea <- readFromTable("Marea", "Marea", nm, envir=envir)
        }
      }
      # if (tab == "Marea") {
      #   Index_ls[[nm]]$Marea$Marea <- readFromTable("Marea", "Marea", nm)
      # } else {
      #   Index_ls[[nm]][[tab]]$Marea <- readFromTable("Marea", tab, nm)
      #   Index_ls[[nm]][[tab]]$Azone <- readFromTable("Azone", tab, nm)
      # }
    }
  }
  Index_ls
}

#CREATE A DATASTORE INDEX
#========================
#' Create datastore index.
#'
#' \code{createIndex} a visioneval framework control function that creates an
#' index for reading or writing module data to the datastore.
#'
#' This function creates indexing functions which return an index to positions
#' in datasets that correspond to positions in an index field of a table. For
#' example if the index field is 'Azone' in the 'Household' table, this function
#' will return a function that when provided the name of a particular Azone,
#' will return the positions corresponding to that Azone.
#'
#' @param Table A string identifying the name of the table the index is being
#'   created for.
#' @param Group A string identifying the name of the group where the table is
#' located in the datastore.
#' @param RunBy A string identifying the level of geography the module is being
#'   run at (e.g. Azone).
#' @param Geo A string identifying the geographic unit to create the index for
#'   (e.g. the name of a particular Azone).
#' @param GeoIndex_ls a list of geographic indices used to determine the
#'   positions to extract from a dataset corresponding to the specified
#'   geography.
#' @return A function that creates a vector of positions corresponding to the
#'   location of the supplied value in the index field.
#' @export
createGeoIndex <- function(Table, Group, RunBy, Geo, GeoIndex_ls) {
  if (Table %in% c("Region", "Marea")) {
    if (Table == "Region") {
      Idx_ <- 1
    }
    if (Table == "Marea") {
      #Identify the Marea from the 'RunBy' table
      GeoIdxNames_ <- GeoIndex_ls[[Group]][[RunBy]][[RunBy]]
      Marea <-
        GeoIndex_ls[[Group]][[RunBy]]$Marea[GeoIdxNames_ == Geo]
      GeoIdxNames_ <- GeoIndex_ls[[Group]]$Marea$Marea
      Idx_ <- which(GeoIdxNames_ == Marea)
    }
  } else {
    #Get the index from the table
    GeoIdxNames_ <- GeoIndex_ls[[Group]][[Table]][[RunBy]]
    Idx_ <- which(GeoIdxNames_ == Geo)
  }
  Idx_
}

#GET DATA SETS IDENTIFIED IN MODULE SPECIFICATIONS FROM DATASTORE
#================================================================
#' Retrieve data identified in 'Get' specifications from datastore
#'
#' \code{getFromDatastore} a visioneval framework control function that
#' retrieves datasets identified in a module's 'Get' specifications from the
#' datastore.
#'
#' This function retrieves from the datastore all of the data sets identified in
#' a module's 'Get' specifications. If the module's specifications include the
#' name of a geographic area, then the function will retrieve the data for that
#' geographic area.
#'
#' @param ModuleSpec_ls a list of module specifications that is consistent with
#' the VisionEval requirements
#' @param RunYear a string identifying the model year being run. The default is
#' the Year object in the global workspace.
#' @param Geo a string identifying the name of the geographic area to get the
#' data for. For example, if the module is specified to be run by Azone, then
#' Geo would be the name of a particular Azone.
#' @param GeoIndex_ls a list of geographic indices used to determine the
#' positions to extract from a dataset corresponding to the specified geography.
#' @param envir An environment from which to extract G / ModelState_ls
#' @return A list containing all the data sets specified in the module's
#' 'Get' specifications for the identified geographic area.
#' @export
getFromDatastore <- function(ModuleSpec_ls, RunYear, Geo = NULL, GeoIndex_ls = NULL,envir=modelEnvironment()) {
  GetSpec_ls <- ModuleSpec_ls$Get
  #Make a list to hold the retrieved data
  L <- initDataList()
  #Add the model state and year to the list
  G <- getModelState(envir=envir)
  G$Year <- RunYear
  L$G <- G
  #Get data specified in list
  for (i in 1:length(GetSpec_ls)) {
    Spec_ls <- GetSpec_ls[[i]]
    Group <- Spec_ls$GROUP
    Table <- Spec_ls$TABLE
    Name <- Spec_ls$NAME
    Type <- Spec_ls$TYPE
    #Identify datastore files and groups to get data from
    if (Group == "Global") {
      DstoreGroup <- "Global"
    }
    if (Group == "BaseYear") {
      DstoreGroup <- G$BaseYear;
    }
    if (Group == "Year") {
      DstoreGroup <- RunYear;
    }
    #Add table component to list if does not exist
    if (is.null(L[[Group]][[Table]])) {
      L[[Group]][[Table]] <- list()
      Length <- getDatasetAttr(Name=NULL, Table, DstoreGroup, envir=envir)$LENGTH
      if ( ! is.numeric(Length) ) {
        msg <- writeLog(paste("getFromDatastore: Table length not available for",file.path(Group,Table)),Level="error")
        stop(msg)
      }
      attributes(L[[Group]][[Table]]) <- list(LENGTH = Length)
    }
    #Make an index to the data
    DoCreateIndex <- (
      !is.null(Geo) &&
      !(Group == "Global" && !(Table %in% c("Marea", "Azone", "Bzone", "Czone")))
    )
    if (DoCreateIndex) {
      Index <-
      createGeoIndex(Table, DstoreGroup, ModuleSpec_ls$RunBy, Geo, GeoIndex_ls)
    } else {
      Index <- NULL
    }

    # Pass envir and search DatastorePath if present
    Data_ <- readFromTable(Name, Table, DstoreGroup, Index,envir=envir)
    if (any(!is.na(Data_))) {
      #Convert currency
      if (Type == "currency") {
        FromYear <- G$BaseYear
        ToYear <- Spec_ls$YEAR
        if (FromYear != ToYear) {
          Data_ <- deflateCurrency(Data_, FromYear, ToYear)
        }
      }
      #Convert units
      #  when getting the target units: BaseYear may not be present, for example.
      SimpleTypes_ <- c("integer", "double", "character", "logical")
      ComplexTypes_ <- names(Types())[!(names(Types()) %in% SimpleTypes_)]
      if (Type %in% ComplexTypes_) {
        AttrGroup <- switch(
          Group,
          Year = RunYear,
          BaseYear = G$BaseYear,
          Global = "Global"
        )
        writeLog(
          paste(
            "Converting units for",file.path(DstoreGroup,Table,Name)
          ), Level="trace"
        )
        # Note: Pass envir rather than G$Datastore to getDatasetAttr because the target
        #  Dataset may exist on virtual Datastore Path rooted at G (current ModelState_ls)
        Conversion_ls <- convertUnits(
          Data_, Type,
          getDatasetAttr(Name, Table, AttrGroup, envir=envir)$UNITS,
          Spec_ls$UNITS
        )
        Data_ <- Conversion_ls$Values
      }
      #Convert magnitude
      Data_ <- convertMagnitude(Data_, 1, Spec_ls$MULTIPLIER)
    }
    if ( is.null(attributes(Data_)) ) {
      writeLog(paste("Does not exist:",file.path(DstoreGroup,Table,Name)),Level="info")
    } else {
      #Add data to list
      L[[Group]][[Table]][[Name]] <- Data_
    }
  }
  #Return the list
  return( L )
}

#SAVE DATA SETS RETURNED BY A MODULE IN THE DATASTORE
#====================================================
#' Save the data sets returned by a module in the datastore
#'
#' \code{setInDatastore} a visioneval framework control function saves to the
#' datastore the data returned in a standard list by a module.
#'
#' This function saves to the datastore the data sets identified in a module's
#' 'Set' specifications and included in the list returned by the module. If a
#' particular geographic area is identified, the data are saved to the positions
#' in the data sets in the datastore corresponding to the identified geographic
#' area.
#'
#' @param Data_ls a list containing the data to be saved. The list is organized
#' by group, table, and data set.
#' @param ModuleSpec_ls a list of module specifications that is consistent with
#' the VisionEval requirements
#' @param ModuleName a string identifying the name of the module (used to document
#' the module creating the data in the datastore)
#' @param GeoIndex_ls a list of geographic indices used to determine the
#' positions to extract from a dataset corresponding to the specified geography.
#' @param Year a string identifying the model run year
#' @param Geo a string identifying the name of the geographic area to get the
#' data for. For example, if the module is specified to be run by Azone, then
#' Geo would be the name of a particular Azone.
#' @param envir An environment from which to extract G / ModelState_ls
#' @return A logical value which is TRUE if the data are successfully saved to
#' the datastore.
#' @export
setInDatastore <- function(Data_ls, ModuleSpec_ls, ModuleName, Year, Geo = NULL, GeoIndex_ls = NULL, envir=modelEnvironment()) {
  # Note: since we're writing, we'll look directly at G$Datastore rather than environment in
  # functions like checkTableExistence... DatastorePath is only used when reading...
  
  #Get the model state
  G <- getModelState(envir=envir)
  BaseYear <- G$BaseYear

  # Make any specified tables
  # This strategy saves work in a single-stage model but won't work for a multi-stage
  #   model where tables may have been created in an earlier stage (when we first saw
  #   NewSetTable) but have not yet been created in the new stage.
  if (!is.null(ModuleSpec_ls$NewSetTable)) {
    # First time table creation
    TableSpec_ls <- ModuleSpec_ls$NewSetTable
    for (i in 1:length(TableSpec_ls)) {
      Table <- TableSpec_ls[[i]]$TABLE
      Group <- TableSpec_ls[[i]]$GROUP
      if (Group == "Global") DstoreGroup <- "Global"
      if (Group == "Year") DstoreGroup <- Year
      if (! checkTableExistence(Table, DstoreGroup, G$Datastore) ) {
        Length <- attributes(Data_ls[[Group]][[Table]])$LENGTH
        initTable(Table, DstoreGroup, Length, envir=envir) # DO pass envir here (place to write)
      }
    }
  }

  #Process module Set specifications
  SetSpec_ls <- ModuleSpec_ls$Set
  for (Spec_ls in SetSpec_ls) {
    Spec_ls$MODULE <- ModuleName
    Group <- Spec_ls$GROUP
    if (Group == "Global") DstoreGroup <- "Global"
    if (Group == "Year") DstoreGroup <- Year
    Table <- Spec_ls$TABLE
    Name <- Spec_ls$NAME
    Type <- Spec_ls$TYPE
    # Table has not been created in current stage - get table length from earlier stage
    if ( ! checkTableExistence( Table, DstoreGroup, getModelState(envir=envir)$Datastore ) ) {
      Length <- getDatasetAttr( Name=NULL, Table, DstoreGroup, envir=envir )$LENGTH
      if ( ! is.numeric(Length) ) { # Table is not created anywhere up the DatastorePath
        msg <- writeLog(paste(file.path(DstoreGroup,Table),"has not been created in any model stage"),Level="error")
        stop(msg)
      }
      initTable(Table, DstoreGroup, Length, envir=envir) # DO pass envir here (place to write)
    }

    #Identify datastore save location from specifications
    #Make an index to the data
    DoCreateIndex <- (
      ! is.null(Geo) &&
      ! (
        Group == "Global" &&
        ! (Table %in% c("Marea", "Azone", "Bzone", "Czone"))
      )
    )
    if (DoCreateIndex) {
      Index <- createGeoIndex(Table, DstoreGroup, ModuleSpec_ls$RunBy, Geo, GeoIndex_ls)
    } else {
      Index <- NULL
    }
    #Transform and save the data
    Data_ <- Data_ls[[Group]][[Table]][[Name]]
    if (!is.null(Data_)) {
      #Convert currency
      if (Type == "currency") {
        FromYear <- Spec_ls$YEAR
        ToYear <- BaseYear
        if (FromYear != ToYear) {
          Data_ <- deflateCurrency(Data_, FromYear, ToYear)
          rm(FromYear, ToYear)
        }
      }
      #Convert units
      SimpleTypes_ <- c("integer", "double", "character", "logical")
      ComplexTypes_ <- names(Types())[!(names(Types()) %in% SimpleTypes_)]
      if (Type %in% ComplexTypes_) {
        FromUnits <- Spec_ls$UNITS
        Conversion_ls <- convertUnits(Data_, Type, FromUnits)
        Data_ <- Conversion_ls$Values
        #Change units specification to reflect default datastore units
        Spec_ls$UNITS <- Conversion_ls$ToUnits
        rm(FromUnits, Conversion_ls)
      }
      rm(SimpleTypes_, ComplexTypes_)
      #Convert magnitude
      Data_ <- convertMagnitude(Data_, Spec_ls$MULTIPLIER, 1)
    } else {
      Message <-
      paste0(
        "setInDatastore got NULL Data_ with arguments Group: ",
        Group, ", Table: ", Table, ", Name: ", Name
      )
      writeLog(Message,Level="error")
      stop(Message)
    }
    if (!is.null(attributes(Data_)$SIZE)) {
      Spec_ls$SIZE <- attributes(Data_)$SIZE
    }
    # Do pass envir here: write to first element of DatastorePath
    writeToTable(Data_, Spec_ls, DstoreGroup, Index, envir=envir)
  }
  TRUE
}

#WRITE PROCESSED INPUTS TO DATASTORE
#===================================
#' Write the datasets in a list of module inputs that have been processed to the
#' datastore.
#'
#' \code{inputsToDatastore} a visioneval framework control function that takes a
#' list of processed module input files and writes the datasets to the
#' datastore.
#'
#' This function takes a processed list of input datasets specified by a module
#' created by the application of the 'processModuleInputs' function and writes
#' the datasets in the list to the datastore.
#'
#' @param Inputs_ls a list processes module inputs as created by the
#' 'processModuleInputs' function.
#' @param ModuleSpec_ls a list of module specifications that is consistent with
#' the VisionEval requirements.
#' @param ModuleName a string identifying the name of the module (used to
#' document the dataset in the datastore).
#' @param envir An environment from which to extract G / ModelState_ls
#' @return A logical indicating successful completion. Most of the outputs of
#' the function are the side effects of writing data to the datastore.
#' @export
inputsToDatastore <- function(Inputs_ls, ModuleSpec_ls, ModuleName, envir=modelEnvironment()) {
  G <- getModelState(envir=envir)
  #Make sure the inputs are error free
  if (length(Inputs_ls$Errors) != 0) {
    Msg <-
    paste0(
      "Unable to write module inputs for module '", ModuleName, "'. ",
      "There are one or more errors in the inputs or input specifications."
    )
    stop(Msg)
  }
  #Set up processing
  Errors_ <- character(0)
  Data_ls <- Inputs_ls$Data
  InpSpec_ls <- processModuleSpecs(ModuleSpec_ls)$Inp
  #Set up new tables
  if (!is.null(ModuleSpec_ls$NewInpTable)) {
    TableSpec_ls <- ModuleSpec_ls$NewInpTable
    for (i in 1:length(TableSpec_ls)) {
      Table <- TableSpec_ls[[i]]$TABLE
      Group <- TableSpec_ls[[i]]$GROUP
      if (Group != "Global") {
        Msg <-
        paste0(
          "NewInpTable specification error for module '", ModuleName, "'. ",
          "New input tables can only be made in the 'Global' group. "
        )
        Errors_ <- c(Errors_, Msg)
      }
      Length <- length(Data_ls[[Group]][[Table]][[1]])
      initTable(Table, Group, Length, envir=envir)
    }
  }
  #Write Global group tables to datastore
  if (length(Data_ls[["Global"]]) > 0) {
    for (Table in names(Data_ls[["Global"]])) {
      if (Table %in% c("Azone", "Bzone", "Czone", "Marea")) {
        Data_df <-
        data.frame(Data_ls[["Global"]][[Table]], stringsAsFactors = FALSE)
        Units_ls <- lapply(Data_df, function(x) unname(attributes(x)$UNITS))
        SortData_df <- sortGeoTable(Data_df, Table, "Global", envir=envir)
        FieldsToSave_ <-
        names(SortData_df)[!(names(SortData_df) %in% "Geo")]
        for (Name in FieldsToSave_) {
          Spec_ls <- findSpec(InpSpec_ls, Name, Table, "Global")
          Spec_ls$MODULE <- ModuleName
          #Modify units spec to reflect units consistent with defaults for
          #datastore
          Spec_ls$UNITS <- Units_ls[[Name]]
          writeToTable(SortData_df[[Name]], Spec_ls, "Global",envir=envir)
          rm(Spec_ls)
        }
        rm(SortData_df, FieldsToSave_, Data_df, Units_ls)
      } else {
        for (Name in names(Data_ls[["Global"]][[Table]])) {
          Data_ <- Data_ls[["Global"]][[Table]][[Name]]
          Spec_ls <- findSpec(InpSpec_ls, Name, Table, "Global")
          Spec_ls$MODULE <- ModuleName
          #Modify units spec to reflect units consistent with defaults for
          #datastore
          Spec_ls$UNITS <- attributes(Data_)$UNITS
          writeToTable(Data_, Spec_ls, "Global",envir=envir)
        }
      }
    }
  }
  #Write BaseYear group tables to datastore
  if (length(Data_ls[["BaseYear"]]) > 0) {
    for (Table in names(Data_ls[["BaseYear"]])) {
      Data_df <-
      data.frame(Data_ls[["BaseYear"]][[Table]], stringsAsFactors = FALSE)
      Units_ls <- lapply(Data_df, function(x) unname(attributes(x)$UNITS))
      Year <- G$BaseYear
      SortData_df <- sortGeoTable(Data_df, Table, Year, envir=envir)
      FieldsToSave_ <-
      names(SortData_df)[!(names(SortData_df) %in% "Geo")]
      for (Name in FieldsToSave_) {
        Spec_ls <- findSpec(InpSpec_ls, Name, Table, "BaseYear")
        Spec_ls$MODULE <- ModuleName
        #Modify units spec to reflect units consistent with defaults for
        #datastore
        Spec_ls$UNITS <- Units_ls[[Name]]
        writeToTable(SortData_df[[Name]], Spec_ls, Year,envir=envir)
        rm(Spec_ls)
      }
      rm(Year, SortData_df, FieldsToSave_, Data_df, Units_ls)
    }
  }
  #Write Year group tables to datastore
  if (length(Data_ls[["Year"]]) > 0) {
    for (Table in names(Data_ls[["Year"]])) {
      Data_df <-
      data.frame(Data_ls[["Year"]][[Table]], stringsAsFactors = FALSE)
      Units_ls <- lapply(Data_df, function(x) unname(attributes(x)$UNITS))
      for (Year in unique(as.character(Data_df$Year))) {
        YrData_df <- Data_df[Data_df$Year == Year,]
        if (Table != "Region") {
          SortData_df <- sortGeoTable(YrData_df, Table, Year, envir=envir)
        } else {
          SortData_df <- YrData_df
        }
        FieldsToSave_ <-
        names(SortData_df)[!(names(SortData_df) %in% c("Year", "Geo"))]
        for (Name in FieldsToSave_) {
          Spec_ls <- findSpec(InpSpec_ls, Name, Table, "Year")
          Spec_ls$MODULE <- ModuleName
          #Modify units spec to reflect units consistent with defaults for
          #datastore
          Spec_ls$UNITS <- Units_ls[[Name]]
          writeToTable(SortData_df[[Name]], Spec_ls, Year,envir=envir)
          rm(Spec_ls)
        }
        rm(YrData_df, SortData_df, FieldsToSave_)
      }
      rm(Data_df)
    }
  }
  TRUE
}

#MERGE TWO DATASTORE LISTINGS
#' Merge two Datastore listings
#'
#' \code{mergeDatastoreListings} a visioneval framework control function that combines
#' multiple Datastore listings (e.g. from different places in a DatastorePath)
#'
#' @param baseListing The datastore listing to be augmented
#' @param addListing The listing with elements that will override or augment the base
#' @return a new datastore listing with updated elements
#' @export
mergeDatastoreListings <- function(baseListing, addListing) {
  # The structure of the Datastore listing (ModelState_ls$Datastore) is as follows:
  #   group            # character vector
  #   name             # character vector
  #   groupname        # character vector
  #   attributes       # list
  # The listing is presented as either a data.frame or a list; we can access the elements
  #   the same way just by using baseListing$group etc.
  if ( ! is.list(baseListing) || !is.list(newListing) ) {
    stop(
      writeLog("Invalid listing types presented to mergeDatastoreListings",Level="error")
    )
  }
  asDF <- is.data.frame(baseListing) # if not a data.frame, it should be a list
  newListing <- as.list(baseListing) # drop data.frame class
  newItems <- which( ! addListing$groupname %in% baseListing$groupname ) # indexes into newListing
  newListing$group <- c(baseListing$group,addListing$group[newItems])
  newListing$name <- c(baseListing$name,addListing$name[newItems])
  newListing$groupname <- c(baseListing$groupname,addListing$groupname[newItems])
  newListing$attributes <- c(baseListing$attributes,addListing$attributes[newItems])

  if ( asDF ) newListing <- as.data.frame(newListing)
  return(newListing)
}

#COPY A DATASTORE
#================
#' Copy a Datastore and ModelState_ls from current directory to another location
#'
#' \code{copyDatastore} a visioneval framework datastore function that copies the
#' Datastore associated with a ModelState_ls to another location.
#'
#' Use cases for this functionn are: (1) standalone copying of a Datastore (copies associated
#'   ModelState and updates Datastore listing); (2) archiveDatastore (copies ModelState for item
#'   being archived); (3) loadDatastore / LoadModel (uses ModelState whose Datastore is being
#''  updated).
#'
#' This function supports "flattening" the Datastore and also changing the Datastore type (from RD '
#' to H5 or vice versa). Linked and Loaded Datastores in LoadModel or prior model stages must be ' the
#' same DatastoreType, so type conversion helps load results from a run using one DatastoreType to ' a
#' child model with a different DatastoreType.
#'
#' Important: the working directory must contain the ModelState.Rda and Datastore that the function
#' will copy (can't work currently from a different directory than that).
#'
#' 'Flattening' the Datastore will convert a linked Datastore into a copy in which all the Datasets
#' are realized in the targetDatastore. That supports loading the Datastore from another model, or
#' preparing a copy of a Datastore for transmission as "bare results."
#'
#' Note that this function is used to perform LoadDatastore (forcing flattening) when linking
#' models by copyign a pre-existing Datastore. It is NOT used by archiveDatastore, which always
#' just makes a file system snapshot of the stages in ResultsDir.
#' 
#' @param ToDir a file path in which to create the Datastore copy (named
#'   ModelState_ls$DatastoreName)
#' @param envir Environment containing ModelState_ls (and implicitly, a Datastore)
#' @param Flatten a logical indicating whether to merge all Datasets from the DatastorePath
#'   (default is TRUE). If Flatten is c(TRUE,TRUE), force use of Flatten machinery even if not
#'   necessary.
#' @param DatastoreType is one of "RD" or "H5". If it is not the same as what is in the source
#'   ModelState, convert the Datastore by flattening it into the requested DatastoreType.
#' @return A logical indicating successful completion.
#' @export
copyDatastore <- function( ToDir, Flatten=TRUE, DatastoreType=NULL, envir=modelEnvironment() ) {

  if ( ! dir.exists(ToDir) ) {
    Msg <- c("Target directory does not exist for copyDatastore:",ToDir)
    Msg <- writeLog(Msg,Level="error")
    stop(Msg)
  }

  ModelState_ls <- getModelState(envir)
  
  if ( is.null(DatastoreType) ) DatastoreType <- ModelState_ls$DatastoreType
  convertDatastoreType <- DatastoreType != ModelState_ls$DatastoreType

  AllowedDstoreTypes_ <- getAllowedDstoreTypes()
  if ( ! DatastoreType %in% AllowedDstoreTypes_ ) {
    Msg <- paste0("Unknown 'DatastoreType': ", DatastoreType)
    writeLog(c(Msg,"\nRecognized Types:",paste(AllowedDstoreTypes_, collapse = ", ")),Level="error")
    stop(Msg)
   }

  paths <- character(0)
  success <- FALSE
  Flatten <- Flatten[1] && ( length(ModelState_ls$DatastorePath) > 1 || isTRUE(Flatten[2]) )
  # Always use Flatten machinery if source model is internally staged, or if Flatten is c(TRUE,TRUE)

  # Don't flatten if already flat
  if ( ! Flatten ) {
    if ( ! convertDatastoreType ) {
      success <- file.copy(file.path(ModelState_ls$ModelStatePath,ModelState_ls$DatastoreName),ToDir,recursive=TRUE,copy.date=TRUE)
      writeLog(paste0("Copying Flat Datastore ",ifelse(success,"Succeeded","Failed"),"."),Level="warn")
    }
    if ( ! success ) {
      paths <- ModelState_ls$DatastorePath[1] # Only copy proximate Datastore (why this would work but not file.copy is mysteriaus...)
    }
  } else {
    paths <- rev(ModelState_ls$DatastorePath) # Copy all the elements from back up the path (overlay onto the oldest path element)
  }

  if ( length(paths) > 0 ) { # if we did file.copy above, length(paths) will be zero - already done
    writeLog("Flattening and/or Converting Datastore takes time...",Level="warn")

    # Construct target ModelState_ls:
    #   Same as ModelState being copied, but ModelStatePath is updated and Datastore listing removed
    toModelState_ls <- ModelState_ls[ ! names(ModelState_ls) %in% "Datastore" ]
    toModelState_ls$ModelStatePath <- ToDir
    toModelState_ls$DatastoreType <- DatastoreType # May differ from ModelState_ls$DatastoreType if converting

    # Set up model state environment for writing the target Datastore
    writeDS <- new.env()
    writeDS$ModelState_ls <- toModelState_ls
    assignDatastoreFunctions(envir=writeDS)
    
    # Create the required basic elements: Datastore structure and geography from the source
    # Do this rather than copying to ensure that the DatastoreListing is correct
    initDatastore(envir=writeDS)          # Create core datastore structure
    initDatastoreGeography(envir=writeDS) # Create basic geography tables

    for ( path in paths ) {
      writeLog(paste("Copying Datastore from path:",path),Level="info")

      # Open ModelState$Datastore from the source path
      readDS <- new.env()
      ms <- readModelState( FileName=file.path(path,getModelStateFileName()), envir=readDS )
      assignDatastoreFunctions(envir=readDS)
      ds <- ms$Datastore

      # Make sure all the groups area present and accounted for
      gtn <- strsplit(ds$groupname,"/") # May need to revise if groupname starts with /
      groups <- unlist(gtn[which(sapply(gtn,length)==1)])
      groupNotInDatastore <- ! groups %in% writeDS$ModelState_ls$Datastore$groupname
      if ( any( groupNotInDatastore  ) ) { # in target?
        AppendGroups <- groups[ groupNotInDatastore ]
        initDatastore(AppendGroups=AppendGroups,envir=writeDS)
      }

      # Copy the datasets
      indices <- which(sapply(gtn,length,simplify=TRUE)==3) # Get Dataset entries
      for ( i in indices ) {
        writeLog(paste("Reading Dataset:",ds$groupname[i]),Level="info")
        item <- gtn[[i]]
        names(item) <- c("Group","Table","Name")
        Attr_ <- ds$attributes[[i]]

        # Read from source Datastore
        dataset <- readFromTable(item["Name"],item["Table"],item["Group"],envir=readDS)

        # Check that table exists in target and create if necessary
        TableName <- file.path(item["Group"], item["Table"])
        if ( ! TableName %in% writeDS$ModelState_ls$Datastore$groupname ) { # in target?
          TableEntry <- which(ds$groupname == TableName)[1]                 # check source
          Length <- ds$attributes[[TableEntry]]$LENGTH                      # Get Length parameter
          if ( is.null(Length) ) {
            writeLog("datastore.R circa #2054: Table LENGTH attribute is NULL",Level="error")
            writeLog("enter debug browser (Q to quit)",Level="error")
            browser()
          }
          initTable(item["Table"], item["Group"], Length, envir=writeDS)    # Initialize the Table
        }

        # Write to target Datastore
        writeLog(paste("Writing dataset to Group",item["Group"]),Level="info")
        writeLog(paste("Spec:",paste(names(attributes(dataset)),collapse=",")),Level="info")
        writeToTable(dataset,attributes(dataset),item["Group"],envir=writeDS)
      }
    }
    success <- TRUE
    setModelState(Save=TRUE,envir=writeDS) # Push out the corresponding model state
  }
  return(success)
}
