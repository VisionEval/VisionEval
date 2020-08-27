#===========
#datastore.R
#===========

#Functions for querying a datastore. See framework/docs/QueryDocs.R

utils::globalVariables("ModelState_ls")

#=============================
#PREPARE FOR A DATASTORE QUERY
#=============================
#' Retrieve information needed to prepare a datastore query.
#'
#' \code{prepareForDatastoreQuery} a visioneval framework query user function that
#' retrieves datastore listings and functions required to make a datastore query.
#'
#' This function prepares a list of information that is used in making a query
#' of a VisionEval model datastore. The list includes the location(s) of the
#' datastore(s) to be queried, the listing(s) for those datastores, and the
#' functions to be used for reading the datastore(s). More than one datastore
#' may be specified so that if datastore references are used in a model run,
#' datasets from the referenced datastores may be queried as well. Note that the
#' capability for querying multiple datastores is only for the purpose of
#' querying datastores for a single model scenario. This capability should not
#' be used to compare multiple scenarios. The function does not segregate
#' datasets by datastore. Attempting to use this function to compare multiple
#' scenarios could produce unpredictable results.
#'
#' @param DstoreLocs_ a string vector identifying the paths to all of the
#' datastores to extract the datasets from. Each entry must be the full relative
#' path to a datastore (e.g. 'tests/Datastore').
#' @param DstoreType a string identifying the type of datastore
#' (e.g. 'RD', 'H5').
#' @return A named list having three components. The 'Dir' component is a
#' string vector identifying the relative path(s) to the datastore(s). The
#' 'Listing' component is a list where each component is the ModelState for the corresponding
#' element in 'Dir'. The 'Functions' component contains dispatch
#' functions for the datastore type for listing the datastore contents and for reading datasets.
#' @export
prepareForDatastoreQuery <- function(DstoreLocs_, DstoreType) {
  #Initialize list to hold query preparation information
  Prep_ls <- list()
  #Check that DstoreTypes are supported
  AllowedDstoreTypes_ <- c("RD", "H5")
  if (!DstoreType %in% AllowedDstoreTypes_) {
    Msg <-
      paste0("Specified 'DatastoreType' - ",
             DstoreType, " - is not a recognized type. ",
             "Recognized datastore types are: ",
             paste(AllowedDstoreTypes_, collapse = ", "), ".")
    stop(Msg,call.=FALSE)
  }
  #Check that DstoreLocs_ are correct and assign
  DstoreLocsExist_ <- sapply(DstoreLocs_, function(x) file.exists(x))
  if (any(!DstoreLocsExist_)) {
    Msg <-
      paste0("One or more of the specified DstoreLocs_ can not be found. ",
             "Maybe they are misspecified. Check the following: ",
             DstoreLocs_[!DstoreLocsExist_])
    stop(Msg,call.=FALSE)
  } else {
    Prep_ls$Dir <- DstoreLocs_
  }
  #Assign datastore reading functions
  Prep_ls$Functions <- list()
  DstoreFuncs_ <- c("readFromTable", "listDatastore")
  for(DstoreFunc in DstoreFuncs_) {
    Prep_ls$Functions[[DstoreFunc]] <- get(paste0(DstoreFunc, DstoreType))
  }
  #Get listing for each datastore
  Prep_ls$Listing <- lapply(DstoreLocs_, function(x) {
    return( readModelState(FileName = file.path(dirname(x),"ModelState.Rda")) )
  })
  names(Prep_ls$Listing) <- DstoreLocs_

  return( Prep_ls )
}
# #Example
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )

#LIST GROUPS
#===========
#' Lists the names of groups in model datastores.
#'
#' \code{listGroups} a visioneval framework query user function which lists the groups in a
#' datastore or datastores that contain data for a scenario.
#'
#' This function lists the names of groups in a model datastore and any other
#' datastores that are referenced by a model run.
#'
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return A named list where each component is a vector of group names for a
#' datastore.
#' @export
listGroups <- function(QueryPrep_ls) {
  lapply(QueryPrep_ls$Listing, function(x) {
    DstoreListing_df <- x$Datastore
    Groups_ <- unique(DstoreListing_df$group)
    Groups_ls <- strsplit(Groups_, "/")
    Groups_ <- unique(unlist(lapply(Groups_ls, function(x) x[2])))
    Groups_[!is.na(Groups_)]
  })
}
# #Example
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )
# listGroups(QPrep_ls)

#LIST TABLES IN GROUP
#====================
#' List names of tables in a group in a datastore.
#'
#' \code{listTables} a visioneval framework query user function which lists the tables in a
#' group in a datastore.
#'
#' This functions lists the tables in a group in a datastore.
#'
#' @param Group a string that is the name of the group to retrieve the table
#' names from.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return A named list where each component is a vector of table names in the
#' group in the datastore(s).
#' @export
listTables <- function(Group, QueryPrep_ls) {
  lapply(QueryPrep_ls$Listing, function(x) {
    DstoreListing_df <- x$Datastore
    Tables_ <- unique(DstoreListing_df$group)
    Tables_ <- Tables_[grep(Group, Tables_)]
    Tables_ls <- strsplit(Tables_, "/")
    Tables_ <- unique(unlist(lapply(Tables_ls, function(x) x[3])))
    Tables_[!is.na(Tables_)]
  })
}
# #Example
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )
# Grp <- listGroups(QPrep_ls)$Datastore
# for (grp in Grp) {
#   print(listTables(grp, QPrep_ls))
# }

#LIST DATASETS IN TABLE
#======================
#' List names and descriptions datasets in a table in a datastore.
#'
#' \code{listTables} a visioneval framework query user function which lists the names and
#' descriptions datasets in a table in a datastore.
#'
#' This functions lists the names and descriptions datasets in a table in a
#' datastore.
#'
#' @param Group a string that is the name of the group to retrieve the table
#' datasets from.
#' @param Table a string that is the name of the table to retrieve the dataset
#' names and descriptions from.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return A data frame which lists the dataset names and descriptions for
#' datasets in the identified table.
#' @export
listDatasets <- function(Table, Group, QueryPrep_ls) {
  lapply(QueryPrep_ls$Listing, function(x) {
    DstoreListing_df <- x$Datastore
    TableRef <- paste0("/", Group, "/", Table)
    DstoreListing_df <-
      DstoreListing_df[DstoreListing_df$group == TableRef, c("name", "attributes")]
    DstoreListing_df$Name <- DstoreListing_df$name
    DstoreListing_df$name <- NULL
    DstoreListing_df$Type <- unlist(lapply(DstoreListing_df$attributes, function(x) {
      Type <- x$TYPE
      if (is.null(Type)) Type <- ""
      Type
    }))
    DstoreListing_df$Units <- unlist(lapply(DstoreListing_df$attributes, function(x) {
      Units <- x$UNITS
      if (is.null(Units)) Units <- ""
      Units
    }))
    DstoreListing_df$Description <- unlist(lapply(DstoreListing_df$attributes, function(x) {
      Description <- x$DESCRIPTION
      if (is.null(Description)) Description <- ""
      Description
      }))
    DstoreListing_df$attributes <- NULL
    DstoreListing_df
  })
}
# #Example
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )
# listDatasets("Household", "2010", QPrep_ls)

#CREATE DOCUMENTATION OF DATASETS IN A DATASTORE
#===============================================
#' Save an zip archive which documents all tables/datasets in a datastore.
#'
#' \code{documentDatastoreTables} a visioneval framework query user function that saves a zip
#' archive of a set of csv-formatted text files which document tables in a datastore.
#'
#' This function inventories all datsets in the tables in a datastore and
#' creates a set of csv-formatted text files where the dataset inventory for
#' each table is in a csv-formatted text file having the name of the table.
#' Each csv file lists the names of the datasets, their types, their units, and
#' descriptions. The files are saved in a zip archive organized by datastore
#' group.
#'
#' @param SaveArchiveName a string identifying the name of the zip archive file
#' that will be saved. The name should not include any suffix (e.g. '.zip') as
#' that will be automatically added. The file will be saved in the current
#' working directory.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return a logical identifying whether the archive file has been saved.
#' @export
#' @import filesstrings
documentDatastoreTables <- function(SaveArchiveName, QueryPrep_ls) {
  GroupNames_ <- QueryPrep_ls$Listing$Datastore$Datastore$groupname
  Groups_ <- GroupNames_[-grep("/", GroupNames_)]
  if (any(Groups_ == "")) {
    Groups_ <- Groups_[-(Groups_ == "")]
  }
  TempDir <- SaveArchiveName
  dir.create(TempDir)
  for (Group in Groups_) {
    GroupDir <- file.path(TempDir, Group)
    dir.create(GroupDir)
    Tables_ <- listTables(Group, QueryPrep_ls)$Datastore
    for (tb in Tables_) {
      Listing_df <- listDatasets(tb, Group, QueryPrep_ls)$Datastore
      write.table(Listing_df, file = file.path(GroupDir, paste0(tb, ".csv")),
                  row.names = FALSE, col.names = TRUE, sep = ",")
    }
  }
  zip(paste0(SaveArchiveName, ".zip"), TempDir)
  remove_dir(TempDir)
  TRUE
}
# #Example
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )
# documentDatastoreTables("Datastore_Documentation", QPrep_ls)
# rm(QPrep_ls)

#READ MULTIPLE DATASETS FROM DATASTORES
#======================================
#' Read multiple datasets from multiple tables in datastores
#'
#' \code{readDatastoreTables} a visioneval framework query user function that reads datasets
#' from one or more tables in a specified group in one or more datastores
#'
#' This function can read multiple datasets in one or more tables in a group.
#' More than one datastore my be specified so that if datastore references are
#' used in a model run, datasets from the referenced datastores may be queried
#' as well. Note that the capability for querying multiple datastores is only
#' for the purpose of querying datastores for a single model scenario. This
#' capability should not be used to compare multiple scenarios. The function
#' does not segregate datasets by datastore. Attempting to use this function to
#' compare multiple scenarios could produce unpredictable results.
#'
#' @param Tables_ls a named list where the name of each component is the name of
#' a table in a datastore group and the value is a named string vector where the
#' names are the names of the datasets to be retrieved and the values are the
#' units of measure to be used for the retrieved values or NULL if the values
#' are to be retrieved in the units they are in in the datastore.
#' @param Group a string that is the name of the group to retrieve the table
#' datasets from.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return A named list having two components. The 'Data' component is a list
#' containing the datasets from the datastores where the name of each component
#' of the list is the name of a table from which identified datasets are
#' retrieved and the value is a data frame containing the identified datasets.
#' The 'Missing' component is a list which identifies the datasets that are
#' missing in each table.
#' @export
readDatastoreTables <- function(Tables_ls, Group, QueryPrep_ls) {
  #Extract the datastore reading functions
  readFromTable <- QueryPrep_ls$Functions$readFromTable
  listDatastore <- QueryPrep_ls$Functions$listDatastore
  #Extract the datastore listings
  MS_ls <- QueryPrep_ls$Listing
  #Datastore locations
  DstoreLocs_ <- QueryPrep_ls$Dir
  #Get data from table
  Tb <- names(Tables_ls)
  Out_ls <- list()
  for (tb in Tb) {
    Out_ls[[tb]] <- list()
    Ds <- names(Tables_ls[[tb]])
    for (Loc in DstoreLocs_) {
      if ( ! "ve.model" %in% search() ) stop("ve.model environment is not available.")
      assign("ModelState_ls", QueryPrep_ls$Listing[[Loc]], envir=as.environment("ve.model"))
      HasTable <- checkTableExistence(tb, Group, ModelState_ls$Datastore)
      if (HasTable) {
        for (ds in Ds) {
          HasDataset <- checkDataset(ds, tb, Group, ModelState_ls$Datastore)
          if (HasDataset) {
            if (is.null(Out_ls[[tb]][[ds]])) {
              Dset_ <-
                readFromTable(ds, tb, Group, DstoreLoc = Loc, ReadAttr = TRUE)
              if (Tables_ls[[tb]][ds] != "") {
                DsetType <- attributes(Dset_)$TYPE
                DsetUnits <- attributes(Dset_)$UNITS
                ToUnits <- Tables_ls[[tb]][ds]
                Dset_ <- convertUnits(Dset_, DsetType, DsetUnits, ToUnits)$Values
                attributes(Dset_)$TYPE <- DsetType
                attributes(Dset_)$UNITS <- ToUnits
              }
              Out_ls[[tb]][[ds]] <- Dset_
            }
          }
        }
      }
    }
    Out_ls[[tb]] <- data.frame(Out_ls[[tb]])
  }
  #Identify missing datasets
  OutDsetNames_ls <- lapply(Out_ls, names)
  Missing_ls <- Tables_ls
  for (tb in Tb) {
    Missing_ls[[tb]] <- Tables_ls[[tb]][!(names(Tables_ls[[tb]]) %in% OutDsetNames_ls[[tb]])]
  }
  #Return the table data
  list(Data = Out_ls, Missing = Missing_ls)
}
# #Example
# #-------
# #Prepare for datastore query
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )
# #Develop the list of tables to get data for
# TablesRequest_ls <- list(
#   Household = c(
#     Bzone = "",
#     HhSize = "",
#     AveCO2ePM = "GM/MI",
#     Income = "",
#     Dvmt = "MI/YR"),
#   Bzone = c(
#     Bzone = "",
#     D1B = "PRSN/ACRE",
#     MFDU = "",
#     SFDU = "")
# )
# #Get the data
# TableResults_ls <-
#   readDatastoreTables(
#     Tables_ls = TablesRequest_ls,
#     Group = "2010",
#     QueryPrep_ls = QPrep_ls
#   )


#CHECK FOR DATASET PRESENCE
#==========================
#' Check whether a dataset is present in a table
#'
#' \code{isDatasetPresent} a visioneval module developer function that checks whether a dataset
#' is present in a table
#'
#' This function checks for the presence of a dataset in a table. The role of
#' this function is to enable the calculation of a measure to be switched
#' based on the presence of one or more datasets. For example, some datasets
#' present in a VE-State model are not present in a VE-RSPM model and vice
#' versa. So in some instances, a measure needs to be calculated in a different
#' way for VE-State and VE-RSPM models.
#'
#' @param Dataset a string identifying the name of the dataset to check the
#' presence of.
#' @param Table a string identifying the name of the table where the dataset may
#' be located.
#' @param Group a string identifying the name of the group where the dataset may
#' be located.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return TRUE if the dataset is present and FALSE if it is not present.
#' @export
isDatasetPresent <- function(Dataset, Table, Group, QueryPrep_ls) {
  DstoreLocs_ <- QueryPrep_ls$Dir
  DatasetPresent_ <- logical(length(DstoreLocs_))
  for (i in 1:length(DstoreLocs_)) {
    ModelState_ls <- QueryPrep_ls$Listing[[DstoreLocs_[i]]]
    DatasetPresent_[i] <-
      checkDataset(Dataset, Table, Group, ModelState_ls$Datastore)
  }
  any(DatasetPresent_)
}

#READ AND SUMMARIZE A DATASET
#============================
#' Summarize the values in a table dataset according to the values in another
#' dataset in the table.
#'
#' \code{summarizeDataset} a visioneval framework query user function that summarizes the values
#' in a table dataset according to the values in another dataset in the table.
#'
#' This function is used to calculate summary measures from one or more
#' datasets in a model datastore. The calculation of a summary measure is
#' specified as an R language expression in the form of a double-quoted string.
#' This expression can summarize datasets using sum, count, mean, weighted mean,
#' median, min, and max functions. The values of datasets and/or dataset
#' summaries may be combined using ordinary arithmetic operations (+, -, *, /).
#' Datasets may be indexed (subsetted) using the standard square brackets
#' notation. Logical comparators and conjunctions can be used for indexing. The
#' calculations can combine datasets from different tables in the datastore.
#' This can be done by merging the datasets into one table using specified keys
#' and applying the specified expression to the merged table. Datasets in
#' different tables can also be combined by summing each to a common geography
#' and then adding the respective sums.
#'
#' @param Expr a string specifying an R language expression to use to summarize
#'   the datasets. Operands in the expression are the names of datasets to use
#'   to create the summary.The only functions that may be used  in the
#'   expression are 'sum', 'count', 'mean', 'wtmean' (weighted mean), 'median',
#'   'min' (minimum), and 'max' (maximum).  The following operators may also be
#'   used in the expression: '+', '-', '*', and '/'.  The calculation can
#'   include data indexing (subsetting) expressions that can include the
#'   following logical comparisons and conjunctions: '==', '>=', '<=', '!=',
#'   '>', '<', '&', '|'. String values in a comparison must be surrounded by
#'   single quotes (e.g. 'Urban') rather than double quotes. Note that if the
#'   expression involves the calculation from datasets in different tables and
#'   if those are not merged using keys (see 'Key' below), only summations are
#'   allowed in the expression ('sum', '+').
#' @param Units_ a named character vector identifying the units to be used for
#'   each operand in 'Expr' and each 'By_' dataset. The element names
#'   are the operands identified in the expression and any dataset names
#'   identified in the 'By_' argument. The element values are the units that the
#'   data are to be converted to when retrieved from the datastore. If no
#'   conversion is required (i.e. retaining the units as they are in the
#'   datastore), set the value equal to "". The specified units value for a
#'   dataset must be consistent with the data type of the dataset. Note that the
#'   'documentDatastoreTables' function can be used to document all the datasets
#'   in a datastore including their unit.
#' @param By_ an optional character vector identifying the names of the datasets
#'   to use for grouping the expression calculation. The default value is NULL
#'   (no grouping is done). If one dataset is identified, the function returns a
#'   vector of values by group. If two datasets are identified, the function
#'   returns a matrix of values with rows corresponding to groups of the first
#'   listed dataset and the columns corresponding to groups of the second listed
#'   dataset. No more than 2 datasets may be listed. Note that if an non-integer
#'   numeric dataset is to be used for grouping, values for splitting the
#'   values into categories must be specified in the 'Breaks_ls' argument.
#' @param Breaks_ls a named list of vectors identifying the values to use for
#'   splitting numeric datasets into categories. This parameter is optional
#'   unless one or more datasets specified for the 'By_' parameter contain
#'   non-integer numeric values. The names of the list components must be the
#'   same as names of the numeric datasets identified in the 'By_' vector. Each
#'   named component of the list is a vector of values to be used to split the
#'   respective By dataset into groups. Minimum and maximum values do not need
#'   to be specified as they are computed from the dataset.
#' @param Table a string or named list identifying the datastore table(s) where
#'   the datasets identified in the 'Expr' argument and 'By_' argument are
#'   located. If all datasets are located in the same table, then the value of
#'   'Table' should be a string. If the datasets are located in more than one
#'   table, then the value of 'Table' must be a named list where the names are
#'   the names of the tables where the datasets are located and the respective
#'   values are character vectors identifying the names of the datasets located
#'   in each of the tables. Note that if the 'By_' argument is not NULL and no
#'   keys for merging table datasets are identified (see 'Key' below) then the
#'   datasets identified in the 'By_' argument must be included in the
#'   identification of datasets for every table. However, if keys for merging
#'   table datasets are identified, then each dataset identified in the 'By_'
#'   argument can't be listed for more than one table.
#' @param Key a optional parameter that may be either a string or named list.
#'   This parameter is used to identify keys to be used for merging datasets
#'   located in different datastore tables. If one key is used to merge all the
#'   table datasets then that key is specified as a string. If datasets from
#'   more than 2 tables are to be merged and more than one key is to be used to
#'   merge the table datasets, then the keys must be specified as a named list
#'   where the component names are the names of the datasets to be used as keys
#'   and the each value is a character vector containing the names of the tables
#'   to be joined with the named key. Only the following keys may be used to
#'   join table datasets: 'Marea', 'Azone', 'Bzone', 'HhId'.
#' @param Group a string identifying the datastore group where the dataset is
#'   located.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#'   function which identifies the datastore location(s), listing(s), and
#'   functions for listing and read the datastore(s).
#' @return If the By_ argument is NULL or has a length of 1, the value of the
#'   specified expression is calculated. Note that if the expression produces a
#'   vector of more than one number the entire vector of numbers will be
#'   returned. Users should check their expression to confirm that it will
#'   produce a single number if that is what is desired. Assuming that the
#'   expression produces a single value, if the 'By_' argument only identifies
#'   one dataset to use for grouping, the function will return a vector of
#'   values. If the 'By_' argument identifies two grouping datasets, the
#'   function will return a matrix of values.
#' @export
summarizeDatasets <-
  function(
    Expr,
    Units_,
    By_ = NULL,
    Breaks_ls = NULL,
    Table,
    Key = NULL,
    Group,
    QueryPrep_ls)
  {
    #----------------
    #Define functions
    #----------------
    #Count function
    count <- function(x) length(x)
    #Weighted mean function
    wtmean <- function(x, w) sum(x * w) / sum(w)
    #Identify whether symbol is an operand
    isOperand <- function(Symbol) {
      Functions_ <- c("sum", "count", "mean", "wtmean", "max", "min", "median", "c")
      Operators_ <- c("+", "-", "*", "/", "%in%")
      Comparators_ <- c("==", ">=", "<=", "!=", ">", "<", "&", "|", "!")
      Group_ <- c("(", ")", "[", "]")
      NonOperands_ <- c(Functions_, Operators_, Comparators_, Group_)
      !(deparse(Symbol) %in% NonOperands_) & !is.character(Symbol) & !is.numeric(Symbol) & !is.logical(Symbol)
    }
    #Recursive function to get the operands in an expression
    getOperands <- function(AST) {
      if (length(AST) == 1) {
        if (isOperand(AST)) deparse(AST)
      } else {
        unlist(lapply(AST, function(x) getOperands(x)))
      }
    }
    #Identify the operands of the Expr
    Operands_ <- unique(getOperands(str2lang(Expr)))
    #Identify whether symbol is a function
    isFunction <- function(Symbol) {
      Functions_ <- c("sum", "count", "mean", "wtmean", "max", "min", "median")
      deparse(Symbol) %in% Functions_
    }
    #Recursive function to get functions in an expression
    getFunctions <- function(AST) {
      if (length(AST) == 1) {
        if (isFunction(AST)) deparse(AST)
      } else {
        unlist(lapply(AST, function(x) getFunctions(x)))
      }
    }
    #Identify the functions of the Expr
    Functions_ <- unique(getFunctions(str2lang(Expr)))
    #Identify whether symbol is an operator
    isOperator <- function(Symbol) {
      Operators_ <- c("+", "-", "*", "/")
      deparse(Symbol) %in% Operators_
    }
    #Recursive function to get operators in an expression
    getOperators <- function(AST) {
      if (length(AST) == 1) {
        if (isOperator(AST)) deparse(AST)
      } else {
        unlist(lapply(AST, function(x) getOperators(x)))
      }
    }
    #Identify the operators of the Expr
    Operators_ <- unique(getOperators(str2lang(Expr)))
    #------------------------------
    #Check Units_ and By_ arguments
    #------------------------------
    #Check that all operands have units
    if (!all(Operands_ %in% names(Units_))) {
      stop("Some of the operands in the expression don't have specified units.")
    }
    #Check that all datasets named in the By_ argument have units
    if (!all(By_ %in% names(Units_))) {
      stop(paste(
        "Some of the datasets listed in the 'By_' argument",
        "don't have specified units."))
    }
    #Check the not more than 2 By_ datasets
    if (length(By_) > 2) {
      stop(paste(
        "Function currently does not support more than 2 'By_' arguments."))
    }
    #---------------------------------------------------------------
    #Check that if Table is a list, check for accounting of datasets
    #---------------------------------------------------------------
    if (is.list(Table)) {
      DsetNames_ <- as.vector(unlist(Table))
      MsngNames_ <- names(Units_)[!(names(Units_) %in% DsetNames_)]
      #Check for missing names
      if (length(MsngNames_) != 0) {
        Msg <- paste(
          "For the calculation of the expression --",
          Expr,
          "-- which involves retrieving data from multiple tables,",
          "the list of datasets to be retrieved from the tables",
          "does not identify all the datasets necessary to compute the",
          "expression by the categories listed in the 'By_' argument.",
          "The missing names are:",
          paste(MsngNames_, collapse = ", "),
          "."
        )
        stop(Msg)
      }
      #Check for duplicated names not used as keys
      DupNames_ <- DsetNames_[duplicated(DsetNames_)]
      if (length(DupNames_) != 0 & !is.null(Key)) {
        Msg <- paste(
          "For the calculation of the expression --",
          Expr,
          "-- which involves retrieving data from multiple tables",
          "and uses keys specified by the 'Key' argument to join the table",
          "datasets, the list of datasets to be retrieved from the tables",
          "has duplicates of the following names:",
          paste(DupNames_, collapse = ", "), ".",
          "Duplicate names will cause problems with the calculation of the",
          "expression by the categories listed in the 'By_' argument",
          "after the data from the tables are merged into one table.",
          "Change the 'Table' argument so there is only one instance of",
          "each name. This error may have occurred because you listed the",
          "names of datasets that are to be used as keys for joining the",
          "datasets from different tables.",
          "Key datasets must not be listed in the 'Table' argument.",
          "They are listed in the 'Key' argument instead."
        )
        stop(Msg)
      }
      #Check that user did not specify as vector of tables
      if (!is.list(Table) & length(Table) != 1) {
        Msg <- paste(
          "For the calculation of the expression --",
          Expr,
          "--, the 'Table' argument is improperly specified.",
          "More than one table is identified but not as a list.",
          "If the datasets are to be retrieved from more than one table,",
          "then the 'Table' argument must be a list where the component",
          "names of the list are the names of the tables",
          "and the corresponding component values are character vectors",
          "identifying the datasets to be retrieved.",
          "Note that all of the datasets identified in the expression",
          "and in the 'By_' argument must be accounted for.",
          "For more information see the function documentation",
          "(?summarizeDatasets)."
        )
        stop(Msg)
      }
    }
    #-----------------------------------------------------------------
    #Check that keys are supplied for joining datasets from all tables
    #-----------------------------------------------------------------
    #If Table is not a list, then there should be no keys
    if (!is.list(Table)) {
      if (!is.null(Key)) {
        Msg <- paste(
          "For the calculation of the expression --",
          Expr,
          "-- all the datasets are identified as coming only one table",
          "but a key for joining datasets from more than one table",
          "is identified in the 'Key' argument.",
          "If all the datasets identified in the expression an in the",
          "'By_' argument are from one table, then the value of",
          "the 'Key' argument must be NULL."
        )
        stop(Msg)
      }
    }
    #If Table is a list, then check the Key argument for consistency
    if (is.list(Table) & !is.null(By_)) {
      #If Key is NULL, check that By_ datasets are specified in all tables
      if (is.null(Key)) {
        HasBy_ <- unlist(lapply(Table, function(x) {
          all(By_ %in% x)
        }))
        if (!all(HasBy_)) {
          TableNotHasBy_ <- names(HasBy_)[!HasBy_]
          Msg <- paste(
            "For the calculation of the expression --",
            Expr,
            "-- the datasets identified in the expression and in the 'By_'",
            "argument are identified as coming from more than one table.",
            "Because no key or keys are identified by the 'Key' argument",
            "for joining the datasets from the different tables, the datasets",
            "identified in the 'By_' argument must be included for all tables",
            "identified in the 'Table' argument in order for the computations",
            "to be completed using the 'By_' groupings.",
            "The following table specifications do not include all 'By_' datasets:",
            paste(TableNotHasBy_, collapse = ", "), ".",
            "See the function documentation (?SummarizeDatasets) for more information.")
          stop(Msg)
        }
      }
      #Check that user did not specify as vector of keys
      if (!is.null(Key)) {
        if (!is.list(Key) & length(Key) != 1) {
          Msg <- paste(
            "For the calculation of the expression --",
            Expr,
            "--, the 'Key' argument is improperly specified.",
            "More than one key is identified but not as a list.",
            "If more than one key is used to join datasets from more than",
            "two tables, then the 'Key' argument must be a list where the",
            "component names of the list are the names of the keys",
            "and the corresponding component values are character vectors",
            "identifying the tables to be joined with the respective key.",
            "For more information see the function documentation",
            "(?summarizeDatasets)."
          )
          stop(Msg)
        }
      }
      #Check that all the specified keys are allowed
      if (!is.null(Key)) {
        if (is.list(Key)) {
          KeyNames_ <- names(Key)
        } else {
          KeyNames_ <- Key
        }
        AllowedKeys_ <- c("Azone", "Bzone", "Marea", "HhId")
        if (!all(KeyNames_ %in% AllowedKeys_)) {
          WrongNames_ <- KeyNames_[!(KeyNames_ %in% AllowedKeys_)]
          Msg <- paste(
            "For the calculation of the expression --",
            Expr,
            "-- the datasets identified in the expression and in the 'By_'",
            "argument are identified as coming from more than one table.",
            "One or more of the keys for joining the datasets from these tables",
            "as identified by the 'Key' argument are not permitted to be used",
            "as keys. The unpermitted keys are:",
            paste(WrongNames_, collapse = ", "), ".",
            "The only permitted keys are:",
            paste(AllowedKeys_, collapse = ", "), "."
          )
          stop(Msg)
        }
      }
      #If Key is list, check that keys are identified for all tables
      if (!is.null(Key)) {
        if (is.list(Key)) {
          KeyNames_ <- as.vector(unlist(Key))
          MsngNames_ <- names(Table)[!(names(Table) %in% KeyNames_)]
          if (length(MsngNames_) != 0) {
            Msg <- paste(
              "For the calculation of the expression --",
              Expr,
              "-- the datasets identified in the expression and in the 'By_'",
              "argument are identified as coming from more than one table.",
              "More than one key is identified for joining the datasets from",
              "these tables by the 'Key' argument, but not all the tables to be",
              "joined are listed by the 'Key' argument.",
              "The names of the missing tables are:",
              paste(MsngNames_, collapse = ", "), ".",
              "For more information on how to properly specify the keys for",
              "joining datasets from different tables, see the function documentation",
              "(?summarizeDatasets)."
            )
            stop(Msg)
          }
        }
      }
    }
    #---------------------------------------------
    #Check that aggregating operations are correct
    #---------------------------------------------
    #If datasets from different tables are to be combined and are not being
    #merged, then the only combining operation allowed is summation.
    if (is.list(Table) & is.null(Key)) {
      OnlySummation <- all(c(Functions_, Operators_) %in% c("sum", "+"))
      if (!OnlySummation) {
        Msg <- paste(
          "The calculation of the expression --",
          Expr,
          "-- ",
          "combines calculations on datasets in different tables",
          "that are not being merged using keys.",
          "This can only be done if all of the calculations are additive.",
          "Only the 'sum' function and the '+' operator can be used in the",
          "calculation expression. The specified expression includes other",
          "functions and/or operators as well."
        )
        stop(Msg)
      }
    }
    #-----------------------------------------------------------
    #Retrieve & and format datasets if they are all in one table
    #-----------------------------------------------------------
    if (!is.list(Table)) {
      #Get the datasets from the datastore
      Tables_ls <- list()
      Tables_ls[[Table]] <- Units_
      Data_ls <- readDatastoreTables(Tables_ls, Group, QueryPrep_ls)
      #Stop if any of the datasets are missing
      if (length(Data_ls$Missing[[Table]]) != 0) {
        MissingDsets_ <- paste(names(Data_ls$Missing[[Table]]), collapse = ", ")
        Msg <- paste("The following datasets are not present in the",
                     Table, "table", "in the", Group, "group:", MissingDsets_)
        stop(Msg)
      }
      #Simplify the data list
      Data_df <- Data_ls$Data[[Table]]
      Data_ls$Data <- list()
      Data_ls$Data[[1]] <- as.list(Data_df)
      rm(Data_df)
    }
    #-------------------------------------------------------------------
    #Retrieve, merge, and format datasets if they are in multiple tables
    #-------------------------------------------------------------------
    if (is.list(Table)) {
      #Initialize table specifications
      Tables_ls <- list()
      for (nm in names(Table)) {
        Tables_ls[[nm]] <- Units_[Table[[nm]]]
      }
      #Add keys if any
      if (!is.null(Key)) {
        #If only one key, convert to list
        if (!is.list(Key)) {
          Key <- local({
            KeyName <- Key
            Key <- list()
            Key[[KeyName]] <- names(Table)
            Key
          })
        }
        #Add keys to table specifications
        for (key in names(Key)) {
          TablesSpecsToChange_ <- Key[[key]]
          for (tbl in TablesSpecsToChange_) {
            Tables_ls[[tbl]] <-
              setNames(c(Tables_ls[[tbl]], ""), c(names(Tables_ls[[tbl]]), key))
          }
        }
      }
      #Retrieve the data
      Data_ls <- readDatastoreTables(Tables_ls, Group, QueryPrep_ls)
      #Stop if any of the datasets are missing
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
        Msg <- paste(
          "For the calculation of the expression --",
          Expr,
          "one or more datasets identified to be retrieved from one or",
          "more tables are missing. Following is a listing of the tables",
          "and missing datasets (in parentheses):",
          paste(Missing_, collapse = ", "),
          "."
        )
        stop(Msg)
      }
      #Merge the table datasets if any keys provided
      if (!is.null(Key)) {
        TableRank_ <- c(
          Marea = 1, Azone = 2, Bzone = 3, Household = 4, Worker = 5, Vehicle = 5
        )
        Merge_ls <- lapply(Key, function(x) {
          Rank_ <- TableRank_[x]
          Rank_[order(Rank_)]
        })
        Merge_ls <- Merge_ls[order(unlist(lapply(Merge_ls, min)))]
        MergeTables_ <- unname(unlist(lapply(Merge_ls, function(x) names(x))))
        MergeKeys_ <- rep(names(Merge_ls), unlist(lapply(Merge_ls, length)))
        Data_df <- Data_ls$Data[[MergeTables_[1]]]
        for (i in 2:length(MergeTables_)) {
          if (MergeTables_[i] != MergeTables_[i-1]) {
            Data_df <- merge(
              Data_df,
              Data_ls$Data[[MergeTables_[i]]],
              MergeKeys_[i])
          }
        }
        Data_ls$Data <- list()
        Data_ls$Data[[1]] <- as.list(Data_df)
        rm(Data_df)
      }
      #Prepare data if there is no key
      if (is.null(Key)) {
        #If no By_ variables, combine all datasets into one list
        if (is.null(By_)) {
          CombiData_ls <- do.call(c, lapply(Data_ls$Data, function(x) as.list(x)))
          names(CombiData_ls) <-
            gsub("^.*\\.", "", names(CombiData_ls)) # Remove the table part of the name
          Data_ls$Data <- list()
          Data_ls$Data[[1]] <- CombiData_ls
          rm(CombiData_ls)
          #If is By_ variables, expand all tables to include all operands
          #assigning value of 0 to missing operands
        } else {
          Data_ls$Data <- lapply(Data_ls[[1]], function(x) {
            Data_df <- x
            AddVars_ <- Operands_[!(Operands_ %in% names(Data_df))]
            for (AddVar in AddVars_) {
              Data_df[[AddVar]] <- 0
            }
            as.list(Data_df)
          })
        }
      }
    }
    #-----------------------------------------------------------
    #Define a function to calculate measures if By_ is specified
    #-----------------------------------------------------------
    calcWithBy <- function(CalcData_ls) {
      #If there is a By_ argument do calculations by group and return as array
      By_ls <- list()
      #Check and process the By data into categories
      for (nm in By_) {
        ByData_ <- CalcData_ls[[nm]]
        # ByData_ <- CalcData_ls[[1]][[nm]]
        if (!is.numeric(ByData_)) {
          if (is.factor(ByData_)) {
            By_ls[[nm]] <- ByData_
          }
          if (is.character(ByData_)) {
            By_ls[[nm]] <- as.factor(ByData_)
          }
        } else {
          if (is.integer(ByData_) | all(round(ByData_) == as.integer(ByData_))) {
            ByData_ <- as.integer(ByData_)
            if (!is.null(Breaks_ls[[nm]])) {
              Breaks_ <- unique(c(min(ByData_), Breaks_ls[[nm]], max(ByData_)))
              By_ls[[nm]] <- cut(ByData_, Breaks_, include.lowest = TRUE)
            } else {
              By_ls[[nm]] <- as.factor(ByData_)
            }
          }
          if (is.double(ByData_) & !all(round(ByData_) == as.integer(ByData_))) {
            if (!is.null(Breaks_ls[[nm]])) {
              Breaks_ <- unique(c(min(ByData_), Breaks_ls[[nm]], max(ByData_)))
              By_ls[[nm]] <- cut(ByData_, Breaks_, include.lowest = TRUE)
            } else {
              stop(paste(nm, "is non-integer number. Breaks must be specified."))
            }
          }
        }
      }
      #Identify the dimension names for each By dimension
      ByNames_ls <- lapply(By_ls, function(x) as.character(levels(x)))
      #Set up array to store results
      Results_ar <- array(NA,
                          dim = unlist(lapply(ByNames_ls, length)),
                          dimnames = ByNames_ls)
      #Calculate values if length of By_ is 1
      if (length(By_) == 1) {
        for (n1 in ByNames_ls[[1]]) {
          Select_ <- By_ls[[1]] == n1
          if (sum(Select_) != 0) {
            DataSelect_ls <- lapply(CalcData_ls, function(x) x[Select_])
            Results_ar[n1] <- eval(parse(text = Expr), envir = DataSelect_ls)
          } else {
            Results_ar[n1] <- NA
          }
        }
      }
      #Calculate values if length of By_ is 2
      if (length(By_) == 2) {
        for (n1 in ByNames_ls[[1]]) {
          for (n2 in ByNames_ls[[2]]) {
            Select_ <- By_ls[[1]] == n1 & By_ls[[2]] == n2
            if (sum(Select_) != 0) {
              DataSelect_ls <- lapply(CalcData_ls, function(x) x[Select_])
              Results_ar[n1,n2] <- eval(parse(text = Expr), envir = DataSelect_ls)
            } else {
              Results_ar[n1,n2] <- NA
            }
          }
        }
      }
      return(Results_ar)
    }
    #----------------------------------------------
    #Calculate the Expression and Return the Result
    #----------------------------------------------
    #If there isn't a By_ argument
    if (is.null(By_)) {
      Result <- eval(parse(text = Expr), envir = Data_ls$Data[[1]])
    }
    #If there is a By_ but only one merged dataset
    if (!is.null(By_) & length(Data_ls$Data) == 1) {
      Result <- calcWithBy(Data_ls$Data[[1]])
    }
    #If there is a By_ but several tables
    if (!is.null(By_) & length(Data_ls$Data) > 1) {
      #Calculate results for each table
      Results_ls <- lapply(Data_ls$Data, function(x) calcWithBy(x))
      #Check that they conform if 1 By_ variable
      if (length(By_) == 1) {
        Names_ls <- lapply(Results_ls, function(x) names(x))
        AllNames_ <- sort(unique(unlist(Names_ls)))
        Results_ls <- lapply(Results_ls, function(x) {
          Vals_ <- setNames(rep(NA, length(AllNames_)), AllNames_)
          Vals_[names(x)] <- x
          Vals_
        })
      }
      #Check that they conform if 2 By_ variables
      if (length(By_) == 2) {

      }
      Result <- do.call("+", Results_ls)
    }
    Result
  }

# #Examples of summarizing datasets
# #================================
# #Assumes Datastore of a VE-State model in working directory
#
# #Prepare to make dataset summaries
# QPrep_ls <- prepareForDatastoreQuery(
#   DstoreLocs_ = c("Datastore"),
#   DstoreType = "RD"
# )
#
# #Summing a dataset
# summarizeDatasets(
#   Expr = "sum(Dvmt)",
#   Units_ = c(
#     Dvmt = ""
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
# #Converting units while summing a dataset
# summarizeDatasets(
#   Expr = "sum(Dvmt)",
#   Units_ = c(
#     Dvmt = "KM/DAY"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
# #Counting number of records in dataset
# #Note: "" for units uses units stored in datastore
# summarizeDatasets(
#   Expr = "count(HhSize)",
#   Units_ = c(
#     HhSize = ""
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
# #Mean of dataset
# summarizeDatasets(
#   Expr = "mean(AveGPM)",
#   Units_ = c(
#     AveGPM = "GGE/MI"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
# #Weighted mean of dataset
# summarizeDatasets(
#   Expr = "wtmean(AveGPM, Dvmt)",
#   Units_ = c(
#     AveGPM = "GGE/MI",
#     Dvmt = "MI/DAY"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
# #Calculate average number of household vehicles per capita
# summarizeDatasets(
#     Expr = "sum(Vehicles) / sum(HhSize)",
#     Units_ = c(
#       Vehicles = "VEH",
#       HhSize = "PRSN"
#     ),
#     Table = "Household",
#     Group = "2010",
#     QueryPrep_ls = QPrep_ls)
#
# #Calculate average number of vehicles per household by household size
# summarizeDatasets(
#   Expr = "sum(Vehicles) / sum(HhSize)",
#   Units_ = c(
#     Vehicles = "VEH",
#     HhSize = "PRSN"
#   ),
#   By_ = "HhSize",
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#
# #Specify breaks for establish household size groups
# summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   By_ = c("HhSize"),
#   Breaks_ls = list(
#     HhSize = c(0,1,2,3,4)
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#
# #Split by household size and income groups
# summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(Drivers)",
#   Units_ = c(
#     NumAuto = "VEH",
#     Drivers = "PRSN",
#     Income = "USD",
#     HhSize = "PRSN"
#   ),
#   By_ = c(
#     "HhSize",
#     "Income"),
#   Breaks_ls = list(
#     HhSize = c(1,2,3,4),
#     Income = c(20000, 40000, 60000, 80000)
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#
#Calculate proportion of workers who pay for parking by Marea and AreaType where
#the worker works. PaysForParking is in Worker table and Marea and AreaType are
#in Bzone table. Table datasets are merged using Bzone as key.
# summarizeDatasets(
#   Expr = "sum(PaysForParking) / count(PaysForParking)",
#   Units_ = c(
#     PaysForParking = "",
#     Marea = "",
#     AreaType = ""
#   ),
#   By_ = c("Marea", "AreaType"),
#   Table = list(
#     Worker = c("PaysForParking"),
#     Bzone = c("AreaType", "Marea")
#   ),
#   Key = "Bzone",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
#Calculate proportion of workers who pay for parking by household income level and AreaType where
#the worker lives. PaysForParking is in Worker table, AreaType is in the Bzone table, and
#Income is in the Household table. The Bzone key is used to merge the AreaType and Income into
#a table that is merged with PaysForParking using the HhId key.
# summarizeDatasets(
#   Expr = "sum(PaysForParking) / count(PaysForParking)",
#   Units_ = c(
#     PaysForParking = "",
#     AreaType = "",
#     Income = ""
#   ),
#   By_ = c("AreaType", "Income"),
#   Breaks_ls = list(
#     Income = c(20000, 40000, 60000, 80000)
#   ),
#   Table = list(
#     Worker = c("PaysForParking"),
#     Household = c("Income"),
#     Bzone = c("AreaType")
#   ),
#   Key = list(
#     Bzone = c("Bzone", "Household"),
#     HhId = c("Household", "Worker")
#   ),
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#
#Sum total CO2e emissions from light-duty vehicles at the Marea level. VanCO2e,
#ComSvcNonUrbanCO2e, and ComSvcUrbanCO2e are in the Marea table. DailyCO2e,
#which is CO2e from household vehicle use is in the Household table. Since we
#want to calculate a total of values in different tables, the tables are not
#merged. The total values by Marea are calculated for each table and then
#summed.
# summarizeDatasets(
#   Expr = "sum(VanCO2e) + sum(ComSvcNonUrbanCO2e) + sum(ComSvcUrbanCO2e) + sum(DailyCO2e)",
#   Units_ = c(
#     VanCO2e = "MT",
#     ComSvcNonUrbanCO2e = "MT",
#     ComSvcUrbanCO2e = "MT",
#     DailyCO2e = "MT/DAY",
#     Marea = ""
#   ),
#   Table = list(
#     Marea = c("Marea", "VanCO2e", "ComSvcNonUrbanCO2e", "ComSvcUrbanCO2e"),
#     Household = c("Marea", "DailyCO2e")
#   ),
#   By_ = "Marea",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
