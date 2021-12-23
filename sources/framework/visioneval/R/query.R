#===========
#query.R
#===========

#Functions for querying a datastore. See framework/docs/QueryDocs.R

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
#' may be specified so that if DATASTORE REFERENCES are used in a model run,
#' datasets from the referenced datastores may be queried as well. Note that the
#' capability for querying multiple datastores is only for the purpose of
#' querying datastores for a single model scenario spread over multiple stages.
#' This capability should not be used to compare multiple scenarios. The
#' function does not segregate datasets by datastore. Attempting to use this
#' function to compare multiple scenarios could produce unpredictable results.
#'
#' @param DstoreLocs_ a string vector identifying the paths to all of the
#' datastores to extract the datasets from. Each entry must be the full relative
#' path to a datastore (e.g. 'tests/Datastore')
#' @param DstoreType a string identifying the type of datastore
#' (e.g. 'RD', 'H5').
#' @return A named list having three components. The 'Dir' component is a
#' string vector identifying the relative path(s) to the datastore(s). The
#' 'Listing' component is a list where each component is the ModelState for the corresponding
#' element in 'Dir'. The 'DstoreType' component contains the DstoreType for use in
#' selecting a suitable readFromTable function.
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
  Prep_ls$DstoreType <- DstoreType;
  #Check that DstoreLocs_ are correct and assign
  DstoreLocsExist_ <- sapply(DstoreLocs_, function(x) file.exists(x))
  if (any(!DstoreLocsExist_)) {
    Msg <-
      paste0("One or more of the specified DstoreLocs_ can not be found. Check the following: ",
             paste(DstoreLocs_[!DstoreLocsExist_],collapse=", "))
    stop(Msg,call.=FALSE)
  } else {
    Prep_ls$Dir <- DstoreLocs_
  }
  #Get listing for each datastore
  Prep_ls$Listing <- lapply(DstoreLocs_, function(x) {
    return( readModelState(FileName = file.path(dirname(x),getModelStateFileName())) )
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
  unlink(TempDir,recursive=TRUE)
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
#' \code{readDatastoreTables} a visioneval framework query user function that reads datasets from
#' one or more tables in a specified group in one or more datastores
#'
#' This function can read multiple datasets in one or more tables in a group. More than one
#' datastore my be specified so that if datastore references are used in a model run, datasets from
#' the referenced datastores may be queried as well. Note that the capability for querying multiple
#' datastores is only for the purpose of querying datastores for a single model scenario. This
#' capability should not be used to compare multiple scenarios. The function does not segregate
#' datasets by datastore. Attempting to use this function to compare multiple scenarios could
#' produce unpredictable results.
#'
#' @param Tables_ls a named list where the name of each component is the name of a table in a
#' datastore group and the value is a named string vector where the names are the names of the
#' datasets to be retrieved and the values are the units of measure to be used for the retrieved
#' values or NULL (or NA) if the values are to be retrieved in the units they are in in the
#' datastore.
#' @param Group a string that is the name of the group to retrieve the table datasets from.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery function which
#' identifies the datastore location(s), listing(s), and functions for listing and read the
#' datastore(s).
#' @return A named list having two components. The 'Data' component is a list containing the
#' datasets (or list of field vectors if asList==TRUE) from the datastores where the name of each
#' component of the list is the name of a table from which identified datasets are retrieved and the
#' value is a data frame or list containing the identified datasets (it will be a data.frame if all
#' the datsets have the same length, otherwise it will be a named list with each element having
#' the name of the dataset. The 'Missing' component is a list which identifies the datasets that
#' are entirely missing in each table.
#' @export
readDatastoreTables <- function(Tables_ls, Group, QueryPrep_ls) {
  #Extract the datastore listings
  MS_ls <- QueryPrep_ls$Listing;
  #Datastore locations
  DstoreLocs_ <- QueryPrep_ls$Dir # Can be a vector...
  #Get data from table
  Tb <- names(Tables_ls)
  Out_ls <- list()

  owd <- getwd()
  on.exit(setwd(owd))

  for (tb in Tb) {
    Out_ls[[tb]] <- list()
    Ds <- names(Tables_ls[[tb]])
    for (Loc in DstoreLocs_) {
      # Though written for a vector, currently only supporting one Datastore
      # Streamline this to use DatastorePath from within root.
      # ModelState; DstoreLoc will then no longer be a list.
      query.env <- new.env()
      query.env$ModelState_ls <- MS_ls[[Loc]]
      assignDatastoreFunctions(envir=query.env) # uses modelstate type
      HasTable <- checkTableExistence(tb, Group, envir=query.env)
      if (HasTable) {
        for (ds in Ds) {
          HasDataset <- checkDataset(ds, tb, Group, envir=query.env)
          if (HasDataset) {
            if (is.null(Out_ls[[tb]][[ds]])) {
              setwd(dirname(Loc)) # Work in Datastore parent directory
              Dset_ <-
                readFromTable(ds, tb, Group, ReadAttr = TRUE, envir=query.env)
              if ( !is.na(Tables_ls[[tb]][ds]) && Tables_ls[[tb]][ds] != "" ) { # NA or "" means use default units
                DsetType <- attributes(Dset_)$TYPE
                browser( expr = (is.null(DsetType)) )
                DsetUnits <- attributes(Dset_)$UNITS
                ToUnits <- Tables_ls[[tb]][ds]
                if ( !is.null(ToUnits) && is.na(ToUnits) ) ToUnits <- NULL # NULL and NA will mean the same
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
    # Try to convert extracted datasets to a data.frame
    # If that fails for some reason, return a list instead
    if ( length(unique(sapply(Out_ls[[tb]],length)))==1 ) {
      try.df <- try( data.frame(Out_ls[[tb]]) )
      if ( is.data.frame(try.df) ) Out_ls[[tb]] <- try.df
    }
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
#' @param Name a string identifying the name of the dataset to check the
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
isDatasetPresent <- function(Name, Table, Group, QueryPrep_ls) {
  # TODO: inefficient implementation: should cache the DatastorePath list in
  #  the QueryPrep_ls.
  MS_ls <- QueryPrep_ls$Listing;
  DstoreLocs <- QueryPrep_ls$Dir # Change so we only process one
  DatasetPresent <- FALSE
  for (loc in DstoreLocs) {
    query.env <- new.env()
    query.env$ModelState_ls <- MS_ls[[loc]]
    DatasetPresent <- DatasetPresent || checkDataset(Name, Table, Group, envir=query.env)
  }
  return( DatasetPresent )
}

#----------------------------
# Query Specification Helpers
# Identify whether symbol is an operator
isOperator <- function(Symbol) {
  Operators_ <- c("+", "-", "*", "/")
  deparse(Symbol) %in% Operators_
}
isOperand <- function(Symbol) {
  Functions_ <- c("sum", "count", "mean", "wtmean", "max", "min", "median", "c")
  Operators_ <- c("+", "-", "*", "/", "%in%")
  Comparators_ <- c("==", ">=", "<=", "!=", ">", "<", "&", "|", "!")
  Group_ <- c("(", ")", "[", "]")
  NonOperands_ <- c(Functions_, Operators_, Comparators_, Group_)
  !(deparse(Symbol) %in% NonOperands_) & !is.character(Symbol) & !is.numeric(Symbol) & !is.logical(Symbol)
}
# Identify whether symbol is a function
isFunction <- function(Symbol) {
  Functions_ <- c("sum", "count", "mean", "wtmean", "max", "min", "median")
  deparse(Symbol) %in% Functions_
}

# Identify whether symbol is an operand
# Recursive function to get the operands in an expression
getOperands <- function(AST) {
  if (length(AST) == 1) {
    if (isOperand(AST)) deparse(AST)
  } else {
    unlist(lapply(AST, function(x) getOperands(x)))
  }
}
# Recursive function to get functions in an expression
getFunctions <- function(AST) {
  if (length(AST) == 1) {
    if (isFunction(AST)) deparse(AST)
  } else {
    unlist(lapply(AST, function(x) getFunctions(x)))
  }
}
# Recursive function to get operators in an expression
getOperators <- function(AST) {
  if (length(AST) == 1) {
    if (isOperator(AST)) deparse(AST)
  } else {
    unlist(lapply(AST, function(x) getOperators(x)))
  }
}

#CHECK A QUERY SPECIFICATION
#===========================
#' Verify parameters proposed for summarizing a Dataset
#'
#' \code{checkQuerySpec} a visioneval framework query user function that locates errors in a
#' specification that might be used to summarize a Dataset.  See \code{summarizeDatasets}.
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
#' @param Units a named character vector identifying the units to be used for
#'   each operand in 'Expr' and each 'By' dataset. The element names
#'   are the operands identified in the expression and any dataset names
#'   identified in the 'By' argument. The element values are the units that the
#'   data are to be converted to when retrieved from the datastore. If no
#'   conversion is required (i.e. retaining the units as they are in the
#'   datastore), set the value equal to "". The specified units value for a
#'   dataset must be consistent with the data type of the dataset. Note that the
#'   'documentDatastoreTables' function can be used to document all the datasets
#'   in a datastore including their unit.
#' @param By an optional character vector identifying the names of the datasets
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
#'   unless one or more datasets specified for the 'By' parameter contain
#'   non-integer numeric values. The names of the list components must be the
#'   same as names of the numeric datasets identified in the 'By' vector. Each
#'   named component of the list is a vector of values to be used to split the
#'   respective By dataset into groups. Minimum and maximum values do not need
#'   to be specified as they are computed from the dataset.
#' @param Table a string or named list identifying the datastore table(s) where
#'   the datasets identified in the 'Expr' argument and 'By' argument are
#'   located. If all datasets are located in the same table, then the value of
#'   'Table' should be a string. If the datasets are located in more than one
#'   table, then the value of 'Table' must be a named list where the names are
#'   the names of the tables where the datasets are located and the respective
#'   values are character vectors identifying the names of the datasets located
#'   in each of the tables. Note that if the 'By' argument is not NULL and no
#'   keys for merging table datasets are identified (see 'Key' below) then the
#'   datasets identified in the 'By' argument must be included in the
#'   identification of datasets for every table. However, if keys for merging
#'   table datasets are identified, then each dataset identified in the 'By'
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
#' @param QuerySpec an optional named list whose elements have the same possible
#'   names as the other parameters of this function. If other parameters are also
#'   provided, their values will replace the ones in this list. So this function
#'   can be used to compose and check a Query Specification based on an existing
#'   and presumably working specification simply by replacing some of its elements.
#' @return A list of three components: "QuerySpec" which contains the assembled Query
#'   Specification, "Errors" which is a character vector of reported errors (or a vector of length
#'   one containing an empty string), and "Compiled" which contains everything needed to process the
#'   Query Specification against a Datastore.
#' @export
checkQuerySpec <- function(
    Expr = NULL,
    Units = NULL,
    By = NULL,
    Breaks_ls = NULL,
    Table = NULL,
    Key = NULL,
    QuerySpec = list()
  ) {

  # Based on QuerySpec, override with named function parameters
  params <- list( Expr = Expr, Units = Units,
    By = By, Breaks_ls = Breaks_ls, Table = Table,
    Key = Key
  )
  params <- params[ ! sapply(params,is.null) ]
  if ( length(params) > 0 ) QuerySpec[ names(params) ] <- params;

  # By, Breaks_ls and Key are optional
  # Expr, Units, and Table are required
  if ( ! all( found<-((reqd<-c("Expr","Units","Table")) %in% names(QuerySpec))) ) {
    Errors_ <- c(
      "Required Query Specification elements are missing:",
      paste( reqd[!found],collapse=", ")
    )
    return( list(CompiledSpec=QuerySpec, Errors = Errors_) )
  }

  # Run the original function, but save all the created variables
  #  in case they are needed later when the Query is performed
  CompiledSpec <- within( QuerySpec,
    {
      backstop <- FALSE
      repeat {
        # Examine the Expr for correctness
        if ( backstop ) break else backstop <- TRUE

        #Identify the operands of the Expr
        Operands_ <- unique(getOperands(str2lang(Expr)))

        #Identify the functions of the Expr
        Functions_ <- unique(getFunctions(str2lang(Expr)))

        #Identify the operators of the Expr
        Operators_ <- unique(getOperators(str2lang(Expr)))
        #------------------------------
        #Check Units and By arguments
        #------------------------------
        #Check that all operands have units
        if (!all(Operands_ %in% names(Units))) {
          Errors_ <- c(
            "Some of the operands in the expression don't have specified units.",
            paste( Operands_[! Operands_ %in% names(Units)], sep=", " )
          )
          break
        }
        #Check that all datasets named in the By argument have units
        if (!all(By %in% names(Units))) {
          Errors_ <- "Some datasets in the 'By' argument don't have specified units."
          break
        }
        #Check the not more than 2 By datasets
        if (length(By) > 2) {
          Errors_ <- "More than 2 'By' arguments are not current supported."
          break
        }
        #---------------------------------------------------------------
        #Check that if Table is a list, check for accounting of datasets
        #---------------------------------------------------------------
        if (is.list(Table)) {
          DsetNames_ <- as.vector(unlist(Table))
          MsngNames_ <- names(Units)[!(names(Units) %in% DsetNames_)]
          #Check for missing names
          if (length(MsngNames_) != 0) {
            Errors_ <- c(
              "Missing datasets required to compute 'By' categories:",
              paste(MsngNames_, collapse = ", ")
            )
            break
          }
          #Check for duplicated names not used as keys
          DupNames_ <- DsetNames_[duplicated(DsetNames_)]
          if (length(DupNames_) != 0 & !is.null(Key)) {
            Errors_ <- c(
              "Tables joined by 'Key' argument contain duplicated Datasets:",
              paste(DupNames_, collapse = ", "),
              "Duplicate names will cause problems with the 'By' argument",
              "Change the 'Table' argument so there is only one instance of each name."
              # "This error may have occurred because you listed the",
              # "names of datasets that are to be used as keys for joining the",
              # "datasets from different tables.",
              # "Key datasets must not be listed in the 'Table' argument.",
              # "They are listed in the 'Key' argument instead."
            )
            break
          }
        } else if (!is.list(Table) & length(Table) != 1) {
          #Check that user did not specify as vector of tables
          Errors_ <- c(
            "More than one table is identified but not as a list.",
            "The 'Table' argument must be a list where the names are the names of the tables,",
            "and the values are names of the datasets to be retrieved."
          )
          break
        }
        #-----------------------------------------------------------------
        #Check that keys are supplied for joining datasets from all tables
        #-----------------------------------------------------------------
        #If Table is not a list, then there should be no keys
        if (!is.list(Table)) {
          if (!is.null(Key)) {
            Errors_ <- c(
              "All the datasets appear to come from one table",
              "A 'Key' has been provided for another Table",
              "'Key' is only needed if the datasets in the 'Expr' and 'By' argument are from different tables"
            )
            break
          }
        }
        #If Table is a list, then check the Key argument for consistency
        if (is.list(Table) & !is.null(By)) {
          #If Key is NULL, check that By datasets are specified in all tables
          if (is.null(Key)) {
            HasBy <- unlist(lapply(Table, function(x) {
              all(By %in% x)
            }))
            if (!all(HasBy)) {
              TableNotHasBy <- names(HasBy)[!HasBy]
              Errors_ <- c(
                "When datasets come from more than one table, a 'Key' must be provided.",
                "No 'Key' has been provided.",
                "These tables do not include all 'By' datasets:",
                paste(TableNotHasBy, collapse = ", ")
              )
              break
            }
          }
          #Check that user did not specify as vector of keys
          if (!is.null(Key)) {
            if (!is.list(Key) & length(Key) != 1) {
              Errors_ <- c(
                "More than one 'Key' is identified but not as a list.",
                "Component names of the list are the names of the keys",
                "Component values are the tables to be joined with the respective key."
              )
              break
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
              Errors_ <- c(
                "One or more of the 'Key' parameters are not permitted:",
                paste(WrongNames_, collapse = ", "),
                "The permitted keys are:",
                paste(AllowedKeys_, collapse = ", ")
              )
              break
            }
          }
          #If Key is list, check that keys are identified for all tables
          if (!is.null(Key)) {
            if (is.list(Key)) {
              KeyNames_ <- as.vector(unlist(Key))
              MsngNames_ <- names(Table)[!(names(Table) %in% KeyNames_)]
              if (length(MsngNames_) != 0) {
                Errors_ <- c(
                  "Not all tables to be joined are listed by the 'Key' argument.",
                  "The names of the missing tables are:",
                  paste(MsngNames_, collapse = ", ")
                )
                break
              }
            }
          }
        }
        #Check that aggregating operations are correct
        #---------------------------------------------
        #If datasets from different tables are to be combined and are not being
        #merged, then the only combining operation allowed is summation.
        if (is.list(Table) & is.null(Key)) {
          OnlySummation <- all(c(Functions_, Operators_) %in% c("sum", "+"))
          if (!OnlySummation) {
            Errors_ <- c(
              "'Expr' uses datasets in different tables that are not being merged using 'Keys'.",
              "Only the 'sum' function and the '+' operator can be used in such a calculation expression.",
              "The specified expression includes other functions and/or operators",
              paste("Expr: ",Expr)
            )
            break
          }
        }
      }
    }
  )
  Errors_ <- CompiledSpec$Errors_
  CompiledSpec["Errors_"] <- NULL # Move Errors out of the spec
  return( list(CompiledSpec=CompiledSpec, Errors = Errors_) )
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
#' @param Units_ see \code{Units} (using the underscore works but is deprecated)
#' @param Units a named character vector identifying the units to be used for
#'   each operand in 'Expr' and each 'By' dataset. The element names
#'   are the operands identified in the expression and any dataset names
#'   identified in the 'By' argument. The element values are the units that the
#'   data are to be converted to when retrieved from the datastore. If no
#'   conversion is required (i.e. retaining the units as they are in the
#'   datastore), set the value equal to "". The specified units value for a
#'   dataset must be consistent with the data type of the dataset. Note that the
#'   'documentDatastoreTables' function can be used to document all the datasets
#'   in a datastore including their unit.
#' @param By_ see \code{By} (using the underscore works but is deprecated)
#' @param By an optional character vector identifying the names of the datasets
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
#'   unless one or more datasets specified for the 'By' parameter contain
#'   non-integer numeric values. The names of the list components must be the
#'   same as names of the numeric datasets identified in the 'By' vector. Each
#'   named component of the list is a vector of values to be used to split the
#'   respective By dataset into groups. Minimum and maximum values do not need
#'   to be specified as they are computed from the dataset.
#' @param Table a string or named list identifying the datastore table(s) where
#'   the datasets identified in the 'Expr' argument and 'By' argument are
#'   located. If all datasets are located in the same table, then the value of
#'   'Table' should be a string. If the datasets are located in more than one
#'   table, then the value of 'Table' must be a named list where the names are
#'   the names of the tables where the datasets are located and the respective
#'   values are character vectors identifying the names of the datasets located
#'   in each of the tables. Note that if the 'By' argument is not NULL and no
#'   keys for merging table datasets are identified (see 'Key' below) then the
#'   datasets identified in the 'By' argument must be included in the
#'   identification of datasets for every table. However, if keys for merging
#'   table datasets are identified, then each dataset identified in the 'By'
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
#' @param QuerySpec an optional named list whose elements have the same possible
#'   names as the other parameters of this function. If other parameters are also
#'   provided, their values will replace the ones in this list. So this function
#'   can be used to compose and check a Query Specification based on an existing
#'   and presumably working specification simply by replacing some of its elements.
#'   See the \code{VEQuery} package.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#'   function which identifies the datastore location(s), listing(s), and
#'   functions for listing and read the datastore(s).
#' @return If the By argument is NULL or has a length of 1, the value of the
#'   specified expression is calculated. Note that if the expression produces a
#'   vector of more than one number the entire vector of numbers will be
#'   returned. Users should check their expression to confirm that it will
#'   produce a single number if that is what is desired. Assuming that the
#'   expression produces a single value, if the 'By' argument only identifies
#'   one dataset to use for grouping, the function will return a vector of
#'   values. If the 'By' argument identifies two grouping datasets, the
#'   function will return a matrix of values.
#' @export
summarizeDatasets <-
function(
  Expr = NULL,
  Units_ = NULL,
  Units = Units_, # rotate toward using "pure" name without underscore
  By_ = NULL,
  By = By_,
  Breaks_ls = NULL,
  Table = NULL,
  Key = NULL,
  Group = NULL,
  QuerySpec = list(), # Just the "Summarize" sub-element from a VEQuery::VEQuerySpec
  QueryPrep_ls
) {

  # Check and prepare the Query Specification
  checkedSpec <- checkQuerySpec(
    Expr=Expr,Units=Units,
    By=By, Breaks_ls=Breaks_ls,
    Table=Table,Key=Key,
    QuerySpec
  )

  # Abort if the query cannot be interpreted
  if ( length(checkedSpec$Errors)>1 || any(nzchar(checkedSpec$Errors)) ) {
    writeLog( c(
      msg<-"Error(s) in Query Specification for SummarizeDatasets",
      checkedSpec$Errors), Level="error"
      )
    stop(msg)
  }
  CompiledSpec <- checkedSpec$CompiledSpec;
  if ( !is.null(Group) && ! "Group" %in% names(CompiledSpec) ) {
    CompiledSpec$Group <- Group
  }

  # Obtain the Datasets needed to run the query
  Datasets <- getQueryDatasets(CompiledSpec, QueryPrep_ls)
  if ( length(Datasets$Errors)>0 && any(nzchar(Datasets$Errors)) ) {
    writeLog(
      c(
        msg<-"Error(s) retrieving Datasets for Query",
        Datasets$Errors
      ), Level="error"
    )
    stop(msg)
  }
  Data_ls <- Datasets$Data_ls;

  # Perform the query computations
  calcResults <- performQuery( CompiledSpec, Data_ls )
  if ( length(calcResults$Errors)>1 || any(nzchar(calcResults$Errors)) ) {
    writeLog( c(
      msg<-"Error(s) performing SummarizeDatasets",
      calcResults$Errors), Level="error"
    )
    stop(msg)
  }
  return( calcResults$Result )
}

#----------------------------------------------------------
# Helper function to get datasets needed to process a Query
#----------------------------------------------------------
getQueryDatasets <- function(CompiledSpec, QueryPrep_ls) with ( CompiledSpec, {
  # Process differently depending on whether we have one Table or a list of them
  if (!is.list(Table)) {
    #-----------------------------------------------------------
    #Retrieve and format datasets if they are in a single tables
    #-----------------------------------------------------------
    #Get the datasets from the datastore
    Tables_ls <- list()
    Tables_ls[[Table]] <- Units
    Data_ls <- readDatastoreTables(Tables_ls, Group, QueryPrep_ls)
    #Stop if any of the datasets are missing
    if (length(Data_ls$Missing[[Table]]) != 0) {
      MissingDsets_ <- paste(names(Data_ls$Missing[[Table]]), collapse = ", ")
      Msg <- paste("Missing datasets in",paste0(Group,"/",Table,":"), MissingDsets_)
      return( list(Data_ls=NULL,Errors=Msg) )
    }
    #Simplify the data list
    Data_df <- Data_ls$Data[[Table]]
    Data_ls$Data <- list()
    Data_ls$Data[[1]] <- as.list(Data_df)
  } else  {
    # isTRUE(is.list(Table))
    #-------------------------------------------------------------------
    #Retrieve, merge, and format datasets if they are in multiple tables
    #-------------------------------------------------------------------
    #Initialize table specifications
    Tables_ls <- list()
    for (nm in names(Table)) {
      Tables_ls[[nm]] <- Units[Table[[nm]]]
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
      msg <- paste("Missing Table (Datasets):",paste(Missing_, collapse = ", "))
      return( list(Data_ls=NULL,Errors=Msg) )
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
      #If no By variables, combine all datasets into one list
      if (is.null(By)) {
        CombiData_ls <- do.call(c, lapply(Data_ls$Data, function(x) as.list(x)))
        names(CombiData_ls) <-
        gsub("^.*\\.", "", names(CombiData_ls)) # Remove the table part of the name
        Data_ls$Data <- list()
        Data_ls$Data[[1]] <- CombiData_ls
        rm(CombiData_ls)
        #If is By variables, expand all tables to include all operands
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
  return( list(Data_ls=Data_ls,Errors="") )
})

#-----------------------------------------------------------
#Helper function to calculate measures if By is specified
#-----------------------------------------------------------
calcWithBy <- function(CompiledQuery, CalcData_ls) with ( CompiledQuery,
  {
    #If there is a By argument do calculations by group and return as array
    By_ls <- list()
    #Check and process the By data into categories
    for (nm in By) {
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
          if (exists("Breaks_ls") && !is.null(Breaks_ls[[nm]])) {
            Breaks_ <- unique(c(min(ByData_), Breaks_ls[[nm]], max(ByData_)))
            By_ls[[nm]] <- cut(ByData_, Breaks_, include.lowest = TRUE)
          } else {
            By_ls[[nm]] <- as.factor(ByData_)
          }
        }
        if (is.double(ByData_) & !all(round(ByData_) == as.integer(ByData_))) {
          if (exists("Breaks_ls") && !is.null(Breaks_ls[[nm]])) {
            Breaks_ <- unique(c(min(ByData_), Breaks_ls[[nm]], max(ByData_)))
            By_ls[[nm]] <- cut(ByData_, Breaks_, include.lowest = TRUE)
          } else {
            msg <- (paste(nm, "is non-integer. Breaks must be specified."))
            return(list(Result=NA,Errors=msg))
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
    #Calculate values if length of By is 1
    if (length(By) == 1) {
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
    #Calculate values if length of By is 2
    if (length(By) == 2) {
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
    return(list(Result=Results_ar,Errors=""))
  }
)

#---------------------------------
# Functions for performing queries
#---------------------------------

#Count function
count <- function(x) length(x)

#Weighted mean function
wtmean <- function(x, w) sum(x * w) / sum(w)

# PERFORM A SPECIFIED QUERY
###########################
#'
#' Compute a metric by applying a query specification to a Datastore
#'
#' @param CompiledSpec a list as returned from checkQuerySpec
#' @param Data_ls a data specification as returned from getQueryDatasets
#' @return a list with two elements, Result and Errors. Result is the numeric result as either a
#' vector, a matrix or an array depending on the By value. Errors is a character vector. If it is
#' length zero or contains any non-empty strings, than an error condition has been specified.
#' @export
performQuery <- function( CompiledSpec, Data_ls )
with( CompiledSpec,
  {
    #----------------------------------------------
    #Calculate the Expression and Return the Result
    #----------------------------------------------
    if ( "By" %in% names(CompiledSpec) ) {
      # There is a By argument, with one or two merged datasets
      if ( length(Data_ls$Data) == 1 ) {
        #If there is a By but only one merged dataset
        calcResults <- calcWithBy(CompiledSpec,Data_ls$Data[[1]])
        if ( length(calcResults$Errors)>0 && any(nzchar(calcResults$Errors)) ) {
          return( list(Result=NA,Errors=calcResults$Errors) )
        } else {
          return( calcResults )
        }
      } else if (length(Data_ls$Data) > 1) {
        #If there is a By but several tables, calculate results for each table
        calcResults_ls <- lapply(
          Data_ls$Data,
          function(x) {d
            calcResults <- calcWithBy(CompiledSpec,x)
            if ( length(calcResults$Errors)>0 && any(nzchar(calcResults$Errors)) ) {
              return( list(Result=NA,Errors=calcResults$Errors) )
            } else return( calcResults )
          }
        )
        # Assemble Errors
        Errors_ <- character(0)
        for ( res in calcResults_ls ) {
          if ( length(res$Errors)>0 && any(nzchar(res$Errors)) ) {
            Errors_ <- c(Errors_,res$Errors)
          }
        }
        if ( length(Errors_)>0 && any(nzchar(Errors_)) ) {
          return( list(Result=NA,Errors=Errors_) )
        } else {
          Results_ls <- lapply( calcResults_ls, function(r) r$Result )
        } 

        #Check that they conform if 1 By variable
        if (length(By) == 1) {
          Names_ls <- lapply(Results_ls, function(x) names(x))
          AllNames_ <- sort(unique(unlist(Names_ls)))
          Results_ls <- lapply(Results_ls, function(x) {
            Vals_ <- setNames(rep(NA, length(AllNames_)), AllNames_)
            Vals_[names(x)] <- x
            Vals_
          })
        }

        #Check that they conform if 2 By variables (e.g. geography and breaks)
        if (length(By) == 2) {
          # TODO: Unimplemented...
          NULL # Not doing this check...
        }
        Result <- do.call("+", Results_ls)
      }
    } else {
      Result <- eval(parse(text = Expr), envir = Data_ls$Data[[1]])
    }
    # Return results
    return(list(Result=Result,Errors=""))
  }
)

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
#   Units = c(
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
#   Units = c(
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
#   Units = c(
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
#   Units = c(
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
#   Units = c(
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
#     Units = c(
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
#   Units = c(
#     Vehicles = "VEH",
#     HhSize = "PRSN"
#   ),
#   By = "HhSize",
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#
# #Specify breaks for establish household size groups
# summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   By = c("HhSize"),
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
#   Units = c(
#     NumAuto = "VEH",
#     Drivers = "PRSN",
#     Income = "USD",
#     HhSize = "PRSN"
#   ),
#   By = c(
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
#   Units = c(
#     PaysForParking = "",
#     Marea = "",
#     AreaType = ""
#   ),
#   By = c("Marea", "AreaType"),
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
#   Units = c(
#     PaysForParking = "",
#     AreaType = "",
#     Income = ""
#   ),
#   By = c("AreaType", "Income"),
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
#   Units = c(
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
#   By = "Marea",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
