#================
#QueryDatastore.R
#================

#<doc>
## QueryDatastore Module
#### August 11, 2019
#
#This module defines functions used to query the Datastore and produce summary measures from the datastore. This documentation explains the overall approach to querying and summarization enabled by these functions, and the higher level functions to produce summaries. Refer to the function documentation for explanations of the lower level functions and details on all functions.
#
### Summary
#
#These functions enable users to specify datasets to be read from the datastore and calculations to be performed on those datasets.
#- Calculations are specified as standard R language expressions (see below)
#- Calculations may be grouped: for example, measures calculated by Azone
#- Groupings may be specified: for example, income groups may be specified
#- Region summaries can be produced for a set of measures specified in a json-formatted definitions file
#
#There are some limitations to the calculations that may be performed, however:
#- All datasets identified in a calculation must be in the same table. The functions at the present time do not allow you to use datasets in different tables (e.g. Households and Vehicles) in a calculation.
#- Summarization functions are limited to:
#  - sum: sum the values in the dataset,
#  - count: count the number of values in the dataset,
#  - mean: calculate the arithmetic average of values in the dataset,
#  - wtmean: calculate the weighted mean of values in the dataset
#- Operators in the calculation expression are limited to '+', '-', '*', and '/'
#
#Before any datasets can be retrieved and summarized, the 'prepareForDatastoreQuery' function must be called in order to establish information about the datastore location(s), type, and structure(s). This information is used by query and summarization functions. This function has 2 arguments:
#- `DstoreLocs_`: a string vector identifying the paths to all of the datastores to extract the datasets from. Each entry must be the full relative path to a datastore (e.g. 'tests/Datastore'). Note that more than one datastore can be specified if the model run used datastore referencing. Datastore referencing however is an experimental feature and won't ordinarily be used and so the value for 'DstoreLocs_' will ordinarily be a single string.
#- `DstoreType`: a string identifying the type of datastore (e.g. 'RD', 'H5'). Note that this will be the same value as the 'DatastoreType' property in the 'run_parameters.json' file for the model run.
#
#The 'documentGroupDatasets' function enables the user to document all datsets in the tables in a datastore group and creates an Excel workbook where the dataset inventory for each table is in a separate worksheet. Each worksheet lists the names of the datasets, their types, their units, and descriptions. The function arguments are as follows:
#- `Group`: a string identifying the name of the Group in the datastore.
#- `File`: a string identifying the name of the Excel workbook file to save. The name must include the relative path information if it is to be saved in other than the current working directory.
#- `QueryPrep_ls`: a list created by calling the prepareForDatastoreQuery function which identifies the datastore location(s), listing(s), and functions for listing and reading the datastore(s).
#
#The 'summarizeDatasets' function enables the user to make a calculation on several datasets in a datastore table to produce a summary measure. Details are explained below.
#
#The 'calcRegionSummaryMeasures' function allows the user to specify a number of region-level summary calculations in a json-formatted file and to either return the results in a data frame for further processing, or save in a csv-formatted file. This function call can be placed at the end of a model run script to automatically produce a table of output measures. This function calls the 'prepareForDatastoreQuery' and 'summarizeDatasets' functions as needed.
#
### summarizeDatasets
#
#This function computes either the total, count, average, or weighted average values for a table dataset or groups of a table dataset. For example, the total income of households by Azone could be computed. The calculation expression is written in standard R language form as a string. The only functions that may be called are 'sum', 'count', 'mean', 'wtmean'. The only arithmetic operators that are supported are '+', '-', '*', and '/'. Note that all of the datasets specified in the calculation must be in the same table in the datastore. Example function calls illustrating different calculations follow the descriptions of the function arguments. The function arguments are as follows:
#- `Expr`: a string specifying a calculation expression to use to summarize the datasets. Operands in the expression are the names of datasets to use to create the summary. The only operators that may be used are '+', '-', '*', and '/'. The only functions that may be used are 'sum', 'count', 'mean', and 'wtmean'. The 'sum', 'count', and 'mean' functions take one argument, the name of the dataset being summarized. The 'wtmean' function takes two arguments. The first argument is the dataset being summarized and the second argument is the dataset used for the weights. The calculation can include data indexing (subsetting) expressions. Note that all the datasets must be located in the table specified by the 'Table' argument. Also note that if indexing is used in the calculation expression and a string is used in the indexing expression, that string must be in single quotes. For example: `"HhSize[LocType == 'Urban']`
#- `Units_`: a named character vector identifying the units to be used for each operand in the expression. The values are allowable VE units values. The names are the names of the datasets specified in the calculation expression. The vector must have an element for each dataset name in the expression. Setting the value equal to "" for an operand will use the units stored in the datastore.
#- `By_`: a vector identifying the names of the datasets that are used to identify datasets to be used to group the expression calculation. If NULL, then the entire datasets are used in the calculation. Note that all the datasets must be located in the table specified by the 'Table' argument.
#- `Breaks_ls`: a named list of vectors identifying the values to use for splitting numeric datasets into categories. The names must be the same as names of the datasets identified in the By_ vector. Each named component of the list is a vector of values to be used to split the respective 'By' dataset into groups. Minimum and maximum values do not need to be specified as they are computed from the dataset.
#- `Table`: a string identifying the table where the datasets are located.
#- `Group`: a string identifying the group where the dataset is located.
#- `QueryPrep_ls`: a list created by calling the prepareForDatastoreQuery function which identifies the datastore location(s), listing(s), and functions for listing and read the datastore(s).
#
#Following are examples of how this function may be called:
#
#**Summing a dataset**
#```summarizeDatasets(
#   Expr = "sum(Dvmt)",
#   Units_ = c(
#     Dvmt = "MI/DAY"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#```
#
#**Converting units while summing a dataset**
#```summarizeDatasets(
#   Expr = "sum(Dvmt)",
#   Units_ = c(
#     Dvmt = "KM/DAY"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#```
#
#**Counting number of records in dataset**
#Note: "" for units uses units stored in datastore
#```summarizeDatasets(
#   Expr = "count(HhSize)",
#   Units_ = c(
#     HhSize = ""
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#```
#
#**Mean of dataset**
#```summarizeDatasets(
#   Expr = "mean(AveGPM)",
#   Units_ = c(
#     AveGPM = "GGE/MI"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#```
#
#**Weighted mean of dataset**
#```summarizeDatasets(
#   Expr = "wtmean(AveGPM, Dvmt)",
#   Units_ = c(
#     AveGPM = "GGE/MI",
#     Dvmt = "MI/DAY"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls
# )
#```
#
#**More complicated expression to calculate average autos per household**
#```summarizeDatasets(
#     Expr = "sum(NumAuto) / sum(HhSize)",
#     Units_ = c(
#       NumAuto = "VEH",
#       HhSize = "PRSN"
#     ),
#     Table = "Household",
#     Group = "2010",
#     QueryPrep_ls = QPrep_ls)
#```
#
#**Including indexing in calculation**
#```summarizeDatasets(
#     Expr = "sum(NumAuto[LocType == 'Urban']) / sum(HhSize[LocType == 'Urban'])",
#     Units_ = c(
#       NumAuto = "VEH",
#       HhSize = "PRSN",
#       LocType = ""
#     ),
#     Table = "Household",
#     Group = "2010",
#     QueryPrep_ls = QPrep_ls)
#```
#
#**Calculate average autos per household by household size**
#```summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   By_ = "HhSize",
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#```
#
#**Specify breaks for establish household size groups**
#```summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   By_ = "HhSize",
#   Breaks_ls = list(
#     HhSize = c(0,1,2,3,4)
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#```
#
#**Split by household size and income groups**
#```summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   By_ = c(
#     "HhSize",
#     "Income"),
#   Breaks_ls = list(
#     HhSize = c(0,1,2,3,4),
#     Income = c(20000, 40000, 60000, 80000, 100000)
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#```
#
### calcRegionSummaryMeasures
#
#The 'calcRegionSummaryMeasures' function calculates a set of summary measures for a model region. It calls the 'summarizeDatasets' function repeatedly to calculate measures that are specifed in a json-formatted file. For each of the specified measures, a single value is computed for the region. In other words, there are no 'By_' arguments in the specifications. However, a portion of the region may be specified in the calculation expression using indexing. The function has the following 3 arguments:
#- `MeasuresDefFile`: a string identifying the path name of the JSON-formatted file which specifes the measures to be calculated and the datastore to use. An example of such a file is listed below.
#- `Years_`: a character vector identifying the model run years to calculate the measures for.
#- `WriteTable`: a string identifying the path name of a file to write the table of measures to. If the value is NULL (the default), a data frame containing the results is returned from the function.
#
#The json-formatted file which includes the specifications has a structure like the following example which specifies the calculation of 2 measures:
#```{
#   "Datastore": {
#     "Path": "Datastore",
#     "Type": "RD"
#   },
#   "Measures": {
#     "Household_DVMT":{
#       "Calculation": "sum(Dvmt)",
#       "Units": {
#         "Dvmt": "MI/DAY"
#       },
#       "Table": "Household",
#       "Description": "Total DVMT of households residing in the region."
#     },
#     "Average_Per_Capita_DVMT":{
#       "Calculation": "sum(Dvmt) / sum(HhSize)",
#       "Units": {
#       "HhSize": "PRSN",
#       "Dvmt": "MI/DAY"
#       },
#       "Table": "Household",
#       "Description": "Total population residing in the region."
#     }
#   }
# }
#```
#
#The 'Datastore' property identies the path name and type for the datastore. 'Path' names can either be relative to the working directory or absolute. 'Type' identifies the type of the datastore. It must be the same as the type property identified in the 'run_parameters.json' file for the model run.
#
#The 'Measures' property establishes specifications for all of the measures to be calculated. Each measure is identified by a name that will be used to identify the name in the output table. The name must be consistent with R language requirements for naming objects. For each named measure the 'Calculation', 'Units', 'Table', and 'Description' of the measure must be specified. The 'Calculation' property is an expression that meets the requirements of the 'Expr' parameter of the 'summarizeDatasets' function. The 'Units' property must be specified in a manner consistent with the 'Units_' parameter of the 'summarizeDatasets' function. The 'Table' property identifies the table in the datastore where the datasets used in the calculation are located. The 'Description' property is documentation of the measure that will be included in the output table.
#
#</doc>

#Code for development/testing
# library(visioneval)
# library(filesstrings)


#=============================
#PREPARE FOR A DATASTORE QUERY
#=============================
#' Retrieve information needed to prepare a datastore query.
#'
#' \code{prepareForDatastoreQuery} retrieves datastore listings and functions
#' required to make a datastore query.
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
#' 'Listing' component is a list where each component is a datastore listing.
#' The 'Functions' component contains the appropriate functions for the
#' datastore type for listing the datastore contents and for reading datasets.
#' @export
prepareForDatastoreQuery <- function(DstoreLocs_, DstoreType) {
  #Initialize list to hold query preparation information
  Prep_ls <- list()
  #Check that DstoreTypes are supported
  AllowedDstoreTypes_ <- c("RD", "H5")
  if (!DstoreType %in% AllowedDstoreTypes_) {
    Msg <-
      paste0("Specified 'DatastoreType' in the 'run_parameters.json' file - ",
             DstoreType, " - is not a recognized type. ",
             "Recognized datastore types are: ",
             paste(AllowedDstoreTypes_, collapse = ", "), ".")
    stop(Msg)
  }
  #Check that DstoreLocs_ are correct and assign
  DstoreLocsExist_ <- sapply(DstoreLocs_, function(x) file.exists(x))
  if (any(!DstoreLocsExist_)) {
    Msg <-
      paste0("One or more of the specified DstoreLocs_ can not be found. ",
             "Maybe they are misspecified. Check the following: ",
             DstoreLocs_[!DstoreLocsExist_])
    stop(Msg)
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
    SplitRef_ <- unlist(strsplit(x, "/"))
    RefHead <- paste(SplitRef_[-length(SplitRef_)], collapse = "/")
    if (RefHead == "") {
      ModelStateFile <- "ModelState.Rda"
    } else {
      ModelStateFile <- paste(RefHead, "ModelState.Rda", sep = "/")
    }
    readModelState(FileName = ModelStateFile)
  })
  names(Prep_ls$Listing) <- DstoreLocs_
  #Return the query preparation list
  Prep_ls
}


#LIST GROUPS
#===========
#' Lists the names of groups in model datastores.
#'
#' \code{listGroups} a function which lists the groups in a datastore
#' or datastores that contain data for a scenario.
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


#LIST TABLES IN GROUP
#====================
#' List names of tables in a group in a datastore.
#'
#' \code{listTables} a function which lists the tables in a group in a
#' datastore.
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


#LIST DATASETS IN GROUP
#======================
#' List names and descriptions datasets in a table in a datastore.
#'
#' \code{listTables} a function which lists the names and descriptions datasets
#' in a table in a datastore.
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


#CREATE DOCUMENTATION OF DATASETS IN A DATASTORE
#===============================================
#' Save an zip archive which documents all tables/datasets in a datastore.
#'
#' \code{documentDatastoreTables} saves a zip archive of a set of csv-formatted
#' text files which document tables in a datastore.
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
  Groups_ <- Groups_[-(Groups_ == "")]
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
#' \code{readDatastoreTables} a visioneval framework model user function that
#' reads datasets from one or more tables in a specified group in one or more
#' datastores
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
      ModelState_ls <<- QueryPrep_ls$Listing[[Loc]]
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
    rm(ModelState_ls, pos = 1)
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


#READ AND SUMMARIZE A DATASET
#============================
#' Summarize the values in a table dataset according to the values in another
#' dataset in the table.
#'
#' \code{summarizeDataset} summarize the values in a table dataset according to
#' the values in another dataset in the table.
#'
#' This function computes either the total, count, average, or weighted average
#' values for a table dataset or groups of a table dataset. For example, the
#' total income of households by Azone could be computed.
#'
#' @param Expr a string specifying an expression to use to summarize the
#' datasets. Operands in the expression are the names of datasets to use to
#' create the summary. The only operators that may be used are '+', '-', '*',
#' and '/'. The only functions that may be used are 'sum', 'count', 'mean', and
#' 'wtmean'. The calculation can include data indexing (subsetting) expressions.
#' Note that all the datasets must be located in the table specified by the
#' 'Table' argument.
#' @param Units_ a named character vector identifying the units to be used for
#' each operand in the expression. The values are allowable VE units values.
#' The names are the names of the operands in the expression. The vector must
#' have an element for each operand in the expression. Setting the value equal
#' to "" for an operand will use the units stored in the datastore.
#' @param By_ a vector identifying the names of the datasets that are used to
#' identify datasets to be used to group the expression calculation. If NULL,
#' then the entire datasets are used in the calculation. Note that all the
#' datasets must be located in the table specified by the 'Table' argument.
#' @param Breaks_ls a named list of vectors identifying the values to use for
#' splitting numeric datasets into categories. The names must be the same as
#' names of the datasets identified in the By_ vector. Each named component of
#' the list is a vector of values to be used to split the respective By
#' dataset into groups. Minimum and maximum values do not need to be specified
#' as they are computed from the dataset.
#' @param Table a string identifying the table where the datasets are located.
#' @param Group a string identifying the group where the dataset is located.
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#' function which identifies the datastore location(s), listing(s), and
#' functions for listing and read the datastore(s).
#' @return If the By_ argument is NULL or has a length of 1, the value of the
#' specified expression is calculated. Note that if the expression produces a
#' vector of more than one number the entire vector of numbers will be returned.
#' Users should check their expression to confirm that it will produce a single
#' number if that is what is desired. If the By_ argument is not null, values
#' will be returned for each group in the datasets specified in the By_
#' argument.
#' @export
summarizeDatasets <-
  function(
    Expr,
    Units_,
    By_ = NULL,
    Breaks_ls = NULL,
    Table,
    Group,
    QueryPrep_ls)
  {
    #Count function
    count <- function(x) length(x)
    #Weighted mean function
    wtmean <- function(x, w) sum(x * w) / sum(w)
    #Function to determine if is summary function in the expression
    # isFun <- function(Name) {
    #   Name %in% c("sum", "count", "mean", "wtmean")
    # }
    #Function to determine if is operator in the expression
    # isOp <- function(Name) {
    #   Name %in% c("+", "-", "*", "/")
    # }
    #Identify whether symbol is an operand
    isOperand <- function(Symbol) {
      Functions_ <- c("sum", "count", "mean", "wtmean")
      Operators_ <- c("+", "-", "*", "/")
      Comparators_ <- c("==", ">=", "<=", "!=", ">", "<")
      Group_ <- c("(", ")", "[", "]")
      NonOperands_ <- c(Functions_, Operators_, Comparators_, Group_)
      !(deparse(Symbol) %in% NonOperands_) & !is.character(Symbol)
    }
    #Function to get the operands in an expression
    # getOperands <- function(AST, Result = "") {
    #   if (length(AST) == 1) {
    #     if (!isFun(deparse(AST)) & !isOp(deparse(AST))) deparse(AST)
    #   } else {
    #     unlist(lapply(AST, function(x) getOperands(x)))
    #   }
    # }
    getOperands <- function(AST) {
      if (length(AST) == 1) {
        if (isOperand(AST)) deparse(AST)
      } else {
        unlist(lapply(AST, function(x) getOperands(x)))
      }
    }
    #Identify the operands of the Expr
    Operands_ <- getOperands(str2lang(Expr))
    #Check that all operands have units
    if (!all(Operands_ %in% names(Units_))) {
      stop("Some of the operands in the expression don't have specified units.")
    }
    #Add the By dataset names and units to Units_
    if (!is.null(By_)) {
      ByUnits_ <- rep("", length(By_))
      names(ByUnits_) <- By_
      # Remove ByUnits_ names that are duplicates of Units_ names
      ByUnits_ <- ByUnits_[!(names(ByUnits_) %in% names(Units_))]
      Units_ <- c(Units_, ByUnits_)
    }
    #Get the datasets from the datastore
    Tables_ls <- list()
    Tables_ls[[Table]] <- Units_
    Data_ls <- readDatastoreTables(Tables_ls, Group, QueryPrep_ls)
    #Stop if any of the datasets are missing
    if (length(Data_ls$Missing[[Table]]) != 0) {
      MissingDsets_ <- paste(Data_ls$Missing[[Table]], collapse = ", ")
      Msg <- paste("The following datasets are not present in the",
                   Table, "table", "in the", Group, "group:", MissingDsets_)
      stop(Msg)
    }
    #Simplify the data list
    Data_ls <- list(data.frame(Data_ls$Data[[Table]]))
    #If there is a By_ argument do calculations by group and return as array
    if (!is.null(By_)) {
      By_ls <- list()
      #Check and process the By data into categories
      for (nm in By_) {
        ByData_ <- Data_ls[[1]][[nm]]
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
      #Split Data_ls by the By_ls
      Data_ls <- split(Data_ls[[1]], By_ls)
      #Evaluate the expression for each component of Data_ls
      Results_ls <- lapply(Data_ls, function(x) {
        eval(parse(text = Expr), envir = x)
      })
      #Identify the dimension names for each By dimension
      ByNames_ls <- lapply(By_ls, function(x) unique(as.character(x)))
      #Set up array to store results
      Results_ar <- array(NA,
                          dim = unlist(lapply(ByNames_ls, length)),
                          dimnames = ByNames_ls)
      #Put results into array
      Idx_mx <- do.call(rbind, strsplit(names(Results_ls), "\\."))
      Results_ar[Idx_mx] <- unlist(Results_ls)
      return(Results_ar)
    }
    #If there isn't a By_ argument, do calculations and return a vector
    if (is.null(By_)) {
      unlist(lapply(Data_ls, function(x) {
        eval(parse(text = Expr), envir = x)
      }))
    }
  }

# #Examples of summarizing datasets
# #================================
# #Assumes Datastore of VE-RSPM in working directory
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
#     Dvmt = "MI/DAY"
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
# #More complicated expression to calculate average autos per household
# summarizeDatasets(
#     Expr = "sum(NumAuto) / sum(HhSize)",
#     Units_ = c(
#       NumAuto = "VEH",
#       HhSize = "PRSN"
#     ),
#     Table = "Household",
#     Group = "2010",
#     QueryPrep_ls = QPrep_ls)
#
# #Alternate expression to calculate average autos per household using weighted mean
# summarizeDatasets(
#   Expr = "wtmean(NumAuto / HhSize, HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#
# #Calculate average autos per household by household size
# summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
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
#   By_ = "HhSize",
#   Breaks_ls = list(
#     HhSize = c(0,1,2,3,4)
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)
#
# #Split by household size and income groups
# summarizeDatasets(
#   Expr = "sum(NumAuto) / sum(HhSize)",
#   Units_ = c(
#     NumAuto = "VEH",
#     HhSize = "PRSN"
#   ),
#   By_ = c(
#     "HhSize",
#     "Income"),
#   Breaks_ls = list(
#     HhSize = c(0,1,2,3,4),
#     Income = c(20000, 40000, 60000, 80000, 100000)
#   ),
#   Table = "Household",
#   Group = "2010",
#   QueryPrep_ls = QPrep_ls)


#CALCULATE SUMMARY MEASURES FOR A MODEL REGION
#=============================================
#' Calculate summary measures for a region and organize results in a table.
#'
#' \code{calcRegionSummaryMeasures} calculates summary measures for a model
#' region for specified model run years and organizes the results in a table.
#'
#' This function computes summary measures for a model region for specified
#' model run years and organizes the results in a table. The function calls the
#' 'summarizeDatasets' function to calculate measures that are specified in a
#' JSON-formatted file. The measures are calculated for each specified model
#' run year and then are organized in a data frame that returned by the function
#' or that may be saved to a csv-formatted text file instead. The JSON-formatted
#' file which includes the specifications has a structure like the following
#' example which specifies the calculation of 2 measures:
#' {
#'   "Datastore": {
#'     "Path": "Datastore",
#'     "Type": "RD"
#'   },
#'   "Measures": {
#'     "Household_DVMT":{
#'       "Calculation": "sum(Dvmt)",
#'       "Units": {
#'         "Dvmt": "MI/DAY"
#'       },
#'       "Table": "Household",
#'       "Description": "Total DVMT of households residing in the region."
#'     },
#'     "Average_Per_Capita_DVMT":{
#'       "Calculation": "sum(Dvmt) / sum(HhSize)",
#'       "Units": {
#'       "HhSize": "PRSN",
#'       "Dvmt": "MI/DAY"
#'       },
#'       "Table": "Household",
#'       "Description": "Total population residing in the region."
#'     }
#'   }
#' }
#'
#' The 'Datastore' property identies the path name and type for the datastore.
#' 'Path' names can either be relative to the working directory or absolute.
#' 'Type' identifies the type of the datastore. It must be the same as the type
#' property identified in the 'run_parameters.json' file for the model run.
#'
#' The 'Measures' property establishes specifications for all of the measures to
#' be calculated. Each measure is identified by a name that will be used to
#' identify the name in the output table. The name must be consistent with R
#' language requirements for naming objects. For each named measure the
#' 'Calculation', 'Units', 'Table', and 'Description' of the measure must be
#' specified. The 'Calculation' property is an expression that meets the
#' requirements of the 'Expr' parameter of the 'summarizeDatasets' function. The
#' 'Units' property must be specified in a manner consistent with the 'Units_'
#' parameter of the 'summarizeDatasets' function. The 'Table' property
#' identifies the table in the datastore where the datasets used in the
#' calculation are located. The 'Description' property is documentation of the
#' measure that will be included in the output table.
#'
#' @param MeasuresDefFile a string identifying the path name of the JSON-formatted
#' file which specifes the measures to be calculated and the datastore to use.
#' @param Years_ a character vector identifying the model run years to calculate
#' the measures for.
#' @param WriteTable a string identifying the path name of a file to write the
#' table of measures to. If the value is NULL (the default), a data frame
#' containing the results is returned from the function.
#' @return If the 'WriteTable' argument is NULL, the function returns a data
#' frame containing the results. If the path name of a file is specified the
#' results are written out the the specified file and no results are returned
#' by the function.
#' @import jsonlite
#' @export
calcRegionSummaryMeasures <- function(
  MeasuresDefFile,
  Years_,
  WriteTable = NULL
){
  #Set up
  #------
  MeasuresDef_ls <- fromJSON(MeasuresDefFile)
  QueryPrep_ls <- prepareForDatastoreQuery(
    DstoreLocs_ = MeasuresDef_ls$Datastore$Path,
    DstoreType = MeasuresDef_ls$Datastore$Type
  )
  Measures_ <- names(MeasuresDef_ls$Measures)

  #Calculate measures for each year
  #--------------------------------
  Measures_ls <- list()
  for (Year in Years_) {
    Measures_ls[[Year]] <- list()
    for (Name in Measures_) {
      Value <- summarizeDatasets(
        Expr = MeasuresDef_ls$Measures[[Name]]$Calculation,
        Units_ = unlist(MeasuresDef_ls$Measures[[Name]]$Units),
        Table = MeasuresDef_ls$Measures[[Name]]$Table,
        Group = Year,
        QueryPrep_ls = QueryPrep_ls
      )
      Measures_ls[[Year]][[Name]] <- list(
        Name = Name,
        Value = Value,
        Description = MeasuresDef_ls$Measures[[Name]]$Description
      )
    }
  }

  #Reformat for output
  #-------------------
  Out_ls <- list()
  Out_ls$Measure = names(Measures_ls[[Years_[1]]])
  for (Year in Years_) {
    Out_ls[[Year]] = unlist(lapply(Measures_ls[[Year]], function(x) x$Value))
  }
  Out_ls$Description = unlist(lapply(Measures_ls[[1]], function(x) x$Description))
  Out_df <- data.frame(Out_ls)
  names(Out_df) <- c("Measure", Years_, "Description")
  rownames(Out_df) <- NULL

  #Either return the dataframe or write to a csv file
  if (!is.null(WriteTable)) {
    write.table(Out_df, file = WriteTable, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    Out_df
  }
}


#DOCUMENT THE MODULE
#-------------------
documentModule("QueryDatastore")
