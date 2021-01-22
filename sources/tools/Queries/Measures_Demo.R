#Measures_Demo.R

#-----------------------------------------
# visioneval has the query functions in it
#-----------------------------------------
requireNamespace("visioneval")

#---------------------------
#Prepare for datastore query
#---------------------------
#Creates a list which includes:
#1) Path to datastore
#2) Functions for reading data given the datastore type
#3) Datastore listing 
QPrep_ls <- visioneval::prepareForDatastoreQuery(
  DstoreLocs_ = "Datastore", 
  DstoreType = "RD")

#----------------------------------
#Make an inventory of the datastore
#----------------------------------
#This creates a zip archive which documents all the datasets in the datastore.
#Archive is organized by group. Within each group folder is a set of CSV files,
#one for each table in the group. Each CSV file lists the datasets included in
#the table giving the dataset name, data type, units, and description.
visioneval::documentDatastoreTables(
  SaveArchiveName = "DatastoreDocumentation", 
  QueryPrep_ls = QPrep_ls)

#------------------------------------------------------
#Retrieving datasets from one or more tables in a group
#------------------------------------------------------
#This is the basic workhorse function which simplifies the process of
#retrieving one or more datasets from one or more groups in a datastore. If
#you can't calculate a desired measure more directly using the
#"summarizeDataset" function, you can use the "readDatastoreTables" function to
#retrieve the datasets you need for calculating the measure and then write code
#to calculate the measure.

#Develop named list of tables and datasets.
#The named components are tables
#Each component is a named vector where the names are the names of datasets
#and the values are the units that the data is to be retrieved in
#"" means retrieve the data in the units used in the datastore.
#Example:
TablesRequest_ls <- list(
  Household = c(
    Bzone = "",
    HhSize = "",
    AveCO2ePM = "GM/MI",
    Income = "",
    Dvmt = "MI/YR",
    Nonsense = ""),
  Bzone = c(
    Bzone = "",
    D1B = "PRSN/ACRE",
    MFDU = "",
    SFDU = "",
    Nonsense = "")
)

#Call the readDatastoreTables function using the list of requested tables and
#datasets
TableResults_ls <-
  visioneval::readDatastoreTables(
    Tables_ls = TablesRequest_ls,
    Group = "2020",
    QueryPrep_ls = QPrep_ls
  )

#The readDatastoreTables function returns a list having two named components:
#"Data" and "Missing"
#The "Data" component is a named list where each named component corresponds
#to a requested table and the value is a data frame containing the requested
#datasets in the table.
lapply(TableResults_ls$Data, function(x) head(x))
#The "Missing" component is a named list where each component identifies
#datasets that are missing from each table.
TableResults_ls$Missing

#--------------------------------------------
#IMPORTANT NOTE ON DEFAULT UNITS IN DATASTORE
#--------------------------------------------
#Note that the units data are stored in in the datastore are determined by the
#"units.csv" file in the "defs" directory. For example, if the default
#time unit is "DAY" and the default distance unit is "MI", the default speed
#values in the datastore will be "MI/DAY" rather than "MI/HR". Also note that 
#units in the description of a dataset in the inventory might not be consistent 
#(needs to be fixed in the future by eliminating specific units from
#dataset descriptions).

#----------------------------
#CALCULATING SUMMARY MEASURES
#----------------------------
#The "summarizeDatasets" function enables summary measures to be calculated 
#from one or more datasets in one or more tables in a datastore. All must be in
#the same group however.
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
#'   
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
#'   
#' @param By_ an optional character vector identifying the names of the datasets
#'   to use for grouping the expression calculation. The default value is NULL
#'   (no grouping is done). If one dataset is identified, the function returns a
#'   vector of values by group. If two datasets are identified, the function
#'   returns a matrix of values with rows corresponding to groups of the first
#'   listed dataset and the columns corresponding to groups of the second listed
#'   dataset. No more than 2 datasets may be listed. Note that if an non-integer
#'   numeric dataset is to be used for grouping, values for splitting the
#'   values into categories must be specified in the 'Breaks_ls' argument.
#'   
#' @param Breaks_ls a named list of vectors identifying the values to use for
#'   splitting numeric datasets into categories. This parameter is optional
#'   unless one or more datasets specified for the 'By_' parameter contain
#'   non-integer numeric values. The names of the list components must be the
#'   same as names of the numeric datasets identified in the 'By_' vector. Each
#'   named component of the list is a vector of values to be used to split the
#'   respective By dataset into groups. Minimum and maximum values do not need
#'   to be specified as they are computed from the dataset.
#'   
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
#'   
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
#'   
#' @param Group a string identifying the datastore group where the dataset is
#'   located.
#'   
#' @param QueryPrep_ls a list created by calling the prepareForDatastoreQuery
#'   function which identifies the datastore location(s), listing(s), and
#'   functions for listing and read the datastore(s).
#'   
#' @return If the By_ argument is NULL or has a length of 1, the value of the
#'   specified expression is calculated. Note that if the expression produces a
#'   vector of more than one number the entire vector of numbers will be
#'   returned. Users should check their expression to confirm that it will
#'   produce a single number if that is what is desired. Assuming that the
#'   expression produces a single value, if the 'By_' argument only identifies
#'   one dataset to use for grouping, the function will return a vector of
#'   values. If the 'By_' argument identifies two grouping datasets, the
#'   function will return a matrix of values.

#Example of calculating the car service DVMT for urban households by Marea
#Total household DVMT is in the Household table while the proportion of
#household DVMT by vehicle access type is in the Vehicle table. The two tables
#are joined using the HhId.
visioneval::summarizeDatasets(
  Expr = "sum((Dvmt * DvmtProp)[LocType == 'Urban' & VehicleAccess %in% c('LowCarSvc', 'HighCarSvc')])",
  Units_ = c(
    Dvmt = "",
    LocType = "",
    Marea = "",
    VehicleAccess = "",
    DvmtProp = ""
  ),
  By_ = "Marea",
  Table = list(
    Household = c("Dvmt", "LocType", "Marea"),
    Vehicle = c("VehicleAccess", "DvmtProp")
  ),
  Key = "HhId",
  Group = "2020",
  QueryPrep_ls = QPrep_ls
)
