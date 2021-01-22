February# QueryDatastore Module
### February 13, 2020

# Documentation for the Query.R section of the visioneval framework - needs a better home!

This module defines functions used to query the Datastore and produce summary measures from the datastore. This documentation explains the overall approach to querying and summarization enabled by these functions, and the higher level functions to produce summaries. Refer to the function documentation for explanations of the lower level functions and details on all functions.

## Summary

These functions enable users to specify datasets to be read from the datastore and calculations to be performed on those datasets.
- Calculations are specified as standard R language expressions (see below)
- Calculations may be grouped: for example, measures calculated by Azone
- Groupings may be specified: for example, income groups may be specified
- Region summaries can be produced for a set of measures specified in a json-formatted definitions file

There are some limitations to the calculations that may be performed, however:
- All datasets identified in a calculation must be in the same table. The functions at the present time do not allow you to use datasets in different tables (e.g. Households and Vehicles) in a calculation.
- Summarization functions are limited to:
  - sum: sum the values in the dataset,
  - count: count the number of values in the dataset,
  - mean: calculate the arithmetic average of values in the dataset,
  - wtmean: calculate the weighted mean of values in the dataset
- Operators in the calculation expression are limited to '+', '-', '*', and '/'

Before any datasets can be retrieved and summarized, the 'prepareForDatastoreQuery' function must be called in order to establish information about the datastore location(s), type, and structure(s). This information is used by query and summarization functions. This function has 2 arguments:
- `DstoreLocs_`: a string vector identifying the paths to all of the datastores to extract the datasets from. Each entry must be the full relative path to a datastore (e.g. 'tests/Datastore'). Note that more than one datastore can be specified if the model run used datastore referencing. Datastore referencing however is an experimental feature and won't ordinarily be used and so the value for 'DstoreLocs_' will ordinarily be a single string.
- `DstoreType`: a string identifying the type of datastore (e.g. 'RD', 'H5'). Note that this will be the same value as the 'DatastoreType' property in the 'run_parameters.json' file for the model run.

The 'documentGroupDatasets' function enables the user to document all datsets in the tables in a datastore group and creates an Excel workbook where the dataset inventory for each table is in a separate worksheet. Each worksheet lists the names of the datasets, their types, their units, and descriptions. The function arguments are as follows:
- `Group`: a string identifying the name of the Group in the datastore.
- `File`: a string identifying the name of the Excel workbook file to save. The name must include the relative path information if it is to be saved in other than the current working directory.
- `QueryPrep_ls`: a list created by calling the prepareForDatastoreQuery function which identifies the datastore location(s), listing(s), and functions for listing and reading the datastore(s).

The 'summarizeDatasets' function enables the user to make a calculation on several datasets in a datastore table to produce a summary measure. Details are explained below.

The 'calcRegionSummaryMeasures' function allows the user to specify a number of region-level summary calculations in a json-formatted file and to either return the results in a data frame for further processing, or save in a csv-formatted file. This function call can be placed at the end of a model run script to automatically produce a table of output measures. This function calls the 'prepareForDatastoreQuery' and 'summarizeDatasets' functions as needed.

## summarizeDatasets

This function computes either the total, count, average, or weighted average values for a table dataset or groups of a table dataset. For example, the total income of households by Azone could be computed. The calculation expression is written in standard R language form as a string. The only functions that may be called are 'sum', 'count', 'mean', 'wtmean'. The only arithmetic operators that are supported are '+', '-', '*', and '/'. Note that all of the datasets specified in the calculation must be in the same table in the datastore. Example function calls illustrating different calculations follow the descriptions of the function arguments. The function arguments are as follows:
- `Expr`: a string specifying a calculation expression to use to summarize the datasets. Operands in the expression are the names of datasets to use to create the summary. The only operators that may be used are '+', '-', '*', and '/'. The only functions that may be used are 'sum', 'count', 'mean', and 'wtmean'. The 'sum', 'count', and 'mean' functions take one argument, the name of the dataset being summarized. The 'wtmean' function takes two arguments. The first argument is the dataset being summarized and the second argument is the dataset used for the weights. The calculation can include data indexing (subsetting) expressions. Note that all the datasets must be located in the table specified by the 'Table' argument. Also note that if indexing is used in the calculation expression and a string is used in the indexing expression, that string must be in single quotes. For example: `"HhSize[LocType == 'Urban']`
- `Units_`: a named character vector identifying the units to be used for each operand in the expression. The values are allowable VE units values. The names are the names of the datasets specified in the calculation expression. The vector must have an element for each dataset name in the expression. Setting the value equal to "" for an operand will use the units stored in the datastore.
- `By_`: a vector identifying the names of the datasets that are used to identify datasets to be used to group the expression calculation. If NULL, then the entire datasets are used in the calculation. Note that all the datasets must be located in the table specified by the 'Table' argument.
- `Breaks_ls`: a named list of vectors identifying the values to use for splitting numeric datasets into categories. The names must be the same as names of the datasets identified in the By_ vector. Each named component of the list is a vector of values to be used to split the respective 'By' dataset into groups. Minimum and maximum values do not need to be specified as they are computed from the dataset.
- `Table`: a string identifying the table where the datasets are located.
- `Group`: a string identifying the group where the dataset is located.
- `QueryPrep_ls`: a list created by calling the prepareForDatastoreQuery function which identifies the datastore location(s), listing(s), and functions for listing and read the datastore(s).

Following are examples of how this function may be called:

**Summing a dataset**
```summarizeDatasets(
   Expr = "sum(Dvmt)",
   Units_ = c(
     Dvmt = "MI/DAY"
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls
 )
```

**Converting units while summing a dataset**
```summarizeDatasets(
   Expr = "sum(Dvmt)",
   Units_ = c(
     Dvmt = "KM/DAY"
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls
 )
```

**Counting number of records in dataset**
Note: "" for units uses units stored in datastore
```summarizeDatasets(
   Expr = "count(HhSize)",
   Units_ = c(
     HhSize = ""
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls
 )
```

**Mean of dataset**
```summarizeDatasets(
   Expr = "mean(AveGPM)",
   Units_ = c(
     AveGPM = "GGE/MI"
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls
 )
```

**Weighted mean of dataset**
```summarizeDatasets(
   Expr = "wtmean(AveGPM, Dvmt)",
   Units_ = c(
     AveGPM = "GGE/MI",
     Dvmt = "MI/DAY"
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls
 )
```

**More complicated expression to calculate average autos per household**
```summarizeDatasets(
     Expr = "sum(NumAuto) / sum(HhSize)",
     Units_ = c(
       NumAuto = "VEH",
       HhSize = "PRSN"
     ),
     Table = "Household",
     Group = "2010",
     QueryPrep_ls = QPrep_ls)
```

**Including indexing in calculation**
```summarizeDatasets(
     Expr = "sum(NumAuto[LocType == 'Urban']) / sum(HhSize[LocType == 'Urban'])",
     Units_ = c(
       NumAuto = "VEH",
       HhSize = "PRSN",
       LocType = ""
     ),
     Table = "Household",
     Group = "2010",
     QueryPrep_ls = QPrep_ls)
```

**Calculate average autos per household by household size**
```summarizeDatasets(
   Expr = "sum(NumAuto) / sum(HhSize)",
   Units_ = c(
     NumAuto = "VEH",
     HhSize = "PRSN"
   ),
   By_ = "HhSize",
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls)
```

**Specify breaks for establish household size groups**
```summarizeDatasets(
   Expr = "sum(NumAuto) / sum(HhSize)",
   Units_ = c(
     NumAuto = "VEH",
     HhSize = "PRSN"
   ),
   By_ = "HhSize",
   Breaks_ls = list(
     HhSize = c(0,1,2,3,4)
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls)
```

**Split by household size and income groups**
```summarizeDatasets(
   Expr = "sum(NumAuto) / sum(HhSize)",
   Units_ = c(
     NumAuto = "VEH",
     HhSize = "PRSN"
   ),
   By_ = c(
     "HhSize",
     "Income"),
   Breaks_ls = list(
     HhSize = c(0,1,2,3,4),
     Income = c(20000, 40000, 60000, 80000, 100000)
   ),
   Table = "Household",
   Group = "2010",
   QueryPrep_ls = QPrep_ls)
```

## calcRegionSummaryMeasures

The 'calcRegionSummaryMeasures' function calculates a set of summary measures for a model region. It calls the 'summarizeDatasets' function repeatedly to calculate measures that are specifed in a json-formatted file. For each of the specified measures, a single value is computed for the region. In other words, there are no 'By_' arguments in the specifications. However, a portion of the region may be specified in the calculation expression using indexing. The function has the following 3 arguments:
- `MeasuresDefFile`: a string identifying the path name of the JSON-formatted file which specifes the measures to be calculated and the datastore to use. An example of such a file is listed below.
- `Years_`: a character vector identifying the model run years to calculate the measures for.
- `WriteTable`: a string identifying the path name of a file to write the table of measures to. If the value is NULL (the default), a data frame containing the results is returned from the function.

The json-formatted file which includes the specifications has a structure like the following example which specifies the calculation of 2 measures:
```{
   "Datastore": {
     "Path": "Datastore",
     "Type": "RD"
   },
   "Measures": {
     "Household_DVMT":{
       "Calculation": "sum(Dvmt)",
       "Units": {
         "Dvmt": "MI/DAY"
       },
       "Table": "Household",
       "Description": "Total DVMT of households residing in the region."
     },
     "Average_Per_Capita_DVMT":{
       "Calculation": "sum(Dvmt) / sum(HhSize)",
       "Units": {
       "HhSize": "PRSN",
       "Dvmt": "MI/DAY"
       },
       "Table": "Household",
       "Description": "Total population residing in the region."
     }
   }
 }
```

The 'Datastore' property identies the path name and type for the datastore. 'Path' names can either
be relative to the working directory or absolute. 'Type' identifies the type of the datastore. It
must be the same as the type property identified in the 'run_parameters.json' file for the model
run.

The 'Measures' property establishes specifications for all of the measures to be calculated. Each
measure is identified by a name that will be used to identify the name in the output table. The name
must be consistent with R language requirements for naming objects. For each named measure the
'Calculation', 'Units', 'Table', and 'Description' of the measure must be specified. The
'Calculation' property is an expression that meets the requirements of the 'Expr' parameter of the
'summarizeDatasets' function. The 'Units' property must be specified in a manner consistent with the
'Units_' parameter of the 'summarizeDatasets' function. The 'Table' property identifies the table in
the datastore where the datasets used in the calculation are located. The 'Description' property is
documentation of the measure that will be included in the output table.
