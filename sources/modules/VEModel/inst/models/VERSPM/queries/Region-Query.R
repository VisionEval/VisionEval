# TODO: Implement these for VEQuery

#CALCULATE SUMMARY MEASURES FOR A MODEL REGION
#=============================================
#' Calculate summary measures for a region and organize results in a table.
#'
#' \code{calcRegionSummaryMeasures} a visioneval framework query user function that
#' calculates summary measures for a model region for specified model run years and
#' organizes the results in a table.
#'
#' This function computes summary measures for a model region for specified
#' model run years and organizes the results in a table. The function calls the
#' 'summarizeDatasets' function to calculate measures that are specified in a
#' JSON-formatted file. The measures are calculated for each specified model
#' run year and then are organized in a data frame that returned by the function
#' or that may be saved to a csv-formatted text file instead. The JSON-formatted
#' file which includes the specifications has a structure like the following
#' example which specifies the calculation of 2 measures:
#' \{
#'   "Datastore": \{
#'     "Path": "Datastore",
#'     "Type": "RD"
#'   \},
#'   "Measures": \{
#'     "Household_DVMT":\{
#'       "Calculation": "sum(Dvmt)",
#'       "Units": \{
#'         "Dvmt": "MI/DAY"
#'       \},
#'       "Table": "Household",
#'       "Description": "Total DVMT of households residing in the region."
#'     \},
#'     "Average_Per_Capita_DVMT":\{
#'       "Calculation": "sum(Dvmt) / sum(HhSize)",
#'       "Units": \{
#'       "HhSize": "PRSN",
#'       "Dvmt": "MI/DAY"
#'       \},
#'       "Table": "Household",
#'       "Description": "Total population residing in the region."
#'     \}
#'   \}
#' \}
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
#' @import jsonlite visioneval
#' @export
requireNamespace(jsonlite)
requireNamespace(visioneval)

calcRegionSummaryMeasures <- function(
  MeasuresDefFile,
  Years_,
  WriteTable = NULL
){
  #Set up
  #------
  MeasuresDef_ls <- jsonlite::fromJSON(MeasuresDefFile)
  QueryPrep_ls <- visioneval::prepareForDatastoreQuery(
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
      Value <- visioneval::summarizeDatasets(
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
