# scenarios.R
#' @include environment.R
#' @include models.R
NULL

# Documentation for VEModelScenarios
#' VEModelScenarios class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEModelScenarios
NULL

self=private=NULL

# Build a scenario management object
ve.scenario.init <- function( baseModel=NULL, create=FALSE, startFrom=NULL ) {
  self$baseModel <- baseModel
  self$scenarioDir <- self$baseModel$setting("ScenarioDir")
  self$configFile <- self$baseModel$setting("ScenarioConfig")
  if ( create && ! dir.exists(scenarioDir) ) {
    self$scenarioPath <- normalizePath(baseModel$modelPath,self$scenarioDir)
    dir.create(scenarioDir)
  }
  if ( dir.exists(scenarioDir) ) {
    scenarioConfigPath <- file.path(scenarioPath,self$configFile)
    self$load(startFrom=startFrom,scenarioPath=scenarioConfigPath)
  }
}

# Load scenario configuration; build by default
ve.scenario.load <- function(startFrom=NULL,build=TRUE,scenarioPath=NULL) {
  if ( dir.exists(scenarioPath) ) {
    # Note: ScenarioConfig can contain "Years" for scenarios
    # reload if file is present
    self$scenarioConfig <- visioneval::loadConfiguration(ParamPath=scenarioPath, mustWork=FALSE)
  } else {
    self$scenarioConfig <- NULL
  }
  # re-process the setup that may just have been loaded
  if ( ! is.null(self$scenarioConfig) ) {
    # Rebuild scenario configuration
    if ( build || is.character(startFrom) ) { # Possibly reset startFrom
      build <- TRUE
      self$build(startFrom=startFrom)         # re-create model stages from scenario configuration
    }
    # Create model stage objects for each scenario
    scenarioStages <- self$scenarioConfig$Scenarios$ModelStages # NA if undefined
    if ( is.list(scenarioStages) ) {
      # (Re-)create the scenario modelStages
      self$scenarioStages <- lapply(names(scenarioStages), # Use pre-defined structures
        function(stage) {
          obj <- scenarioStages[[stage]] # Get the stageParam_ls structure
          writeLog(paste("Scenario Stage:",stage),Level="info")
          VEModelStage$new(
            Name=stage,
            Model=self$baseModel,
            stageParam_ls=obj
          )
        }
      )
    }
    if ( build ) self$save()
  } else {
    self$scenarioStages <- list()
  }
  return(self)
}

# Return the scenario ModelStages
# Note that they must already have been built and loaded
ve.scenario.stages <- function() {

  return( self$scenarioStages )
}

# Print summary information about scenarios
ve.scenario.print <- function(details=FALSE) {
  # does self$scenarioConfig exist?
  # list its folder scenarios (just the name)
  # list its categories (how many files and levels in each)
  # list number of ModelStages present in self$scenarioConfig (from build)
  # details adds in:
  #   under categories, list files and levels
  #   under model stages, list how many are in each run status
}

# List available inputs for each scenario and (if details) whether it has a local version
ve.scenario.inputs <- function(scenario=NULL,category=NULL,details=TRUE) {
  # if "scenario" is a character vector, only show those folder scenarios
  # if "category" is a character vector, only show those categories
  # Folder scenarios list/compare files from overall StartFrom (if any) else just files in folder
  #   Use baseModel$inputs(stage=StartFrom)
  #   List each folder scenario, inspect its InputPath and if the file is present there, mark it as "Used"
  # Category scenarios list all files in Category StartFrom and tags them with the Category they
  #   are associated with (from the Categories configuration, only one possible Category per file)
}

ve.scenario.categories <- function(category=NULL,details=FALSE) {
  # TODO: list categories
  # Show category name by default (not informative)
  # If "category" is a character vector, only show those categories
  #   (and set details=TRUE if missing)
  # With details return a data.frame:
  #   TRUE == all details (== c("levels","files"))
  #   "levels" == add one row for each distinct set of level columns (LevelName, Label, Description)
  #   "files" == add one row with File name for each distinct file in the category
  #   if details is a character vector with both "levels" and "files", list files within each
  #     level and in addition to the file name, list out its directory, its size, and its
  #     modification date.
}

ve.scenario.list <- function(scenario=NULL, details=FALSE) {
  # TODO: list scenarios
  # Show scenario names by default (character vector)
  # If "scenario" parameter is a character vector, only show those scenarios
  #   (and set details=TRUE if missing)
  # With details return a data.frame:
  #  Show if it is a "Folder" scenario or a "Category" scenario
  #  Show scenario (stage) RunStatus (check baseModel stages - should (re-)load baseModel)
  # Can subset details by providing a character string instead of a logical
  #  TRUE == all details
  #  "status" == name plus run status
  #  "type" == folder/category
}

ve.scenario.build <- function(startFrom=NULL) {
}

ve.scenario.save <- function(overwrite=TRUE) {
  # write scenarioConfig to a file
  writeSetup(self$scenarioConfig,self$configPath,overwrite=overwrite)
}

#' @export
VEModelScenarios <- R6::R6Class(
  "VEModelScenarios",
  public = list(
    # Data elements
    baseModel = NULL,                   # Model object to which scenarios are attached
    scenarioDir = NULL,                 # Name of the current scenario directory (within baseModel$modelPath)
    configFile = NULL,                  # Name of the scenario configuration file
    scenarioConfig = NULL,              # RunParam_ls with "Scenarios" and "ModelStages" keys
    scenarioPath = NULL,                # Normalized full path to scenaro configuration file
    scenarioStages = list(),            # list of VEModelStage object, built during $load, empty if undefined/invalid
    startFrom = NULL,                   # ModelStage to start from (from config, set during $load)

    # Functions
    initialize=ve.scenario.init,        # Initializes VEModelScenarios object
    load=ve.scenario.load,              # loads ScenarioDir/ScenarioConfig
    stages=ve.scenario.stages,          # Returns list of VEModelStage representing scenarios (will build if needed)
    print=ve.scenario.print,            # Display scenario configuration
    inputs=ve.scenario.inputs,          # Set/View list of inputs by category (or just list of files if no categories)
    categories=ve.scenario.categories,  # Return categories, or replace/update them (optionally save config to .csv files)
    list=ve.scenario.list,              # List out the scenario configuration and stages (details optional)
    save=ve.scenario.save,              # Save the in-memory configuration back out to the config (mostly after build)
    build=ve.scenario.build             # Convert categories/level csv into scenarioConfig + ModelStages (or load manual stages)
  )
)
