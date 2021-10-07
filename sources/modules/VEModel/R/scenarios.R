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

ve.scenario.init <- function( baseModel=NULL, create=FALSE, startFrom=NULL ) {
  self$baseModel <- baseModel
  self$scenarioDir <- self$baseModel$setting("ScenarioDir"),
  self$configFile <- self$baseModel$setting("ScenarioConfig"),
  if ( create && ! dir.exists(scenarioDir) ) {
    self$scenarioPath <- normalizePath(baseModel$modelPath,self$scenarioDir)
    dir.create(scenarioDir)
  }
  if ( dir.exists(scenarioDir) ) {
    scenarioConfigPath <- file.path(scenarioPath,self$configFile)
    self$load(startFrom=startFrom,scenarioPath=scenarioConfigPath)
  }
}

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
        # At a minimum, must provide Dir or Config
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

ve.scenario.stages <- function() {
  return( self$scenarioStages )
}

ve.scenario.print <- function() {
  # TODO
}

ve.scenario.inputs <- function() {
  # TODO: extract input list from baseModel/startFrom
  # Want actual inputs from modules in startFrom script
}

ve.scenario.categories <- function() {
  # TODO
}

ve.scenario.list <- function() {
  # TODO: list scenarios
}

ve.scenario.build <- function(startFrom=NULL) {
  # TODO: construct model stages from ScenarioDir/ScenarioConfig
  # Update config if needed
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
