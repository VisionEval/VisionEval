# scenarios.R
#' @include environment.R models.R
self=private=NULL

ve.scenario.init <- function( baseModel=NULL ) {
  self$baseModel <- baseModel
  self$load()
}

ve.scenario.load <- function(fromFile=TRUE) {
  # NOTE: Scenarios should fix Years for processing
  if ( fromFile ) {
    # reload if requested
    self$scenarioConfig <- visioneval::loadConfiguration(
      ParamDir=self$setting("ScenarioDir"),
      ParamFile=self$setting("ScenarioConfig"),
      mustWork=FALSE
    )
  }
  # re-process the setup
  if ( ! is.null(self$scenarioConfig) ) {
    # Process stages in scenarios
    scenarioStages <- self$scenarioConfig$Scenarios$ModelStages
    if ( is.list(scenarioStages) ) {
      self$scenarioStages <- lapply(names(scenarioStages), # Use pre-defined structures
        # At a minimum, must provide Dir or Config
        function(stage) {
          obj <- scenarioStages[[stage]] # Get the stageParam_ls structure
          writeLog(paste("Scenario Stage:",stage),Level="info")
          VEModelStage$new(
            Name=stage,
            Model=self,
            stageParam_ls=obj
          )
        }
      )
    }  
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
  # TODO
}

ve.scenario.build <- function() {
  # TODO: construct model stages from ScenarioDir/ScenarioConfig
  # Update config if needed
}

#' @export
VEModelScenarios <- R6::R6Class(
  "VEModelScenarios",
  public = list(
    # Data elements
    baseModel = NULL,                   # Model object to which scenarios are attached
    scenarioConfig = NULL,              # RunParam_ls with "Scenarios" and "ModelStages" keys
    scenarioStages = NULL,              # list of VEModelStage object, built during $load
    startFrom = NULL,                   # ModelStage to start from (from config, set during $load)

    # Functions
    initialize=ve.scenario.init,        # Initializes VEModelScenarios object
    load=ve.scenario.load,              # loads ScenarioDir/ScenarioConfig
    stages=ve.scenario.stages,          # Returns list of VEModelStage representing scenarios (will build if needed)
    print=ve.scenario.print,            # Display scenario configuration
    inputs=ve.scenario.inputs,          # Set/View list of inputs by category (or just list of files if no categories)
    categories=ve.scenario.categories,  # Return categories, or replace/update them (optionally save config to .csv files)
    list=ve.scenario.list,              # List out the scenario configuration and stages (details optional)
    build=ve.scenario.build             # Convert categories/level csv into scenarioConfig + ModelStages (or load manual stages)
  )
)
