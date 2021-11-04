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
ve.scenario.init <- function( baseModel=NULL, fromFile=FALSE ) {
  self$baseModel <- baseModel
  self$scenarioDir <- self$baseModel$setting("ScenarioDir")
  self$scenarioPath <- normalizePath(file.path(self$baseModel$modelPath,self$scenarioDir))
  if ( dir.exists(self$scenarioPath) ) {
    self$load(fromFile=fromFile)
  }
}

# Load scenario's visioneval.cnf (constructing self$RunParam_ls and self$loadParam_ls)
ve.scenario.load <- function(fromFile=FALSE) {
  if ( ! fromFile && ! is.null(self$modelStages) ) return(NULL) # do not reload model stages

  # Reload scenario configuration file and then build the scenario stages
  self$loadedParam_ls <- if ( dir.exists(self$scenarioPath) ) {
    visioneval::loadConfiguration(ParamDir=self$scenarioPath, mustWork=FALSE)
  } else list()

  # Layer in the base model run parameters as basis for scenarios
  modelParam_ls <- visioneval::mergeParameters(self$baseModel$RunParam_ls,self$loadedParam_ls)

  # Load different types of scenarios
  # Build ModelStages for them
  writeLog("Loading model Scenarios",Level="info")
  self$modelStages <- NULL
  # Explicit ModelStages are defined first (available to use as StartFrom for Category stages)
  if ( "ModelStages" %in% names(modelParam_ls) ) {
    writeLog(paste("Parsing explicit Scenarios from",self$scenarioDir),Level="info")
    # TODO: overlay self$RunParam_ls on self$baseModel$RunParam_ls
    modelStages <- lapply(names(modelParam_ls$ModelStages), # Use pre-defined structures
      # At a minimum, must provide Dir or Config
      function(stage) {
        obj <- modelParam_ls$ModelStages[[stage]] # Get the stageParam_ls structure
        writeLog(paste("Model Stage:",stage),Level="info")
        VEModelStage$new(
          Name=stage,
          Model=self$baseModel,
          ScenarioDir=self$scenarioPath,
          modelParam_ls=modelParam_ls,
          StageIsScenario=TRUE,
          stageParam_ls=obj
        )
      }
    )
  } else modelStages <- list()
  self$modelStages <- modelStages

  if ( all("Categories","Scenarios") %in% names(modelParam_ls) ) {
    writeLog(paste("Parsing Category combination Scenarios from",self$ScenarioDir),Level="info")

    # Get CategorySettings if any and overlay on self$RunParam_ls for use in building these stages
    # Only present to support setting a StartFrom from among the explicit ModelStages in the
    # ScenarioDir - that stage will always use a StartFrom from the BaseModel (or have no start
    # from). All the Category/Scenario stages will StartFrom the CategorySetting/StartFrom
    if ( "CategorySettings" %in% names(modelParam_ls) ) {
      categoryParam_ls <- visioneval::addParameterSource(modelParam_ls$CategorySettings,Source="Scenario CategorySettings")
      if ( length(categoryParam_ls)>0 ) {
        modelParam_ls <- visioneval::mergeParameters(modelParam_ls,categoryParam_ls)
      }
    }

    # TODO: Verify Category Files and warn if any duplicates or categories without files

    # TODO: Figure out which Category/Level constitutes each ModelStage. Zero level is injected
    # into the category implicitly: we only permute the non-zero levels, but we do include all the
    # variations of "just this category's levels", add one more category, add another category,
    # etc. The StartFrom stage has no Levels, and if we ask for one we always get zero. That will
    # be true throughout. So the ModelStage lets us add a Level (which includes a Scenario
    # index/name. the Level indictor for that Scenario, and an InputPath entry). Later, we can ask
    # for levels for a list of Scenario index/names and get back the numbers if present, or zero
    # if not, so each Model stage will generate its descriptors based on giving it a list of all
    # scenarios known to the visualizer and we'll have a complete set in VEData.

    # TODO: visit each scenario that is part of the category level included in the stage and add its
    # InputPath and descriptor (Scenario Tag, Level) to the ModelStage Levels list.
    
    scenarioList <- list()
    # Process Categories
    for ( category in modelParam_ls$Categories ) {
      writeLog(paste("Processing category",category$Name),Level="info")

      # Construct levels for this category
      levels <- category$Levels

      catLevel <- lapply(
        levels,
        function(level) {
          catLevelName <- paste0(category,"-",level$Name)
          list(
            Name=catLevelName,
            Description=paste0("(Category: ",category$Label,") ",level$Description),
            InputPath=file.path(self$scenarioPath,catLevelName)
          )
        }
      )
      # Construct scenarios from combinations
      if ( length(scenarioList)==0 ) scenarioList <- catLevel else {
        augmentList <- list()
        for ( nextLevel in catLevel ) {
          augmentList <- c(augmentList,lapply(scenarioList,function(scen) c(scen,list(nextLevel))))
        }
        scenarioList <- augmentList
      }
    }


    # TODO: Keep the Category and Level definitions around for building the visualizer. Don't need
    # the files for the Visualizer. Do need to add ScenarioGroup and ScenarioGroupLevel to the
    # Category definition and create the inside-out structure ScenarioGroup / ScenarioGroupLevel
    # with each ScenarioGroupLevel composed of one or more Categories and Levels. Maybe we attach
    # the ScenarioGroupLevel as an attribute of the Category-Level. That will help avoid duplicates
    # TODO: Verify that all Category-Levels are assiged to ScenarioGroup and ScenarioGroupLevel.

    # Convert Category-Level construction to ModelStage objects
    modelStages <- lapply(
      scenarioList,
      function(scen) {
        Dir = paste(sapply(scen,function(sc) sc$Name),collapse="")
        return(
          c(
            list(
              Name=paste(sapply(scen,function(sc) sc$Name),collapse=""),
              Dir=Dir
            ),
            modelParam_ls[ ! names(modelParam_ls) %in% c("Name","Dir","Description") ]
          )
        )
      }
    )
    self$modelStages <- lapply(scenarioList,
      function(stage) {
        Dir <- paste(sapply(stage,function(sc) sc$Name),collapse="")
        stageParam_ls <- list(
          Dir=Dir,                    # For stage output
          Name=Dir,                   # Root for stage
          InputPath=sapply(stage,function(sc) sc$InputPath), # character vector
          Description=paste(sapply(stage,function(sc) sc$Description),collapse="\n")
        )
        VEModelStage$new(
          Name = Dir,
          Model = self$baseModel,
          ScenarioDir=self$scenarioPath,
          modelParam_ls=modelParam_ls,
          stageParam_ls=stageParam_ls
        )
      }
    )
  } else modelStages <- list()
  self$modelStages <- c(self$modelStages,modelStages)
  
  # If no stage definitions in the configuration, try to do folder-based stages (only)
  if ( length(self$modelStages)==0 && ! any(c("ModelStages","Categories") %in% names(modelParam_ls)) ) {
    # Attempt to make sub-folders of self$scenarioPath into stages
    # In general, to avoid errors with random sub-directories becoming stages
    #  it is best to explicitly set ModelStages in the model's main visioneval.cnf
    writeLog("Parsing implicit Scenarios from directories",Level="info")
    stages <- list.dirs(self$scenarioPath,full.names=FALSE,recursive=FALSE)
    structuralDirs <- c(
      self$baseModel$setting("DatastoreName"),
      self$baseModel$setting("QueryDir"),
      self$baseModel$setting("ScriptsDir"),
      self$baseModel$setting("InputDir"),
      self$baseModel$setting("ParamDir"),
      self$baseModel$setting("ScenarioDir"),
      self$baseModel$setting("ResultsDir")
    )
    stages <- stages[ ! stages %in% structuralDirs ]
    writeLog(paste0("Scenario Stage directories:\n",paste(stages,collapse=",")),Level="info")
    self$modelStages <- lapply(stages,
      function(stage) {
        stageParam_ls <- list(
          Dir=stage,                              # Relative to modelPath
          Name=stage,                             # Will only change root directory
          Path=file.path(self$scenarioPath,stage) # Root for stage inputs
        )
        VEModelStage$new(
          Name = stageParam_ls$Name,
          Model = self$baseModel,
          ScenarioDir=self$scenarioPath,
          modelParam_ls=modelParam_ls,
          stageParam_ls=stageParam_ls
        )
      }
    )
  }
  self$RunParam_ls <- modelParam_ls # save scenarios RunParam_ls
  # Get here with self$modelStages containg a list of VEModelStage objects
}

# Return the scenario ModelStages
# Note that they must already have been built and loaded
ve.scenario.stages <- function() {
  return( self$modelStages )
}

# Return TRUE or FALSE depending on whether the indicated stage name is the Scenario StartFrom
# In a scenario-bearing model, only the startFrom stage will be Reportable from the Base Model
ve.scenario.reportable <- function(stageName) {
  startFrom <- self$RunParam_ls$StartFrom
  return(
    is.character(startFrom) &&
    is.character(stageName) &&
    stageName==startFrom
  )
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
    scenarioPath = NULL,                # Normalized full path to scenaro directory
    loadedParam_ls = NULL,              # Scenario parameters as loaded from configFile (or to be rewritten)
    RunParam_ls = NULL,                 # RunParam_ls for Scenarios (runtime)
    modelStages = list(),            # list of VEModelStage object, built during $load, empty if undefined/invalid
    startFrom = NULL,                   # ModelStage to start from (from config, set during $load)

    # Functions
    initialize=ve.scenario.init,        # Initializes VEModelScenarios object
    load=ve.scenario.load,              # loads ScenarioDir/ScenarioConfig
    stages=ve.scenario.stages,          # Returns list of VEModelStage representing scenarios (will build if needed)
    reportable=ve.scenario.reportable,  # Returns TRUE if the supplied stage name is the Scenario StartFrom stage, else FALSE
    print=ve.scenario.print,            # Display scenario configuration
    inputs=ve.scenario.inputs,          # Set/View list of inputs by category (or just list of files if no categories)
    categories=ve.scenario.categories,  # Return categories, or replace/update them (optionally save config to .csv files)
    list=ve.scenario.list,              # List out the scenario configuration and stages (details optional)
    save=ve.scenario.save               # Save the in-memory configuration back out to the config (mostly after build)
  )
)
