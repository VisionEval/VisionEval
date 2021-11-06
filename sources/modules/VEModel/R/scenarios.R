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
  # Must ignore keys that we may read for Scenarios that are distinct from base model
  scenarioParams <- c("ModelStates","Categories","Scenarios")
  baseParam_ls <- self$baseModel$RunParam_ls[ ! names(self$baseModel$RunParam_ls) %in% scenarioParams ]

  # Now add the loaded scenario parameters (scenarioParams, but possibly others)
  modelParam_ls <- visioneval::mergeParameters(self$baseModel$RunParam_ls,self$loadedParam_ls)

  # Load different types of scenarios and build ModelStages for them
  writeLog("Loading model Scenarios",Level="info")
  self$modelStages <- NULL
  # Explicit ModelStages are defined first (available to use as StartFrom for Category stages)
  # These will use the top-level StartFrom for the scenarios
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
    # from). All tahe Category/Scenario stages will StartFrom the CategorySetting/StartFrom
    if ( "StartFrom" %in% names(modelParam_ls$Categories) ) {
      categoryParam_ls <- visioneval::addParameterSource(list(StartFrom=modelParam_ls$Categories$StartFrom),Source="Category Setting")
      modelParam_ls <- visioneval::mergeParameters(modelParam_ls,categoryParam_ls)
      # Set the StartFrom (if any) to use for the individual categories
      # Supports starting from an explicit ModelStage defined in ScenarioDir
    }

    # TODO: Complete and verify code with inst/scenarios/VERSPM-pop

    # TODO: It might be handy to have a ModelStage diagnostic that reports where the ModelStage gets
    # all its input files (get the list of required files for the stage via index, and report each
    # InputDir and the set of files pulled from that location). Make that a function on the
    # ModelStage. Try VEModel$list(inputs=TRUE,stage=Stage$Name,details="INPUTDIR"). Should be able
    # to run that on arbitrary stages and just get their list of inputs. We'll need that eventually
    # to ensure that the InputPath is working right and the scenarios are set up correctly.

    # Once we've loaded a model with such "defective" scenarios, we can retrieve
    # BaseModel$scenarios() and use the "build" and "verify" functions to see if everything is
    # complete and present. "verify" will report inconsistent file space compared to definitions
    # and "built" will create the scenario structure (copying files from the ScenarioDir's StartFrom
    # stage - ignoring the StartFrom for categories)

    # Categories key needs these elements:
    # see inst/scenarios/VERSPM-pop for current data we need to process
    # We can refer to (undefined) Level: 0 when defining a Scenario Level for a Category
    # That specification will simply be dropped since refers to the base case for the scenarios
    # Files. That can help visually validate Categories that access multiple scenarios. so we could
    # see combinations like A:1, B:0 then A:0, B:1 and finally A:1,B:1. While those could be
    # specified as A:1, B:1, A:1 + B:1, it reads better to explicitly state the zero case.

    # TODO: expand.grid is our friend - need to include the zero levels.
    # We can prepend NA to the vector of values and we just ignore those as we compile the list of
    # Scenario properties for building model stages - zero length list (pure base case) means do
    # not create a ModelStage, otherwise for each Category (column) and Level (value of column)
    # that is not NA, build a list of the Scenario-Level item. Ultimately for the Scenario-Level
    # need the Scenario Name (for use by visualizer) and level Name, plus the ScenarioDir/LevelDir
    # (can just compose - don't need to keep ScenarioDir separately). The ModelStage Dir (and Name)
    # will be built by flattening character renditions of Scenario Name and Level Name
    # (SN-SL+...).

    # Look at previous BuildScenarios for ideas - it's too complex and messy, but has the idea of
    # filtering the scenarios by categories - doesn't have the "StartFrom" idea obviously. Does
    # check for presence of required files (which are attached to "scenarios",
    # not "categories") Set that up from the extended example.

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
ve.scenario.stages <- function() {
  if ( length(invalidStages <- self$verify()) ) {
    writeLog("Stage configurtion has errors:",Level="error")
    for ( error in invalidStages ) {
      writeLog(error,Level="error")
    }
  }
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

# TODO: Add a "verify" function (perhaps just a flag on "build" that says to report
# inconsistencies without doing anything to repair them). It makes sure that all listed
# Scenarios and Categories are complete (Scenarios in each Category must be distinct - in the
# same way a File can only appear in one Scenario, a Scenario can appear in only one Category).
# For ModelStage scenarios, make sure that they have a Directory. All the Files in the Scenarios
# must be physically present in the StartFrom for the Scenarios (which might be a local explicit
# ModelStage) - that's not a physical requirement. We'll still look up the InputPath for
# anything missing. However it is a logical requirement for successful scenario management: in
# fact, we could make it more precise and say that the Category StartFrom must have files in its
# local InputPath and they must be all and only the set of Files accumulated from the Scenarios
# - at the least, generate a report on which files are missing from the base, and which files
# are "extra" (though that's less a concern since that StartFrom stage might a have a complete
# set of inputs including files that are not altered in scenarios.

# TODO: if a required Category/Scenario input directory is missing, verify the input
# directories and report scenarios or levels that are not present, or are missing files,
# or that have extra files. We'll still set up whatever model stages we can (and allow them to
# run) but we will note that we tried to define additional scenarios but were missing input
# files (or, for explicit ModelStages, the indicated scenario parameters were not runnable;
# No file checking is done for explicit ModelStages.). That could come up as a warning when we
# do scenarios$stages().
ve.scenario.verify <- function() {
  # Figure out what to verify in scenarios
  # All files in place? Files modified?
  # Complete scenarios?
}

# TODO: Include a "build" function to construct (or verify) the input directories. Copy any
# missing files from the Base Model inputs (use the InputDir reported for each file in the
# BaseModel input path). Report on files present but not listed in the Files key, and delete
# them if they do not differ from the Base Model. Any missing files get copied into place. Also
# generate (perhaps a separate function) a report on which files in the Scenario levels differ
# from the base model (and from other Scenario levels), and perhaps even go deeper looking at
# which columns differ and creating a table with "before-and-after" values for each of the
# fields that are changed.
ve.scenario.build <- function(copyInputs=TRUE) {
  # if not copyInputs, just build the Category/Scenario folders but put nothing
  # in them can re-run with copyInputs==TRUE to populate files later
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
  # print(scenarios$verify()) to get diagnostics on broken stuff
}

# List available inputs for each scenario and (if details) whether it has a local version
# TODO: do we need this? Verify should report deviant cases
ve.scenario.inputs <- function(scenario=NULL,category=NULL,details=TRUE) {
  # if "scenario" is a character vector, only show those folder scenarios
  # if "category" is a character vector, only show scenarios in those categories
  # Folder scenarios list/compare files from overall StartFrom (if any) else just files in folder
  #   Use baseModel$inputs(stage=StartFrom)
  #   List each folder scenario, inspect its InputPath and if the file is present there, mark it as "Used"
  # Category scenarios list all files in Category StartFrom and tags them with the Category they
  #   are associated with (from the Categories configuration, only one possible Category per file)
}

# TODO: use this list to give VEModel a subset of categories to visualize
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

# TODO: do we need this? Lists available scenarios for getting lists of inputs or seeing what's out
# there. should also report their category.
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
  writeSetup(self$loadedParam_ls,self$scenarioPath,overwrite=overwrite)
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
    modelStages = list(),               # list of VEModelStage object, built during $load, empty if undefined/invalid
    startFrom = NULL,                   # ModelStage to start from (from config, set during $load)
    invalidStages = list(),             # List of diagnostics (generated during "load" by calling "verify")

    # Functions
    initialize=ve.scenario.init,        # Initializes VEModelScenarios object
    load=ve.scenario.load,              # loads ScenarioDir/ScenarioConfig
    stages=ve.scenario.stages,          # Returns list of VEModelStage representing scenarios
    verify=ve.scenario.verify,          # Returns scenario diagnostics
    build=ve.scenario.build,            # If Category/Scenario defined, will create any missing directories/files then return verify
    reportable=ve.scenario.reportable,  # Returns TRUE if the supplied stage name is the Scenario StartFrom stage, else FALSE
    print=ve.scenario.print,            # Display scenario configuration
    inputs=ve.scenario.inputs,          # Set/View list of inputs by category (or just list of files if no categories)
    categories=ve.scenario.categories,  # Return categories, or replace/update them (optionally save config to .csv files)
    list=ve.scenario.list,              # List out the scenario configuration and stages (details optional)
    save=ve.scenario.save               # Save the in-memory configuration back out to the config (mostly after build)
  )
)
