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
  } # else there are no scenarios apart from baseModel stages
}

# Load scenario's visioneval.cnf (constructing self$RunParam_ls and self$loadParam_ls)
# Then build whatever model stages are defined there
ve.scenario.load <- function(fromFile=FALSE) {

  if ( ! fromFile && ! is.null(self$modelStages) ) return(NULL) # do not reload model stages

  # Reload scenario configuration file and then build the scenario stages
  self$loadedParam_ls <- if ( dir.exists(self$scenarioPath) ) {
    visioneval::loadConfiguration(ParamDir=self$scenarioPath, mustWork=FALSE)
  } else list()

  # Layer in the base run parameters as basis for scenarios

  # If StartFrom is defined in the scenario RunParam_ls, use the parameters from that stage as the
  # basis for the current scenarios. Otherwise, just load the parameters from the base model,
  # expecting to fill in the required missing ones here.
  if ( "StartFrom" %in% names(self$loadedParam_ls) ) {
    startFrom <- self$baseModel$modelStages[[ self$loadedParam_ls$StartFrom ]]
    baseParam_ls <- startFrom$RunParam_ls
    # Drop keys that we will force stages here to define
    baseParam_ls <- baseParam_ls[ - which( names(baseParam_ls) %in% c("Scenario","Description","ModelStages") ) ]
  } else {
    baseParam_ls <- self$baseModel$RunParam_ls
  }

  # Now add the loaded scenario parameters (scenarioParams, but possibly others)
  modelParam_ls <- visioneval::mergeParameters(baseParam_ls,self$loadedParam_ls)

  # Load different types of scenarios and build ModelStages for them
  writeLog("Loading model Scenarios",Level="info")

  # Explicit ModelStages are defined first (available to use as StartFrom for Category stages)
  # These will use the top-level StartFrom for the scenarios
  if ( "ModelStages" %in% names(self$loadedParam_ls) ) {
    writeLog(paste("Parsing explicit Scenarios from",self$scenarioDir),Level="info")
    modelStages <- lapply(names(self$loadedParam_ls$ModelStages), # Use pre-defined structures
      # At a minimum, must provide Dir or Config
      function(stage) {
        obj <- self$loadedParam_ls$ModelStages[[stage]] # Get the stageParam_ls structure
        obj$IsScenario = TRUE # Force scenario marker into stageParam_ls (stage configuration structure)
        writeLog(paste("Model Stage:",stage),Level="info")
        VEModelStage$new(
          Name=stage,
          Model=self$baseModel,
          ScenarioDir=self$scenarioPath,
          modelParam_ls=modelParam_ls,
          stageParam_ls=obj
        )
      }
    )
    names(modelStages) <- names(self$loadedParam_ls$ModelStages) # so we can look up the category StartFrom
  } else modelStages <- list()
  self$modelStages <- modelStages

  # Trigger for combination scenarios is presence of ScenarioElements
  # Must be in the scenario's visioneval.cnf (not inherited)
  if ( "ScenarioElements" %in% names(self$loadedParam_ls) ) {
    writeLog(paste("Parsing Category combination Scenarios from",self$scenarioDir),Level="info")

    self$Elements <- self$loadedParam_ls$ScenarioElements

    # Add the fully built input path to each Level in the Element.
    # Those will be composed via the Category Levels into the InputPath vector for finding
    #   the input files that make up the scenario category level.
    self$Elements <- lapply( self$Elements,
      function(element) {
        scenarioRoot <- if ( "ScenarioRoot" %in% names(element) ) { element$ScenarioRoot } else { element$Name }
        element$Levels <- lapply(element$Levels,
          function(level) {
            level$Path <- file.path(
              self$scenarioPath,scenarioRoot,level$Name # level$Name is subdirectory inside element$ScenarioRoot
            )
            return(level)
          }
        )
        names(element$Levels) <- sapply(element$Levels,function(lvl) lvl$Name)
        element
      }
    )
    names(self$Elements) <- sapply(self$Elements,function(e) e$Name)

    # TODO: for diagnostic purposes, list out any Element Levels that do not have at least one
    # unique file. That should probably just be a separate VEModelScenarios function. Here, we're
    # just building the structures and throwing errors if we can't find what we need.

    # Compose the ScenarioCategories
    if ( "ScenarioCategories" %in% names(self$loadedParam_ls) ) {
      # All the Category/Scenario stages will StartFrom the CategorySetting/StartFrom, which is
      # expected to be an explicit ModelStage in ScenarioDir. That explicit stage will use the
      # overall StartFrom, if any.
      if ( ! "Category" %in% names(self$loadedParam_ls$ScenarioCategories) ) {
        stop(
          writeLog("No ScenarioCategories present in model scenario configuration",Level="error")
        )
      }
      
      self$Categories <- modelParam_ls$ScenarioCategories$Category
      if ( "StartFrom" %in% names(modelParam_ls$ScenarioCategories) ) {
        # Handle the case where StartFrom is a manual scenario defined here
        # rather than one inherited from the base model.
        categoryStartFrom <- visioneval::addParameterSource(
          list(StartFrom=modelParam_ls$ScenarioCategories["StartFrom"]),
          Source=attr(modelParam_ls$ScenarioCategories,"source")
        )
        if ( length(self$modelStages) > 1 ) {
          stop(
            writeLog("Only one explicit model stage allowed if using ScenarioCategories",Level="error")
          )
        }
        modelParam_ls <- visioneval::mergeParameters(modelParam_ls,categoryStartFrom)
        if ( "StartFrom" %in% names(modelParam_ls) ) { # should be there, but just checking
          if ( modelParam_ls$StartFrom %in% names(self$modelStages) ) { # Manually defined StartFrom within ScenarioDir
            startFrom <- self$modelStages[[modelParam_ls$StartFrom]]
            if ( ! is.null(startFrom) ) {
              startFrom$elements(update="VEModel scenario init") # Put default ScenarioElement levels on StartFrom stage
            }
          }
        }
      } else {
        if ( length(self$modelStages) > 0 ) {
          stop(
            writeLog("Only one explicit model stage allowed with ScenarioCategories, which must be the ScenarioCategories StartFrom",Level="error")
          )
        }
      }
    } else {
      # Construct default ScenarioCategories from Scenario Elements
      self$Categories <- lapply(
        self$Elements,
        function(element) {
          list(
            Name = element$Label, # Category has Name, Description, Levels
            Description = element$Description,
            Levels = lapply(element$Levels,
              function(level) {
                list(
                  Name=level$Name, # Levelhas Name, Inputs
                  Inputs=lapply(level,
                    function(input) {
                      list(
                        Name=element$Name, # Input keys to Scenario Element by Name
                        Level=level$Name   # Level says which Scenario Element Level to include
                      )
                    }
                  )
                )
              }
            )
          )
        }
      )
    }
    # Make Categories into a named list (for ease of access later when building ModelStages)
    names(self$Categories) <- sapply(self$Categories,function(c) c$Name)

    # Now that we have the Categories, add the full InputPath to each Level
    for ( category in names(self$Categories) ) {
      self$Categories[[category]]$Levels <- lapply(self$Categories[[category]]$Levels,
        function(level) {
          level$InputPath <- sapply(level$Inputs,
            function(input) {
              if ( as.numeric(input$Level)==0 ) return("")
              e <- self$Elements[[input$Name]]
              e$Levels[[input$Level]]$Path
            }
          )
          level$InputPath <- level$InputPath[ nzchar(level$InputPath) ]
          if ( ! is.character(level$InputPath) ) {
            stop(writeLog(paste("Input path for category",level$Name,"is undefined"),Level="error"))
          }
          return(level)
        }
      )
    }
    # TODO: It might be handy to have a ModelStage diagnostic that reports where the finished
    # ModelStages get their inputs (listing just the files that are different in each stage from
    # the BaseStage. Perhaps use VEModel$list(inputs=TRUE,stage=Stage$Name,details="INPUTDIR"). 

    # Expand Grid takes a named list where the names are all the Category Names and the Values
    #   are the list of possible levels for that Category (treated as indexes into the Category's
    #   Levels list - 0 drops out and is then ignored.
    expandCategories <- lapply(self$Categories, function(c) 0:length(c$Levels))
    names(expandCategories) <- sapply(self$Categories, function(c) c$Name )

    # each row of stagesToBuild will become a model stage
    stagesToBuild <- expand.grid(expandCategories,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)

    # Make a ModelStage from each resulting combination of Category/Level. Additional information in
    # each ModelStage:
    #   1. Combined InputPath (just concatenate the previously built level InputPath from each
    #      included Category Level).
    #   2. named list of Element Name : Element Level Name to identify the ModelStage to the
    #      Visualizer
    # Need to construct Scenario Name and Description in order to have a Runnable model stage
    # Also need to create the Stage Directory (conventional name)

    scenarioList <- list()
    usedElements <- character(0) # Accumulate ScenarioElements that are in use
    for ( stage in 1:nrow(stagesToBuild ) ) {
      # Pull out each non-zero level category
      catLevels <- stagesToBuild[stage,]
      catLevels <- unlist(catLevels[ , catLevels != 0, drop=FALSE ])
      if ( length(catLevels)==0 ) next # StartFrom stage covers these...

      # Concatenate the category description
      catNames <- names(catLevels)
      scenarioName <- paste( collapse="+", paste( gsub("[^[:alnum:]]","_",catNames), catLevels, sep="=" ) )

      # Now get the Level information
      inputPath <- character(0)
      description <- character(0)
      elements <- character(0)
      for ( levelIndex in seq_along(catLevels) ) {
        catValues <- self$Categories[[catNames[levelIndex]]]
        levelValues <- catValues$Levels[[catLevels[levelIndex]]]
        inputPath <- c(inputPath,levelValues$InputPath)
        description <- if(length(description)>0) paste(description,catValues$Description,sep=":") else catValues$Description
        elementLevels <- sapply(levelValues$Inputs,function(v) v$Level)
        names(elementLevels) <- sapply(levelValues$Inputs,function(v) v$Name)
        elements <- c( elements,elementLevels )
      }
      description <- paste(description,collapse="//")
      scenarioList[[length(scenarioList)+1]] <- list(
        Scenario=scenarioName,    # Also becomes ScenarioDir for results output
        Description=description,
        InputPath=inputPath,
        ScenarioElements=elements,
        IsScenario=TRUE
      )
      usedElements <- unique(c( usedElements, names(elements) ))
    }
    # Remove ScenarioElements that are not in use
    elementNames <- as.character(sapply(self$Elements,function(e) e$Name))
    if ( any( undefined <- ( ! usedElements %in% elementNames ) ) ) {
      stop(
        writeLog(paste("ScenarioElements used but not defined:",paste(usedElements[undefined],collapse=",")),Level="error")
      )
    }
    usedElements <- sapply(self$Elements,function(e) e$Name %in% usedElements  )
    self$Elements <- self$Elements[usedElements]
    elementNames <- elementNames[usedElements]

    # Update scenarioList elements so all ScenarioElements are present
    scenarioList <- lapply(scenarioList,
      function(scn) {
        allElements <- scn$ScenarioElements[ elementNames ]
        allElements[ is.na(allElements) ] <- "0" # Zero level for any scenario elements that are missing
        names(allElements) <- elementNames
        scn$ScenarioElements <- allElements
        return(scn)
      }
    )

    # Convert Category-Level construction to ModelStage objects
    # Remove Scenario/Description inherited from "StartFrom" model run parameters
    modelParam_ls <- modelParam_ls[ ! names(modelParam_ls) %in% c("Scenario","Description","ScenarioElements") ]
    # Construct model stages
    categoryStages <- lapply(scenarioList,
      function(stage) {
        Dir <- stage$Scenario
        stage$Dir <- Dir
        stage$Name <- Dir
        VEModelStage$new(
          Name = Dir,
          Model = self$baseModel,
          ScenarioDir=self$scenarioPath,
          modelParam_ls=modelParam_ls,
          stageParam_ls=stage
        )
      }
    )
    self$modelStages <- c( self$modelStages, categoryStages ) # preserve local StartFrom, if any
  } else {
    # If still no stage definitions in the configuration, try to do folder-based stages (only)
    if ( length(self$modelStages)==0 && ! any(c("ModelStages","ScenarioCategories") %in% names(modelParam_ls)) ) {
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
            Path=file.path(self$scenarioPath,stage),# Root for stage inputs
            IsScenario=TRUE                         # Mark it as a scenario
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
  }

  # If self$Elements is undefined, scenarios are just the individual modelStages
  # In that case, make each scenario as its own scenario element, make the list of Elements be the
  # scenario model stages, and make the levels of the sole Category be each Scenario Element (model
  # stage_)
  if ( is.null(self$Elements) ) {
    self$Elements <- lapply(
      self$modelStages,
      function(stage) {
        description <- stage$RunParam_ls$Description
        element <- list(
          Name=stage$Name,
          Label=stage$Name,
          Description=description,
          Instructions=description,
          Levels=list(
            list(
              Name="1",
              Label=stage$Name,
              Description=description
            )
          )
        )
        return(element)
      }
    )
    # Now construct the levels for the single Category called "Scenarios"
    elementNames <- names(self$Elements) <- unlist(sapply(self$Elements,function(e) e$Name))
    levels <- lapply(self$modelStages,
      function(stage) {
        list(
          Name=stage$Name,
          Inputs=list(
            list(
              Name=stage$Name,
              Level="1"
            )
          )
        )
      }
    )
    names(levels) <- NULL # don't confuse the visualizer
    self$Categories <- list( # List of one category
      list(
        Name="Scenarios",
        Label="Scenarios",
        Description="Scenarios",
        Levels=levels
      )
    )
    # Finally, fill in the list of elements represented in each stage
    # The stage itself will be given Level 1; other stages get level 0
    for (stage in self$modelStages ) {
      stage$ScenarioElements <- "1"
      names(stage$ScenarioElements) <- stage$Name
      allElements <- stage$ScenarioElements[ elementNames ]
      allElements[ is.na(allElements) ] <- "0" # Zero level for any scenario elements that are missing
      names(allElements) <- elementNames
      stage$ScenarioElements <- allElements
    }
  }

  self$RunParam_ls <- modelParam_ls # save scenarios RunParam_ls
  # Get here with self$modelStages containing a list of VEModelStage objects
  # Plus a table we can use to extract the Category/Level status of each model stage,
  # including the StartFrom stage.
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

# TODO: use this list to give VEQuery a subset of categories to visualize
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

ve.scenario.catconfig <- function() {
  # iterate over self$Categories
  catconfig <- structure( names=NULL,
    lapply( self$Categories,
      function(cat) {
        Label <- if ( "Label" %in% names(cat) ) cat$Label else cat$Name
        Description <- if ( "Description" %in% names(cat) ) cat$Description else NULL
        baseCategoryLevel <- cat$Levels[[1]]
        baseCategoryLevel$Name <- "0"
        baseCategoryLevel$Inputs <- lapply( baseCategoryLevel$Inputs,
          function(inp) {
            return( ( list( Name=inp$Name, Level="0" ) ) )
          }
        )
        # Build baseCategoryLevel as list of of NAME/LEVEL pairs
        # where NAME is the NAME from each cat$Levels, and LEVEL is "0"
        Levels <- list()
        Levels[[1]] <- baseCategoryLevel
        for ( level in cat$Levels ) Levels[[length(Levels)+1]] <- level
        return( list(
          NAME=cat$Name,
          LABEL=Label,
          DESCRIPTION=Description,
          LEVELS=lapply( Levels,
            function(level) {
              return( list(
                NAME=level$Name,
                INPUTS=lapply(level$Inputs,
                  function(input) {
                    return( list(
                      NAME=input$Name,
                      LEVEL=input$Level
                    ) )
                  }
                )
              ) )
            }
          )
        ) )
      }
    )
  )
  return(catconfig)
}

baseScenarioLevel <- list(
  Name="0",
  Label="Base Case",
  Description="Unaltered from base scenario"
)

ve.scenario.scenconfig <- function() {
  scenconfig <- lapply( self$Elements,
    function(scen) {
      Instructions <- if( "Instructions" %in% names(scen) )
      { scen$Instructions } else { scen$Description }
      Levels <- list()
      Levels[[1]] <- baseScenarioLevel
      for ( level in scen$Levels ) Levels[[length(Levels)+1]] <- level
      return( list(
        NAME=scen$Name,
        LABEL=scen$Label,
        DESCRIPTION=scen$Description,
        INSTRUCTIONS=Instructions,
        LEVELS=structure(
          names=NULL,
          lapply( Levels,
            function(level) {
              return( list(
                NAME=level$Name,
                LABEL=level$Label,
                DESCRIPTION=level$Description
              ) )
            }
          )
        )
      ) )
    }
  )
  names(scenconfig) <- NULL # Don't have named objects (visualizer does not like that!)
  return( scenconfig )
}

#' @export
VEModelScenarios <- R6::R6Class(
  "VEModelScenarios",
  public = list(
    # Data elements
    baseModel = NULL,                     # Model object to which scenarios are attached
    scenarioDir = NULL,                   # Name of the current scenario directory (within baseModel$modelPath)
    scenarioPath = NULL,                  # Normalized full path to scenaro directory
    loadedParam_ls = NULL,                # Scenario parameters as loaded from configFile (or to be rewritten)
    RunParam_ls = NULL,                   # RunParam_ls for Scenarios (runtime)
    modelStages = list(),                 # list of VEModelStage object, built during $load, empty if undefined/invalid
    startFrom = NULL,                     # ModelStage to start from (from config, set during $load)
    invalidStages = list(),               # List of diagnostics (generated during "load" by calling "verify")
    Elements = NULL,                      # list of ScenarioELements for this scenario set
    Categories = NULL,                    # list of ScenarioCategories for this scenario set

    # Functions
    initialize=ve.scenario.init,          # Initializes VEModelScenarios object
    load=ve.scenario.load,                # loads ScenarioDir/ScenarioConfig
    stages=ve.scenario.stages,            # Returns list of VEModelStage representing scenarios
    verify=ve.scenario.verify,            # Returns scenario diagnostics
    build=ve.scenario.build,              # If Category/Scenario defined, will create any missing directories/files then return verify
    reportable=ve.scenario.reportable,    # Returns TRUE if the supplied stage name is the Scenario StartFrom stage, else FALSE
    print=ve.scenario.print,              # Display scenario configuration
    inputs=ve.scenario.inputs,            # Set/View list of inputs by category (or just list of files if no categories)
    categories=ve.scenario.categories,    # Return categories, or replace/update them (optionally save config to .csv files)
    list=ve.scenario.list,                # List out the scenario configuration and stages (details optional)
    save=ve.scenario.save,                # Save the in-memory configuration back out to the config (mostly after build)
    categoryConfig=ve.scenario.catconfig, # Generate list with Category elements required by the visualizer
    scenarioConfig=ve.scenario.scenconfig # Generate list with Category elements required by the visualizer
  )
)
