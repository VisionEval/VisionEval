# Author: Jeremy Raw

# TODO: get the dir() and clear() functions working on multi-stage models
# Should take a stage parameter on any of those. Shows all the "Results"
# subdirectories for the stages, and all relevant files inside those
# (Datastore, model state).  Also show the outputs folder (if it exists)
# and allow clearing that too.

# VEModel Package Code

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

self=private=NULL # To avoid R6 class "undefined global" errors

## Helper
confirmDialog <- function(msg) {
  conf <- utils::askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE # Cancel is the same as No
  return(conf)
}

## Helper
#  return TRUE if modelPath looks like an absolute file path
isAbsolutePath <- function(modelPath) {
  # TODO: may need a more robust regular expression
  any(grepl("^([[:alpha:]]:|/)",modelPath))
}

## Helper
#  Model roots: ve.runtime/models, getwd()/models, ve.runtime, getwd()
getModelRoots <- function(Param_ls,get.root=0) {
  roots <- c( getwd() )
  if ( exists("ve.runtime") ) {
    ve.runtime <- get("ve.runtime")
    if ( ve.runtime != getwd() ) roots <- c( ve.runtime, roots )
  }
  # Hierarchy of roots:
  #    ve.runtime/ModelRoot (if exists)
  #    getwd()/ModelRoot (if exists)
  #    ve.runtime
  #    getwd()
  modelRoot <- file.path(roots,visioneval::getRunParameter("ModelRoot","models",Param_ls))
  if ( length(modelRoot)>0 ) {
    if ( isAbsolutePath(modelRoot[1]) ) {
      modelRoot <- modelRoot[1]
    } else {
      test.paths <- normalizePath(file.path(roots,modelRoot))
      modelRoot <- test.paths[dir.exists(test.paths)]
      if ( length(modelRoot)==0 || ! nzchar(modelRoot[1]) ) {
        modelRoot <- NULL
      } else {
        modelRoot <- test.paths
      }
    }
  }
  roots <- c( modelRoot, roots)
  if ( get.root > length(roots) ) get.root <- 1
  if ( get.root>0 ) return(roots[get.root]) else return(roots)
}

## Helper
#  Get unique file name based on newName in folder newPath
#  NewPath is the directory it should go in, newName is the name to disambiguate
getUniqueName <- function(newPath,newName) {
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  return (newModelPath)
}

## Helper function
#  Look for run_model.R (any case) in root or subdirectories and return full paths
modelInRoot <- function(modelPath,searchScript,searchExact) {
  # Search initially just in modelPath, then in first-level subdirectories
  # Expect modelPath to be normalized
  grepArgs <- list(searchScript,fixed=searchExact,value=TRUE)
  if ( ! searchExact ) grepArgs <- c(grepArgs,list(ignore.case=TRUE))
  dirs <- list(dir(modelPath,full.names=TRUE))
  paths <- do.call("grep",c(grepArgs,dirs))
  if ( length(paths) == 0 ) {
    dirs <- list(dir(list.dirs(modelPath,recursive=FALSE),full.names=TRUE))
    paths <- do.call("grep",c(grepArgs,dirs))
    visioneval::writeLog(dirs,Level="debug")
  }
  if ( length(paths) > 0 ) {
    # All matching searchScript files with path relative to modelPath
    paths <- sub("^\\./","",sub( modelPath,".",paths )) # still includes the file name
    # Must have no more than one run_model.R per subdirectory
    if ( ( any(dupes <- duplicated(dirname(paths))) ) ) {
      Msg <- paste(
        "Model script ",
        if ( searchExact ) "exact search" else "regex search",
        " yielded more than one script file per subdirectory:\n\n",
        paste(paths[ dirname(paths) %in% dirname(paths)[dupes] ],"\n",sep="",collapse="\n"),
        "\nAdjust ModelScript or ModelScriptFile pattern in configuration\n",
        "or remove/rename one of the excess scripts."
      )
      stop(Msg)
    }
  }
  return(paths)
}

## Helper function

## Helper function
# Examine modelPath and return (sub-)directories containing run_model.R
# Param_ls provides a list of configuration values 
# Returns a list with the following elements:
#   isValid : a boolean, TRUE if we found model(s), FALSE if we did not
#   ModelScript : a vector (length of stagePaths) with the actual files found matching
#      the ModelScript RunParameter.
#   modelPath : character vector (length 1); the normalized version of parameter
#   stagePaths : character vector of absolute paths below modelPath that were found
#      to contain files named <runModelName> (per configuration). First part of
#      each path will exactly match modelPath.
findModel <- function( modelPath, Param_ls ) {

  find_ls <- list()
  find_ls$isValid <- FALSE # we'll change our mind later...

  if ( missing(modelPath) || ! is.character(modelPath) ) {
    message("Must provide modelPath locator.")
    return( find_ls )
  }

  # Establish the local name for run_model.R
  # VisionEval.R startup will set ModelScript from site configuration file
  #   located in runtime root (first of .visioneval, VisionEval.ini, VisionEval.cnf)
  #   with same default as here ("run_model.R")
  searchScript <- visioneval::getRunParameter("ModelScript",Default=NA,Param_ls=Param_ls)
  searchExact <- is.na(searchScript)
  if ( searchExact ) {
    searchScript <- visioneval::getRunParameter("ModelScriptFile",Default="run_model.R",Param_ls=Param_ls)
  }

  # check if modelPath contains a runModelName
  findPath <- grep(modelPath,searchScript,fixed=searchExact,ignore.case=TRUE,value=TRUE)
  if ( length(findPath>0) ) {
    modelPath <- findPath[1]
    message("Using explicit script matching ",searchScript,":\n")
    find_ls$modelPath <- normalizePath(dirname(modelPath),winslash="/")
    find_ls$stagePaths <- "." # modelPath root is the only stagePath
    find_ls$stageScripts <- basename(modelPath)
    find_ls$isValid <- TRUE
    message( modelPath )
    return(find_ls)
  }

  # if modelPath is not an absolute path, search for it amongst the "roots"
  if ( ! isAbsolutePath(modelPath) ) {
    roots<-getModelRoots(Param_ls)
    possiblePaths <- file.path(roots,modelPath)
    existing <- dir.exists(possiblePaths)
    if ( ! any(existing) ) {
      message("Failed find to model ",modelPath," in these locations:\n",paste(roots,collapse="\n"))
      find_ls$modelPath <- modelPath
      return(find_ls)
    } else {
      # Use first of the existing paths
      find_ls$modelPath <- normalizePath(possiblePaths[existing][1],winslash="/")
    }
  } else if ( ! dir.exists(modelPath) ) {
    message("Model directory ",modelPath," does not exist")
    return(find_ls)
  } else {
    find_ls$modelPath <- modelPath
  }

  # Attempt to locate runModelName in directory and first-level sub-directories
  # The resulting vector are the stage paths (relative to find_ls$modelPath)
  find_ls$stagePaths <- modelInRoot(find_ls$modelPath,searchScript,searchExact)
  if ( length(find_ls$stagePaths)<=0 ) {
    message("No matching pattern for",searchScript," found in ",find_ls$modelPath)
  } else {
    find_ls$stageScripts <- basename(find_ls$stagePaths)
    find_ls$stagePaths <- dirname(find_ls$stagePaths)
    find_ls$isValid <- TRUE
  }
  
  return( find_ls )
}

## Dir
#  Look up a standard model
#  model is bare name of standard model
findStandardModel <- function( model ) {
  standardModels <- system.file("models",package="VEModel")
  if ( is.null(model) || ! nzchar(model) ) return( dir(standardModels) )

  model <- model[1]
  if ( ! nzchar(standardModels) || ! model %in% dir(standardModels) ) {
    standardModels <- getOption("VEStandardModels",default=normalizePath("inst/models"))
  }
  model <- file.path(standardModels,model)
  if ( ! dir.exists(model) ) {
    stop(
      "No standard model called ",model,"\n",
      "installModel() to list available models"
      )
  }
  return(model)
}

## install a standard model either as a template (skeleton==TRUE) or
## a sample (skeleton==FALSE)
#  We're still expecting to distribute with standard models pre-installed
#  Called automatically from findModel, where modelPath must be a bare model name
#  Can install from other locations by calling this function with a more elaborate modelPath

SampleModelDataFormat <- c( templ="Template",samp="Sample" )

installStandardModel <- function( modelName, modelPath, confirm, skeleton, Param_ls=NULL ) {
  # Locate and install standard modelName into modelPath
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first available standard root
  #   If modelPath is an existing directory, put modelName into it (conflict-resolved name)
  #   If modelPath does not exist, but dirname(modelPath) exists, create new directory and put the model there
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and they have to try again
  model <- findStandardModel( modelName )
  if ( is.null(modelName) || ! nzchar(modelName) ) return(model) # Vector of available standard model names

  # Set up destination modelPath
  if ( ! is.list(Param_ls) ) Param_ls <- list()
  root <- getModelRoots(Param_ls,1)
  if ( missing(modelPath) || is.null(modelPath) ) modelPath <- modelName
  if ( ! isAbsolutePath(modelPath) ) {
    installPath <- normalizePath(file.path(root,modelPath),winslash="/",mustWork=FALSE)
  }
  if ( dir.exists(installPath) ) {
    installPath <- getUniqueName( dirname(installPath), basename(modelPath) )
  }

  # Confirm installation if requested
  install <- TRUE
  skeleton <- if ( skeleton ) "templ" else "samp"
  if ( confirm && interactive() ) {
    msg <- paste0("Install standard model '",modelName,"' (",SampleModelDataFormat[skeleton],") in ",installPath,"?\n")
    install <- confirmDialog(msg)
  }

  if ( ! install ) stop("Model ",modelName," not installed.",call.=FALSE)

  # Now do the installation
  message("Installing ",modelName," from ",model," as ",SampleModelDataFormat[skeleton])
  dir.create(installPath)

  # Locate the model and data source files
  model.path <- file.path(model,"model")
  model.files <- dir(model.path,full.names=TRUE)

  data.path <- file.path(model,skeleton)
  if ( ! dir.exists(data.path) ) stop("No ",SampleModelDataFormat[skeleton]," available for ",modelName)
  data.files <- dir(data.path,full.names=TRUE)

  file.copy(model.files,installPath,recursive=TRUE) # Copy standard model into modelPath
  file.copy(data.files,installPath,recursive=TRUE) # Copy skeleton data into modelPath
  message("Installed ",modelName," in ",installPath)

  return( list(modelName=modelName,modelPath=installPath) )
}

ve.model.copy <- function(newName=NULL,newPath=NULL) {
  if ( is.null(newPath) ) {
    if ( self$stageCount>1 ) {
      newPath <- dirname(self$stagePaths)
      newPath <- stripPathPrefix(newPath)
      if ( ! dir.exists(newPath) ) newPath <- dirname(newPath) # match might extend into basename
      if ( ! nzchar(newPath) ) {
        newPath <- dirname(self$stagePaths[1])
      } else {   # assume there's an embracing directory
        newPath <- dirname(newPath)
      }
    } else {
      newPath <- dirname(self$modelPath[1])
    }
  } else {
    if ( ! dir.exists(newPath) ) newPath <- dirname(newPath)
  }

  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- getUniqueName(newPath,newName)

  get.destination <- 
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath,showWarnings=FALSE)
  for ( s in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(state=s),private$artifacts(state=s))
    copy.to <- file.path(newModelPath,self$stagePaths[s])
    dir.create(copy.to,showWarnings=FALSE)
    file.copy(copy.from,copy.to,recursive=TRUE)
  }

  return( openModel(newModelPath) )
}

ve.model.loadModelState <- function(log="error") {
  # Load all the ModelStates for the model stages, using initializeModel with RunModel=FALSE
  ResultsDir <- visioneval::getRunParameter("ResultsDir",Default=".",Param_ls=self$RunParam_ls)
  ModelStateFileName <- visioneval::getRunParameter("ModelStateFileName",Default="ModelState.Rda",Param_ls=self$RunParam_ls)
  BaseInputPath <- visioneval::getRunParameter("InputPath",Default=self$modelPath,Param_ls=self$RunParam_ls)
  private$ModelState <- list()
  owd <- setwd(file.path(self$modelPath,ResultsDir))
  on.exit(setwd(owd))
  for ( stage in seq_along(self$stagePaths) ) {
    Param_ls <- self$RunParam_ls
    stagePath <- self$stagePaths[stage]
    stageInput <- file.path(self$modelPath,stagePath)
    Param_ls$ResultsDir <- file.path(ResultsDir,stagePath)
    setwd(file.path(self$modelPath,Param_ls$ResultsDir))
    modelState <- normalizePath(ModelStateFileName,winslash="/",mustWork=FALSE)
    visioneval::initLog(Save=FALSE,Threshold=log)
    initMsg <- paste("Loading Model",self$modelName)
    if ( length(self$stagePaths)>1 ) {
      visioneval::writeLog(paste(initMsg,"Stage",stage,": ",stagePath),Level="info")
    } else {
      visioneval::writeLog(initMsg,Level="info")
    }
    if ( visioneval::loadModelState(modelState, ( ms.env <- new.env() )) ) {
      private$ModelState[[ basename(stagePath) ]] <- ms.env$ModelState_ls
    } else {
      scriptFile <- self$stageScripts[stage]
      modelScriptFile <- normalizePath(file.path(stageInput,scriptFile),winslash="/")
      ve.model <- visioneval::modelEnvironment()
      ve.model$RunModel <- FALSE
      ve.model$RunStatus <- "Not Run"
      ve.model$ModelStateStages <- private$ModelState; # previously loaded model states
      Param_ls$InputPath <- c(stageInput,BaseInputPath)
      parsedScript <- visioneval::parseModelScript(modelScriptFile)

      # Execute the initializeModel function from the model script
      initArgs                   <- parsedScript$InitParams_ls; # includes LoadDatastore etc.
      initArgs$Param_ls          <- Param_ls;  # Naming explicit arguments makes them higher priority than Param_ls
      initArgs$ModelScriptFile   <- modelScriptFile
      initArgs$ParsedModelScript <- parsedScript
      initArgs$LogLevel          <- log
      do.call(visioneval::initializeModel,args=initArgs)

      # Keep the model state so later stages can refer to it
      private$ModelState[[ toupper(basename(stagePath)) ]] <- ve.model$ModelState_ls
    }
  }
  if ( length(private$ModelState)!=length(self$stagePaths) ) {
    stop(
      "ModelState list not the same length as stagePaths list: ",
      length(private$ModelState)," ",length(self$stagePaths)
    )
  }
  invisible(private$ModelState)
}

getRuntimeConfig <- function(ParamDir=NULL) {
  # Function loads configuration from ParamDir/VisionEval.cnf
  # ParamDir defaults to ve.runtime if defined, else getwd()
  # override is a list whose elements may be replaced by this configuration
  # keep is a list whose elements will take precedence over this configuration
  if ( ! is.character(ParamDir) ) {
    ParamDir <- get0("ve.runtime",ifnotfound=getwd())
  }
  return(
    visioneval::loadConfiguration(ParamDir=ParamDir)
  )
}

# Initialize a VEModel from modelPath
ve.model.init <- function(modelPath=NULL,verbose=TRUE,log="error") {
  # Load system and user model configuration
  # Opportunity to override names of ModelState, run_model.R, Datastore, etc.
  # Also to ejstablish standard model directory structure (inputs, results)
  self$RunParam_ls <- getRuntimeConfig()

  # Identify the run_model.R root location(s)
  modelPaths <- findModel(modelPath,self$RunParam_ls)
  if ( ! modelPaths$isValid ) {
    stop("No VisionEval model (",modelPaths$runModelName,") found at ",modelPath)
  }

  # Set up the model name, folder tree and script files
  self$modelPath <- modelPaths$modelPath
  self$modelName <- basename(self$modelPath)
  if ( verbose ) print(self$modelPath)
  self$stagePaths <- modelPaths$stagePaths; # named stage names (folders with run_model.R, relative to ModelPath)
  self$stageScripts <- modelPaths$stageScripts; # actual names of ModelScriptFile for each stage
  self$stageCount <- length(self$stagePaths)

  # Load the Model State
  private$loadModelState(log=log)

  # TODO: Make this work with nested (interior) stages
  self$runStatus <- sapply(
    simplify=TRUE,
    private$ModelState,
    function(ms) {
      if ( length(ms) > 0 ) {
        # TODO: ms might be a list of interior stages
        ifelse(
          "runStatus" %in% names(ms),
          ms$runStatus,
          "Prior Run"
        )
      } else {
        "Not Run"
      }
    }
  )
  self$status <- self$runStatus[length(self$runStatus)]
}

# Run the modelPath (through its stages)
# Find the runModel script
# Work in the Results directory - need to relay locations from here to initializeModel
# Need to set Inputs directory
ve.model.run <- function(stage=NULL,lastStage=NULL,stageScript=NULL,lastScript=NULL,log="ERROR",verbose=TRUE) {
  # stage and lastStage will run a sequence of *interior* model stages
  #   (defined by runStage directives in the run_model.R script)
  # stageScript and lastScript will run a sequence of *exterior* model stages
  #   (separate run_model.R scripts)
  # If self$stagePaths has length > 1, stage refers to that *exterior* stage unless stageScript is
  #   also provided (in which case stageScript is the exterior stage, and stage is the interior)
  # TODO: implement for interior scripts

  resultsDir <- file.path(self$modelPath,visioneval::getRunParameter("ResultsDir",Default=".",Param_ls=self$RunParam_ls))
  modelStateName <- visioneval::getRunParameter("modelStateFileName",Default="ModelState.Rda",Param_ls=self$RunParam_ls)

  # TODO: look up stages by name/identifier
  # Name will match the "stagePath" basename if we have "exterior" stages
  #   (explicit directories with their own run_model.R script)
  # If interior stages, we can match those too
  # Combining exterior and interior stages will yield result directories like
  #   exterior-stage-1/interior-stage-1
  # Where the exterior stage is the location of run_model.R (if subdirectory of
  #   modelPath) and the interior stage is from the "name" parameter of a runStage
  #   directive (just defaulting to Stage-N for the Nth runStage directive).

  pathLength <- length(self$stagePaths)

  if ( is.null(stage) ) {
    stageStart <- 1
    if ( is.null(lastStage) ) {
      lastStage <- self$stageCount
    } else if ( lastStage < stageStart ) {
      lastStage <- stageStart
    }
  } else {
    stageStart <- stage
    if ( is.null(lastStage) ) {
      lastStage <- stageStart
    } else if ( lastStage < stageStart ) {
      lastStage <- stageStart
    }
  }
    
  if ( is.null(lastStage) || lastStage < stageStart ) {
    lastStage <- stageStart
  } else if ( lastStage==0 ) {
    lastStage <- self$stageCount
  } # else it is what it is (hopefully a number > stageStart)

  if ( is.null(lastStage) ) lastStage <- self$stageCount;

  # Set up the model runtime environment
  ve.model <- visioneval::modelEnvironment()

  for ( ms in stageStart:lastStage ) {
    stage <- self$stagePaths[ms]
    runModel <- self$stageScripts[ms]

    if ( verbose ) {
      if ( stage != "." ) {
        message("Running model stage:")
        message(stage)
      } else {
        message("Running model ",self$modelName)
      }
    }
    owd <- setwd(file.path(self$modelPath,stage))
    self$status <- ""

    rm(list=ls(ve.model),envir=ve.model) # ve.model is always fresh for each model stage
    ve.model$RunParam_ls <- self$RunParam_ls; # put it where initializeModel can see it
    ve.model$RunParam_ls$LogLevel <- log
    tryCatchLog::tryCatchLog(
      {
        self$status <- "Running"
        ve.model$RunModel <- TRUE

        # Find and run the model script for the stage
        # Formally set run parameters for post mortem debugging
        ModelDir <- normalizePath(file.path(self$modelPath,stage),winslash="/",mustWork=TRUE)
        modelRunParam_ls <- list(
          ModelDir=ModelDir,
          ModelScriptFile=file.path(ModelDir,runModel)
        )
        ve.model$RunParam_ls <- visioneval::mergeParameters(
          ve.model$RunParam_ls,
          visioneval::addParameterSource(modelRunParam_ls,"VEModel$run constructed")
        )
        ModelDir <- visioneval::getRunParameter("ModelDir",Default=ModelDir,Param_ls=ve.model$RunParam_ls)
        ModelScriptFile <- visioneval::getRunParameter("ModelScriptFile",Default=runModel,Param_ls=ve.model$RunParam_ls)

        sys.source(ModelScriptFile,envir=ve.model)

        if (verbose) visioneval::writeLog(paste0("Model stage ",stage," complete"),Level="info")
        self$status <- self$runStatus[ms] <- "Complete"
      },
      error = function(e) {
        remark <- paste0("Model stage ",stage," failed")
        msg <- as.character(e)
        if ( ! nzchar(msg) ) msg <- "Stopped."
        self$status <- "Error"
        visioneval::writeLog(c(remark,msg),Level="error")
        self$runStatus[ms] <- "Failed"
      },
      finally =
      {
        ve.model$RunModel <- FALSE
        if ( self$status == "Running" ) {
          self$status <- "Failed"
        }
        if ( self$status == "" ) {
          self$status <- "Stopped"
        }
        if (verbose) {
          visioneval::writeLog(
            c(
              paste("Model Stage:",stage),
              self$status
            )
          )
        }
        modelStatePath <- file.path(resultsDir,self$stagePaths[ms],modelStateName)
        visioneval::setModelState(
          list(
            runStatus=self$runStatus[ms]
          ),
          FileName=modelStatePath,
          Save=file.exists(modelStatePath)
        )
        setwd(owd)
      }
      # NOTE: ve.model is left with all its elements in place for
      # post mortem analysis. It will get cleared when the final
      # stage
    )
    if (verbose) {
      cat("Status:",self$status,"\n")
    }
    if ( self$status != "Complete" ) {
      break;
    }
  }
  # Reload the ModelState files for each stage
  private$loadModelState()

  return(invisible(self$status))
}

ve.model.dir <- function(pattern=NULL,recursive=FALSE,shorten="",stage=NULL,stageScript=NULL) {
  # path/stage can be a vector of discrete stages (e.g. c(1,3); only
  # those will be inspected.
  # TODO: separate script specification from stageScript specification
  if ( is.null(stage) ) stage<-c(1:self$stageCount)
  files <- dir(self$modelPath[stage],pattern=pattern,recursive=recursive,full.names=TRUE)
  if ( nzchar(shorten) ) files <- gsub(shorten,"",files)
  return(files)
}

# outputOnly will just report the extraction results
# (not the model run results)
ve.artifacts <- function(stage=NULL,outputOnly=FALSE) {
  if ( ! outputOnly ) {
    mstates <- self$dir(pattern=".*(Previous)*ModelState\\.Rda",stage=stage)
    mstates <- mstates[!dir.exists(mstates)]
    dstores <- self$dir(pattern="Datastore_*",stage=stage)
    dstores <- dstores[dir.exists(dstores)]
    logs    <- self$dir(pattern="Log_*.*\\.txt",stage=stage)
    logs    <- logs[!dir.exists(logs)]
    artifacts <- c(mstates,dstores,logs)
  } else artifacts <- character(0)
  outputs <- self$dir(pattern="output",stage=stage)
  return(c(artifacts,outputs))
}

ve.model.clear <- function(force=FALSE,outputOnly=NULL,stage=NULL) {
  if ( missing( outputOnly ) ) {
    # If "output" exists in any stage, only offer to clear outputs
    # unless the user manually overrides. Makes it harder to
    # accidentally delete a model run.
    outputOnly = any( dir.exists( file.path(self$modelPath,"output") ) )
  }
  to.delete <- private$artifacts(stage=stage,outputOnly=outputOnly)
  if ( length(to.delete)>0 ) {
    to.delete <- gsub(paste0("^",getwd(),"/"),"",to.delete)
    print(to.delete)
    if ( interactive() ) {
      choices <- to.delete
      preselect <- if (force || outputOnly ) to.delete else character(0)
      to.delete <- utils::select.list(choices=choices,preselect=preselect,multiple=TRUE,title="Delete Selected Outputs")
      force <- length(to.delete)>0
    } else {
      force <- ( force || length(to.delete)>0 )
    }
    if ( force) {
      print(dir(to.delete),recursive=TRUE)
      unlink(to.delete,recursive=TRUE)
      private$ModelState <- lapply(
        self$modelPath,
        function(x) list()
      )
      cat("Model",(if(outputOnly) "outputs" else "results"),"cleared.\n")
    } else {
      cat("Model results NOT cleared.\n")
    }
  } else {
    cat("No prior results to clear.\n")
    force = FALSE
  }
  return(invisible(force))
}

stripPathPrefix <- function(x) {
    x   <-sort(x)                          # sort the vector
    d_x <-strsplit(x[c(1,length(x))],"")   # split the first and last element by character (list of two vectors of single characters)
    pfx <-match(FALSE,do.call("==",d_x))-1 # match the first not common element and back up to last matching one
    if(is.na(pfx)) {                       # all vector elements are the same
      return(x[1])
    } else if (pfx==0) {                   # if there is no matching element, return an empty vector, else return the common part
      return(character(0))
    } else {
      return(substr(x[1],1,pfx))
    }
}

ve.model.print <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$RunParam_ls$DatastoreType,"\n")
  cat("Status:", self$status,"\n")
  self$status
}

selectName <- c(fields="Name",tables="Table",groups="Group")
selectOrder <- c("Name","Table","Group","Stage")
prepSelect <- function(self,what,details=FALSE) {
  # self is a VEModel object
  # what is the character name of the thing we're selecting (groups,tables,fields)
  # details if TRUE pastes all the fields, otherwise
  df <- self[[what]]
  name <- selectName[what]
  if ( details ) {
    show <- selectOrder[ selectOrder %in% names(df) ]
    detail.fields <- show[-grep(name,show)]
    if ( what=="fields" ) {
      list.fields <- self[["list"]](details=TRUE,selected=FALSE)
      df$Description <- substr(list.fields$Description,1,40)
      detail.fields <- c(detail.fields,"Description")
      show <- c(show,"Description")
      }
    detail.function <- function(x) {
      paste(x[name],paste(paste(detail.fields,x[detail.fields],sep=": "),collapse=", "),sep=" | ")
    }
  } else {
    show <- name
    detail.function <- function(x) x
  }
  choices <- apply(df[,show,drop=FALSE], 1, detail.function)
  selected <- choices[which(df$Selected=="Yes")]
  names <- df[,name,drop=TRUE]
  return( list(choices=choices,selected=selected,names=names) )
}

ve.model.output <- function(stage) {
  # Create an output object wrapping the directory that contains the model
  # results for the given stage (or last stage if not given)
  if ( missing(stage) || !is.numeric(stage) ) {
    stage <- length(self$stagePaths)
  }
  output <- VEOutput$new(private$ModelState,self,stage) # parameters TBD
  if ( output$valid() ) {
    return(output)
  } else {
    if (stage!=length(self$stagePaths)) {
      warning("There is no output for stage ",stage," of this model yet.")
    } else {
      warning("There is no output for this model yet.")
    }
  }
  return(output)
}

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,
    modelPath=NULL,
    stagePaths=NULL,
    stageScripts=NULL,
    stageCount=NULL,
    RunParam_ls=NULL,
    runStatus=NULL,
    status="Uninitialized",

    # Methods
    initialize=ve.model.init,               # initialize a VEModel obje ct
    run=ve.model.run,                       # run a model (or just a subset of stages)
    print=ve.model.print,                   # provides generic print functionality
    dir=ve.model.dir,                       # list model elements (output, scripts, etc.)
    clear=ve.model.clear,                   # delete results or outputs (current or past)
    copy=ve.model.copy,                     # copy a self$modelPath to another path (ignore results/outputs)
    output=ve.model.output                  # Create a VEOutput object (if model is run); option to open a past result
  ),
  private = list(
    # Private Members
    runError=NULL,
    artifacts = ve.artifacts,               # Function may interrogate an existing Output
    ModelState=NULL,                        # ModelState placeholder
    # Private Methods
    loadModelState=ve.model.loadModelState  # Function to load a model state file
  )
)

#' Open a VisionEval Model
#'
#' @description
#' \code{openModel} opens a VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' @section Model Path and Name:
#'
#' The `modelPath` parameter locates a model object. When a model is opened, a
#'   a relative modelPath will be sought in the user's runtime `models` directory.
#'   An absolute path will be sought only in the user's file system.
#'
#' You can set an alternate location for the "models" subdirectory by providing an
#'   a path using, for example, `options(VEModelPath='mymodels')`. Relative paths
#'   will be sought below the VisionEval runtime directory. Absolute paths will
#'   be sought in the user's file system.
#'
#' An error will be raised if a model cannot be found with the indicated
#'   modelPath and modelName.
#'
#' @section Available Models:
#'
#' You can see the available models by providing an empty string for the `modelPath`.
#'   A VEModelList (q.v.) object will be printed as a side-effect, and also returned invisibly.
#'   You can open an installed model from the VEModelList using
#'   square brackets to index the list by position (`VEModelList[1]`) or by name
#'   (`VEModelList['VERSPM']`).
#'
#' @param modelPath Directory containing a VisionEval model; if an empty character string is
#'     provided, prints a list of available models (see details)
#' @param log a character string identifying the log level to be displayed
#' @return A VEModel object or a VEModelList of available models if no modelPath or modelName is
#'   provided; see details and `vignette("VEModel")`
#' @export
openModel <- function(modelPath="",log="error") {
  if ( missing(modelPath) || !nzchar(modelPath) ) {
    Param_ls <- getRuntimeConfig()
    if ( exists("ve.runtime") ) {
      ve.runtime <- get("ve.runtime")
    } else {
      ve.runtime <- getwd()
    }
    return(dir(file.path(ve.runtime,visioneval::getRunParameter("ModelDir",Default="models",Param_ls=Param_ls))))
  } else {
    return( VEModel$new(modelPath = modelPath,log=log) )
  }
}

#' Install a Standard VisionEval Model
#'
#' @description
#' `installModel` installs a local copy of a standard VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' An error will be raised if a model cannot be found or created with the indicated
#'   modelPath and modelName.
#'
#' You can see the available built-in (standard) models by providing an empty string for the `modelName`.
#'
#' @param modelName Name of a standard model to install; if empty or NULL (default), list
#'   available standard models.
#' @param modelPath Location to place the copy of modelName standard model. Created relative to
#'   ve.runtime/models. If directory does not exist, create it and copy the modelName into it.
#'   If directory does exist, create a unique variant of modelName adjacent to it. If it is NULL
#'   create a unique variant of modelName in ve.runtime/models.
#' @param skeleton if TRUE (default), install just skeleton files for the model; otherwise
#'   install the sample model.
#' @param confirm if TRUE (default) and running interactively, prompt user to confirm, otherwise
#'   just do it.
#' @param log a string describing the minimum level to display
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, skeleton=FALSE, confirm=!skeleton, log="error") {
  model <- installStandardModel(modelName, modelPath, confirm, skeleton)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath, log=log ) )
  } else {
    return( model ) # should be a character vector of available standard models
  }
}
