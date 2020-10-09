# Author: Jeremy Raw

# VisionEval helper tools
# NOTE: these all depend on having set up the VisionEval environment
# See VisionEval.R in the installation runtime (VE-Installer/boilerplate)

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

tool.contents <- c("openModel","verpat","verspm","vestate")

requireNamespace("jsonlite")
requireNamespace("R6")
requireNamespace("visioneval")
requireNamespace("futile.logger")
requireNamespace("tryCatchLog")

# Function: ve.model.path
# Determine if parameter is a list of locations of run_model.R riles
# First parameter is character vector of directories that may contain
# run_model.R
# Check first whether modelPath contains directories or full paths
# to run_model.R (if the latter, replace with dirnames)
# If modelPath contains directories, each one must have a run_model.R
# If only one directory, and it does not contain run_model.R, look
# for a staged model scenario - so find all the subdirectories and
# verify that each has a run_model.R.

ve.model.path <- function(modelPath=NULL) {
  # Check how modelPath specifies a run_model.R file
  # Does modelPath make sense (absolute or relative to getwd())?
  if ( is.null(modelPath) ) stop("Must provide model path locator.\n")
  if ( ! all( file.exists(modelPath) ) ) {
    modelPath = file.path(ve.runtime,"models",modelPath)
    if ( ! all( dir.exists(modelPath) ) ) {
      stop("Could not locate model directory for [",paste(modelPath,dir.exists(modelPath),sep=":",collapse=","),"]")
    }
  }
  # Figure out if we can use modelPath to find "run_model.R"
  # script(s)
  if ( all ( file.exists(modelPath) & toupper(basename(modelPath))=="RUN_MODEL.R" ) ) {
    # Provided full path to run_model.R (possibly more than one)
    modelPath <- dirname(modelPath)
  } else if ( ! all( file.exists( file.path(modelPath,"run_model.R") ) ) ) {
    if ( length(modelPath)==1 ) { # no run_model.R in modelPath
      # Check for staged model (must be single root directory)
      subs <- dir(modelPath,full.names=TRUE)
      modelPath <- subs[dir.exists(subs) & file.exists(file.path(subs,"run_model.R"))]
      if ( length(modelPath)==0 ) {
        stop("No run_model.R in [",paste(modelPath,collapse=","),"]")
      }
    } else {
      stop("Could not locate run_model.R for [",paste(modelPath,collapse=","),"]")
    }
  }
  return(normalizePath(modelPath,winslash="/",mustWork=TRUE))
}

load.model.state <- function(path) {
  ms.env <- new.env()
  if ( ! grepl("ModelState\\.Rda$",path) ) path <- file.path(path,"ModelState.Rda")
  if ( file.exists(path) ) {
    model.state <- visioneval::readModelState(FileName=path, envir=ms.env)
  } else {
    model.state <- list()
  }
  rm(ms.env)
  return(model.state)
}

ve.init.model <- function(modelPath=NULL,modelName=NULL   ,...) {
  self$modelPath <- ve.model.path(modelPath)
  names(self$modelPath) <- basename(self$modelPath)
  if ( is.null(modelName) ) {
    self$modelName <- if ( length(self$modelPath)>1 ) {
       # default modelName for multi-stage model is basename of
       # the directory containing the first stage subdirectory
      basename( dirname(self$modelPath[1]) )
    } else { # For a one-stage model it is the basename of the first path itself
      names(self$modelPath)[1]
    }
  } else {
    self$modelName <- modelName
  }
  # Gather defs/run_parameters.json
  if ( file.exists(rpfile <- file.path(self$modelPath[1],"defs","run_parameters.json")) ) {
    self$runParams <- jsonlite::fromJSON(rpfile)
  } else {
    stop("Cannot construct model; missing: ",rpfile)
  }
  self$stageCount <- length(self$modelPath)
  self$modelState <- lapply(
    self$modelPath,
    load.model.state
  )
  if ( length(self$modelState)>0 && any(unlist(lapply(self$modelState,length))>0) ) {
    private$index()
  }

  self$runStatus <- sapply(
    simplify=TRUE,
    self$modelState,
    function(ms) {
      if ( length(ms) > 0 ) {
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

log.level <- function(level) {
  legal.levels <- list(
    "DEBUG"=futile.logger::DEBUG,
    "ERROR"=futile.logger::ERROR,
    "FATAL"=futile.logger::FATAL,
    "INFO"=futile.logger::INFO,
    "TRACE"=futile.logger::TRACE,
    "WARN"=futile.logger::WARN
  )
  if ( level %in% names(legal.levels) ) {
    return(legal.levels[level])
  } else {
    return(legal.levels["ERROR"])
  }
}

ve.run.model <- function(verbose=TRUE,path=NULL,stage=NULL,log="ERROR") {
  # Unlike .dir the path/stage says where to start - the run will
  # then continue by running that stage then each following stage
  if ( missing(path) ) path <- stage    # Still might be NULL; allow alias
  pathLength <- length(self$modelPath)
  stageStart <- if ( ! is.null(path) ) path else 1
  for ( ms in stageStart:self$stageCount ) {
    stage <- self$modelPath[ms]
    if ( verbose ) {
      message("Running model stage:")
      message(stage)
    }
    owd <- setwd(stage)
    if ( ! "ve.model" %in% search() ) {
      envir <- attach(NULL,name="ve.model")
    } else {
      envir <- as.environment("ve.model")
    }
    self$status <- ""
    futile.logger::flog.threshold(log.level(log))
    tryCatchLog::tryCatchLog(
      {
        self$status <- "Running"
        sys.source("run_model.R",envir=envir)
        if (verbose) message("Model stage ",stage," complete")
        self$status <- self$runStatus[ms] <- "Complete"
      },
      error = function(e) {
        message("Model stage ",stage," failed")
        msg <- as.character(e)
        if ( ! nzchar(msg) ) msg <- "Stopped."
        self$status <- msg
        self$runStatus[ms] <- "Failed"
      },
      finally =
      {
        if ( self$status == "Running" ) {
          self$status <- "Failed"
        }
        if ( self$status == "" ) {
          self$status <- "Stopped"
        }
        if (verbose) {
          cat("Model Stage:",gsub(ve.runtime,"",stage),"\n")
          if ( self$status != "Complete" ) cat("Error:",self$status,"\n")
        }
        model.state.path <- file.path(self$modelPath[ms],"ModelState.Rda")
        if ( file.exists(model.state.path) ) {
          visioneval::setModelState(
            list(runStatus=self$runStatus[ms]),
            FileName=model.state.path
          )
        }
        setwd(owd)
      }
    )
    if (verbose) {
      cat("Status:",self$status,"\n")
    }
    if ( self$status != "Complete" ) break;
  }
  self$modelState <- lapply(
    self$modelPath,
    load.model.state
  )
  if ( length(self$modelState)>0 && all(unlist(lapply(self$modelState,length))>0) ) {
    private$index()
  }

  return(invisible(self$status))
}

ve.model.dir <- function(pattern=NULL,recursive=FALSE,shorten="",path=NULL,stage=NULL) {
  # path/stage can be a vector of discrete stages (e.g. c(1,3); only
  # those will be inspected.
  if ( missing(path) ) path <- stage
  if ( is.null(path) ) path<-c(1:self$stageCount)
  files <- dir(self$modelPath[path],pattern=pattern,recursive=recursive,full.names=TRUE)
  if ( nzchar(shorten) ) files <- gsub(shorten,"",files)
  return(files)
}

confirm <- function(msg) {
  conf <- askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE
  return(conf)
}

# outputOnly will just report the extraction results
# (not the model run)
ve.artifacts <- function(path=NULL,stage=NULL,outputOnly=FALSE) {
  if ( missing(path) ) path <- stage
  if ( ! outputOnly ) {
    mstates <- self$dir(pattern=".*(Previous)*ModelState\\.Rda",path=path)
    mstates <- mstates[!dir.exists(mstates)]
    dstores <- self$dir(pattern="Datastore_*",path=path)
    dstores <- dstores[dir.exists(dstores)]
    logs    <- self$dir(pattern="Log_*.*\\.txt",path=path)
    logs    <- logs[!dir.exists(logs)]
    artifacts <- c(mstates,dstores,logs)
  } else artifacts <- character(0)
  outputs <- self$dir(pattern="output",path=path)
  return(c(artifacts,outputs))
}

ve.model.clear <- function(force=FALSE,outputOnly=NULL,path=NULL,stage=NULL) {
  if ( missing(path) ) path <- stage
  if ( missing( outputOnly ) ) {
    # If "output" exists in any stage, only offer to clear outputs
    # unless the user manually overrides. Makes it harder to
    # accidentally delete a model run.
    outputOnly = any( dir.exists( file.path(self$modelPath,"output") ) )
  }
  to.delete <- private$artifacts(path=path,outputOnly=outputOnly)
  if ( length(to.delete)>0 ) {
    to.delete <- gsub(paste0("^",getwd(),"/"),"",to.delete)
    print(to.delete)
    if ( interactive() ) {
      choices <- to.delete
      preselect <- if (force || outputOnly ) to.delete else character(0)
      to.delete <- select.list(choices=choices,preselect=preselect,multiple=TRUE,title="Delete Select Outputs")
      force <- length(to.delete)>0
    } else {
      force <- ( force || length(to.delete)>0 )
    }
    if ( force) {
      print(dir(to.delete),recursive=TRUE)
      unlink(to.delete,recursive=TRUE)
      self$modelState <- lapply(
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

ve.path.prefix <- function(x) {
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

ve.model.copy <- function(newName=NULL,newPath=NULL) {
  if ( is.null(newPath) ) {
    if ( self$stageCount>1 ) {
      newPath <- dirname(self$modelPath)
      newPath <- ve.path.prefix(newPath)
      if ( ! dir.exists(newPath) ) newPath <- dirname(newPath) # match might extend into basename
      if ( ! nzchar(newPath) ) {
        newPath <- dirname(self$modelPath[1])
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
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  get.destination <- if ( self$stageCount == 1 ) {
    function(modelPath,...) modelPath
  } else {
    function(modelPath,basenameStage) file.path(modelPath,basenameStage)
  }
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath,showWarnings=FALSE)
  for ( p in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(path=p),private$artifacts(path=p))
    copy.to <- get.destination(newModelPath,basename(self$modelPath[p]))
    print(copy.to)
    dir.create(copy.to,showWarnings=FALSE)
    file.copy(copy.from,copy.to,recursive=TRUE)
  }
    
  return( openModel(newModelPath,newName) )
}

# Check if a specified attribute belongs to the Datastore row
attributeExist <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(TRUE)
    }
  }
  return(FALSE)
}

# Get a specified attribute for a Datastore row
attributeGet <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(attr_value)
    }
  }
  return(NA)
}

ve.model.index <- function() {
  # Check that there is a model state
  # message("indexing model stages...")
  if ( length(self$modelState)==0 || ! any(unlist(lapply(self$modelState,length))>0) ) {
    stop("Model does not appear to have been run yet.")
  }

  # message("Processing model stages...")
  Index <- data.frame()
  Inputs <- data.frame()
  for ( stage in self$stageCount ) {
    ms <- self$modelState[[stage]]
    if ( length(ms)==0 ) next
    ds <- (ms$Datastore)
    model.path <- file.path(basename(dirname(self$modelPath[stage])),basename(self$modelPath[stage]))

    # TODO: change this to parse the model from run_model.R if modelstate does not exist
    # Use

    # message("Processing ",basename(self$modelPath[stage]))
    # NOTE: Datastore element ds of ModelState is a data.frame.
    #       The attributes column contains a list for each row
    # Datastore elements with a "FILE" attribute are inputs; we want the outputs
    # the non-FILE elements are creations living in the Datastore (i.e. not inputs => outputs)
    InputIndex <- sapply(ds$attributes, attributeExist, "FILE")
    Description <- sapply(ds$attributes, attributeGet, "DESCRIPTION",simplify=TRUE) # should yield a character vector
    Module <- sapply(ds$attributes, attributeGet, "MODULE",simplify=TRUE) # should yield a character vector
    Units <- sapply(ds$attributes, attributeGet, "UNITS",simplify=TRUE) # should yield a character vector

    # Build parallel data.frame for Inputs
    # message("Input data frame...")
    File <- sapply(ds$attributes, attributeGet, "FILE",simplify=TRUE) # should yield a character vector
    inputs <- data.frame(
      Module = Module[InputIndex],
      Name = ds$name[InputIndex],
      File = File[InputIndex],
      Description = Description[InputIndex],
      Units = Units[InputIndex],
      Stage = rep(as.character(stage),length(which(InputIndex))),
      Path = model.path
    )
    Inputs <- rbind(Inputs,inputs)
    # message("Length of inputs:",nrow(inputs))

    # message("Output data frame...")
    Description <- Description[!InputIndex]
    Module <- Module[!InputIndex]
    Units <- Units[!InputIndex]
    splitGroupTableName <- strsplit(ds[!InputIndex, "groupname"], "/")
    if ( length(Description) != length(splitGroupTableName) ) stop("Inconsistent table<->description correspondence")
    # message("Length of outputs:",length(splitGroupTableName))

    maxLength <- max(unlist(lapply(splitGroupTableName, length)))
    if ( maxLength != 3 ) {
      warning("Model state ",self$modelPath[stage],"is incomplete (",maxLength,")")
      next
    }
    splitGroupTableName <- lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x))))

    # Add modelPath and Description to Index row
    # message("Adding Description and modelPath")
    PathGroupTableName <- list()
    for ( i in 1:length(splitGroupTableName) ) {
      PathGroupTableName[[i]] <- c(
        splitGroupTableName[[i]],
        Description[i],
        Units[i],
        Module[i],
        as.character(stage),
        model.path
      )
    }
    if ( any((cls<-lapply(PathGroupTableName,class))!="character") ) {
      bad.class <- which(cls!="character")
      print( PathGroupTableName[[bad.class[1]]] )
      print( length(bad.class) )
      stop("Non-character vector in Datastore index row")
    }

    # Using 'do.call' turns each element of the splitGroupTableName list into one argument for rbind.data.frame
    # By contrast, calling rbind.data.frame(splitGroupTableName) simply converts the list (a single argument) into a
    # data.frame (so each element becomes one column) Explanation:
    # https://www.stat.berkeley.edu/~s133/Docall.html
    # message("Adding to output data.frame")
    GroupTableName <- data.frame()
    GroupTableName <- do.call(rbind.data.frame, PathGroupTableName)
    colnames(GroupTableName) <- c("Group", "Table", "Name","Description", "Units","Module","Stage","Path")
    # message("length of output data:",nrow(GroupTableName))

    # GroupTableName is now a data.frame with five columns
    # complete.cases blows away the rows that have any NA values
    # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each
    # column)
    # message("Adding inputs to Inputs data.frame")
    ccases <- complete.cases(GroupTableName[,c("Group","Table","Name")])
    GroupTableName <- GroupTableName[ccases,]
    # message("Length of complete.cases:",nrow(GroupTableName))
    Index <- rbind(Index,GroupTableName)
    # message("length of Index:",nrow(Index))
  }
  # message("Attaching ve.inputs attribute to Index")
  self$modelIndex <- Index
  self$modelInputs <- Inputs
  invisible(list(Index=self$modelIndex,Inputs=self$modelInputs))
}

ve.model.status <- function(status) {
  if ( missing(status) ) return(private$runError)
  private$runError <- status
}

ve.print.model <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
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

ve.model.select <- function( what, details=FALSE ) {
  # interactive utility to select groups, tables or fields
  # 'what' can be "groups","tables" or "fields" (either as strings or names without quotes)
  # 'details' = FALSE (default) will present just the item name
  # 'details' = TRUE will present all items details
  # Interactive dialog will pre-select whatever is already selected (everything if
  #   nothing has been selected yet (either by assignment or earlier invocation of ve.model.select)
  sub.what <- substitute(what)
  if ( class(sub.what) == "name" ) {
    what <- deparse(sub.what)
  }
  if ( class(what) != "character" ) {
    message("What to select must be 'groups','tables' or 'names'")
    invisible(character(0))
  }
  if ( ! interactive() ) {
    message("VEModel$select(",paste(what,collapse=","),") called from non-interactive session.")
    message("In a script, just assign desired selection to VEModel$groups (or tables or fields)")
    invisible(character(0))
  }
  what <- what[1] # if there's a vector, use the first element
  select.from <- which(c("groups","tables","fields") %in% what)
  select.from <- prepSelect(self,what,details)
  # select.from is a list with two elements:
  #  "names" which is a character vector of names corresponding to "choices" (just the name)
  #  "choices" which are the text lines that appear in the display
  #            (pasted text with name, details)
  #  "selected" which are the subset of the strings in "choices" that are already selected
  if ( is.null(select.from) ) {
    message("Unknown entity to select from:",paste(what,collapse=","))
    invisible(character(0))
  }
  selected <- select.list(choices=select.from$choices,preselect=select.from$selected,multiple=TRUE,
    title=paste("Select",paste(toupper(substring(what,1,1)),substring(what,2),sep=""),sep=" "))
  self[[what]] <- select.from$names[ select.from$choices %in% selected ] # character(0) if none selected => selects all
  invisible(self[[what]]) # print result to see what actually got selected.
}

ve.model.groups <- function(groups) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxGroups <- unique(self$modelIndex[,c("Group","Stage")])
  row.names(idxGroups) <- NULL
  if ( ! missing(groups) ) {
    years <- ( tolower(groups) %in% c("years","year") ) # magic shortcut
    if ( any(years) ) {
      # Expand literal "Years" into all the year-like groups (name is exactly 4 digits)
      groups <- c( groups[!years], grep("^[[:digit:]]{4}$",idxGroups$Group,value=TRUE) )
    }
    if ( is.character(groups) && length(groups)>0 ) {
      self$groupsSelected <- groups[ groups %in% idxGroups$Group ]
    } else {
      self$groupsSelected <- character(0)
    }
  }
  if ( length(self$groupsSelected)==0 ) {
    idxGroups$Selected <- "Yes"
  } else {
    idxGroups$Selected <- ifelse(idxGroups$Group %in% self$groupsSelected,"Yes","No")
  }
  return(idxGroups)
}

ve.group.selected <- function(test.group,groups) {
  return( test.group %in% groups$Group[groups$Selected=="Yes"] )
}

ve.model.tables <- function(tables) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxTables <- unique(self$modelIndex[,c("Group","Table","Stage")])
  row.names(idxTables) <- NULL
  if ( ! missing(tables) ) {
    if ( is.character(tables) && length(tables)>0 ) {
      self$tablesSelected <- tables[ tables %in% idxTables$Table ]
    } else {
      self$tablesSelected <- character(0)
    }
  }
  group.selected <- ve.group.selected(idxTables$Group,self$groups)
  if ( length(self$tablesSelected)==0 ) {
    idxTables$Selected <- ifelse( group.selected, "Yes", "No (!Group)" )
  } else {
    idxTables$Selected <- ifelse(
      idxTables$Table %in% self$tablesSelected,
      ifelse( group.selected,
        "Yes","No (!Group)"
      ),
      "No")
  }
  return(idxTables)
}

ve.table.selected <- function(test.table,tables) {
  return ( test.table %in% tables$Table[tables$Selected=="Yes"] )
}

ve.model.fields <- function(fields) {
  # extract fields from the index where groups and tables are selected
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxFields <- self$modelIndex[,c("Group","Table","Name","Stage")]
  row.names(idxFields) <- NULL
  if ( ! missing(fields) ) {
    if ( is.character(fields) && length(fields)>0 ) {
      self$fieldsSelected <- fields[ fields %in% idxFields$Name ]
    } else {
      self$fieldsSelected <- character(0)
    }
  }
  table.selected <- ve.table.selected(idxFields$Table,self$tables)
  group.selected <- ve.group.selected(idxFields$Group,self$groups)
  tg.selected <- table.selected & group.selected
  if ( length(self$fieldsSelected)==0 ) {
    idxFields$Selected <- ifelse( tg.selected, "Yes", "No (!Table)" )
  } else {
    idxFields$Selected <- ifelse(
      idxFields$Name %in% self$fieldsSelected,
      ifelse( tg.selected,
        "Yes","No (!Table)"
      ),
      "No")
  }
  return(idxFields)
}

ve.field.selected <- function(test.field,fields) {
  return ( test.field %in% fields$Name[fields$Selected=="Yes"] )
}

ve.model.list <- function(selected=TRUE, pattern="", details=FALSE) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame self$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from self$modelIndex
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  filter <- if ( missing(selected) || selected ) {
    self$fields$Selected=="Yes"
  } else {
    rep(TRUE,nrow(self$modelIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$modelIndex$Name,ignore.case=TRUE )
  }
  if ( missing(details) || ! details ) {
    ret.fields <- c("Name")
  } else {
    ret.fields <- names(self$modelIndex)
  }
  ret.value <- self$modelIndex[ filter, ret.fields, drop=TRUE ]
  if ( class(ret.value)!='character' ) ret.value <- ret.value[order(ret.value$Stage, ret.value$Group, ret.value$Name),]
  return(unique(ret.value))
}

ve.model.inputs <- function( fields=FALSE, module="", filename="" ) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  if ( ! missing(fields) && fields ) {
    ret.fields <- c("File","Name","Description","Units","Module","Stage","Path")
  } else {
    ret.fields <- c("Module","File","Stage","Path")
  }

  filter <- rep(TRUE,nrow(self$modelInputs))
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,self$modelInputs$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,self$modelInputs$File)
  }

  ret.value <- unique(self$modelInputs[ filter, ret.fields ])
  return( ret.value[order(ret.value$Stage,ret.value$File),] )
}

# Build data.frames based on selected groups, tables and dataset names
ve.model.extract <- function(
  stage=NULL,
  saveTo="output",
  overwrite=FALSE,
  quiet=FALSE
) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  if ( is.null(stage) ) stage <- self$stageCount # Last one should have everything
  saving <- is.character(saveTo) && nzchar(saveTo)[1]
  
  visioneval::assignDatastoreFunctions(self$runParams$DatastoreType)
  fields <- ( self$fields )

  extract <- fields[ ( fields$Selected=="Yes" & fields$Stage==stage ) ,c("Name","Table","Group","Stage")]

  tables <- split( extract$Name, list(extract$Table,extract$Group,extract$Stage) )
  tables <- tables[which(sapply(tables,length)!=0)]
  DataSpecs <- lapply( names(tables), function(T.G.S) {
        TGS <- unlist(strsplit(T.G.S,"\\."))
        stage <- as.integer(TGS[3])
        mp <- self$modelPath[stage]
        ms <- self$modelState[[stage]]
        dstoreloc <- file.path(mp,ms$DatastoreName)
        df <- data.frame(
          Name  = tables[[T.G.S]],
          Table = TGS[1],
          Group = TGS[2],
          Loc   = dstoreloc
        )
        list(
          Data=df,
          File=paste0(paste(gsub("\\.","_",T.G.S),format(ms$LastChanged,"%Y-%m-%d_%H%M%S"),sep="_"),".csv"),
          Stage=stage
        )
      }
    )
  results <- lapply(DataSpecs, function(d) {
        if (!quiet && saving ) message("Extracting data for Table ",d$Data$Table[1]," in Group ",d$Data$Group[1])
        # Do this in a for-loop rather than faster "apply" to avoid dimension and class/type problems.
        # TODO: make sure this works for earlier stages where not all fields will be defined...
        ds.ext <- list()
        for ( fld in 1:nrow(d$Data) ) {
          dt <- d$Data[fld,]
          ds.ext[[dt$Name]] <- readFromTable(Name=dt$Name,Table=dt$Table,Group=dt$Group,DstoreLoc=dt$Loc,ReadAttr=FALSE)
        }
        return( data.frame(ds.ext) )
      }
    )
  files <- sapply(DataSpecs, function(x) x$File)
  stages <- sapply(DataSpecs, function(x) x$Stage)
  names(results) <- files
  if ( saving ) {
    mapply(
      names(results),
      stages,
      FUN=function(f,s) {
        data <- results[[f]]
        out.path <- file.path(self$modelPath[s],saveTo)
        if ( ! dir.exists(out.path) ) dir.create(out.path,recursive=TRUE)
        fn <- file.path(out.path,f)
        write.csv(data,file=fn)
        if (!quiet) message("Write output file: ",gsub(ve.runtime,"",fn))
      }
    )
  } else {
    names(results) <- sub("\\.[^.]*$","",names(results))
    if (!quiet) message("Returning extracted data as invisible list of data.frames\n(quiet=TRUE to suppress this message)")
  }
  invisible(results)
}

# Query.R

###########################################################################
# Required libraries
# Need to affix namespace resolution operator (stringr::...) to use functions

requireNamespace("stringr")

###########################################################################
# FUNCTION DEFINITIONS - helpers
###########################################################################

###########################################################################
# FUNCTION: makeMeasure
#
# Process a measureSpec for a Year and Geography
# Return the measureName, with the side effect that the value(s) of the measure
# are placed into measureEnv (whence they will later be summarized)
#
makeMeasure <- function(measureSpec,thisYear,Geography,QPrep_ls,measureEnv) {
  if ( Geography["Type"] != "Region" ) {
    GeoValue <- Geography["Value"]
    byRegion <- FALSE
  } else {
    GeoValue <- ""
    byRegion <- TRUE # skip processing "By" specifications
    # Yields one less dimension on results from summarizeDatasets
  }
  measureName <- measureSpec$Name

  # Skip or include measures based on presence of required Dataset
  if ( "Require" %in% names(measureSpec) ) {
    if ( ! visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      return(paste(measureName,"(SKIPPED due to Require:)"))
    }
  } else
  if ( "RequireNot" %in% names(measureSpec) ) {
    if ( visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      return(paste(measureName,"(SKIPPED due to RequireNot:)"))
    }
  }

  # Compute the measure based on the measureSpec     
  if ( "Function" %in% names(measureSpec) ) {
    measure <- eval(parse(text=measureSpec$Function), envir=measureEnv)
    names(measure) <- measureName
  } else
  if ( "Summarize" %in% names(measureSpec) ) {
    sumSpec <- measureSpec$Summarize
    if ( ! byRegion ) {
      if ( ! "By" %in% names(sumSpec) ||
           ! Geography["Type"] %in% sumSpec$By ) {
        stop(paste("Script wants Geography Type ",Geography["Type"]," in 'By' but got ",sumSpec$By,"",sep="'"))
      }
    }
    usingBreaks <- "Breaks" %in% names(sumSpec) && ! is.null(sumSpec$Breaks)
    usingKey <- "Key" %in% names(sumSpec) && ! is.null(sumSpec$Key)
    assign("spec",sumSpec,envir=globalenv())
    measure <- visioneval::summarizeDatasets(
        Expr = sumSpec$Expr,
        Units_ = sumSpec$Units,
        By_ = if ( ! byRegion || usingBreaks ) sumSpec$By else NULL,
        Breaks_ls = if ( usingBreaks) sumSpec$Breaks else NULL,
        Table = sumSpec$Table,
        Key = if ( usingKey ) sumSpec$Key else NULL,
        Group = thisYear,
        QueryPrep_ls = QPrep_ls
      )
    if ( ! byRegion && ! usingBreaks ) {
      # For now, GeoValue must be present and just a single value
      # TODO: if measure is a vector, its elements will have names of the
      # values that were applied (all value of Geography, but if By was breaks
      # at a regional level, it will be the default break names)
      measure <- measure[GeoValue]  # reduce to scalar value (one geographical unit)
      names(measure) <- measureName
    } else {
      # TODO: base the following on the dimensions of measure and whether or not
      # we have GeoValue. Rows are the first dimension, columns are the second dimension
      # Using as.vector() to flatten the matrix will do (though it removes names)
      # for each column; within column, for each row. Assign the names first,
      # then name the elements - names do not have to be unique, so we can build
      # one dimension at a time.
      if ( ! byRegion ) { # need to reduce to vector for GeoValue
        measure <- measure[,GeoValue]
      }
      assign("measure",measure,envir=globalenv())
      if ( usingBreaks ) {
        if ( "BreakNames" %in% names(sumSpec) ) {
          breakNames <- sumSpec$BreakNames[[sumSpec$By[1]]]
        } else {
          breakNames <- as.character(sumSpec$Breaks[[sumSpec$By[1]]])
        }
        names(measure) <- paste(measureName,c("min",breakNames),sep=".")
      } else {
        if ( length(measure) != 1 ) {
          message("Processing measure: ",measureName)
          stop("Program error: expected scalar measure, got vector:",measure)
        }
        names(measure) <- measureName
      }
    }
  } else {
    stop("Invalid Measure Specification")
  }
  
  # Stash the measure results in measureEnv
  for ( nm in names(measure) ) {
    msr <- measure[nm]
    attributes(msr) <- list(
      Units = measureSpec$Units,
      Description = measureSpec$Description
    )
    assign(nm,msr,env=measureEnv)
  }

  # Return the name(s), for output tracking
  return(names(measure))
}

###########################################################################
# FUNCTION: makeMeasureDataFrame
#
# Extract the measures made by makeMeasure from measureEnv and put them in a
# data.frame suitable for writing to the output file
#
makeMeasureDataFrame <- function(measureEnv) {
  Measures_     <- objects(measureEnv)
  Values_       <- sapply(Measures_, get, envir=measureEnv)
  Units_        <- unname(sapply(Measures_, function(x) attributes(get(x,envir=measureEnv))$Units))
  Description_  <- unname(sapply(Measures_, function(x) attributes(get(x,envir=measureEnv))$Description))
  Data_df       <- data.frame(
    Measure     = Measures_,
    thisYear    = Values_,
    Units       = Units_,
    Description = Description_
  )
  # The following addresses a unique naming standard in original script
  # TODO: make it obsolete
  Data_df$Measure <- gsub("_Ma", "_", Data_df$Measure)
  Data_df$Measure <- gsub("_$", "", Data_df$Measure)
  Data_df$Measure <- gsub("_\\.", ".", Data_df$Measure)
  rownames(Data_df) <- NULL
  return(Data_df)
}

###########################################################################
# FUNCTION: ve.model.query
#
# VEModel function to process a query specification against a Datastore
#

#PROCESS QUERY DEFINITIONS AGAINST A DATASTORE
#================================================================
#' Process a set of query definitions against the model's final Datastore
#'
#' \code{ve.model.query} an R6 function for VEModel that processes query specifications
#' using the model's Datastore
#'
#' This function will create a set of output files with summary metrics in it. It will
#' compute the specs for all the years in the Datastore, but if one of those years is
#' left out of the VEmodel$groups, it will not be processed.
#'
#' @param Geography a named character vector with elements "Type" (currently supports
#' either "Region", "Azone" or "Marea") and "Value" (any valid value for the corresponding Type
#' from the model's geo.csv file; for Region, an empty string).
#' @param SpecFile is the file name of the file containing the query. Relative path
#' interpreted relative to the model path (so you can put it next to run_model.R)
#' @param Spec is a list of specifications (already loaded - for one-offs or testing)
#' @param outputFile template for generating scenario output file
#' @param saveTo sub-directory of model path into which to write output files (defaults to "output" like extract)
#' @param log, one of c("WARN","ERROR") for level at which to generate trace details (default "ERROR")
#' @return A character vector with the names of the .csv files containing the computed measures.
#' @export
ve.model.query <- function(
  Geography, # required - although perhaps default to 'Region'?
  GeoValue, # optional - if Geography is not Region, only compute for this list
  Spec, # Can submit a list (like what is contained in Query-Spec.R)
  SpecFile = "Query-Spec.R",
  outputFile = "Measures_%scenario%_%years%_%geography%.csv",
  #   Default is a long-but-informative filename
  saveTo="output", # folder in which to put outputFile (if FALSE or empty string, return list of data.frames)
  log="ERROR"  # set to "WARN" if to get detailed information on warnings as they happen
  )
{
  if ( missing(Geography) ||
    ( ! is.character(Geography) ) ||
    ( ! Geography %in% c("Region", "Azone","Marea") ) )
  {
    message("Geography must be one of 'Region','Marea' or 'Azone'")
    return(character(0))
  }
  if ( Geography %in% c("Azone","Marea") ) {
    if ( missing(GeoValue) || ! is.character(GeoValue) || length(GeoValue)>1 || ! nzchar(GeoValue) ) {
      message("Not supported: Breaking measures by ",Geography,"; including all values")
      # TODO: need to assemble proper combinations of By/GeoValues when unpacking results from
      # summarizeDatasets in makeMeasure: we end up with a 2-D matrix, not a vector or scalar, and
      # we need to transform that to a long form with suitable names for each element
      # (Measure-GeoValue-ByLevel). Not hard, just book-keeping (remove matrix dim to get a vector,
      # but understand row/column order and build suitable names, checking length/order of names
      # against original dim)
      # NOTE: the resulting array/matrix has dimension names reflecting the "By" element values;
      # Use those by default (but we can override the break descriptions)
      return(character(0))
    } else {
      message("Evaluating measures for each ",Geography,": ",GeoValue)
    }
  } else {
    GeoValue <- "" # Region has no GeoValue
    message("Evaluating measures for region")
  }
  Geography <- c(Type=Geography,Value=GeoValue) # prepare to do the query

  # Get SecenarioRoot and Scenarios using modelPaths
  # TODO: if we processing actual scenarios, we'll set the root a little differently

  # We'll look for the SpecFile in path or dirname(path)
  dataPath <- self$modelPath[self$stageCount]
  ScenarioRoot <- dirname(dataPath) # For now, just the one final stage of the model

  Scenarios <- normalizePath(dataPath,mustWork=FALSE)
  sd <- dir(Scenarios)
  if (
    self$status != "Complete" ||
    length(grep("Datastore",sd))==0 ||
    length(grep("ModelState.Rda",sd))==0
  ) {
    # TODO: this may work with a vector of Scenarios; we can use the grep
    #   results to determine which of Scenarios has model outputs
    # TODO: eventually open the ModelState and determine the run status
    message("Model appears not to have been run yet: ",self$status)
    return(character(0))
  }

  # Compute the saveTo location (if requested) relative to dataPath
  if ( is.logical(saveTo) ) {
    if ( saveTo ) # If somebody says saveTo=TRUE, they mean saveTo="output" (default)
      saveTo = "output" # re-install the default
  } 
  if ( is.character(saveTo) ) { # could be a directory
    if ( ! nzchar(saveTo) ) saveTo <- "output" # empty string replaced by default
    if ( ! ( grepl("^[/\\]",saveTo) | grepl("^.:",saveTo) ) ) { # relative path
      # saveTo is a relative path, so create it relative to dataPath
      saveTo <- normalizePath(file.path(dataPath,saveTo),winslash="/",mustWork=FALSE)
    } # else use saveTo as it is
    if ( ! dir.exists(saveTo) ) dir.create(saveTo,recursive=TRUE,showWarnings=FALSE)
    if ( ! dir.exists(saveTo) ) { # which it won't if saveTo was absolute but made no sense, e.g. missing drive
      message("saveTo directory invalid: ",saveTo)
      saveTo <- FALSE
    } else {
      message("Writing query output into directory: ",saveTo)
    }
  } else {
    if ( ! is.logical(saveTo) ) message("saveTo parameter is invalid: ",saveTo)
    saveTo <- FALSE
  }

  # Years are those defined for the model, less any that are not selected via $groups
  # We will only query groups that are Years from the runParams
  Years <- self$runParams$Years
  groups <- self$groups
  Years <- Years[Years %in% groups$Group[groups$Selected=="Yes"]]
  if ( length(Years)==0 || any(is.na(Years)) ) {
    message("Invalid Years specified")
    cat("No years appear to be selected (check VEmodel$groups)\n")
    cat("Model has these Years available:",self$runParams$Years,"\n")
    return(character(0))
  }

  # Gather the specifications, if they're not supplied via "Spec" parameter
  if ( ! missing(Spec) && ! is.list(Spec) ) {
    PMSpecifications <- Spec
    message("Specifications from existing list")
  } else {
    Spec <- NULL
  }
  if ( is.null(Spec) ) {
    # No pre-manufactured Spec.
    # Read SpecFile if Spec not passed as a parameter
    if ( ! is.character(SpecFile) || ! nzchar(SpecFile[1]) ) {
      message("Invalid SpecFile")
      cat("Provide the name, with optional path, to the file with the Query Specifications.\n")
      cat("SpecFile Path is relative to the model directory.\n")
      return(character(0))
    }
    for ( specName in unique(c(SpecFile,"Query-Spec.R")) ) {
      if ( !file.exists( SpecFile ) ) {
        SpecFile <- file.path(dataPath,SpecFile)
        if ( ! file.exists( SpecFile ) ) {
          SpecFile <- file.path(ScenarioRoot,SpecFile)
        }
      }
      if ( file.exists(SpecFile) ) break
    }
    SpecFile = normalizePath(SpecFile,winslash="/",mustWork=FALSE)
    if ( ! file.exists(SpecFile) ) {
      message("Specification File ",SpecFile," does not exist")
      return(character(0))
    } else {
      specEnv <- new.env()
      sys.source(SpecFile,envir=specEnv)
      specs <- objects(specEnv)
      if ( length(specs) != 1 ) {
        print(specs)
        message("Must define a single specification list in ",SpecFile)
        cat(SpecFile,"contains: ",paste(specs,collapse=", "),"\n")
        return(character(0))
      }
      PMSpecifications <- get(specs,envir=specEnv)
      displaySpec <- if ( exists("ve.runtime") ) {
        sub(ve.runtime,"",SpecFile)
      } else {
        SpecFile
      }
      message(paste("Specification File: ",displaySpec,"",sep="'"))
      rm(specEnv)
    }
  }

  # Superficial sanity check of PMSpecifications (deeper checks within visioneval::summarizeDatasets)
  # Also rewrite the geography (so we can mostly reuse spec files for "Region","Marea", or "Azone"
  # TODO: Probably need better error recovery
  have.names <- character(0)
  spec.valid <- is.list(PMSpecifications)
  specProcessed <- list()
  small.geo <- c("Marea","Azone")
  if ( spec.valid ) {
    for ( test.spec in PMSpecifications ) {
      nm.test.spec <- names(test.spec) # may be NULL
      if ( is.null(nm.test.spec) ) spec.valid <- FALSE
      if ( spec.valid ) {
        have.names <- nm.test.spec %in% c("Name","Units","Description","Function","Summarize","Require","RequireNot")
        # have.names will be logical(0) if nm.test.spec is NULL
        spec.valid <- all(have.names)
        if ( ! spec.valid ) {
          if ( !all(is.na(have.names)) ) {
            message("Unknown specification elements: ",
              paste(nm.test.spec[!have.names],collapse=", ")
            )
          } else {
            message("Unrecognized specification list element:")
            print(test.spec)
          }
          spec.valid <- FALSE
        } else if ( "Summarize" %in% nm.test.spec ) {
          test.sum <- test.spec[["Summarize"]]
          if ( Geography["Type"] == "Region" ) {
            # Region: remove Marea or Azone from "By" and "Units", if present
            if ( "By" %in% names(test.sum) ) {
              test.by <- test.sum[["By"]]
              any.geo <- ( test.by %in% small.geo )
              if ( any ( ! any.geo ) ) { # By includes tables other than geography
                # remove geography but leave the rest
                # cat( "In ",test.spec[["Name"]],"By from:",test.by,"to",test.by[!any.geo],"\n" )
                test.sum[["By"]] <- test.by[!any.geo]
              } else if ( all(any.geo) ) {
                # cat( "In ",test.spec[["Name"]],"Removing all from",test.by,"\n" )
                test.sum["By"] <- NULL # Single brackets - remove element entire
              } # else leave "By" untouched.
            }
            if ( "Units" %in% names(test.sum) ) {
              test.units <- test.sum[["Units"]]
              any.geo <- ( names(test.units) %in% small.geo )
              if ( any ( ! any.geo ) ) {
                # remove geography
                test.sum[["Units"]] <- test.units[!any.geo]
              } else if ( all(any.geo) ) { # Only has the geo table in Units
                test.sum["Units"] <- NULL # Single brackets - remove element entire
              } # else leave "Units" untouched
            }
          } else { # Summarizing by geography ("Azone" or "Marea")
            # TODO: Geography "Value" should eventually be screened against model's 'defs/geo.csv'
            # TODO: If "Table" is the same as "By" and not GeographyType, skip that specification
            # with a message (or we could use Require). So any dip into the Marea table or the
            # Azone table only gets processed if we are running for that GeographyType.
            if ( ! Geography["Type"] %in% small.geo ) {
              message("Invalid Geography Type for query specification: ",Geography["Type"])
              spec.valid <- FALSE
              next
            }
            geotest <- ( test.sum[["Table"]] %in% small.geo ) # Which Table elements are the small geography
            # Write the following in case more than one Table element is a small geography
            # Mostly, that would probably be a logic error in the query specification
            if ( any( geotest) && any(test.sum[["Table"]][geotest] != Geography["Type"]) ) {
              message(
                "Skipping specification ",test.spec[["Name"]],
                " due to Table mismatch: ",
                Geography["Type"]," vs. Table ",paste(test.sum[["Table"]][geotest],collapse=", ")
              )
              next  # Skip thist test.spec
            }
            # If Table is not a conflicting small.geo,
            # swap small geography in spec with Geography["Type"]
            if ( Geography["Type"] == "Marea" ) {
              geo.from <- "Azone"
              geo.to <- "Marea"
            } else if ( Geography["Type"] == "Azone" ) {
              geo.from <- "Marea"
              geo.to <- "Azone"
            }
            # Check that "By" and "Units" include geo.from
            # If geo.from BUT NOT geo.to in "By" and "Units", change geo.from to geo.to
            #   if we have both in the spec, don't touch geo.from or geo.to
            # If geo.to not in "By" and "Units", add geo.to to By and geo.to = '' to "Units"
            test.sum.by <- test.sum[["By"]]
            azb <- ( test.sum.by %in% geo.from )
            if ( ! ( geo.to %in% test.sum.by ) ) {
              if ( any(azb) ) {
                test.sum.by[azb] <- geo.to
              } else {
                test.sum.by <- c(test.sum.by,geo.to)
              }
              test.sum[["By"]] <- test.sum.by
            }
            test.sum.units <- test.sum[["Units"]]
#             cat("Spec name:",test.spec[["Name"]],"\n")
#             cat("Units before:",paste(names(test.sum.units),collapse=","),"\n")
            azb <- test.sum.units %in% geo.from
            if ( ! (geo.to %in% names(test.sum.units)) ) {
              if ( any(azb) ) {
                names(test.sum.units)[azb] <- geo.to
              } else {
                test.sum.units[geo.to] <- ""
              }
              test.sum[["Units"]] <- test.sum.units
            }
#             cat("Units after:",paste(names(test.sum.units),collapse=","),"\n")
          }
          test.spec[["Summarize"]] <- test.sum
        }
        if ( spec.valid ) {
          specProcessed[[length(specProcessed)+1]] <- test.spec
        }
      }
      if ( ! spec.valid ) break
    }
    if ( spec.valid) PMSpecifications <- specProcessed
  }
  if ( ! spec.valid ) {
    # Report failing spec name, if it has one...
    if ( !is.na(have.names) && have.names[1] ) cat("In Specification '",test.spec$Name,"\n")
    message("Invalid measure specification.")
    return(character(0))
  }
  if ( length(specProcessed) == 0 ) {
    message("No valid measure specifications provided.")
    return(character(0))
  }

  # Now run the query
  outputFiles <- doQuery(
    Scenarios=Scenarios,
    Years=Years,
    Geography=Geography,
    Specifications=PMSpecifications,
    outputFile=outputFile,
    saveTo=saveTo,
    DatastoreType=self$runParams$DatastoreType,
    log=log.level(log)
  )

  invisible(outputFiles)
}

############################################################
# PROCESS QUERY SPECIFICATIONS ON DATASTORE
#
###########################################################################
# Process the Specification list
###########################################################################

doQuery <- function (
  Scenarios,
  Years,
  Geography,
  Specifications,
  outputFile,
  saveTo,
  DatastoreType,
  log=futile.logger(futile.logger::WARN)
)
{
  if (
    missing(Scenarios) ||
    missing(Years) ||
    missing(Geography) ||
    missing(Specifications) ||
    missing(outputFile) ||
    missing(DatastoreType)
  ) {
    message("Invalid Setup for doQuery function")
    return(character(0))
  }
    
  saving <- is.character(saveTo) && length(saveTo)==1 && nzchar(saveTo)[1] && dir.exists(saveTo)
  if ( ! saving ) {
    outputFiles <- list() # will return list of data.frames
  } else {
    outputFiles <- character(0) # will return vector of file names
  }
  # Put the Specifications where we can review them against the outputs
  attr(outputFiles,"Specifications") <- Specifications

  old.wd <- getwd()

  outputFiles <- character(0)
  futile.logger::flog.threshold(log)
  tryCatchLog::tryCatchLog(
    {
      for ( scenario in Scenarios ) {
        # scenario contains a path to a working directory with a Datastore in it

        # Move to scenario directory
        setwd(scenario)

        # Scenario Name for reporting / outputFile
        scenarioName <- basename(scenario)

        # Confirm what we're working on
        catYears <- paste(Years,collapse=",")
        catGeography <- Geography["Type"]
        if ( Geography["Type"]!="Region" &&
          (
            "Value" %in% Geography &&
            ! any(is.null(Geography["Value"])) &&
            ! any(is.na(Geography["Value"]))
          )
        ) {
          catGeography <- paste(catGeography,"=",paste0("'",Geography["Value"],"'"))
        }
        cat(
          "Building measures for:\n",
          "Scenario:",scenarioName,"\n",
          "Years:",catYears,"\n",
          "Geography:",catGeography,"\n"
        )

        # Build the outputFile name using the just reported specifications
        outputFileToWrite <- stringr::str_replace(outputFile,"%scenario%",scenarioName)
        outputFileToWrite <- stringr::str_replace(outputFileToWrite,"%years%",catYears)
        outputFileToWrite <- stringr::str_replace(outputFileToWrite,"%geography%",stringr::str_remove_all(catGeography,"[ ']"))
        if ( saving ) {
          outputFileToWrite <- normalizePath(file.path(saveTo,outputFileToWrite),mustWork=FALSE)
        } else {
          outputFileToWrite <- sub("\\.[^.]+$","",outputFileToWrite) # use this as data.frame name (dropping any file extension)
        }

        # Prepare for datastore queries
        #------------------------------
        prepForQuery <- function() {
          visioneval::prepareForDatastoreQuery(
            DstoreLocs_ = c("Datastore"),
            DstoreType = DatastoreType
          )
        }
        QPrep_ls <- prepForQuery()

        # Create the name of the data.frame that will collect the results
        Measures_df <- NULL

        # Iterate across the Years in the scenario
        for ( thisYear in Years ) {

          cat("Working on Year",thisYear,"\n")
          results <- new.env()

          # Iterate over the measures
          for ( measureSpec in Specifications ) {
            cat("Processing ",measureSpec$Name,"...",sep="")
            measure <- makeMeasure(measureSpec,thisYear,Geography,QPrep_ls,results)
            if ( length(measure)>1 ) for ( m in measure ) cat( paste0(m,"||") )
            cat("..Processed\n")
          }

          # Add this Year's measures to the output data.frame
          temp <- makeMeasureDataFrame(results)
          if ( is.null(Measures_df) ) {
            Measures_df<-temp[,c("Measure","Units","Description","thisYear")]
          } else {
            Measures_df<-cbind(Measures_df,thisYear=temp$thisYear )
          }
          names(Measures_df)[names(Measures_df)=="thisYear"]<-thisYear
        }

        # Add the measures to the output list
        if ( saving ) {
          cat("Saving measures in",basename(dirname(outputFileToWrite)),"as",basename(outputFileToWrite),"...")
          write.csv(Measures_df, row.names = FALSE, file = outputFileToWrite)
          cat("Saved\n")
          outputFiles <- c(outputFiles,outputFileToWrite) # Saving: return list of file names
        } else {
          outputFiles[outputFileToWrite] <- (Measures_df) # Not Saving: return list of data.frames
        }
      }
    },
    error=function(e) {
      cat("Error:",geterrmessage(),"\n")
    },
    finally=setwd(old.wd)
  )
  return(outputFiles)
}

# ve.query.move function
ve.query.move <- function(from,to="top") {
  # from is a vector of indices into self$querySpec, either integers or names
  # to is an index (integer or name) of length 1, which must not exist in "from"
  #  (moving a slice inside itself is a noop).
}

# S3 classes and generic functions to support them (used by VEOutput)
# These wrap a data.frame (subset from VEOutput$modelIndex)
# VEfields
# VEgroups
# VEtables

# S3 class wrapping a single query spec
# Could construct an edit function for it...
# Helper function creates an individual spec from parameters
# VEQuerySpec

# Here is the emerging VEQuery R6 class
# One of these is constructed by VEOutput$query
# Perhaps have some S3 generic functions defined...

VEQuery <- R6::R6Class(
  "VEQuery",
  public = list(
    initialize=ve.init.query,
    print=ve.query.print,
    run=ve.query.run,               # Option to save
    copy=ve.query.copy,             # Duplicates the query (for further editing)
    save=ve.query.save,             # With optional file name prefix (this does an R 'dump' to source)
    load=ve.query.load,             # With a file selection dialog if no name available and interactive()
    print=ve.query.print,           # With optional details
    search=ve.query.search,         # Search for a spec by some feature (returns a list)
    insert=ve.query.insert,         # Add a pre-constructed spec (list element class VEQuerySpec) to querySpec list (before param)
    move=ve.query.move,             # Reorder querySpec using from/to specification
    check=ve.query.check            # Ensure specs are all valid (can use a subset from search)
  ),
  active = list(
    geography=ve.query.geography,
    geovalue=ve.query.geovalue,
    specs=ve.query.specs            # Active interface to querySpec
    ),
  private = list(
    queryGeo=NULL,
    queryGeoValue=NULL,
    queryFile="",
    queryResults=NULL,              # list of data.frames with query results
    querySpec=list(),               # access via active specs
    queryPrep=NULL,                 # structure for running with summarizeDatasets code
    queryRunSpec=NULL,              # specs after filtering for validity against geography, etc.
    outputObject=NULL               # VEOutput object, passed through "$new"
  )
)

# Here is the VEOutput R6 class
# One of these is constructed by VEModel$output

VEOutput <- R6::R6Class(
  "VEOutput",
  public = list(
    model=ve.output.model,          # Access to parent model
    initialize=ve.init.output,
    select=ve.output.select,
    extract=ve.output.extract,
    list=ve.output.list,
    search=ve.output.list,
    inputs=ve.output.inputs,
    print=ve.output.print,
    units=ve.output.units,          # Set units on field list (modifies private$modelIndex)
    query=ve.output.query           # Create a VEQuery object from named query file, or menu
  active = list(
    groups=ve.output.groups,
    tables=ve.output.tables,
    fields=ve.output.fields
  ),
  private = list(
    modelObject=NULL,               # passed to $initialize from object creating it
    outputPath=NULL,                # root for extracts and queries; modelPath/output
    modelInputs=NULL,
    modelIndex=NULL,
    ModelState=NULL,
    groupsSelected=character(0),
    tablesSelected=character(0),
    fieldsSelected=character(0),
    index=ve.output.index
  )
)

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    modelName=NULL,
    modelPath=NULL,
#     modelInputs=NULL,                      # VEOutput object
#     modelIndex=NULL,                       # VEOutput object
#     groupsSelected=character(0),           # VEOutput object
#     tablesSelected=character(0),           # VEOutput object
#     fieldsSelected=character(0),           # VEOutput object
    stageCount=NULL,
    runParams=NULL,
    runStatus=NULL,
    initialize=ve.init.model,
    run=ve.run.model,
    print=ve.print.model,                  # provides generic print functionality
    dir=ve.model.dir,
    clear=ve.model.clear,
    copy=ve.model.copy,
    output=ve.model.output,                # Create a VEOutput object (if model is run)
#     select=ve.model.select,                # VEOutput object
#     extract=ve.model.extract,              # VEOutput object
#     list=ve.model.list,                    # VEOutput object
#     search=ve.model.list,                  # VEOutput object
#     inputs=ve.model.inputs                 # VEOutput object
  ),
  active = list(
    status=ve.model.status,
#     groups=ve.model.groups,                # VEOutput
#     tables=ve.model.tables,                # VEOutput
#     fields=ve.model.fields                 # VEOutput
  ),
  private = list(
    runError=NULL,
    artifacts = ve.artifacts,
#     ModelState=NULL,                       # VEOutput
#     index=ve.model.index                   # VEOutput
  )
)

# The openModel function is the only thing exported
openModel <- function(modelPath, modelName = NULL) {
  return( VEModel$new(modelPath = modelPath, modelName = modelName) )
}

# The following functions run the command line model versions per the
# Getting Started document.  Optional "scenarios" argument, if TRUE, will
# run the scenarios version of the test models.

verpat <- function(Scenarios = FALSE) {
  if ( ! Scenarios ) {
    model <- openModel("VERPAT")
  } else {
    model <- openModel("VERPAT_Scenarios")
  }
  model$run()
  model
}

verspm <- function(Scenarios = FALSE, MM = FALSE) {
  if ( Scenarios ) {
    model <- openModel("VERSPM_Scenarios")
  } else if ( MM ) {
    model <- openModel("VERSPM_MM")
  } else {
    model <- openModel("VERSPM")
  }
  model$run()
  model
}

vestate <- function(staged = FALSE) {
  if ( ! staged ) {
    model <- openModel("VE-State")
  } else {
    model <- openModel(dir(pattern=".*Stage-\\d",file.path(ve.runtime,"models","VE-State-Staged"),full.names=TRUE))
  }
  model$run()
  model
}
