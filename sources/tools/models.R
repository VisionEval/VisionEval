# Author: Jeremy Raw

# VisionEval helper tools
# NOTE: these all depend on having set up the VisionEval environment
# See VisionEval.R in the installation runtime (VE-Installer/boilerplate)

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

# tool.contents <- c("openModel","verpat","verspm","vestate","go")
tool.contents <- c("openModel","verpat","verspm","vestate","go")

requireNamespace("jsonlite")
requireNamespace("R6")
requireNamespace("visioneval")

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
  # Figure out if we can use modelPath to find "run_model.R" script(s)
  if ( all ( file.exists(modelPath) & toupper(basename(modelPath))=="run_model.R" ) ) {
    # Provided full path to run_model.R (possibly more than one)
    modelPath <- dirname(modelPath)
  } else if ( ! all( file.exists( file.path(modelPath,"run_model.R") ) ) ) {
    if ( length(modelPath)==1 ) {
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
    self$modelName <- names(self$modelPath)[1]
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

ve.run.model <- function(verbose=TRUE,path=NULL) {
  pathLength <- length(self$modelPath)
  stageStart <- if ( ! is.null(path) ) path else 1
  for ( ms in stageStart:self$stageCount ) {
    stage <- self$modelPath[ms]
    if ( verbose ) {
      if ( is.na(stage) ) {
        message("ms: ",ms)
        message("stageStart: ",stageStart)
        stop("bad first stage")
      }
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
    tryCatch(
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

ve.model.dir <- function(pattern=NULL,recursive=FALSE,shorten="",path=NULL) {
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

# outputOnly will just delete the extraction results
# (not the model run)
ve.artifacts <- function(path=NULL,outputOnly=FALSE) {
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

ve.model.clear <- function(force=FALSE,outputOnly=FALSE,path=NULL) {
  to.delete <- private$artifacts(path=path,outputOnly=outputOnly)
  if ( length(to.delete)>0 ) {
    print(gsub(ve.runtime,"",to.delete))
    if ( force || (force <- confirm("Clear ALL prior model results?")) ) {
      unlink(to.delete,recursive=TRUE)
      self$modelState <- lapply(
        self$modelPath,
        function(x) list()
      )
      cat("Model results cleared.\n")
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
  }
  if ( ! dir.exists(newPath) ) newPath <- dirname(newPath)
  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath,showWarnings=FALSE)
  for ( p in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(path=p),private$artifacts(path=p))
    copy.to <- newModelPath # file.path(newModelPath,basename(self$modelPath[p]))
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
    ccases <- complete.cases(GroupTableName)
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

ve.model.groups <- function(groups) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxGroups <- unique(self$modelIndex[,c("Group","Stage")])
  row.names(idxGroups) <- NULL
  if ( ! missing(groups) ) {
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

ve.model.list <- function(selected=TRUE,pattern="",index=FALSE) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  filter <- if ( missing(selected) || selected ) {
    ve.field.selected( self$modelIndex$Name, self$fields )
  } else {
    rep(TRUE,nrow(self$modelIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$modelIndex$Name )
  }
  if ( missing(index) || ! index ) {
    ret.fields <- c("Name")
  } else {
    ret.fields <- names(self$modelIndex)
  }
  ret.value <- self$modelIndex[ filter, ret.fields, drop=TRUE ]
  if ( class(ret.value)!='character' ) ret.value <- ret.value[order(ret.value$Stage, ret.value$Group, ret.value$Name),]
  return(ret.value)
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
  fields <- self$fields
  extract <- fields[fields$Selected=="Yes",c("Name","Table","Group","Stage")]
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
    if (!quiet) message("Returning extracted data as invisible list of data.frames\n(quiet=TRUE to suppress this message)")
  }
  invisible(results)
}

# Here is VEModel R6 class

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    modelName=NULL,
    modelPath=NULL,
    modelState=NULL,
    modelOutputs=NULL,
    modelInputs=NULL,
    modelIndex=NULL,
    groupsSelected=character(0),
    tablesSelected=character(0),
    fieldsSelected=character(0),
    stageCount=NULL,
    runParams=NULL,
    runStatus=NULL,
    initialize=ve.init.model,
    run=ve.run.model,
    print=ve.print.model,
    dir=ve.model.dir,
    clear=ve.model.clear,
    copy=ve.model.copy,
    extract=ve.model.extract,
    list=ve.model.list,
    inputs=ve.model.inputs
  ),
  active = list(
    status=ve.model.status,
    groups=ve.model.groups,
    tables=ve.model.tables,
    fields=ve.model.fields
  ),
  private = list(
    runError=NULL,
    ModelState=NULL,
    artifacts = ve.artifacts,
    index=ve.model.index
  )
)

# The openModel function is the only thing exported
openModel <- function(modelPath,modelName=NULL) {
  return( VEModel$new(modelPath=modelPath,modelName=modelName) )
}

# The following functions run the command line model versions per the
# Getting Started document.  Optional "scenarios" argument, if TRUE, will
# run the scenarios version of the test models.

verpat <- function(Scenarios=FALSE) {
  if ( ! scenarios ) {
    model <- openModel("VERPAT")
  } else {
    model <- openModel("VERPAT_Scenarios")
  }
  model$run()
  model
}

verspm <- function(Scenarios=FALSE,MM=FALSE) {
  if ( scenarios ) {
    model <- openModel("VERSPM_Scenarios")
  } else if ( mm ) {
    model <- openModel("VERSPM_MM")
  } else {
    model <- openModel("VERSPM")
  }
  model$run()
  model
}

vestate <- function(staged=FALSE) {
  if ( ! staged ) {
    model <- openModel("VE-State")
  } else {
    model <- openModel(dir(pattern=".*Stage-\\d",file.path(ve.runtime,"models","VE-State-Staged"),full.names=TRUE))
  }
  model$run()
  model
}
