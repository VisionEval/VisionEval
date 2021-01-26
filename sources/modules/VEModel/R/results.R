# Results.R
#' @include defaults.R
self=private=NULL

# Output just wraps a ModelState and Datastore for one stage
# It maintains everything we need for a QueryPrep_ls structure for queries
# Plus it can export slices of the Datastore into .csv or data.frame
ve.results.init <- function(OutputPath,Param_ls=NULL) {
  # OutputPath is the normalized path to a directory containing the model results
  #  typically from the last model stage. Expect to find a ModelState.Rda file
  #  and a Datastore in that folder.
  # Param_ls is the list of Run Parameters used by the model
  self$path <- OutputPath
  self$Name <- basename(OutputPath)
  private$RunParam_ls <- Param_ls
  private$index()
  return(self$valid())
}

ve.results.valid <- function() {
  valid <- ! is.null(private$RunParam_ls) &&
           dir.exists(self$path) &&
           !is.null(private$modelIndex) && length(private$modelIndex)>0 &&
           !is.null(private$modelInputs) && length(private$modelInputs)>0
  modelStateFile <- file.path(self$path,visioneval::getRunParameter("ModelStateFileName",Param_ls=private$RunParam_ls))
  valid <- valid && all(file.exists(modelStateFile))
  return(valid)
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

ve.results.index <- function() {
  # Load model state from self$path
  ve.model <- new.env()
  FileName=file.path(self$path,visioneval::getModelStateFileName(Param_ls=private$RunParam_ls))
  # TODO: Make this work with archived ModelState (if it has a timestamp in its name)
  ms <- private$ModelState <- try(visioneval::readModelState(FileName=FileName))
  if ( ! is.list(private$ModelState) ) {
    private$ModelState <- NULL
    visioneval::writeLog(Level="error",paste("Cannot load ModelState from:",FileName))
    return(list())
  }
  if ( is.null(private$RunParam_ls) && is.list(private$ModelState ) ) {
    private$RunParam_ls <-private$ModelState$RunParam_ls
  }
  owd <- setwd(self$path)
  on.exit(setwd(owd))
    
  # TODO: sort out the "Load" DatastoreName parameter and the "Run" DatastoreName
  if ( ! file.exists( ms$DatastoreName ) ) {
    message("Datastore for this model is not available. Has it run successfully?")
    return(list())
  }

  Index <- data.frame()
  Inputs <- data.frame()

  if ( length(ms)==0 ) {
    return(list())
  }
  if ( ! "Datastore" %in% names(ms) ) {
    message("Datastore not defined in ModelState: ",paste(names(ms),collapse=","))
    message("Clear model results and try again.")
    return(list())
  } else if ( ! is.list(ms$Datastore) ) {
    message("Datastore is incomplete: ",class(ms$Datastore)," ",length(ms$Datastore))
    message("Clear model results and try again.")
    return(list())
  }
  ds <- (ms$Datastore)

  InputIndex <- sapply(ds$attributes, attributeExist, "FILE")
  Description <- sapply(ds$attributes, attributeGet, "DESCRIPTION",simplify=TRUE) # should yield a character vector
  Module <- sapply(ds$attributes, attributeGet, "MODULE",simplify=TRUE) # should yield a character vector
  Units <- sapply(ds$attributes, attributeGet, "UNITS",simplify=TRUE) # should yield a character vector

  # Build parallel data.frame for Inputs
  # message("Input data frame...")
  File <- sapply(ds$attributes, attributeGet, "FILE",simplify=TRUE) # should yield a character vector
  inputs <- data.frame(
    Module      = Module[InputIndex],
    Name        = ds$name[InputIndex],
    File        = File[InputIndex],
    Description = Description[InputIndex],
    Units       = Units[InputIndex],
    Scenario    = visioneval::getRunParameter("Scenario",Default="Unknown Scenario",Param_ls=private$RunParam_ls),
    Path        = self$path
  )
  Inputs <- rbind(Inputs,inputs)

  Description <- Description[!InputIndex]
  Module <- Module[!InputIndex]
  Units <- Units[!InputIndex]
  splitGroupTableName <- strsplit(ds[!InputIndex, "groupname"], "/")
  if ( length(Description) != length(splitGroupTableName) ) stop("Inconsistent table<->description correspondence")

  maxLength <- max(unlist(lapply(splitGroupTableName, length)))
  if ( maxLength != 3 ) {
    visioneval::writeLog(Level="warn",paste0("Model state at ",self$path," is incomplete (",maxLength,")"))
    return(list())
  }
  splitGroupTableName <- lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x))))

  # Add modelPath and Scenario Description to Index row
  PathGroupTableName <- list()
  for ( i in 1:length(splitGroupTableName) ) {
    PathGroupTableName[[i]] <- c(
      splitGroupTableName[[i]],
      Description[i],
      Units[i],
      Module[i],
      visioneval::getRunParameter("Scenario",Default="Unknown Scenario",Param_ls=private$RunParam_ls),
      self$path
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
  GroupTableName <- data.frame()
  GroupTableName <- do.call(rbind.data.frame, PathGroupTableName)
  colnames(GroupTableName) <- c("Group", "Table", "Name","Description", "Units","Module","Scenario","Path")

  # GroupTableName is now a data.frame with five columns
  # complete.cases blows away the rows that have any NA values
  # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each column)
  ccases <- stats::complete.cases(GroupTableName)
  GroupTableName <- GroupTableName[ccases,]
  Index <- rbind(Index,GroupTableName)

  private$modelIndex <- Index
  private$modelInputs <- Inputs
  invisible(list(Index=private$modelIndex,Inputs=private$modelInputs))
}

ve.results.select <- function( what, details=FALSE ) {
  # interactive utility to select groups, tables or fields
  # 'what' can be "groups","tables" or "fields" (either as strings or names without quotes)
  # 'details' = FALSE (default) will present just the item name
  # 'details' = TRUE will present all items details
  # Interactive dialog will pre-select whatever is already selected (everything if
  #   nothing has been selected yet (either by assignment or earlier
  #   invocation of ve.results.select)
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
  selected <- utils::select.list(choices=select.from$choices,preselect=select.from$selected,multiple=TRUE,
    title=paste("Select",paste(toupper(substring(what,1,1)),substring(what,2),sep=""),sep=" "))
  self[[what]] <- select.from$names[ select.from$choices %in% selected ] # character(0) if none selected => selects all
  invisible(self[[what]]) # print result to see what actually got selected.
}

ve.results.groups <- function(groups) {
  if ( ! self$valid() ) stop("Model has not been run yet.")

  idxGroups <- unique(private$modelIndex[,c("Group"),drop=FALSE])
  row.names(idxGroups) <- NULL
  if ( ! missing(groups) ) {
    years <- ( tolower(groups) %in% c("years","year") ) # magic shortcut
    if ( any(years) ) {
      # Expand literal "Years" into all the year-like groups (name is exactly 4 digits)
      groups <- c( groups[!years], grep("^[[:digit:]]{4}$",idxGroups$Group,value=TRUE) )
    }
    if ( is.character(groups) && length(groups)>0 ) {
      private$groupsSelected <- groups[ groups %in% idxGroups$Group ]
    } else {
      private$groupsSelected <- character(0)
    }
  }
  if ( length(private$groupsSelected)==0 ) {
    idxGroups$Selected <- "Yes"
  } else {
    idxGroups$Selected <- ifelse(idxGroups$Group %in% private$groupsSelected,"Yes","No")
  }
  return(idxGroups)
}

ve.group.selected <- function(test.group,groups) {
  return( test.group %in% groups$Group[groups$Selected=="Yes"] )
}

ve.results.tables <- function(tables) {
  if ( ! self$valid() ) stop("Model has not been run yet.")

  idxTables <- unique(private$modelIndex[,c("Group","Table")])
  row.names(idxTables) <- NULL
  if ( ! missing(tables) ) {
    if ( is.character(tables) && length(tables)>0 ) {
      private$tablesSelected <- tables[ tables %in% idxTables$Table ]
    } else {
      private$tablesSelected <- character(0)
    }
  }
  group.selected <- ve.group.selected(idxTables$Group,self$groups)
  if ( length(private$tablesSelected)==0 ) {
    idxTables$Selected <- ifelse( group.selected, "Yes", "No (!Group)" )
  } else {
    idxTables$Selected <- ifelse(
      idxTables$Table %in% private$tablesSelected,
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

ve.results.fields <- function(fields) {
  # extract fields from the index where groups and tables are selected
  if ( ! self$valid() ) stop("Model has not been run yet.")

  idxFields <- private$modelIndex[,c("Group","Table","Name")]
  row.names(idxFields) <- NULL
  if ( ! missing(fields) ) {
    if ( is.character(fields) && length(fields)>0 ) {
      private$fieldsSelected <- fields[ fields %in% idxFields$Name ]
    } else {
      private$fieldsSelected <- character(0)
    }
  }
  table.selected <- ve.table.selected(idxFields$Table,self$tables)
  group.selected <- ve.group.selected(idxFields$Group,self$groups)
  tg.selected <- table.selected & group.selected
  if ( length(private$fieldsSelected)==0 ) {
    idxFields$Selected <- ifelse( tg.selected, "Yes", "No (!Table)" )
  } else {
    idxFields$Selected <- ifelse(
      idxFields$Name %in% private$fieldsSelected,
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

ve.results.list <- function(pattern="", details=FALSE, selected=TRUE, ...) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame private$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from private$modelIndex
  
  if ( ! self$valid() ) stop("Model has not been run yet.")

  filter <- if ( missing(selected) || selected ) {
    self$fields$Selected=="Yes"
  } else {
    rep(TRUE,nrow(private$modelIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,private$modelIndex$Name,ignore.case=TRUE )
  }
  if ( missing(details) || ! details ) {
    ret.fields <- c("Name")
  } else {
    ret.fields <- names(private$modelIndex)
  }
  ret.value <- private$modelIndex[ filter, ret.fields, drop=TRUE ]
  if ( class(ret.value)!='character' ) ret.value <- ret.value[order(ret.value$Group, ret.value$Name),]
  return(unique(ret.value))
}

ve.results.inputs <- function( fields=FALSE, module="", filename="" ) {
  if ( ! self$valid() ) stop("Model has not been run yet.")

  if ( ! missing(fields) && fields ) {
    ret.fields <- c("File","Name","Description","Units","Module","Scenario","Path")
  } else {
    ret.fields <- c("Module","File","Sceneario","Path")
  }

  filter <- rep(TRUE,nrow(private$modelInputs))
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,private$modelInputs$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,private$modelInputs$File)
  }

  ret.value <- unique(private$modelInputs[ filter, ret.fields ])
  return( ret.value[order(ret.value$Scenario,ret.value$File),] )
}

ve.results.units <- function() {
  NULL
}

# Build data.frames based on selected groups, tables and dataset names
ve.results.extract <- function(
  saveTo=visioneval::getRunParameter("OutputDir",Param_ls=private$RunParam_ls),
  overwrite=FALSE,
  quiet=FALSE
) {
  if ( ! self$valid() ) stop("Model State contains no results.")

  saving <- is.character(saveTo) && nzchar(saveTo)[1]

  ms <- private$ModelState
  
  visioneval::assignDatastoreFunctions(ms$DatastoreType)
  fields <- ( self$fields )

  extract <- fields[ ( fields$Selected=="Yes" ) ,c("Name","Table","Group")]

  tables <- split( extract$Name, list(extract$Table,extract$Group) )
  tables <- tables[which(sapply(tables,length)!=0)]
  DataSpecs <- lapply( names(tables), function(T.G.S) {
        TGS <- unlist(strsplit(T.G.S,"\\."))
        mp <- self$path
        dstoreloc <- file.path(mp,ms$DatastoreName)
        df <- data.frame(
          Name  = tables[[T.G.S]],
          Table = TGS[1],
          Group = TGS[2],
          Loc   = dstoreloc
        )
        list(
          Data=df,
          File=paste0(paste(gsub("\\.","_",T.G.S),format(ms$LastChanged,"%Y-%m-%d_%H%M%S"),sep="_"),".csv")
        )
      }
    )
  model.env = visioneval::modelEnvironment()
  results <- lapply(DataSpecs, function(d) {
        if (!quiet && saving ) message("Extracting data for Table ",d$Data$Table[1]," in Group ",d$Data$Group[1])
        # Do this in a for-loop rather than faster "apply" to avoid dimension and class/type problems.
        ds.ext <- list()
        for ( fld in 1:nrow(d$Data) ) {
          dt <- d$Data[fld,]
          ds.ext[[dt$Name]] <- model.env$readFromTable(Name=dt$Name,Table=dt$Table,Group=dt$Group,DstoreLoc=dt$Loc,ReadAttr=FALSE)
        }
        return( data.frame(ds.ext) )
      }
    )
  files <- sapply(DataSpecs, function(x) x$File)
  names(results) <- files
  if ( saving ) {
    sapply(
      names(results),
      FUN=function(f) {
        data <- results[[f]]
        out.path <- file.path(self$path,saveTo)
        if ( ! dir.exists(out.path) ) dir.create(out.path,recursive=TRUE)
        fn <- file.path(out.path,f)
        utils::write.csv(data,file=fn)
        if ( ! exists("ve.runtime") ) ve.runtime <- getwd()
        if (!quiet) message("Write output file: ",gsub(get("ve.runtime"),"",fn))
      }
    )
  } else {
    names(results) <- sub("\\.[^.]*$","",names(results))
    if (!quiet) message("Returning extracted data as invisible list of data.frames\n(quiet=TRUE to suppress this message)")
  }
  invisible(results)
}

ve.results.print <- function() {
  # Update for output
  cat("VEResults object for these results:\n")
  print(basename(self$path))
  cat("Output is valid:",self$valid(),"\n")
}

# Here is the VEResults R6 class
# One of these is constructed by VEModel$output()

VEResults <- R6::R6Class(
  "VEResults",
  public = list(
    # public data
    Name = NULL,

    # methods
    initialize=ve.results.init,
    path=NULL,
    valid=ve.results.valid,          # has the model been run, etc.
    select=ve.results.select,
    extract=ve.results.extract,
    list=ve.results.list,
    search=ve.results.list,
    inputs=ve.results.inputs,
    print=ve.results.print,
    units=ve.results.units           # Set units on field list (modifies private$modelIndex)
  ),
  active = list(
    groups=ve.results.groups,
    tables=ve.results.tables,
    fields=ve.results.fields
  ),
  private = list(
    queryObject=NULL,               # object to manage queries for this output
    outputPath=NULL,                # root for extract
    modelInputs=NULL,
    modelIndex=NULL,
    ModelState=NULL,
    RunParam_ls=NULL,
    groupsSelected=character(0),
    tablesSelected=character(0),
    fieldsSelected=character(0),
    index=ve.results.index
  )
)

#' Open VisionEval results from a directory
#'
#' @description
#' `openResults` opens a directory containing VisionEval model run results and
#'    returns a VEObject instance that can be used to extract the results or
#'    to perform queries.
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#'
#' The path provided as a parameter needs to contain ModelState.Rda and Datastore, using the names
#'   for those elements in the VisionEval run parameters ModelStateFileName and DatastoreName
#'   respectively. Generally, it is most reliable to open an output using the model object returned
#'   by VEModel::openModel, since that will ensure that the same run environment is used to find the
#'   result files as when those results were created. The openResults file does not load any
#'   configurations.
#'
#' @param path A relative or absolute path to a directory (default is the working directory)
#'   in which VisionEval results can be found for a single model run, stage or scenario
#'   combination.
#' @return A VEResults object giving access to the VisionEval results in `path`
#' @export
openResults <- function(path=NULL) {
  if ( ! dir.exists(path) ) path <- getwd()
  return(VEResults$new(path))
}