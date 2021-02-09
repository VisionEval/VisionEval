# Results.R
#' @include environment.R
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
  self$selection <- VESelection$new(self)
  return(self$valid())
}

ve.results.valid <- function() {
  valid <- ! is.null(private$RunParam_ls) &&
           dir.exists(self$path) &&
           !is.null(self$modelIndex) && length(self$modelIndex)>0
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
  FileName=normalizePath( file.path(
    self$path, # Should already include ResultsDir
    visioneval::getModelStateFileName(Param_ls=private$RunParam_ls)
  ), winslash="/", mustWork=FALSE)
  ms <- self$ModelState <- try(visioneval::readModelState(FileName=FileName))
  if ( ! is.list(self$ModelState) ) {
    self$ModelState <- NULL
    visioneval::writeLog(Level="error",paste("Cannot load ModelState from:",FileName))
    return(list())
  }
  if ( is.null(private$RunParam_ls) && is.list(self$ModelState ) ) {
    private$RunParam_ls <-self$ModelState$RunParam_ls
  }
  owd <- setwd(self$path)
  on.exit(setwd(owd))

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
  InputDir <- sapply(ds$attributes, attributeGet, "INPUTDIR",simplify=TRUE) # should yield a character vector
  scenario <- visioneval::getRunParameter("Scenario",Default="Unknown Scenario",Param_ls=private$RunParam_ls)

  # TODO: do we want to keep the inputs in the same table as the other fields?
  # In the specifications, inputs create columns in the datastore.
  
  splitGroupTableName <- strsplit(ds$groupname, "/")
  if ( length(Description) != length(splitGroupTableName) ) stop("Inconsistent table<->description correspondence")

  maxLength <- max(unlist(lapply(splitGroupTableName, length)))
  if ( maxLength != 3 ) {
    visioneval::writeLog(Level="warn",paste0("Model state at ",self$path," is incomplete (",maxLength,")"))
    return(list())
  }
  splitGroupTableName <- lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x))))
  # splitGroupTableName is a list of character vectors with Group, Table, Name components

  inputGTN <- do.call(rbind.data.frame,splitGroupTableName)[InputIndex,]
  names(inputGTN) <- c("Group","Table","Name")

  # Build parallel data.frame for Inputs
  # message("Input data frame...")
  File <- sapply(ds$attributes, attributeGet, "FILE",simplify=TRUE) # should yield a character vector
  inputs <- data.frame(
    Group       = inputGTN$Group[InputIndex],
    Table       = inputGTN$Table[InputIndex],
    Name        = ds$name[InputIndex], # Should be identical to inputGTN$Name
    Description = Description[InputIndex],
    Units       = Units[InputIndex],
    Module      = Module[InputIndex],
    Scenario    = scenario,
    File        = File[InputIndex],
    InputDir    = InputDir[InputIndex]
  )
  Inputs <- rbind(Inputs,inputs)

  Description <- Description[!InputIndex]
  Module <- Module[!InputIndex]
  Units <- Units[!InputIndex]
  splitGroupTableName <- splitGroupTableName[!InputIndex]

  # Add modelPath and Scenario Description to Index row
  PathGroupTableName <- list()
  for ( i in 1:length(splitGroupTableName) ) {
    PathGroupTableName[[i]] <- c(
      splitGroupTableName[[i]],
      Description[i],
      Units[i],
      Module[i],
      scenario,
      "",         # File
      ""          # InputDir
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
  names(GroupTableName) <- c("Group", "Table", "Name","Description", "Units","Module","Scenario","File","InputDir")

  # GroupTableName is now a data.frame with five columns
  # complete.cases blows away the rows that have any NA values
  # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each column)
  ccases <- stats::complete.cases(GroupTableName)
  GroupTableName <- GroupTableName[ccases,]
  Index <- rbind(Index,GroupTableName)

  self$modelIndex <- rbind(Index,Inputs)
  invisible(self$modelIndex)
}

ve.results.list <- function(pattern="", details=FALSE, selected=TRUE, ...) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame self$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from self$modelIndex
  
  if ( ! self$valid() ) stop("Model has not been run yet.")

  filter <- if ( missing(selected) || selected ) {
    self$selection$index
  } else {
    rep(TRUE,nrow(self$modelIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$modelIndex$Name,ignore.case=TRUE )
  }

  # TODO: return Group/Table/Name format, not bare name
  if ( missing(details) || ! details ) {
    ret.fields <- c("Name")
  } else {
    ret.fields <- names(self$modelIndex)
  }
  ret.value <- self$modelIndex[ filter, ret.fields, drop=TRUE ]
  if ( class(ret.value)!='character' ) ret.value <- ret.value[order(ret.value$Group, ret.value$Name),]
  return(unique(ret.value))
}

ve.results.inputs <- function( fields=FALSE, module="", filename="" ) {
  # fields=TRUE, show all names of inputs
  # fields=FALSE, just show the module, file, input directory
  if ( ! self$valid() ) stop("Model has not been run yet.")

  if ( ! missing(fields) && fields ) {
    ret.fields <- c("Module","Group","Table","Name","File","InputDir","Units","Description")
  } else {
    ret.fields <- c("Module","Name","File","InputDir")
  }

  filter <- nzchar(self$modelIndex$File)
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,self$modelIndex$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,self$modelIndex$File)
  }

  ret.value <- unique(self$modelIndex[ filter, ret.fields ])
  return( ret.value[order(ret.value$InputDir,ret.value$File),] )
}

ve.results.units <- function() {
  # Not implemented yet
  NULL
}

ve.results.extract <- function(
  saveTo=visioneval::getRunParameter("OutputDir",Param_ls=private$RunParam_ls),
  overwrite=FALSE,
  quiet=FALSE,
  select=NULL # replaces self$selection if provided
) {
  if ( ! self$valid() ) stop("Model State contains no results.")
  if ( is.null(select) ) select <- self$selection else self$selection <- select
  if ( is.na(select$selection) || length(select$selection)<1 ) {
    stop("Nothing selected to extract.")
  }

  saving <- is.character(saveTo) && nzchar(saveTo)[1]

  ms <- self$ModelState
  
  visioneval::assignDatastoreFunctions(ms$DatastoreType)
  extract <- self$modelIndex[ select$selection, c("Name","Table","Group") ]

  tables <- split( extract$Name, list(extract$Table,extract$Group) )
  tables <- tables[which(sapply(tables,length)!=0)]
  DataSpecs <- lapply( names(tables), function(T.G.S) { # T.G.S : Table Group Split
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
  model.env = visioneval::modelEnvironment() # holds assigned datastore functions
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

# Update this selection, or just return what is already selected
ve.results.select <- function(select=integer(0)) {  # integer(0) says select all by default. Use NA or NULL to select none
  if ( ! is.null(select) ) {
    self$selection <- VESelection$new(self,select=select)
  }
  return(self$selection)
}

ve.results.queryprep <- function() {
  visioneval::prepareForDatastoreQuery(
    DstoreLocs_ = file.path(self$path,self$ModelState$DatastoreName),
    DstoreType  = self$ModelState$DatastoreType
  )
}

ve.results.print <- function(details=FALSE) {
  # Update for output
  cat("VEResults object for these results:\n")
  print(self$path)
  cat("Output is valid:",self$valid(),"\n")
  if ( ! details ) {
    cat("Selected",length(self$selection$selection),"out of",nrow(self$modelIndex),"fields.\n")
    print(self$selection)
  } else {
    print(self$selection,details=TRUE)
  }
}

# Here is the VEResults R6 class
# One of these is constructed by VEModel$output()

VEResults <- R6::R6Class(
  "VEResults",
  public = list(
    # public data
    Name = NULL,
    path=NULL,
    ModelState=NULL,
    modelIndex=NULL,
    selection=NULL,

    # methods
    initialize=ve.results.init,
    valid=ve.results.valid,          # has the model been run, etc.
    select=ve.results.select,
    extract=ve.results.extract,      # alias the extract/export function
    export=ve.results.extract,
    list=ve.results.list,
    queryprep=ve.results.queryprep,  # For query or other external access
    print=ve.results.print,
    units=ve.results.units           # TODO: Set units on field list (modifies self$modelIndex)
  ),
  private = list(
    queryObject=NULL,               # object to manage queries for this output
    outputPath=NULL,                # root for extract
    RunParam_ls=NULL,
    index=ve.results.index
  )
)

ve.select.initialize <- function( results, select=integer(0) ) {
  # default select (integer(0)) selects everything
  # self$selection is just a list of integers pointing to rows
  #  in self$results$modelIndex
  self$results <- results
  rows <- self$parse(select)
  if ( is.null(rows) || any(is.na(rows)) ) {
    self$selection <- as.integer(NA) # no rows selected
  } else if (
    ! is.numeric(rows) ||
    length(rows)==0 ||
    ! min(rows)>0 ||
    max(rows)>nrow(self$results$modelIndex) ) {
    self$selection <- 1:nrow(self$results$modelIndex)
  } else {
    self$selection <- rows
  }
}

ve.select.print <- function(details=FALSE) {
  # print the selected fields
  if ( ! details ) {
    print( self$fields() )
  } else {
    print( self$results$modelIndex[ self$selection, ] )
  }
}

ve.select.groups <- function() {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No groups selected")
    return(character(0))
  }
  idxGroups <- unique(self$results$modelIndex[self$selection,c("Group"),drop=FALSE])
  return(sort(idxGroups)) # Group
}

ve.select.tables <- function() {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No tables selected")
    return(character(0))
  }
  idxTables <- unique(self$results$modelIndex[self$selection,c("Group","Table")])
  return(sort(paste(idxTables$Group,idxTables$Table,sep="/"))) # Group/Table
}

ve.select.fields <- function() {
  # extract fields from the index where groups and tables are selected
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No fields selected")
    return(character(0))
  }
  idxFields <- self$results$modelIndex[self$selection,c("Group","Table","Name")]
  return(sort(paste(idxFields$Group,idxFields$Table,idxFields$Name,sep="/"))) # Group/Table/Name
}

ve.select.parse <- function(select) {
  # Though select can be a vector of field names, they need to be the full Group/Table/Name field names,
  #  so you should get them from ve.select.find, rather than manually construct them.
  # if select is NA, return NA
  # select can be another VESelection
  #   if it is the same model, just copy its selection
  #   if not the same model, get other selection's VEResults object and parse that
  if ( "VESelection" %in% class(select) ) {
    if ( select$results$path != self$results$path ) {
      select <- select$fields()
      # fall through to parse the character strings
    } else {
      return( select$selection )
    }
  }
  # select can be another VEResults object
  #   if the other VEResults is not from the same model, use its $fields set of names
  #   then parse as a character vector
  #   if it is the same model, just copy its selection
  if ( "VEResults" %in% class(select) ) {
    if ( select$path != self$results$path ) {
      select <- select$selection$fields()
    } else {
      return( select$selection$selection )
    }
  }
  # select can be a character vector
  #   split the vector into group/table/name, providing defaults
  # locate the rows with matching group/table/name in results$modelIndex
  #   That vector of row indices becomes the selection to act on
  if ( is.character(select) ) {
    build <- integer(0)
    for ( s in select ) {
      t <- unlist(strsplit(s,"/"))
      name <- c( rep(NA,3-length(t)), t )
      if ( is.na(name[3]) || ! nzchar(name[3]) ) next  else field=name[3]
      if ( is.na(name[2]) || ! nzchar(name[2]) ) table <- NULL else table=name[2]
      if ( is.na(name[1]) || ! nzchar(name[1]) ) group <- NULL else group=name[1]
      build <- union( build, self$find(Name=field,Group=group,Table=table,as.object=FALSE) )
    }
    select <- build; # should be a vector of integers
  }
  
  # if select is a numeric vector, validate its range and return it
  if ( is.numeric(select) ) {
    if ( length(select)>0 ) {
      if ( any(is.na(select)) ) return( as.integer(NA) )
      if ( ! ( min(select)>0 && max(select)<=nrow(self$results$modelIndex) ) ) {
        message("Field selection out of range")
        return( as.integer(NA) )
      }
    }
    return( select )
  }
  message("Invalid field selection specification")
  message(deparse(select))
  return( as.integer(NA) )
}

#' Assign new selection to VESelection
#'
#' @param x a VESelection object that will be added to
#' @param select an object to be made into a VESelection and then assigned to this one
#' @return the VESelection that was modified
#' @export
"<-.VESelection" <- function(select) self$select(select)

ve.select.select <- function(select) {
  if ( ! missing(select) ) self$selection <- self$parse(select)
  return(self)
}

ve.select.find <- function(pattern=NULL,Group=NULL,Table=NULL,Name=NULL,as.object=TRUE) {
  # if pattern (regexp) given, find names matching pattern (only within the "fields" part)
  # if group or table not specified, look in any group or table
  # return vector of indices for matching rows
  searchGroup <- Group
  searchTable <- Table
  searchName  <- Name
  newSelection <- self$selection
  newSelection <- with( self$results$modelIndex, {
    if ( !is.null(pattern ) ) {
      fld <- grepl(pattern,Name,ignore.case=TRUE)     # RegEx search for name
    } else if ( !is.null(searchName) ) {
      fld <- Name == searchName;                      # Exact name match
    } else {
      fld <- rep(TRUE,nrow(self$results$modelIndex))  # Start with all selected
    }
    if ( !is.null(searchGroup) ) {
      if ( searchGroup %in% c("Year","Years","AllYears") ) {  # shorthand for non-Global group
        group <- Group != "Global"
      } else {
        group <- Group == searchGroup
      }
      fld <- fld & group
    }
    if ( !is.null(searchTable) ) fld <- fld & Table==searchTable
    which(fld)
  })
  if ( length(newSelection) == 0 ) newSelection <- as.integer(NA)
  if ( as.object ) {
    return(VESelection$new(self$results, select=newSelection))
  } else {
    return(newSelection)
  }
}

ve.select.add <- function(select) {
  select <- self$parse(select)
  select <- union(self$selection,select)
  VESelection$new(self$results, select=select)
}

#' Add items to VESelection
#'
#' @param x a VESelection object that will be added to
#' @param y an object from which a new VESelection can be created; fields that it
#'   references will be added to x
#' @return parameter x, updated to also select items from y
#' @export
"+.VESelection" <- function(x,y) x$add(y)

ve.select.remove <- function(select) {
  select <- self$parse(select)
  VESelection$new(self$results, select=setdiff(self$selection,select))
}

#' Remove items from VESelection
#'
#' @param x a VESelection object from which selections will be removed
#' @param y an object from which a new VESelection can be created; fields that it
#'   references will be removed from x if they are already selected
#' @return parameter x, updated to also select items from y
#' @export
"-.VESelection" <- function(x,y) x$remove(y)

ve.select.and <- function(select) {
  select <- self$parse(select)
  VESelection$new(self$results, select=intersect(self$selection,select))
}

ve.select.all <- function() {
  select <- 1:nrow(self$results$modelIndex)
  VESelection$new(self$results, select=select)
}

ve.select.none <- function() {
  select <- integer(NA)
  VESelection$new(self$results, select=select)
}

# Build data.frames based on selected groups, tables and dataset names
ve.select.extract <- function(
  saveTo=visioneval::getRunParameter("OutputDir",Param_ls=private$RunParam_ls),
  overwrite=FALSE,
  quiet=FALSE
) {
  # Delegates to the result object, setting its selection in the process
  return( self$results$extract(saveTo,overwrite,quiet,select=self) )
}

#' Conversion method to turn a VESelecton into a vector of selection indices
#'
#' @param select a VESelection object (or something that can be coerced to one)
#' @return an integer vector of selected fields
#' @export
as.integer.VESelection <- function(select) select$selection

# The VESelection R6 class
# This interoperates with VEResult to keep track of what to print
VESelection <- R6::R6Class(
  "VESelection",
  public = list(
    # public data
    selection = integer(0),
    results = NULL,

    # methods
    initialize=ve.select.initialize,
    print=ve.select.print,
    extract=ve.select.extract,
    export=ve.select.extract,
    find=ve.select.find,
    parse=ve.select.parse,
    select=ve.select.select,      # assign: Also implements as "<-" operator
    add=ve.select.add,            # "union": Also implements as "+" operator
    remove=ve.select.remove,      # "setdiff": Also implements as "-" operator
    and=ve.select.and,            # "intersection" operation
    or=ve.select.add,             # Just does a "union" operation
    all=ve.select.all,
    none=ve.select.none,

    # Field lists (read-only)
    groups=ve.select.groups,
    tables=ve.select.tables,
    fields=ve.select.fields
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
