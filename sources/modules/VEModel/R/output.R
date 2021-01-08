# Output.R
self=private=NULL

# TODO: get rid of the "Stage" element of tables, fields, groups, etc.
# The output itself is bound to a specific model stage (usually the last one)
# Then check that the functionality is working.

# Output just wraps a ModelState and Datastore for one stage
# It maintains everything we need for a QueryPrep_ls structure for queries
# Plus it can export slices of the Datastore into .csv or data.frame
ve.init.output <- function(OutputPath) { # parameters yet to come - hook to model
  # OutputPath is the normalized path to a directory containing the model results
  #  typically from the last model stage. Expect to find a ModelState.Rda file
  #  and a Datastore in that folder.
  self$path <- OutputPath
  private$index()
  return(self$valid())
}

ve.output.valid <- function() {
  return(
    dir.exists(self$path) &&
    !is.null(private$modelIndex) && length(private$modelIndex)>0 &&
    !is.null(private$modelInputs) && length(private$modelInputs)>0
  )
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

# Simplify - we're only going to look at the results of one stage at a time.
ve.output.index <- function() {
  # Load model state from self$path
  # If no path, or no model state, or no Datastore, just abort
    
  # message("indexing model stages...")
  if ( no.output ) {
    message("Model does not appear to have been run yet.")
    return(list())
  }

  # message("Processing model stages...")
  Index <- data.frame()
  Inputs <- data.frame()

  ms <- private$ModelState
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

  # TODO: update to current results dir approach: the Output just examines one output path.
  ds <- (ms$Datastore)
  model.path <- file.path(basename(dirname(self$model$modelPath[stage])),basename(self$model$modelPath[stage]))

  # message("Processing ",basename(self$model$modelPath[stage]))
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
    warning("Model state ",self$model$modelPath[stage],"is incomplete (",maxLength,")")
    return(list())
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
  ccases <- stats::complete.cases(GroupTableName)
  GroupTableName <- GroupTableName[ccases,]
  # message("Length of complete.cases:",nrow(GroupTableName))
  Index <- rbind(Index,GroupTableName)
  # message("length of Index:",nrow(Index))

  # message("Attaching ve.inputs attribute to Index")
  private$modelIndex <- Index
  private$modelInputs <- Inputs
  invisible(list(Index=private$modelIndex,Inputs=private$modelInputs))
}

# TODO: change this so the fields are always sought within the
# Selected groups and tables (so with no tables selected, we'll
# get all group/table/field combinations for the selecte group).
ve.output.select <- function( what, details=FALSE ) {
  # interactive utility to select groups, tables or fields
  # 'what' can be "groups","tables" or "fields" (either as strings or names without quotes)
  # 'details' = FALSE (default) will present just the item name
  # 'details' = TRUE will present all items details
  # Interactive dialog will pre-select whatever is already selected (everything if
  #   nothing has been selected yet (either by assignment or earlier
  #   invocation of ve.output.select)
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

ve.output.groups <- function(groups) {
  if ( ! all(file.exists(file.path(self$model$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxGroups <- unique(private$modelIndex[,c("Group","Stage")])
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

ve.output.tables <- function(tables) {
  if ( ! all(file.exists(file.path(self$model$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxTables <- unique(private$modelIndex[,c("Group","Table","Stage")])
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

ve.output.fields <- function(fields) {
  # extract fields from the index where groups and tables are selected
  if ( ! all(file.exists(file.path(self$model$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxFields <- private$modelIndex[,c("Group","Table","Name","Stage")]
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

ve.output.list <- function(selected=TRUE, pattern="", details=FALSE) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame private$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from private$modelIndex
  if ( ! all(file.exists(file.path(self$model$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  filter <- if ( missing(selected) || selected ) {
    private$fields$Selected=="Yes"
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
  if ( class(ret.value)!='character' ) ret.value <- ret.value[order(ret.value$Stage, ret.value$Group, ret.value$Name),]
  return(unique(ret.value))
}

ve.output.inputs <- function( fields=FALSE, module="", filename="" ) {
  if ( ! all(file.exists(file.path(self$model$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  if ( ! missing(fields) && fields ) {
    ret.fields <- c("File","Name","Description","Units","Module","Stage","Path")
  } else {
    ret.fields <- c("Module","File","Stage","Path")
  }

  filter <- rep(TRUE,nrow(private$modelInputs))
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,private$modelInputs$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,private$modelInputs$File)
  }

  ret.value <- unique(private$modelInputs[ filter, ret.fields ])
  return( ret.value[order(ret.value$Stage,ret.value$File),] )
}

ve.output.units <- function() {
  NULL
}

# Build data.frames based on selected groups, tables and dataset names
ve.output.extract <- function(
  saveTo="output",
  overwrite=FALSE,
  quiet=FALSE
) {
  if ( ! all(file.exists(file.path(self$model$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  saving <- is.character(saveTo) && nzchar(saveTo)[1]
  
  visioneval::assignDatastoreFunctions(private$ModelState$DatastoreType)
  fields <- ( self$fields )

  extract <- fields[ ( fields$Selected=="Yes" & fields$Stage==stage ) ,c("Name","Table","Group","Stage")]

  tables <- split( extract$Name, list(extract$Table,extract$Group,extract$Stage) )
  tables <- tables[which(sapply(tables,length)!=0)]
  DataSpecs <- lapply( names(tables), function(T.G.S) {
        TGS <- unlist(strsplit(T.G.S,"\\."))
        stage <- as.integer(TGS[3])
        mp <- self$model$modelPath[stage]
        ms <- private$ModelState[[stage]]
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
  model.env = visioneval::modelEnvironment()
  results <- lapply(DataSpecs, function(d) {
        if (!quiet && saving ) message("Extracting data for Table ",d$Data$Table[1]," in Group ",d$Data$Group[1])
        # Do this in a for-loop rather than faster "apply" to avoid dimension and class/type problems.
        # TODO: make sure this works for earlier stages where not all fields will be defined...
        ds.ext <- list()
        for ( fld in 1:nrow(d$Data) ) {
          dt <- d$Data[fld,]
          ds.ext[[dt$Name]] <- model.env$readFromTable(Name=dt$Name,Table=dt$Table,Group=dt$Group,DstoreLoc=dt$Loc,ReadAttr=FALSE)
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
        out.path <- file.path(self$model$modelPath[s],saveTo)
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

ve.output.print <- function() {
  # Update for output
  cat("VEOutput object:\n")
  print(basename(self$model$modelPath))
}

ve.output.query <- function(...) {
  if ( is.null(private$queryObject) ) {
    query <- VEQuery$new(...) # parameters TBD
    if ( query$check() ) {
      private$queryObject <- query
    } else {
      private$queryObject <- NULL
    }
  }
  return(private$queryObject)
}

# Here is the VEOutput R6 class
# One of these is constructed by VEModel$output()

VEOutput <- R6::R6Class(
  "VEOutput",
  public = list(
    initialize=ve.init.output,
    path=NULL,                      # Back-reference to the VEModel for this output
    valid=ve.output.valid,          # has the model been run, etc.
    select=ve.output.select,
    extract=ve.output.extract,
    list=ve.output.list,
    search=ve.output.list,
    inputs=ve.output.inputs,
    print=ve.output.print,
    units=ve.output.units,          # Set units on field list (modifies private$modelIndex)
    query=ve.output.query           # Create a VEQuery object from named query file, or menu
  ),
  active = list(
    groups=ve.output.groups,
    tables=ve.output.tables,
    fields=ve.output.fields
  ),
  private = list(
    queryObject=NULL,               # object to manage queries for this output
    outputPath=NULL,                # root for extract
    modelInputs=NULL,
    modelIndex=NULL,
    ModelState=NULL,
    groupsSelected=character(0),
    tablesSelected=character(0),
    fieldsSelected=character(0),
    index=ve.output.index
  )
)
