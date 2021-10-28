#========================
#build-function-graphic.R
#========================

# [Jeremy Raw, 2020-06-23] The basic function of building and organizing the function documentation
# has been moved to build/scripts/build-framework-docs.R and the framework-docs build target for the
# installation system, and the products are added to a "function_docs" folder in the visioneval
# package, and built into PDFs in the installer.

# This script presents some cute techniques for visualizing the Framework function interconnections

# This script creates supplemental documentation files for the visioneval framework as javascript
# files that are used in a webpage visualization of the functions and their calling relationships.

#-------------------------------------------------------------------------------
#PROCESS FUNCTION DOCUMENTATION RD FILES TO CREATE LISTS OF REQUIRED INFORMATION
#-------------------------------------------------------------------------------

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

if ( ! suppressWarnings(requireNamespace("jsonlite",quietly=TRUE)) ) {
  install.packages("jsonlite", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}
if ( ! suppressWarnings(requireNamespace("Rd2md",quietly=TRUE)) ) {
  install.packages("Rd2md", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}
if ( ! suppressWarnings(requireNamespace("purrr",quietly=TRUE)) ) {
  install.packages("purrr", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}

message("========== BUILD VISUAL FRAMEWORK DOCS ==========")

# Using visioneval framework we just built to do work here.
.libPaths(c(ve.lib,.libPaths()))

# Set the boilperplate folder
ve.boilerplate <- file.path(ve.installer,"boilerplate")
ve.html <- file.path(ve.boilerplate,"visual_docs")

ve.framework <- pkgs.db[pkgs.framework,]$Package # Expecting one, but could be more
framework.rd <- file.path(ve.src,ve.framework,"man")
if ( ! dir.exists(framework.rd) ) {
  stop("Missing ",framework.rd,". Did you build the modules yet?")
}

ve.visual.docs <- file.path(ve.docs,ve.framework,"visual_docs","js")
if ( ! dir.exists(ve.visual.docs) ) {
  dir.create(ve.visual.docs, recursive=TRUE, showWarnings=FALSE )
}

# We created the "js" subdirectory, but we'll also be referring to the
# parent of that directory (for the .html file copy from boilerplate)
ve.visual.js <- ve.visual.docs
ve.visual.docs <- normalizePath(dirname(ve.visual.docs),winslash="/")

# Set up the html boilerplate file (for displaying the .js data)
ve.html.files <- file.path(ve.html,dir(ve.html,pattern="\\.html$"))
invisible(file.copy(ve.html.files,ve.visual.docs))

# Short-circuit if the contents of the built .js directory is not newer than "/man"
need.new.docs <- newerThan(framework.rd,ve.visual.js)

if ( need.new.docs ) {

  cat("Building Framework Visual Documentation into:\n",ve.visual.docs,"\n")

  #Make a vector of function documentation file paths
  #--------------------------------------------------
  DocFileNames_ <- dir(framework.rd)
  FuncNames_ <- gsub(".Rd", "", DocFileNames_)
  DocFilePaths_ <- file.path(framework.rd, dir(framework.rd))

  #Identify functions called by each function
  #------------------------------------------
  #Define function to search for function calls
  findFunctionCalls <- function(FuncName, FuncNames_) {
    FuncBody <-
      body(eval(parse(text = paste0("visioneval::", FuncName))))
    IsCalled_ <- sapply(FuncNames_, function(x) {
      Test1 <- length(grep(paste0(x, "\\("), FuncBody)) > 0
      Test2 <- length(grep(paste0(x, " \\("), FuncBody)) > 0
      Test1 | Test2
    })
    FuncNames_[IsCalled_]
  }
  #Iterate through functions and identify the functions that each function calls
  FunctionCalls_ls <- lapply(FuncNames_, function(x){
    findFunctionCalls(x, FuncNames_)
  })
  names(FunctionCalls_ls) <- FuncNames_

  #Iterate through documentation files, parse, find group, add to FunctionDocs_ls
  # NOTE: redundant with build/scripts/build-docs-framework.R - make FunctionDocs_ls
  # into an intermediate buildable so we don't have to do it again.
  #------------------------------------------------------------------------------
  #Initialize a functions documentation list to store documentation by function group
  FunctionDocs_ls <- list(
    user = list(),
    query = list(),
    developer = list(),
    control = list(),
    datastore = list()
  )
  #Define function to extract function group name from the function description
  getGroup <- function(ParsedRd_ls) {
    Description <- gsub("\n", "", ParsedRd_ls$description)
    GroupCheck_ <- c(
      user = length(grep("model user", Description)) != 0,
      query = length(grep("query user", Description)) != 0,
      developer = length(grep("module developer", Description)) != 0,
      control = length(grep("control", Description)) != 0,
      datastore = length(grep("datastore connection", Description)) != 0
    )
    names(GroupCheck_)[GroupCheck_]
  }
  #Iterate through documentation files
  for (DocFile in DocFilePaths_) {
    cat("Parsing file:",DocFile)
    ParsedRd_ls <- Rd2md::parseRd(tools::parse_Rd(DocFile))
    Group <- getGroup(ParsedRd_ls)
    if ( length(Group)==0 ) {
      Group <- "control"
      cat(" (default)","\n")
    } else cat("\n")
    ParsedRd_ls$group <- Group
    FunctionName <- ParsedRd_ls$name
    ParsedRd_ls$calls <- FunctionCalls_ls[[FunctionName]]
    FunctionDocs_ls[[Group]][[FunctionName]] <- ParsedRd_ls
    rm(ParsedRd_ls, Group, FunctionName)
  }
  rm(DocFile)

  #--------------------------------------------
  #CREATE WEB VISUALIZATION DOCUMENTATION FILES
  #--------------------------------------------

  #Make list with information needed for function visualization
  #------------------------------------------------------------
  #Convert datastore functions to generic function names
  VisDocs_ls <- FunctionDocs_ls
  FuncToKeep_ <- c(
    "initDatasetRD", "initDatastoreRD", "initTableRD", "listDatastoreRD",
    "readFromTableRD", "writeToTableRD"
  )
  VisDocs_ls$datastore <- FunctionDocs_ls$datastore[FuncToKeep_]
  #Define function to make visualization documentation list for a function
  makeVisList <- function(Doc_ls) {
    #Clean out quotes
    Doc_ls <- lapply(Doc_ls, function(x) {
      gsub("\\\"", "", gsub("\\n", "", x))
    })
    #Make JSON list
    Vis_ls <- list()
    Vis_ls$Name <- Doc_ls$name
    Vis_ls$Group <- Doc_ls$group
    Vis_ls$Description <- Doc_ls$description
    Vis_ls$Details <- Doc_ls$details
    Vis_ls$Group <- Doc_ls$group
    Vis_ls$Arguments <- data.frame(
      Name = names(Doc_ls$arguments),
      Description = Doc_ls$arguments
    )
    rownames(Vis_ls$Arguments) <- NULL
    Vis_ls$Return <- Doc_ls$value
    Vis_ls$Calls <- Doc_ls$calls
    #Return function visualization documentation list
    Vis_ls
  }
  #Process list in correct form with correct information
  Vis_ls <- lapply(purrr::flatten(VisDocs_ls), makeVisList)

  #Define function to clean brackets from JSON created by jsonlite toJson function
  #-------------------------------------------------------------------------------
  cleanJSON <- function(JsonStringToClean) {
    OutJson <- gsub("\\[", "", JsonStringToClean)
    gsub("\\]", "", OutJson)
  }

  #Define function to create nodes data js file that can be used by vis.js
  #-----------------------------------------------------------------------
  writeNodes <- function(Data_ls) {
    Names_ <- unname(unlist(lapply(Data_ls, function(x) x$Name)))
    Groups_ <- unname(unlist(lapply(Data_ls, function(x) x$Group)))
    Nodes_ls <- list()
    for (i in 1:length(Names_)) {
      Nodes_ls[[i]] <-
        list(id = i, label = Names_[i], group = Groups_[i])
    }
    OutJs_ <- character(length(Nodes_ls) + 2)
    OutJs_[1] <- "var nodes = new vis.DataSet(["
    for (i in 1:length(Nodes_ls)) {
      if (i == length(Nodes_ls)) {
        OutJs_[i + 1] <- cleanJSON(jsonlite::toJSON(Nodes_ls[[i]]))
      } else {
        OutJs_[i + 1] <- paste0(cleanJSON(jsonlite::toJSON(Nodes_ls[[i]])), ",")
      }
    }
    OutJs_[length(Nodes_ls) + 2] <- "]);"
    return(OutJs_)
  }

  #Define function to create edges data js file that can be used by vis.js
  #-----------------------------------------------------------------------
  writeEdges <- function(Data_ls) {
    Nodes_ <- unname(unlist(lapply(Data_ls, function(x) x$Name)))
    Edges_ls <- list()
    for (i in 1:length(Nodes_)) {
      Calls_ <- Data_ls[[i]]$Calls
      if (length(Calls_) == 0) next()
      for (j in 1:length(Calls_)) {
        Edges_ls[[length(Edges_ls) + 1]] <- 
          list(from = i, to = which(Nodes_ == Calls_[j]), arrows = "to")
      }
    }
    OutJs_ <- character(length(Edges_ls) + 2)
    OutJs_[1] <- "var edges = new vis.DataSet(["
    for (k in 1:length(Edges_ls)) {
      if (k == length(Edges_ls)) {
        OutJs_[k + 1] <- cleanJSON(jsonlite::toJSON(Edges_ls[[k]]))
      } else {
        OutJs_[k + 1] <- paste0(cleanJSON(jsonlite::toJSON(Edges_ls[[k]])), ",")
      }
    }
    OutJs_[length(Edges_ls) + 2] <- "]);"
    return(OutJs_)
  }

  #Define function to make an HTML tag text string
  #-----------------------------------------------
  makeHtmlItem <- 
    function(Tag, Text, Id = NULL) {
      if (is.null(Id)) {
        OpenTag <- 
          paste0("<", Tag, ">")
      } else {
        OpenTag <-
          paste0("<", Tag, " id=", Id, ">")
      }
      CloseTag <- paste0("</", Tag, ">")
      paste0(OpenTag, Text, CloseTag)
    }

  #Function to make an HTML list text string
  makeHtmlList <- 
    function(Type, Text_, Id = NULL) {
      if (is.null(Id)) {
        OpenTag <- 
          paste0("<", Type, ">")
      } else {
        OpenTag <-
          paste0("<", Type, " id=", Id, ">")
      }
      Html_ <- OpenTag
      for (i in 1:length(Text_)) {
        Html_ <-
          c(Html_, makeHtmlItem("li", Text_[i]))
      }
      Html_ <-
        c(Html_, paste0("</", Type, ">"))
      Html_
    }

  #Define function to make a js object that contains HTML strings which document function details
  #----------------------------------------------------------------------------------------------
  writeFunctionDetails <- function(Data_ls) {
    Names_ <- unname(unlist(lapply(Data_ls, function(x) x$Name)))
    Details_ls <- list() 
    for (i in 1:length(Names_)) {
      Args_ <- 
        apply(Data_ls[[i]]$Arguments, 1, function(x) paste0(x[1], ": ", x[2]))
      if (length(Args_) == 0) Args_ <- "None"
      Html_ <-
        c(
          makeHtmlItem("h2", paste0("Function: ", Names_[i])),
          makeHtmlItem("h3", "Description"),
          makeHtmlItem("p", Data_ls[[i]]$Description),
          makeHtmlItem("h3", "Details"),
          makeHtmlItem("p", Data_ls[[i]]$Details),
          makeHtmlItem("h3", "Arguments"),
          makeHtmlList("ul", Args_),
          makeHtmlItem("h3", "Return Value"),
          makeHtmlItem("p", Data_ls[[i]]$Return)
        )
      Details_ls[[i]] <- 
        list(name = Names_[i], details = paste(Html_, collapse = " "))
    }
    OutJs_ <- character(length(Details_ls) + 2)
    OutJs_[1] <- "var functionDetails = ["
    for (j in 1:length(Details_ls)) {
      if (j == length(Details_ls)) {
        OutJs_[j + 1] <- cleanJSON(jsonlite::toJSON(Details_ls[[j]]))
      } else {
        OutJs_[j + 1] <- paste0(cleanJSON(jsonlite::toJSON(Details_ls[[j]])), ",")
      }
    }
    OutJs_[length(Details_ls) + 2] <- "];"
    return(OutJs_)
  }

  #Read functions documentation file and produce nodes.js, edges.js, & details.js
  #------------------------------------------------------------------------------
  cat("Writing .js files for function relationships.\n")
  OutJs_ <- writeNodes(Vis_ls)
  writeLines(OutJs_, file.path(ve.visual.js,"nodes.js"))

  OutJs_ <- writeEdges(Vis_ls)
  writeLines(OutJs_, file.path(ve.visual.js,"edges.js"))

  OutJs_ <- writeFunctionDetails(Vis_ls)
  writeLines(OutJs_, file.path(ve.visual.js,"details.js"))
} else {
  cat("Framework Visual Documentation is up to date.\n")
}
