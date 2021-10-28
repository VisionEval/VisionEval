#=======================
#build-docs-framework.R
#=======================

#This script assembles supplemental markdown files of documentation that can be added to the
#visioneval framework documentation files. The framework has a different version of this script that
#also builds a webpage visualization of the functions and their calling relationship.

#Other modules just make .pdf files from the .md files from inst/module_docs
#This script reassembles the .md files that visioneval generates in the same way
#but packages them into functional groups, with a heading derived from the top
#of the .R "module" (as with regular modules). Unlike the modules, we include
#.md versions of the .Rd standard help files, as well as the synopsis.

# Basic procedure:
#   0. Run this build function after build-modules
#   1. Appeal to the .Rd files that are in the /src directory
#   2. Look at the comment at the top to find the file they were defined in
#   3. Produce a markdown file from the .Rd file (in a sub-folder of src/visioneval/inst/framework_docs)
#       a. Put that sub-folder in .Rignore (along with any .Rproj files)
#       b. Build the documents here into 

#-------------------------------------------------------------------------------
#PROCESS FUNCTION DOCUMENTATION RD FILES TO CREATE LISTS OF REQUIRED INFORMATION
#-------------------------------------------------------------------------------

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

if ( ! suppressWarnings(requireNamespace("Rd2md",quietly=TRUE)) ) {
  install.packages("Rd2md", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}

message("========== BUILD FRAMEWORK DOCS ==========")

# Using visioneval framework we just built to do work here.
.libPaths(c(ve.lib,.libPaths()))

ve.framework <- pkgs.db[pkgs.framework,]$Package # Expecting one, but could be more
framework.rd <- file.path(ve.src,ve.framework,"man")
if ( ! dir.exists(framework.rd) ) {
  stop("Missing ",framework.rd,". Did you build the modules yet?")
}

ve.framework.docs <- file.path(ve.src,ve.framework,"inst","framework_docs")
cat("Building framework documentation into:\n",ve.framework.docs,"\n")
if ( ! dir.exists(ve.framework.docs) ) {
  dir.create(ve.framework.docs, recursive=TRUE, showWarnings=FALSE )
}

TempDir_ <- file.path(ve.framework.docs,"temp")
if ( ! dir.exists(TempDir_) ) {
  dir.create(TempDir_, recursive=TRUE, showWarnings=FALSE )
}

# Short-circuit if the contents of "/man" is not newer than "/inst/function_docs".
need.new.docs <- newerThan(framework.rd,ve.framework.docs)

if ( need.new.docs ) {

  cat("Building Framework Documentation into:\n",ve.framework.docs,"\n")

  #Make a vector of function documentation file paths
  #Look in the "src" directory for the documents (as built)
  #--------------------------------------------------
  DocFileNames_ <- dir(framework.rd)
  FuncNames_ <- gsub(".Rd", "", DocFileNames_)
  DocFilePaths_ <- file.path(framework.rd, dir(framework.rd))

  cat("Associating .Rd files with their source .R file\n")
  # Make a note of which VisionEval source file led to creation of which .Rd file
  # Warning: requires working RTools40 installation on Windows...
  # Should work on Mac or Linux using "system" instead of "shell"
  grep.paths <- paste(DocFilePaths_,collapse=" ")
  # t <- system(paste("bash -c \"echo grep -n \\'document in R/\\' ",grep.paths,"\""),intern=TRUE)

  # WARNING: the following will fail if the list of grep.paths gets uber-long
  file.docs <- system(paste("grep -n 'documentation in R/'",grep.paths),intern=TRUE)
  file.docs <- strsplit(file.docs,":\\d+:%.*R/")
  DocFileSources_ <- sapply(file.docs,function(x)x[2])
  names(DocFileSources_) <- sapply(file.docs,function(x)x[1])

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

  cat("Parsing .Rd files for Markdown conversion...\n")
  #Iterate through documentation files, parse, find group, add to FunctionDocs_ls
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
    print(GroupCheck_)
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
    ParsedRd_ls$source <- DocFileSources_[FunctionName]
    FunctionDocs_ls[[Group]][[FunctionName]] <- ParsedRd_ls
    rm(ParsedRd_ls, Group, FunctionName)
  }
  rm(DocFile)

  #--------------------------------------------
  #CREATE FUNCTION DOCUMENTATION MARKDOWN FILES
  #--------------------------------------------

  #Make lists of function names and paths to Rd files by group
  #-----------------------------------------------------------
  #Function names by group
  FuncNames_ls <- lapply(FunctionDocs_ls, function(x) {
    unlist(lapply(x, function(y) {
      y$name
    }))
  })
  #Rd file paths by group
  DocFilePaths_ls <- lapply(FunctionDocs_ls, function(x) {
    unlist(lapply(x, function(y) {
      paste0(framework.rd, "/", y$name, ".Rd")
    }))
  })

  #Function to compose markdown for a function
  #-------------------------------------------
  makeFunctionMarkdown <- function(RdFilePath, FunctionCalls_, RdFileSource) {
    #Convert function Rd file to markdown and save to temporary file
    outfile <- file.path(TempDir_,"temp.md")
    Rd2md::Rd2markdown( rdfile=RdFilePath, outfile=outfile )
    #Read the contents of the temporary file
    MdContents_ <- readLines(outfile)
    #Insert the file origin after the first row (which has the function name)
    MdContents_ <- c(
      MdContents_[1],
      paste0("(Function Source code is in **visioneval/R/",RdFileSource,"**)"),
      MdContents_[2:length(MdContents_)]
    )
    #Add 2 levels to each heading
    WhichHeadings_ <- grep("#", MdContents_)
    MdContents_[WhichHeadings_] <- paste0("##", MdContents_[WhichHeadings_])
    #Add function calls information
    if ( length(FunctionCalls_)>0 )
      c(MdContents_, "#### Calls", paste(FunctionCalls_, collapse = ", "), "", "")
    return(MdContents_)
  }

  #Write function documentation
  #----------------------------
  writeFunctionDocumentation <- function(doc.file,doc.title,doc.group)
  {
    cat("Writing",doc.title,"\n")
    cat("Into:",doc.file,"\n")
    Con <- file(doc.file, "w") # overwrite if there already
    writeLines(
      c(paste0("### ",doc.title), "", ""),
      Con
      )
    for (i in 1:length(DocFilePaths_ls[[doc.group]])) {
      func.name <- FuncNames_ls[[doc.group]][i]
      cat("Marking down function:",func.name,"in group",doc.group,"\n")
      Markdown_ <- makeFunctionMarkdown(
        DocFilePaths_ls[[doc.group]][i], 
        FunctionCalls_ls[[func.name]],
        DocFileSources_[func.name]
        )
      writeLines(Markdown_, Con)
    }
    close(Con)
  }

  writeFunctionDocumentation(
    file.path(ve.framework.docs,"User_Functions.md"),
    "VisionEval Model User Functions",
    "user"
  )
  writeFunctionDocumentation(
    file.path(ve.framework.docs,"Query_Functions.md"),
    "VisionEval Query Functions",
    "query"
  )
  writeFunctionDocumentation(
    file.path(ve.framework.docs,"Control_Functions.md"),
    "VisionEval Model Control Functions",
    "control"
  )
  writeFunctionDocumentation(
    file.path(ve.framework.docs,"Developer_Functions.md"),
    "VisionEval Model Developer Functions",
    "developer"
  )
  writeFunctionDocumentation(
    file.path(ve.framework.docs,"Datastore_Functions.md"),
    "VisionEval Model Datastore Functions",
    "datastore"
  )

} else {
  cat("Framework Documentation is up to date.\n")
}

# Clean up temporary directory
if ( exists("TempDir_") && dir.exists(TempDir_) ) unlink(TempDir_,recursive=TRUE)
