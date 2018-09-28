# You should run "build-packages.R" first to make the local packages available

# Locate repositories - warning: must coincicde with build-packages
# TODO: Consider creating a VE-devtools package with all these things as functions
# TODO: Dependencies for VE-devtools include devtools, miniCRAN
# TODO: Put devtools, miniCRAN plus dependencies in the miniCRAN!

# Construct folder for repository
if (!exists("ve.root") || !file.exists(ve.root) )
	ve.root <- choose.dir(getwd(),caption="Locate Repository Root Directory")
if (!exists("ve.root") || is.na(ve.root) || !file.exists(ve.root) ) # NA generated if user cancels choose.dir
	stop("Can't build packages without knowing the repository")

# Where to put the built results (these are not version controlled, so create if missing)
dir.create( built.path <- file.path(ve.root,"install","built","miniCRAN"), showWarnings=FALSE, recursive=TRUE )

# TODO: how to add a Github package like namedCapture to the miniCRAN
addGithubPackage <- function(githubPath,...){
    packageName <- basename(githubPath)
    exDir <- file.path(tempdir(),packageName)
    if(file.exists(exDir)) unlink(exDir, recursive=TRUE)
    zipFile <- file.path(tempdir(),paste0(packageName,".zip"))
    download.file(paste0(githubPath,"/","zipball/master"),zipFile)
    unzip(zipFile, exdir = exDir)
    file.rename(list.files(exDir,full.names=TRUE)[1],file.path(exDir,packageName))
    addLocalPackage(packageName,exDir,...)
}
addGithubPackage(githubPath="https://github.com/js229/Vennerable/",path=reposPath,build=TRUE)

cat("\nInstalling dependencies\n")
install.packages(c("curl","devtools", "roxygen2", "stringr", "knitr", "digest"), dependencies = TRUE, quiet=TRUE)
install.packages(c("shiny", "shinyjs", "shinyFiles", "data.table", "DT", "shinyBS", "future", "testit", "jsonlite", "shinyAce", "envDocument", "rhandsontable","shinyTree"), dependencies = TRUE, quiet=TRUE)
devtools::install_github("tdhock/namedCapture", quiet=TRUE)
source("https://bioconductor.org/biocLite.R")
biocLite(c("rhdf5","zlibbioc"), suppressUpdates=TRUE, quiet=TRUE)

