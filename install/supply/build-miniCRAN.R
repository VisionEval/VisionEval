# This script will install R packages in a repository
# MiniCRAN: https://docs.microsoft.com/en-us/sql/advanced-analytics/r/create-a-local-package-repository-using-minicran?view=sql-server-2017
# Drat: http://dirk.eddelbuettel.com/code/drat.html
# Packrat/RStudio: https://rstudio.github.io/packrat/custom-repos.html
# Script requirements:
#	1. Build a local repository with all dependencies
#	2. Make it available for installation (can GitHub serve up pre-packages .zip's? - use GitHub's "release" function)
#	3. Allow the installation to use an online R repository (hosted, e.g., at visioneval.jeremyraw.com)
#		a. Use this script to create the repository, then move it to the server
#	4. Build an end-user script in R to manage the following:
#		a. Location of "repository" (local folder, or internet URL - default to internet, but point at downloaded .zip file)
#		b. Location of VisionEval installation folder (create if necessary)
#		c. Firing up VisionEval (e.g. within R, using a visioneval() function)
#	5. Package all of that up in a single end-user package, which has one function "visioneval()" that:
#		a. Checks for installed visioneval in the current working directory (skip to "c" if already present)
#			i. e.g. present user with "visioneval" as a subdirectory of the current one
#			ii. if no directory found, or it doesn't contain VisionEval, offer to install
#		b. Offers to install:
#			i. From Github release (large binary)
#			ii. From local file
#			iii. From VisionEval repository (e.g. at visioneval.jeremyraw.com)
#			iv. Or search for installed visioneval on current computer
#		c. Launches a startup page from the visioneval subdirectory
		
# How about this:
#   install_github builds the visioneval package (so kind of a hybrid approach with WSP/RSG)
#   then you get the visioneval package in place and it has a visioneval() function that retrieves the rest.
#   You can use that function to install from github (using install.R), from visioneval online repo, or from local miniCRAN repository
#   We could also add in a build function that will (a) retrieve missing stuff from github; (b) build everything

require(miniCRAN)

# Base R packages (so we can ignore those as dependencies)
pkgs.BaseR <- as.vector(installed.packages(priority=c("base","recommended"))[,"Package"])

# miniCRAN additions (we'll keep these, as well as miniCRAN itself)
pkgs.miniCRAN <- miniCRAN::pkgDep("miniCRAN",repos="https://cran.rstudio.org",suggests=FALSE)

# CRAN Dependencies
pkgs.CRAN <- c(
"curl",
"devtools",
"roxygen2",
"stringr",
"knitr",
"digest",
"shiny",
"shinyjs",
"shinyFiles",
"data.table",
"DT",
"shinyBS",
"future",
"testit",
"jsonlite",
"shinyAce",
"envDocument",
"rhandsontable",
"shinyTree"
)
pkgs.CRAN <- miniCRAN::pkgDep(pkgs.CRAN)
pkgs.all <- setdiff(unique(c(pkgs.CRAN,pkgs.miniCRAN)),pkgs.BaseR)

# Possibly the easiest way - just grab the repository and build the package,
# then place the built files directly in the "built" folders
# Possibly a script to download the repository and build the package...
# It is only used in VEGUI to report status messages - has to be an easier way

# Now move all the CRAN packages into miniCRAN
path.miniCRAN = "../built/miniCRAN"
dir.create(path.miniCRAN)
miniCRAN::makeRepo(pkgs.all,path=path.miniCRAN,repos="https://cran.rstudio.org",type=c("source","win.binary"))

# Finally, add the BioConductor packages to miniCRAN (this will pull a few additional dependencies from CRAN itself)
pkgs.BioC <- c("rhdf5")

# Get the BioConductor magic that manages their repositories
# They have their own installation program to manage dependencies across repositories
# The package descriptions say where to look, and miniCRAN knows about that...
bioc <- local({
  env <- new.env()
  on.exit(rm(env))
  evalq(source("http://bioconductor.org/biocLite.R", local = TRUE), env)
  biocinstallRepos()
})
miniCRAN::addPackage(pkgs.BioC,path=path.miniCRAN,repos=bioc,type=c("source","win.binary"))

# Finally, add the local packages
pkgs.LocalDir <- "../built"
dir.src <- file.path(pkgs.LocalDir,"src")
dir.bin <- file.path(pkgs.LocalDir,"bin")
miniCRAN::addLocalPackage(dir(dir.src),pkgPath=dir.src,path=path.miniCRAN,type="source")
miniCRAN::addLocalPackage(dir(dir.bin),pkgPath=dir.bin,path=path.miniCRAN,type="win.binary")
