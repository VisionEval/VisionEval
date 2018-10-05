#!/bin/R
# This script will install R packages in a repository
# MiniCRAN: https://docs.microsoft.com/en-us/sql/advanced-analytics/r/create-a-local-package-repository-using-minicran?view=sql-server-2017
		
# Build the miniCRAN in a web-ready location
path.miniCRAN = "../www/R" # This directory already exists in the Github

# Pick the CRAN mirror to use for the packages
CRAN.mirror <- "https://cran.rstudio.org"

# Dependencies (need to keep this up to date: consider automating generation)
# CRAN Dependencies
pkgs.CRAN <- c(
	"curl"
	,"data.table"
	,"devtools"
	,"digest"
	,"DT"
	,"envDocument"
	,"future"
	,"jsonlite"
	,"knitr"
	,"rhandsontable"
	,"roxygen2"
	,"shiny"
	,"shinyAce"
	,"shinyBS"
	,"shinyFiles"
	,"shinyjs"
	,"shinyTree"
	,"stringr"
	,"testit"
# The following additional dependencies don't get identified by pkgDeps for some reason...
	,"filesstrings"
	,"reshape"
	,"car"
	,"pbkrtest"
	,"quantreg"
	,"geosphere"
	,"fields"
	,"pscl"
	,"ordinal"
	,"nanotime"
	,"Cairo"
	,"V8"
	,"R.utils"
	,"R.rsp"
	,"dplyr"
)

# BioConductor dependencies (consider replacing with CRAN h5)
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

require(miniCRAN)

# Base R packages (so we can ignore those as dependencies)
pkgs.BaseR <- as.vector(installed.packages(priority=c("base","recommended"))[,"Package"])

# miniCRAN additions (we'll keep these, as well as miniCRAN itself)
pkgs.miniCRAN <- miniCRAN::pkgDep("miniCRAN",repos=CRAN.mirror,suggests=FALSE)

pkgs.CRAN <- miniCRAN::pkgDep(pkgs.CRAN,repos=CRAN.mirror,suggests=FALSE)
pkgs.all <-  setdiff(unique(c(pkgs.CRAN,pkgs.miniCRAN)),pkgs.BaseR) # don't do redundant installations

pkgs.BioC <- miniCRAN::pkgDep(pkgs.BioC,repos=bioc)
pkgs.BioC <- setdiff( pkgs.BioC, pkgs.all ) # Possible risk here: don't double-install packages

# Now move all the CRAN and BioConductor packages into miniCRAN
# Need two rows because we're reaching to different repositories
miniCRAN::makeRepo(pkgs.all,path=path.miniCRAN,repos="https://cran.rstudio.org",type=c("source","win.binary"))
miniCRAN::addPackage(pkgs.BioC,path=path.miniCRAN,repos=bioc,type=c("source","win.binary"))

# VEGUI depends (perhaps gratuitously) on namedCapture package, which is only available on Github
# The external folder contains scripts to download and built that package.
# The script places the build packages (source and binary into the built folders for the local VE packages)

# Finally, add the local packages
pkgs.LocalDir <- "../built"
dir.src <- file.path(pkgs.LocalDir,"src")
dir.bin <- file.path(pkgs.LocalDir,"bin")
miniCRAN::addLocalPackage(dir(dir.src),pkgPath=dir.src,path=path.miniCRAN,type="source")
miniCRAN::addLocalPackage(dir(dir.bin),pkgPath=dir.bin,path=path.miniCRAN,type="win.binary")
