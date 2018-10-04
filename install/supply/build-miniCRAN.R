#!/bin/R
# This script will install R packages in a repository
# MiniCRAN: https://docs.microsoft.com/en-us/sql/advanced-analytics/r/create-a-local-package-repository-using-minicran?view=sql-server-2017
		
# Dependencies (need to keep this up to date: consider automating generation)
# CRAN Dependencies
pkgs.CRAN <- c(
"curl",
"data.table",
"devtools",
"digest",
"DT",
"envDocument",
"future",
"jsonlite",
"knitr",
"rhandsontable",
"roxygen2",
"shiny",
"shinyAce",
"shinyBS",
"shinyFiles",
"shinyjs",
"shinyTree",
"stringr",
"testit"
)

# BioConductor dependencies (consider replacing with CRAN h5)
pkgs.BioC <- c("rhdf5")

require(miniCRAN)

# Base R packages (so we can ignore those as dependencies)
pkgs.BaseR <- as.vector(installed.packages(priority=c("base","recommended"))[,"Package"])

# miniCRAN additions (we'll keep these, as well as miniCRAN itself)
pkgs.miniCRAN <- miniCRAN::pkgDep("miniCRAN",repos="https://cran.rstudio.org",suggests=FALSE)

pkgs.CRAN <- miniCRAN::pkgDep(pkgs.CRAN)
pkgs.all <- setdiff(unique(c(pkgs.CRAN,pkgs.miniCRAN)),pkgs.BaseR)

# Now move all the CRAN packages into miniCRAN
path.miniCRAN = "../built/miniCRAN"
dir.create(path.miniCRAN)
miniCRAN::makeRepo(pkgs.all,path=path.miniCRAN,repos="https://cran.rstudio.org",type=c("source","win.binary"))

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

# VEGUI depends (perhaps gratuitously) on namedCapture package, which is only available on Github
# Visit install/external, clone https://github.com/tdhock/namedCapture, then build it using buildNamedCapture.sh
# The script places the build packages (source and binary into the built folders for the local VE packages)

# Finally, add the local packages
pkgs.LocalDir <- "../built"
dir.src <- file.path(pkgs.LocalDir,"src")
dir.bin <- file.path(pkgs.LocalDir,"bin")
miniCRAN::addLocalPackage(dir(dir.src),pkgPath=dir.src,path=path.miniCRAN,type="source")
miniCRAN::addLocalPackage(dir(dir.bin),pkgPath=dir.bin,path=path.miniCRAN,type="win.binary")
