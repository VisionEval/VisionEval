#!/bin/env Rscript

# Author: Jeremy Raw

# Script to install all required packages into ve.lib from within the local
# pkg-repository. This will install the packages necessary for the local runtime
# environment (source on Linux or MacOS; binaries on Windows, with an option to
# compile if Rtools is installed)

# We install these locally prior to the VE package build process (as a backstop
# to ensure that all required packages really are in the repository).

# NOTE: it is a bad idea to put the VE dependencies in your development
# environment since you will not easily be able to tell if you missed one

# Load runtime configuration
source(file.path(getwd(),"scripts/get-runtime-config.R"))

# uncomment the following line on Windows if you just want the pre-compiled
# binaries otherwise, if RTools is installed the newer sources packages will be
# compiled.  You should allow compilation to happen if there is discrepancy in
# behavior between a Windows installation and a source (e.g. Linux/Docker)
# installation
options(install.packages.compile.from.source="never")

# you will need miniCRAN and dependencies installed in your local R environment
if ( ! suppressWarnings(require(miniCRAN)) ) {
  install.packages("miniCRAN", lib=dev.lib, type=.Platform$pkgType )
}

cat("========================= BUILDING RUNTIME LIBRARY =========================\n")

cat("pkgs.db$Package:\n")
print(pkgs.db$Package)
cat("pkgs.CRAN:\n")
print(pkgs.CRAN)
cat("pkgs.BioC:\n")
print(pkgs.BioC)
cat("c(pkgs.db$Package[pkgs.CRAN], pkgs.db$Package[pkgs.BioC])\n")
print(c(pkgs.db$Package[pkgs.CRAN], pkgs.db$Package[pkgs.BioC]))

sought.pkgs <- miniCRAN::pkgDep(c(pkgs.db$Package[pkgs.CRAN], pkgs.db$Package[pkgs.BioC]),
                                repos=ve.deps.url, suggests=FALSE, type=ve.build.type)
pkgs.BaseR <- as.vector(installed.packages(lib.loc=.Library,
                                           priority=c("base", "recommended"))[,"Package"])

sought.pkgs <- setdiff(sought.pkgs, pkgs.BaseR)

new.pkgs <- sought.pkgs[ ! (sought.pkgs %in% installed.packages(lib.loc=ve.lib)[,"Package"]) ]

if( length(new.pkgs) > 0 ) {
  cat("---Still missing these packages:\n")
  print(sort(new.pkgs))
  print(ve.lib)
  cat("---Installing missing packages---\n")
  install.packages(
      new.pkgs,
      lib=ve.lib,
      repos=ve.deps.url,
      dependencies=c("Depends", "Imports", "LinkingTo"),
      type=ve.build.type
  )
  cat("---Finished installing---\n")
} else {
  cat("All dependencies accounted for in ve-lib (",ve.lib,")\n",sep="")
}
