#!/bin/env Rscript

# Author: Jeremy Raw

# This script downloads required R packages from CRAN and BioConductor into the
# local pkg-repository. It will only download formats required to build a runtime
# binary on the current architecture (the one on which the build is running)

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

# uncomment the following line on Windows if you just want the pre-compiled
# binaries otherwise, if RTools is installed the newer sources packages will be
# compiled.  You should allow compilation to happen if there is discrepancy in
# behavior between a Windows installation and a source (e.g. Linux/Docker)
# installation
options(install.packages.compile.from.source="never")

# Load required libraries
if ( ! suppressWarnings(require("miniCRAN",quietly=TRUE)) ) {
  install.packages("miniCRAN", lib=dev.lib, dependencies=NA, type=.Platform$pkgType)
}

require(tools)

message("========== GATHER DEPENDENCIES (CRAN / BioConductor) ==========")

# BioConductor setup
# if ( ! require("BiocManager") ) {
#     install.packages("BiocManager", repos=CRAN.mirror)
# }
# bioc <- BiocManager::repositories()
bioc <- BioC.mirror

if ( ! exists("pkgs.CRAN") || ! exists("pkgs.BioC") || ! exists("pkgs.db") ) {
  stop("Please run build-config.R to build dependency lists")
}

# Base R packages (so we can ignore those as dependencies)
base.lib <- dirname(find.package("MASS")) # looking for recommended packages; picking one that is required
pkgs.BaseR <- as.vector(installed.packages(lib.loc=base.lib, priority=c("base", "recommended"))[,"Package"])

# This script will only generate the packages needed for a binary installation in the
# current architecture.

# A couple of helper functions
# havePackages: check for presence of basic repository structure
# findMissingPackages: list packages not present in a particular sub-repository

havePackages <- function() {
  # Determine if pkg-repository is well-formed
  #
  # Returns:
  #   TRUE/FALSE depending on existence of pkg-repository file tree
  #
  # If the tree is there, don't need to build the miniCRAN from scratch
  bin.contrib <- contrib.url(ve.dependencies, type=ve.build.type)
  got.bin <- FALSE
  if ( dir.exists(bin.contrib) ) {
    if ( ! file.exists(file.path(bin.contrib, "PACKAGES")) ) {
      cat("Updating VE repository ",ve.build.type," PACKAGES files\n")
      got.bin <- (write_PACKAGES(bin.contrib, type=ve.build.type)>0)
    } else {
      got.bin <- TRUE
    }
  }
  return( got.bin )
}

findMissingPackages <- function( required.packages ) {
  # Determine if any packages are missing from the pkg-repository
  # compared to the required.packages passed in.
  #
  # Args:
  #   required.packages: a character vector containing names of packages
  #                      we hope to find in pkg-repository
  #
  # Returns:

  #   A character vector of package names that are missing from the
  #   ve.built.type section of the pkg-repository compared to the
  #   required.packages
  
  apb <- available.packages(repos=ve.deps.url, type=ve.build.type)
  return( setdiff( required.packages, apb[,"Package"]) )
}

# cat("Computing dependencies.\n")
pkgs.CRAN.lst <- pkgs.db$Package[pkgs.CRAN]
pkgs.CRAN.lst <- setdiff(pkgs.CRAN.lst, pkgs.BaseR) # don't search for base package dependencies
pkgs.CRAN.all <- pkgs.CRAN.lst <- miniCRAN::pkgDep( pkgs.CRAN.lst, repos=CRAN.mirror, suggests=FALSE)
pkgs.CRAN.lst <- setdiff(pkgs.CRAN.lst, pkgs.BaseR) # don't keep base packages
# cat("pkgs.CRAN.all\n")
# print(sort(pkgs.CRAN.all))

pkgs.BioC.lst <- pkgs.db$Package[pkgs.BioC]
pkgs.BioC.all <- pkgs.BioC.lst <- miniCRAN::pkgDep( pkgs.BioC.lst, repos=bioc, suggests=FALSE)
pkgs.BioC.lst <- setdiff( pkgs.BioC.lst, pkgs.CRAN.lst ) # Possible risk here: don't double-install packages
# cat("pkgs.BioC.all\n")
# print(sort(pkgs.BioC.all))

# cat("Dependencies:\n")
stated.dependencies <- as.character(c(pkgs.CRAN.lst, pkgs.BioC.lst))
all.dependencies <- setdiff(as.character(c(pkgs.CRAN.all, pkgs.BioC.all)),pkgs.BaseR)
save(stated.dependencies, all.dependencies, file=ve.all.dependencies)

cat("Repository location:",ve.dependencies,"\n")
# Attempt a minimal build of the repository (adding just new packages if we already have the whole thing)
# We won't attempt to delete - cleanup just by rebuilding when cruft gets to be too much.
if ( havePackages() ) {
  pkgs.missing.CRAN <- findMissingPackages(pkgs.CRAN.lst)
  up.to.date = TRUE
  if ( any(sapply(pkgs.missing.CRAN, length)) > 0 ) {
    if ( length(pkgs.missing.CRAN ) > 0 ) {
      up.to.date = FALSE
      cat("Updating VE dependency repository to add from CRAN:\n")
      print(pkgs.missing.CRAN)
      miniCRAN::addPackage(pkgs.missing.CRAN, path=ve.dependencies, repos=CRAN.mirror, type=ve.build.type, deps=FALSE)
    }
  }
  pkgs.missing.BioC <- findMissingPackages(pkgs.BioC.lst)
  if ( any(sapply(pkgs.missing.BioC, length)) > 0 ) {
    if ( length(pkgs.missing.BioC) > 0 ) {
      up.to.date = FALSE
      cat("Updating VE dependency repository to add from BioConductor:\n")
      print(pkgs.missing.BioC)
      miniCRAN::addPackage(pkgs.missing.BioC, path=ve.dependencies, repos=bioc, type=ve.build.type, deps=FALSE)
    }
  }
  if ( up.to.date ) {
    cat("VE dependency repository up to date with BioConductor\n")
  }
  if ( up.to.date ) {
    cat("Updating CRAN dependency packages...\n")
    miniCRAN::updatePackages(path=ve.dependencies, repos=CRAN.mirror, type=ve.build.type, oldPkgs=pkgs.CRAN.lst, ask=FALSE)
    cat("Updating BioConductor dependency packages...\n")
    ignore <- miniCRAN::updatePackages(path=ve.dependencies, repos=bioc, type=ve.build.type, oldPkgs=pkgs.BioC.lst, ask=FALSE)
  }
} else {
  cat("Building VE repository from scratch from CRAN packages\n")
  miniCRAN::makeRepo(pkgs.CRAN.lst, path=ve.dependencies, repos=CRAN.mirror, type=ve.build.type)

  cat("Adding BioConductor packages to new VE repository\n")
  # BioConductor depends on some CRAN packages - no need to download those twice, so deps=FALSE
  miniCRAN::addPackage(pkgs.BioC.lst, path=ve.dependencies, repos=bioc, type=ve.build.type, deps=FALSE)
}

# Finally, set up a blank source repository to receive source packages
if ( ve.build.type != "source" ) {
  # build blank source repository tree
  # won't populate it until build-external.R, and then later build-runtime-pkgs-full.R
  src.contrib <- contrib.url(ve.dependencies, type="source")
  if ( ! dir.exists(src.contrib) ) {
    dir.create( src.contrib, recursive=TRUE, showWarnings=FALSE )
  }
}

# Verify the VE Repository with the following independent cross-check of dependencies

# pkgs.VE <- c(pkgs.CRAN.lst, pkgs.BioC.lst)
# ap <- available.packages(repos=ve.deps.url)
# getDependencies <- function(x) {
#   # Used in apply below to avoid a painfully long one-liner
#   # Extracts package names from a standard list of R dependencies
#   strsplit(split=", [ \n]*", paste( (y<-x[c("Package", "Depends", "Imports", "Extends", "LinkingTo")])[!is.na(y)], collapse=", "))
# }
# pkg <- sort(unique(unlist(apply(ap, 1, getDependencies))))
# pkg <- unique(sub("( |\\n)*\\(.*\\)", "", pkg))
# pkg <- setdiff(pkg, c(pkgs.BaseR, "R")) # Kill the BaseR packages from the list of dependencies, as well as dependency on R itself
# if ( length(setdiff(pkgs.VE, pkg)) > 0 ) {
#   cat("Discrepancy:\n")
#   print(setdiff(pkgs.VE, pkg))
# } else if (length(setdiff(pkg, pkgs.VE)) > 0 ) {
#   cat("Discrepancy:\n")
#   print(setdiff(pkg, pkgs.VE))
# } else {
#   cat("VE repository contents are complete\n")
# }
