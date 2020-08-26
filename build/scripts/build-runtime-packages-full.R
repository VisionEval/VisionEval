#!/bin/env Rscript

# Author: Jeremy Raw

# Merges dependencies and VE packages into a single repository (for Mac or Unix)
# If the current platform is Mac, build a repository of mac binary packages
# If the current platform is Unix, build a repository of source packages

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))
  
message("========== BUILD PACKAGES ( for Full Source Installation) ==========")

# Relay dependency list

load(ve.all.dependencies) # use all.dependencies

# Short circuit if platform is Windows (we'll be using "ve-lib")
request.build <- commandArgs(trailingOnly=TRUE)
cat("Source build requested\n")
ve.build.type = "source" # override for docker build on Windows etc.

if ( ! exists("all.dependencies") ) {
  stop("Run build-config.R to set up build environment")
}

# Load required Library
require(tools)

# Prepare package names and output repository contriburl

ve.pkgnames <- pkgs.db[pkgs.module,]$Package

# Start work...

cat("Preparing source distribution repository\n")
runtime.contrib.url <- contrib.url(ve.pkgs, type="source")
if ( ! dir.exists(runtime.contrib.url ) ) dir.create(runtime.contrib.url, recursive=TRUE, showWarnings=FALSE)
if ( ! file.exists(file.path(runtime.contrib.url,"PACKAGES")) ) {
  cat("Building fresh runtime distribution repository\n")
  for ( repo in c(ve.dependencies,ve.repository) ) {
    contriburl <- contrib.url(repo,type="source")
    pkgs <- dir(contriburl,full.names=TRUE)
    invisible( file.copy( pkgs, runtime.contrib.url, recursive=TRUE, overwrite=TRUE ) )
  }
  write_PACKAGES(runtime.contrib.url, type="source")
} else {
  cat("Checking if required packages are present\n")
  ap <- available.packages(repos=paste("file:", ve.pkgs, sep=""), type="source")
  missing.deps <- setdiff( all.dependencies, ap[,"Package"])
  if ( length(missing.deps) > 0 ) {
    cat("Adding missing dependencies to runtime distribution repository\n")
    deps.dir <- contrib.url(ve.dependencies,type="source")
    print(missing.deps)
    missing.deps <- file.path( deps.dir,modulePath( missing.deps, deps.dir ) )
    print(missing.deps)
    file.copy( missing.deps, runtime.contrib.url, recursive=TRUE, overwrite=TRUE )
  } else {
    cat("Runtime distribution repository dependencies are up to date\n")
  }
  missing.pkgs <- setdiff( ve.pkgnames, ap[,"Package"])
  if ( length(missing.pkgs) > 0 ) {
    cat("Adding missing VE packages to runtime distribution repository\n")
    pkgs.dir <- contrib.url(ve.repository,type="source")
    print(missing.pkgs)
    missing.pkgs <- file.path( pkgs.dir,modulePath( missing.pkgs, pkgs.dir ) )
    print(missing.pkgs)
    file.copy( missing.pkgs, runtime.contrib.url, recursive=TRUE, overwrite=TRUE )
  }
  write_PACKAGES(runtime.contrib.url, type="source")
}
cat("Runtime distribution repository VE packages are up to date\n")
