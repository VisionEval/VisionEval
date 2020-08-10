#!/bin/env Rscript

# Author: Jeremy Raw

# Read names of packages from command line and remove all artifacts of their
# build. Do something like this:
#   VE_DELETE="VEHouseholdVehicles" make clean-module

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

# Get the list of packages to clean

delete.me <- commandArgs(trailingOnly=TRUE)

if ( length(delete.me)==0 ) { message("No module list provided"); quit(status=1) }

built.path.src <- contrib.url(ve.repository, type="source")
built.path.binary <- contrib.url(ve.repository, type=ve.build.type)

# unlink <- function(x,...) { cat("Would delete:\n",x,"\n") }

cat("============ DELETE FILES =============\n")
print( delete.me )
for ( pkg in delete.me ) {
  pkg.src <- dir( file.path(built.path.src),pattern=paste0("^",pkg), full.names=TRUE )
  pkg.bin <- dir( file.path(built.path.binary),pattern=paste0("^",pkg), full.names=TRUE )
  pkg.tmp <- file.path(ve.src,pkg)
  pkg.lib <- file.path(ve.lib,pkg)

  if ( length(pkg.src)>0 )     { unlink(pkg.src); cat(pkg.src,"\n") } else cat("No Source Package.\n")
  if ( length(pkg.bin)>0 )     { unlink(pkg.bin); cat(pkg.bin,"\n") } else cat("No Binary Package.\n")
  if ( dir.exists( pkg.tmp ) ) { unlink(pkg.tmp,recursive=TRUE); cat(pkg.tmp,"\n") } else cat("No Built Source.\n")
  if ( dir.exists( pkg.lib ) ) { unlink(pkg.lib,recursive=TRUE); cat(pkg.lib,"\n") } else cat("No ve-lib Package.\n")
}
