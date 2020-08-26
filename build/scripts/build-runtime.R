#!/bin/env Rscript

# Author: Jeremy Raw

# Copies boilerplate (e.g. end user installation script) source tree files to
# the runtime - for things like the model data, run scripts and VEGUI app that
# are not in packages.

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

message("========== BUILD RUNTIME ENVIRONMENT (scripts, models) ==========")

# Copy the runtime boilerplate

# Get the boilerplate files from boilerplate.lst
# boilerplate.lst just contains a list of the files to copy to runtime separated by
# whitespace (easiest just to do one file/directory name per line.

# Copy the boilerplate files, checking to see if what we expected was there.
# WARNING: this won't work with boilerplate **directories**, only **files**

# cat("Copying boilerplate files...\n")
# # Set the boilperplate folder
# ve.boilerplate <- file.path(ve.installer,"boilerplate")
# 
# # bp.file.list <- scan(file=file.path(ve.boilerplate, "boilerplate.lst"),
#                      quiet=TRUE, what=character())
# 
# bp.files <- file.path(ve.boilerplate, bp.file.list)
# if ( length(bp.files) > 0 ) {
#   any.newer <- FALSE
#   for ( f in bp.files ) {
#     any.newer <- any( any.newer, newerThan(f,file.path(ve.runtime,basename(f))) )
#   }
#   success <- FALSE
#   if ( any.newer ) {
#     # there may be nothing to recurse into...
#     cat(paste("Copying Boilerplate File: ", bp.files),sep="\n")
#     success <- suppressWarnings(file.copy(bp.files, ve.runtime, recursive=TRUE))
#     if ( ! all(success) ) {
#       cat("WARNING!\n")
#       print(paste("Failed to copy boilerplate: ", basename(bp.files[!success])))
#       cat("which may not be a problem (e.g. .Rprofile missing)\n")
#       cat(".Rprofile is principally intended to house default ve.remote for online installer.\n")
#       cat("If something else is missing, you should revisit boilerplate/boilerplate.(bash.)lst\n")
#     }
#   } else {
#     cat("Boilerplate runtime is up to date\n")
#   }
# } else {
#   stop("No boilerplate files defined to setup runtime!")
# }

# Get the VisionEval runtime files
# This will process the 'runtime'

cat("Runtime sources...\n")
copy.runtime <- pkgs.db[pkgs.runtime,c("Root","Path","Package")]
paths <- file.path(copy.runtime$Root, copy.runtime$Path, copy.runtime$Package)
copy.paths <- dir(paths,all.files=TRUE,full.names=TRUE,no..=TRUE)
if ( length(copy.paths) > 0 ) {
  any.newer <- FALSE
  for ( f in seq_along(copy.paths) ) {
    newer <- newerThan(copy.paths[f], file.path(ve.runtime,basename(copy.paths[f])))
    any.newer <- any( any.newer, newer )
  }
  if ( any.newer ) {
    cat(paste("Copying Runtime: ", copy.paths),sep="\n")
    invisible(file.copy(copy.paths, ve.runtime, recursive=TRUE))
  } else {
    cat("Runtime files are up to date.\n")
  }
}

# Create the R version tag in the runtime folder
cat("that.R:",this.R,"\n",sep="",file=file.path(ve.runtime,"r.version"))
cat("ve.lib.name:",basename(ve.lib),"\n",sep="",file=file.path(ve.runtime,"r.version"),append=TRUE)
cat("ve.pkgs.name:",basename(ve.pkgs),"\n",sep="",file=file.path(ve.runtime,"r.version"),append=TRUE)

# Get the VisionEval sources, if any are needed
# This will process the 'script' and 'model' items listed in dependencies/VE-dependencies.csv

cat("Script sources...\n")

copy.scripts <- pkgs.db[pkgs.script,c("Root","Path","Package")]
copy.paths <- file.path(copy.scripts$Root, copy.scripts$Path, copy.scripts$Package)
if ( length(copy.paths) > 0 ) {
  any.newer <- FALSE
  for ( f in seq_along(copy.paths) ) {
    target <- file.path(ve.runtime,copy.scripts$Package[f])
    newer <- newerThan(copy.paths[f], target)
    any.newer <- any( any.newer, newer )
  }
  if ( any.newer ) {
    cat(paste("Copying Scripts: ", copy.paths),sep="\n")
    invisible(file.copy(copy.paths, ve.runtime, recursive=TRUE))
  } else {
    cat("Script files are up to date.\n")
  }
}

cat("Model sources...\n")

copy.models <- pkgs.db[pkgs.model,c("Root","Path","Package")]
copy.paths <- file.path(copy.models$Root, copy.models$Path, copy.models$Package)
model.path <- file.path(ve.runtime,"models")
if ( length(copy.paths) > 0 ) {
  any.newer <- FALSE
  for ( f in seq_along(copy.paths) ) {
    target <- file.path(model.path,copy.models$Package[f])
    newer <- newerThan(copy.paths[f], target)
    any.newer <- any( any.newer, newer )
  }
  if ( any.newer ) {
    cat(paste("Copying Models: ", copy.paths),sep="\n")
    dir.create( model.path, recursive=TRUE, showWarnings=FALSE )
    invisible(file.copy(copy.paths, model.path, recursive=TRUE))
  } else {
    cat("Model files are up to date.\n")
  }
}
cat("Runtime setup is complete.\n")
