#!/bin/env Rscript

# Author: Jeremy Raw

# Copies boilerplate (e.g. end user installation script) source tree files to
# the runtime - for things like the model data, run scripts and VEGUI app that
# are not in packages.

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

message("========== BUILD RUNTIME ENVIRONMENT (scripts, models) ==========")

# Get the VisionEval runtime files
# This will process the 'runtime' boilerplate (e.g. VisionEval.R startup script)

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

# Process the 'script' and 'model' items

cat("Script sources...\n")

copy.scripts <- pkgs.db[pkgs.script,c("Root","Path","Package","Target")]
copy.paths <- file.path(copy.scripts$Root, copy.scripts$Path, copy.scripts$Package)
if ( length(copy.paths) > 0 ) {
  for ( f in seq_along(copy.paths) ) {
    if ( nzchar(copy.scripts$Target[f]) ) {
      out.dir <- file.path(ve.runtime,copy.scripts$Target[f])
      if ( ! dir.exists(out.dir) ) {
        dir.create(out.dir,recursive=TRUE)
      }
    } else {
      out.dir <- ve.runtime
    }
    test.path <- target.path <- file.path(out.dir,copy.scripts$Package[f])
    do.copy <- FALSE
    if ( dir.exists(copy.paths[f]) && ! dir.exists(target.path) ) {
      dir.create(target.path)
      target.path <- dirname(target.path) # otherwise it duplicates the script name redundantly as a sub-directory
      test.path <- target.path
      do.copy <- TRUE
    } else if ( dir.exists(target.path) ) {
      target.path <- dirname(target.path)
    }
    pkg.files <- dir(copy.paths[f],recursive=TRUE,all.files=TRUE) # force deeper check for actual files
    if ( do.copy || (test.newer <- newerThan(copy.paths[f], test.path, pkg.files=pkg.files)) ) {
      cat("Copying Script", copy.paths[f],"\n")
      invisible(file.copy(copy.paths[f], target.path, recursive=dir.exists(copy.paths[f])))
    }
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
