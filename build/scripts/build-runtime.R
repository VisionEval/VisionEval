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

# Process any framework or module package tests (locate them in the "tools/tests/<Package>" folder
# Runtime ve.test("<Package>") will attach those scripts  contents into "test:<Package>"
cat("Setting up Framework and Module tests...\n")
copy.tests <- pkgs.db[c(pkgs.framework,pkgs.module),]
copy.tests <- copy.tests[nzchar(copy.tests$Test),c("Root","Path","Package","Test")]
test.scripts <- data.frame(Root=NA,Path=NA,Package=NA,Test=NA)
for ( test in 1:nrow(copy.tests) ) {
  test.files <- unlist(strsplit(copy.tests[test,"Test"],";",fixed=TRUE))
  test.row <- copy.tests[test,c("Root","Path","Package","Test")]
  for ( fn in test.files ) {
    if ( ! grepl("/|\\\\",fn ) ) fn <- file.path("tests",fn)
    test.row$Test <- fn
    test.scripts <- rbind(test.scripts,test.row)
  }
}
# Reformat the test.scripts data.frame so we can just treat the tests as scripts...
test.scripts <- test.scripts[-1, ] # remove dummy row
if ( nrow(test.scripts) > 0 ) {
  test.scripts$Target <- file.path("tools/tests",test.scripts$Package)
  test.scripts$Path <- file.path(test.scripts$Path,test.scripts$Package,dirname(test.scripts$Test))
  test.scripts$Package <- basename(test.scripts$Test)
  test.scripts <- test.scripts[,c("Root","Path","Package","Target")]
}

# Process the 'script' (and tests, if any) and 'model' items

copy.scripts <- pkgs.db[pkgs.script,c("Root","Path","Package","Target")]
if ( nrow(test.scripts) > 0 ) {
  copy.scripts <- rbind(copy.scripts,test.scripts)
  test.msg <- paste(" (and",nrow(test.scripts),"Test files)")
} else test.msg <- ""
cat("Script sources",test.msg,"...\n",sep="")

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
      if ( file.exists(copy.paths[f]) ) {
        cat("Copying Script", copy.paths[f],"\n")
        cat("            TO",target.path,"\n")
        invisible(file.copy(copy.paths[f], target.path, recursive=dir.exists(copy.paths[f])))
      } else {
        cat("Configured source file does not exist:",copy.paths[f],"\n")
      }
    }
  }
}

cat("Model sources...\n")
# NOTE: as of "Next Gen" 2022, probably only copies a Readme.md telling the user
# to use VEModel::installModel to populate the "models" directory.
# We don't need the readme: the standard runtime script (sources/runtime/VisionEval.R) will create
# the "models" directory if it does not exists.
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
    cat(paste("Copying Model: ", copy.paths),sep="\n")
    dir.create( model.path, recursive=TRUE, showWarnings=FALSE )
    invisible(file.copy(copy.paths, model.path, recursive=TRUE))
  } else {
    cat("Model files are up to date.\n")
  }
}
cat("Runtime setup is complete.\n")
