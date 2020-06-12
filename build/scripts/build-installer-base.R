#!/bin/env Rscript

# Author: Jeremy Raw

# Builds .zip files for installers

# Load runtime configuration
source(file.path(getwd(),"scripts/get-runtime-config.R"))

cat("==================== BUILDING INSTALLER BASE (.zip files) ====================\n")

require(utils)

# Set up .zip file names
# Need the ".zip" extension?
build.date <- Sys.Date()

# TESTING
ve.zipout <- file.path(ve.output,this.R)

installer.base   <- paste0(file.path(ve.zipout,paste0("VE-Runtime-R",this.R,"_",build.date)),".zip")

owd <- getwd()

cat("Building runtime base...")
setwd(ve.runtime) # Always set up installer.base
unlink( installer.base ) # Always start fresh
zip(installer.base,".",
  flags="-r9Xq", # recurse diretories, maximum compression, discard fancy attributes
  extras=c("-x",        # leave out a bunch of undesirable stuff
    # WARNING: each pattern is an occult incantation
    # No explanation, e.g., why "*.gitignore*" or "*.Rbuildignore" finds them in all subdirectories
    # but *.Rdata* only leaves them out of subdirectories and we need a second pattern for the root
    ".Renviron",        # This will get regenerated on install
    "*.md",             # These have been built into 'docs'
    "*.gitignore*",     # Not relevant for installer
    "*.Rbuildignore*",
    "*/*.Rproj",        # Exclude in subdirectories
    "models/*.Rproj",   # Need both this and previous
    ".R[Dd]ata",        # These are artifacts of testing the runtime
    "*.R[Dd]ata*",      # In particular, not VisionEval.Rdata
    "*/*.R[Dd]ata",
    ".Rhistory",
    "*.Rproj.user/*",
    "*/Datastore/*",
    "*/ModelState.Rda",
    "*/Log*.txt"
  )
)
cat("Done\n")

cat("Adding",basename(ve.docs),"to runtime base...")
setwd(file.path(ve.docs,".."))
zip(installer.base,basename(ve.docs),flags=c("-r9Xq"))
cat("Done\n")

setwd(owd)
