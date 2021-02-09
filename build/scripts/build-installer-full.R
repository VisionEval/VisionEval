#!/bin/env Rscript

# Author: Jeremy Raw

# Builds .zip files for installers

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

message("========== BUILD FULL SOURCE INSTALLER (.zip files) ==========")

ve.build.type = "source"

# Set up .zip file names
# Need the ".zip" extension?
build.date <- Sys.Date()

installer.base   <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-Runtime-R",this.R,"_",build.date)),".zip")
if ( ! file.exists(installer.base) ) {
  stop("Must run installer-base build first for base elements",call.=FALSE)
}
installer.pkg  <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-Installer-Full-R",this.R,"_",build.date)),".zip")

owd <- getwd()

pkg.build.type = "source"
pkgs.contrib.url <- contrib.url(basename(ve.pkgs), type=pkg.build.type)
cat("pkgs located here:",pkgs.contrib.url,"\n")
# Add ve-pkgs (
cat("Building cross-platform Full Installer...")
unlink( installer.pkg )
setwd(file.path(ve.pkgs,".."))
zip(installer.base,pkgs.contrib.url,flags=c("-r9Xq",paste0("--output-file=",installer.pkg)))
cat("Done\n")

setwd(owd)