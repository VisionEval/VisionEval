#!/bin/env Rscript

# Author: Jeremy Raw

# Builds .zip files for installers

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

message("========== BUILD BINARY INSTALLERS (.zip files) ==========")

# Short circuit if platform is Windows (we'll be using "ve-lib")
request.build = "binary"

# Set up .zip file names
# Need the ".zip" extension?
build.date <- Sys.Date()

installer.base   <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-Runtime-R",this.R,"_",build.date)),".zip")
if ( ! file.exists(installer.base) ) {
  stop("Must run installer-base build first for base elements",call.=FALSE)
}
installer.binary <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-Installer-",ve.platform,"-R",this.R,"_",build.date)),".zip")
installer.source <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-PackageSources-R",this.R,"_",build.date)),".zip")

if ( length(grep("^mac",ve.build.type))>0 ) {
  installer.pkg  <- installer.binary    # On Mac, the standard binary installer uses ve.pkgs
}

if ( ve.build.type != "source" ) {

  owd <- getwd()

  # Add ve-lib to base if on Windows
  if ( ve.platform == "Windows" ) {

    cat("Building Windows installer...")
    os.files <- basename(ve.lib)
    setwd(file.path(ve.lib,".."))

  } else if ( ve.platform == "MacOSX") {

    cat("Building Macintosh installer...")
    pkg.build.type = ve.build.type
    os.files <- contrib.url(basename(ve.pkgs), type=pkg.build.type)
    # Add ve-pkgs (
    cat("pkgs located here:",os.files,"\n")
    setwd(file.path(ve.pkgs,".."))

  } else stop("Unknown OS for binary installer build",call.=FALSE)

  unlink( installer.binary )
  zip(installer.base,os.files,flags=c("-r9Xq",paste0("--output-file=",installer.binary)))
  cat("Done\n")

  # Create package sources from src folder
  cat("Building Package Source installer...")
  unlink(installer.source)
  setwd(file.path(ve.src,".."))
  zip(installer.source,basename(ve.src),
    flags="-r9Xq",
    extras=c("-x",
      "*/*.Rproj",
      "src/*.Rproj",
      "*/*.Rcheck/*",
      "*.md"          # Already built these into 'docs'
    )
  )
  cat("Done\n")

  setwd(owd)

} else {

  cat("No binary installer possible for this OS",call.=FALSE)

}