#' @include environment.R models.R results.R query.R
# zzz.R - visioneval package .onLoad function

# This file contains an .onAttach function that loads the VisionEval
# runtime environment

# .onAttach is called when a library attached to

.onAttach <- function(libname, pkgname) {
  # library(VEModel) will always put us in VE_RUNTIME from the system environment
  packageStartupMessage("Welcome to the new VisionEval!")
  initLog(Save=FALSE,Threshold=Sys.getenv("VE_LOGLEVEL",unset="warn")) # Set default logging threshold
  getSetup(reload=TRUE)   # with no arguments, reload the ve.runtime configuration and return ve.env$RunParam_ls
  packageStartupMessage(paste0("Running in ",getwd()))
}

.onDetach <- function(libpath) {
  # If attaching the package put us in a different working directory,
  # then detaching should put us back where we came from.
  if ( !is.na(ve.env$start.dir) ) setwd(ve.env$start.dir)
}
