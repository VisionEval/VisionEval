# zzz.R - visioneval package .onLoad function

# This file contains an .onAttach function that verifies presence of
# the visioneval environment, and creates it if not present. If
# ve.runtime is not defined in that environment, it creates ve.runtime
# and sets it to getwd()

# .onAttach is called when a library attached to

.onLoad <- function(libname, pkgname) {
  # create and populate the VisionEval environment, ve.env
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the new VisionEval!")
}

.onDetach <- function(libpath) {
  # Detach and destroy the visioneval environment
}