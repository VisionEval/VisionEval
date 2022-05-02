### setup.R
#   Set up the walkthrough environment
#   It's harmless to run this again, but generally you should just
#   let it auto-run by using "ve.test()" or (end-user runtime)
#   "walkthrough"
#   In dev environment, no parameters on ve.test will load the walkthrough.

# Establish the VisionEval environment (VEModel comes afterwards)

# Locate ve-lib
checkVE <- function(lib.loc=NULL) {
  return(all( c("visioneval","VEModel") %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
}

vePresent <- function() {
  if ( checkVE() ) return(TRUE)
  for ( wd in c(getwd(),dirname(getwd()),dirname(dirname(getwd()))) ) {
    message("Checking ",wd)
    lib.loc <- file.path(wd,"ve-lib")
    if ( checkVE(lib.loc) ) {
      .libPaths( c( lib.loc, .libPaths() ) )
      return(TRUE)
    }
  }
  return(FALSE)
}

# Set up a runtime directory for a walkthrough
# Specifically, don't look at VE_RUNTIME (that's just for the end user)
setupWalkthroughRuntime <- function() {
  if ( ! vePresent() ) stop("Cannot locate ve.lib - are you starting from your VE runtime?")

  walkthrough.action <- "Using"
  # Create a walkthrough directory within the current working directory
  message("Setting up walkthrough in ",getwd())
  walkthroughRuntime <- grep("runtime.*",list.dirs(),value=TRUE)[1]
  if ( ! dir.exists(walkthroughRuntime) ) {
    walkthroughRuntime <- normalizePath(tempfile(pattern="runtime",tmpdir="."),winslash="/",mustWork=FALSE)
    dir.create(walkthroughRuntime)
    walkthrough.action <- "Creating"
  } else {
    walkthroughRuntime <- normalizePath(walkthroughRuntime,winslash="/",mustWork=TRUE)
  }
  message(walkthrough.action," walkthrough runtime directory:")
  message(walkthroughRuntime)
  setwd(walkthroughRuntime)
}

setupWalkthroughRuntime()

