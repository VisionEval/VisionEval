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

if ( ! vePresent() ) stop("Cannot locate ve.lib - are you starting from your VE runtime?")

# Set up a runtime directory for a walkthrough
local( {
  ve.runtime <- Sys.getenv("VE_RUNTIME",unset=NA)
  if ( ! is.na(ve.runtime ) ) {
    if ( ! dir.exists(ve.runtime) ) {
      ve.runtime <- NA
    }
  }
  walkthrough.action <- "Using"
  if ( is.na(ve.runtime) ) {
    # Create a walkthrough directory within the current working directory
    message("Setting up walkthrough in ",getwd())
    ve.runtime <- grep("runtime.*",list.dirs(),value=TRUE)[1]
    if ( ! dir.exists(ve.runtime) ) {
      ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="."),winslash="/",mustWork=FALSE)
      dir.create(ve.runtime)
      walkthrough.action <- "Creating"
    }
    ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=TRUE)
  }
  message(walkthrough.action," walkthrough runtime directory:")
  message(ve.runtime)

  Sys.setenv(VE_RUNTIME=ve.runtime)
  setwd(ve.runtime)
} )

# create helper function to switch logLevel
# ("info" gives a lot more details, "trace" is overwhelming)
logLevel <- function(log="warn") visioneval::initLog(Save=FALSE,Threshold=log)
