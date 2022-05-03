### setup.R
#   Set up the walkthrough environment
#   It's harmless to run this again, but generally you should just
#   let it auto-run by using "ve.test()" or (end-user runtime) "walkthrough"
#   In dev environment, no parameters on ve.test() will load the walkthrough
#   In runtime enviroment, "walkthrough" will do the same (in walkthrough's own runtime)

local( {  # Wrapping in "local" will leave no new names in the user's environment
  # First part is redundant with standard developer or runtime setup
  # It will support starting the walkthrough from an arbitrary directory if VE is nearby
  # This script presumes we're working in the directory that contains the walkthrough scripts

  checkVE <- function(lib.loc=NULL) {
    # VE is already located?
    return(all( c("visioneval","VEModel") %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
  }

  # Set up .libPaths with locally-discovered (or pre-existing) ve-lib
  vePresent <- function() {
    if ( checkVE() ) return(TRUE)
    # No obvious VE installation - look around nearby for "ve-lib"
    for ( wd in c(getwd(),dirname(getwd()),dirname(dirname(getwd()))) ) {
      message("Checking ",wd)
      lib.loc <- file.path(wd,"ve-lib")
      if ( checkVE(lib.loc) ) {
        .libPaths( c( lib.loc, .libPaths() ) )
        return(TRUE)
      }
    }
    # Failed to find ve-lib
    return(FALSE)
  }
  if ( ! vePresent() ) stop("Cannot locate ve.lib - are you working in your VE runtime?")

  # Create (or find) a walkthrough runtime directory within the working directory
  # which is presumed to be "walkthrough"
  message("Setting up walkthrough in ",getwd())
  walkthrough.action <- "Using"
  walkthroughRuntime <- grep("runtime.*",list.dirs(),value=TRUE)[1]
  if ( ! dir.exists(walkthroughRuntime) ) {
    walkthroughRuntime <- normalizePath(tempfile(pattern="runtime",tmpdir="."),winslash="/",mustWork=FALSE)
    dir.create(walkthroughRuntime)
    walkthrough.action <- "Created"
  } else {
    walkthroughRuntime <- normalizePath(walkthroughRuntime,winslash="/",mustWork=TRUE)
  }
  message(walkthrough.action," walkthrough runtime directory:")
  setwd(walkthroughRuntime)
  message(getwd())
} )
