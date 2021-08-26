# Locate ve-lib
checkVE <- function(lib.loc=NULL) {
  return(all( c("visioneval","VEModel") %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
}

vePresent <- function() {
  if ( checkVE() ) return(TRUE)
  for ( wd in c(getwd(),dirname(getwd())) ) {
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
    ve.runtime <- grep("walkthrough.*",list.dirs(),value=TRUE)[1]
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

# Load VEModel package (in effect, the visioneval environment)
require(VEModel)

# Set up sample models
message("Creating model environment")
if ( ! dir.exists("models") ) {
  dir.create("models")
}
message("Pre-running model (Staged VERSPM)")
model.path <- "VERSPM-run"
if ( ! dir.exists(file.path("models",model.path)) ) {
  message("Installing VERSPM ('pop' variant) as ",model.path)
  vr <- installModel("VERSPM",modelPath=model.path,variant="pop",confirm=FALSE)
} else {
  message("Using existing ",model.path)
  vr <- openModel(model.path)
}
message("Making sure model '",model.path,"' has been run...")
vr$run() # default "continue" will not re-run model if already "Run Complete"
print(vr)

