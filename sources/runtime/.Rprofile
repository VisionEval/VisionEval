# Run VisionEval
# Set VE_HOME in .Renviron to the directory containing the VisionEval
# installation. Can be either enduser (from installer) or developer
# (from Github)

local( {
  this.R <- paste(R.version[c("major","minor")],collapse=".")

  VE.home <- Sys.getenv("VE_HOME",unset=getwd()) # Where to look for runnable VisionEval
  VE.developer <- file.exists(dev.dir<-file.path(VE.home,"build/VisionEval-dev.R"))
  if ( ! VE.developer && ! file.exists(file.path(VE.home,"VisionEval.R")) ) {
    stop("Cannot locate VisionEval startup file in ",VE.home)
  }

  # VisionEval will run in VE_RUNTIME - generally leaving the default is preferred
  ve.runtime <- Sys.getenv("VE_RUNTIME",unset=getwd()) # can also set VE_RUNTIME in .Renviron
  Sys.setenv(VE_RUNTIME=ve.runtime)

  setwd(VE.home) # get ready to run the startup file
  if ( VE.developer ) {
    dev.lib <- file.path(VE.home,"dev/lib",this.R)
    ve.lib  <- file.path(VE.home,"built/visioneval",this.R,"ve-lib")
    source("build/VisionEval-dev.R")
    if ( ! dir.exists(dev.lib) || ! dir.exists(ve.lib) ) {
      message("VisionEval in ",VE.home," has not been built yet")
      message("Start R from within ",VE.home," and run ve.build()")
    } else {
      .libPaths( c(dev.lib, ve.lib) )
      ve.run()
    }
  } else source("VisionEval.R")
  invisible(NULL)
})


