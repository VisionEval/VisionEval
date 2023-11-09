# Run VisionEval

# Set VE_HOME in .Renviron to the directory containing the VisionEval installation.
# Can be either enduser (from installer) or developer (from Github)

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
    VE.build <- Sys.getenv("VE_BUILD",VE.home)
    dev.lib <- file.path(VE.build,"dev/lib",this.R)   #
    ve.lib  <- file.path(VE.build,"built/visioneval",this.R,"ve-lib")
    source("build/VisionEval-dev.R")  # Will return to VE_RUNTIME set above
    if ( ! dir.exists(dev.lib) || ! dir.exists(ve.lib) ) {
      message("Seeking built VisionEval in ",VE.build)
      message("However, VisionEval from ",VE.home," is not there.")
      message("Please run ve.build()")
      setwd(VE.home)
      source("build/VisionEval-dev.R")
    } else {
      ve.run()  # Ready to run
    }
  } else {
    source("VisionEval.R")
  }
  invisible(NULL)
})


