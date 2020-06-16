# Source at the top of scripts to ensure runtime configuration is loaded
# Source this file at the beginning of each build step module

# Restructure for r-based build

# Check if "ve.env" is on the search path
# Create the build environment for VisionEval
local(
  {
    r <- getOption("repos")
    r["CRAN"] <- "https://cloud.r-project.org"
    options(repos=r)

    env.loc <- grep("^ve.env$",search(),value=TRUE)
    if ( length(env.loc)==0 ) {
      attach(NULL,name="ve.env")
      ve.runtime.config <- Sys.getenv("VE_RUNTIME_CONFIG","")
      if (
        length(ve.runtime.config)==0 ||
        ! nzchar(ve.runtime.config[1]) ||
        ! file.exists(normalizePath(ve.runtime.config,winslash="/"))
      ) {
        stop("Missing VE_RUNTIME_CONFIG ",ve.runtime.config,
          "\nRun build-config.R to set up build environment",call.=FALSE)
      }
      load(ve.runtime.config,envir=as.environment("ve.env"))
    }
  }
)
if ( exists("dev.lib") ) {
  .libPaths(c(dev.lib,.libPaths()))
  if ( ! suppressWarnings(require(git2r,quietly=TRUE)) ) {
    install.packages("git2r",
      lib=dev.lib,
      dependencies=NA, type=.Platform$pkgType )
  }
} else {
  stop("run build-config.R to set up build environment; missing dev/lib",call.=FALSE)
}
if ( ! exists("checkVEEnvironment",envir=as.environment("ve.env")) || ! checkVEEnvironment() ) {
  stop("Run build-config.R to set up build environment")
}
