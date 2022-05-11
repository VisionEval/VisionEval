# Source at the top of scripts to ensure runtime configuration is loaded
# Source this file at the beginning of each build step module

# Check if "ve.builder" is on the search path
# Create the build environment for VisionEval

env.builder <- local(
  {
    if ( length(grep("^ve.builder$",search()))==0 ) {
      env.builder <- attach(NULL,name="ve.builder")
      # Makefile will export VE_RUNTIME_CONFIG
      # R builder will have created "ve.builder" environment on search path
      ve.runtime.config <- Sys.getenv("VE_RUNTIME_CONFIG","")
      if ( !exists("no.config.msg",env.builder) ) {
        assign(
          "no.config.msg",
          "Build the 'configure' step to set up build environment",
          envir=env.builder
        )
      }
      if (
        length(ve.runtime.config)==0 ||
        ! nzchar(ve.runtime.config[1]) ||
        ! file.exists(normalizePath(ve.runtime.config,winslash="/"))
      ) {
        stop(no.config.msg,"\nMissing VE_RUNTIME_CONFIG ",ve.runtime.config,
          no.config.msg,call.=FALSE)
      }
      load(ve.runtime.config,envir=env.builder)
      env.builder
    } else as.environment("ve.builder")
  }
)

# Set a non-interactive default for development package installation

if ( ! exists("CRAN.mirror",envir=env.builder) ) {
  stop(
    "Error: configuration does not include CRAN.mirror\n",
    "Build the 'configure' step to set up build environemnt"
  )
}
# the build scripts generally have repos set explicitly to CRAN.mirror
# this is just a backstop...

r <- getOption("repos")
r["CRAN"] <- env.builder$CRAN.mirror
options(repos=r)

if ( exists("dev.lib") ) {
  .libPaths(dev.lib)
  if ( ! suppressWarnings(require("git2r",quietly=TRUE)) ) {
    install.packages("git2r",
      lib=dev.lib,
      repos=CRAN.mirror,
      dependencies=NA, type=.Platform$pkgType )
  }
} else {
  stop(no.config.msg,"\nMissing dev/lib",call.=FALSE)
}
if ( ! exists("checkVEEnvironment",envir=env.builder) || ! checkVEEnvironment() ) {
  stop(no.config.msg,"\ncheckVEEnvironment",call.=FALSE)
}

rm(env.builder)