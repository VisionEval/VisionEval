# test_setup.R
# Comprehensively test VEModel and related interfaces from within the
# VEModel Github source directory.

# You should run this from the VEModel package root.
# Make sure your .libPaths includes the built ve-lib (e.g. by
# creating a suitable .REnviron file).

# If you're looking at this file in a built runtime, you should look
# at the test.R function the individual test_*
# functions are code you can try interactively to test the framework.
# You should NOT run the "rewind" or "setup" functions...
# The test functions are a more detailed and up-to-date version of what the
# walkthrough does: those are the target of test-based development for the
# framework.

# starting in the root of the package, just do this
#  source("tests/test_run.r")
# It will create a temporary folder to use as a runtime
# and list the available test functions.

if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}

# function: pseudo_package
setup <- function(ve.runtime=NULL) {
  # Creates or uses a fresh minimal runtime environment as a sub-directory of "tests"
  # Set VE_RUNTIME to some other location if desired (does not need to have a runtime
  # there yet, and in fact it's better if it doesn't).
  if ( ! is.character(ve.runtime) ) {
    ve.runtime <- Sys.getenv("VE_RUNTIME",unset=NA)
    if ( ! is.na(ve.runtime ) ) {
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- NA
      }
    }
    if ( is.na(ve.runtime) ) {
      ve.runtime <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)[1]
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="tests"),winslash="/",mustWork=FALSE)
        dir.create(ve.runtime)
      }
    }
  }
  ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=TRUE)
  Sys.setenv(VE_RUNTIME=ve.runtime)
  pkgload::load_all()
  ve.env <- VEModel::runtimeEnvironment()
  ve.env$ve.runtime <- ve.runtime; # override default from package load (working directory)
  setwd(ve.env$ve.runtime)

  base.test.file <- "test.R"
  test.file <- base.test.file
  if ( ! file.exists(test.file) ) test.file <- file.path("tests",base.test.file)
  if ( ! file.exists(test.file) ) test.file <- file.path("..",base.test.file)
  if ( ! file.exists(test.file) ) stop("Can't find test.R. Is your working directory correct?")
  source(test.file)

  if ( ! dir.exists("models") ) dir.create("models")
  message("Available Test Functions:")
  print(ls(pattern="^test_",envir=parent.frame(2)))

  logLevel("info") # Can override for specific test functions
}

takedown <- function() {
  start.dir <- NA
  ve.runtime <- NA
  if ( isNamespaceLoaded("VEModel") ) {
    ve.env <- VEModel::runtimeEnvironment()
    if ( exists("ve.runtime",envir=ve.env,inherits=FALSE) ) {
      ve.runtime <- ve.env$ve.runtime
    }
    if ( exists("start.dir",envir=ve.env,inherits=FALSE) ) {
      start.dir <- ve.env$start.dir
    }
  }
  if ( "package:VEModel" %in% search() ) detach("package:VEModel")
  unloadNamespace("VEModel")
  if ( ! is.na(start.dir) ) setwd(start.dir)
  if ( ! is.na(ve.runtime) ) {
    message("To remove runtime directory:")
    message("unlink('",ve.runtime,"',recursive=TRUE)")
  }
  loadhistory(".Rhistory") # get rid of rep("n",a.million.times) and other debugging leftovers
}

rewind <- function() {
  cat("Rewinding...")
  takedown()
  setup()
}

cleanup <- function() {
  takedown()
  runtimes <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)
  message("Removing:")
  print(runtimes)
  if ( length(runtimes)>0 && isTRUE(askYesNo("Remove runtimes?")) ) unlink(runtimes,recursive=TRUE)
}

# Now set it all up
rewind()
