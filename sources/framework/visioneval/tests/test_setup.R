# Test.R
# Comprehensively test VEModel and related interfaces
# Also provides working examples of the API

# You should run this from the visioneval package root.
# Make sure your .libPaths includes the built ve-lib (e.g. by
# creating a suitable .REnviron file).
# If you're looking at this file in a built runtime, the individual test_*
# functions are code you can try interactively to test the framework.
# You should NOT run the "rewind" or "setup" functions...
# The test functions are a more detailed and up-to-date version of what the
# walkthrough does: those are the target of test-based development for the
# framework.

# starting in the root of the package, just do this
#  source("tests/test_run.r")
# It will create a temporary folder to use as a runtime
# and list the available test functions.

# function: pseudo_package
# 
if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}

# Working directory should be the package base directory
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
  if ( ! "ve.env" %in% search() ) {
    message("Setup attaching ve.env")
    ve.env <- attach(name="ve.env",NULL)
  } else {
    ve.env <- as.environment("ve.env")
  }
  ve.env$ve.runtime <- ve.runtime # override default from package load (working directory)
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
}

ve.packages <- c(
  "VE2001NHTS",
  "VEPowertrainsAndFuels",
  "VEHouseholdTravel",
  "VEHouseholdVehicles",
  "VELandUse",
  "VEReports",
  "VEScenario",
  "VESimHouseholds",
  "VESimLandUse",
  "VESimLandUseData",
  "VESimTransportSupply",
  "VESyntheticFirms",
  "VETransportSupply",
  "VETransportSupplyUse",
  "VETravelDemandMM",
  "VETravelPerformance",
  "VEModel"
)

takedown <- function() {
  start.dir <- NA
  ve.runtime <- NA
  if ( isNamespaceLoaded("visioneval") && "ve.env" %in% search() ) {
    ve.env <- as.environment("ve.env")
    if ( exists("ve.runtime",envir=ve.env,inherits=FALSE) ) {
      ve.runtime <- ve.env$ve.runtime
    }
    if ( exists("start.dir",envir=ve.env,inherits=FALSE) ) {
      start.dir <- ve.env$start.dir
    }
  }
  if ( "package:visioneval" %in% search() ) {
    detach("package:visioneval")
  }
  if ( "ve.env" %in% search() && ! "start.dir" %in% ls("ve.env") ) {
    detach("ve.env")
    if ( exists("ve.env",inherits=FALSE) ) rm(ve.env)
  }
  for ( p in ve.packages ) {
    if ( isNamespaceLoaded(p) ) unloadNamespace(p)
  }
  unloadNamespace("visioneval")

  if ( ! is.na(start.dir) ) setwd(start.dir)
  if ( ! is.na(ve.runtime) ) {
    message("To remove runtime directory:")
    message("unlink('",ve.runtime,"',recursive=TRUE)")
  }
  loadhistory(".Rhistory") # get rid of rep("n",a.million.times) and other debugging leftovers
}

rewind <- function() {
  if ( ! "ve.env" %in% search() ) {
    message("rewind creating ve.env")
    message(getwd())
    ve.env <- attach(name="ve.env",NULL)
  } else {
    ve.env <- as.environment("ve.env")
  }
  if ( ! "start.dir" %in% ls("ve.env") ) {
    message("setting start.dir")
    ve.env$start.dir <- getwd()
  }
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
