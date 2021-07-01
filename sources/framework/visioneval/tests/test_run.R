# Test.R
# Comprehensively test VEModel and related interfaces
# Also provides working examples of the API

# function: pseudo_package
# 
if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}
if ( ! requireNamespace("stringr",quietly=TRUE) ) {
  stop("Missing required package: 'stringr'")
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
  ve.env$ve.runtime <- ve.runtime; # override default from package load (working directory)
  setwd(ve.env$ve.runtime)

  if ( ! dir.exists("models") ) dir.create("models")
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

testStep <- function(msg) {
  cat("",paste(msg,collapse="\n"),"",sep="\n")
}

test_classic <- function(modelName="CLASSIC",save=TRUE) {
  modelPath <- file.path("models",modelName)
  owd <- getwd()
  on.exit(setwd(owd))

  if ( ! save && dir.exists(modelPath) ) {
    testStep("Clearing runtime environment")
    unlink(modelPath,recursive=TRUE)
  } else {
    testStep("Should save previous Datastore")
    print(dir(modelPath))
  }

  if ( !save || ! dir.exists(modelPath) ) {
    testStep(paste("Copying VERSPM model from package as",modelName))
    dir.create(modelPath)
    file.copy(dir(file.path(start.dir,"../..","models","VERSPM"),full.names=TRUE),modelPath,recursive=TRUE)
  }

  testStep("Run the model...")
  setwd(modelPath)
  message("Directories before:\n")
  for ( f in dir() ) message(f)
  source("run_model.R")
  if ( save) {
    message("Directories after:\n")
    for ( f in dir() ) message(f)
  }

  invisible(NULL)
}

# Now set it all up
rewind()
