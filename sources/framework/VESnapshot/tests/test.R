# Load required packages

if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}

message("Loading built VEModel package")
suppressPackageStartupMessages(require("VEModel",quietly=TRUE))

logLevel <- function(log="info") {
  visioneval::initLog(Save=FALSE,Threshold=log)
}

testStep <- function(msg) {
  cat("",paste(msg,collapse="\n"),"",sep="\n")
}

stopTest <- function(msg) {
  stop(msg)
}

# Test the Dynamic module
test_dynamic <- function(log="warn") {
  # 1. Dynamic: only a message from model's configuration
  # 2. Dynamic: show summary of a field in the model run (from dynamic
  #    configuration file)
  # 3. Dynamic: no configuration at all (adds simple instructions to log)
  # 4. Dynamic: defective configuration (warning issued, model still runs)

  # install the basic model, then copy it and fiddle with its configuration
  # using inline editing (see VEModel walkthrough 09-run-parameters.R)

  # Need to require inside function if running via pkgloadd
  testStep("Clean up previous...")
  # Clean up prior VESnap models
  for ( snapmodel in grep("^VESnap-",dir("models",full.names=TRUE) ) ) {
    unlink(snapmodel,recursive=TRUE)
  }

  # 1. standard installation uses Dynamic inline message
  testStep("Dynamic inline")
  # Need VEModel namespace resolution to work with pkgload, otherwise function
  # environment does not start with .Globalenv (it is nailed to the search()
  # that is in place when it is defined)
  dyn.1 <- installModel("VESnap",var="dynamic",confirm=FALSE)
  if ( is.data.frame(dyn.1) ) { # No model/variant found
    print(dyn.1)
    stopTest("Dynamic model not found")
  }
  # dyn.1$run(log=log)
  print(ve.runtime)
  print(dyn.1)
  print(getwd())
  stopTest("Just installing")

  # make a new version using pre-packaged configuration in "dynamic" dir
  # which in the sample model is located in the model root. It can also be
  # in "defs". It will print a message a summarize a field
  testStep("Dynamic configuration file")
  dyn.2 <- dyn.1$copy("VESnap-dynamic-cfgfile",copyResults=FALSE)
  # adjust its configuration
  updateSetup(dyn.2,drop="Dynamic",DynamicDir="dynamic",inFile=TRUE)
  viewSetup(dyn.2)
  dyn.2$configure(fromFile=FALSE)
  dyn.2$run(log=log)

  stopTest("After trying to change the configuration")

  # run with Dynamic requested but no configuration at all
  # should warn but continue
  testStep("Dynamic with no configuration")
  dyn.3 <- dyn.2$copy("VESnap-dynamic-noconfig",copyResults=FALSE)
  updateSetup(dyn.3,drop=c("Dynamic","DynamicDir"))
  dyn.3$configure(fromFile=FALSE)
  dyn.3$run()

  # run with Dynamic requested, but bad configuration
  testStep("Dynamic mis-configured")
  dyn.fail <- dyn.3$copy("VESnap-dynamic-error",copyResults=FALSE)
  updateSetup(dyn.3,Dynamic="Failure")
  dyn.fail$configure(fromFile=FALSE)
  dyn.fail$run()

}  

# Test the Snapshot module
test_snapshot <- function(log="warn") {
  # 1. Snapshot with a single instance, defined in model's configuration
  # 2. Snapshot with two instances, defined in snapshot directory
  # 3. Snapshot unconfigured (warning issued, model still runs)
  # 4. Snapshot defective configuration (error issued, model stops)

  if ( ! missing(log) ) logLevel(log)

  testStep("No tests yet for Snapshot")
}
