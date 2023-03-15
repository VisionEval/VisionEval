# Load required packages

if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}
if ( ! requireNamespace("VEModel",quietly=TRUE) ) {
  stop("Missing required package: 'VEModel'")
}

message("Loading built VEModel package")
suppressPackageStartupMessages(require("VEModel",quietly=TRUE))

logLevel <- function(log="info") {
  visioneval::initLog(Save=FALSE,Threshold=log)
}

testStep <- function(msg) {
  # Use 'message' to get contrasting color in RStudio
  message(paste("\n",paste(msg,collapse="\n"),"\n",sep="\n"))
}

stopTest <- function(msg) {
  stop(msg)
}

# Test the Dynamic module
test_dynamic <- function(log="warn") {

  logLevel(log)

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
  cleanups <- grep("VESnap-",dir("models",full.names=TRUE),value=TRUE)
  if ( length(cleanups) > 0 ) for ( snapmodel in cleanups ) {
    cat("Removing previous ",snapmodel,"\n")
    unlink(snapmodel,recursive=TRUE)
  } else cat("Nothing to clean up\n")

  # 1. standard installation uses Dynamic inline message
  testStep("Dynamic model with message configuration")
  # Need VEModel namespace resolution to work with pkgload, otherwise function
  # environment does not start with .Globalenv (it is nailed to the search()
  # that is in place when it is defined)
  dyn.1 <- VEModel::installModel("VESnap",var="dynamic",confirm=FALSE,log=log)
  print(dyn.1)
  if ( is.data.frame(dyn.1) ) { # No model/variant found
    stopTest("Dynamic model not found")
  }
  dyn.1$run(log=log)

  testStep("Original dynamic configuration:")
  viewSetup(dyn.1)
  # make a new version using pre-packaged configuration in "dynamic" dir
  # which in the sample model is located in the model root. It can also be
  # in "defs". It will print a message a summarize a field
  testStep("Copied model")
  dyn.2 <- dyn.1$copy("VESnap-dynamic-cfgfile",copyResults=FALSE)
  # adjust its configuration
  updateSetup(dyn.2,drop="Dynamic",DynamicDir="snap-dynamic")
  viewSetup(dyn.2,viewSource=TRUE)
  testStep("Reconfiguring model")
  dyn.2$configure()
  viewSetup(dyn.2$modelStages[[1]],viewSource=TRUE)
  testStep("Run with dynamic configuration file")
  dyn.2$run(log=log)
  cat("\n")
  print(dyn.2)

  # run with Dynamic requested but no configuration at all
  # should warn but continue
  testStep("Dynamic with no configuration")
  dyn.3 <- dyn.2$copy("VESnap-dynamic-noconfig",copyResults=FALSE)
  updateSetup(dyn.3,drop=c("Dynamic","DynamicDir"))
  dyn.3$configure()
  viewSetup(dyn.3$modelStages[[1]],viewSource=TRUE)
  dyn.3$run()

  # run with Dynamic requested, but bad configuration
  testStep("Dynamic mis-configured")
  dyn.fail <- dyn.3$copy("VESnap-dynamic-error",copyResults=FALSE)
  updateSetup(dyn.fail,drop="DynamicDir",Dynamic="Failure")
  dyn.fail$configure()
  viewSetup(dyn.fail$modelStages[[1]],viewSource=TRUE)
  dyn.fail$run()

  invisible(list(
    dyn.1=dyn.1,
    dyn.2=dyn.2,
    dyn.3=dyn.3,
    dyn.fail=dyn.fail
  ))
}  

# Test the Snapshot module
test_snapshot <- function(log="warn") {
  logLevel(log)

  testStep("Install VESnap-snapshot")
  snap <- installModel("VESnap","snapshot",overwrite=TRUE,confirm=FALSE)
  print(snap)
  testStep("Run the snapshot model")
  snap$run(log=log)
  testStep("Show available fields")
  print(snap$list())
  testStep("Extract source and snapshot fields")
  result <- snap$results()$select()$find(".*HhSize.*",Table="Household")
  print(result)
  df <- result$extract()[[1]] # first data.frame in a list
  testStep("Compare result fields")
  nm <- names(df)
  cat(sep="","all(",nm[1]," == ",nm[2],"): ",all(df[[nm[1]]]==df[[nm[2]]]),"\n")
  return(snap)
}
