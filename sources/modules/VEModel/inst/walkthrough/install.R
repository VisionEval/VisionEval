# Load VEModel package (in effect, the visioneval environment)
require(VEModel)

# framework functions need namespace resolution, visioneval::frameworkFunction
print(search())            # Notice VEModel package attached, but not visioneval
print(loadedNamespaces())  # Notice "visioneval" among the loaded namespaces

##########################
# CREATE MODEL ENVIRONMENT
##########################

# Eliminate everything other than possible pre-existing VERSPM-run
message("Creating model environment")
if ( ! dir.exists("models") ) {
  dir.create("models")
} else {
  # clean up the mini-model if it's still there
  if ( dir.exists("model/BARE") ) unlink("model/BARE",recursive=TRUE)

  # delete any other models in "models" directory
  otherModels <- grep("VERSPM-run",dir("models",full.names=TRUE),invert=TRUE,value=TRUE)
  if ( length(otherModels) > 0 ) unlink(otherModels,recursive=TRUE)
}

message("Pre-running model (Base VERSPM)")
model.path <- "VERSPM-run"
if ( ! dir.exists(file.path("models",model.path)) ) {
  message("Installing VERSPM ('base' variant) as ",model.path)
  vr <- installModel("VERSPM",modelPath=model.path,variant="base",confirm=FALSE)
} else {
  message("Using existing ",model.path)
  vr <- openModel(model.path)
}
message("Making sure model '",model.path,"' has been run...")
vr$run() # default "continue" will not re-run model if already "Run Complete"
print(vr)

#######################################
# DEMO INSTALLING OTHER STANDARD MODELS
#######################################

# The standard models are the same ones we distribute in the current VisionEval
#  but they have been restructured into "Next Generation" model setups

# install models
# Show available models
installModel()

# Show avaialble variants for one of the models
installModel("VERSPM",var="") # "var" is short for "variant" - you can spell it out
# Empty string for variant requests the list of available variants for VERSPM

# Install the base variant as "VERSPM" (with a confirm dialog)
dir("models") # Setup.R already installed and ran "VERSPM-run" (using the "pop" variant - more on that below)

installModel("VERSPM") # default if running interactively is to ask for a "y" to confirm installation
# If no variant is specified, you get "base"

installModel("VERSPM",variant="pop")  # Multi-stage version of VERSPM

dir("models") # Note that the installed name includes the variant: VERSPM-base
# Can try VERPAT too
installModel("VERPAT",modelPath="MYRPAT",confirm=FALSE) # base variant, but with name we chose

# See what we've got
dir("models")

# opening models
vrb <- openModel("VERSPM-base")
print(vrb) # Initialized
mwr <- model.with.results <- openModel("VERSPM-run")
print(mwr)  # Run Complete (has results from being run above, or in an earlier session)
