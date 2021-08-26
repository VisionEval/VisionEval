# Run initial setup
source("Setup.R")

# install models
# Show available models
installModel()

# Show avaialble variants for one of the models
installModel("VERSPM",var="") # "var" is short vor "variant" - you can spell it out

# Install the base variant as "VERSPM" (with a confirm dialog)
installModel("VERSPM") # default if running interactively is to ask for a "y" to confirm installation
dir("models") # Note that installed name includes the variant: VERSPM-base

# Install a different variant under another name (without interactive confirmation)
# (we'll work with this one below to examine how to run scenarios)
installModel("VERSPM",modelPath="VERSPM-staged",variant="pop",confirm=FALSE)

# Can try VERPAT too
installModel("VERPAT",modelPath="VERPAT",confirm=FALSE) # base variant, but with name we chose

# See what we've got
dir("models")

# opening models
vrb <- openModel("VERSPM-base")
print(vr)

# inspecting model inputs
inputs <- vrb$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
print(inputs)
input.dir <- unique(vrb$dir(inputs=TRUE,shorten=FALSE))
required.files <- unique(file.path(input.dir,inputs[,"FILE"]))

# inspecting model stages
print(vr$modelStages)  # list of stage objects - only one in "base" model

vr <- openModel("VERSPM-run") # pre-created and run in setup.R
print(vrs$modelStages) # Three stages - we'll get back to stages

# running models
  # reset
  # save
  # continue
vr$dir()              # List the contents of the model
vr$dir(inputs=TRUE)   # List just the model input directories
vr$dir(inputs=TRUE,all.files=TRUE) # List all the input files...

vr$run("reset")    # throw away existing results and re-run
vr$dir()           # notice presence of results directory
vr$dir(results=T,all.files=TRUE)

vr$run("save")     # move existing results into an archive and re-run
vr$dir()

vr$run("continue") # re-run any stage that is not "Run Complete" - does nothing here
vr$run()           # same as vr$run("continue")

# examine model parameters
# default visioneval and VEModel parameters
visioneval::defaultVERunParameters()

# parameters defined in ve.runtime (initially, none)
viewSetup(fromFile=TRUE)

# parameters defined in vrs (Staged) model
viewSetup(vrs,fromFile=TRUE)

# parameters defined in a vrs (Staged) model stage configuration file
viewSetup(vrs$modelStages[[2]],fromFile=TRUE)

# Let's change the overall runtime configuration for the base VERSPM, altering the Seed parameter
# NOTE: VERSPM-base does not define Seed - it will use the runtime or the VE default
print(vr$setting("Seed"))

viewSetup(fromFile=TRUE)
updateSetup(inFile=TRUE,Seed=2.3) # if inFile==FALSE, just update working set of parameters in memory
writeSetup(overwrite=TRUE)
viewSetup(fromFile=TRUE)

# To apply the new setting, we need to re-open the model
vr$reopen()
print(vr$setting("Seed"))

# Probably the Seed should be set in the model not the runtime, so do this:
updateSetup(vr,inFile=TRUE,Seed=1.5)
writeSetup(vr,overwrite=TRUE)
viewSetup(vr$reopen())
print(vr$setting("Seed"))

# stages
  # inspecting stages (in memory)
  # defining stages
    # implicitly as a model subfolder
    # explicitly in model visioneval.cnf
  # stage visioneval.cnf

# StartFrom stages
  # structure of configuration
  # options
    # stage meta-parameters
    # stage directory
    # stage visioneval.cnf (via Config)
    # stage inputs
    # stage results
    # stage descriptors

# Copying Datastores
  # Flattening
  # Converting DatastoreType
# LoadModel
  # Building stages across "models"
  # Using LoadModel for post-mortem debugging
# Extracting results
  # Basic extraction
  # Setting display_units
  # Filtering Group/Table/Name
  # Setting OutputDir (sub-directory of model's results)
# Querying results
  # Defining query specifications
    # Writing a file
    # Building in memory and saving
  # Running a query (set of specifications)
  # What comes up (summary of scenarios)
  # Using a different geography
# Using model stages to make scenarios
  # Adding a scenario stage to a model
    # StartFrom
    # InputDir
    # run_model.R
    # Years, Scenario, Description
  # Where do scenario results to?
  # How to extract or query scenario results