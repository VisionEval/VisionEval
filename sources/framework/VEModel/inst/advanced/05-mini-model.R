require(VEModel)

# This code shows how to assemble a model stage programmatically

# Remember: a model stage is the "mimimum runnable unit" of a VE Model.
# It assembles definitions, inputs and a script to generate some or
# all of a Datastore. A VEModel consists of at least one model stage.

# In this walkthrough we'll look first at how stages are built and
# manipulated. Then we'll look at how to use them make your models
# simpler, better organized, easier to maintain, and faster to run.

visioneval::initLog(Save=FALSE,Threshold="warn")
# level 'info' shows more steps
# Save=FALSE does not create a copy of the log to a file: just show
# on console.

# Set up a base model (so we can copy its parts rather than build
# them from scratch).

if ( ! ( baseModel <- openModel("VERSPM-base") )$valid() ) {
  baseModel <- installModel("VERSPM",var="base")
}
# baseModel$run() -- we're just going to pilfer files; no need to run...

# Clear out any models left over from walking through previously

message("Cleaning up previous mini models.")
models.dir <- file.path(getwd(),"models")
obsolete <- dir(models.dir,pattern="^mini",ignore.case=TRUE,full.names=TRUE)
for ( oo in obsolete ) {
  if ( dir.exists(oo) ) unlink(oo,recursive=TRUE)
}

# Set up the basic model structure

mini.dir <- file.path(models.dir,"MINI")
message("Making MINI model in ",mini.dir)

mini.script <- file.path(mini.dir,visioneval::getRunParameter("ScriptsDir"))
mini.inputs <- file.path(mini.dir,visioneval::getRunParameter("InputDir"))
mini.defs   <- file.path(mini.dir,visioneval::getRunParameter("ParamDir"))
dir.create(mini.dir)            # Model directory
dir.create(mini.script)         # Scripts directory
dir.create(mini.inputs)         # Inputs directory
dir.create(mini.defs)           # Defs directory
print( dir(mini.dir,full.names=TRUE) ) # Show everything using R dir function

# Set up the model configuration (visioneval.cnf)
# Can also put it in "defs/run_parameters.json", like the old days

message("Create the model configuration / run parameters")
runConfig_ls <-  list(
    Model       = "Mini Model Test",
    Scenario    = "MiniModel",
    Description = "Minimal model constructed programmatically",
    Region      = "RVMPO",
    State       = "OR",
    BaseYear    = "2010",
    Years       = c("2010")
  )
viewSetup(Param_ls=runConfig_ls)  # Helper function to display a configuration

# Create the model's visioneval.cnf (in the model directory)
# We'll write it as YAML, but it can also be JSON

configFile <- file.path(mini.dir,"visioneval.cnf")
cat(configFile,"\n")
yaml::write_yaml(runConfig_ls,configFile)

# Set up a script file; we'll write this one from scratch
# Each model stage can have its own script (some or all of a "total
# model"), which lets you do things like use different modules in
# different scenarios (e.g. with different estimation of powertrain
# and fuel proportions).

# The walkthrough does this in R, but you could just use a text editor

runModelFile <- file.path(mini.script,"run_model.R")
runModel_vc <- c(
  '', # Don't ask why (it's an R thing): without this blank line the script gets written wrong...
  'for(Year in getYears()) {',
  'runModule("CreateHouseholds","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
  'runModule("PredictWorkers","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
  '}'
)
cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
writeLines(runModel_vc,con=runModelFile)

# Likewise with the defs - we're just borrowing the base VERSPM model
# defs and inputs for the relevant modules

# Locate the defs in the base model using its settings
base.defs <- baseModel$setting("ParamPath",shorten=FALSE)
from <- file.path(
  base.defs,c(
    "units.csv",
    "deflators.csv",
    "geo.csv"
  )
)

# Copy it to the mini model
file.copy(from=from,to=mini.defs)
print(mini.defs)
print(dir(mini.defs,full.names=TRUE))

# Now (even without inputs) the mini model can be opened
# At a minimum, it needs scripts and defs
mini <- openModel(basename(mini.dir))
print(mini) # Still has no input files

# Now let's use VEModel functions to tell us what the necessary inputs are.

# Note that this strategy won't work if the baseModel has a complex InputPath
#   (files in stage and scenario folders or otherwise distributed)
# In that case, you probably want to use "insider information" to copy the files using
#   File Explorer or an equivalent tool

# Locate the base model inputs directory
base.inputs <- unique(baseModel$dir(inputs=TRUE,shorten=FALSE))

# Get the list of inputs required in the mini script we set up earlier
inputs <- mini$list(inputs=TRUE,details=c("FILE"))

# Always need model_parameters.json

# Note that "model_parameters.json" does not meet the formal definition of an "input" (i.e.
# something explicitly used by modules in their specifications).
required.files <- unique(file.path(base.inputs,c(inputs[,"FILE"],"model_parameters.json")))
required.files <- required.files[which(file.exists(required.files))]

message("Copying required files...")
file.copy(from=required.files,to=mini.inputs)

# Re-open the mini model, applying the configuration changes
# You need to do this explicitly (or implicitly by using openModel again) every time
# you change a model configuration, inputs or defs (either in memory or in the file system)

mini$configure()

# list the inputs again showing that INPUTDIR is now the mini model
# inputs (previously NA)
inputs <- mini$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
print(inputs)

# List what is actually in the model's inputs directory using its own
# "dir" function. Notice that the model directory is shown first;
# other files are relative to that directory.
print(mini$dir(inputs=TRUE,all.files=TRUE))

# And again. Without shortening the file names are in a form ready to
# copy, open, etc.
print(mini$dir(inputs=TRUE,all.files=TRUE,shorten=FALSE))

# The following steps just make for a nicer display...

required.files <- file.path(ifelse(is.na(inputs$INPUTDIR),"",inputs$INPUTDIR),inputs$FILE)
required.exists <- file.exists(required.files)
required.files <- sub( runtimeEnvironment()$ve.runtime, "", required.files )
required.files <- data.frame(EXIST=ifelse(is.na(inputs$INPUTDIR),FALSE,required.exists),FILE=required.files)

# The required Files (all should EXIST)
print(unique(required.files))

# Now let's see what we can do with the mini model
# We'll re-do some of the things from the walkthrough's "structure.R"

# Tour the mini model structure in File Explorer
shell.exec(mini$modelPath)

# Tour the model using VEModel exploration functions
mini$dir()              # List the summary contents of the model
mini$dir(all.files=TRUE) # List all the files

# We did this above, but now we can see it all
mini$dir(inputs=TRUE)   # List just the model input directories
mini$dir(inputs=TRUE,all.files=TRUE) # List all the input files...

# Run the model
mini$run("reset")    # throw away existing results and re-run

# Show what is there after the run
mini$dir()           # notice presence of results directory
mini$dir(results=TRUE,all.files=TRUE) # Notice it doesn't show the "inside" of the Datastore
mini$dir(results=TRUE,all.files=TRUE,shorten=FALSE)

# Re-run the model but save its results to an archive directory
mini$run("save")     # move existing results into an archive and re-run

mini$dir()           # notice presence of archive directory
mini$dir(archive=TRUE) # call out just the archive directories
mini$dir(archive=TRUE,all.files=TRUE) # List all the files in the archive directories
mini$dir(archive=TRUE,all.files=TRUE,shorten=FALSE)
# Notice that the archive DOES show the contents of the archived Datastore 

# Re-run the model but only do stages that have not run.
# We'll use this again in "model-stages.R" and "scenarios.R"
mini$run("continue") # re-run any stage that is not "Run Complete" - does nothing here
mini$run()           # same as vr$run("continue"); does nothing here
mini$run("reset")
mini$dir()           # archive is still present after reset
