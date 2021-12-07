require(VEModel)

mini <- openModel("BARE")

###########################################
# RUNTIME PARAMETERS (GLOBAL, MODEL, STAGE)
###########################################

# examine model parameters
# default visioneval and VEModel parameters
# other parameters exist (e.g. a stage's RunPath) but they do not have defaults
viewSetup(Param_ls=visioneval::defaultVERunParameters())

# parameters defined in ve.runtime (initially, none)
viewSetup(fromFile=TRUE)

# parameters defined in model configuration
viewSetup(mini,fromFile=TRUE)

# all parameters
viewSetup(mini)

# parameters defined in stage configuration file
# note: there are additional parameters in the stage, even for a one-stage model
viewSetup(mini$modelStages[[1]])

# inspect parameters in configuration file versus loaded model
print(names(getSetup(mini,fromFile=TRUE))) # in the file
print(names(getSetup(mini)))               # all constructed parameters in memory

# easier to inspect constructed parameter with the model's "setting" function
mini$setting() # list the names

#######################################
# CHANGING MODEL PARAMETERS FROM R CODE
#######################################

# Let's change the overall runtime configuration for the mini model, altering the Seed parameter
# NOTE: mini does not define Seed - it will use the VE default

viewSetup(mini$modelStages[[1]]) # Runtime settings for mini model
mini$setting("Seed")
mini$setting("Seed",source=TRUE)

viewSetup(fromFile=TRUE)
updateSetup(Seed=2.3)                # update working set of parameters in memory
mini$configure(fromFile=FALSE)       # use updated parameter to rebuild model
viewSetup(fromFile=TRUE)             # No settings since file didn't exist
viewSetup()                          # Created Seed=2.3 in memory

mini$setting("Seed")
mini$setting("Seed",source=TRUE)     # source is "interactive"

updateSetup(inFile=TRUE,Seed=2.3)    # if inFile==TRUE, update file parameters in memory
viewSetup(fromFile=TRUE)             # Staged change - not yet in file

file.exists("visioneval.cnf")        # FALSE - not saved yet
writeSetup(overwrite=TRUE)           # Commit the file changes to the configuration file
file.exists("visioneval.cnf")        # TRUE - now saved
getSetup(reload=TRUE)                # reload global parameter file
viewSetup(fromFile=TRUE)

# To apply the new setting from the configuration file, we need to re-open the model
# Default is to revisit configuration file (see mini$configure(fromFile=FALSE) above
mini$configure()          
mini$setting("Seed")

# Now put the settings back the way they were

# The Seed parameter should probably be set in the model not globally, so do this:
updateSetup(mini,inFile=TRUE,Seed=1.5)  # Change the configuration
writeSetup(mini,overwrite=TRUE)         # Save it in the model's configuration file
mini$configure()                        # Reload the model from its configuration

print(mini$setting("Seed"))             # Show value of runtime setting
print(mini$setting("Seed",source=TRUE)) # show where the setting came from
viewSetup(mini$modelStages[[1]])

# Run the model again, saving the values one more time
mini$run("save")
mini$dir(results=TRUE,archive=TRUE)
