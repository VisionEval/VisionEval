# See nearby Readme.md for instructions on the walkthrough
# Copy a few lines at a time into your R interpreter of choice to see what happens
# You can run this file non-interactively (e.g. via "source") but it won't print
# out all the interesting stuff...

###########################
# INITIAL WALKTHROUGH SETUP
###########################

source("00-setup.R")
# Establishes VisionEval runtime environment for walkthrough
# Only needs to be run when setting up a new runtime

#########################
# INSTALL BUILT-IN MODELS
#########################

source("../01-install.R")
# Demonstrates installing built-in models that you can
# use as the basis for your own models

#########################
# INSTALL BUILT-IN MODELS
#########################

source("../02-running.R")   # Basic elements of running models

################################
# FIRST VIEW OF MODEL STRUCTURES
################################

source("../03-structure.R")
# Shows some basic elements of the model structures
# Also see "run-parameters.R" below for understanding
# how VisionEval and its models are configured.

##########################
# EXTRACTING MODEL RESULTS
##########################

source("../04-extract.R")
# Shows how to extract raw results from a model into
# tabular form that you can analyze in another system
# (or continue to analyze in R)
#
# Expects "VERSPM-run" model to be present and "Run Complete"

##########################
# ADDING STAGES TO A MODEL
##########################

source("../05-mini-model.R")    # Create a minimal model to illustrate model structure
source("../06-model-stages.R")  # Explore model stages in detail

####################################
# BUILDING AND RUNNING MODEL QUERIES
####################################

source("../07-queries.R")       # Develop and run queries

#################
# USING SCENARIOS
#################

source("../08-scenarios.R")     # Use model stages as scenarios

################
# MODEL SETTINGS
################

source("../09-run-parameters.R") # Explore visioneval.cnf and parameters programmatically

##########################
# DEBUGGING A FAILED MODEL
##########################

source("../10-debugging.R")      # How to work with partial model runs
# Illustrating how to copy a Datastore from an earlier model into a new one
# Applications include post-mortem debugging and pre-building a Datastore to
#   test a module under development.
