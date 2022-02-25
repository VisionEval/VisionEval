# See nearby Readme.md for instructions on the walkthrough
# Copy a few lines at a time into your R interpreter of choice to see what happens
# You can run this file non-interactively (e.g. via "source") but it won't print
# out all the interesting stuff...

###########################
# INITIAL WALKTHROUGH SETUP
###########################

source("setup.R")
# Establishes VisionEval runtime environment for walkthrough
# Only needs to be run when setting up a new runtime

#########################
# INSTALL BUILT-IN MODELS
#########################

source("../install.R")
# Demonstrates installing built-in models that you can
# use as the basis for your own models

#########################
# INSTALL BUILT-IN MODELS
#########################

source("../running.R")   # Basic elements of running models

################################
# FIRST VIEW OF MODEL STRUCTURES
################################

source("../structure.R")
# Shows some basic elements of the model structures
# Also see "run-parameters.R" below for understanding
# how VisionEval and its models are configured.

##########################
# EXTRACTING MODEL RESULTS
##########################

source("../extract.R")
# Shows how to extract raw results from a model into
# tabular form that you can analyze in another system
# (or continue to analyze in R)
#
# Expects "VERSPM-run" model to be present and "Run Complete"

##########################
# ADDING STAGES TO A MODEL
##########################

source("../mini-model.R")    # Create a minimal model to illustrate
source("../model-stages.R")  # Explore model stages in detail

####################################
# BUILDING AND RUNNING MODEL QUERIES
####################################

source("../queries.R")       # Develop and run queries

#################
# USING SCENARIOS
#################

source("../scenarios.R")     # Use model stages as scenarios

################
# MODEL SETTINGS
################

source("../run-parameters.R") # Explore visioneval.cnf and parameters programmatically

##########################
# DEBUGGING A FAILED MODEL
##########################

source("../debugging.R")      # How to work with partial model runs
# Illustrating how to copy a Datastore from an earlier model into a new one
# Applications include post-mortem debugging and pre-building a Datastore to
#   test a module under development.
