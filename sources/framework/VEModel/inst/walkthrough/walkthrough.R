# See nearby Readme.md for instructions on the walkthrough
# Copy a few lines at a time into your R interpreter of choice to see what happens
# You can run this file non-interactively (e.g. via "source") but it won't print
# out all the interesting stuff...

###########################
# INITIAL WALKTHROUGH SETUP
###########################

source("setup.R")
# Establishes VisionEval runtime environment for walkthrough

#########################
# INSTALL BUILT-IN MODELS
#########################

source("../install.R")
# Demonstrates installing built-in models, runs one of them, caches results

################################
# FIRST VIEW OF MODEL STRUCTURES
################################

source("../structure.R")

##########################
# EXTRACTING MODEL RESULTS
##########################

source("../extract.R")
# Expects "VERSPM-run" model to be present and "Run Complete"

##########################
# ADDING STAGES TO A MODEL
##########################

source("../model-stages.R")

#################
# USING SCENARIOS
#################

source("../scenarios.R")

####################################
# BUILDING AND RUNNING MODEL QUERIES
####################################

source("../queries.R")

#######################
# LOADING ANOTHER MODEL
#######################

source("../load-model.R")
# Illustrating how to copy a Datastore from an earlier model into a new one
# Applications include post-mortem debugging and pre-building a Datastore to
#   test a module under development.
