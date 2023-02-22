# See nearby Readme.md for instructions on the walkthrough
# Copy a few lines at a time into your R interpreter of choice to see what happens
# You can run this file non-interactively (e.g. via "source") but it won't print
# out all the interesting stuff...

###########################
# INITIAL WALKTHROUGH SETUP
###########################

# source("00-setup.R")
# Establishes VisionEval runtime environment for walkthrough
# Only needs to be run when setting up a new runtime
# If you do "walkthrough()" in the runtime, or do
# ve.test("walkthrough") it is called automatically

#########################
# INSTALL BUILT-IN MODELS
#########################

source("../01-install.R")
# Demonstrates installing built-in models that you can
# use as the basis for your own models

#########################
# RUN BUILT-IN MODELS
#########################

source("../02-running.R")   # Basic elements of running models

source("../03-extract.R")
# Shows how to extract raw results from a model into
# tabular form that you can analyze in another system
# (or continue to analyze in R)
#
# Expects "VERSPM-run" model to be present and "Run Complete"

source("../04-queries.R")       # Develop and run queries

source("../05-export.R")    # Move VisionEval results into another analysis system