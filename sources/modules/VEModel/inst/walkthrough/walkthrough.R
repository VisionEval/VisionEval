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

# Tour the model structures in file explorer
shell.exec("models")

# inspecting model inputs
vrb <- openModel("VERSPM-base") # See Install.R for installation

inputs <- vrb$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
print(inputs[1:10,])
required.files <- unique(file.path(inputs$INPUTDIR,inputs$FILE))
print(required.files[1:10]) # full paths

# Hack for shortening model paths:
print(sub( getRuntimeDirectory(),"",required.files ))

# inspecting model stages
# VERSPM-base model (one stage, new structure)
print(vrb)  # list of stage objects - only one in "base" model

# VERSPM-pop model (three stages, new structure)
vrs <- openModel("VERSPM-pop") # see Install.R for installation
print(vrs) # Three stages - see Stages.R below for stage walkthrough

##########################
# EXTRACTING MODEL RESULTS
##########################

source("../extract.R")
# Expects "VERSPM-run" to be present and "Run Complete"

############################################
# BUILD AND RUN A NEXT-GENERATION MINI-MODEL
############################################

source("../mini-model.R")

#########################################
# MANIPULATE MODEL CONFIGURATIONS USING R
#########################################

source("../run-parameters.R")

##########################
# ADDING STAGES TO A MODEL
##########################

source("../model-stages.R")

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
