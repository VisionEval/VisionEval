### structure.R
#   Tour the model directory structure
#   You really should walk through this one, rather than just run it...

require(VEModel)

# Tour the model structures in file explorer
shell.exec("models")

# inspecting model inputs (before the model is run)x
vrb <- openModel("VERSPM-base") # See install.R for installation

# the model's dir function will list different elements of the model
print( vrb$dir() ) # overview

# Let's look at (just) the inputs for VERSPM-base

# The list function for inputs shows metadata for each column of data
# You get identifiers of "what" (the group/table/name for the column) and
# "where" (stage/spec, where spec is Set, Get or Inp).

inputs <- vrb$list(inputs=TRUE)
print(inputs[1:10,])

# More often, we're interested in the files and locations for the input files
# Use the "details" parameter to add additional names

# What are the default metadata fields for model inputs?
print( names( vrb$list(inputs=TRUE) ) )

# What details are available (all the names of possible metadata fields)
print( names( vrb$list(inputs=TRUE,details=TRUE ) ) )

# Just list details associated with the file names
# In multi-stage models, it is important to consider the INPUTDIR; what
# you will 
details <- vrb$list(inputs=TRUE,details=c("FILE","INPUTDIR"))

# There will be one row of input for each column in an input file
# so there will be hundreds. Here's how to look at just a few.
print(details[1:10,])                     # only show 10
print(details[sample(nrow(details),10),]) # show a random selection of 10

# If you just want to see the unique required input files (without knowing
# their specific contents), you can do this:

required.files <- unique(file.path(details$INPUTDIR,details$FILE))
print(required.files[1:10]) # full paths, which may be very long

# Here's a hack for shortening model paths when you're displaying them
print(sub( getRuntimeDirectory(),"",required.files ))

# inspecting model stages (the STAGE is one of the list metadata fields)
# See "model-stages.R" for more details
# VERSPM-base model (one stage, new structure)
print(vrb)  # list of stage objects - only one in "base" model

# VERSPM-pop model (three stages, new structure)
vrs <- openModel("VERSPM-pop") # see Install.R for installation
print(vrs) # Three stages - see Stages.R below for stage walkthrough
