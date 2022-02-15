### structure.R
#   Tour the model directory structure
#   You really should walk through this one, rather than just run it...

require(VEModel)

# Tour the model structures in file explorer
shell.exec("models")

# inspecting model inputs
vrb <- openModel("VERSPM-base") # See install.R for installation

# Let's look at (just) the inputs for VERSPM-base

# We're adding the Input directory and File to the standard table, which usually just lists the
# Group/Table/Name plus metadata like Units.
inputs <- vrb$list(inputs=TRUE,details=c("FILE","INPUTDIR"))

# There will be one row of input for each column in an input file
# so that will give hundreds. Here's how to look at just a few:

print(inputs[1:10,])                    # only show 10
print(inputs[sample(nrow(inputs),10),]) # show a random selection of 10

# If you just want to see the unique required input files:
required.files <- unique(file.path(inputs$INPUTDIR,inputs$FILE))
print(required.files[1:10]) # full paths, which may be very long

# Here's a hack for shortening model paths:
print(sub( getRuntimeDirectory(),"",required.files ))

# inspecting model stages
# VERSPM-base model (one stage, new structure)
print(vrb)  # list of stage objects - only one in "base" model

# VERSPM-pop model (three stages, new structure)
vrs <- openModel("VERSPM-pop") # see Install.R for installation
print(vrs) # Three stages - see Stages.R below for stage walkthrough
