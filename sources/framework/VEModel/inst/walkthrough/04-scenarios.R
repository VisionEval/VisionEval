# 04-scenarios.R
# Tour the scenario sample models and explain how to set up scenarios
#   and query the scenario results

require(VEModel)

# VisionEval runs "model stages", which are separate scripts and
# inputs for a particular portion of a model. The "pop" model breaks
# the model apart into seveal pieces.

# Scenarios are created just by defining a new model stage that
# starts from an earlier stage (often the unaltered future year)

# Setting up scenarios is beyond the scope of this document, but you
# can find out more at https://visioneval.org/docs.

# Here, we'll just show you that scenarios are just additional
# "model stages", which are often the future year stage all over
# again with some of the inputs adjusted.

# We'll run the scenario model here so you can see how queries are used
# in the next walkthrough file, 05-queries.R

mod.scenarios <- installModel("VERSPM",var="scenarios-ms",modelPath="VERSPM-scenarios",confirm=FALSE)

mod.scenarios$dir(scenarios=TRUE,all.files=TRUE) # Look at the elements of the setup

# Run the model
mod.scenarios$run()

# Check that everything ran correctly
print(mod.scenarios)

# Details can be interesting
print(mod.scenarios,details=TRUE)

# Once the mod.scenarios model is run, move on to the 05-queries
# walkthrough to find out how to use queries and extract scenario
# results.
