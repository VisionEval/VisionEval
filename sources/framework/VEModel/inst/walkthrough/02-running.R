### 02-running.R
#   Walk through running models

# Load VEModel package (in effect, the visioneval environment)
require(VEModel)

# Assuming you've done install.R and have some models around

# Only need the following if you're in a new R session after doing 01-install.R
mod <- openModel("VERSPM-run")
print(mod) # Should say "Run complete"

mod$run() # basic instruction to run the model
# but it does nothing here since "mod" has already run

mod$run("reset") # Throws away the results and runs it all again

# Moves the results into an archive folder then reruns everything
mod$run("save")

# explicit version of the default action:
#   try re-running everything that is not already "Run Complete"
# Use this if you've added a stage or scenario just to run the new stuff
mod$run("continue")

# let's look at a multi-stage model
# Install if need be:
#  mod.pop <- installModel("VERSPM",var="pop")
mod.pop <- openModel("VERSPM-pop")

mod.pop$run() # just run it - one stage at a time

# Continue with 03-extract.R to learn about the parts of a VisionEval model
# See scenarios.R for more information on model stages and scenarios