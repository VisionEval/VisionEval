# scenarios.R -- tour the scenario sample models and explain how to
# set up scenarios.

# In particular:
#   notion of a "Reportable" stage
#   notion of a "StartFrom" stage

# Extend the query discussion to include "Export" and the
# visualizer...

require(VEModel)

# Two approaches to scenarios are illustrated in sample versions of VERSPM

# "Category" Scenarios, where input adjustments are made by category, and
# when the model is loaded, model stages are constructed internally for all
# combinations of configured input categories.

scenarios.cat <- installModel("VERSPM",var="scenarios-cat")
scenarios.cat$dir(scenarios=TRUE,all.files=TRUE)
edit( file.path(scenarios.cat$modelPath,"scenarios/visioneval.cnf") )
