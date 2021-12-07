require(VEModel)

mini <- openModel("BARE")

# Add stages to the mini model
mini$modelStages # list the one stage constructed from the root folder

# Recreate the mini model with two stages, for base and future years
if ( file.exists( file.path("models","BARE-Stages") ) ) {
  unlink("models/BARE-stages",recursive=TRUE)
}
mini.1 <- mini$copy("BARE-Stages",copyResults=FALSE)

# Overall model configuration identifies the stages
modelParam_ls <- list(
  Model       = "Implicit Staged Mini Model Test",
  Region      = "RVMPO",
  State       = "OR",
  BaseYear    = "2010",
  ModelStages = list(
    "BaseYear" = list (
      Dir = "BaseYear",
      Reportable = TRUE
    ), # Dir need not match name of stage
    "FutureYear" = list(
      Dir = "FutureYear",
      Reportable = TRUE
    )
  )
)
yaml::write_yaml(modelParam_ls,file.path(mini.1$modelPath,"visioneval.cnf"))

# First stage definition
dir.create( stage1.dir <- file.path(mini.1$modelPath,"BaseYear") )
StageParam_ls <- list(
  Scenario    = "MiniModel Base Year",
  Description = "Minimal model base year, constructed programatically",
  Years       = c("2010")
)
yaml::write_yaml(StageParam_ls,file.path(stage1.dir,"visioneval.cnf"))

# Second stage definition
dir.create( stage2.dir <- file.path(mini.1$modelPath,"FutureYear") )
StageParam_ls <- list(
  Scenario    = "MiniModel Future Year",
  Description = "Minimal model future year, constructed programatically",
  Years       = c("2038"),
  StartFrom   = "BaseYear" # match name of earlier stage in ModelStages (see below)
  # Standard VERSPM future year requires access to BaseYear results.
)
yaml::write_yaml(StageParam_ls,file.path(stage2.dir,"visioneval.cnf"))

# Reload the updated model configuration with stages
mini.1$configure()

# Let's see what we've got
print(mini.1)

mini.1$run("reset")
mini.1$dir()

# Finally, let's add a stage to mini.1 with a different scenario
# Note that this stage is added IN MEMORY ONLY, but we can still run the model
# The use case is for doing a quick test as we are here, and also for
#  programatically injecting a new scenario into a base model based on
#  combinations of inputs, where we are only interested in the model results

mini.1$addstage(
  Name="AltFuture",
  stageParam_ls=list(
    Dir="AltFuture"
  ),
  Scenario="Mini-Model Alternative Future",
  Description="Scramble an input file",
  StartFrom="BaseYear",
  BaseYear = mini.1$setting("BaseYear"),
  Years=mini.1$setting("Years",stage="FutureYear")
)
# add stage automatically "configures" the model

# Even though the stage definition is in memory, the specific inputs for
#  the alternative scenario are maintained in a directory.

dir.create( stage3.dir   <- file.path(mini.1$modelPath,"AltFuture") )
dir.create( stage3.input <- file.path( stage3.dir,mini.1$setting("InputDir") ) )

# Create an alternate future input (extra 3% on hh_pop distributions)
# For a real "in memory" stage, this scenario file would exist somewhere else, and we would
#   locate it by setting an InputPath parameter saying where to find an InputDir with the
#   updated files in it.
mini.files <- mini.1$list(reset=TRUE,inputs=TRUE,details=c("FILE","INPUTDIR"))
mini.files <- unique(file.path(mini.files$INPUTDIR,mini.files$FILE))
scenario.input <- grep(
  "hh_pop",
  mini.files,
  value=TRUE
)
adj.input <- read.csv(scenario.input)
adj.names <- grep("^Age",names(adj.input),value=TRUE)
adj.input[adj.input$Year=="2038", adj.names] <- adj.input[adj.input$Year=="2038", adj.names] * 1.03
write.csv(adj.input,row.names=FALSE,file=file.path(stage3.input,basename(scenario.input)))

# Finally, run the new stage

mini.1$run() # Watch closely: will only run the new stage!
mini.1$run() # just reports each stage status

mini.1$plan("multisession") # Run stages in parallel
mini.1$run("save") # Move existing results aside to timestamped results folder and re-run
mini.1$run("reset") # Blow away existing results and re-run the model
