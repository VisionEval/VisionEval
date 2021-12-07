
# Run the models created by Create_Single_Scenarios.R script
# utilize Single_Scenarios_Status.csv file for model name and path

# read in csv which contains models to be run
csvpath <- file.path(ve.runtime,"models","VERSPM_Scenarios","Single_Scenarios_Status.csv")
scenarios <- read.csv(csvpath)

#Iterate through and run each model in the CSV
for(i in 1:nrow(scenarios)){
  
  name <- scenarios[i, "name"]
  modelpath <- scenarios[i,"location"]
  
  cat('Running', name, '\n\n')
  
  model <- openModel(modelpath)
  model$run()
  
  # Update status when complete
  scenarios[i, 'status'] <- model$overallStatus
  
}

# Update all model status
write.csv(scenarios, csvpath, row.names = F)

