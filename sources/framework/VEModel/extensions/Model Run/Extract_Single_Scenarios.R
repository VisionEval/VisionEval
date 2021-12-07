# Extract metrics from all run models in Single_Scenario_Status.csv and save the results
library(dplyr)

# User Input
### Input future year as string
futureyear <- '2038'


# define function to extract metrics from each scenario folder
# Note that this has been set up to extract particular metrics from just the Marea and Households files, which was used in 
# an application for one particular project.
# Extensive customization is possible.

extract_scenario_metrics <- function(modelName, modelPath, Year = futureyear){
  # Will return an error if the model doesn't exist yet
  mod <- openModel(modelPath) 
  
  # Set groups to only the future year
  mod$groups <- Year
  
  # First extract Marea outputs
  mod$tables <- 'Marea'
  mod$fields <- c('UrbanHhDvmt',
                  'TownHhDvmt',
                  'RuralHhDvmt',
                  'HvyTrkUrbanDvmt',
                  'ComSvcUrbanDvmt',
                  'ComSvcTownDvmt',
                  'ComSvcRuralDvmt',
                  'LdvTotDelay',
                  'HvyTrkDelay',
                  'BusTotDelay',
                  'ComSvcUrbanGGE',
                  'ComSvcNonUrbanGGE',
                  'HvyTrkUrbanGGE',
                  'ComSvcUrbanKWH',
                  'ComSvcNonUrbanKWH',
                  'HvyTrkUrbanKWH',
                  'ComSvcUrbanCO2e',
                  'ComSvcNonUrbanCO2e',
                  'HvyTrkUrbanCO2e',
                  'BusGGE',
                  'RailGGE',
                  'VanGGE',
                  'BusKWH',
                  'RailKWH',
                  'VanKWH',
                  'BusCO2e',
                  'RailCO2e',
                  'VanCO2e'
  )
  # Review the selections:
  cat('Extracting \t', mod$groupsSelected, '\t',
      mod$tablesSelected, '\n', 
      paste(mod$fieldsSelected, collapse = '\t'))
  
  marea_results <- mod$extract(saveTo = F, quiet = T)
  
  # Household level
  # First clear the selections
  mod$tables <- ''
  mod$fields <- ''
  
  mod$tables <- 'Household'
  mod$fields <- c('Bzone',
                  'Income',
                  'OwnCost',
                  'WalkTrips',
                  'VehicleTrips',
                  'BikeTrips',
                  'TransitTrips',
                  'DailyGGE',
                  'DailyKWH',
                  'DailyCO2e')
  
  cat('Extracting \t', mod$groupsSelected, '\t',
      mod$tablesSelected, '\n', 
      paste(mod$fieldsSelected, collapse = '\t'))
  
  hh_results <- mod$extract(saveTo = F, quiet = T)
  
  # i switched it out for model path - how do i get the correct modelName from path
  
  # Save output as a list of two data frames: Marea and Household level
  results = list(Marea = data.frame(modelName, marea_results[[1]]),
                 Hh = data.frame(modelName, hh_results[[1]]))
  
  results
}

#### Looping through the run scenarios to extract the information

# read in csv 
csvpath <- file.path(ve.runtime, "models", "VERSPM_Scenarios", "Single_Scenarios_Status.csv")
scenarios <- read.csv(csvpath)
# View(scenarios) # in RStudio, allows viewing of the data frame after read in to R

marea_compiled <- vector()
hh_compiled <- vector()


# Iterate through model scenarios in CSV by using the location field to run the model
# Extract and compile information to Marea and Household Level CSV file
for(i in 1:nrow(scenarios)){
  
  # i = 1 is VERSPM_base_model
  modelname <- scenarios[i,"name"]
  cat("\n\n Extracting metrics from", modelname, '\n')
  modelpath <- scenarios[i,"location"]
  results_future <- extract_scenario_metrics(modelname, modelpath, futureyear)
  
  marea <- results_future[[1]] # Marea
  hh    <- results_future[[2]] # Household
  
  marea_compiled <- rbind(marea_compiled, marea)
  hh_compiled <- rbind(hh_compiled, hh)
  
}

write.csv(marea_compiled, file.path(ve.runtime, 'models', "VERSPM_Scenarios", 'Single_Scenarios_Metrics_Marea.csv'),
          row.names = F)
write.csv(hh_compiled,  file.path(ve.runtime, 'models', "VERSPM_Scenarios", 'Single_Scenarios_Metrics_Hh.csv'),
          row.names = F)

View(marea_compiled)
View(hh_compiled)


# Get units ----

# DatastoreListing
# This listing is maintained internally and contains the definitive list of what is in the
# Datastore.
# Use the first model for getting units
use_model_path = scenarios[1, 'location']

dfls <- get(load(file.path(use_model_path, "Datastore", "DatastoreListing.Rda")))

# DatastoreListing attributes
attr.names <- unique(unlist(sapply(dfls$attributes, names)))

attr2df <- function(atts) {
  td <- lapply(atts,function(x)lapply(x, paste, collapse=" | ")) # make attributes into strings (some are vectors)
  df <- data.frame(sapply(attr.names, function(x) character(0)))          # empty data.frame to hold results
  el <- lapply(attr.names, function(x) "")                                # create list of empty strings
  names(el) <- attr.names                                               # name the list after attr.names
  df.rows <- lapply(td, function(x) { nw <- el; nw[names(x)] <- x; nw })
  atts <- data.frame(bind_rows(df.rows))
  atts <- atts[which(atts$NAME != "" & atts$TABLE != "" & atts$GROUP != ""),]  # Keep only the datasets
  return(atts)
}

atts <- attr2df(dfls$attributes)

# Subset to the metrics we use

metric_units <- atts %>%
  filter(NAME %in% c(names(hh_compiled), names(marea_compiled))) %>%
  filter(TABLE %in% c('Marea', 'Household')) %>%
  select(MODULE, NAME, TABLE, TYPE, UNITS, DESCRIPTION)

write.csv(metric_units, file.path(ve.runtime, "models", "VERSPM_Scenarios","Extracted_Metric_Units.csv"),
          row.names = F)

# Save as RData for faster loading in R (compared to loading individual .csv files)

save(file = 'models/VERSPM_Scenarios/Single_Scenarios_Complete.RData',
     list = c('metric_units',
              'hh_compiled',
              'marea_compiled',
              'scenarios',
              'atts'))
