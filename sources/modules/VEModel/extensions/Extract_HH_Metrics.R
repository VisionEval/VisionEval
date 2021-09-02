# Extract metrics from a Portland Metro VERSPM model and save the results
# Assumptions: 
# 1. You have completed one or more VERSPM models
# 2. These models are in the `models` directory of your VisionEval installation
# 3. You launched VisionEval.Rproj to open this session in RStudio
# 4. You want to extract a select set of household level metrics 

# Setup ----
library(dplyr)   # install.packages(dplyr)
library(stringr) # install.packages(stringr)

# User Input ----
# Input the name of the completed VERSPM model

modelName <- 'VERSPM'  # could be RefCase or similar


# Extract ----
# Define a function to extract metrics from each model folder

extract_scenario_metrics <- function(modelName){
  # Will return an error if the model doesn't exist yet
  mod <- openModel(modelName) 
  
  # Household level
  # First clear the selections
  mod$tables <- ''
  mod$fields <- ''
  
  mod$tables <- 'Household'
  mod$fields <- c('Bzone',
                  'Dvmt',
                  'DailyCO2e'
                  
                  # Other options include:
                  # 'Income',
                  # 'OwnCost',
                  # 'WalkTrips',
                  # 'VehicleTrips',
                  # 'BikeTrips',
                  # 'TransitTrips',
                  # 'DailyKWH',
                  # Daily GGE
                  )
  
  cat('Extracting \t', mod$groupsSelected, '\t',
      mod$tablesSelected, '\n', 
      paste(mod$fieldsSelected, collapse = '\t'))
  
  hh_results <- mod$extract(saveTo = F, quiet = T)
  
  hh_results
}



results <- extract_scenario_metrics(modelName)

# This looks for a four-digit number in the names of the results tables, e.g. _2010_, and returns just the year 2010
years_run = str_match(names(results), '(?:_)(\\d{4})(?:_)')[,2]

hh_compiled <- vector() # make one big data frame for all years

# Also have individual year data frames, e.g. hh_2010

for(y in years_run){
  
  assign(paste0('hh_', y), results[[grep(y, names(results))]]) 
  
  hh_compiled <- rbind(hh_compiled, data.frame(Year = y, get(paste0('hh_', y))))
  
  # Save to csv as well
  output_path = file.path(ve.runtime, 'models', modelName, 'output')
  if(!dir.exists(output_path)){ dir.create(output_path)}
  
  write.csv(get(paste0('hh_', y)),
            file.path(output_path, paste0('Household_Metrics_', y, '.csv')),
            row.names = F)
  
}


# Now you have one big data frame with all the years (this could be super big, one row per household per year)
View(hh_compiled)

# To extract metrics for just a subset of Bzones, now you can join in a table with two columns:
# Bzone | CityofPortland
# Then you can subset the whole hh_compiled data frame for just those Bzones which are in the City of Portland

# You also have a data frame for each year
# View(hh_2010) # if you have a 2010 year

# Get units ----

# DatastoreListing
# This listing is maintained internally and contains the definitive list of what is in the Datastore.

dfls <- get(load(file.path('models', modelName, "Datastore", "DatastoreListing.Rda")))

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

# Subset to the metrics we used

metric_units <- atts %>%
  filter(NAME %in% c(names(hh_compiled))) %>%
  filter(TABLE %in% c('Household')) %>%
  select(NAME, TABLE, TYPE, UNITS, DESCRIPTION, MODULE) %>%
  filter(!duplicated(NAME))


write.csv(metric_units, file.path(output_path, "Extracted_Metric_Units.csv"),
          row.names = F)
