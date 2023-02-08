# 05-queries.R
# More detailed walkthrough of using queries to summarize scenarios

require(VEModel)

# Analyzing multiple scenarios is usually easiest to perform if you run queries - extracting results
# directly is designed to do just one scenario at a time, so you'll have to loop manually (or in
# your own script) over all the scenarios (queries do that automatically). Plus, extracting a large
# model's results will generate hundreds of megabytes of output data (perhaps even gigabytes...). A
# query lets you zero in on results you want to see, without having to sift through large amounts of
# data, summarizing the results by region or individual geography. More detailed documentation on
# queries is available at https://visioneval.org/docs.

# Make sure the scenario model is installed and run
# These first steps just 
mod.scenarios <- if ( "VERSPM-scenarios" %in% dir("models") ) {
  openModel("VERSPM-scenarios")
} else {
  installModel("VERSPM",var="scenarios-ms",modelPath="VERSPM-scenarios",confirm=FALSE)
}
mod.scenarios$run()       # will do nothing if you just ran the model
print(mod.scenarios,details=TRUE) # without 'details' just says how many scenarios...

#######################
# BASIC QUERY OPERATION
#######################

message("Show query directory (has VERSPM-scenarios.VEqry)...")
print(mod.scenarios$query())

# You can open the VERSPM-scenarios.VEqry file in a text editor to see what it looks like
# and to read some basic documentation.

# A query is just an R script that defines a list of query specifications.
# Each specification describes one resulting metric to evaluate for each scenario (model stage).

# Open the query and run it on the scenario results
qry <- mod.scenarios$query("VERSPM-scenarios") # open the query
qry$run() # do the query on mod.scenarios model

# No "outputs" (extracted query results) yet
mod.scenarios$dir(output=TRUE) # No "outputs" yet (just "results")

# But we do have query results (notice Query_VERSPM-scenarios.Rda for each scenario)
mod.scenarios$dir(results=TRUE,all.files=TRUE)

# Extracting query outputs into an R data.frame
q.df <- qry$extract()

# Here are the columns (the scenarios)
print(names(q.df))

# Here are the rows (metrics evaluated for each scenario)
print(q.df$Measure)

# Here's a random measure and scenario
# Some basic R voodoo
print(
  q.df[
    sample(nrow(q.df),1),         # Pick one index number from among the rows
    c(1,sample(ncol(q.df)-3,1)+3) # Pick one index number from among scenarios, keep measure name)
  ]
)

# Still no outputs - extract produces a data.frame for further processing
mod.scenarios$dir(output=TRUE)

# Export (instead of, or in addition to, extract) to generate a file
# Currently, only .csv is supported. But you can save earlier q.df in any
# file format that supports tables and has an R package - see below

qry$export(format="csv") # Default CSV file name in output directory
mod.scenarios$dir(outputs=TRUE,all.files=TRUE) # Now we have output in a .csv file

#################################
# WRITING TO AN EXTERNAL DATABASE
#################################

# You can also write the extract data.frame into an external database
# See 03-extract.R for how to export raw model results to a database

require(RSQLite)  # you may need to install RSQLite and its dependencies

# Open DBI connection
dbPath <- file.path(mod.scenarios$modelResults,mod.scenarios$setting("OutputDir"))
if ( ! dir.exists(dbPath) ) dir.create(dbPath,recursive=TRUE)

db.name <- paste0("VERSPM-scenarios_Database.sqlite")

db.connection <- file.path(dbPath,db.name)

mydb <- DBI::dbConnect(RSQLite::SQLite(), db.connection)

# Make a table name for the query results
table.name <- paste0("Query_",format(Sys.time(),"%Y_%m_%d-%H_%M"))

# We could have used q.df from above, but we'll just regenerate it
dbWriteTable( mydb, table.name , qry$extract(), overwrite=TRUE )

# Finally, clean up the database connection
DBI::dbDisconnect(mydb)

# See that there is now an SQLite database
mod.scenarios$dir(outputs=TRUE,all.files=TRUE)
