require(VEModel)
vrb <- openModel("VERSPM-run")

# Clean up leftovers from previous walkthrough
qfiles <- grep("Full-Query",vrb$query(),invert=TRUE,value=TRUE)
qfiles <- file.path(vrb$modelPath,vrb$setting("QueryDir"),qfiles)
unlink(qfiles)

vrb$clear(force=TRUE,outputOnly=TRUE) # blow away previous extractions or queries

message("Show query directory (has Full-Query.VEqry)...")
print(vrb$query())

#######################
# BASIC QUERY OPERATION
#######################

qry <- vrb$query("Full-Query")
qry$run(vrb,Geography="Marea",GeoValue="RVMPO")
vrb$dir(output=TRUE,all.files=TRUE)
shell.exec(file.path(vrb$modelPath,vrb$dir(output=TRUE,all.files=TRUE)[1]))

##################################
# CREATING QUERIES PROGRAMATICALLY
##################################

message("Create an empty query object and print it...")
qry <- vrb$query("Test-Query",load=FALSE) # Don't open it if file exists already
cat("Query valid:",qry$valid(),"\n")
cat("Print qry$checkResults:"); print(qry$checkResults)
cat("Print query\n")
print(qry)

message("Add a query specification formulated as a list element...")
spec <- list(
  Name = "UrbanHhDvmt",
  Summarize = list(
    Expr = "sum(UrbanHhDvmt)",
    Units = c(
      UrbanHhDvmt = "MI/DAY",
      Marea = ""
    ),
    By = "Marea",
    Table = "Marea"
  ),
  Units = "Miles per day",
  Description = "Daily vehicle miles traveled by households residing in the urban area"
)
qry$add(spec)
qry$print(details=TRUE)

print(qry$names())    # List names of QuerySpecifications in order
print(qry)

message("Re-add a query at the beginning of the list")
print(qry)
spec <- VEQuerySpec$new(spec)
spec <- spec$update(Name="UrbanHhDvmt_before")
cat("Adding spec:\n")
print(spec)
qry$add(spec,before=TRUE) # Should be placed at location=1 (first element); existing list after
cat("Before goes at beginning\n")
print(qry)

spec <- VEQuerySpec$new(spec)
spec <- spec$update(Name="UrbanHhDvmt_loc2")
qry$add(spec,location=2,before=TRUE) # should put loc2 in between "before" and original
cat("loc2 goes between 'before' and original\n")
print(qry)

spec <- VEQuerySpec$new(spec)
spec <- spec$update(Name="UrbanHhDvmt_loc45")
qry$add(spec,location=45) # should put loc45 at the end
cat("loc45 goes at end\n")
print(qry)

spec <- VEQuerySpec$new(spec)
spec <- spec$update(Name="UrbanHhDvmt_loc0")
qry$add(spec,location=2,before=TRUE)  # should put loc0 "after" first element: 2nd position
cat("loc0 goes after first element\n")
print(qry)

message("Remove test specifications...")
cat("Removing:\n")
print( nm <- qry$names()[1:3] )
qry$remove(nm) # remove by name (bye-bye before,loc2 and loc0)
print(qry)
cat("Removing:\n")
print(c("2",qry$names()[2]))
qry$remove(2) # remove by position (bye-bye loc45)
print(qry)

message("Construct bare query...")
spec <- VEQuerySpec$new()
cat("Bare query is valid (FALSE): ")
print(spec$valid())   # Should return FALSE
print(spec)

message("Add spec details to bare query using $update...")
spec$update(
  Name = "UrbanHhDvmt_MixNbrhd",
  Description = "Daily vehicle miles traveled by households residing in mixed use in the urban area",
  Units = "Miles per day", # Purely advisory...
  Summarize = list(
    Expr = "sum(Dvmt[LocType == 'Urban' & IsUrbanMixNbrhd == '1'])",
    Units = c(
      Dvmt = "MI/DAY",        # Will force to this unit, with conversion if needed
      LocType = "",           # Leaving it blank says use Datastore default
      IsUrbanMixNbrhd = "",
      Marea = ""
    ),
    By = "Marea",
    Table = "Household"
  )
)
cat("Updated query is valid (TRUE): ")
print(spec$valid())   # Should return TRUE
print(spec)

message("Add updated spec to Query and print...")
qry$add(spec)
print(qry)

message("Print query with details...")
print(qry,details=TRUE)

message("Complete the initial query by adding two more 'Summarize' specs...")

spec <- list(
  list(
    Name = "UrbanVanDvmt",
    Summarize = list(
      Expr = "sum(VanDvmt)",
      Units = c(
        VanDvmt = "MI/DAY",
        Marea = ""
      ),
      By = "Marea",
      Table = "Marea"
    ),
    Units = "Miles per day",
    Description = "Daily vehicle miles traveled by on-demand transit vans in the Urban area."
  ),
  list(
    Name = "UrbanComSvcDvmt",
    Summarize = list(
      Expr = "sum(ComSvcUrbanDvmt)",
      Units = c(
        ComSvcUrbanDvmt = "MI/DAY",
        Marea = ""
      ),
      By = "Marea",
      Table = "Marea"
    ),
    Units = "Miles per day",
    Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the urban area"
  )
)
print(qry)
qry$add(spec,location=1,after=TRUE)
print(qry)

message("Create a 'Function' query specification...")

spec <- VEQuerySpec$new()
spec$update(QuerySpec=list(
    Name = "UrbanLdvDvmt",
    Function = "UrbanHhDvmt + UrbanVanDvmt + UrbanComSvcDvmt",
    Units = "Miles per day",
    Description = paste0("Sum of daily vehicle miles traveled by households residing in the urban area,\n",
    "commercial service travel attributable to the demand of urban area households and businesses,\n",
    "and on-demand transit van travel in the urban area.")
  )
)
cat("Function spec is valid (TRUE):"); print(spec$valid())
print(spec)

message("Add the Function spec to the query...")

print(qry)
qry$add(spec)
print(qry)

####################
# MANAGE QUERY FILES
####################

message("Clear test queries, if any, but keep Full-Query")
qfiles <- grep("Full-Query",vrb$query(),invert=TRUE,value=TRUE)
qfiles <- file.path(vrb$modelPath,"queries",qfiles)
print(qfiles)
unlink(qfiles)
print(vrb$query())

message("Save the query and fix its extension...")
qry$save() # as Test-Query.VEqry
cat("Saved values in original query...\n")
cat("Name; "); print(qry$QuerydName)
cat("Path: "); print(qry$QueryFile)
cat("Directory: "); print(qry$QueryDir)
print(dir(qry$QueryDir))
print(vrb$query())

message("Save a copy of the query and fix its extension...")
qry2 <- qry$copy("Copy-Query.R") # .R will be removed from the name
qry2$save() # Essentially as "Save As"
cat("Saved values in renamed query...\n")
cat("Directory: "); print(qry2$QueryDir)
cat("Name; "); print(qry2$QueryName)
cat("Path: "); print(qry2$QueryFile)
cat("Contents of copied query...\n")
print(qry2)

message("Model QueryDir contents...")

cat("Expecting "); print(c("Copy-Query.VEqry","Full-Query.VEqry","Test-Query.VEqry"))
print(vrb$query())

message("Save a query somewhere else...")
qry2$save(qfile <- file.path(vrb$modelPath,"queries","Dump-Query.R"))
print(vrb$query())

message("Save a query without overwriting...")
actualFile <- qry2$save(overwrite=FALSE)
qfile <- c(qfile,actualFile)
print(vrb$query())
unlink(qfile); rm(qry2)

message("Open the query by short name in a different object from the file...")

runqry <- vrb$query("Test-Query")
cat("Loaded query...\n")
cat("Directory: "); print(runqry$QueryDir)
cat("Name; "); print(runqry$QueryName)
cat("Path: "); print(runqry$QueryFile)
print(runqry)

message("Open the query again from the file, using full file name...")

runqry <- vrb$query("Test-Query.VEQry")
cat("Re-Loaded query with name extension...\n")
cat("Directory: "); print(runqry$QueryDir)
cat("Name; "); print(runqry$QueryName)
cat("Path: "); print(runqry$QueryFile)
print(runqry)
rm(runqry)

#############
# RUN QUERIES
#############

message("Run the query on the model...")
qry$run(vrb,OutputRoot=vrb$modelResults)

message("Run the query on the results...")
rs <- vrb$results()
qry$run(rs,OutputRoot=vrb$modelResults)

message("Run the full query on the model...")
qry <- vrb$query("Full-Query")
# Automatically attaches model to query

# Runn
qry$run()     # uses attached model - will give error if no model attached
# Query results are added to each stage results
print(vrb$dir(results=TRUE,all.files=TRUE))

# Running again does as little work as possible
qry$run(vrb)  # re-attach the model; does nothing if query is up to date
  # Will re-run for new model stages are present, or if the stage
  # results had been re-generated

# Can always force a complete re-do
qry$run(Force=TRUE) # Ignore existing query results and run again

# If you forget what model is attached to the query:
print(qry$Model) # Shows vrb...

# Explicitly set the query model (without running)
qry$model(vrb)

# Extract query results from attached model
# Makes a data.frame from the results files created for each model stage
# Work in Progress: filter by geography type or specific geography
# value, filter by Scenario Year, filter by list of measure names
df <- qry$extract()
print(df) # rows are meaasures; columns are model stages/scenarios

# Export query results to a CSV file
qry$export(format="csv") # Default CSV file name in output directory
vrb$dir(outputs=TRUE)

qry$export() # Does the same thing again, possibly overwriting (file is timestamped)
vrb$dir(outputs=TRUE)

qry$export(format="csv",SaveTo=paste0("TestQuery_%timestamp%",qry$Name)) # use our own file name template
vrb$dir(outputs=TRUE)
