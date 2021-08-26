require(VEModel)
mwr <- openModel("VERSPM-run") # Run Install.R to install/run that model

##########################
# EXTRACTING MODEL RESULTS
##########################

# Dump all of a model's results:
results <- mwr$results()
if ( is.list(results) ) { # TRUE if there are multiple Reportable stages
  results <- results[[1]]  # or select a different stage by name or index
}
print(results)
# Result extraction must be done stage-by-stage

# If you want to extract from the whole model, you can flatten the Datastore (see below)
#   into a new directory. but be careful if you have a bunch of stages that fill up a 2050
#   future year group with variants of the same data; you'll only get the data from the
#   final one.

# Queries, on the other hand, work just fine with modelStages as we'll see below.

# Here's the basic extraction of everything
results$extract()
mwr$dir(output=TRUE)
mwr$dir(output=TRUE,all.files=TRUE)

# See what is selected
print(results)
sl <- results$select() # Get full field list
print(
  head(
    capture.output( print(sl) ),
    n=12
  )
)

#######################################
# SELECTING GROUPS, TABLES AND DATASETS
#######################################

# Do some basic field extraction - list fields
print(sl$groups())
print(sl$tables())
fld <- sl$fields()
print(fld[sample(length(fld),20)])

# Select some subsets by group, table or field name...
# Can we easily identify group names, table names, field names and zero in on selecting them?
sl$select( sl$find(Group="Years") )
print(sl$groups())
sl$select( sl$find(Group=sl$groups()[1]) )         # Just the first ones
print(sl$groups())

sl$select( sl$find(Group=sl$groups()[1],Table=c("Household","Vehicle")) )
print(sl)
print(head(capture.output(print(sl,details=TRUE)),n=12))

sl$extract()
results$extract() # WARNING: Uses "sl" selection
sl$all() # deselect everything

########################
# CHANGING DISPLAY UNITS
########################

# Programatically set up the display_units file with a useful conversion
un <- results$list(details=TRUE)[,c("Group","Table","Name","Units")]
spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
spd$DisplayUnits <- "MI/HR"
print(spd)

# Put the display_units file in a useful place (model 'defs' directory)
display_units_file <- file.path(
  mwr$modelPath,
  mwr$setting("ParamDir"),
  mwr$setting("DisplayUnitsFile") # defaults to 'display_units.csv'
)
cat(display_units_file,"\n")
write.csv(spd,file=display_units_file)

# Select speed fields...
sl$all() # re-select everything
sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
print(sl)

# Showing currently defined UNITS/DISPLAYUNITS (via sl$results)
print(sl$results$units())

# Showing currently defined UNITS/DISPLAYUNITS (directly from results)
print(results$units())

# Add the geography fields in the Marea Table
sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
print(sl$fields())
print(results$units())

# Extracting speed fields using DISPLAY units
sl$extract(prefix="DisplayUnits")                 # Using DISPLAY units

# Extract speed fields using DATASTORE units
sl$export(prefix="Datastore",convertUnits=FALSE)  # Using DATASTORE units

shell.exec(results$resultsPath)
# In general, it is better to use queries to do the extraction and unit conversion...

################################
# FLATTEN OR CONVERT A DATASTORE
################################

# to do this right with a staged model, you need to iterate over the
# "Reportable" stages and copy their results one by one - you'll end up
# with multiple copies of the preceding stages.

test.dir <- file.path(getRuntimeDirectory(),"Test-Results-Copy")
if ( dir.exists(test.dir) ) unlink(test.dir,recursive=TRUE)
dir.create(test.dir)
results$copy(test.dir,Flatten=c(TRUE,TRUE)) # Force use of "Flatten" even if result is already flat
tr <- openResults(test.dir)                 # Any directory with ModelState.Rda and Datastore can be opened
print(tr)

# results$copy(test.dir,Flatten=FALSE) # exact copy of just this stage (not previous stages)
# results$copy(test.dir,DatastoreType="H5") # convert result to H5 Datastore
# NOTE: it is not clear that the H5 DatastoreType still works at all

#############################
# CLEARING OUTPUTS (EXTRACTS)
#############################

print(mwr$dir())
print(mwr$dir(outputs=TRUE))

mwr$clear(outputOnly=TRUE, force=FALSE) # interactive clearing of outputs

