### 03A-advanced-extract.R
#   Advanced examples of retrieving raw data from a finished model run
#   You don't need to run thi unless you're curious about some advanced
#   use of the export functions:
#   1. Setting display_units to adjust units of certain fields in exported data
#   2. Accessing exported data in various other formats than CSV or SQLite (include Excel)
#   3. Output partitioning (recombining tables by Scenario or Year or other fields)
#   4. Using alternate DBI databases (e.g. a "real" SQL database)

####################
# SAMPLE MODEL SETUP
####################

require(VEModel) # just in case it's not already out there...
mwr <- openModel("VERSPM-run") # Run Install.R to install/run that model
results <- mwr$run()
print(results)

########################
# CHANGING DISPLAY UNITS
########################

# You can create a file called "display_units.csv" and put it in your
# model's "defs" folder. Then the fields listed there can have their
# units automatically converted when you extract them.

# Here, we'll set up the display_units file with a useful conversion
# This shows how to construct a DisplayUnitsFile
# Practically speaking, you can do this once in your life and drop it
# into your model's "defs" directory.
# You don't have to do it like this: you can also build it by hand
results$select()$all()
un <- results$list(details=TRUE)[,c("Group","Table","Name","Units")]
spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
spd$DisplayUnits <- "MI/HR"
print(spd)

# Put the display_units file in a useful place (model 'defs' folder)
# This file will be automatically used during export!
display_units_file <- file.path(
  mwr$modelPath,           # folder for model, inside the "models" folder
  mwr$setting("ParamDir"), # "defs" by default
  mwr$setting("DisplayUnitsFile") # defaults to 'display_units.csv'
)
cat(display_units_file,"\n")
write.csv(spd,file=display_units_file,row.names=FALSE)

# Select speed fields...
# "select=TRUE" changes the selection within "results" (rather than creating an
# independent selection - then we can just continue to use "results". Do
# "results$select()$all()" (see above) to reset to everything.
selected <- results$select()$find(pattern="speed",Group="Year",Table="Marea",select=TRUE)

# Add the key fields
selected <- selected$addkeys() # Forces tables to have basic geography fields plus "Id"
print(results$units())

# Extract the speed fields to lists of data.frames
# Using DISPLAY units (convertUnits=TRUE is the default if "display_units.csv" exists)
convertedvalues <- results$extract(selection=selected,convertUnits=TRUE)  # Using Display Units
# Using DATASTORE units
rawvalues <- results$extract(selection=selected,convertUnits=FALSE)  # Using DATASTORE units
udnames <- names(convertedvalues[[1]]) # The data.frame fields may be in a random order...
print(convertedvalues[[1]][,udnames])
print(rawvalues[[1]][,udnames])

# Once you create "display_units.csv", it is "set and forget" for standard exports
# Every future export will do unit conversions by default for those named fields
# unless you say "convertUnits=FALSE" when you export.
defaultvalues <- results$extract(selection=selected)
print(defaultvalues[[1]][,udnames]) # same as "convertedvalues"

#########################
# ACCESSING EXPORTED DATA
#########################

# Export to a different Database name and save the exporter for further investigation
# (Timestamp goes on the file name)
results$select()$all()
exporter <- results$export("sqlite",connection=list(Database="My-SQLite-Database"))
exporter$list()

# See the exported SQLite databases
print(mwr$dir(output=TRUE,all.files=TRUE))

# Read the data back from the export ("extracts" the exported results)
all.the.data <- exporter$data()       # Returns a list of data.frames by reading back what you exported
print(class(all.the.data[[1]]))       # bare data.frame
all.the.data.tables <- exporter$data(format="data.table") # If you would rather work with data.tables
print(class(all.the.data.tables[[1]])) # data.table is a superset of data.frame
all.the.tibbles <- exporter$data(format=tibble::tibble)   # Any (package::)function that knows about data.frames will work
print(class(all.the.tibbles[[1]]))    # tibbles ("tbl") are also a superset of data.frame
rm( all.the.data, all.the.data.tables, all.the.tibbles)

# If you have a function that knows about lists of data.frames, you can use that too:
# additional arguments sent to "data" are passed to the formatting function.
# The exportPath function returns the absolute path to the model's Output Directory
# formatList=TRUE passes a single list of data.frames to the format function
# formatList=FALSE passes each data.frame one at a time to the formatter
# Which to use depends on the formatting function
if ( ! require("writexl") ) {
  install.packages("writexl") # goes into .libPaths()[1], which is probably ve-lib
}
# This dumps EVERYTHING, so the Excel workbook is pretty huge (~150Mb)
# though "data" by default returns a huge list of data.frames, if you use
# a formatter function, it returns whatever comes back from the function.
# Using writexl::write_xlsx returns the full path of the created workbook
excel.workbook.name <- results$export()$data(
  formatList=TRUE,
  format=writexl::write_xlsx,
  path=file.path(mwr$exportPath(),"My-Excel-Data.xlsx")
)
mwr$dir(output=TRUE,all.files=TRUE)

unlink(excel.workbook.name) # Removes the big file (skip this step to review later)

# The "extract" function also understands the "format" options, so you
# can open, run and extract your model in a single line of code like this:
excel.workbook.name <- openModel("VERSPM-run")$run()$extract(
  formatList=TRUE,
  format=writexl::write_xlsx,
  path=file.path(mwr$exportPath(),"All-in-one-Excel.xlsx")
)

# If you're exporting to one of the built-in formatters, the "export" function is more direct:
# Here is how to run VisionEval scenarios and retrieve results in a single line of code
exporter <- openModel("VERSPM-run")$run()$export("sqlite") # into SQLite

# And you can still move the exported data around.
# This line copies everything from the SQLite database we just created into Excel
exporter$data(
  formatList=TRUE,
  format=writexl::write_xlsx,
  path=file.path(mwr$exportPath(),"Back-from-SQLite.xlsx")
)

rm(exporter) # will close, eventually, the SQLite database; run gc() if still appears to be "in use"

mwr$dir(output=TRUE,all.files=TRUE)

############################
# PARTITIONING EXPORTED DATA
############################

# The default export creates one table per scenario (model stage) per group (Global,
# Year) per table type (Household, Worker, Vehicle)

# You can "partition" exported data differently depending on your analysis needs. Data can
# be partitioned into folders (what the CSV export does), or the partition can be written
# into the table names (which is the default for SQLite)

# To make a partition, you just list a field in the output data (usually a geography, or
# the scenario year, or the scenario name) and explain whether you want the partitioned
# table identified by a "folder" or coded in the table "name". Data from any source with
# the same partition ends up in the same table (so you can accumulate every year of every
# scenario in a single "Household" table, but keep the Global group tables with the same
# name separate).

# Here are some examples of partitioning:

# Clear ALL the outputs in a hurry so we can start fresh:
mwr$clear(force=TRUE)

# This partition will put all the scenarios and years into single tables (the Global
# group is always kept separate, using a "folder" by default)
partition = c(Global="folder")
exporter <- mwr$results()$export(partition=partition)
# Yields a single CSV file for each table with all the Years and Scenarios together
# The Global group will end up in a sub-folder.

# show the output files
mwr$dir(output=TRUE,all.files=TRUE)

# Show all the table names exported
print(exporter$list())
# Show what's in each of the tables (metadata) - generates a LOT of output
elist <- exporter$list(namesOnly=FALSE)
print(names(elist)) # same as default exporter$list()
print(elist[[1]])   # Each item in the list contains a data.frame of metadata for each table


# partitioning fields as "folders" become prefixed "names" in SQL:
results <- mwr$results()
sqlexporter <- results$export("sql",partition=c(Global="folder"),connection=(Database="All-in-one-Tables.sqlite"))
print(sqlexporter$list())
# show fields in one table out of the database and notice that "Year" is a field...
hhtable <- sqlexporter$list()[[11]]
print(hhtable) # should be the Household table, with a timestamp
dflist <- sqlexporter$data(hhtable) # List of tables (hhtable can be a character vector); in this case, a list of one
str(dflist) # Notice that "Year" is a field
print( unique(dflist[[1]][["Year"]]) ) # Should show "2010" and "2038"

#################################
# CONNECTING TO A "REAL" DATABASE
#################################

# You can use any DBI connection for export to databases like MySQL, MariaDB, SQL Server,
# or PostgreSQL (or through ODBC connections, though those are messier to set up).
#   ODBC will get you Microsoft Access or an alternate route to writing Excel
# Read the R documentation for DBI drivers (https://dbi.r-dbi.org)
# Your setup will look like the following:

mariadb <- list(
  driver = "sql", # send into DBI driver
  package = "RMariaDB",
  install.package = TRUE, # try to install the driver package if not already present
  drv = "RMariaDB::MariaDB()", # or drv=RMariaDB::MariaDB() if you know the package is already installed
  Timestamp = "prefix",
  DBIConfig = list(
    # items here are anything you would pass to dbConnect for that database
    # database must already exist and be accessible for writing with these credentials
    dbname = "visioneval", # Adjust for local database name / user / password
    user = "visioneval",
    password = "showme"
  )
)

# Then just do this (You need a working database!!!)
# Note that this will just keep piling a model's worth of tables into the database
# It does not remove or overwrite anything
# Tables with the same name are distinguished by their initial timestamp
# You'll need to use your own database utility to clean them up
# If you're wanting to use MySQL or MariaDB (or some other "real" SQL), you should already know how to do that.
exporter <- mwr$results()$export(connection=mariadb)
exporter$list() # Only shows the tables created this time

# You can set up some or all of the connection in your model's visioneval.cnf
# (or in the runtime visioneval.cnf for "all model" defaults. You can either
# create a new exporter name, or redefine defaults for an existing one.

# This block in visioneval.cnf makes "mysql" use your local database (uncomment it!)
# FYI the MariaDB driver will also work for branded MySQL databases
# Probably don't need install.package flag since you will presumably already have installed it
#  but there's no harm including "install.package: true" here as well

# Exporters:
#   mysql:
#     Connection:
#       driver: sql
#       package = RMariaDB,       # will require the package, which must be installed
#       drv = RMariaDB::MariaDB() # character string will be parsed
#       Timestamp: prefix         # Timestamp each table at the beginning of its name
#       DBIConfig:
#         dbname: visioneval      # Adjust for local database name / user / password
#         user: visioneval
#         password: showme
#     Partition:                  # Do it however you like
#         Global: path
#         Year: name              # Puts all scenarios into one table per Year
#
# Then when you do the following in R, it will use your database:
#    mwr$run()$export("mysql")
