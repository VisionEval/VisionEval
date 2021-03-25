# Test.R
# Comprehensively test VEModel and related interfaces
# Also provides working examples of the API

# function: pseudo_package
# 
if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}
if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}
if ( ! requireNamespace("jsonlite",quietly=TRUE) ) {
  stop("Missing required package: 'jsonlite'")
}
if ( ! requireNamespace("yaml",quietly=TRUE) ) {
  stop("Missing required package: 'yaml'")
}

setup <- function(ve.runtime=NULL) {
  # Creates or uses a fresh minimal runtime environment as a sub-directory of "tests"
  # Set VE_RUNTIME to some other location if desired (does not need to have a runtime
  # there yet, and in fact it's better if it doesn't).
  if ( ! is.character(ve.runtime) ) {
    ve.runtime <- Sys.getenv("VE_RUNTIME",unset=NA)
    if ( ! is.na(ve.runtime ) ) {
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- NA
      }
    }
    if ( is.na(ve.runtime) ) {
      ve.runtime <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)[1]
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="tests"),winslash="/",mustWork=FALSE)
        dir.create(ve.runtime)
      }
    }
  }
  ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=TRUE)
  Sys.setenv(VE_RUNTIME=ve.runtime)
  pkgload::load_all()
  ve.env <- VEModel::runtimeEnvironment()
  ve.env$ve.runtime <- ve.runtime; # override default from package load (working directory)
  setwd(ve.env$ve.runtime)

  if ( ! dir.exists("models") ) dir.create("models")
}

takedown <- function() {
  start.dir <- NA
  ve.runtime <- NA
  if ( isNamespaceLoaded("VEModel") ) {
    ve.env <- VEModel::runtimeEnvironment()
    if ( exists("ve.runtime",envir=ve.env,inherits=FALSE) ) {
      ve.runtime <- ve.env$ve.runtime
    }
    if ( exists("start.dir",envir=ve.env,inherits=FALSE) ) {
      start.dir <- ve.env$start.dir
    }
  }
  if ( "package:VEModel" %in% search() ) detach("package:VEModel")
  unloadNamespace("VEModel")
  if ( ! is.na(start.dir) ) setwd(start.dir)
  if ( ! is.na(ve.runtime) ) {
    message("To remove runtime directory:")
    message("unlink('",ve.runtime,"',recursive=TRUE)")
  }
  loadhistory(".Rhistory") # get rid of rep("n",a.million.times) and other debugging leftovers
}

rewind <- function() {
  cat("Rewinding...")
  takedown()
  setup()
}

cleanup <- function() {
  takedown()
  runtimes <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)
  message("Removing:")
  print(runtimes)
  if ( length(runtimes)>0 && isTRUE(askYesNo("Remove runtimes?")) ) unlink(runtimes,recursive=TRUE)
}

testStep <- function(msg) {
  cat("",paste(msg,collapse="\n"),"",sep="\n")
}

test_run <- function(modelName="JRSPM",log="warn") {
  testStep("Install and Run a Full Model")
  modelPath <- file.path("models",modelName)
  owd <- getwd()
  tryCatch(
    {
      testStep("Clearing previous model, if any")
      if ( dir.exists(modelPath) ) {
        message("Clearing runtime environment")
        unlink(modelPath,recursive=TRUE)
      }
      testStep(paste("Installing VERSPM model from package as",modelName))
      rs <- installModel("VERSPM",modelName,log=log,confirm=FALSE)
      testStep("Running model...")
      rs$run(log=log)
      return(rs)
    },
    error=function(e) { cat("Runtime error:\n",conditionMessage(e),"\n"); takedown(); stop(e) },
    finally=setwd(owd)
  )
  return("Failed to run.")
}

test_model <- function(oldstyle=TRUE, test.copy=FALSE, log="warn") {
  testStep("Model Management Functions")

  if ( test.copy ) {
    testStep("open a model")
    jr <- openModel("JRSPM")

    testStep("model directory original")
    print(jr$dir())

    testStep("copy a model")
    cp <- jr$copy("CRSPM")
    cp$clear(force=TRUE)

    testStep("model directory copy")
    print(cp)
    print(cp$dir())

    testStep("remove model copy")
    unlink("models/CRSPM",recursive=TRUE)
  }

  testStep("construct a bare model from scratch")
  bare.dir <- file.path("models","BARE")
  base.dir <- file.path("models","JRSPM")
  if ( dir.exists(bare.dir) ) unlink(bare.dir,recursive=TRUE)
  dir.create(bare.dir)

  testStep("Create minimal run_model.R")

  runModelFile <- file.path(bare.dir,"run_model.R")
  runModel_vc <- c(
    'library(visioneval)',
    'initializeModel()',
    'for(Year in getYears()) {',
    'runModule("CreateHouseholds","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictWorkers","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    '}'
  )
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  testStep("Set up model directory structure.")

  # Borrow model geography, units, deflators from VERSPM
  base.defs <- file.path(base.dir,"defs")
  base.inputs <- file.path(base.dir,"inputs")
  bare.defs <- file.path(bare.dir,"defs")
  bare.inputs <- file.path(bare.dir,"inputs")
  dir.create(bare.defs)
  dir.create(bare.inputs)
  print(dir(bare.dir,recursive=TRUE,full.names=TRUE,include.dirs=TRUE))

  testStep(paste0("Create configuration: ",if (oldstyle) "defs/run_parameters.json" else "visioneval.cnf"))

  # Create run_model.R script (two variants)
  # Create model-specific configuration
  # Could equivalently place these in run_parameters.json
  runConfig_ls <-  list(
      Model       = jsonlite::unbox("BARE Model Test"),
      Scenario    = jsonlite::unbox("Test"),
      Description = jsonlite::unbox("Minimal model constructed programmatically"),
      Region      = jsonlite::unbox("RVMPO"),
      BaseYear    = jsonlite::unbox("2010"),
      Years       = c("2010", "2038")
    )

  if ( oldstyle ) {
    cat("Old style model setup (defs/run_parameters.json)")
    configFile <- file.path(bare.defs,"run_parameters.json")
    write(jsonlite::toJSON(runConfig_ls, pretty=TRUE),configFile)
    print(bare.defs)
    print(configFile)
    cat(readLines(configFile),sep="\n")
  } else {
    configFile <- file.path(bare.dir,"visioneval.cnf")
    yaml::write_yaml(runConfig_ls,configRile)
    print(bare.dir)
    print(configFile)
    cat(readLines(configFile),sep="\n")
  }

  testStep("Copy other configuration files (geo, units, deflators)")

  from <- file.path(base.defs,c("units.csv","deflators.csv","geo.csv"))
  file.copy(from=from,to=bare.defs)
  print(bare.defs)
  print(dir(bare.defs,full.names=TRUE))

  testStep("Copy basic input files (model_parameters.json could also live in 'defs')")

  # Inputs for CreateHouseholds and PredictWorkers
  from <- file.path(base.defs,"model_parameters.json")
  file.copy(from=from,to=bare.inputs)

  from <- file.path( base.inputs,c(
    "azone_hh_pop_by_age.csv",
    "azone_hhsize_targets.csv",
    "azone_gq_pop_by_age.csv"
  ) )
  file.copy(from=from, to=file.path(bare.inputs) )
  print(bare.inputs)
  print(dir(bare.inputs,full.names=TRUE))

  testStep("Open BARE model using defaults...")
  bare <<- openModel("BARE",log="info")

  testStep("List model inputs...")

  print(bare$list(inputs=TRUE))

  testStep("run the bare model")
  bare$run()
  
  testStep("directory of the bare model: results")
  print(bare$dir(results=TRUE))

  testStep("list all fields in bare model")
  print(bare$list())

  testStep("extract model results")
  br <- bare$results()
  br$extract(prefix="BareTest")

  testStep("clear the bare model")
  print(bare$dir(output=TRUE))
  bare$clear(force=TRUE)

  testStep("model after clearing outputs...")
  print(bare$dir())

  testStep("clear results as well...")
  bare$clear(force=TRUE,outputOnly=FALSE) # default is FALSE if no outputs exist - delete results
  print(bare$dir())

  testStep("remove model")

  unlink(bare.dir,recursive=TRUE)
  bare$dir() # reports an empty character vector
}

test_results <- function (log="warn") {
  testStep("Manipulate Model Results in Detail")

  testStep("Opening model and pulling out results and selection...")
  jr <- openModel("JRSPM")
  rs <- jr$results()
  sl <- rs$select() # Get full field list
  # Check error: No results or selection if the model has not been run.

  # Test display units, select speeds, create unit conversion
  testStep("Creating and Writing Display Units...")
  un <- rs$list(details=TRUE)[,c("Group","Table","Name","Units")]
  spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
  spd$DisplayUnits <- "MI/HR"
  cat("Writing display_units.csv into ")
  display_units_file <- file.path(
      jr$modelPath,
      visioneval::getRunParameter("ParamDir",Param_ls=jr$RunParam_ls),
      visioneval::getRunParameter("DisplayUnitsFile",Param_ls=jr$RunParam_ls)
    )
  cat(display_units_file,"\n")
  write.csv(spd,file=display_units_file)

  testStep("Selecting speed fields...")
  sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
  print(sl$fields())

  testStep("Showing currently defined UNITS/DISPLAYUNITS")
  print(rs$units())

  # Clean up the fields to add the geography fields in the Marea Table
  testStep("Adding geography fields to selection...")
  sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
  print(sl$fields())
  print(rs$units())

  testStep("Extracting speed fields using DISPLAY units")
  sl$extract(prefix="DisplayUnits")                 # Using DISPLAY units

  testStep("Exporting speed fields using DATASTORE units")
  sl$export(prefix="Datastore",convertUnits=FALSE) # Using DATASTORE units

  testStep("Model directory")
  print(jr$dir())

  testStep("Model directory of results")
  print(jr$dir(results=TRUE))
  
  testStep("Model directory of outputs")
  print(jr$dir(outputs=TRUE))
  
  testStep("Clear outputs")
  jr$clear(outputOnly=TRUE, force=FALSE)

  testStep("Directory after clearing")
  jr$dir()
}

test_query <- function(log="warn") {
  # Process the standard query list for the test model
  testStep("Set up Queries and Run on Model Results")
  testStep("Opening test model and caching its results...")
  jr <- openModel("JRSPM")
  rs <- jr$results()

  testStep("Show query directory (may be empty)...")
  jr$query()

  testStep("Show model directory for queries")

  testStep("Create an empty query object and print it...")
  # create a query object
  qry <- jr$query("Test-Query")
  cat("Query valid:",qry$valid(),"\n")
  cat("Print qry$checkResults:"); print(qry$checkResults)
  cat("Print query\n")
  print(qry)

  testStep("Add a query specification formulated as a list element...")
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

  testStep("Names of specifications in added query...")
  print(qry$names())    # List names of QuerySpecifications in order
  testStep("Print function for added queries...")
  print(qry)

  testStep("Re-add a query at the beginning of the list")
  print(qry)
  spec <- VEQuerySpec$new(spec)
  spec <- spec$update(Name="UrbanHhDvmt_before")
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
  qry$add(spec,location=0)  # should put loc0 "after" first element: 2nd position
  cat("loc0 goes after first element\n")
  print(qry)

  testStep("Remove test queries...")
  cat("Removing:\n")
  print( nm <- qry$names()[1:3] )
  qry <- qry$remove(nm) # remove by name (bye-bye before,loc2 and loc0)
  print(qry)
  cat("Removing:\n")
  print(c("2",qry$names()[2]))
  qry <- qry$remove(2) # remove by position (bye-bye loc45)
  print(qry)

  testStep("Construct bare query...")
  spec <- VEQuerySpec$new()
  cat("Bare query is valid (FALSE): ")
  print(spec$valid())   # Should return FALSE
  print(spec)

  testStep("Add spec details to bare query using $update...")
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

  testStep("Add updated spec to Query and print...")
  qry$add(spec)
  print(qry)

  testStep("Print again with details...")
  print(qry,details=TRUE)

  testStep("Print query with details...")
  print(qry,details=TRUE)

  testStep("Complete the initial query by adding two more 'Summarize' specs...")

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
  qry$add(spec)
  
  testStep("Create a 'Function' query specification...")

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

  testStep("Add the Function spec to the query...")

  qry$add(spec)
  qry$print()

  testStep("Save the query and fix its extension...")

  qry$save()
  cat("Saved values in original query...\n")
  cat("Directory: "); print(qry$QueryDir)
  cat("Name; "); print(qry$QueryName)
  cat("Path: "); print(qry$QueryFile)

  testStep("Save a copy of the query and fix its extension...")

  qry$save("Copy-Query.R") # Essentially as "Save As"
  cat("Saved values in renamed query...\n")
  cat("Directory: "); print(qry$QueryDir)
  cat("Name; "); print(qry$QueryName)
  cat("Path: "); print(qry$QueryFile)

  testStep("Model QueryDir contents...")

  cat("Expecting "); print(c("Copy-Query.VEqry","Test-Query.VEqry"))
  jr$query()

  testStep("Open the query in a different object from the file...")

  runqry <- VEquery$new(QueryName="Test-Query")
  cat("Loaded query...\n")
  cat("Directory: "); print(runqry$QueryDir)
  cat("Name; "); print(runqry$QueryName)
  cat("Path: "); print(runqry$QueryFile)
  print(runqry)

  runqry <- VEquery$new(QueryName="Test-Query.VEQry")
  cat("Re-Loaded query with name extension...\n")
  cat("Directory: "); print(runqry$QueryDir)
  cat("Name; "); print(runqry$QueryName)
  cat("Path: "); print(runqry$QueryFile)
  print(runqry)

  testStep("Run the query on the model...")
  qry$run(jr)

  testStep("Run the query on the results...")
  qry$run(rs)

  # Throw some additional specific broken queries at it to see if errors are correct.
}

# Now set it all up
rewind()
