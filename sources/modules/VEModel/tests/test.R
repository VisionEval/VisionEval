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

# Check that we can source run_model.R to run a classic model
test_classic <- function(modelName="VERSPM-Classic",clear=TRUE,log="info") {
  modelPath <- file.path("models",modelName)
  owd <- getwd()
  on.exit(setwd(owd))

  if ( dir.exists(modelPath) && clear ) {
    testStep("Clearing model to start from scratch")
    unlink(modelPath,recursive=TRUE)
  } else {
    testStep("Re-running existing installation")
  }

  if ( ! dir.exists(modelPath) ) {
    testStep(paste("Installing classic VERSPM model from package as",modelName))
    rs <- installModel("VERSPM",variant="classic",modelName,log=log,confirm=FALSE)
    modelName <- rs$modelName
    rm(rs)  # Don't keep the VEModel around
  }

  testStep(paste("Running",modelName,"by sourcing scripts/run_model.R"))

  setwd(modelPath)
  require(visioneval) # Put it on the search path for GetYears, RunModule, etc
  source("run_model.R")
  detach("package:visioneval") # But leave the namespace loaded

  testStep("Reviewing model status")

  setwd(owd)
  rs <- openModel(modelName)
  cat("Model Status:",rs$printStatus(),"\n")
  return(rs)
}
  
test_install <- function(modelName="VERSPM",variant="base",installAs=NULL,run=FALSE,log="warn") {

  if ( ! nzchar(variant) ) variant <- ""
  if ( missing(installAs) || is.null(installAs) ) {
    if ( nzchar(variant) && nzchar(modelName) ) {
      installAs <- paste0("test-",modelName,"-",variant)
    }
  }

  if ( nzchar(installAs) ) {
    if ( dir.exists(file.path("models",installAs)) ) {
      testStep(paste0("Clearing previous installation at ",installAs))
      unlink(file.path("models",installAs), recursive=TRUE)
    }

    testStep(paste("Installing",modelName,"model from package as",installAs))
    rs <- installModel("VERSPM",installAs,variant,log=log,confirm=FALSE)
  } else {
    if ( nzchar(modelName) ) {
      testStep(paste0("Directory of available variants for ",modelName))
    } else {
      testStep("Directory of available models")
    }
    rs <- installModel(modelName,"")
  }
  if ( ! "VEModel" %in% class(rs) ) {
    # It's not a model, so it is probably a diagnostic showing available models or variants
    if ( is.character(rs) ) cat(paste(rs,collapse="\n")) else print(rs)
  }
  return(rs)
}

test_run <- function(modelName="TEST-RUN",baseModel="VERSPM",variant="base",reset=FALSE,log="warn") {
  if ( ! reset ) {
    testStep(paste("Attempting to re-open existing",modelName))
    rs <- openModel(modelName)
    if ( rs$status != "Complete" ) {
      reset <- TRUE
      message("Rebuilding model")
    } else {
      message("Using existing model run")
      return(rs)
    }
  }
  if (reset) {
    testStep("Install and Run a Full Model")
    modelPath <- file.path("models",modelName)
    owd <- getwd()
    tryCatch(
      {
        if ( dir.exists(modelPath) ) {
          testStep("Clearing runtime environment")
          unlink(modelPath,recursive=TRUE)
        }
        testStep(paste("Installing",baseModel,"model from package as",modelName))
        rs <- installModel(baseModel,modelName,variant=variant,log=log,confirm=FALSE)

        testStep("Running model...")
        rs$run(run="reset",log=log) # clears results directory
        return(rs)
      },
      error=function(e) { cat("Runtime error:\n",conditionMessage(e),"\nRemember to takedown()\n") },
      finally=setwd(owd)
    )
    return("Failed to run.")
  }
}

test_model <- function(oldstyle=TRUE, test.copy=FALSE, log="info") {

  cat("*** Test Model Management Functions ***\n")
  options(warn=2) # Make warnings into errors...

  testStep("open (and maybe run) the full test version of VERSPM")
  jr <- openModel("JRSPM")
  if ( ! jr$status=="Complete" ) {
    cat("Re-running model due to status",jr$status,"\n")
    jr <- test_run(modelName="JRSPM",log=log)
  }
  if (! "VEModel" %in% class(jr) ) {
    return(jr)
  } else print(jr)

  testStep("gather base model parameters")
  base.dir <- jr$modelPath
  cat("Base Model directory:\n")
  print(base.dir)

  jrParam_ls <- jr$RunParam_ls
  base.defs <- normalizePath(
    file.path(
      base.dir,
      visioneval::getRunParameter("ParamDir",Param_ls = jrParam_ls)
    ),winslash="/",mustWork=TRUE
  )
  cat("Base defs:\n")
  print(base.defs)

  base.inputs <- normalizePath(
    file.path(
      base.dir,
      visioneval::getRunParameter("InputPath",Param_ls = jrParam_ls),
      visioneval::getRunParameter("InputDir",Param_ls = jrParam_ls)
    ),winslash="/",mustWork=TRUE
  )
  cat("Base inputs:\n")
  print(base.inputs)

  testStep("construct a bare model from scratch")
  bare.dir <- file.path("models","BARE")
  if ( dir.exists(bare.dir) ) {
    cat("Blowing away existing bare model.\n")
    unlink(bare.dir,recursive=TRUE)
  }
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
  bare.defs <- file.path(bare.dir,"defs")
  bare.inputs <- file.path(bare.dir,"inputs")
  dir.create(bare.defs)
  dir.create(bare.inputs)
  print(dir(bare.dir,recursive=TRUE,full.names=TRUE,include.dirs=TRUE))

  testStep(paste0("Create configuration: ",if (oldstyle) "defs/run_parameters.json" else "visioneval.cnf"))

  # Create run_model.R script (two variants)
  # Create model-specific configuration
  # TODO: does the "unboxing" also work for yaml?
  runConfig_ls <-  list(
      Model       = jsonlite::unbox("BARE Model Test"),
      Scenario    = jsonlite::unbox("Test"),
      Description = jsonlite::unbox("Minimal model constructed programmatically"),
      Region      = jsonlite::unbox("RVMPO"),
      BaseYear    = jsonlite::unbox("2010"),
      Years       = c("2010") #, "2038")
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
    yaml::write_yaml(runConfig_ls,configFile)
    print(bare.dir)
    print(configFile)
    cat(readLines(configFile),sep="\n")
  }

  testStep("Copy other configuration files (geo, units, deflators)")

  from <- file.path(base.defs,c("units.csv","deflators.csv","geo.csv"))
  file.copy(from=from,to=bare.defs)
  print(bare.defs)
  print(dir(bare.defs,full.names=TRUE))

  testStep("Open BARE model using defaults (no inputs yet) and save object in .GlobalEnv")
  bare <<- openModel("BARE",log="info")

  testStep("List model inputs (only)...")

  # NOTE: though the specs will have an "INPUTDIR" column, it will be "NA"
  #   if the file does not exist on the model's InputPath (which is the case
  #   for the bare model).
  # TODO: ModelDir/Inputs should be the default INPUTDIR in that case.

  print(inputs <- bare$list(inputs=TRUE,details="FILE")) # or just details=TRUE
  required.files <- unique(file.path(base.inputs,inputs[,"FILE"]))
  required.files <- required.files[which(file.exists(required.files))]

  testStep("Copy model parameters to 'inputs' - could also be in 'defs')")

  # Inputs for sample modules: CreateHouseholds and PredictWorkers
  from <- file.path(base.defs,"model_parameters.json")
  file.copy(from=from,to=bare.inputs)
  print(dir(bare.inputs))

  testStep(paste("Copy the other required input files from",jr$modelName))

  print(required.files)
  from <- required.files
  file.copy(from=from, to=bare.inputs )
  print(bare.inputs)
  print(dir(bare.inputs,full.names=TRUE))

  testStep("run the bare model")

  bare$run() # no results yet - it will try to 'continue' then 'reset' if not 'Complete'
  print(bare$dir(results=TRUE))
  cat("Log path for the initial bare model run:\n")
  print(bare$log())

  testStep("run the bare model again with 'save'")

  bare$set(
    Param_ls=visioneval::addParameterSource(
      Param_ls=list(
        Scenario="Run with save",
        Description="This run will save prior results"
      ),Source="test.R/test_model()"
    )
  )
  bare$run(run="save") # should generate a results archive
  print(bare$dir(results=TRUE))
  cat("Log path should be different from the previous run:\n")
  print(bare$log())

  testStep("run (really DON'T run) the bare model again with 'continue'")

  bare$set(
    Param_ls=visioneval::addParameterSource(
      Param_ls=list(
        Scenario="Run with 'continue'",
        Description="This run should not do anything"
      ),Source="test.R/test_model()"
    )
  )
  bare$run(run="continue") # examine last run status and don't run if "Complete"
  print(bare$dir(results=TRUE))
  cat("Log path should be the same as previous run:\n")
  print(bare$log())
  
  testStep("run the bare model with 'reset'")

  bare$set(
    Param_ls=visioneval::addParameterSource(
      Param_ls=list(
        Scenario="Run with 'reset'",
        Description="This run should rebuild current results but not change archived list"
      ),Source="test.R/test_model()"
    )
  )
  bare$run(run="reset") # Should regenerate just the unarchived results
  cat("Results should still have one saved version plus the current results:\n")
  print(bare$dir(results=TRUE))
  cat("Log path should be new compared to latest run:\n")
  print(bare$log())

  testStep("list all fields in bare model - Inp/Get/Set")
  flds <- bare$list(inputs=TRUE,outputs=TRUE,details=c("INPUTDIR","FILE"))
  print(names(flds))
  flds$INPUTDIR[!is.na(flds$INPUTDIR)] <- basename(flds$INPUTDIR[!is.na(flds$INPUTDIR)]) # Just to keep it from spilling over...
  print(nrow(flds))
  print(flds[sample(nrow(flds),10),])

  testStep("extract model results - should see them in the directory")
  br <- bare$results()
  br$extract(prefix="BareTest")

  testStep("clear the bare model extracts")
  print(bare$dir(output=TRUE))
  bare$clear(force=!interactive())

  testStep("model after clearing outputs...")
  print(bare$dir())

  testStep("clear results as well...")
  bare$clear(force=!interactive(),outputOnly=FALSE) # default is FALSE if no outputs exist - delete results
  print(bare$dir())

  testStep("copy a model (includes results and outputs)")
  cp <- bare$copy("BARE-COPY")
  print(cp)
  print(cp$dir())

  testStep("Forcibly clear results from model copy")
  cp$clear(force=TRUE,outputOnly=FALSE) # forcibly removes outputs and results
  print(cp$dir())

  testStep("Break the run_model.R script in the copy and observe failure")
  runModelFile <- file.path(cp$modelPath,cp$stageScripts[1])
  runModel_vc[4] <- 'runModule("BorrowHouseholds","VESimHouseholds",RunFor="AllYears",RunYear=Year)'
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)
  cp$run() # Should throw error message about missing module...

  testStep("Display log from failed run...")
  log <- cp$log()
  cat("Log file",log,"\n")
  cat(readLines(log),sep="\n")
  
  testStep("remove model copy")
  unlink("models/BARE-COPY",recursive=TRUE)
  print(cp$dir())
  rm(cp)

  testStep("return bare model")
  return(bare)
}

test_results <- function (log="warn") {
  testStep("Manipulate Model Results in Detail")

  testStep("Copy model and get 'results' and 'selection' from empty model...")
  jr <- openModel("JRSPM")
  if ( "COPY" %in% dir("models") ) unlink("models/COPY",recursive=TRUE)
  cp <- jr$copy("COPY")
  cat("Directory before clearing...\n")
  print(cp$dir())
  cp$clear(force=TRUE,outputOnly=FALSE)
  cat("Directory after clearing...\n")
  print(cp$dir())
  cat("Results after clearing...\n")
  rs <- cp$results()
  print(rs)
  cat("Selection after clearing...\n")
  sl <- rs$select()
  print(sl)

  testStep("Pull out results and selection (head 12)...")
  cat("Results...\n")
  rs <- jr$results()
  print(rs)
  cat("Selection...\n")
  sl <- rs$select() # Get full field list
  print(head(capture.output(print(sl)),n=12))

  # Do some basic field extraction - list fields
  cat("Groups\n")
  print(sl$groups())
  cat("Tables\n")
  print(sl$tables())
  cat("Fields (random 20)\n")
  fld <- sl$fields()
  print(fld[sample(length(fld),20)])
  
  # Select some subsets by group, table or field name and extract those...
  # Can we easily identify group names, table names, field names and zero in on selecting them?
  testStep("Select some Groups")
  cat("Only the years...\n")
  sl$select( sl$find(Group="Years") )
  print(sl$groups())
  cat("Only the first of the years...\n")
  sl$select( sl$groups()[1] )         # Just the first ones
  print(sl$groups())

  testStep("Select Household and Vehicle Tables")
  sl$select( sl$find(Table=c("Household","Vehicle")) )
  print(sl)
  cat("Print selection with details...\n")
  print(head(capture.output(print(sl,details=TRUE)),n=12))
  
  # Test display units, select speeds, create unit conversion
  testStep("Creating and Writing Display Units...")
  sl$all() # Deselect everything
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
  sl$all() # re-select everything
  sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
  print(sl$fields())

  testStep("Showing currently defined UNITS/DISPLAYUNITS (via sl$results)")
  print(sl$results$units())
  testStep("Showing currently defined UNITS/DISPLAYUNITS (directly from rs)")
  print(rs$units())

  # Clean up the fields to add the geography fields in the Marea Table
  testStep("Adding geography fields to selection...")
  sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
  print(sl$fields())
  print(rs$units())

  testStep("Extracting speed fields using DISPLAY units")
  sl$extract(prefix="DisplayUnits")                 # Using DISPLAY units

  testStep("Exporting speed fields using DATASTORE units")
  sl$export(prefix="Datastore",convertUnits=FALSE)  # Using DATASTORE units

  testStep("Model directory")
  print(jr$dir())

  testStep("Model directory of results")
  print(jr$dir(results=TRUE))
  
  testStep("Model directory of outputs")
  print(jr$dir(outputs=TRUE))
  
  testStep("Interactively clear outputs but leave results")
  jr$clear(outputOnly=TRUE, force=FALSE)

  testStep("Directory after clearing")
  jr$dir()
}

test_query <- function(log="warn",multiple=FALSE) {
  # Process the standard query list for the test model
  # If multiple==TRUE, copy the test model and its results a few times, then submit the
  # list of all the copies to VEQuery. Each column of results will be the same (see
  # test_scenarios (TODO) for a run that will generate different results in each column).

  testStep("Set up Queries and Run on Model Results")
  testStep("Opening test model and caching its results...")
  jr <- openModel("JRSPM")
  rs <- jr$results()

  testStep("Show query directory (may be empty)...")
  print(jr$query())

  testStep("Create an empty query object and print it...")
  # create a query object
  qry <- jr$query("Test-Query",load=FALSE) # Don't open it if file exists already
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

  testStep("Remove test specifications...")
  cat("Removing:\n")
  print( nm <- qry$names()[1:3] )
  qry$remove(nm) # remove by name (bye-bye before,loc2 and loc0)
  print(qry)
  cat("Removing:\n")
  print(c("2",qry$names()[2]))
  qry$remove(2) # remove by position (bye-bye loc45)
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
  print(qry)
  qry$add(spec,location=1,after=TRUE)
  print(qry)
  
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

  print(qry)
  qry$add(spec)
  print(qry)

  testStep("Make a new VEQuery and add various bad specifications to it...")
  # TODO: Throw some additional specific broken queries at it to see if errors are correct.
  # TODO: destroy that object once we're done abusing it.

  testStep("Clear test queries, if any...")
  qfiles <- jr$query()
  print(qfiles <- file.path(jr$modelPath,"queries",qfiles))
  unlink(qfiles)
  print(jr$query())
  
  testStep("Save the query and fix its extension...")

  qry$save() # as Test-Query.VEqry
  cat("Saved values in original query...\n")
  cat("Name; "); print(qry$QuerydName)
  cat("Path: "); print(qry$QueryFile)
  cat("Directory: "); print(qry$QueryDir)
  print(dir(qry$QueryDir))

  testStep("Save a copy of the query and fix its extension...")

  qry2 <- qry$copy("Copy-Query.R") # .R will be removed from the name
  qry2$save() # Essentially as "Save As"
  cat("Saved values in renamed query...\n")
  cat("Directory: "); print(qry2$QueryDir)
  cat("Name; "); print(qry2$QueryName)
  cat("Path: "); print(qry2$QueryFile)
  cat("Contents of copied query...\n")
  print(qry2)

  testStep("Model QueryDir contents...")

  cat("Expecting "); print(c("Copy-Query.VEqry","Test-Query.VEqry"))
  print(jr$query())

  testStep("Save a query somewhere else...")
  qry2$save(qfile <- file.path(jr$modelPath,"queries","Dump-Query.R"))
  print(jr$query())

  testStep("Save a query without overwriting...")
  actualFile <- qry2$save(overwrite=FALSE)
  qfile <- c(qfile,actualFile)
  print(jr$query())
  unlink(qfile); rm(qry2)

  testStep("Open the query by short name in a different object from the file...")

  runqry <- jr$query("Test-Query")
  cat("Loaded query...\n")
  cat("Directory: "); print(runqry$QueryDir)
  cat("Name; "); print(runqry$QueryName)
  cat("Path: "); print(runqry$QueryFile)
  print(runqry)

  testStep("Open the query again from the file, using full file name...")

  runqry <- jr$query("Test-Query.VEQry")
  cat("Re-Loaded query with name extension...\n")
  cat("Directory: "); print(runqry$QueryDir)
  cat("Name; "); print(runqry$QueryName)
  cat("Path: "); print(runqry$QueryFile)
  print(runqry)
  rm(runqry)

  testStep("Run the query on the model...")
  qry$run(jr)

  testStep("Run the query on the results...")
  qry$run(rs)

  # TODO: ensure that the query outputs are disambiguated and remain available
    
  if ( multiple) {
    testStep("Query multiple models or scenarios...")
    # Generate several copies of jr
    testStep("Making model copies")
    cp.1 <- jr$copy("Scenario1")
    cp.1$rename(Scenario="Scenario 1",Description="Same as original...")
    cp.2 <- jr$copy("Scenario2")
    cp.1$rename(Scenario="Scenario 2",Description="Same as original...")

    # TODO: add a flag to VEModel:$copy to copy or ignore any results (currently does
    # results if they exist; want to be able to force ignoring them.)

    testStep("Multiple query by model name")
    # Query the vector of model names (character vector says "model names" to VEQuery)
    nameList <- c(jr$modelName,cp.1$modelName,cp.2$modelName)
    names(nameList) <- nameList
    qry$run(nameList,outputFile="%queryname%_ByModelName_%timestamp%")
    
    testStep("Multiple query as a list of opened VEModel objects")
    # Make a list of VEModel objects from the names and query that
    modelList <- lapply(nameList,openModel)
    names(modelList) <- nameList
    qry$run(modelList,outputFile="%queryname%_ByModelObject_%timestamp%")

    testStep("Multiple query as a list of VEResult objects")
    # Make a list of VEResults objects from the VEModel list and query that
    resultList <- lapply(modelList,function(m) m$results())
    names(resultList) <- nameList
    qry$run(resultList,outputFile="%queryname%_ByResultObject_%timestamp%")

    testStep("Multiple query as a list of ResultsDir path names")
    # Make a list of ResultsDir path names (i.e. list of character strings) from the
    # VEResults and query that (Note difference between a character vector - list of
    # model names and a list of character strings, which are the result paths).
    pathList <- lapply(resultList,function(r) r$resultsPath)
    names(pathList) <- nameList
    qry$run(pathList,outputFile="%queryname%_ByResultPath%timestamp%")

    testStep("Cleaning up model copies")
    unlink("models/Scen1",recursive=TRUE)
    unlink("models/Scen2",recursive=TRUE)
    rm( cp.1, cp.2)
  }
  testStep("Returning test model")
  return(jr)
}

test_rpat <- function(run=TRUE) {
  testStep("Testing VERPAT as JRPAT")
  verpat <- openModel("JRPAT")
  if ( ! verpat$valid() ) {
    testStep("Installing VERPAT as JRPAT")
    verpat <- installModel("VERPAT","JRPAT")
  }
  if ( run || ! verpat$results()$valid() ) {
    testStep("Clearing previous extracts")
    verpat$clear(force=TRUE) # outputs only
    testStep("Running JRPAT")
    verpat$run()
  }
  testStep("Extracting JRPAT results...")
  verpat$results()$extract()
}

# Now set it all up
rewind()
