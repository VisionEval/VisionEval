# Test functions for VEModel

# Load required packages

if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}
if ( ! requireNamespace("jsonlite",quietly=TRUE) ) {
  stop("Missing required package: 'jsonlite'")
}
if ( ! requireNamespace("yaml",quietly=TRUE) ) {
  stop("Missing required package: 'yaml'")
}

# future.callr also loads future
# required for multitasking test
if ( ! requireNamespace("future.callr",quietly=TRUE) ) {
  stop("Missing required package: 'future.callr'")
}

if ( ! "package:VEModel" %in% search() ) {
  # Will be there already if running from test_setup.R inside VEModel
  # package or using pkgLoad via ve.test()
  message("Loading built VEModel package")
  require("VEModel",quietly=TRUE)
}

logLevel <- function(log="info") {
  visioneval::initLog(Save=FALSE,Threshold=log)
}

testStep <- function(msg) {
  # Use 'message' to get contrasting color in RStudio
  message(paste("\n",paste(msg,collapse="\n"),sep="\n"))
}

stopTest <- function(msg="Stop Test") {
  stop(msg,call.=FALSE)
}

getModelDirectory <- function() { # hack to support pkgload which won't see the function as exported for some reason
  # NOTE: probably obsolete: since pkgload loads from sources/modules or sources/framework, there's
  # no NAMESPACE by default. I added NAMESPACE to .gitignore, and setting up to do "live
  # development" with ve.test just requires that the package be built once and the NAMESPACE copied
  # back from /built/.../src/VEModel/NAMESPACE. As long as the development doesn't produce any new
  # exported functions, we're fine. And if it does, we do one build and copy back NAMESPACE from
  # built/.../src
  return(
    if ( ! "getModelDirectory" %in% getNamespaceExports("VEModel") ) {
      # Hack to support pkgload from source folder which does not have a Namespace
      if ( "package:VEModel" %in% search() ) {
        vem <- as.environment("package:VEModel")
        vem$getModelDirectory()
      } else stop("package:VEModel failed to load!")
    } else {
      VEModel::getModelDirectory()
    }
  )
}

listTests <- function() {
  ls(envir=parent.env(environment()),pattern="^test_") # list defined tests
}

# Basic installer test - also used later to wrap model installations for other tests
test_00_install <- function(
  modelName="VERSPM",variant="base",installAs="",
  overwrite=TRUE,confirm=FALSE,log="warn"
) {

  logLevel(log)

  if ( ! nzchar(variant) ) variant <- ""
  if ( missing(installAs) || ! nzchar(installAs[1]) ) {
    if ( all( nzchar(c(variant,modelName)) ) ) installAs <- paste0("test-",modelName,"-",variant)
  }

  mod <- NULL
  if ( all( nzchar( c(modelName[1],variant[1],installAs[1]) ) ) ) {
    if ( dir.exists(existingModel <- file.path("models",installAs)) ) {
      if ( overwrite ) {
        testStep(paste0("Clearing previous installation at ",installAs))
        unlink(file.path("models",installAs), recursive=TRUE)
        overwrite <- TRUE
      } else {
        message("install opens existing model")
        mod <- openModel(existingModel,log=log)
      }
    }
    if ( overwrite || is.null(mod) ) {
      testStep(paste("Installing",modelName,"model variant",variant,"from package as",installAs))
      mod <- installModel(modelName,variant=variant,modelPath=installAs,log=log,confirm=confirm,overwrite=TRUE)
    }
  } else {
    if ( nzchar(modelName) ) {
      testStep(paste0("Directory of available variants for ",modelName))
    } else {
      testStep("Directory of available models")
    }
    mod <- installModel(modelName=modelName,variant="",log=log,private=TRUE)
  }
  # NOTE: pkgload bug - print.VEAvailableModels/Variants not recognized for class dispatch...
  if ( "VEAvailableModels" %in% class(mod) ) {
    print.VEAvailableModels(mod)
  } else if ( "VEAvailableVariants" %in% class(mod) ) {
    print.VEAvailableVariants(mod)
  } else {
    print(mod)
  }
  return(invisible(mod))
}

test_00_install_all <- function(listonly=FALSE,private=FALSE,overwrite=FALSE,log="warn") {

  logLevel(log)

  testStep("Listing all available Models and Variants")
  models     <- installModel("")     # List available models by package
  modelnames <- unique(models$Model) # Vector of available model names
  
  cat("Available models:\n")
  print(models) # a data.frame

  variants <- lapply(modelnames,
    function(mn) {
      installModel(mn,var="",private=private)
    }
  ) # returns list of data.frames
  names(variants) <- modelnames

  if ( listonly ) {
    testStep("Available models and variants:")
    print(variants)
    return(invisible(variants))
  }

  testStep("Installing all available model variants")

  openFirst <- missing(overwrite) # If model exists, open it and overwrite if not Run Complete
  modelRoot <- getModelDirectory()
  installed <- lapply(
    modelnames,
    function(m) {
      cat("Installing model variants for ",m,"\n",sep="")
      vars <- variants[[m]]$Variant
      mods <- lapply(
        vars,
        function(v) {
          installMsg <- "Installing"
          cat("\n   Model",m," Variant",v,": ")
          do.overwrite <- overwrite
          modelPath <- paste("all-install",m,v,sep="-")
          if ( ( openFirst || overwrite ) && dir.exists( file.path(modelRoot,modelPath) ) ) {
            model <- openModel(modelPath)
            if ( ! openFirst || ! model$valid() || model$printStatus() != "Run Complete" ) {
              do.overwrite <- TRUE
              installMsg <- paste0("Re-",installMsg)
            } else {
              cat(" Preserving run ('overwrite=TRUE' to remove)")
              return(model)
            }
          }
          # Not returning opened model - re-install it (overwriting if bad earlier effort)
          cat(installMsg)
          model <- installModel(
            m,variant=v,
            modelPath=modelPath,
            confirm=FALSE,overwrite=do.overwrite,log=log
          )
          return(model)
        }
      )
      cat("\n\n")
      names(mods) <- sapply(mods,function(m)m$modelName)
      return(mods)
    }
  )
  names(installed) <- modelnames
  cat("\nInstallation completed successfully.\n")
  
  # installed will be a list with elements for each model type (VERSPM, VERPAT, etc)
  # each element is a list of opened (but not run) VEModel's for each model variant
  testStep("Installed these models:")
  print(installed)
  return(invisible(installed))
}

# Check that we can source run_model.R to run a classic model
# Does direct installation (keep consistent with test_nn_install)
test_01_classic <- function(modelName="VERSPM-classic",clear=TRUE,log="info") {

  logLevel(log)

  modelPath <- file.path("models",modelName)
  owd <- getwd()
  testStep(paste("Runtime is",owd))
  on.exit(setwd(owd))

  needInstall <- TRUE
  if ( dir.exists(modelPath) && clear ) {
    testStep("Clearing model to start from scratch")
    unlink(modelPath,recursive=TRUE)
  } else if ( dir.exists(modelPath) ) {
    needInstall <- ! all( existingFiles <- file.exists(
      file.path( modelPath, requiredFiles <- c(
        "run_model.R",
        "defs/geo.csv",
        "defs/units.csv",
        "defs/deflators.csv",
        "inputs/azone_carsvc_characteristics.csv" # hoping that if one is there, all are
      ) )
    ) )
    if ( ! needInstall ) {
      testStep("Attempting to re-run without install")
      unlink(dir(modelPath,pattern="^Log.*txt$",full.names=TRUE))
      unlink(dir(modelPath,pattern="^Datastore.*",full.names=TRUE),recursive=TRUE)
      unlink(dir(modelPath,pattern="^ModelState.*",full.names=TRUE))
    } else {
      message("Need to re-install due to missing files")
      print(requiredFiles[ ! existingFiles ])
      unlink(modelPath,recursive=TRUE) # leave nothing to chance
    }
  }
  if ( needInstall ) {
    testStep(paste("Installing classic VERSPM model from package as",modelName))
    mod <- test_00_install(
      "VERSPM",variant="classic",installAs=modelName,
      log=log,overwrite=clear,confirm=FALSE
    )
    modelName <- mod$modelName
    rm(mod)  # Don't keep the VEModel around - will run manually below
  }

  testStep(paste("Running",modelName,"by sourcing scripts/run_model.R in",modelPath))

  # Note that requiring visioneval explicitly is needed to run a classic model
  # due to explicit dependencies in the run_model.R script. New-style models handle
  # that internally.
  setwd(modelPath)
  require(visioneval) # Put it on the search path for GetYears, RunModule, etc
  source("run_model.R")
  detach("package:visioneval") # But leave the namespace loaded

  testStep("Reviewing model status (will get confused by incomplete RunParam_ls)")

  setwd(owd)
  mod <- openModel(modelName)
  cat("Model Status:",mod$printStatus(),"\n")
  return(mod)
  # return model object for further analysis (can be re-run in VEModel environment)
  # mod$run() will ignore classic results and construct a "results" directory,
  #   which makes the "classic" model slightly "post-modern"
}

# This test is also used internally later to make sure a model run is available
# for things like extracting results or running queries.

test_01_run <- function(
  modelName="VERSPM-base",baseModel="VERSPM",variant="base",
  reset=FALSE,log="info",confirm=FALSE,multicore=FALSE
) {
  logLevel(log)
  model.dir <- dir("models")
  if ( all(c(missing(modelName),missing(baseModel))) ) {
    message("Choose a model to run, or name a new one to install")
    return(model.dir)
  }
  if ( missing(modelName) ) modelName <- paste(baseModel,variant,sep="-")
  if ( ! modelName %in% model.dir || reset ) {
    if ( modelName %in% model.dir ) {
      reset <- TRUE
      modelPath <- file.path("models",modelName)
      unlink(modelPath,recursive=TRUE)
    }
    mod <- test_00_install(modelName=baseModel,variant=variant,installAs=modelName,log="warn",confirm=confirm)
  }

  if ( ! reset ) {
    testStep(paste("Attempting to re-open existing model:",modelName))
    mod <- openModel(modelName,log=log)
    if ( mod$overallStatus != codeStatus("Run Complete") ) {
      print(mod)
      message("\nModel is not Complete; Rebuilding...")
      reset <- TRUE
    } else {
      message("Returning existing model run")
    }
  }
  if (reset) {
    mod <- openModel(modelName,log=log)
    testStep(paste("Running model with 'reset'",mod$modelName))
    if ( multicore ) {
      if ( ! is.numeric(multicore) ) multicore <- 3
      mod$plan(workers=multicore)
      message("Running with ",multicore," workers")
    }
    mod$run(run="reset",log=log) # clears results directory
  }
  return(mod)
}

test_01A_flatten <- function(useResults=FALSE, log="info") {
  logLevel(log)

  testStep("Flatten Datastore (Low-level test)")
  message("Ensure staged model is available (with DatastorePath)")
  vr <- test_01_run("VERSPM-pop","VERSPM","pop",log=log)
  print(vr)

  testStep("prepare receiving directory")
  ToDir <- normalizePath("testFlatten")
  if ( dir.exists(ToDir) ) {
    if ( useResults ) {
      rs <- openResults(ToDir)
      if ( rs$valid() ) {
        return(rs)
      }
    }
    unlink(ToDir,recursive=TRUE)
  }
  if ( ! dir.exists(ToDir) ) dir.create(ToDir)

  testStep(paste("Flattening Datastore"))
  ms <- tail(vr$modelStages,1)[[1]]
  assign("ModelState_ls",ms$ModelState_ls,envir=visioneval::modelEnvironment(Clear="test_01A_flatten"))
  modelPath <- ms$RunPath
  owd <- setwd(modelPath) # so we can find the datastore to copy
  on.exit(setwd(owd))     # return to original directory even on failure
  message("Flatten Parameters")
  print( c(ls(visioneval::modelEnvironment()),modelPath,ToDir) )
  visioneval::copyDatastore(ToDir,Flatten=c(TRUE,TRUE))
  message("Directory after flattening")
  print(dir(ToDir)) # Should now have the flattened datastore and modelstate
  
  testStep("Return flattened Datastore as a VEResults object")
  results <- openResults(ToDir)
  print(results)
  setwd(owd)
  return(results)
}

# test_02_model runs through basic model configuration "by hand"
# oldstyle creates defs/run_parameters.json; if not oldstyle, create visioneval.cnf
# modelName is the particular variant of VERSPM to use as a base. The base model MUST
#  be a version of VERSPM since we use its first two modules to create the "Bare" model
# log="warn" will confine to a streamlined list of log messages like what a regular user
#  would see. "info" gives lots of gory details.
# brief=TRUE conducts some deeper tests.
test_02_model <- function(modelName="VERSPM-Test", oldstyle=FALSE, log="info", brief=FALSE) {

  if ( ! missing(log) ) logLevel(log)

  cat("*** Test Model Management Functions ***\n")
  options(warn=2) # Make warnings into errors...

  testStep("open the full test version of VERSPM")
  reinstall <- FALSE
  if ( modelName %in% openModel() ) {
    mod <- openModel(modelName)
    if ( ! mod$valid() ) reinstall <- TRUE
  } else reinstall <- TRUE
  if ( reinstall ) {
    mod <- test_00_install(modelName="VERSPM",variant="base",installAs=modelName,overwrite=FALSE,confirm=FALSE,log=log)
  }

  if (! "VEModel" %in% class(mod) ) {
    message("Hmm: ",modelName," is not a VEModel")
    return(mod)
  } else {
    message("We'll mine this model for sample inputs")
    print(mod,details=TRUE)
  }

  testStep("Gather base model parameters")
  base.dir <- mod$modelPath
  cat("Base Model directory:\n")
  print(base.dir)

  cat("Base model structural directories - including stages\n")
  for ( stage in mod$modelStages ) {
    cat("Stage:",stage$Name,"\n")
    jrParam_ls <- stage$RunParam_ls
    cat("  ParamPath    :",visioneval::getRunParameter("ParamPath",Param_ls=jrParam_ls),"\n")
    cat(paste("  InputPath    :",visioneval::getRunParameter("InputPath",Param_ls=jrParam_ls),"\n"))
    cat("  InputDir     :",visioneval::getRunParameter("InputDir",Param_ls=jrParam_ls),"\n")
    cat(paste("  DatastorePath:",visioneval::getRunParameter("DatastorePath",Param_ls=jrParam_ls),"\n"))
  }

  testStep("Construct a bare model from scratch, borrowing from base model")
  bare.dir <- file.path("models","BARE")
  if ( dir.exists(bare.dir) ) {
    cat("Blowing away existing bare model.\n")
    unlink(bare.dir,recursive=TRUE)
  }
  dir.create(bare.dir)

  testStep("Create minimal run_model.R")

  # NOTE: VEModel does not require, but will process, the "initializeModel() function
  runModelFile <- file.path(bare.dir,"run_model.R")
  runModel_vc <- c(
    '',
    'for(Year in getYears()) {',
    'runModule("CreateHouseholds","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictWorkers","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    '}'
  )
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  testStep("Set up model directory structure.")

  # Borrow model geography, units, deflators from base model
  bare.defs <- file.path(bare.dir,"defs")
  bare.inputs <- file.path(bare.dir,"inputs")
  dir.create(bare.defs)
  dir.create(bare.inputs)
  print(dir(bare.dir,recursive=TRUE,full.names=TRUE,include.dirs=TRUE))

  testStep(paste0("Create configuration: ",if (oldstyle) "defs/run_parameters.json" else "visioneval.cnf"))

  # Create model-specific configuration
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

  base.defs <- mod$setting("ParamPath",shorten=FALSE)
  from <- file.path(base.defs,c("units.csv","deflators.csv","geo.csv"))
  file.copy(from=from,to=bare.defs)
  print(bare.defs)
  print(dir(bare.defs,full.names=TRUE))

  testStep("Open BARE model using defaults (no inputs yet)")
  bare <- openModel("BARE",log=log)

  testStep("List model inputs (only)...")

  # NOTE: though the specs will have an "INPUTDIR" column, it will be "NA"
  #   if the file does not exist on the model's InputPath (which is the case
  #   for the bare model).

  base.inputs <- unique(mod$dir(inputs=TRUE,all.files=TRUE,showRootDir=FALSE)) # List short names of input paths for each stage
  cat("Base Inputs",base.inputs,collapse="\n",sep="\n")
  base.inputs <- unique(mod$dir(inputs=TRUE,all.files=TRUE,shorten=FALSE)) # Now get the input directory path names for inputs
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"),reset=TRUE) # reset: Opening it before made a "dud" ModelState_ls
  cat("Input Directories (should be NA - files don't exist yet):\n")
  print( unique(inputs[,"INPUTDIR"]) )
  # INPUTDIR will be NA since files don't exist
  # INPUTDIR is where it actually found the files,
  #   versus INPUTPATH (which is all the places they might be
  required.files <- unique(sapply(inputs[,"FILE"],function(f) grep(paste0(f,"$"),base.inputs)))
  required.files <- base.inputs[ required.files ]
  required.files <- required.files[which(file.exists(required.files))]

  testStep(paste("Copy required input files from",mod$modelName))

  print(required.files)
  from <- required.files
  file.copy(from=from, to=bare.inputs )
  print(bare.inputs)
  print(dir(bare.inputs,full.names=TRUE))

  testStep("Copy model parameters to 'inputs' - could also be in 'defs')")

  # Inputs for sample modules: CreateHouseholds and PredictWorkers
  # Historically, model_parameters.json was in ParamDir; the framework will
  #   look in both ParamDir and InputDir; sample model has it in InputDir
  from <- base.inputs[ grep("model_parameters.json$",base.inputs) ]
  file.copy(from=from,to=bare.inputs) # or copy to bare.defs...
  print(dir(bare.inputs))

  testStep("Re-open the bare model")
  bare$configure()

  testStep("List the inputs again: this time showing directory")
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  print(inputs)
  required.files <- file.path(inputs$INPUTDIR,inputs$FILE)
  print(unique(required.files))
  required.files <- data.frame(EXISTS=file.exists(required.files),FILE=required.files)
  cat("Required Files (all should EXIST):\n")
  print(unique(required.files))

  testStep("run the bare model")

  bare$run() # no results yet - it will try to 'continue' then 'reset' if not 'Run Complete'

  if ( brief ) {
    testStep("Skipping deeper tests")
    return(bare)
  }

  print(bare$dir(results=TRUE))
  cat("Log path for the initial bare model run:\n")
  print(bare$log(shorten=FALSE))

  testStep("run the bare model again with 'save'")

  updateSetup(
    bare$modelStages[[1]],
    Source="test.R/test_02_model()",
    Scenario="Run with save",
    Description="This run will save prior results"
  )
  bare$run(run="save") # should generate a results archive
  print(bare$dir(root=TRUE,results=TRUE,archive=TRUE))
  cat("Log path should be different from the previous run:\n")
  print(bare$log())

  testStep("run (really DON'T run) the bare model again with 'continue'")

  updateSetup(
    bare$modelStages[[1]],
    Source="test.R/test_02_model()",
    Scenario="Run with 'continue'",
    Description="This run should not do anything"
  )
  bare$run(run="continue") # examine last run status and don't run if "Complete"
  print(bare$dir(root=TRUE,results=TRUE,archive=TRUE))
  cat("Log path should be the same as previous run:\n")
  print(bare$log())
  
  testStep("run the bare model with 'reset'")

  updateSetup(
    bare$modelStages[[1]],
    Source="test.R/test_02_model()",
    Scenario="Run with 'reset'",
    Description="This run should rebuild current results but not change archived list"
  )
  bare$run(run="reset") # Should regenerate just the unarchived results
  cat("Results should still have one saved version plus the current results:\n")
  print(bare$dir(results=TRUE,archive=TRUE))
  cat("Log path should be new compared to latest run:\n")
  print(bare$log())

  testStep("list all fields in bare model - Inp/Get/Set")
  flds <- bare$list(inputs=TRUE,outputs=TRUE,details=c("INPUTDIR","FILE"))
  flds$INPUTDIR[!is.na(flds$INPUTDIR)] <- basename(flds$INPUTDIR[!is.na(flds$INPUTDIR)]) # Just to keep it from spilling over...
  print(nrow(flds))
  print(flds[sample(nrow(flds),10),])

  return(invisible(bare)) # Return model for additional processing
}

test_02_export_partitions <- function() {
  # Note - doesn't work with models yet - see test_02_basic_export below
  makeData <- function() {
    # prepare a data set
    df <- CO2 # standard R dataset
    for ( i in 1:length(df) ) {
      if ( is.factor(df[[i]]) ) df[[i]] <- as.character(df[[i]])
    }
    df$Scenario <- sample(paste("Scenario",1:2,sep="_"),nrow(df),replace=TRUE)

    dfg <- df
    dfg$Global <- "Global"
    dfg$Group <- dfg$Global

    dfy <- dfg
    dfy$Year <- "2023"
    dfy$Group <- dfy$Year
    dfy[["Global"]] <- NULL

    dfy2 <- dfy
    dfy2$Year <- "2044"
    dfy2$Group <- dfy2$Year
    dfy2[["Global"]] <- NULL
    
    return(list(
      Global=df,
      Yr2023=dfy,
      Yr2044=dfy2
    ) )
  }

  # Try several partitions
  message("Testing VEPartition")
  dataSets <- makeData() # pseudo-data with various fields for reviewing output
  part <- VEPartition$new(c(Global="folder",Year="folder",Type="path",Treatment="name"))
  print(part)
  testStep("Testing $location function")
  loc <- part$location(dataSets$Global,"MyTable") # view location structure
  print(loc)
  testStep("Testing $partition function")
  message("Fields in Global:")
  print(names(dataSets$Global))
  testStep("Testing Partitions")
  message("Global\n")
  partGlobal <- part$partition(dataSets$Global,"Global")
  print(partGlobal)
  message("Year 2023\n")
  part2023 <- part$partition(dataSets$Yr2023,"CO2-data")
  print(part2023)
  message("Year 2044\n")
  part2044 <- part$partition(dataSets$Yr2044,"CO2-data")
  print(part2044)
  testStep("Change partition to make Treatment a folder\n")
  part <- VEPartition$new(c(Global="folder",Year="folder",Type="name",Treatment="folder"))
  print(part$partition(dataSets$Yr2044,"RetryTreatment"))
  testStep("Try with NA data in a partition field\n")
  dftry <- dataSets$Yr2044$Global <- NA
  print(part$partition(dataSets$Yr2044,"GlobalWithNA"))
}

test_dbname <- function(name=c("Junk.try.sqlite","Junk","Junk.csv","Junk.sqlite")) {
  Timestamp = format(Sys.time(), )
  results <- sapply( name, function(basename) {
    dbname <- strsplit(basename, "\\.")[[1]] # now a vector with the extension
    if ( length(dbname) == 1 ) dbname <- append(dbname,"sqlite")
    dbname[1] <- paste("Model",dbname[1],sep="_") # prepend model name
    dbname <- c(paste(dbname[-length(dbname)],collapse="."),dbname[length(dbname)])
    dbname[1] <- paste(dbname[1],Timestamp<-format(Sys.time(),"%Y%m%d%H%M"),sep=TimeSeparator<-"_")
    dbname <- paste(dbname,collapse=".")
  } )
  return(
    data.frame(
      original   = name,
      configured = results
    )
  )
}

test_02_export_connections <- function(
  testConnections=c("data.frame","csv","sql","mysql"),
  mysql=FALSE,
  reset=TRUE
) {
  # Run through the nameTable / createTable / writeTable elements
  # Connections are initialized with defaults and exemplary settings
  # We need a model to satisfy things like OutputDir used e.g. by CSV or SQLite
  testStep("Set up a model for testing; just for output location")
  Model <- test_01_run("VERSPM-export",reset=FALSE,log="warn")
  print(Model)
  if ( reset ) Model$clear(force=TRUE,outputOnly=TRUE)

  # Really dumb data designed to be clearly readable
  #   when we do the export. Note that we're writing
  #   unpartitioned data here - plain table with limited
  #   features. See test_02_basic_export for full test of
  #   proper placement of partitioned data.
  Data <- data.frame(
    Marea = c("Marea1","Marea2"),
    Azone = c("Azone1","Azone2"),
    Value1 = 1:2,
    Value2 = 3:4,
    Value3 = letters[5:6],
    Value4 = LETTERS[7:8]
  )

  writeMe <- function(Connection,Data) {
    # Test a Connection by writing different things to it
    # Exercise nameTable
    cat("\nTesting Connection:\n")
    message("Starting at ",Sys.time())
    print(Connection$summary())
    cat("Name Table:\n")
    loc <- VETableLocator$new(Paths=c("path1","path2"),Names=c("name1","name2"),Table="TestTable")
    cat("Table Locator:\n")
    print(loc)
    Table <- Connection$nameTable(loc)
    print(Table) # Connection formatted
    # We'll save actually writing to a Path for the basic_export test below
    cat("Putting Data in bare table: ",Table,"\n") # in the real world, export will call nameTable
    # Table goes into OutputDir
    cat("Field Names:\n")
    print( Connection$createTable(Data,Table) )
    cat("Read back the created table contents:\n")
    print( Connection$readTable(Table) )
    cat("What's in the model outputs?\n")
    print(Model$dir(outputs=TRUE,all.files=TRUE))
    cat("Append the Data to the Table\n")
    print( Connection$writeTable(Data,Table) )
    cat("Should be two copies of the Data rows\n")
    print( Connection$readTable(Table) )
    cat("Re-create the table; should have one copy of Data\n")
    print( Connection$createTable(Data,Table) )
    print( Connection$readTable(Table) )
    cat("Test adding Data with columns missing from Data compared to existing ",Table,"\n")
    DataReduced <- Data[,-which( names(Data) == "Value2" )]
    print( Connection$writeTable(DataReduced,Table) )
    print( Connection$readTable(Table) )
    cat("Test adding Data with columns missing from ",Table,"\n")
    DataAugmented <- cbind(Data,Value5=paste0(Data$Value3,Data$Value4))
    print( Connection$writeTable(DataAugmented,Table) )
    print( Connection$readTable(Table) )
    cat("Test connection summary:\n")
    cat( Connection$summary(),"\n" )  # summary produces a single string, possibly with embedded newlines
    cat("Test connection list:\n")
    print( Connection$list() )               # The field names only
    cat("Test connection list with full details (field names, not just tables):\n")
    print( Connection$list(nameOnly=FALSE) ) # Tables with their names
    message("Ended at ",Sys.time())
  }

  # We're doing the connection at a low level, so none of the defaults injected by
  #  the exporter will apply. Need to fully specify config with the options
  #  we want to test

  # TODO: need to test model Exporters configuration setting
  # Could add Exporters section but that might trigger a re-run (need to mark Exporters
  # as irrelevant to "out of date").
  if ( "data.frame" %in% testConnections ) {
    # data.frame connection
    cdf <- makeVEConnection(Model, config=list(driver="data.frame"))
    writeMe(cdf,Data)
  }
  if ( "csv" %in% testConnections ) {
    # ccsv <- makeVEConnection(Model) # default is csv
    ccsv <- makeVEConnection(Model, config=list(driver="csv",Timestamp="database"))
    writeMe(ccsv,Data)
  }
  if ( "sql" %in% testConnections ) {
    csql <- makeVEConnection(Model,config=list(driver="sql",Timestamp="database")) # SQLite, default DB Name + Extension
    writeMe(csql,Data)

    sqlConfig <- list( # SQLite with explict DBName
      driver="sql",
      Database="ExplicitDBName",
      Timestamp="none"
      # Can add DBIConfig elements to deeply override the the database name
    )
    csql <- makeVEConnection(Model,sqlConfig) # Positional parameter work for configuration
    writeMe(csql,Data)
  }
  if ( "mysql" %in% testConnections ) {
    # Requires that the named database already be set up for user 'visioneval' password 'showme'
    mysqlConfig <- list(
      # In makeVEConnection, if "driver" is not present but "drv" is present, presume driver="sql"
      # driver = "dbi",    # same as "sql" - uses low-level R DBI driver
      # parameters in the connection DBIConfig sub-list will be passed to dbConnect using do.call
      driver = "sql", # send into DBI driver
      package = "RMariaDB",
      drv = RMariaDB::MariaDB(), # or drv="RMariaDB::MariaDB()" - character string will be parsed
      Timestamp = "prefix",
      DBIConfig = list(
        dbname = "visioneval",
        user = "visioneval",
        password = "showme"
      )
    )
    # NOTE: better to do the following to keep the credentials out of the model script:
    # mysqlConfig <- list(
    #    driver="sql",
    #    DBIConfig = list(
    #      drv="RMariaDB::RMariaDB()",
    #      group="visionevalDB"
    #    )
    # )
    # Then in your "my.cnf" file, put this block:
    #
    # [visionevalDB]
    # database=visioneval
    # user=visioneval
    # password=showme
    #
    requireNamespace("DBI")
    mysql <- makeVEConnection(Model,mysqlConfig)
    if ( reset ) {
      # TODO: create a "reset" method for VEConnection generally?
      # Could remove CSV/SQLite/other tables
      con = mysql$raw() # for DBI/SQL connection, returns the DBI connection
      # NOTE: Example SQL below for creating database/user presumes MariaDB; You might have to
      #   tweak the syntax for original MySQL or other DB services
      # In MySQL/MariaDB, the user permissions (once set) don't care if the the database is dropped
      #   and re-created. Permissions are by name not by object.
      # The specific syntax here might need to be tweaked for your database
      DBI::dbExecute(con,"CREATE OR REPLACE DATABASE visioneval;")
      DBI::dbExecute(con,"USE visioneval;")
      # We won't build the user here, but you can set up the test user like the following,
      #   presuming you're running MariaDB/MySQL on localhost
      # con <- dbConnect(RMariaDB::MariaDB(),user='admin',password='$adminpassword')
      # dbExecute(con,"CREATE USER 'visioneval'@localhost IDENTIFIED BY 'showme';")
      # dbExecute(con,"GRANT ALL PRIVILEGES ON 'visioneval'.* TO 'visioneval'@localhost;")
      # dbExecute(con,"FLUSH PRIVILEGES;")
      # dbDisconnect(con); rm(con)
    }
    writeMe(mysql,Data)
  }
  return(Model) # to view Model$dir(output=TRUE,all.files=TRUE)
}

test_02_basic_export <- function(exporter="sql",reset=FALSE,log="warn",connection=list())
{
  modelReset <- is.character(reset) && reset=="model"
  if ( modelReset || !is.logical(reset) ) reset=TRUE

  testStep("Set up VERSPM-base model instance for export tests")
  mod <- test_01_run("VERSPM-export",reset=modelReset,log="warn")
  print(mod)

  testStep("extract model results, show directory")
  br <- mod$results()
  print(br)

  testStep("Set up connection")
  if ( ! "TablePrefix" %in% names(connection) ) {
    connection=c(connection,list( TablePrefix="ExportTest_" )) # NOTE: must include necessary delimiter, if any
  }
  if ( ! "Timestamp" %in% names(connection) ) {
    connection=c(connection,Timestamp="database") # mostly for SQLite
  }

  testStep(paste("Exporting to ",paste(exporter,collapse=", ")))
  for ( format in exporter ) {
    if ( format == "data.frame" ) {
      testStep("Extract to data.frames")
      R.data <- br$extract(connection=connection)  # Returns a list of R data.frames
      str(R.data)
      extractor <- attr(R.data,"Exporter")
    } else if ( format == "default" ) {
      testStep("Export to default format (usually CSV)")
      extractor <- br$export(connection=connection) # Creates files and folders in OutputDir
      cat("Directory:\n")
      print(mod$dir(outputs=TRUE,all.files=TRUE))
    } else if ( format == "csv" ) {
      testStep("Export to CSV explicitly")
      extractor <- br$export("csv",connection=connection) # Creates files and folders in OutputDir
      cat("Directory:\n")
      print(mod$dir(outputs=TRUE,all.files=TRUE))
    } else if ( format == "mysql" ) {
      testStep(paste0("Export to MySQL exporter"))
      mysqlConnection<- list(
        TablePrefix=connection$TablePrefix,
        driver = "sql", # send into DBI driver
        package = "RMariaDB",
        drv = RMariaDB::MariaDB(), # or drv="RMariaDB::MariaDB()" - character string will be parsed
        Timestamp = "prefix",
        DBIConfig = list(
          dbname = "visioneval", # Adjust for local mysel database / user / password
          user = "visioneval",
          password = "showme"
        )
      )
      if ( reset ) {
        mysqlConnection <- makeVEConnection(Model,mysqlConnection)
        con <- mysqlConnectionxs$raw()
        DBI::dbExecute(con,"CREATE OR REPLACE DATABASE visioneval;")
        DBI::dbExecute(con,"USE visioneval;")
      }
      extractor <- br$export(exporter,connection=mysqlConnection) # returns a VEExporter object
      cat("Exporter list of tables:\n")
      print(extractor$list()) # names of tables
      R.data <- extractor$data()
      str(R.data)
      extractor$close() # generally nice to do for DBI
    } else {
      testStep(paste0("Export to exporter '",format,"'"))
      partition <- c(Global="path") # Merge scenarios and years
      cat("Exporting...\n")
      extractor <- br$export(exporter=exporter,partition=partition,connection=connection) # returns a VEExporter object
      cat("Exporter list of tables:\n")
      print(extractor$list())
      cat("Database list of tables:\n")
      R.data <- extractor$data()
      # str(R.data) # may generate voluminous output
      extractor$close() # if it's SQLite need to close the extractor to delete the file below
    }
  }

  testStep("Test save function for exporter")
  extractor$save("LastExporter")
  ext2 <- mod$exporter("LastExporter")
  testStep("Tables in loaded exporter")
  print(tables <- ext2$list())
  df <- ext2$data( tables[1] )
  str(df)

  testStep("clear the model extracts")
  cat("Interactive clearing of outputs (not results):\n")
  mod$clear(force=!interactive())

  testStep("model after clearing outputs...")
  print(mod$dir())

  return(mod)
}

test_02_multicore <- function(model=NULL, log="info", workers=3) {

  logLevel(log)

  testStep("Finding BARE model template")
  if ( is.null(model) ) {
    model <- test_02_model(brief=TRUE,log=log) # skip the deeper tests, use BARE model
  } else {
    print(model)
  }

  testStep("Copy model...")
  modelPath <- file.path("models","CORE-test")
  if ( dir.exists(modelPath) ) unlink(modelPath,recursive=TRUE)
  coreModel <- model$copy("CORE-test",copyResults=FALSE)
  print(coreModel)

  testStep("Create CORE-base stage")
  updateSetup(coreModel,inFile=TRUE,drop=c("Scenario","Description"))
  writeSetup(coreModel,overwrite=TRUE)
  owd <- setwd(coreModel$modelPath)
  file.rename(coreModel$setting("InputDir"),"CORE-base")
  stageConfig_ls <-  list(
      Scenario    = jsonlite::unbox("CORE-base"),
      Description = jsonlite::unbox("Base Stage for Multicore Test")
    )
  configFile <- file.path("CORE-base","visioneval.cnf")
  yaml::write_yaml(stageConfig_ls,configFile)
  setwd(owd)
  coreModel$configure(reloadFile=TRUE) # Will re-load CORE-base stage configuration from disk
  print(coreModel)

  testStep("Run the CORE-test model inline")
  coreModel$plan("inline")   # "sequential" maps over to the same thing
  coreModel$run()            # should look identical to test_model run and BARE
  # In particular, you should see the Logfile scrolling past "live"
  print(coreModel)

  testStep("Add model stages (just duplicates) to run in parallel")
  # To run in parallel, different stages must have the same "StartFrom"

  # For this test, we'll restructure the CORE-test model so it has four more identical
  # stages, each of which just runs the base model over and over. We'll get CORE-base,
  # Stage-1, Stage-2, Stage-3 and Stage-4 as sub-directories of
  # coreModel$modelPath/ResultsDir.
  for ( newstage in 1:4 ) {
    coreModel$addstage(
      Name=paste0("Simultaneous-",newstage),
      Dir=paste0("Stage-",newstage),
      Scenario=paste0("Simultaneous ",newstage),
      Description=paste("Run stage",newstage,"in parallel"),
      StartFrom="CORE-base"
    )
  }
  cat("Show model with new stages\n")
  print(coreModel)
  
  logLevel(log=log)

  testStep("Run model with callr")
  cat("Running with callr plan",workers, "workers; should work on any R version\n")
  coreModel$plan("callr",workers=workers)
  coreModel$run("continue") # Use existing Core-base stage run
  print(coreModel)
  # then will run each of the new stages in parallel (asynchronously)

  testStep("Run model with multisession")
  coreResults <- try ( {
    cat("Historically, multisession fails on Windows machines due to firewall restrictions.\n")
    cat("Recent tests on a pretty locked-down Windows 10 suggest it may now work.\n")
    coreModel$plan("multisession",workers=workers)
    coreModel$run("reset") # Will re-run CORE-base stage
    coreModel
  } )
  print(coreResults)
  testStep("Done with multitasking test")

  return(invisible(coreModel))
}

# existingResults parameter if provided should be a VEResultsList object
# TODO: factor this into more granular parts.
test_03_results <- function (existingResults=FALSE,testCopy=TRUE,log="info") {

  testStep("Manipulate Model Results in Detail, including export")

  # Use case is mostly for doing queries over a set of scenarios...
  # Return a list if mod$results(all.stages=TRUE) or mod$results(stages=c(stage1,stage2)) with
  # length(stages)>1 : all reportable stages in that case.
  # An individual stage can also be called out explicitly (and in that case, it does not
  #   need to be Reportable).

  # Get some results
  rs <- if ( missing(existingResults) || ! existingResults ) {
    logLevel("warn")
    mod <- test_01_run("VERSPM-pop","VERSPM",var="pop",log="warn") # use staged model to exercise DatastorePath

    if ( testCopy ) {
      # Testing model copy, just because...
      cat("Copying model...\n")
      if ( "COPY" %in% dir("models") ) unlink("models/COPY",recursive=TRUE)
      cp <- mod$copy("COPY") # Also copies results by default
      cat("Directory before clearing...\n")
      print(cp$dir())
      cp$clear(force=TRUE,outputOnly=FALSE) # Blow away all outputs
      cat("Directory after clearing...\n")
      print(cp$dir())
      rm(cp) # Should be no results 
    }
    testStep("Pull out results and selection from VERSPM-pop test model...")
    cat("Results...\n")
    mod$results()  # Gets results for all stages as VEResultsList (default print lists scenarios)
  }
  logLevel(log)

  # Selection can select Scenario=..., Stage=...,
  # Group=ExplicitYears, Group=Years, Group=Global, or Table=c(name1,name2,...)

  cat("Results:\n") # rs is a VEResultsList
  print(rs)
  cat("Selection...\n")
  sl <- rs$select() # Get full field list
  print(head(capture.output(print(sl)),n=12))

  # Do some basic extraction - list fields
  # Note that scenarios can also be selected when the Results/rs is generated
  cat("Scenarios\n")
  print(sl$scenarios())  # Lists only reportable stages, implemented as sl$stages(Reportable=TRUE)
  cat("Scenarios (identified as 'stages')\n")
  # Note that results list will only contain reportable stages, unless
  #  others are called for explicitly.
  print(sl$stages())
  cat("Groups\n")
  print(sl$groups())
  cat("Tables\n")
  print(sl$tables())
  cat("Fields (random 20)\n")
  fld <- sl$fields()
  print(fld[sample(length(fld),20)])
  
  # Select some subsets by group, table or field name and extract those...
  # These will show all scenarios, which in the population model is only one
  # See one of the multi-scenario tests for what happens with scenarios and selections
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
  sl$all() # Select everything
  un <- rs$list(details=TRUE)[,c("Scenario","Group","Table","Name","Units")]
  spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
  spd$DisplayUnits <- "MI/HR"
  cat("Writing display_units.csv\n")
#   if ( is.null(mod) ) {
#     # just write it into the runtime directory (getwd())
#     message("No model: saving display_units in runtime directory")
#     display_units_file <- file.path(
#       runtimeEnvironment()$ve.runtime, # VEModel function
#       visioneval::getRunParameter("DisplayUnitsFile",Param_ls=mod$RunParam_ls)
#     )
#   } else {
  message("Model provided: display_units in ParamDir")
  display_units_file <- file.path(
    mod$modelPath,
    visioneval::getRunParameter("ParamDir",Param_ls=mod$RunParam_ls),
    visioneval::getRunParameter("DisplayUnitsFile",Param_ls=mod$RunParam_ls)
  )
#   }
  cat(display_units_file,"\n")
  write.csv(spd,file=display_units_file)

  testStep("Selecting speed fields...")
  sl$all() # re-select everything
  sl$select( with(spd,paste(Scenario,Group,Table,Name,sep="/")) )
  print(sl$fields())

  testStep("Showing currently defined UNITS/DISPLAYUNITS (via sl$resultsList)")
  print(sl$resultsList$units())
  testStep("Showing currently defined UNITS/DISPLAYUNITS (directly from rs")
  print(rs$units())

  # Clean up the fields to add the geography fields in the Marea Table
  testStep("Adding geography fields to selection...")
  sl$addkeys() # Forces tables to have basic geography fields plus "Id"
  print(sl$fields())
  print(rs$units())

  exportPartition <- c(Global="name") # Put all scenarios in the same tables, make Global tables separate
  unitsConnection <- list( Directory="UnitTest", TablePrefix="DisplayUnits", Timestamp="none" )
  testStep("Extracting speed fields using DISPLAY units")
  rs$export("csv",connection=unitsConnection,partition=exportPartition)
  # Write to default output format using DISPLAY units
  # sl$export also works - dispatches to results list

  testStep("Exporting speed fields using DATASTORE units")
  datastoreConnection <- list( Directory="UnitTest", TablePrefix="Datastore", Timestamp="none" )
  rs$export("csv",connection=datastoreConnection,partition=exportPartition,convertUnits=FALSE)
  # Default output format using DATASTORE units

  if ( ! is.null(mod) ) {
    testStep("Model directory")
    print(mod$dir())

    testStep("Model directory of results")
    print(mod$dir(results=TRUE))

    testStep("Model directory of outputs")
    print(mod$dir(outputs=TRUE))

    testStep("Interactively clear outputs but leave results")
    mod$clear(outputOnly=TRUE, force=FALSE)

    testStep("Directory after clearing")
    print(mod$dir())
  } else {
    testStep("Outputs written to runtime directory")
    print(getwd())
    print(dir())
    message("Contents of outputs directory")
    print(dir("outputs"))
  }
  return(rs) 
}

# Test selection manipulation
test_03_select <- function( log="info" ) {

  logLevel(log)

  testStep("Manipulate model selection to pick and retrieve fields")
  mod <- test_01_run("VERSPM-pop","VERSPM","pop")
  rs <- mod$results("stage-pop-future")
  # rs is a VEResultsList, which can perform export operations
  # in this case, it just has one scenario/stage

  testStep("Directly access results using 'find'")
  cat("Result has",length(find <- rs$find()),"fields\n") # All the fields...
  print(head(find$fields(),n=10)) # First 10 or so field descriptors

  testStep("Access the selection")
  sl <- rs$select()
  cat("Fields to select from:",length(sl$fields()),"\n")

  testStep("Finding Worker table for 2038")
  wkr <- sl$find(Group="2038",Table="Worker")
  print(wkr)

  testStep("Selecting Worker table")
  sl$select(wkr)
  rs$select(sl)
  sl
  testStep("Extracting Worker table only to data.frames")
  # The extract function performs an export to data.frames in R (without saving them exernally).
  wrk.table <- rs$extract()
  print(class(wrk.table)) # should be list
  print(names(wrk.table)) # should contain "Worker"
  print(class(wrk.table[[1]])) # should be dataframe with worker elements
  print(wrk.table[[1]][sample(nrow(wrk.table[[1]]),min(nrow(wrk.table[[1]]),10)),])
  rm(wrk.table)

  testStep("Export all data.frames to SQLite")
  sl$all()
  rs$select( sl ) # redundant if sl is attached to rs...
  # NOTE: "sql" and "sqlite" are the same but may have different setups in visioneval.cnf
  extr <- rs$export("sqlite",partition=c(Global="name")) # Break out Global tables from Year tables
  message("List of output tables")
  print(extr$list()) # List all the table identifiers

  testStep("Extract some tables as data.frames from exporter")
  table.list <- extr$list()
  message("List of available tables")
  print(class(table.list)) # should be a vector of table identifiers
  locators <- table.list[sample(length(table.list),3)]
  df <- extr$data(locators) # Reload the locator tables
  
  message("Extract list with details")
  raw.list <- extr$list(namesOnly=FALSE)
  print(class(raw.list))
  message("A subset of locators:")
  locators <- raw.list[sample(length(raw.list),3)]
  print(locators)
  testStep("Extract data.frames from tables in 'locators'")
  df <- extr$data(locators) # list of data.frames...
  print(class(df))       # Should be a list
  message("locator names:")
  print(names(locators))
  message("names of extracted data.frames")
  print(names(df))
  message("class and first ten rows of first data.frame: ",names(df)[1]," with ",nrow(df[[1]])," rows")
  print(class(df[[1]]))  # Should be a data.frame
  print(df[[1]][1:min(10,nrow(df[[1]])),])

  return(rs)
}

test_05_query_extract <- function(log="info") {
  testStep("Set up model")
  mod <- test_01_run("VERSPM-query",baseModel="VERSPM",variant="pop",log="warn")
  testStep("Build a query spec...")
  spec <- VEQuerySpec$new()
  QuerySpec <- list(
    list(
      Name = "TotalHhDvmt",
      Summarize = list(
        Expr = "sum(Dvmt)",
        Units = c(
          Dvmt = "MI/DAY"
        ),
        Table = "Household"
      ),
      Units = "Miles per day",
      Description = "Total daily vehicle miles traveled by households"
    ),
    list(
      Name = "MareaHhDvmt",
      Summarize = list(
        Expr = "sum(Dvmt)",
        Units = c(
          Dvmt = "MI/DAY",
          Marea = ""
        ),
        By = "Marea",
        Table = "Household"
      ),
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in each Marea"
    ),
    list(
      Name = "AzoneHhDvmt",
      Summarize = list(
        Expr = "sum(Dvmt)",
        Units = c(
          Dvmt = "MI/DAY",
          Azone = ""
        ),
        By = "Azone",
        Table = "Household"
      ),
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in each Azone"
    ),
    list(
      Name = "AzoneHhPopByInc",
      Summarize = list(
        Expr = "sum(HhSize)",
        Units = c(
          HhSize = "",
          Income = "USD",
          Azone = ""
        ),
        By = c("Azone","Income"),
        Breaks = list(
          Income = c(20000, 40000, 60000, 80000, 100000)
        ),
        BreakNames = list(
          Income = c("20000+", "40000+", "60000+", "80000+", "100000+")
        ),
        Table = "Household"
      ),
      Units = "Persons",
      Description = "Number of persons by income strata in each Azone"
    ),
    list(
      Name = "BzoneVehByPowertrain",
      Summarize = list(
        Expr = "count(VehId)",
        Units = c(
          VehId = "",
          HhId = "",
          Bzone = "",
          Powertrain = ""
        ),
        By = c("Bzone","Powertrain"),
        Table = list(
          Household = c("HhId","Bzone"),
          Vehicle = c("VehId","Powertrain")
        ),
        Key = "HhId"
      ),
      Units = "Vehicles",
      Description = "Number of vehicles by powertrain in each Bzone"
    )
  )
  qry <- VEQuery$new(QuerySpec=QuerySpec)
  print(qry)
  testStep("Force run the query...")
  qry$run(mod,Force=TRUE,log=log)
  testStep("Classic extraction...")
  extr <- qry$extract()
  print(extr[sample(nrow(extr),20),])
  testStep("Long extraction...")
  extr <- qry$extract(longScenarios=TRUE)
  print(nrow(extr))
  print(extr[sample(nrow(extr),min(nrow(extr),20)),])
  testStep("Export the long format as .csv")
  qry$export("csv",longScenarios=TRUE)
  return(qry)
}

test_05_build_query <- function(log="info",break.query=TRUE,reset=FALSE) {
  # Process the standard query list for the test model
  # If multiple==TRUE, copy the test model and its results a few times, then submit the
  # list of all the column copies to VEQuery. Each column of results will be the same (see
  # test_06_scenarios for a run that will generate different results in each column).
  # if break.query, do some deliberately bad stuff to see the error messages

  testStep("Set up Queries")
  testStep("Opening test model and caching its results...")
  mod <- test_01_run("VERSPM-query",baseModel="VERSPM",variant="pop",log="warn",reset=reset)

  testStep("Show query directory (may be empty)...")
  print(mod$query())

  testStep("Create an empty query object and print it...")
  # create a query object
  qry <- mod$query("Test-Query",load=FALSE) # Don't open it if file exists already
  cat("Query valid:",qry$valid(),"\n")
  cat("Print qry$checkResults:"); print(qry$checkResults)
  cat("Print query:\n")
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
  qry$add(VEQuerySpec$new(spec))
  qry$print(details=TRUE)

  testStep("Names of specifications in added query (just one)...")
  print(qry$names())    # List names of QuerySpecifications in order
  testStep("Print function for added queries...")
  print(qry)

  testStep("Re-add a query at the beginning of the list")
  print(qry)
  spec <- VEQuerySpec$new(spec)
  spec <- spec$update(Name="UrbanHhDvmt_before")
  print(spec)
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
  cat("Removing by name:\n")
  print( nm <- qry$names()[1:3] )
  qry$remove(nm) # remove by name (bye-bye before,loc2 and loc0)
  print(qry)
  cat("Removing by position:\n")
  cat("2: ",qry$names()[2],"\n",sep="")
  qry$remove(2) # remove by position (bye-bye loc45)
  print(qry)

  if ( break.query ) {
    testStep("Make a new VEQuery and add various bad specifications to it (not implemented)...")
    # TODO: Throw some additional specific broken queries at it to see if errors are correct.
    # TODO: destroy that object once we're done abusing it.
  }

  return(qry)
}

test_05_query <- function(log="info",Force=TRUE,runModel=FALSE) {
  # Test the basic query mechanism (yields only scalar results)

  testStep("Set up Queries and Run on Model Results")
  testStep("Opening test model and caching its results...")
  mod <- test_01_run("VERSPM-query",baseModel="VERSPM",variant="pop",log="warn",reset=runModel)

  testStep("Show query directory (may be empty)...")
  print(mod$query())

  qry <- mod$query("Test-Query",load=FALSE) # Don't open it if file exists already

  testStep("Build a query from scratch and run it.")
  testStep("Construct bare query specification...")
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

  testStep("Complete the initial query by adding more 'Summarize' specs...")

  # Just load a list of specifications straight into the query
  spec <- list(
    list(
      Name = "UrbanVanDvmt",
      Summarize = list(
        Expr = "sum(VanDvmt)",
        Units = c(
          VanDvmt = "MI/DAY",
          Marea = ""
        ),
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
        Table = "Marea"
      ),
      Units = "Miles per day",
      Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the urban area"
    ),
    list(
      Name = "UrbanHhDvmt",
      Summarize = list(
        Expr = "sum(UrbanHhDvmt)",
        Units = c(
          UrbanHhDvmt = "MI/DAY",
          Marea = ""
        ),
        Table = "Marea"
      ),
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in the urban area"
    )
  )
  qry$add(spec,location=1,after=TRUE)
  print(qry)

  testStep("Create a 'Function' query specification...")

  spec <- VEQuerySpec$new()
  spec$update(QuerySpec=list(
      Name = "UrbanLdvDvmt",
      Function = "UrbanHhDvmt + UrbanVanDvmt + UrbanComSvcDvmt",
      Units = "Miles per day",
      Description = "Sum of daily vehicle miles traveled in the urban area"
    )
  )
  cat("Function spec is valid (TRUE):"); print(spec$valid())
  print(spec)

  testStep("Add the Function spec to the query...")

  print(qry)
  qry$add(spec)
  print(qry)

  testStep("Clear test queries, if any...")
  qfiles <- grep("^(Test-Query|Copy-Query)",mod$query(),value=TRUE)
  print(qfiles)
  qfiles <- file.path(mod$modelPath,"queries",qfiles)
  unlink(qfiles)

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
  print(mod$query())

  testStep("Save a query somewhere else...")
  qfile <- file.path(mod$modelPath,"queries","Dump-Query.R")
  qry2$save(qfile)
  print(mod$query())

  testStep("Save a query without overwriting...")
  actualFile <- qry2$save(overwrite=FALSE)
  qfile <- c(qfile,actualFile)
  print(mod$query())
  unlink(qfile); rm(qry2)

  testStep("Open the query by short name in a different object from the file...")

  runqry <- mod$query("Test-Query")
  cat("Loaded query...\n")
  cat("Directory: "); print(runqry$QueryDir)
  cat("Name; "); print(runqry$QueryName)
  cat("Path: "); print(runqry$QueryFile)
  print(runqry)

  testStep("Open the query again from the file, using full file name...")

  runqry <- mod$query("Test-Query.VEQry")
  cat("Re-Loaded query with name extension...\n")
  cat("Directory: "); print(runqry$QueryDir)
  cat("Name: "); print(runqry$QueryName)
  cat("Path: "); print(runqry$QueryFile)
  print(runqry)
  rm(runqry)

  testStep("Run the query on the model...")
  qry$run(mod,Force=Force) # using original query above

  testStep("Display query results...")
  rs <- qry$results()
  cat("Number of query results:",length(rs),"\n")
  for ( r in seq(length(rs)) ) {
    cat("Result #",r,"\n")
    result <- rs[[r]]
    cat("  Path:",result$Path)
    cat("  Results:",paste(names(result$Results),collapse=", "),"\n")
    cat("  ModelStage Results:",result$Source$Name,"\n")
  }

  testStep("Extract query results into data.frame (wide format)")
  wdf <- query.results <- qry$extract(longScenarios=FALSE) # Constructs the data.frame of query results
  print(names(wdf))
  print(wdf)

  testStep("Extract query results into data.frame (long format)")
  ldf <- query.results <- qry$extract(longScenarios=TRUE) # Constructs the data.frame of query results
  print(names(ldf))
  print(ldf)

  testStep("Extract query results into .csv file (default name, wide format)")
  df <- qry$export("csv")
  df <- qry$export() # Does the same thing again, possibly overwriting
  # Each extract creates a new file with a different timestamp, but
  # the timestamps only differ by minutes

  testStep("Export query results into explicitly named .csv file (long format)")
  qry$export("csv",longScenarios=TRUE)

  testStep("Show output files, which will include exports and queries")
  mod$dir(outputs=TRUE,all.files=TRUE)

  testStep("Run the query again on the bare results (should do nothing)...")
  rs <- mod$results()
  qry$run(rs) # Won't re-run if query results are up to date with the scenario runs

  testStep("Force the query to run on the bare results rather than the model...")
  qry$run(rs,Force=TRUE) # Won't re-run if query is up to date

  testStep("Export just the data (wide format)...")
  qry$export("csv",longScenarios=FALSE)

  testStep("Extract just the metadata (long format)...")
  df <- qry$extract(wantMetadata=TRUE,wantData=FALSE,longScenarios=TRUE)
  print(df[sample(nrow(df),min(nrow(df),20)),]) # Random sample of 20 rows in "long" format

  testStep("Extract just the metadata (wide format)...")
  df <- qry$extract(wantMetadata=TRUE,wantData=FALSE,longScenarios=FALSE)
  print(df[sample(nrow(df),min(nrow(df),20)),]) # Random sample of 20 rows in "long" format

  testStep("Returning extracted query data.frame, in long format, for further exploration")
  return(
    list(
      Results = qry$extract(longScenarios=TRUE),
      Model = mod,
      Query = qry
    )
  )
}

# Test query dimensions and filtering
qrydir <- Sys.getenv("VE_test_source",unset=getwd())
qryfile <- normalizePath(file.path(qrydir,"Filter-Query.VEqry"),winslash="/",mustWork=FALSE)

# Test query filter mechanism (and basic query processing)
test_05_queryfilter <- function(runModel=FALSE,log="info") {
  testStep("Load base model")
  mod <- test_01_run("VERSPM-query",baseModel="VERSPM",variant="base",log=log,reset=runModel)

  testStep(paste("Loading test query:",basename(qryfile)))
  logLevel(log)
  qr <- mod$query(FileName=qryfile)
  print(qr)

  testStep("Run filter query (Force=TRUE)...")
  qr$run(Force=TRUE)

  testStep("Extract results")
  df <- qr$extract()
  qr$export()  # Write to output directory

  testStep("Show model outputs")
  print(mod$dir(outputs=TRUE,all.files=TRUE))
  
  invisible(mod)
}

# Torture test the query mechanism
# Give it a non-existent query via queryName, for example...
# TODO: need many more broken things to test...
test_06_fullquery <- function(Force=TRUE,runModel=FALSE,queryName="Full-Query",log="info") {
  logLevel("warn")
  testStep("Test Full-Query.VEqry")
  testStep("Opening test model and caching its results...")
  mod <- test_01_run("VERSPM-query",baseModel="VERSPM",variant="pop",log=log,reset=runModel)
  testStep("Loading Query")
  qry <- mod$query(queryName,load=TRUE)
  if ( qry$valid() ) {
    testStep("Run Query")
    logLevel(log)
    qry$run(Force=Force)
    testStep("Extract results (return invisible data.frame)")
    df <- qry$extract()
    if ( nrow(df) > 0 ) {
      print(names(df))
      print(nrow(df))
      return( invisible(df) )
    } # else fall through to return the qry itself for debugging
  } else {
    testStep("Full-Query.VEqry is not valid!")
    qry$check(verbose=TRUE)
  }
  invisible(qry)
}

test_06_quickquery <- function(model=NULL,log="info",multicore=3,reset=FALSE) {
  testStep("Set up scenarios-ms model for testing")
  if ( is.null(model) ) {
    model <- test_01_run(baseModel="VERSPM",variant="scenarios-ms",reset=reset,multicore=multicore,log="warn")
  }
  # Make quick queries following VDOT / NVTA model test
  # Marea queries
  Marea_Fields <- c(
    'UrbanHhDvmt',
    'TownHhDvmt',
    'RuralHhDvmt',
    'HvyTrkUrbanDvmt',
    'ComSvcUrbanDvmt',
    'ComSvcTownDvmt',
    'ComSvcRuralDvmt',
    'LdvTotDelay',
    'HvyTrkTotDelay',
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
  Household_Fields <- c(
    'Dvmt',
    'Income',
    'OwnCost',
    'WalkTrips',
    'VehicleTrips',
    'BikeTrips',
    'TransitTrips',
    'DailyGGE',
    'DailyKWH',
    'DailyCO2e'
  )
  
  testStep("Construct specifications")
  qry <- VEQuery$new(Model=model,QueryName="Quick-Query")

  Table <- "Marea"
  for ( Field in Marea_Fields ) {
    qry$quickSpec(Table,Field)
  }
  Table <- "Household"
  Geography <- "Azone"
  for ( Field in Household_Fields ) {
    qry$quickSpec(Table,Field,Geography=Geography)
  }
  print(qry)
  testStep("Run queries")
  qry$run(Force=TRUE,log=log)
  testStep("Generate results (wide format)")
  wideResults <- qry$extract(longScenarios=FALSE)
  testStep("Generate results (long format)")
  longResults <- qry$extract(longScenarios=TRUE)
  message("Returning invisible list of list(wide=,long=)")
  invisible(list(query=qry,wide=wideResults,long=longResults))
}

# Construct programmatic stages, run the model, and query it
test_06_addstages <- function(reset=FALSE,log="info") {
  # Merge this with test_scenario
  if ( ! missing(log) ) logLevel(log)
  testStep("Acquiring test model and test query")
  results <- test_05_query(log=log,Force=reset,runModel=reset)
  mod <- results$Model
  
  testStep("Build multiple scenarios...")
  # Generate several copies of mod future year
  # Inputs will be sought up the "StartFrom" tree.
  # To customize inputs for Scenario-1 (as an actual scenario),
  #   create InputDir ("inputs") inside stagePath.1 and put in
  #   just the files you want to change. Without such an input,
  #   it just uses the inputs found in earlier stages. For the
  #   purposes of testing the query functionality, it suffices here
  #   to have all the scenarios be the same.
  print(mod$modelPath)
  stagePath.1 <- file.path(mod$modelPath,"Scenario-1")
  if ( ! dir.exists( stagePath.1 ) ) dir.create(stagePath.1)
  cat("Adding Stage 1\n")
  mod$addstage(
    Name="Scenario-1",
    Dir="Scenario-1",
    # TODO: InputPath = NULL, # from one of the category test scenarios
    Scenario="Scenario 1",
    Description="Same as original...",
    StartFrom="stage-pop-future",
    BaseYear=mod$setting("BaseYear",stage="stage-pop-future"),
    Years=mod$setting("Years",stage="stage-pop-future"),
    ModelScript=mod$setting("ModelScript",stage="stage-pop-future")
  )
  cat("Adding Stage 2\n")
  stagePath.2 <- file.path(mod$modelPath,"Scenario-2")
  if ( ! dir.exists( stagePath.2 ) ) dir.create(stagePath.2)
  mod$addstage(
    Name="Scenario-2",
    Dir="Scenario-2",
    # TODO: InputPath = NULL, # from a different category test scenario
    Scenario="Scenario 2",
    Description="Same as original...",
    StartFrom="stage-pop-future",
    BaseYear=mod$setting("BaseYear",stage="stage-pop-future"),
    Years=mod$setting("Years",stage="stage-pop-future"),
    ModelScript=mod$setting("ModelScript",stage="stage-pop-future")
  )

  testStep("Model should have stages")
  print(mod)

  # Force Reportable on stage-pop-future (auto-detect says no since
  # the other scenarios start from it).
  mod$modelStages[["stage-pop-future"]]$Reportable=TRUE

  # NOTE: without an InputDir or a different InputPath or different Script, this will just re-run
  # the model using all the inputs from stage-pop-future and put the results in this stage's output
  # directory.
  testStep("Running two additional scenarios")
  mod$run() # runs with "continue" - will just do the newly added stages

  testStep("Query the model")

  cat("Available queries:\n")
  print(mod$query())

  qry <- mod$query("Test-Query")
  print(qry)

  # Make a list of VEResults objects from the VEModel list and run query on it
  qry$run(mod,Force=TRUE) # might be leftovers from another query test
  extract <- qry$extract()
  qry$export()

  print(mod$dir(outputs=TRUE))

  testStep("Query the results list explicitly (should use cached query results)")
  # Make a list of ResultsDir path names (i.e. list of character strings) from the
  # VEResults and query that (Note difference between a character vector - list of
  # model names and a list of character strings, which are the result paths).
  qry$run(mod$results())
  testStep("Query results in a data.frame")
  print( qry$extract()[,-2:-3] )

  testStep("Export and List outputs directory contentss")
  qry$export()
  print(mod$dir(outputs=TRUE))

  testStep("Done with programmatic stage tests")
  return(qry)
}

test_07_load <- function(log="info" ) {
  # Tests the LoadModel functionality (pre-load Datastore and then
  #   execute additional steps from the copied data.all)
  testStep("Finding BARE model template")

  baseModelName <- 
  model <- test_02_model(brief=TRUE,log=log) # skip the deeper tests
  model$run(log=log)                         # run it anyway (default="continue" does nothing)

  testStep("Copy model...")
  modelPath <- file.path("models","LOAD-test")
  if ( dir.exists(modelPath) ) unlink(modelPath,recursive=TRUE)
  loadModelName <- "LOAD-test"
  loadModel <- model$copy(loadModelName,copyResults=FALSE)
  print(loadModel)
  testStep("Set up load script...")
  baseModelPath <- loadModel$setting("ModelDir",shorten=FALSE)
  runModelFile <- file.path(
    baseModelPath,
    loadModel$setting("ModelScript")
  )
  # These are the steps from VERSPM that would follow those in the BARE model test
  runModel_vc <- c(
    '',
    'for(Year in getYears()) {',
    'runModule("AssignLifeCycle","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictIncome", "VESimHouseholds", RunFor = "AllYears", RunYear = Year)',
    'runModule("PredictHousing", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    'runModule("LocateEmployment", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    'runModule("AssignLocTypes", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    '}'
  )
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  testStep("Configure LoadModel")
  # We'll just do it from scratch rather than reading/modifying
  # Region, BaseYear and Years don't change
  runConfig_ls <-  list(
    Model       = jsonlite::unbox("LOAD Model Test"),
    Scenario    = jsonlite::unbox("Test LoadModel / LoadDatastore"),
    Description = jsonlite::unbox("Add a step onto a different previous model"),
    Region      = jsonlite::unbox(model$setting("Region")),
    BaseYear    = jsonlite::unbox(model$setting("BaseYear")),
    Years       = model$setting("Years"), # don't unbox - expecting a list
    LoadModel   = jsonlite::unbox(model$modelPath) # could be any form accepted by openModel
    # Could also set LoadStage (last Reportable stage is used by default)
  )
  configFile <- file.path(baseModelPath,"visioneval.cnf")
  yaml::write_yaml(runConfig_ls,configFile)

  testStep(paste("Reload model",loadModelName," with LoadDatastore"))
  loadModel <- openModel(loadModelName,log=log)

  testStep("Acquiring base model for additional inputs")
  # should be the same model used in test_02_mode.()
  base.model <- test_00_install(modelName="VERSPM",variant="base",installAs="VERSPM-Test",overwrite=FALSE,confirm=FALSE)

  testStep("Copy additional inputs")
  base.inputs <- base.model$setting("InputPath",shorten=FALSE)
  
  cat("Base Inputs",base.inputs,"\n")
  inputs <- loadModel$list(inputs=TRUE,details=c("FILE","INPUTDIR")) # needed files comes from new model
  required.files <- unique(file.path(base.inputs,inputs[,"FILE"]))   # location comes from base model
  required.files <- required.files[which(file.exists(required.files))]
  cat("Required Files:\n")
  print(basename(required.files))

  testStep("Copying additional input files")
  bare.inputs <- file.path(loadModel$setting("InputPath",shorten=FALSE))

  testStep("Remove base model inputs - will just have new ones")
  unlink(bare.inputs,recursive=TRUE)
  dir.create(bare.inputs)
  from <- required.files
  file.copy(from=from, to=bare.inputs )

  testStep("Copy model parameters to 'inputs' - could also be in 'defs')")
  from <- file.path(base.inputs,"model_parameters.json")
  file.copy(from=from,to=bare.inputs) # or copy to bare.defs...
  print(dir(bare.inputs))

  testStep("Run model, loading datastore")
  loadModel$run("reset",log=log)
  print(loadModel)

  return(invisible(loadModel))
}

# TODO: once scenario testing is complete, add a test for the visualizer (writing to file
# and also launching with jrc), check that we can do VERPAT all the way (set up the
# queries in the new structure, set up the category scenarios variant, and make sure
# it can run all the way through (model run, query generation on single scenario,
# category scenario generation, visualizer on multiple scenarios)
test_07_verpat <- function(run=TRUE) {
  testStep("Testing VERPAT as JRPAT")
  verpat <- openModel("JRPAT")
  if ( ! verpat$valid() ) {
    testStep("Installing VERPAT as JRPAT")
    verpat <- installModel("VERPAT",modelPath="JRPAT",confirm=FALSE)
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

# Test the setup management functions
test_08_setup <- function(model=NULL) {
  testStep("Raw parameter defaults (named list with attributes)...")
  print(visioneval::defaultVERunParameters())

  testStep("Viewing parameter defaults in data.frame format")
  viewSetup(Param_ls=visioneval::defaultVERunParameters())

  testStep("Global runtime setup: visioneval.cnf within ve.runtime")
  conf.file <- file.path(runtimeEnvironment()$ve.runtime,"visioneval.cnf")
  if ( file.exists(conf.file) ) unlink(conf.file)
  getSetup(reload=TRUE) # Loaded an empty file

  testStep("View initial global runtime setup (empty list)")
  viewSetup()

  testStep("Install test model (based on runtime configuration)...")
  setup <- test_00_install("VERSPM","pop",installAs="Setting-Test",overwrite=TRUE)

  testStep("Initial model configuration (notice explicit Seed)")
  viewSetup(setup)
  cat("\nSeed setting in model (source is model's visioneval.cnf):\n")
  print(setup$setting("Seed",source=TRUE))
  
  testStep("Remove Seed parameter from model configuration file")
  updateSetup(setup,drop="Seed")
  writeSetup(setup,overwrite=TRUE) # changes model's visioneval.cnf - careful becuase it trashes comments
  cat("Notice: no Seed listed explicitly; will use global or default\n")
  viewSetup(setup)

  testStep("Reload model, now without explicit Seed")
  setup$configure(reloadFile=TRUE) # or use openModel again; the latter is better if model has been run
  cat("Value of Seed:",setup$setting("Seed",defaults=TRUE),"\n")
  cat("Source of Seed setting...\n")
  print(setup$setting("Seed",defaults=TRUE,source=TRUE)) # Show source for Seed parameter: now default

  testStep("Current loaded model setup...")
  viewSetup(setup)

  testStep("Update Seed for global visioneval.cnf")
  updateSetup(Seed=2.5)

  testStep("View runtime setup with changed Seed (what would be in the file)")
  viewSetup()

  testStep("Save new runtime visioneval.cnf")
  writeSetup() # creates a backup file if setup already exists

  cat("Existing runtime configuration in memory (empty list:  not loaded):\n")
  viewSetup(fromFile=FALSE) # Still has no value for Seed in global runtime

  cat("Runtime configuration after reloading from file (now has value):\n")
  getSetup(reload=TRUE) # force reload of runtime configuration
  viewSetup(fromFile=FALSE) # Now has reloaded file value in regular runtime
  # could combine those two steps with viewSetup(fromFile=TRUE)

  testStep("Reopen model and see changed setup (Seed = 2.5) from global")
  setup$configure(reloadFile=TRUE) # Reopen the model from saved configuration
  cat("Seed setting that will be used (global config):\n")
  print(setup$setting("Seed",defaults=TRUE,source=TRUE))

  testStep("Removing global configuration file for ve.runtime")
  unlink(conf.file) # Don't leave the runtime visioneval.cnf around
  getSetup(reload=TRUE)

  testStep("Reopen model one more time and Seed is now back to default")
  setup$configure(reloadFile=TRUE) # Reopen the model from saved configuration
  cat("Seed setting that will be used (default):\n")
  print(setup$setting("Seed",defaults=TRUE,source=TRUE))

}

test_06_scenarios <- function(
  useStages=TRUE,
  run=useStages,
  querySpec="VERSPM-scenarios",
  install=FALSE,
  multicore=TRUE, # or set to number of workers (default is 3)
  log="info"
) {
  logLevel(log)

  scenarioVariant <- if (useStages) "scenarios-ms" else "scenarios-cat"
  scenarioModelName <- paste0("VERSPM-",scenarioVariant)
  running <- if ( run) ", installing, and running" else " and installing"
  testStep(paste(paste0("Selecting",running," scenarios as",if(useStages)"Model Stages"else"Scenario Combinations")))
  existingModel <- dir.exists(modelPath <- file.path("models",scenarioModelName))
  if ( run ) {
    mod <- test_01_run(scenarioModelName,baseModel="VERSPM",variant=scenarioVariant,reset=install,log=log,confirm=FALSE,multicore=multicore)

    testStep("Loading scenario query")
    qr <- mod$query(querySpec) # Fails if model has not been run
    qf <- qr$QueryFile
    print(qr)
    cat("QueryFile:",qf,"\n")

    testStep("Running Query")
    qr$run(Force=TRUE,log="info")
    print(qr)

    testStep("Examine Query Results")
    qrr <- qr$results()
    print(class(qrr))
    print(length(qrr))

    testStep("Extracting Query Results")
    qrs <- qr$extract()
    print(qrs)

    testStep("Exporting Query Results")
    qr$export()

    testStep("Returning scenario model")
    print(mod,scenarios=TRUE)
    return(invisible(list(
      Model=mod, Query=qr, QueryFile=qf, QueryResults=qrs
    )))
  } else {
    mod <- test_00_install("VERSPM",variant=scenarioVariant,installAs=scenarioModelName,log=log,confirm=FALSE)
    return(mod)
  }
}

test_06_scenario_results <- function(
  install=FALSE,
  full.extract = TRUE,
  multicore=TRUE, # or set to number of workers (default is 3)
  log="warn"
) {
  logLevel(log)

  mod <- test_01_run("VERSPM-scenarios-ms",baseModel="VERSPM",variant="scenarios-ms",reset=install,log=log,confirm=FALSE,multicore=multicore)
  testStep("Examine Model Results")
  rs <- mod$results()
  print(class(rs))

  testStep("Printing scenario model")
  print(mod)
  testStep("Printing scenario model results")
  print(rs)

  if ( full.extract ) {
    testStep("Extracting model Results for All Scenarios")
    rs.extract <- rs$extract()
    cat("Names of resulting tables:\n")
    print(names(rs.extract))
  }

  testStep("Selecting Scenarios Design-1 and Design-2 with find")
  select <- rs$select()
  cat("Printing sample of selected fields...\n")
  flds <- select$find(Scenario=c("Design-1","Design-2"),select=TRUE)$fields()
  print(flds[sample(length(flds),20)])

  testStep("Does the selection propagate to the results?")
  print(rs$select()$scenarios())

  testStep("Selecting Scenarios Design-1 and Design-2 indirectly")
  select2 <- rs$select()$find(Scenario=c("Pricing-1","Pricing-2"))
  print(select2$scenarios())

  testStep("Does the selection propagate to the results? NO")
  print(rs$select()$scenarios())

  testStep("Select pricing again, this time with select=TRUE")
  select2 <- rs$select()$find(Scenario=c("Pricing-1","Pricing-2"),select=TRUE)
  print(select2$scenarios())

  testStep("Does the selection propagate to the results? YES")
  print(rs$select()$scenarios())

  partition <- c(Global="path",Scenario="name",year="name")
  
  testStep("Exporting model Results with implicit selection")
  rs$export(partition=partition,connection=list(Database="Pricing_Implicit"))

  testStep("Exporting model Results from explicit selection")
  rs$export(selection=select,partition=partition,connection=list(Database="Design_Explicit"))

  testStep("Exporting model Results just for the Year groups") # implicit selection
  select$find(Scenario=c("Design-1","Design-2"),Group="Year",select=TRUE)
  rs$export(selection=select,partition=partition,connection=list(Database="Design_Years_Only"))
  
  testStep("Directory after export")
  print(mod$dir(outputs=TRUE,all.files=TRUE))

  invisible(list(Model=mod,Results=rs))
}

test_07_extrafields <- function(reset=FALSE,installSQL=TRUE,log="info") {

  testStep("Will modify a version of VERSPM-base as VERSPM-export")
  mod <- test_01_run("VERSPM-export",reset=reset,log="warn")
  print(mod)

  testStep("Get existing geo.csv and see if TagField is there")
  # Could also consider looking for Geo_df in the model state for the first stage
  paramPath <- mod$setting("ParamPath",shorten=FALSE)
  geoFile <- mod$setting("GeoFile")
  geoPath <- file.path(paramPath,geoFile)
  if ( ! file.exists(geoPath) ) stop("Failed to locate geo.csv")
  Geo_df <- read.csv(geoPath, colClasses="character") # currently will only support character field extensions
  print(geoNames <- names(Geo_df))
  hasTagField <- "TagField" %in% geoNames

  if ( hasTagField ) {
    testStep("Also check that the model has been run and output includes TagField")
    geoNames <- mod$results()$find(Group="Global",Table="Bzone")$fields()
    cat("Show geography names in model output (if any)\n")
    print(geoNames)
    hasTagField <- any(grepl("/TagField$",geoNames))
    cat("Has TagField:",hasTagField,"\n")
  }
  
  if ( ! hasTagField ) {
    testStep("Building TagField: Tag half the Bzones randomly with a new label")
    TagField <- sample(paste("Tag",1:2,sep="_"),nrow(Geo_df),replace=TRUE)
    Geo_df$TagField <-TagField
    print(geoNames <- names(Geo_df))
    write.csv(Geo_df,file.path(paramPath,geoFile),row.names=FALSE,na="NA")

    testStep("Re-open the model and run it to add updated geography to Datastore")
    mod <- openModel("VERSPM-export")
    mod$run("reset",log="warn")
  }

  testStep("See Global/Bzone/TagField in selection fields")
  rs <- mod$results()
  print( sl <- rs$find(Group="Global",Table="Bzone",select=TRUE) )
  # Tables with Bzone in them will also get the extra fields.
  # Doublecheck that extra fields show up in Households (for example)

  if ( ( ! require(DBI) || ! require(RSQLite) ) && installSQL ) {
    testStep("Install DBI and RSQLite...")
    install.into <- .libPaths()[1]     # Pick a better lib location if you have one
    install.packages("DBI",lib=install.into)
    require(DBI)
    install.into <- .libPaths()[1]     # Pick a better lib location if you have one
    install.packages("RSQLite",lib=install.into)
    require(RSQLite)
  }
  testStep("Extract the Bzone tables and review")
  df.list <- rs$extract() # df.list is a list of data.frames, one for each table
  print(names(df))
  print(df.list[[1]][sample(nrow(df.list[[1]]),10),])

  testStep("Construct a query that does multi-level breakpoints on Azone + Field Tags")

  spec <- list(
    list(
      Summarize = list(
        Expr = "sum(Dvmt)",
        By = c("Azone","TagField"),
        Units = c(
          Bzone = "",
          Azone = "",
          TagField = "",
          Dvmt = "MI/DAY"
        ),
        Table = list(
          Household = c("Dvmt","Bzone"),
          Bzone = c("Azone","TagField")
        ),
        Key = "Bzone"
      ),
      Name = "VMTbyAzoneTagField",
      Description = "VMT broken out by Azone and TagField",
      Units = "Daily Household VMT"
    )
  )
  print(spec)
  qry <- VEQuery$new(QueryName="TagField-Query",Model=mod,QuerySpec=spec)
  print(qry)
  qry$save()
  mod$query() # list available queries; should include TagField-Query.VEqry
  mod$dir()

  testStep("Run the query")
  qry$run(Force=TRUE)
  print(qry)

  testStep("Display table of query results (wide format)")
  print(qry$extract())

  testStep("Display table of query results (long format)")
  print(qry$extract(longScenarios=TRUE))

  return(mod)
}

test_08_visual <- function(popup=FALSE,reset=FALSE,log="info") {

  scenarioVariant <- "scenarios-cat"
  scenarioModelName <- paste0("VERSPM-",scenarioVariant)
  testStep(paste("Selecting, installing, and running scenarios as Scenario Combinations"))

  existingModel <- dir.exists(modelPath <- file.path("models",scenarioModelName))
  mod <- test_01_run(scenarioModelName,baseModel="VERSPM",variant=scdenarioVariant,reset=install,log=log,confirm=FALSE)

  # TODO: Ensure models created with test_00_install...
  testStep("Opening Query")
  logLevel(log) # Now examine at requested log level
  qr <- mod$query( mod$query() )
  print(qr)

  testStep(paste0("Running Query, Force=",reset))
  results <- qr$run(Force=reset)

  testStep("Extracting Query (prints data.frame)")
  extract <- qr$extract(metadata=FALSE,exportOnly=TRUE)
  print( extract )

  testStep("Building visual data")
  jsonvars <- qr$visual(QueryResults=extract) # pure extraction return jsonvars
  if ( popup ) {
    testStep("Launching jrc visualizer")
    qr$visual(SaveTo=NULL) # Popup visualizer
  } else {
    testStep("Writing file-system visualizer")
    qr$visual(SaveTo=TRUE) # save to sub-directory of ResultsDir/OutputDir
  }
  invisible( jsonvars )
}
