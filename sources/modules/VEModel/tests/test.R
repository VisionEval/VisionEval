# Test.R
# Comprehensively test VEModel and related interfaces
# Also provides working examples of the API

# function: create_test_environment
# Expects to run from <pkg>/tests; looks in parent directory
# Creates a temporary directory and initializes it as a ve.runtime
# Sets the working directory to that temporary location

# function: pseudo_package
# 
if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}
if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}

setup <- function(ve.runtime=NULL) {
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
}

cleanup <- function() {
  takedown()
  runtimes <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)
  message("Removing:")
  print(runtimes)
  if ( length(runtimes)>0 && isTRUE(askYesNo("Remove runtimes?")) ) unlink(runtimes,recursive=TRUE)
}

test_model <- function(log="warn") {
  owd <- getwd()
  tryCatch(
    {
      if ( dir.exists("models/JRSPM") ) {
        message("Clearing runtime environment")
        unlink("models/JRSPM",recursive=TRUE)
      }
      rs <- installModel("VERSPM","JRSPM",log=log)
      rs$run(log=log)
      return(rs)
    },
    error=function(e) { cat(conditionMessage(e),"\n"); takedown(); stop(e) },
    finally=setwd(owd)
  )
  return("Failed to run.")
}

test_results <- function (log="warn") {
  jr <- openModel("JRSPM")
  sl <- jr$results()$select()

  # Test display units, select speeds, create unit conversion
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

  sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
  sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
  cat("Exporting fields:\n")
  print(sl$fields())
  cat("Exporting speed fields using DISPLAY units\n")
  sl$export(prefix="DisplayUnits")                 # Using DISPLAY units
  cat("Exporting speed fields using DATASTORE units\n")
  sl$export(prefix="Datastore",convertUnits=FALSE) # Using DATASTORE units
}
