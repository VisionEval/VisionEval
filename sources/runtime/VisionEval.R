# Author: Jeremy Raw

# VisionEval runtime initialization script

# May need to run a second time to complete installation on Mac or Linux

# The working directory when this script is run is the "load directory"
# The load directory should have "tools", "walkthrough" and "models" subdirectories
# "models" will be created if it does not exist.
# After loading, if there is a ve.runtime set, that will become the "run directory"

require(utils,quietly=TRUE)                 # For install package

# Establish visioneval R environment on the R search path
env.loc <- if ( ! "ve.env" %in% search() ) {
  attach(NULL,name="ve.env")
} else {
  as.environment("ve.env")
}

# Development environment may have set an alternate location
if ( ! exists("ve.runtime",envir=env.loc) ) {
  env.loc$ve.runtime <- Sys.getenv("VE_RUNTIME",getwd())
}

# Set up the load directory

if ( ! exists("ve.load.dir",envir=env.loc,inherits=FALSE ) ) {
  assign("ve.load.dir",getwd(),envir=env.loc)
}
if ( env.loc$ve.load.dir != getwd() ) {
  message("Loading from other directory: ",ve.load.dir)
}

# Check the R version (redundant on Windows, but saves having to
# have a separate VisionEval.R for Linux/Mac)

local({
  ve.incomplete <- "VisionEval environment is unavailable; please re-install."
  r.version = file.path(ve.load.dir,"r.version")
  if ( ! file.exists(r.version) ) {
    message("Missing r.version file in ",ve.load.dir)
    stop(ve.incomplete)
  }
  ve.vars <- data.frame(
    scan(file=r.version,
      sep=":",
      what=list( var=character(), var=character() ),
      quiet=TRUE
    ), stringsAsFactors=FALSE
  )
  this.R <- paste(R.version[c("major","minor")],collapse=".")
  if ( nrow(ve.vars)==0 ) {
    message("r.version is empty")
    stop(ve.incomplete)
  }

  for ( i in 1:nrow(ve.vars) ) {
    # Expect ve.vars[i,1] == "that.R"
    # Expect ve.vars[i,2] == R version like what comes from system R.version
    assign(ve.vars[i,1],ve.vars[i,2],envir=env.loc)
  }

  if ( ! exists("that.R") || ! nzchar(that.R[1]) ) {
    message("Missing R version from r.version")
    stop(ve.incomplete)
  }
  if ( this.R != that.R ) {
    stop("Incorrect R version for this VisionEval installation: expecting R",that.R)
  } else {
    message("Loading VisionEval for R",this.R)
  }
})

# Check what installation we need
# Presume that getwd() is the installed runtime folder, which will either contain or
# be adjacent to a ve-lib or ve-pkg folder with the required packages.

# Locate ve-lib
env.loc$checkVE <- function(lib.loc=NULL) {
  return(all( c("visioneval","VEModel") %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
}

install.success <- env.loc$checkVE()

if ( ! exists("ve.lib.name") ) ve.lib.name <- "ve-lib"
ve.lib <- file.path(ve.load.dir,ve.lib.name)

# Create function to check or perform installation
env.loc$ve.install <- if ( ! install.success ) {
  function() {

    # Put the library directory into ve.lib
    # Note that ve.lib is already present and fully provisioned in the Windows offline installer

    install.success <- env.loc$checkVE(ve.lib)

    if ( ! install.success ) {
      ve.lib.dev <- normalizePath(file.path(ve.load.dir,"..",ve.lib.name),winslash="/",mustWork=FALSE)
      install.success <- env.loc$checkVE(ve.lib.dev)
      if ( install.success ) ve.lib <- ve.lib.dev # use development environment
    } # else ve.lib is working

    if ( ! install.success ) {
      # Attempt to install from package sources in ve-pkg
      if ( ! exists("ve.pkg.name") ) ve.pkg.name <- "ve-pkg"
      ve.pkg <- file.path(ve.load.dir,ve.pkg.name)
      if ( ! dir.exists(ve.pkg) ) {
        ve.pkg.local  <- normalizePath(file.path(ve.load.dir,"..",ve.pkg.name),winslash="/",mustWork=FALSE)
        if ( dir.exists(ve.pkg.local) ) ve.pkg <- ve.pkg.local
      }
      if ( ! dir.exists(ve.pkg) ) {
        message("Unable to locate ",ve.lib.name," or ",ve.pkg.name," in the file system.")
        message("VisionEval packages are not available.")
        stop("Installation failed - check error and warning messages.")
      }

      # Set up installation for current platform
      message("ve-lib not found. Installing from ve-pkg...")
      ve.pkg.type <- .Platform$pkgType
      ve.contrib.url <- contrib.url(ve.pkg,type=ve.pkg.type) # binary (Windows, MacOSX) or source (Unix)
      if ( ! dir.exists(ve.contrib.url) ) { # contrib.url does not have a binary branch
        ve.pkg.type = "source"
        ve.contrib.url <- contrib.url(ve.pkg,type=ve.pkg.type) # for source install on Windows or Mac
      }

      # Check if the packages are complete
      VE.pkgs <- available.packages(contriburl=paste0("file:",ve.contrib.url),type=ve.pkg.type)[,"Package"]
      if ( ! all( c("visioneval","VEModel") %in% VE.pkgs ) ) {
        message(paste("VisionEval not present in",ve.contrib.url))
        message("VisionEval packages are not available.")
        stop("Installation failed - check error and warning messages.")
      }

      # Install to local environment
      if ( ! dir.exists(ve.lib) ) dir.create(ve.lib)

      message("Installing packages from ",paste0("file:",ve.pkg))
      install.packages(
          VE.pkgs,
          lib=ve.lib,
          repos=paste0("file:",ve.pkg),
          dependencies=c("Depends", "Imports", "LinkingTo"),
          type=ve.pkg.type,
          INSTALL_opts="--no-test-load"
      )
    }
    install.success <- env.loc$checkVE(ve.lib)

    if ( install.success ) {
      # Set .libPaths()
      .libPaths(c(ve.lib,.libPaths()))
      invisible(TRUE)
    } else {
      message("Installation failed.")
      invisible(FALSE)
    }
  }
} else {
  function() {
    # Set .libPaths()
    .libPaths(c(ve.lib,.libPaths()))
    invisible(TRUE)
  }
}

# Do the installation and set up .libPaths
if ( .Platform$OS.type == 'windows' || install.success ) {
  install.success <- env.loc$ve.install()
}

# Load visioneval
if ( install.success ) {
  require("VEModel") # load explicitly onto the search path

  # Load tools (helper functions) from their subdirectory in ve.load.dir
  env.loc$load.helpers <- function() {
    requireNamespace("import",quietly=TRUE)   # to load the tools
    ve.tools <- file.path(ve.load.dir,"tools")
    tool.files <- file.path(ve.tools,dir(ve.tools,pattern="\\.R$"),fsep="/")
    if ( length(tool.files)>0 ) {
      tools <- character(0)
      for ( tf in tool.files ) {
        # Add error checking for tool.contents not present
        message("Loading tool file: ",tf)
        try(
          silent=TRUE,
          eval(parse(text=paste0("import::here(tool.contents,.from='",tf,"')")))
        )
        if ( ! exists("tool.contents") ) next
        eval(parse(text=paste0("import::into(.into='ve.env',",paste(tool.contents,collapse=","),",.from='",tf,"')")))
        rm(tool.contents)
      }
      rm(tf,tools)
    }
    rm(tool.files,ve.tools)
  }
  env.loc$load.helpers()

  # create the loadTest function (makes package test functions available)
  env.loc$loadTest <- function(Package=NULL,files=NULL,clear=FALSE) {
    test.root <- file.path(ve.load.dir,"tools","tests")
    if ( !is.character(Package) ) {
      tests <- dir(test.root,pattern="\\.R$",recursive=TRUE)
      if ( length(tests) == 0 ) {
        tests <- "No package tests available"
      }
      return(tests)
    }

    # Environment to receive test functions/objects
    test.env <- if ( ! "ve.tests" %in% search() ) {
      attach(NULL,name="ve.tests")
    } else {
      as.environment("ve.tests")
    }

    if ( clear ) {
      to.clear <- ls(test.env,all=TRUE)
      if ( length(to.clear)>0 ) rm(list=to.clear,envir=test.env)
    }

    for ( pkg in Package ) {
      test.dir <- file.path(test.root,Package)
      if ( dir.exists(test.dir) ) {
        if ( !is.character(files) ) {
          tests <- dir(test.dir,pattern="\\.R$",full.names=TRUE)
        } else tests <- files
        for ( test in tests ) {
          message("Loading tests from ",test," for ",Package)
          sys.source(test,envir=test.env)
        }
      } else {
        message("No tests available for Package ",Package)
      }
    }
    return( objects(test.env) )
  }

  # function to set up the walkthrough and its runtime
  env.loc$walkthrough <- function(reset=FALSE) {
    # Locate walkthrough directory (sub-directory of ve.runtime)
    if ( ! dir.exists("walkthrough") ) { # in case we're not in ve.runtime
      if ( dir.exists(env.loc$ve.runtime) ) {
        setwd(env.loc$ve.runtime)
      }
      if ( ! dir.exists("walkthrough") ) {
        stop("Walkthrough is not available in ",env.loc$ve.runtime)
      }
    }
    setwd("walkthrough") # Go there

    # Load the setup to create the walkthrough runtime if one is not already present
    # Will stop in normalizePath if setup.R is not present in getwd()
    message("setwd(ve.runtime) to exit walkthrough environment")
    message("Loading walkthrough from ",normalizePath("00-setup.R",winslash="/",mustWork=TRUE))
    source("00-setup.R") # will stop if we cannot create or change to walkthrough runtime directory
    walkthroughScripts <- grep("00-setup.R",invert=TRUE,value=TRUE,dir("..",pattern="^[01].*\\.R$",full.names=TRUE))
    message("Open these script files in order and try out the commands:")
    print(walkthroughScripts)
  }

  # Change to the runtime directory
  if ( dir.exists(env.loc$ve.runtime) ) setwd(env.loc$ve.runtime) else env.loc$ve.runtime <- getwd()
  message("Running in ",env.loc$ve.runtime)

  # Make sure there is a "Models" directory in the actual runtime folder
  env.loc$ModelRoot <- file.path(
    env.loc$ve.runtime,
    visioneval::getRunParameter("ModelRoot") # Uses runtime configuration or default value "models"
  )
  if ( ! dir.exists(env.loc$ModelRoot) ) dir.create(env.loc$ModelRoot,recursive=TRUE,showWarnings=FALSE)

} else {
  # We need the following for the Mac or Linux to do the installation in "user space"
  # and not hang up RStudio for a long time while everything compiles.
  # Making the user call a function is better UX than having them wait for a long time with no
  # explanation
  message("Please run ve.install() to complete installation.")
}

# clean up variables created during startup
if ( exists("ve.lib",inherits=FALSE) ) rm(ve.lib)
rm(env.loc,install.success)
