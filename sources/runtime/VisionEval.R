# Author: Jeremy Raw

# VisionEval runtime initialization script
# Run once to install everything and build RunVisionEval.Rdata
# Configure alternate library locations (e.g. development environment
# through .Renviron): do not rewrite if it already exists. And when
# starting the VisionEval .Rproj, load up the environment from the
# project root.

require(utils,quietly=TRUE)                 # For install package
requireNamespace("import",quietly=TRUE)     # for tools implementation
requireNamespace("visioneval",quietly=TRUE)

# Establish visioneval R environment on the R search path
env.loc <- if ( ! "ve.env" %in% search() ) {
  attach(NULL,name="ve.env")
} else {
  as.environment("ve.env")
}

# Check the R version (redundant on Windows, but saves having to
# have a separate VisionEval.R for Linux/Mac)

local({
  ve.incomplete <- "VisionEval environment is unavailable; please re-install."
  if ( ! file.exists("r.version") ) {
    message("Missing r.version file.")
    stop(ve.incomplete)
  }
  ve.vars <- data.frame(
    scan(file="r.version",
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
    message("Loading VisionEval for R",this.R,"")
  }
})

# Set the runtime directory (could be overridden from development environment)
if ( ! exists("ve.runtime") ) {
  assign("ve.runtime",getwd(),envir=env.loc)
} else {
  setwd(ve.runtime)
}
# Check what installation we need

# Locate ve-lib
env.loc$checkVE <- function(lib.loc=NULL) {
  return(all( c("visioneval","VEModel") %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
}

install.success <- env.loc$checkVE()

if ( ! exists("ve.lib.name") ) ve.lib.name <- "ve-lib"
ve.lib <- file.path(ve.runtime,ve.lib.name)

local( {

  # Load tools (helper functions) from their subdirectory
  env.loc$load.helpers <- function() {
    ve.tools <- file.path(ve.runtime,"tools",fsep="/")
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

  # Create function to check or perform installation
  env.loc$ve.install <- if ( ! install.success ) {

    function() {

      # Put the library directory into ve.lib
      # Note that ve.lib is already present and fully provisioned in the Windows offline installer

      install.success <- env.loc$checkVE(ve.lib)

      if ( ! install.success ) {
        ve.lib.dev <- normalizePath(file.path(ve.runtime,"..",ve.lib.name),winslash="/",mustWork=FALSE)
        install.success <- env.loc$checkVE(ve.lib.dev)
        if ( install.success ) ve.lib <- ve.lib.dev # use development environment
      } # else ve.lib is working
      
      if ( ! install.success ) {
        # Attempt to install from package sources in ve-pkg
        if ( ! exists("ve.pkg.name") ) ve.pkg.name <- "ve-pkg"
        ve.pkg <- file.path(ve.runtime,ve.pkg.name)
        if ( ! dir.exists(ve.pkg) ) {
          ve.pkg.local  <- normalizePath(file.path(ve.runtime,"..",ve.pkg.name),winslash="/",mustWork=FALSE)
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
})

# Do the installation
if ( .Platform$OS.type == 'windows' || install.success ) {
  install.success <- env.loc$ve.install()
}

# Load visioneval
if ( install.success ) {
  require("VEModel")
  env.loc$load.helpers()
} else {
  message("Please run ve.install() to complete installation.")
}

# Make sure there is a "Models" directory
env.loc$ModelRoot <- file.path(
  getRuntimeDirectory(),
  visioneval::getRunParameter("ModelRoot") # Uses runtime configuration or default value "models"
)
if ( ! dir.exists(env.loc$ModelRoot) ) dir.create(env.loc$ModelRoot,recursive=TRUE,showWarnings=FALSE)

# create the LoadTest function
env.loc$loadTest <- function(Package=NULL,files=NULL,clear=FALSE) {
  test.root <- file.path(getRuntimeDirectory(),"tools","tests")
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

# clean up variables created during startup
if ( exists("ve.lib",inherits=FALSE) ) rm(ve.lib)
rm(env.loc,install.success)
