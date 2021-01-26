# Author: Jeremy Raw

# VisionEval runtime initialization script
# Run once to install everything and build RunVisionEval.Rdata
# Configure alternate library locations (e.g. development environment
# through .Renviron): do not rewrite if it already exists. And when
# starting the VisionEval .Rproj, load up the environment from the
# project root.

require(utils,quietly=TRUE) # For install package

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

install.success <- exists("ve.lib") && length(grep("^visioneval",dir(ve.lib)))>0

local( {

  # Load tools (helper functions) from their subdirectory
  env.loc$load.helpers <- function() {
    ve.tools <- file.path(ve.runtime,"tools",fsep="/")
    tool.files <- file.path(ve.tools,dir(ve.tools,pattern="\\.R$"),fsep="/")
    if ( length(tool.files)>0 ) {
      tools <- character(0)
      for ( tf in tool.files ) {
        # Add error checking for tool.contents not present
        try(
          silent=TRUE,
          eval(parse(text=paste0("import::here(tool.contents,.from='",tf,"')")))
        )
        if ( ! exists("tool.contents") ) next
        tools <- c(tools,tool.contents)
        eval(parse(text=paste0("import::into(.into='ve.env',",paste(tool.contents,collapse=","),",.from='",tf,"')")))
        rm(tool.contents)
      }
      rm(tf,tools)
    }
    rm(tool.files,ve.tools)
  }

  env.loc$ve.install <- if ( ! install.success ) {

    function() {

      # Put the library directory into ve.lib
      # Note that ve.lib is already present and fully provisioned in the Windows offline installer

      if ( ! exists("ve.lib.name") ) ve.lib.name <- "ve-lib"
      ve.lib <- file.path(ve.runtime,ve.lib.name)

      if ( ! dir.exists(ve.lib) || length(grep("visioneval",dir(ve.lib)))==0 ) {
        # Look in the build environment for ve-lib
        ve.lib.base <- ve.lib
        ve.lib.local <- normalizePath(file.path(ve.runtime,"..",ve.lib.name),winslash="/",mustWork=FALSE)
        if ( dir.exists(ve.lib.local) && (ve.len <- length(grep("visioneval",dir(ve.lib.local)))>0) ) {
          ve.lib <- ve.lib.local # Use the build environment installed library
          message("Development environment detected")
        } else {
          # See if ve-lib is set through .libPaths (and thus in .Renviron)
          ve.lib <- .libPaths()[grep(ve.lib.name,.libPaths(),fixed=TRUE)]
          if ( length(ve.lib)==1 && length(grep("visioneval",dir(ve.lib)))>0 ) {
            message("Using pre-established environment.")
          } else {
            # Look for source or mac.binary packages to install
            message("ve-lib not found. Installing from ve-pkg...")
            if ( ! exists("ve.pkg.name") ) ve.pkg.name <- "ve-pkg"
            ve.lib <- ve.lib.base
            ve.pkg <- file.path(ve.runtime,ve.pkg.name)
            if (  ! dir.exists(ve.pkg) ) {
              ve.pkg.local  <- normalizePath(file.path(ve.runtime,"..",ve.pkg.name),winslash="/",mustWork=FALSE)
              if ( dir.exists(ve.pkg.local) ) {
                ve.pkg <- ve.pkg.local
              } else {
                message("Unable to locate ",ve.lib.name," or ",ve.pkg.name," in the file system.")
                message("VisionEval packages are not available.")
                stop("Installation failed - check error and warning messages.")
              }
            }
            ve.pkg.type <- .Platform$pkgType
            ve.contrib.url <- contrib.url(ve.pkg,type=ve.pkg.type) # binary (Windows, MacOSX) or source (Unix)
            if ( ! dir.exists(ve.contrib.url) ) { # contrib.url does not have a binary branch
              ve.pkg.type = "source"
              ve.contrib.url <- contrib.url(ve.pkg,type=ve.pkg.type) # for source install on Windows or Mac
            }
            VE.pkgs <- available.packages(contriburl=paste0("file:",ve.contrib.url),type=ve.pkg.type)[,"Package"]
            # Installation list is everything in the repository
            # Consequently: test and abort if visioneval isn't in it
            if ( ! "visioneval" %in% VE.pkgs ) {
              message(paste("VisionEval not present in",ve.contrib.url))
              message("VisionEval packages are not available.")
              stop("Installation failed - check error and warning messages.")
            }

            # Install to local environment
            dir.create(ve.lib.base,recursive=TRUE,showWarnings=FALSE) # under ve.runtime
            message("Installing packages from ",paste0("file:",ve.pkg))
            install.packages(
                VE.pkgs,
                lib=ve.lib,
                repos=paste0("file:",ve.pkg),
                dependencies=c("Depends", "Imports", "LinkingTo"),
                type=ve.pkg.type,
                INSTALL_opts="--no-test-load"
            )
            message("Done installing")
          }
        }
      }

      install.success <- exists("ve.lib") && length(grep("^visioneval",dir(ve.lib)))>0

      if ( install.success ) {
        # Set .libPaths()
        .libPaths(c(ve.lib,.libPaths()))
        load.helpers() # defined from environment creating this function
        require("visioneval",quietly=TRUE)
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
      load.helpers()
      require("visioneval",quietly=TRUE)
      invisible(TRUE)
    }
  }
})

if ( .Platform$OS.type == 'windows' || install.success ) {
  if ( ve.install() ) message("Welcome to VisionEval!")
} else {
  message("Please run ve.install() to complete installation.")
}

if ( exists("ve.lib",inherits=FALSE) ) rm(ve.lib)
rm(env.loc,install.success)
