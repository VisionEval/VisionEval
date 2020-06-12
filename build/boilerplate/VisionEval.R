# Author: Jeremy Raw

# VisionEval runtime initialization script
# Run once to install everything and build RunVisionEval.Rdata

require(utils)

# Check the R version (redundant on Windows, but saves having to
# have a separate VisionEval.R for Linux/Mac
this.R <- paste(R.version[c("major","minor")],collapse=".")
that.R <- scan("r.version",what=character())
if ( this.R != that.R ) {
stop("Incorrect R version for this VisionEval installation: expecting R",that.R)
} else {
message("Loading VisionEval for R version",this.R,"\n")
}
rm(this.R,that.R)

# Put the current directory into ve.root
if ( (ve.root <- Sys.getenv("VE_ROOT",unset="" )) == "" ) {
ve.root <- getwd()
} else {
ve.root <- normalizePath(ve.root,winslash="/")
}

ve.install <- function() {

  # Put the library directory into ve.lib
  # Note that ve.lib is already present and fully provisioned in the Windows offline installer

  ve.lib.name <- "ve-lib" # TODO: Will be a problem if build VE-config uses some other name
  ve.lib <- file.path(ve.root,ve.lib.name)

  if ( ! dir.exists(ve.lib) || length(grep("visioneval",dir(ve.lib)))==0 ) {
    # Look in the build environment for ve-lib
    ve.lib.base <- ve.lib
    ve.lib.local <- normalizePath(file.path(ve.root,"..",ve.lib.name),winslash="/",mustWork=FALSE)
    if ( dir.exists(ve.lib.local) && (ve.len <- length(grep("visioneval",dir(ve.lib.local)))>0) ) {
      ve.lib <- ve.lib.local # Use the build environment installed library
      message("Using ve-lib from full build environment:",ve.lib)
    } else {
      print(dir(ve.lib))
      # See if ve-lib is set through .libPaths (and thus in .Renviron)
      ve.lib <- .libPaths()[grep(ve.lib.name,.libPaths(),fixed=TRUE)]
      if ( length(ve.lib)==1 && length(grep("visioneval",dir(ve.lib)))>0 ) {
        message("Using ve-lib from .libPaths()/Environment:",ve.lib)
      } else {
        # Look for source or mac.binary packages to install
        message("Installing from ve-pkg")
        ve.pkg.name <- "ve-pkg" # TODO: Will be a problem if build VE-config uses something else
        ve.lib <- ve.lib.base
        ve.pkg <- file.path(ve.root,ve.pkg.name)
        if (  ! dir.exists(ve.pkg) ) {
          ve.pkg.local  <- normalizePath(file.path(ve.root,"..",ve.pkg.name),winslash="/",mustWork=FALSE)
          if ( dir.exists(ve.pkg.local) ) {
            ve.pkg <- ve.pkg.local
          } else {
            message("Unable to locate ve-lib or ve-pkg in the file system,")
            message("VisionEval packages are not available")
            stop("Installation failed - check error and warning messages.")
          }
        }
        ve.pkg.type <- .Platform$pkgType
        ve.contrib.url <- contrib.url(ve.pkg,type=ve.pkg.type) # binary (Windows, MacOSX) or source (Unix)
        if ( ! dir.exists(ve.contrib.url) ) { # contrib.url does not have a binary branch
          ve.pkg.type = "source"
          ve.contrib.url <- contrib.url(ve.pkg,type=ve.pkg.type) # for source install on Windows or Mac
        }

        # Check availability of packages to install
        VE.pkgs <- available.packages(contriburl=paste0("file:",ve.contrib.url),type=ve.pkg.type)[,"Package"]
        # Installation list is everything in the repository
        # Consequently: test and abort if visioneval isn't in it
        if ( ! "visioneval" %in% VE.pkgs ) {
          message(paste("VisionEval not present in",ve.contrib.url))
          message("VisionEval packages are not available")
          stop("Installation failed - check error and warning messages.")
        }

        # Install to local environment
        dir.create(ve.lib.base,recursive=TRUE,showWarnings=FALSE) # under ve.root
        message("Installing packages from ",paste0("file:",ve.pkg))
        install.packages(
            VE.pkgs,
            lib=ve.lib,
            repos=paste0("file:",ve.pkg),
            dependencies=c("Depends", "Imports", "LinkingTo"),
            type=ve.pkg.type
        )
        message("Done installing")
        rm( ve.lib.local, ve.contrib.url, ve.lib.base, ve.pkg.name, ve.pkg.type )
      }
    }
  }
  rm(ve.lib.name)

  # Create .Renviron to support interactive package development
  # But don't overwrite if it's already there - the user may have
  # changed it. Use the VE-installer created development version
  # if that's available (which includes dev-lib)
  r.environ <- file.path(ve.root,".Renviron")
  if ( ! file.exists( r.environ ) ) {
    r.environ.dev <- normalizePath(file.path(ve.root,"../.Renviron"),winslash="/",mustWork=FALSE)
    if ( file.exists(r.environ.dev) ) {
      file.copy( r.environ.dev, "./.Renviron" )
    } else {
      write(paste0("R_LIBS_USER=",ve.lib,"\n"),file=r.environ)
    }
    rm( r.environ.dev )
  }
  rm( r.environ )

  if ( install.success ) {
    save(file="VisionEval.RData"
      ,ve.root
      ,ve.lib
      ,.First
      ,list=tools
    )
  } else {
    stop("Installation failed - check error and warning messages.")
  }
}

# Construct "VisionEval.Rdata" from the following objects
# Something to "double-click" in windows for a rapid happy start in RGui...

.First <- function() {
  if ( ! ve.lib %in% .libPaths() ) {
    .libPaths(c(ve.lib,.libPaths()))
    # cat(".libPaths() inside .First\n")
    # print(.libPaths())
  }
  if ( install.success <- require(visioneval) ) {
    setwd(ve.root)
    message("Welcome to VisionEval!\n")
  } else {
    message("VisionEval is not present: please re-run the installation")
  }
  install.success
}

# Load tools (helper functions) from their subdirectory
ve.tools <- file.path(ve.root,"tools",fsep="/")
tool.files <- file.path(ve.tools,dir(ve.tools,pattern="\\.R$"),fsep="/")
tools <- character(0)
if ( length(tool.files)>0 ) {
  for ( tf in tool.files ) {
    if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib,.libPaths()))
    # Add error checking for tool.contents not present
    eval(parse(text=paste0("import::here(tool.contents,.from='",tf,"')")))
    tools <- c(tools,tool.contents)
    eval(parse(text=paste0("import::here(",paste(tool.contents,collapse=","),",.from='",tf,"')")))
    rm(tool.contents,tf)
  }
}
rm(tool.files,ve.tools)

# Write objects to RunVisionEval.RData configuration file

if ( .Platform$OS.type == 'windows' ) {
  ve.install()
  install.success <- .First()

} else {
  message("Please run ve.install() to complete installation.")
}
