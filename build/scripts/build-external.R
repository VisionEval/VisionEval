#!/bin/env Rscript

# Author: Jeremy Raw

# Build any Github packages (e.g. namedCapture).

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

# uncomment the following line on Windows if you just want the pre-compiled
# binaries otherwise, if RTools is installed the newer sources packages will be
# compiled.  You should allow compilation to happen if there is discrepancy in
# behavior between a Windows installation and a source (e.g. Linux/Docker)
# installation
options(install.packages.compile.from.source="never")

# Load required libraries (install as needed)

if ( ! suppressWarnings(require("git2r",quietly=TRUE)) ) {
  install.packages("git2r", lib=dev.lib, dependencies=NA, type=.Platform$pkgType)
}
require(tools,quietly=TRUE) # for write_PACKAGES below

message("========== BUILD EXTERNAL (GITHUB) DEPENDENCIES ==========")

# relay dependencies
load(ve.all.dependencies) # use all.dependencies
if ( ! exists("all.dependencies") ) {
  stop("Run build-config.R to set up build environment")
}

pkgs.external <- pkgs.db[pkgs.Github,]

# Need to change this so we clone the repository into "ve.external"
# Update Locations to require/identify ve.external (defaults to
# "external" in the output folder).
# Use git2r package, "clone" function, but only if cloned folder
# does not exist.
# Once we've retrieved the package (and have its path) we can just do
# the build as usual

if ( nrow(pkgs.external) > 0 ) {
  cat("Building external packages\n")

  if ( ! suppressWarnings(require("devtools",quietly=TRUE)) ) {
    install.packages("devtools", lib=dev.lib, type=.Platform$pkgType)
  }

  # Where to put the built results (these should exist after build-repository.R)
  # TODO: Verify that this works on Mac
  built.path.src <- contrib.url(ve.dependencies, type="source")
  if ( ve.binary.build ) {
    built.path.binary <- contrib.url(ve.dependencies, type=ve.build.type)
  }

  # External packages to build
  pkg.names <- as.character(pkgs.external[,"Package"])

  # Update the dependencies report
  all.dependencies <- c( all.dependencies, pkg.names)
  stated.dependencies <- c( stated.dependencies, pkg.names )
  save(stated.dependencies, all.dependencies, file=ve.all.dependencies)

  # make sure the external package location exists
  if ( ! exists("ve.external") ) ve.external <- file.path(ve.output,"external")
  pkg.paths <- file.path(ve.external, pkg.names)

  # clone the github dependencies if their folder is not present
  # NOTE: if github package is damaged, you must delete its cloned
  # folder to re-clone...
  git.paths <- paste("https://github.com/",pkgs.external[,"Path"],sep="")
  for ( pkg in seq_along(pkg.paths) ) {
    if ( ! dir.exists(pkg.paths[pkg]) ) {
      cat("Cloning missing Github dependency:",git.paths[pkg],"into",pkg.paths[pkg],"\n")
      repo <- git2r::clone(git.paths[pkg],pkg.paths[pkg],progress=FALSE)
    }
  }

  # Build the packages (skipping ones that are already built)
  num.src <- 0
  num.built <- 0
  pkgs.installed <- installed.packages(lib.loc=ve.lib)[,"Package"]

  for ( pkg in seq_along(pkg.paths) ) {

    # Build or locate the source package
    if ( ! moduleExists(pkg.names[pkg],built.path.src) ||
         newerThan( pkg.paths[pkg],
                    file.path(built.path.src,modulePath(pkg.names[pkg],built.path.src)) ) ) {
      src.pkg <- devtools::build(pkg.paths[pkg], path=built.path.src,vignettes=FALSE,manual=FALSE)
      num.src <- num.src + 1
    } else {
      src.pkg <- file.path( built.path.src, modulePath(pkg.names[pkg], built.path.src) )
    }

    # Determine if a binary build step is required
    package.built <- ( ! ve.binary.build ) ||
                     (
                       moduleExists(pkg.names[pkg], built.path.binary) &&
                       ! newerThan( pkg.paths[pkg],
                                    file.path(built.path.binary,modulePath(pkg.names[pkg],built.path.binary)))
                     )
    package.installed <- package.built && pkg.names[pkg] %in% pkgs.installed

    if ( ve.binary.build ) {
      # Windows or Mac: build binary package
      if ( ! package.built ) {
        cat("Building",pkg.names[pkg],"\n")
        cat("Errors may happen if the package needs compilation and build tools are not installed\n")
        built.package <- devtools::build(pkg.paths[pkg], path=built.path.binary, binary=TRUE)
        num.built <- num.built + 1
      } else {
        built.package <- file.path(built.path.binary, modulePath(pkg.names[pkg], built.path.binary))
      }
    } # No binary if source build
    if ( ! package.installed ) {
      if ( exists("built.package") && length(built.package) > 1 ) { # Fix weird bug that showed up in R 3.6.2 devtools::build
        built.package <- grep("zip$",built.package,value=TRUE)
        }
      cat("Installing ")
      if ( ve.binary.build ) { # Windows or Mac: install from binary
        cat(built.package,"\n")
        install.packages(built.package, repos=NULL, lib=ve.lib, type=ve.build.type)
      } else { # Not Windows: install from source package
        cat(src.pkg,"\n")
        install.packages(src.pkg, repos=NULL, lib=ve.lib, type="source")
      }
    }
  }
  if ( num.src > 0) {
      write_PACKAGES(built.path.src, type="source")
      cat(sprintf("Done building %d external source packages.\n", num.src))
  } else {
      cat("No external packages to build (source)\n")
  }
  if ( ve.binary.build && num.built > 0 ) {
    write_PACKAGES(built.path.binary, type=ve.build.type)
    cat(sprintf("Done building %d external binary packages\n",num.built))
  } else {
    cat("No external packages to build (binary)\n")
  } 
} else {
    cat("No external packages configured.\n")
}
