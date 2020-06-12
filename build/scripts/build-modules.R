#!/bin/env Rscript

# Author: Jeremy Raw

# Build and optionally test the VE packages

# Very important to set up VE-config.yml and VE-components.yml correctly (see
# build-config.R, and the example configurations in VE-Installer and in
# VisionEval-dev).

# Load runtime configuration
source(file.path(getwd(),"scripts/get-runtime-config.R"))

# Build tool dependencies
require(tools)
if ( ! suppressWarnings(require(devtools)) ) {
  install.packages("devtools", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require(roxygen2)) ) {
  install.packages("roxygen2", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require(rcmdcheck)) ) {
  install.packages("rcmdcheck", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require(rmarkdown)) ) {
  install.packages("rmarkdown", lib=dev.lib, type=.Platform$pkgType )
}

cat("========================= BUILDING MODULES =========================\n")

# Reach for ve.lib first when seeking packages used by the ones we're
# building
.libPaths( c(ve.lib, .libPaths()) ) # push runtime library onto path stack

built.path.src <- contrib.url(ve.repository, type="source")
if ( ! dir.exists(built.path.src) ) dir.create( built.path.src, recursive=TRUE, showWarnings=FALSE )

# Set up contrib.url for binary build, if available for this architecture
if ( ve.binary.build ) {
  built.path.binary <- contrib.url(ve.repository, type=ve.build.type)
  if ( ! dir.exists(built.path.binary) ) dir.create( built.path.binary, recursive=TRUE, showWarnings=FALSE )
} else {
  built.path.binary <- NULL
}

# Where to find the package sources (in the VisionEval repository)
# note that we're classifying 'framework' and 'module' differently, though they
# are treated the same here (visioneval always goes first).  They get different
# treatment when we assemble the documentation
ve.framework <- pkgs.db[pkgs.framework,]
ve.packages  <- rbind(ve.framework,pkgs.db[pkgs.module,])

package.names <- ve.packages$Package
package.paths <- file.path(ve.packages[,"Root"], ve.packages[,"Path"], package.names)
# Want to assert that all of these eventually have the same length!
# cat("Number of names:",length(package.names),"\n")
# cat("Number of paths:",length(package.paths),"\n")
# cat("Number of test paths:",length(package.testdir),"\n")
# cat("Number of test scripts:",length(test.scripts),"\n")

# Build the framework and modules as binary packages if the local system wants one of the supported
# binary types.
# We do "build" for Windows or Mac so we can get the .zip package file into the binary pkg-repository
# On platforms other than Windows or Mac, simply installing will do the necessary build

debug <- as.integer(Sys.getenv("VE_DEBUG_LEVEL",0)) # 0: no debug, 1: lightweight, 2: full details
if ( is.na(debug) ) debug <- 0 # in case VE_INST_DEBUG

# Locate modules to build in source repository (always build from source package)
source.modules <- unlist(sapply(package.names,
                  FUN=function(x) file.path(built.path.src,modulePath(x,built.path.src))))
if ( debug>1 ) {
  print(source.modules)
  cat("Source modules identified:\n")
  print(paste(package.names,source.modules[package.names],file.exists(source.modules[package.names]),sep=":"))
}

# Copy test elements from components, if requested in configuration
if (ve.runtests) {
  # Copy any additional test folders to ve.src
  # Mostly for "Test_Data", but any set of stuff needed for all tests
  ve.src.files <- pkgs.db[pkgs.test,]
  if ( nrow(ve.src.files)>0 ) {
    test.paths <- file.path(ve.src.files$Root, ve.src.files$Path, ve.src.files$Package)
    need.copy <- newerThan(test.paths,file.path(ve.src,ve.src.files$Package))
    if ( need.copy ) {
      cat(paste(paste("Copying Test Item",test.paths,"to",ve.src,sep=" "),"\n"),sep="")
      invisible(file.copy(test.paths, ve.src, recursive=TRUE))
    } else {
      cat("Test data is up to date\n")
    }
  }
}

# Build binary packages on systems with supported .Platform$pkgType
# (whatever R supports, currently "win.binary" and "mac.binary(.el-capitan)")

num.src <- 0
num.bin <- 0
pkgs.info <- installed.packages(lib.loc=ve.lib)[,c("Package","Version")]
pkgs.installed <- pkgs.info[,"Package"]
pkgs.version <- pkgs.info[,"Version"]

# Version verification functions

samePkgVersion <- function( pkg.path, version ) {
  # Compare version from package path to a target version (already built)
  #
  # Args:
  #   pkg.path: path to root of a package containing DESCRIPTION
  #   version: a Version string from some other package
  #
  # Returns:
  #   TRUE if the versions are the same, else FALSE

  # The "all" will handle pathological cases where version is a vector longer than 1
  result <- all(getPathVersion(pkg.path) == version)
  if (debug) {
    cat("samePkgVersion checks",pkg.path,"against",version,":",result,"\n")
  }
  return( result )
}

getPathVersion <- function( path ) {
  # Extract version string from DESCRIPTION on path
  #
  # Args:
  #   path: path to root of a package containing DESCRIPTION
  #         error if no DESCRIPTION on that path
  #
  # Returns:
  #   Version string from DESCRIPTION file
  desc.path <- file.path(path,"DESCRIPTION")
  if ( ! file.exists(desc.path) ) stop("getPathVersion: Did not find package at",desc.path)
  return ( read.dcf(file=desc.path)[1,"Version"] )
}

getPackageVersion <- function( package ) {
  # Extract version string from a built source module (using version encoded in its name)
  #
  # Args:
  #   package: path to a source or binary package (with version encoded)
  #
  # Returns:
  #   Version string from package file name

  # Eliminate package compression formats
  version <- sapply(strsplit(substr(package,1,regexpr(".(\\.tar\\.gz|\\.zip)",package)),"_"),FUN=function(x)x[2],simplify=TRUE)
  return( version )
}

# Build missing or out-of-date source modules
for ( module in seq_along(package.names) ) {
  src.module <- source.modules[package.names[module]]

  # Step 1: picky message to see if we're updating or creating the module fresh
  need.update <- newerThan( package.paths[module], src.module, quiet=(debug<2) )
  if ( ! (me <- moduleExists(package.names[module], built.path.src)) || need.update ) {
    if ( me ) { # module exists
      cat("Updating package",package.names[module],"from",package.paths[module],"(Exists: ",me,")\n")
    } else {
      cat("Creating package",package.names[module],"from",package.paths[module],"(Exists:",me,")\n")
    }
  }

  # Step 2: Determine package status (built, installed)
  build.dir <- file.path(ve.src,package.names[module])
  check.dir <- file.path(build.dir,paste0(package.names[module],".Rcheck"))
  if ( debug ) cat( build.dir,"exists:",dir.exists(build.dir),"\n")
  if ( ve.binary.build ) {
    # On Windows, the package is built if:
    #   a. Binary package is present, and
    #   b. package source is not newer than ve.src copy of source
    #   c. check.dir exists (previous built test will verify age of check.dir)
    #   d. Binary package is newer than source package
    me <- de <- ck <- nt <- vr <- as.logical(NA)
    package.built <- (me <- moduleExists(package.names[module], built.path.binary)) &&
                     (de <- ( dir.exists(build.dir) && ! newerThan(package.paths[module],build.dir,quiet=(!debug))) ) &&
                     (ck <- dir.exists(check.dir) ) &&
                     (nt <- ! newerThan( quiet=(debug<2),
                              src.module,
                              file.path(built.path.binary,
                                        modulePath(package.names[module],built.path.binary))) ) &&
                     (vr <- samePkgVersion(package.paths[module],getPathVersion(build.dir)) )
    if ( debug && ! package.built ) {
      cat("Status of unbuilt",package.names[module],"\n")
      cat("Module",me," ","Dir",de," ","Chk",ck," ","Newer",nt," ","Inst",(package.names[module] %in% pkgs.installed),"Ver",vr,"\n")
    }
  } else {
    # If Source build, the package is "built" if:
    #   a. package source is not newer than ve.src copy of source
    package.built <- (
      ! is.na(src.module) &&
        dir.exists(build.dir) &&
      ! newerThan( package.paths[module], build.dir ) &&
        samePkgVersion(package.paths[module],getPackageVersion(src.module))
      )
  }
  if ( ! package.built ) cat(package.names[module],"is NOT built\n")

  # Package is installed if it is built and is an available installed package
  package.installed <- (
    package.built &&
    ! is.na( pkgs.installed[package.names[module]] ) &&
    samePkgVersion(package.paths[module],pkgs.version[package.names[module]])
  )
  if ( ! package.installed ) cat(package.names[module],"is NOT installed\n")

  # Step 3: If package is not built, (re-)copy package source to ve.src
  # On Windows: ve.src copy is used to build source and binary packages and to run tests
  # For Source build: ve.src copy is used to build source package
  if ( ! package.built ) {
    if ( debug>1 ) {
      # Dump list of package source files if debugging
      pkg.files <- file.path(package.paths[module],dir(package.paths[module],recursive=TRUE))
      cat(paste("Copying",pkg.files,"to",build.dir,"\n",sep=" "),sep="")
    } else {
      cat("Copying module source",package.paths[module],"to build/test environment...\n")
    }
    if ( dir.exists(build.dir) || file.exists(build.dir) ) unlink(build.dir,recursive=TRUE) # Get rid of the build directory and start fresh
    pkg.files <- dir(package.paths[module],recursive=TRUE,all.files=FALSE) # not hidden files, relative to package.paths[module]
    pkg.dirs <- dirname(pkg.files)
    lapply( grep("^\\.$",invert=TRUE,value=TRUE,unique(file.path(build.dir,pkg.dirs))), FUN=function(x) { dir.create(x, showWarnings=FALSE, recursive=TRUE ) } )
    invisible(file.copy(from=file.path(package.paths[module],pkg.files),to=file.path(build.dir,pkg.files),overwrite=TRUE, recursive=FALSE))
    if ( ! dir.exists(build.dir) ) {
      stop("Failed to create build/test environment:",build.dir)
    }
    if ( newerThan(package.paths[module],build.dir,quiet=(!debug)) ) {
      stop("After copying, build/test environment is still older than package.paths")
    }
  }

  # Step 4: Check the module in order to rebuild the /data directory in build.dir
  if ( ! package.built ) {
    cat("Checking and pre-processing ",package.names[module]," in ",build.dir,"\n",sep="")
    # Run the module tests (prior to building anything)
    # Note that "check.dir" here is created within "check_dir=build.dir"
    check.results <- devtools::check(build.dir,check_dir=build.dir,error_on="error")
    cat("Check results\n")
    print(check.results)
    # devtools::check leaves the package loaded after its test installation to a temporary library
    # Therefore we need to explicitly detach it so we can install it properly later on
    detach(paste("package:",package.names[module],sep=""),character.only=TRUE,unload=TRUE)

    # Then get rid of the temporary (and possibly obsolete) source package that is left behind
    # Must build again rather than use that built package, because the results of devtools::check
    #   updates (but does not include) any files in /data
    tmp.build <- file.path(build.dir,modulePath(package.names[module],build.dir))
    if ( length(tmp.build)>0 && file.exists(tmp.build) ) unlink(tmp.build)

    # Run the tests on build.dir if requested
    if ( ve.runtests ) {
      test.script <- file.path(build.dir,ve.packages$Test[module])
      cat("Executing tests from ",test.script,"\n")
      callr::rscript(script=test.script,wd=build.dir,libpath=.libPaths(),fail_on_status=FALSE)
      cat("Completed test script.\n")
    }
  }

  # If not built, rebuild the source module from build.dir (this time, with updated /data)
  # and place the result in built.path.src (the VE package repository we're building)
  if ( ! package.built ) {
    obsolete <- dir(built.path.src,pattern=paste0(package.names[module],"*_"))
    if ( length(obsolete)>0 ) cat("obsolete:",obsolete,"\n")
    unlink( file.path(built.path.src,obsolete) )
    src.module <- devtools::build(build.dir, path=built.path.src)
    num.src <- num.src + 1
  }

  # Step 6: Build the binary package (Windows or Mac) and install the package
  if ( ve.binary.build ) {
    # Binary build and install works a little differently from source
    if ( ! package.built ) {
      # Rebuild the binary package from the ve.src folder
      # We do this on Windows (rather than building from the source package) because
      # we want to use devtools::build, but a bug in devtools prior to R 3.5.3 or so
      # prevents devtools:build from correctly building from a source package (it
      # requires an unpacked source directory, which we have in build.dir)
      cat("building",package.names[module],"from",build.dir,"as",ve.build.type,"\n")
      cat("building into",built.path.binary,"\n")

      obsolete <- dir(built.path.binary,pattern=paste0(package.names[module],"*_"))
      if ( length(obsolete)>0 ) cat("obsolete:",obsolete,"\n")
      unlink( file.path(built.path.binary,obsolete) )
      built.package <- devtools::build(build.dir,path=built.path.binary,binary=TRUE)
      if ( length(built.package) > 1 ) { # Fix weird bug that showed up in R 3.6.2 devtools::build
        built.package <- grep("zip$",built.package,value=TRUE)
      }
      num.bin <- num.bin + 1
    } else {
      cat("Existing binary package:",package.names[module],ifelse(package.installed,"(Already Installed)",""),"\n")
      built.package <- file.path(built.path.binary, modulePath(package.names[module], built.path.binary))
    }
    if ( ! package.installed ) {
      # On Windows, install from the binary package
      cat("Installing built package:",built.package,"\n")
      if ( package.names[module] %in% pkgs.installed ) {
        cat("First removing obsolete package version:",pkgs.version[package.names[module]],"\n")
        remove.packages(package.names[module])
      }
      install.packages(built.package, repos=NULL, lib=ve.lib, type=ve.build.type) # so they will be available for later modules
    }
  } else { # source build
    # Just do installation directly from source package (no binary package created)
    if ( ! package.installed ) {
      cat("Installing source package:",src.module,"\n")
      if ( package.names[module] %in% pkgs.installed ) remove.package(package.names[module])
      install.packages(src.module, repos=NULL, lib=ve.lib, type="source")
    } else {
      cat("Existing source package",package.names[module],"(Already Installed)\n")
    }
  }
}

# Update the repository PACKAGES files (source and binary) if we rebuilt any
# of the packages.
warnings()
if ( num.src > 0 ) {
  cat("Writing source PACKAGES file\n")
  write_PACKAGES(built.path.src, type="source")
} else {
  cat("No source packages needed to be built\n")
}
if ( ve.binary.build ) {
  if ( num.bin > 0 ) {
    cat("Writing binary PACKAGES file\n")
    write_PACKAGES(built.path.binary, type=ve.build.type)
  } else {
    cat("No binary packages needed to be built.\n")
  }
}

# Completion message, reporting what happened in this step
building <- paste( "building",ifelse(ve.runtests,", testing","") )
cat("Done ",building," and installing VisionEval packages.\n",sep="")
