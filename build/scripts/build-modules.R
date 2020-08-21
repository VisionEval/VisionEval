#!/bin/env Rscript

# Author: Jeremy Raw

# Build and optionally test the VE packages

# Very important to set up VE-config.yml and VE-components.yml correctly (see
# build-config.R, and the example configurations in VE-Installer and in
# VisionEval-dev).

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

# Build tool dependencies
require(tools,quietly=TRUE)
if ( ! suppressWarnings(require("devtools",quietly=TRUE)) ) {
  install.packages("devtools", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require("roxygen2",quietly=TRUE)) ) {
  install.packages("roxygen2", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require("rcmdcheck",quietly=TRUE)) ) {
  install.packages("rcmdcheck", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require("rmarkdown",quietly=TRUE)) ) {
  install.packages("rmarkdown", lib=dev.lib, type=.Platform$pkgType )
}
if ( ! suppressWarnings(requireNamespace("withr",quietly=TRUE)) ) {
  install.packages("withr", lib=dev.lib, type=.Platform$pkgType )
}

message("========== BUILD MODULES ==========")

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

# Where to find the module package sources (in the VisionEval repository)
ve.packages  <- pkgs.db[pkgs.module,]

package.names <- ve.packages$Package
ve.only.build <- Sys.getenv("VE_ONLY_BUILD","")
if ( nzchar(ve.only.build[1]) ) { # will always have at least one "exclusion" but it may be empty
  ve.only.build <- unlist(strsplit(ve.only.build," *, *"))
  package.names <- intersect(package.names,ve.only.build) # only (re)build certain packages
  cat("++++++++++ Only building:",paste0(ve.only.build,collapese=", "),"\n")
} else {
  ve.only.build <- character(0)
}
if ( length(package.names) == 0 ) {
  cat("++++++++++ Nothing to build due to VE_ONLY_BUILD:",paste0(ve.only.build,collapese=", "),"\n")
  quit(status=0)
}

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

# Oonfigure the build process
ve.express <- Sys.getenv("VE_EXPRESS","NO") != "NO"

# Copy test elements from components, if requested in configuration
if (ve.runtests && ! ve.express) {
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

# Get list of installed packages, for checking obsoleteness etc below

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
      cat("++++++++++ UPDATING package",package.names[module],"\nfrom",package.paths[module],"(Exists: ",me,")\n")
    } else {
      cat("++++++++++ CREATING package",package.names[module],"\nfrom",package.paths[module],"(Exists:",me,")\n")
    }
  }

  # Step 2: Determine package status (built, installed).
  # Force to "unbuilt" if package is in VE_ONLY_BUILD
  build.dir <- file.path(ve.src,package.names[module])
  if ( length(ve.only.build)>0 && nzchar(ve.only.build[1]) ) {
    cat("+++++++++++++REMOVING PREVIOUS BUILD FILES\n")
    local({
      pkg.src <- modulePath(package.names[module],built.path.src)
      if ( length(pkg.src)>0 ) { # built source package
        pkg.src <- file.path(built.path.src,pkg.src)
      } else pkg.src <- character(0)
      pkg.bin <- modulePath(package.names[module],built.path.binary)
      if ( length(pkg.bin)>0 ) { # built binary package
        pkg.bin <- file.path(built.path.binary,pkg.bin)
      } else pkg.bin <- character(0)
      if ( length(pkg.src)>0 )     { unlink(pkg.src); cat(pkg.src,"\n") } else cat("No Source Package.\n")
      if ( length(pkg.bin)>0 )     { unlink(pkg.bin); cat(pkg.bin,"\n") } else cat("No Binary Package.\n")
    })
    if ( dir.exists( build.dir) ) { unlink(build.dir,recursive=TRUE); cat(build.dir,"\n") } else cat("No Build Directory.\n")
    cat("++++++++++++ Done removing previous files\n")
  }
  check.dir <- file.path(build.dir,paste0(package.names[module],".Rcheck"))
  if ( debug ) cat( build.dir,"exists:",dir.exists(build.dir),"\n")
  if ( ve.binary.build ) {
    # On Windows, the package is built if:
    #   a. Binary package is present, and
    #   b. Source package is present, and
    #   c. package source is not newer than ve.src copy of source
    #   d. check.dir exists (previous built test will verify age of check.dir)
    #   e. Binary package is newer than source package
    me <- de <- ck <- nt <- vr <- as.logical(NA)
    package.built <- (me <- moduleExists(package.names[module], built.path.binary)) &&
                     (sc <- moduleExists(package.names[module], built.path.src)) &&
                     (de <- ( dir.exists(build.dir) && ! newerThan(package.paths[module],build.dir,quiet=(!debug))) ) &&
                     (ck <- ( ve.express || dir.exists(check.dir) ) ) &&
                     (nt <- ! newerThan( quiet=(debug<2),
                              src.module,
                              file.path(built.path.binary,
                                        modulePath(package.names[module],built.path.binary))) ) &&
                     (vr <- samePkgVersion(package.paths[module],getPathVersion(build.dir)) )
    if ( debug && ! package.built ) {
      cat("Status of unbuilt",package.names[module],"\n")
      cat("Module",me," Src",sc," Dir",de," Chk",ck," Newer",nt," Inst",(package.names[module] %in% pkgs.installed),"Ver",vr,"\n")
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
  if ( ! package.installed ) {
    cat(package.names[module],"is NOT installed\n")
    if ( package.names[module] %in% pkgs.installed ) { # installed version is obsolete
      cat("Removing obsolete module package version:",pkgs.version[package.names[module]],"\n")
      remove.packages(package.names[module],lib=ve.lib)
    }
  }

  # Step 3: If package is not built, (re-)copy package source to ve.src
  # On Windows: ve.src copy is used to build source and binary packages and to run tests
  # For Source build: ve.src copy is used to build source package
  if ( ! package.built ) {
    if ( debug>1 ) {
      # Dump list of package source files if debugging
      pkg.files <- file.path(package.paths[module],dir(package.paths[module],recursive=TRUE,all.files=TRUE))
      if ( ! any(grepl("Rbuildignore",pkg.files)) ) stop("No .Rbuildignore for package ",package.names[module])
      cat(paste("Copying",pkg.files,"to",build.dir,"\n",sep=" "),sep="")
    } else {
      cat("++++++++++ Copying module source",package.paths[module],"to build/test environment...\n")
    }
    if ( ! ve.express ) {
      if ( dir.exists(build.dir) || file.exists(build.dir) )
        unlink(build.dir,recursive=TRUE) # Get rid of the build directory and start fresh
    }
    all.files <- dir(package.paths[module],recursive=TRUE,all.files=FALSE) # not hidden files, relative to package.paths[module]
    pkg.files <- grep("^data/",all.files,value=TRUE,invert=TRUE)
    if ( length(all.files)!=length(pkg.files) ) {
      data.files <- setdiff(all.files,pkg.files)
      cat("Ignoring pre-built data files in data/ directory\n")
      print(data.files)
    }
    dot.files <- dir(package.paths[module],pattern="^\\.Rbuildignore$",all.files=TRUE)
    if ( length(dot.files)>0 ) {
      pkg.files <- c(pkg.files,dot.files)
    } else {
      message("No .Rbuildignore found in",package.paths[module],"\n")
    }
    pkg.dirs <- c(dirname(pkg.files),"data")
    lapply( grep("^\\.$",invert=TRUE,value=TRUE,unique(file.path(build.dir,pkg.dirs))),
      FUN=function(x) { dir.create(x, showWarnings=FALSE, recursive=TRUE ) } )
    invisible(
      file.copy(
        from=file.path(package.paths[module],pkg.files),
        to=file.path(build.dir,pkg.files),
        overwrite=TRUE, recursive=FALSE
      )
    )

    ###### HACK ALERT
    # Code above prevents the build from looking at the Github 'data' directory, since it is too
    # hard to ensure that such data gets updated when new source data is provided. We will rebuild
    # the data directory in all cases.
    #     HOWEVER:
    # VETravelDemandMM includes pre-estimated data files based on confidential NHTS that have to go
    # into the 'data' directory - they are found in 'data-raw/estimated', so we'll just copy them
    # into place...
    ######
    withr::with_dir(build.dir,{
      MM.estimated <- dir("data-raw/estimated",full.names=TRUE)
      if ( length(MM.estimated)>0 ) {
        file.copy(MM.estimated,"data")
      }
    })
    ###### END HACK

    if ( ! dir.exists(build.dir) ) {
      stop("Failed to create build/test environment:",build.dir)
    }
    if ( newerThan(package.paths[module],build.dir,quiet=(!debug)) ) {
      stop("After copying, build/test environment is still older than package.paths")
    }
  }

  # Step 4: Run devtools::document() separately to rebuild the /data directory
  if ( ! package.built ) {
    cat("++++++++++ Pre-build / Document ",package.names[module],"\nin ",build.dir,"\n",sep="")
    if ( ve.express ) {
      withr::with_dir(build.dir,roxygen2::roxygenise(roclets=c("collate","namespace")))
    } else {
      withr::with_dir(build.dir,roxygen2::roxygenise())
    }
    if ( ! ve.express ) {
      cat("++++++++++ Checking and pre-processing ",package.names[module],"\nin ",build.dir,"\n",sep="")
      # Run the module check (prior to building anything)
      # Run Roxygen with load='source' option in package DESCRIPTION
      # Requires us to set the working directory outside devtools:check, otherwise it gets very
      # confused about where to put the generated /data elements.
      # Need to set "check.dir" location explicitly to "check_dir=build.dir" (otherwise lost in space)
      check.results <- withr::with_dir(build.dir,devtools::check(".",check_dir=build.dir,document=FALSE,error_on="error"))
      cat("++++++++++ Check results\n")
      print(check.results)
    }
    
    # devtools::document with load_pkgload leaves the package loaded to a temporary library
    # Therefore we need to explicitly detach it so we can install it properly later on
    if ( (bogus.package <- paste("package:",package.names[module],sep="")) %in% search() ) {
      cat("Detaching",bogus.package,"\n")
      detach(bogus.package,character.only=TRUE,unload=TRUE)
      print(search())
    }

    # TO TEST: Might be able to move/copy tmp.build source package to built.path.src instead of deleting
    # it; then build binary and install from that version.
    # Also get rid of the temporary (and possibly obsolete) source package that is left behind
    # Must build again rather than use that built package, because the results of devtools::check
    #   updates (but does not include) any files in /data
    tmp.build <- file.path(build.dir,modulePath(package.names[module],build.dir))
    if ( length(tmp.build)>0 && file.exists(tmp.build) ) unlink(tmp.build)

    # Run the tests on build.dir if requested
    if ( ve.runtests && ! ve.express ) {
      test.script <- file.path(build.dir,ve.packages$Test[module])
      message("Executing tests from ",test.script,"\n")
      callr::rscript(script=test.script,wd=build.dir,libpath=.libPaths(),fail_on_status=FALSE)
      message("Completed test script.")
    } else {
      cat("\nNot running tests.\n\n")
    }
  }

  # If not built, rebuild the source module from build.dir (this time, with updated /data)
  # and place the result in built.path.src (the VE package repository we're building)
  if ( ! package.built ) {
    obsolete <- dir(built.path.src,pattern=paste0(package.names[module],"*_"))
    if ( length(obsolete)>0 ) cat("obsolete:",obsolete,"\n")
    unlink( file.path(built.path.src,obsolete) )
# TO TEST: do we need to rebuild here? Could use results of "check" for source package
    src.module <- devtools::build(build.dir, path=built.path.src)
    num.src <- num.src + 1
  }

  # Step 6: Build the binary package (Windows or Mac) and install the package

  tryCatch(
    {
      # VE_BUILD_PHASE="BUILD" says remove package datasets from R/ space (see visioneval/R/module.R)
      # Tells visioneval::savePackageDataset to remove the dataset object rather than save it again
      # Running devtools::document() will have already saved the dataset for the old-style modules
      # New style modules (e.g. VETravelDemandMM) have pre-built data which gets copied into data/
      # above (see the 'hack' which will eventually become standard procedure). So they don't use
      # visioneval::savePackageDataset and don't need/are immune to this flag.
      Sys.setenv(VE_BUILD_PHASE="BUILD")
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
          cat("++++++++++ Installing built package:",built.package,"\n")
          install.packages(built.package, repos=NULL, lib=ve.lib, type=ve.build.type) # so they will be available for later modules
        }
      } else { # source build
        # Just do installation directly from source package (no binary package created)
        if ( ! package.installed ) {
          cat("++++++++++ Installing source package:",src.module,"\n")
          if ( package.names[module] %in% pkgs.installed ) remove.package(package.names[module])
          install.packages(src.module, repos=NULL, lib=ve.lib, type="source")
          cat("++++++++++ DONE",package.names[module],"\n\n")
        } else {
          cat("Existing source package",package.names[module],"(Already Installed)\n")
        }
      }
    }, # we define no handlers: conditions are just passed through to the parent after calling finally
    finally = Sys.unsetenv("VE_BUILD_PHASE")
  )
}

# Update the repository PACKAGES files (source and binary) if we rebuilt any
# of the packages.
warnings()
if ( num.src > 0 ) {
  cat("Writing source PACKAGES file\n")
  tools::write_PACKAGES(built.path.src, type="source")
} else {
  cat("No source packages needed to be built\n")
}
if ( ve.binary.build ) {
  if ( num.bin > 0 ) {
    cat("Writing binary PACKAGES file\n")
    tools::write_PACKAGES(built.path.binary, type=ve.build.type)
  } else {
    cat("No binary packages needed to be built.\n")
  }
}

# Completion message, reporting what happened in this step
building <- paste( "building",ifelse(ve.runtests,", testing","") )
cat("++++++++++ Done ",building," and installing VisionEval packages.\n",sep="")
