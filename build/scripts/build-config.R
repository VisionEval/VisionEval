#!/bin/env Rscript

# Author: Jeremy Raw

# uncomment the following line on Windows if you just want the pre-compiled
# binaries otherwise, if RTools is installed the newer sources packages will be
# compiled.  You should allow compilation to happen if there is discrepancy in
# behavior between a Windows installation and a source (e.g. Linux/Docker)
# installation
options(install.packages.compile.from.source="never")

message("========== BUILD CONFIGURATION ==========")

# Set a default repository for non-interactive installation of packages
r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org"
options(repos=r)

if ( length(grep("^ve.builder$",search()))==0 ) {
  attach(NULL,name="ve.builder")
} else {
  blow.away <- ls("ve.builder")
  blow.away <- setdiff(blow.away,c("ve.root","ve.build.dir","ve.dev","dev.lib","this.R"))
  if ( length(blow.away) > 0 ) {
    rm(list=blow.away,pos=as.environment("ve.builder")) # blow away all but basic build elements
  }
}

evalq(
  envir=as.environment("ve.builder"),
  expr={

  if ( ! exists("this.R") ) {
    this.R <- paste(R.version[c("major","minor")],collapse=".")
  }
  if ( ! exists("ve.build.dir") ) {
    cat("Setting build to current directory.\n")
    ve.build.dir <- getwd()
  }
  if ( ! exists("ve.dev") ) {
    ve.dev <- normalizePath(file.path(ve.build.dir,"..","dev"),winslash="/",mustWork=FALSE)
  }
  if ( ! exists("dev.lib") ) {
    dev.lib <- normalizePath(file.path(ve.dev,"lib",this.R),winslash="/",mustWork=FALSE)
    dir.create(dev.lib, recursive=TRUE, showWarnings=FALSE )
  } else {
    if ( ! dir.exists(dev.lib) ) dir.create(dev.lib, recursive=TRUE, showWarnings=FALSE )
  }
  .libPaths(dev.lib)

  # Bootstrap development packages by loading current CRAN version of yaml from cloud.r-project.org
  # r-versions.yml will override based on R version for visioneval packages themselves
  if ( ! suppressWarnings(require("yaml",quietly=TRUE)) ) {
    install.packages("yaml", lib=dev.lib, repos="https://cloud.r-project.org", dependencies=NA, type=.Platform$pkgType )
  }

  # Get VisionEval release version (used in installer file.names
  ve.version <- Sys.getenv("VE_VERSION","2.0")
  cat("Building VisionEval version",ve.version,"\n")

  # Specify dependency repositories for known R versions
  rversions <- yaml::yaml.load_file(file.path(ve.build.dir,"R-versions.yml"))

  cat("Building for R version",this.R,"\n")
  if ( ! this.R %in% names(rversions) ) {
    cat("Supported R versions:\n")
    print(names(rversions))
    stop("R version ",this.R,"is not supported",call.=FALSE)
  }
  CRAN.mirror <- rversions[[this.R]]$CRAN
  BioC.mirror <- rversions[[this.R]]$BioC

  # Identify the platform and supported binary package built types
  ve.platform <- .Platform$OS.type # Used to better identify binary installer type
  ve.platform <- paste(toupper(substring(ve.platform,1,1)),substring(ve.platform,2),sep="")
  ve.binary.build.types <- c("win.binary","mac.binary","mac.binary.el-capitan")
  ve.build.type <- .Platform$pkgType
  ve.binary.build <- ve.build.type %in% ve.binary.build.types
  if ( ! ve.binary.build ) {
    ve.build.type <- "source"
  }

  # Locate the installer tree (used for boilerplate)
  # The following includes a hack to fix a common path problem if you are
  # developing on Windows in a subfolder of "My Documents"
  ve.installer <- ve.build.dir
  if ( ve.platform == "Windows" || ve.build.type == "win.binary" ) {
    ve.installer <- sub("My Documents", "Documents", ve.installer)
    ve.installer <- gsub("\\\\", "/", ve.installer)
  } else if ( ve.platform == "Unix" && ve.build.type %in% c("mac.binary","mac.binary.el-capitan") ) {
    ve.platform <- "MacOSX"
  }

  # ========== CREATE HELPER FUNCTIONS ==========

  if ( ! suppressWarnings(require("git2r",quietly=TRUE)) ) {
    install.packages("git2r", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
  }

  # Helper function to get name of current branch on repopath
  localBranch <- function(repopath) {
    localbr <- git2r::branches(repopath,flags="local")
    hd <- which(sapply(localbr,FUN=git2r::is_head,simplify=TRUE))
    return( localbr[[hd]]$name )
  }

  # Helper function to get roots and branches
  checkBranchOnRoots <- function(roots,branches) {
  #  rtb <- names(branches)
    for ( rt in roots ) {
      # It's okay if there is not branch (i.e. not github)
      # but it's an error if there is a local branch name and it doesn't match
      if ( length(branches) > 0 ) {
        br <- branches[rt]
        if ( length(br)==1 && nzchar(br) ) {
          repopath <- get(rt)
    #       cat("Examining branch for",rt,"which should be",paste0("'",br,"'"),"\n")
          if ( dir.exists(repopath) && git2r::in_repository(repopath) ) {
            # Find the currently checked out branch by looking at HEAD for local branches
            # cat("Need branch",paste0("<",br,">"),"on repository",repopath,"\n")
            hd <- localBranch(repopath)
            # cat("Have branch",paste0("<",hd,">"),"on repository",repopath,"\n")
            cat("hd:",hd,"\n")
            cat("br:",br,"\n")
            if ( hd != br) {
              cat(paste("Root",rt,"wants branch",paste0("<",br,">"),"but has",paste0("<",hd,">")),"\n")
              return(FALSE)
            }
          } else {
            end.message <- if ( ! dir.exists(repopath) ) {
              "does not exist"
            } else if ( ! git2r::in_repository(repopath) ) {
              "is not a Git repository"
            } else {
              "is bad for an unknown reason"
            }
            cat("Branch",paste0("'",br,"'"),"specified, but",repopath,end.message,".")
            return(FALSE)
          }
        }
      }
    }
    return(TRUE)
  }

  # Helper function for other scripts, to verify situational awareness

  checkVEEnvironment <- function() {
    # Check for situational awareness, and report if we are lost
    # Returns 0 or 1
    if ( ! exists("ve.installer") || is.na(ve.installer) || ! file.exists(ve.installer) ) {
      message("Missing ve.installer; run build-config.R")
      return(FALSE)
    } else if ( ! exists("ve.repository") || is.na(ve.repository) ) {
      message("Missing ve.repository definition; run build-config.R")
      return(FALSE)
    } else if ( ! exists("ve.dependencies") || is.na(ve.dependencies) ) {
      message("Missing ve.dependencies definition; run build-config.R")
      return(FALSE)
    } else if ( ! exists("ve.runtime") || is.na(ve.runtime) ) {
      message("Missing ve.runtime definition; run build-config.R")
      return(FALSE)
    } else if ( ! exists("ve.pkgs") || is.na(ve.pkgs) ) {
      message("Missing ve.pkgs definition; run build-config.R")
      return(FALSE)
    } else if ( ! exists("ve.lib") || is.na(ve.lib) ) {
      message("Missing ve.lib definition; run build-config.R")
      return(FALSE)
    } else if ( ! exists("ve.roots") || ! exists("ve.branches") || ! checkBranchOnRoots(ve.roots,ve.branches) ) {
      message("Missing roots, or incorrect branches")
      return(FALSE)
    }
    return(TRUE)
  }

  # The following two helpers extract modules from built packages
  # Used in the scripts to detect whether a module has been built yet.
  modulePath <- function( module, path ) {
    # determine which module in a vector of names is present in the
    # path
    #
    # Args:
    #   module: a character vector of module names to look for
    #   path: a file system path to look for the modules
    #
    # Returns:
    #   A character vector of the file system names (include version
    #   number strings) for the corresponding packages in module,
    #   if any
    mods <- dir(path)
    # result <- mods[grep(paste("^", basename(module), "_", sep=""), mods)]
    matching <- paste("^", basename(module), "_", sep="")
    test<-sapply(matching,FUN=function(x){ grep(x,mods) },simplify=TRUE,USE.NAMES=FALSE)
    if ( class(test)=="list" ) test <- integer(0) # weirdness of sapply(simplify=TRUE) when empty
    result <- mods[test]
  }

  moduleExists <- function( module, path ) {
    # determine if modulePath found any 'modules' in 'path'
    #
    # Args:
    #   module: a character vector of module names to look for
    #   path: a file system path to look for the modules
    #
    # Returns:
    #   TRUE if any matching modules were found in path, else FALSE
    #
    # Let us genuflect briefly toward a coding standard that calls for
    # a dozen lines of documentation for a one line "alias"
    found <- modulePath(module,path)
    found.test <- length(found)>0
  }

  # Helper function to compare package path (source) to a built target (modification date)
  newerThan <- function( srcpath, tgtpath, quiet=TRUE ) {
    # Compare modification time for a set of files to a target file
    #
    # Args:
    #   srcpath - a single folder containing a bunch of files that might be newer, or a vector of files
    #   tgtpath - one (or a vector) of files that may be older, or may not exist
    #   quiet - if TRUE, then print a message about what is being tested
    #
    # Value: TRUE if the most recently modified source file is newer
    #        than the oldest target file
    if (!quiet) cat("Comparing",srcpath,"to",paste(tgtpath,collapse="\n"),"\n")
    if ( any(is.null(srcpath)) || any(is.na(srcpath)) || any(nchar(srcpath))==0 || ! file.exists(srcpath) ) return(TRUE)
    if ( any(is.null(tgtpath)) || any(is.na(tgtpath)) || any(nchar(tgtpath))==0 || ! file.exists(tgtpath) ) return(TRUE)
    if ( dir.exists(srcpath) ) srcpath <- file.path(srcpath,dir(srcpath,recursive=TRUE,all.files=TRUE))
    if ( dir.exists(tgtpath) ) tgtpath <- file.path(tgtpath,dir(tgtpath,recursive=TRUE,all.files=TRUE))
    if ( length(tgtpath) < 1 ) return(TRUE)
    source.time <- file.mtime(srcpath)
    target.time <- file.mtime(tgtpath)
    source.newest <- order(source.time,decreasing=TRUE)
    target.newest <- order(target.time,decreasing=TRUE)
    if (!quiet) cat("Source:",srcpath[source.newest[1]],strftime(source.time[source.newest[1]],"%d/%m/%y %H:%M:%S"),"\n")
    if (!quiet) cat("Target:",tgtpath[target.newest[1]],strftime(target.time[target.newest[1]],"%d/%m/%y %H:%M:%S"),"\n")
    newer <- source.time[source.newest[1]] > target.time[target.newest[1]]
    if (!quiet) cat("Newer:",newer,"\n")
    newer
  }

  # ========== DONE WITH HELPER FUNCTIONS ==========

  # Read the configuration file
  ve.config.file <- Sys.getenv("VE_CONFIG",file.path(ve.installer,"config/VE-config.yml"))
  cat(paste("Loading Configuration File:",ve.config.file,"\n",sep=" "))
  if ( !file.exists(ve.config.file) ) {
    stop("Configuration file",ve.config.file,"not found.",call.=FALSE)
  }
  ve.cfg <- yaml::yaml.load_file(ve.config.file)

  # Extracting root paths, plus branch if specified.
  branches <- character(0)
  # branches is a named vector - name is the root, value is the branch
  if ( "Roots" %in% names(ve.cfg) ) {
    ve.roots <- names(ve.cfg$Roots)
    ve.branches <- invisible(
      sapply(
        ve.roots,
        FUN=function(x,venv) {
          rt <- ve.cfg$Roots[[x]]
          nt <- names(rt)
          br <- ""
          rtp <- ifelse ( "path" %in% nt , rt$path , rt )
          if ( substr(rtp,1,1)=="." ) rtp<-file.path(ve.installer,rtp)
          assign(x,normalizePath(rtp,winslash="/",mustWork=FALSE),pos=venv);
          if ( "branch" %in% nt ) {
              br <- rt$branch     # Vector element is branch
              cat("Root",x,"requires branch",paste0("'",rt$branch,"'"),"\n")
          }
          names(br) = x       # Vector name is root
          return(br)
        },
        venv=as.environment("ve.builder"),
        USE.NAMES=FALSE
      )
    )
  } else {
    stop("No roots in",ve.config.file,"- Check file format.",call.=FALSE)
  }

  if ( ! "ve.root" %in% ve.roots ) {
    cat("Default ve.root\n")
    if ( ! exists("ve.root") ) { # use existing one if available
      # otherwise set a useful default
      ve.root <- normalizePath(file.path(ve.installer,".."),winslash="/",mustWork=TRUE)
    }
  }

  if ( ! all(nzchar(ve.branches)) ) ve.branches <- character(0)

  # Invoke helper function before continuing to apply branch constraint
  if ( ! checkBranchOnRoots(ve.roots,ve.branches) ) {
    stop("Incorrect branch specified for root.\n",call.=FALSE)
  }

  # Set branch for constructing branch-dependent output
  if ( "ve.root" %in% names(ve.branches) ) {
    ve.branch <- ve.branches["ve.root"]
    cat("Using explicit branch from config:",ve.branch,"\n")
  } else {
    ve.branch <- Sys.getenv("VE_BRANCH","")
    if ( ! nzchar(ve.branch) ) ve.branch <- localBranch(ve.root)
    if ( ! nzchar(ve.branch) ) ve.branch <- "visioneval"
    cat("Using branch via environment:",ve.branch,"\n")
  }

  if ( ! "ve.output" %in% ve.roots || ! exists("ve.output") ) {
    ve.output = normalizePath(file.path(ve.root,"built"),winslash="/",mustWork=FALSE)
  }

  if ( ! exists("ve.logs") ) {
    ve.logs <- Sys.getenv("VE_LOGS",file.path(ve.dev,"logs",ve.branch,this.R))
  } else {
    cat("Existing ve.logs:",normalizePath(ve.logs,winslash="/",mustWork=FALSE),"\n")
  }
  if ( ! dir.exists(ve.logs) ) dir.create( ve.logs, recursive=TRUE, showWarnings=FALSE )

  cat("Building in (and around)",ve.output,"\n")
  cat("Logging into",ve.logs,"\n")

  # Extracting location paths:
  locs.lst <- names(ve.cfg$Locations)
  makepath <- function(x,venv) {
    # Build a location path from root and path and assign it
    # Note that this function is used for its SIDE-EFFECTS, not
    # its return value.
    #
    # Args:
    #   x - the name of a Location (and its veriable)
    #   venv - the environment in which to create the variable (current)
    #
    loc <- ve.cfg$Locations[[x]]
    subdir <- switch(
      loc$augment,
      root    = "",
      branch  = file.path(ve.branch),
      version = file.path(ve.branch,this.R),
      ve.branch
    )
    assign(
      x,
      normalizePath(
        file.path(
          get(loc$root),
          subdir,
          loc$path
        ),
        winslash="/",
        mustWork=FALSE
      ),
      pos=venv
    )
  }
  invisible(sapply(locs.lst,FUN=makepath,venv=as.environment("ve.builder")))

  # Create the locations
  # Packages and libraries are distinguished by R versions since the
  # R versions are sometimes hidden and we may want to use the same
  # VE-config.yml with different versions of R (e.g. 3.5.1 and 3.5.2)

  for ( loc in locs.lst ) dir.create( get(loc), recursive=TRUE, showWarnings=FALSE )
  ve.zipout <- dirname(ve.runtime) # Installer zip files always go next to ve.runtime

  # Determine whether build should include tests
  # Look at environment (possibly from Makefile) then at ve.cfg
  # Result is TRUE (run tests) or FALSE (skip tests)
  ve.runtests <- switch(
    tolower(Sys.getenv("VE_RUNTESTS",unset="Default")),
    false=FALSE,
    true=TRUE,
    ! is.null(ve.cfg[["RunTests"]]) && all(ve.cfg[["RunTests"]])
    )

  # Create the .Renviron file in ve.output so dev-lib is included
  # That way we can have things like miniCRAN that are not needed by the runtime
  if ( ! exists("ve.lib") ) {
    stop("ve.lib must be defined in VE-config.yml")
  }

  # Convey key file locations to the 'make' environment
  ve.runtime.config <- file.path(ve.logs,"dependencies.RData")
  make.target <- file.path(
      ve.installer, # ve.installer is already normalized
      paste0(paste("ve-output",ve.branch,this.R,sep="-"),".make")
    )
  cat("make.target:",make.target,"\n")
  make.target <- Sys.getenv(
    "VE_MAKEVARS",
    unset=make.target
  )
  make.variables <- c(
     VE_R_VERSION      = this.R
    ,VE_VERSION        = ve.version
    ,VE_LOGS           = ve.logs
    ,VE_DEVLIB         = dev.lib
    ,VE_BRANCH         = ve.branch
    ,VE_RUNTIME_CONFIG = ve.runtime.config
    ,VE_PLATFORM       = ve.platform
    ,VE_INSTALLER      = ve.installer
    ,VE_OUTPUT         = ve.output
    ,VE_ZIPOUT         = ve.zipout
    ,VE_LIB            = ve.lib
    ,VE_REPOS          = ve.repository
    ,VE_PKGS           = ve.pkgs
    ,VE_RUNTIME        = ve.runtime
    ,VE_SRC            = ve.src
    ,VE_DOCS           = ve.docs
    ,VE_RUNTESTS       = ve.runtests
    ,VE_DEPS           = ve.dependencies
    ,VE_ROOT           = ve.root
  )

  # Set up .Renviron for future runs
  r.environ <- file.path(ve.root,".Renviron")
  r.ve.lib <-gsub(this.R,"%V",ve.lib)
  r.dev.lib <- gsub(this.R,"%V",dev.lib)
  r.libs.user <- paste0("R_LIBS_USER=",paste(r.ve.lib,r.dev.lib,sep=";"))
  writeLines(r.libs.user,con=r.environ)
  rm( r.environ,r.libs.user,r.dev.lib,r.ve.lib )

  writeLines( paste( names(make.variables), make.variables, sep="="),make.target)

  # The following are constructed in Locations above, and must be present
  # ve.runtime <- file.path(ve.output, "runtime")
  # ve.lib <- file.path(ve.output, "ve-lib",this.R)
  # ve.repository <- file.path(ve.output, "pkg-repository")
  # ve.dependencies
  # ve.runtime

  # ve.dependencies hosts the external R packages
  # ve.repository hosts the built VE packages
  ve.deps.url <- paste("file:", ve.dependencies, sep="")
  ve.repo.url <- paste("file:", ve.repository, sep="")

  # Load the Components
  cat("Loading Components...\n")

  catn <- function(...) { cat(...); cat("\n") }

  # ve.components can be set as a location in VE-config.yml
  if ( ! exists("ve.components") ) ve.components <- file.path( ve.root,"build","config","VE-components.yml" )
  if ( ! file.exists(ve.components) ) {
    cat("ve.components = ",ve.components,"\n")
    stop("Cannot find VE-components.yml in VisionEval build folder",call.=FALSE)
  }
  component.file <- c( ve.root = ve.components )
  includes <- list()
  excludes <- list()
  ##### WARNING - we make use of the fact that:
  #   "ve.root" will always be at component.file[1] !!!!
  if ( "Components" %in% names(ve.cfg) ) {
    comps <- ve.cfg$Components
    components.lst <- names(comps)
  #   catn("Component list from VE-config.yml:")
  #   print(components.lst)
    for ( root in components.lst ) {
      if ( ! exists(root) ) {
        stop(paste("Undefined",root,"in Roots: section of",ve.config.file,sep=" "),call.=FALSE)
      }
  #     catn("Root:",root,"is",get(root))
  #     print(names(comps[[root]]))
      if ( "Root" %in% names(comps[[root]]) ) {
        comps.root <- comps[[root]]$Root
      } else {
        comps.root <- root
      }
      component.file[root] <- file.path( get(comps.root),comps[[root]]$Config )
      if ( "Include" %in% names(comps[[root]]) ) {
        includes[[root]] <- comps[[root]]$Include
  #       cat("Includes from",root,"\n")
  #       print(includes[[root]])
      } else {
        includes[[root]] <- character(0)
      }
      if ( "Exclude" %in% names(comps[[root]]) ) {
        excludes[[root]] <- comps[[root]]$Exclude
  #       catn("Excludes from",root)
  #       print(comps[[root]]$Exclude)
      } else {
        excludes[[root]] <- character(0)
      }
    }
  }

  # Process component.file like this:
  #   1. Load components from file into temporary list
  #   2. Add component from "Include" if not empty
  #   3. Else skip component if it's in "Exclude"
  #   4. Put each remaining element of temporary list into final
  #      component list (by component name, so there is replacement)

  # cat("Processing component.file\n")

  build.comps <- list()
  for ( root in names(component.file) ) {
  #   catn("Processing components for",root,"from",component.file[root])
    comps <- ve.cfg <- yaml::yaml.load_file(component.file[root])$Components
    if ( is.null(comps) ) stop("Failed to find components in",component.file[root],call.=FALSE)
    for ( cn in names(comps) ) {
      comp <- comps[[cn]]
      if ( ( length(excludes[[root]])==0 || ! cn %in% excludes[[root]] ) &&
           ( length(includes[[root]])==0 || cn %in% includes[[root]] ) ) {
        comp$Root <- get(root) # retrieves path from variable whose name is in 'root'
        build.comps[[cn]] <- comp
      }
    }
  }
  # catn("Build roots:")
  # print(names(build.comps))
  # print(build.comps[[names(build.comps)[2]]])

  # Parse the Components for Dependencies
  # Do this in a specific order:
  #   "Type: framework"
  #   "Type: module"
  #      Within Module by Test$Group
  #      Within Group in order from build.comps
  #   "Type: model"
  #   "Type: test"
  #   "Type: script"

  catn("Parsing dependencies...\n")

  pkgs.db <- data.frame(Type="Type",Package="Package",Target="Target",Root="Root",Path="Path",Group=0,Test="Test")
  save.types <- c("framework","module","model","runtime","script","test","docs")
  # iterate over build.comps, creating dependencies
  for ( pkg in names(build.comps) ) {
    it <- build.comps[[pkg]]
    if ( it$Type %in% save.types ) {
      # These are the required elements: Type, Package, Root, and Path
      it.db <- data.frame(Type=it$Type,Package=pkg,Root=it$Root,Path=it$Path)
      if ( "Target" %in% names(it) ) {
        # used for now only in the 'docs' type, indicating sub-folder of output 'docs'
        # in which to place elements found at it$Path
        it.db$Target <- it$Target
      } else {
        it.db$Target <- ""
      }
      if ( "Test" %in% names(it) ) {
        tst <- names(it[["Test"]])
        if ( "Group" %in% tst ) {
          it.db$Group <- it$Test$Group
        } else {
          it.db$Group <- NA
        }
        if ( "Script" %in% tst ) {
          it.db$Test <- it$Test$Script
        } else {
          it.db$Test <- ""
        }
      } else {
        it.db$Group <- NA
        it.db$Test <- ""
      }
      pkgs.db <- rbind(pkgs.db,it.db)
      rm(it.db)
      if ( "CRAN" %in% names(it) ) {
        for ( dep in it$CRAN ) {
          dep.db <- data.frame(Type="CRAN",Package=dep,Root=NA,Path=NA,Target=NA,Group=NA,Test=NA)
          pkgs.db <- rbind(pkgs.db,dep.db)
        }
      }
      if ( "BioC" %in% names(it) ) {
        for ( dep in it$BioC ) {
          dep.db <- data.frame(Type="BioC",Package=dep,Root=NA,Path=NA,Target=NA,Group=NA,Test=NA)
          pkgs.db <- rbind(pkgs.db,dep.db)
        }
      }
      if ( "Github" %in% names(it) ) {
        for ( dep in it$Github ) {
          dep.db <- data.frame(Type="Github",Package=basename(dep),Root=NA,Path=dep,Target=NA,Group=NA,Test=NA)
          pkgs.db <- rbind(pkgs.db,dep.db)
        }
      }
    }
  }
  # print(pkgs.db)
  pkgs.db <- unique(pkgs.db[-1,])           # Remove dummy row
  row.names(pkgs.db) <- NULL                # Remove artificial row.names
  for ( d in names(pkgs.db))                # Convert factors to strings
    if ( is.factor(pkgs.db[,d]) )
      pkgs.db[,d] <- as.character(pkgs.db[,d])
  pkgs.db <- pkgs.db[order(pkgs.db$Type,pkgs.db$Group,pkgs.db$Package),] # Sort by Group (for modules)

  # New strategy:
  # We'll save pkgs.db into dependencies.RData
  # Also save row indices of the different types

  pkgs.CRAN      <- which(pkgs.db$Type=="CRAN")
  pkgs.BioC      <- which(pkgs.db$Type=="BioC")
  pkgs.Github    <- which(pkgs.db$Type=="Github")
  pkgs.framework <- which(pkgs.db$Type=="framework")
  pkgs.module    <- which(pkgs.db$Type=="module")
  pkgs.model     <- which(pkgs.db$Type=="model")
  pkgs.runtime   <- which(pkgs.db$Type=="runtime")
  pkgs.script    <- which(pkgs.db$Type=="script")
  pkgs.test      <- which(pkgs.db$Type=="test")
  pkgs.docs      <- which(pkgs.db$Type=="docs")

  # catn("Sorted by Group:")
  # print(pkgs.db[,c("Type","Package","Group")])

  # Save out the basic setup that is used in later build scripts

  ve.all.dependencies <- file.path(ve.logs,"all-dependencies.RData")

  # Non-Standard Coding: Keep this list in an easy maintain format:

  # Commas precede the item so it can be moved, or deleted, or a new item
  # added without having to edit more than one line.

  cat("Saving runtime configuration to:\n",ve.runtime.config,"\n",sep="")
  ve.env.save <- c(ve.roots,locs.lst
    , "this.R"
    , "ve.dev"
    , "dev.lib"
    , "ve.roots"
    , "ve.branches"
    , "ve.output"
    , "ve.logs"
    , "ve.installer"
    , "ve.zipout"
    , "ve.version"
    , "ve.platform"
    , "ve.build.type"
    , "ve.binary.build"
    , "ve.runtests"
    , "ve.all.dependencies"
    , "CRAN.mirror"
    , "BioC.mirror"
    , "ve.deps.url"
    , "ve.repo.url"
    , "pkgs.db"
    , "pkgs.CRAN"
    , "pkgs.BioC"
    , "pkgs.Github"
    , "pkgs.framework"
    , "pkgs.module"
    , "pkgs.model"
    , "pkgs.runtime"
    , "pkgs.script"
    , "pkgs.test"
    , "pkgs.docs"
    , "localBranch"
    , "checkBranchOnRoots"
    , "checkVEEnvironment"
    , "modulePath"
    , "moduleExists"
    , "newerThan"
    )
#   print(ve.env.save[grep("function",sapply(ve.env.save,FUN=function(x){ paste(class(get(x)),collapse=",")}))])
  suppressWarnings(save(file=ve.runtime.config,list=ve.env.save))
})
