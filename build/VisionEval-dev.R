# ve.build()                        # interface to the Makefile build system
# ve.test("Package",tests="test.R") # Loads a script from the package tests directory

local({
  # Explicit repository setting for non-interactive installs
  # Replace with your preferred CRAN mirror
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos=r)

  # Establish visioneval R developer environment on the R search path
  env.loc <- grep("^ve.bld$",search(),value=TRUE)
  if ( length(env.loc)==0 ) {
    env.loc <- attach(NULL,name="ve.bld")
  } else {
    env.loc <- as.environment(env.loc)
    rm(list=ls(env.loc,all=T),pos=env.loc)
  }

  # Check that Rtools or a development environment is available.
  # pkgbuild should be in ve-lib
  if ( requireNamespace("pkgbuild", quietly=TRUE) && ! pkgbuild::has_build_tools() ) {
    if ( .Platform$OS.type == "Windows" ) {
      msg <- paste0(
        "Please install Rtools 4.0 for Windows, and make sure it is available in the system PATH.\n",
        "https://cran.r-project.org/bin/windows/Rtools/"
      )
    } else {
      msg <- "R Build environment is not available."
    }
    stop(msg)
  }

  # Establish the VisionEval development tree root
  assign("ve.root",getwd(),pos=env.loc) # VE-config.yml could override, but not tested for R builder
})

evalq(
  envir=as.environment("ve.bld"),
  expr={
    # Get the local git branch (if use.git, else just "visioneval")
    getLocalBranch <- function(repopath,use.git) {
      if ( use.git && requireNamespace("git2r",quietly=TRUE) ) {
        localbr <- git2r::branches(repopath,flags="local")
        hd <- which(sapply(localbr,FUN=git2r::is_head,simplify=TRUE))
        return( localbr[[hd]]$name )
      } else {
        return( "visioneval" )
      }
    }

    ve.build <- function(
      targets=character(0),
      r.version=paste(R.version[c("major","minor")],collapse="."),
      config=character(0), # defaults to config/VE-config.yml
      express=TRUE, # if FALSE, will run devtools::check; otherwise build as rapidly as possible
      flags=character(0), # flags for VE_CONFIG or VE_EXPRESS will override the direct flag 
      use.git=FALSE
    ) {
      owd <- setwd(file.path(ve.root,"build"))
      r.home <- Sys.getenv("R_HOME")
      tryCatch(
        { 
          if ( length(r.version)>0 ) {
            r.version <- r.version[1] # only the first is used
            Sys.setenv(VE_R_VERSION=r.version)
            Sys.unsetenv("R_HOME") # R Studio forces this to its currently configured R
          }
          if ( length(config)>0 ) {
            config <- config[1]
            if ( file.exists(config) ) {
              Sys.setenv(VE_CONFIG=config)
            }
          }
          if ( express ) {
            Sys.setenv(VE_EXPRESS="YES")
          } else {
            Sys.unsetenv("VE_EXPRESS")
          }
          # Force our own version of VE_BRANCH
          Sys.setenv(VE_BRANCH = getLocalBranch(ve.root, use.git))
          make.me <- paste("make",paste(flags,collapse=" "),paste(targets,collapse=" "))
          if ( invisible(status <- system(make.me)) ) stop("Build exited with status: ",status,call.=FALSE)
        },
        error = function(e) e, # Probably should clean up
        finally =
        {
          Sys.setenv(R_HOME=r.home)
          setwd(owd)
        }
      )
    }

    get.ve.runtime <- function(use.git=FALSE) {
      # TODO: add override via VE_RUNTIME system environment variable
      ve.runtime = Sys.getenv("VE_RUNTIME",NA)
      if ( is.na(ve.runtime) ) {
        if ( ! "ve.builder" %in% search() ) {
          env.builder <- attach(NULL,name="ve.builder")
        } else {
          env.builder <- as.environment("ve.builder")
        }
        if ( ! exists("ve.runtime",envir=env.builder) ) {
          ve.branch <- getLocalBranch(ve.root,use.git)
          this.r <- paste(R.version[c("major","minor")],collapse=".")
          build.file <- file.path(ve.root,"dev","logs",ve.branch,this.r,"dependencies.RData")
          if ( file.exists(build.file) ) {
            load(build.file,envir=env.builder)
          } else {
            cat("Could not locate build for R-",this.r," in ",build.file,"\n",sep="")
          }
        }
        if ( exists("ve.runtime",envir=env.builder) ) {
          ve.runtime <- get("ve.runtime",pos=env.builder)
        }
      }
      return(ve.runtime)
    }

    ve.run <- function() {
      ve.runtime <- get.ve.runtime()
      owd <- getwd()
      if (
        ! is.na(ve.runtime) &&
        dir.exists(ve.runtime) &&
        file.exists(file.path(ve.runtime,"VisionEval.R"))
      ) {
        owd <- setwd(ve.runtime)
        message("setwd('",owd,"') to return to previous directory")
        message("setwd(ve.root) to return to Git root directory")
        source("VisionEval.R")
      } else {
        message("Could not locate runtime. Run ve.build()")
      }
      return( invisible(owd) )
    }

    ve.test <- function(VEPackage,tests="test.R",localRuntime=TRUE) {
      if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
        stop("Missing required package: 'pkgload'")
      }
      VEPackage <- VEPackage[1] # can only test one at a time

      # Make sure we start looking from the Github root
      setwd(ve.root)
 
      # Locate the full path to VEPackage
      framework.package <- FALSE
      if (
        ! grepl("/|\\\\",VEPackage) && (
          ( framework.package <- file.exists( VEPackage.path <- file.path("sources","framework",VEPackage) ) ) ||
          ( file.exists( VEPackage.path <- file.path("sources","modules",VEPackage) ) )
        )
      ) {
        VEPackage.path <- normalizePath(VEPackage.path,winslash="/",mustWork=FALSE)
        message("Found ",VEPackage.path)
      } else if ( file.exists(VEPackage) ) {
          VEPackage.path <- VEPackage
          VEPackage <- basename(VEPackage.path)
      } else {
        stop("Could not locate ",VEPackage)
      }
      message("Testing ",VEPackage," in ",VEPackage.path)

      # expand "tests" to the full path of each test file
      # (1) prepend file.path(VEPackage.path,"tests") to all files without separators
      # (2) check each file for existing and report those that don't exist
      # (3) normalize all the test paths
      owd <-setwd(VEPackage.path)
      expand.tests <- ! grepl("/|\\\\",tests)
      tests[expand.tests] <- file.path(VEPackage.path,"tests",tests[expand.tests])
      tests[!expand.tests] <- normalizePath(tests[!expand.tests],winslash="/",mustWork=FALSE) # relative to VEPackage.path
      tests <- tests[ file.exists(tests) ]
#       if ( length(tests)==0 ) {
#         stop("No 'test.R' file in ",VEPackage.path)
#       }

      # Locate the runtime folder where the tests will run
      # TODO: work nicely with ve.run()
      if ( localRuntime ) {
        owd <- setwd(VEPackage.path)
        if ( dir.exists("tests") ) { # in VEPackage.path
          ve.runtime <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)[1]
          if ( dir.exists(ve.runtime) ) {
            ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=FALSE)
          }
        } else {
          dir.create("tests")
          ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="tests"),winslash="/",mustWork=FALSE)
        }
        if ( ! dir.exists(ve.runtime) ) {
          ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="tests"),winslash="/",mustWork=FALSE)
          dir.create(ve.runtime)
        }
        model.path <- file.path(ve.runtime,"models")
        if ( ! dir.exists(model.path) ) dir.create(model.path)
        Sys.setenv(VE_RUNTIME=ve.runtime) # override VEModel notion of ve.runtime
        message("Testing in Local runtime: ",ve.runtime)
      } else {
        ve.runtime <- ve.run()
        message("Testing in Built runtime: ",ve.runtime)
      }

      # Detach and unload VE-like Packages
      pkgsLoaded <- names(utils::sessionInfo()$otherPkgs)
      VEpackages <- grep("(^VE)|(^visioneval$)",pkgsLoaded,value=TRUE)
      if ( length(VEpackages)>0 ) {
        VEpackages <- paste0("package:",VEpackages)
        for ( pkg in VEpackages ) {
          message("detaching ",pkg)
          detach(pkg,character.only=TRUE)
        }
      }
      nameSpaces <- names(utils::sessionInfo()$loadedOnly)
      VEpackages <- grep("^VE",nameSpaces,value=TRUE)
      if ( length(VEpackages)>0 ) {
        # sessionInfo keeps packages in the reverse order of loading (newest first)
        # so we can unload in the order provided and not trip over dependencies
        for ( pkg in VEpackages ) {
          message("unloading package ",pkg)
          unloadNamespace(pkg)
        }
      }
      message("unloading visioneval")
      unloadNamespace("visioneval")

      if ( framework.package ) {
        # Use pkgload::load_all to load up the VEPackage (setwd() to the package root first)
        pkgload::load_all(VEPackage.path)
      } else {
        # Don't want to use pkgload with module packages since it will re-estimate them
        # Force them to be built and loaded as built; we'll still grab their tests
        require(VEModel,quietly=TRUE)
        require(VEPackage,quietly=TRUE) # Use the built package
      }

      # (Delete and Re-)Create an environment for the package tests ("test.VEPackage) on the search
      # path sys.source each of the test files into that environment
      test.env <- attach(NULL,name="test.VEPackage")
      for ( test in tests ) {
        sys.source(test,envir=test.env)
      }

      message("Trying to run in ",ve.runtime)
      setwd(ve.runtime)
      
      # A list of the objects loaded from the "tests" will be displayed after loading
      tests <- objects(test.env)
      if ( length(tests)==0 ) stop(paste0("No test objects defined for ",VEPackage))
      return( tests )
    }
    message("ve.build() to (re)build VisionEval\n\t(see build/Building.md for advanced configuration)")
    message("ve.run() to run VisionEval (once it is built)")
    message("ve.test('VEPackage',tests='test.R') to pkgload a VisionEval package and load its tests")
  }
)
