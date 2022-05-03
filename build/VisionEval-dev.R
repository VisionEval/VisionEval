# ve.build()                        # interface to the Makefile build system
# ve.test("Package",tests="test.R") # Loads a script from the package tests directory

local({
  # Explicit repository setting for non-interactive installs
  # Replace with your preferred CRAN mirror
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos=r)

  # Establish visioneval R developer environment on the R search path
  env.builder <- grep("^ve.bld$",search(),value=TRUE)
  if ( length(env.builder)==0 ) {
    env.builder <- attach(NULL,name="ve.bld")
  } else {
    env.builder <- as.environment(env.builder)
    rm(list=ls(env.builder,all=T),pos=env.builder)
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
  assign("ve.root",getwd(),pos=env.builder) # VE-config.yml could override, but not tested for R builder
})

evalq(
  envir=as.environment("ve.bld"),
  expr={
    # Get the local git branch (if use.git, else just "visioneval")
    getLocalBranch <- function(repopath,use.git) {
      if ( use.git && suppressWarnings(requireNamespace("git2r",quietly=TRUE)) ) {
        localbr <- git2r::branches(repopath,flags="local")
        hd <- which(sapply(localbr,FUN=git2r::is_head,simplify=TRUE))
        branch <- localbr[[hd]]$name
        message("Using git branch: ",branch)
        return( branch )
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

    get.ve.runtime <- function(use.git=FALSE,use.env=TRUE) {
      # use.git will use git branch name if TRUE, otherwise "visioneval" for branch
      #   (must be consistent with ve.build(use.git=...)
      # use.env if TRUE will use system environment VE_RUNTIME (otherwise VE_RUNTIME is ignored)
      env.builder <- grep("^ve.bld$",search(),value=TRUE)
      if ( length(env.builder)==0 ) {
        env.builder <- attach(NULL,name="ve.bld")
      } else {
        env.builder <- as.environment(env.builder)
      }
      # Load the build file created by last ve.build() (to find ve-lib)
      ve.branch <- getLocalBranch(ve.root,use.git)
      this.r <- paste(R.version[c("major","minor")],collapse=".")
      build.file <- file.path(ve.root,"dev","logs",ve.branch,this.r,"dependencies.RData")
      if ( file.exists(build.file) ) {
        load(build.file,envir=env.builder)
      } else {
        message("Could not locate built runtime for R-",this.r," in ",build.file,"\n",sep="")
        return(NA)
      }
      ve.runtime <- if ( exists("ve.runtime",envir=env.builder) ) {
        if ( ! dir.exists(env.builder$ve.runtime) ) {
          message("Runtime directory for ",ve.branch," is missing.")
          NA
        } else get("ve.runtime",pos=env.builder)
      }

      # Now replace the built runtime with VE_RUNTIME if requested to do so and if it is present
      if ( ! is.na(ve.runtime) && use.env ) {
        ve.runtime <- Sys.getenv("VE_RUNTIME",env.builder$ve.runtime)
      }
      return(structure(ve.runtime,raw.runtime=env.builder$ve.runtime))
    }

    ve.run <- function(use.git=FALSE,use.env=TRUE) {
      ve.runtime <- get.ve.runtime(use.git=use.git,use.env=use.env)
      if ( ! is.na(ve.runtime) && dir.exists(ve.runtime) ) {
        owd <- setwd(ve.runtime)
        message("setwd('",owd,"') to return to previous directory")
        # Read the VisionEval.R startup from the built runtime location even if not running there
        raw.runtime <- attr(ve.runtime,"raw.runtime")
        if ( is.null(raw.runtime) ) raw.runtime <- ve.runtime
        startup.file <- file.path(raw.runtime,"VisionEval.R")
        if ( file.exists(startup.file) ) {
          message("setwd(ve.root) to return to Git root directory")
          message("startup file: ",startup.file)
          source(startup.file) # Will report directory in which we are running
        }
      } else {
        message("Have you run ve.build()?")
        owd <- getwd()
      }
      return( invisible(owd) )
    }

    ve.test <- function(VEPackage,tests="test.R",changeRuntime=TRUE,usePkgload=NULL,...) {
      # ... parameters are passed to ve.run when changeRuntime is TRUE (use.git or use.env)
      walkthroughScripts = character(0)
      if ( missing(VEPackage) || tolower(VEPackage)=="walkthrough" ) { # load the walkthrough
        if ( changeRuntime ) {
          ve.run(...)
          setwd("walkthrough")
        } else {
          message("Running in VEModel source (for developing walkthrough)")
          setwd(file.path(ve.root,"sources/framework/VEModel/inst/walkthrough"))
        }
        if ( ! file.exists("00-setup.R") ) {
          stop("No walkthrough 00-setup.R in ",getwd())
        } else {
          message("Loading walkthrough from ",normalizePath("00-setup.R",winslash="/"))
          source("00-setup.R") # will create shadow runtime directory in "walkthrough"
        }
        walkthroughScripts = dir("..",pattern="\\.R$",full.names=TRUE)
        if ( is.logical(usePkgload) && usePkgload ) {
          # Do a compatible test load of VEModel itself -- useful for using
          # walkthrough to test (and fix) VEModel.
          VEPackage = "VEModel"         # debug VEModel
          changeRuntime = FALSE         # run in location selected above
          ve.runtime <- getwd()         # override global ve.runtime
          usePkgload = NULL             # revert to default pkgload behavior
          tests = character(0)          # don't load the VEModel tests
          # Fall through to do the following while running in the walkthrough runtime
          # ve.test("VEModel",tests=character(0),changeRuntime=FALSE,usePkgload=NULL)
        } else {
          require(VEModel,quietly=TRUE)      # Walkthrough requires VEModel
          message("\nWalkthrough scripts:")
          print(walkthroughScripts)
          return(invisible(walkthroughScripts))
        }
      } else {
        ve.runtime <- get.ve.runtime(...) # use standard runtime due to changeRuntime=FALSE
      }

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
      setwd(VEPackage.path)
      if ( length(tests) > 0 ) {
        print(tests)
        expand.tests <- ! grepl("/|\\\\",tests)
        tests[expand.tests] <- file.path(VEPackage.path,"tests",tests[expand.tests])
        tests[!expand.tests] <- normalizePath(tests[!expand.tests],winslash="/",mustWork=FALSE) # relative to VEPackage.path
        tests <- tests[ file.exists(tests) ]
      }

      # Locate the runtime folder where the tests will run
      if ( changeRuntime ) {
        # Use a runtime associated specifically with the tested package
        setwd(VEPackage.path)
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
        # Use changeRuntime=FALSE to debug this module in a different module's runtime directory
        # Load the other module with changeRuntime=TRUE
        # then the new module with changeRuntime=FALSE
        message("Testing in Package runtime: ",ve.runtime)
      } else {
        # Use the standard runtime folder
        message("Testing in Existing runtime: ",ve.runtime)
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
        # so we can hopefully unload in the order provided and not trip over dependencies
        for ( pkg in VEpackages ) {
          backstop = 0
          repeat {
            message("trying to unload package ",pkg)
            try.unload <- try( unloadNamespace(pkg), silent=TRUE )
            if ( "try-error" %in% class(try.unload) && backstop < 2 ) {
              try.first <- sub("^.*imported by .([^']+). so cannot be unloaded","\\1",trimws(try.unload))
              message("Trying first to unload package ",try.first)
              try( unloadNamespace(try.first), silent=TRUE )
              backstop <- backstop + 1
            } else break
          }
        }
      }
      message("unloading visioneval")
      unloadNamespace("visioneval")

      if ( ! is.logical(usePkgload) ) usePkgload <- framework.package
      if ( usePkgload ) {
        # Use pkgload::load_all to load up the VEPackage (setwd() to the package root first)
        pkgload::load_all(VEPackage.path)
      } else {
        # Don't want to use pkgload with module packages since it will re-estimate them
        # Expect them to be built and loaded; we'll still grab their tests
        require(VEModel,quietly=TRUE)
        eval(expr=parse(text=paste0("require(",VEPackage,",quietly=TRUE)"))) # Use the built package
      }

      # (Delete and Re-)Create an environment for the package tests ("test.VEPackage) on the search
      # path sys.source each of the test files into that environment
      if ( length(tests) > 0 ) {
        test.env <- attach(NULL,name="test.VEPackage")
        for ( test in tests ) {
          # Set environment variable with path to test file.
          # Inside the test file that can be used to load auxiliary files
          # (e.g. testquery.VEqry in VEModel/tests/test.R)
          Sys.setenv(VE_test_source=dirname(test))
          sys.source(test,envir=test.env)
        }
        Sys.unsetenv("VE_test_source")
      }

      setwd(ve.runtime)
      
      # A list of the objects loaded from the "tests" will be displayed after loading
      if ( length(walkthroughScripts)>0 ) {
        message("\nWalkthrough scripts:")
        print(walkthroughScripts)
        return(invisible(walkthroughScripts))
      } else {
        tests <- objects(test.env)
        if ( length(tests)==0 ) stop(paste0("No test objects defined for ",VEPackage))
        message("\nTest functions:")
        print( tests )
        return(invisible(tests))
      }
    }
    message("ve.build() to (re)build VisionEval\n\t(see build/Building.md for advanced configuration)")
    message("ve.run() to run VisionEval (once it is built)")
    message("ve.test('VEPackage',tests='test.R') to pkgload a VisionEval package and load its tests")
    message("ve.test() loads the VEModel walkthrough runtime - VE must be built")
  }
)
