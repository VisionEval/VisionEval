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

    get.ve.runtime <- function(runtime=NULL, use.git=FALSE,use.env=TRUE) {
      # Locate dthe working directory in which to look for the VisionEval.R initialization. Options are:
      #   1. if runtime is an existing directory (could be getwd()), establish that as ve.runtime
      #   2. Find load.runtime via the "built" tree (for loading VisionEval.R, walkthrough or tests)
      #      "runtime" directory within the "built" tree (use.git controls how that tree was built)
      #   2. Set the runtime working directory like this:
      #      1. Use runtime parameter if provided
      #      1. Use current ve.runtime in "ve.env" environment, if present
      #      3. Use VE_RUNTIME environment variable (if set, and use.env is TRUE)
      #      4. Issue warning about writing in build tree, then use load.runtime
      #      
      # use.git will use git branch name if TRUE, otherwise "visioneval" for branch
      #   (must be consistent with ve.build(use.git=...)
      # use.env if TRUE to enable using system environment VE_RUNTIME
      # (otherwise VE_RUNTIME is ignored; see above)

      # env.builder has information related to building VisionEval
      env.builder <- grep("^ve.bld$",search(),value=TRUE)
      if ( length(env.builder)==0 ) {
        env.builder <- attach(NULL,name="ve.bld")
      } else {
        env.builder <- as.environment(env.builder)
      }

      # runtime parameter is where to do the work:
      runtime <- if ( ! is.character(runtime) || ! dir.exists(runtime) )
        NULL else normalizePath(runtime,mustWork=TRUE,winslash="/")

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

      # load.runtime is location for loading VisionEval.R and r.version
      # That folder will also locate the walkthrough scripts and tools if they are not present in
      # the ve.runtime 
      load.runtime <- if ( exists("ve.runtime",envir=env.builder) ) {
        if ( ! dir.exists(env.builder$ve.runtime) ) {
          message("Runtime directory for ",ve.branch," is missing.")
          NA
        } else get("ve.runtime",pos=env.builder)
      }

      # Now replace the built runtime with runtime parameter or VE_RUNTIME if requested to do so and if it is present
      ve.runtime <- if ( is.character(runtime) ) {
        # message("Runtime override: ",runtime)
        runtime
      } else if ( use.env ) {
        env.runtime <- Sys.getenv("VE_RUNTIME",ve.runtime)
        if ( dir.exists(env.runtime) ) {
          # message("Explicit VE_RUNTIME exists: ",env.runtime)
          env.runtime
        } else {
          # message("Explicit VE_RUNTIME missing: ",env.runtime)
          load.runtime
        }
      } else {
        # "runtime.test" is created as a default "live" runtime by ve.run(0
        # if there is not better choice.
        runtime.test <- file.path(dirname(load.runtime),"runtime.test")
        if ( dir.exists(runtime.test) ) {
          # message("Using existing runtime.test: ",runtime.test)
          runtime.test
        } else {
          # message("Runtime.test not created yet: ",runtime.test)
          load.runtime
        }
      }
      
      return(structure(ve.runtime,load.runtime=load.runtime))
    }

    # runtime says where to set up the "live" runtime
    # use.git says that the build will be using the git branch name instead of "visioneval"
    # use.env says to look for VE_RUNTIME in the environment
    # changeDir says to change the directory to ve.runtime; if FALSE, stay in current directory
    # copyFiles says to copy the built files from the built runtime to the "working" runtime (e.g. runtime.test)
    #   copyFiles can be TRUE (copy entire runtime) or a vector of sub-directories of load.runtime to copy
    #   (e.g. "walkthrough")
    ve.run <- function(runtime=NULL, use.git=FALSE,use.env=TRUE,changeDir=TRUE,copyFiles=FALSE) {
      ve.runtime <- get.ve.runtime(runtime, use.git=use.git,use.env=use.env)
      message("get.ve.runtime returned ",ve.runtime,": use.env ",use.env)
      if ( is.na(ve.runtime) ) stop("No runtime available. Have you run ve.build()")
      load.runtime <- attr(ve.runtime,"load.runtime")
      useWorkingDir <- ! changeDir
      if ( ! useWorkingDir ) {
        if ( is.na(ve.runtime) || load.runtime == ve.runtime ) {
          # Try to avoid running in the built runtime directory
          runtime.test <- file.path(dirname(load.runtime),"runtime.test")
          if ( ! dir.exists(runtime.test) ) {
            message("Creating runtime.test directory:")
            dir.create(runtime.test)
          }
          message("Using runtime test directory:",runtime.test)
          ve.runtime <- runtime.test
        } else if ( ! is.na(ve.runtime) && dir.exists(ve.runtime) ) {
          owd <- setwd(ve.runtime)
        } else useWorkingDir <- TRUE
      }

      # Fall back to current directory
      if ( useWorkingDir ) owd <- ve.runtime <- getwd() else owd <- getwd()

      # Report how to step back to previous directory
      if ( owd != ve.runtime ) message("setwd('",owd,"') to return to previous directory")

      # Load auxiliary files to the current runtime if requested (will overwrite existing)
      if ( ( is.character(copyFiles) || isTRUE(copyFiles) ) && load.runtime != ve.runtime ) {
        if ( is.character(copyFiles ) ) {
          # copy
          for ( cf in copyFiles ) {
            # need to copy all.files to include .Rprofile; full.names to identify which
            # file and sub-hierarchy to bring across.
            to.copy <- dir( file.path(load.runtime, cf), all.files=TRUE, full.names=TRUE, no..=TRUE )
            to.dir <- file.path(ve.runtime,cf)
            if ( ! dir.exists(to.dir) ) dir.create(to.dir)
            file.copy( to.copy, to.dir, recursive=TRUE )
          }
        } else {
          to.copy <- dir(load.runtime,all.files=TRUE, full.names=TRUE, no..=TRUE)
          file.copy( to.copy , ve.runtime, recursive=TRUE )
          load.runtime <- ve.runtime # Load the startup script from load.runtime
        }
      }
      # In general, there's no need to copy the runtime files over except for
      # running the tests or the walkthrough. See ve.test implementation below

      # Now run the startup script
      startup.file <- file.path(load.runtime,"VisionEval.R")
      if ( file.exists(startup.file) ) {
        if ( ve.root != owd ) message("setwd(ve.root) to return to Git root directory")

        env.loc <- if ( ! "ve.env" %in% search() ) {
          attach(NULL,name="ve.env")
        } else {
          as.environment("ve.env")
        }
        env.loc$ve.runtime <- ve.runtime
        if ( ve.runtime != getwd() ) setwd(ve.runtime)
        source(startup.file,chdir=TRUE)
      } else {
        message("No startup file in ",ve.load.dir)
        message("Have you run ve.build()?")
      }

      return( invisible(structure(ve.runtime,load.runtime=load.runtime)) )
    }

    ve.test <- function(VEPackage,tests="test.R",changeRuntime=TRUE,usePkgload=NULL,use.git=FALSE,use.env=TRUE) {
      # Run with the walkthrough if no package provided (or "walkthrough")

      # Then we'll redirect to other locations as needed
      walkthroughScripts = character(0)
      if ( missing(VEPackage) || tolower(VEPackage)=="walkthrough" ) { # load the walkthrough
        if ( changeRuntime ) {
          # If ve.runtime has not moved away from ve.root (i.e. ve.run() was not yet called)
          # then do the default ve.run first. Otherwise, we'll place the walkthrough in ve.runtime.
          changeDir <- getwd()==ve.root
          # Do walkthrough below current runtime directory
          ve.runtime <- ve.run(changeDir=changeDir,copyFiles="walkthrough",use.git=use.git,use.env=use.env)
          setwd(file.path(ve.runtime,"walkthrough"))
        } else {
          message("Running in VEModel source (for developing walkthrough)")
          setwd(file.path(ve.root,"sources/framework/VEModel/inst/walkthrough"))
        }
        if ( ! file.exists("00-setup.R") ) {
          stop("No walkthrough 00-setup.R in ",getwd())
        } else {
          # 00-setup.R will create a temporary runtime in "walkthrough"
          message("Loading walkthrough from ",normalizePath("00-setup.R",winslash="/"))
          source("00-setup.R") # will create shadow runtime directory in "walkthrough"
        }
        # Running now in temporary runtime of "walkthrough"
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
        # Set the base runtime
        ve.runtime <- get.ve.runtime(use.git=use.git,use.env=use.env) # use standard runtime for testing another package
      }

      # Make sure pkgload is available
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
        env.testPackage <- "test.VEPackage"
        if ( env.testPackage %in% search() ) {
          detach(env.testPackage,character.only=TRUE)
        }
        test.env <- attach(NULL,name=env.testPackage)
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
    } # end of ve.test function definition

    message("ve.build() to (re)build VisionEval\n\t(see build/Building.md for advanced configuration)")
    message("ve.run() to run VisionEval (once it is built)")
    message("ve.test('VEPackage',tests='test.R') to pkgload a VisionEval package and load its tests")
    message("ve.test() loads the VEModel walkthrough runtime - VE must be built")
  }
)
