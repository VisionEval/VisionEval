# Replace the Makefile with an R builder function
# Transitional to putting the build system back into VisionEval(-dev)

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

  # Establish the VisionEval development tree root
  assign("ve.root",getwd(),pos=env.loc) # VE-config.yml could override, but not tested for R builder
})

evalq(
  envir=as.environment("ve.bld"),
  expr={
    ve.config.step <- "configure"

    # Write scripts to reset each build step based on the config
    ve.build.steps <- list(
      configure=list(   # name "configure" needs to match ve.config.step
        Path="build-config.R",
        Deps=character(0)
        ),
      repository=list(
        Path="build-repository.R",
        Deps="configure"
        ),
      external=list(
        Path="build-external.R",
        Deps="repository"
        ),
      velib=list(
        Path="build-velib.R",
        Deps="repository"
        ),
      modules=list(
        Path="build-modules.R",
        Deps="velib"
        ),
      runtime=list(
        Path="build-runtime.R",
        Deps="modules"
        ),
      docs=list(
        Path="build-docs.R",
        Deps="modules"
        ),
      inventory=list(
        Path="build-inventory.R",
        Deps=c("modules")
        ),
      runtime.pkg.bin=list(
        Path="build-runtime-packages-bin.R",
        Deps="modules"
        ),
      installer.base=list(
        Path="build-installer-base.R",
        Deps=c("runtime","docs")
      ),
      installer=list(
        Path="build-installer-bin.R",
        Deps=c("inventory", "runtime.pkg.bin","installer.base")
        ),
      runtime.pkg.src=list(
        Path="build-runtime-packages-full.R",
        Deps=c("inventory")
        ),
      installer.full=list(
        Path="build-installer-full.R",
        Deps=c("installer.base","runtime.pkg.src")
        )
    )

    ve.build.sequence = c(
      "configure",
      "repository",
      "external",
      "velib",
      "modules",
      "runtime",
      "docs",
      "inventory",
      "runtime.pkg.bin",
      "installer.base",
      "installer",
      "runtime.pkg.src",
      "installer.full"
    )

    # Developer checking (new steps)
    if ( length( unseq <- setdiff(names(ve.build.steps),ve.build.sequence) ) ) {
      print(unseq)
      stop("Build steps configured that are not in ve.build.sequence")
    } else rm(unseq)
    if ( length( unconf <- setdiff(ve.build.sequence,names(ve.build.steps)) ) ) {
      print(unconf)
      stop("Sequence steps listed with no module configuration")
    } else rm(unconf)

    ve.make <- function(
      targets=character(0),
      r.version=paste(R.version[c("major","minor")],collapse="."),
      flags=character(0)
    ) {
      owd<-setwd(file.path(ve.root,"build"))
      if ( length(r.version)>0 ) {
        r.version=paste0("VE_R_VERSION=",r.version)
      }
      make.me <- paste("make",flags,r.version,targets)
      shell(make.me)
      setwd(owd)
    }

    # ve.build : Helper function to run the build process
    # Default "runtime" step will build a working runtime (but not docs, inventory, installer)

    ve.build <- function(
      steps="runtime",   # list of build steps (no special order); explicit names get automatically reset
      reset=FALSE,       # if TRUE, automatically add "configure" step
      dependencies=TRUE) # add dependencies to build steps (otherwise just rebuild the explicit step)
      {
        # run this function from ve.root
        ve.root <- getwd()
        assign("ve.root",ve.root,pos=as.environment("ve.bld")) # VE-config.yml could override, but not tested for R builder
        setwd(ve.root)

        # ========================= CONSISTENCY CHECKS =========================

        # Check for errors in steps
        bad.steps <- ! steps %in% ve.build.sequence
        if ( any(bad.steps) ) stop("Build steps provided but not in sequence: ",steps[bad.steps])

        # ========================= BUILD ENVIRONMENT =========================

        # Create the builder environment for VisionEval
        # We will re-create this when we build different branches or R versions
        env.builder <- if ( length(grep("^ve.builder$",search()))>0 ) {
          as.environment("ve.builder")
        } else {
          reset = TRUE
          attach(NULL,name="ve.builder")
        }

        # Now get the ve.builder environment set up
        current.R <- paste(R.version[c("major","minor")],collapse=".")
        current.dev <- normalizePath(file.path(ve.root,"dev"),winslash="/",mustWork=FALSE)
        current.dev.lib <- normalizePath(file.path(current.dev,"lib",current.R),winslash="/",mustWork=FALSE)
        if ( ! dir.exists(current.dev.lib) ) dir.create(current.dev.lib, recursive=TRUE, showWarnings=FALSE )
        .libPaths(current.dev.lib)

        # Figure out R version and branch
        # Construct ve.dev, dev.lib, ve.logs (all of which live outside the "built" folder)
        # Look in ve.logs for the ve.runtime.configuration (for this branch and version)
        # If it exists, then load it into "ve.builder"
        if ( ! suppressWarnings(require("git2r",quietly=TRUE)) ) {
          require("utils")
          utils::install.packages("git2r",
            lib=current.dev.lib,
            dependencies=NA, type=.Platform$pkgType )
        }
        current.branch <- if ( git2r::in_repository(ve.root) ) {
          localbr <- git2r::branches(ve.root,flags="local")
          hd <- which(sapply(localbr,FUN=git2r::is_head,simplify=TRUE))
          localbr[[hd]]$name
        }  else "visioneval"
        if ( exists("localbr") ) rm(localbr) 
        if ( exists("hd") )      rm(hd)

        if (
          (
            exists("this.R",envir=env.builder) &&
            get("this.R",envir=env.builder) != current.R
          ) ||
          (
            exists("ve.branch",envir=env.builder) &&
            get("ve.branch",envir=env.builder) != current.branch
          )
        ) {
          rm(list=ls(env.builder,all=T),pos=env.builder)
          assign("this.R",current.R,envir=env.builder)
          assign("ve.branch",current.branch,envir=env.builder)
        }

        # Set up the "developer" elements of the builder environment
        # Key elements are always be reset)
        current.logs <- file.path(current.dev,"logs",current.branch,current.R)
        ve.build.dir <- file.path(getwd(),"build")
        ve.runtime.config <- file.path(current.logs,"dependencies.RData")
        if ( file.exists(ve.runtime.config) && ! reset ) {
          load(ve.runtime.config,envir=env.builder)
        } else {
          reset = TRUE # need to rebuild
        }
        assign("ve.build.dir",ve.build.dir,envir=env.builder)
        assign("ve.root",getwd(),envir=env.builder)
        assign("ve.dev",current.dev,envir=env.builder)
        assign("dev.lib",current.dev.lib,envir=env.builder)
        assign("ve.logs",current.logs,envir=env.builder)

        # ========== IDENTIFY ALREADY-BUILT STEPS ==========

        built.prefix <- "ve.built."
        built.pattern <- paste0("^",gsub("\\.","\\\\.",built.prefix))

        getBuiltSteps <- function(steps=character(0)) {
          built.steps <- ls("ve.builder",pattern=built.pattern)
          if ( length(built.steps)>0 && all(nzchar(built.steps)) ) {
            names(built.steps) <- gsub(built.pattern,"",built.steps)
            if ( any(nzchar(steps)) ) {
              built.steps <- built.steps[ names(built.steps) %in% steps ]
            } else {
              built.steps = character(0)
            }
          } else {
            built.steps = character(0)
          }
          return(built.steps) # names are steps, values are built flag variable names
        }
        isBuilt <- function(step) return( length(getBuiltSteps(step))>0 )

        # ========================= CONSTRUCT BUILD STEPS =========================

        # Set up build order
        build.set <- unique(steps)

        # Reset requires us to rebuild config, even if not explicitly requested
        if ( reset && ! ve.config.step %in% build.set ) {
          build.set <- c(build.set,ve.config.step)
        }

        # recursive function to add steps and dependencies to build list
        add.module <- function(build.set,module) {
          if ( dependencies ) {
            for ( m in ve.build.steps[[module]]$Deps ) {
              # Add dependency to list if is not built and not already requested
              # Also add it if we've requested full reset
              if ( reset ||( ! m %in% build.set && ! isBuilt(m) ) ) {
                build.set <- add.module(build.set,m)
              }
            }
          }
          return(c(build.set,module))
        }
        for ( m in steps ) build.set <- add.module(build.set,m)

        build.set <- unique(build.set) # may get a step twice if multiple dependents
        to.build <- ve.build.sequence %in% build.set
        build.set <- ve.build.sequence[ to.build ] # establish sequence for build set

        # ========================= PERFORM RESET =========================

        # remaining explicit build steps will be reset (not dependencies)
        if ( reset ) {
          cat("Resetting build status for Build Set.\n")
          rm(list=getBuiltSteps(build.set),envir=as.environment("ve.builder"))
        } else {
          rm(list=getBuiltSteps(steps),envir=as.environment("ve.builder")) # only the named ones
          build.set <- build.set [ ! ( build.set %in% names(getBuiltSteps(build.set)) ) ]
        }

        # ========================= PERFORM BUILD STEPS =========================

        for ( step in build.set ) {
          # Get build step configuration
          s.config <- ve.build.steps[[step]]
          s.path <- file.path(ve.build.dir,"scripts",s.config$Path)
          if ( ! file.exists(s.path) ) {
            stop("Missing build script: ",s.path)
            next
          }
          build.success <- FALSE
          tryCatch( {
            sys.source(s.path,envir=as.environment("ve.builder"))
            build.success <- TRUE
          },
            error=function(e) { print(e) },
            finally=setwd(ve.root)
          )
          if ( ! build.success ) {
            stop("Build step failure in ",step)
          } else {
            assign(paste0(built.prefix,step), Sys.time(),envir=as.environment("ve.builder"))
          }
        }
      }

    # Run the resultant runtime
    get.ve.runtime <- function() {
      ve.runtime <- Sys.getenv("VE_RUNTIME","") # built in previous session
      if ( ! nzchar(ve.runtime) ) {             # built in this session
        if ( "ve.builder" %in% search() &&
          exists("ve.runtime",pos=as.environment("ve.builder") ) ) {
          ve.runtime <- get("ve.runtime",pos=as.environment("ve.builder"))
        }
      }
      return(ve.runtime)
    }

    ve.run <- function() {
      ve.runtime <- get.ve.runtime()
      if (
        dir.exists(ve.runtime) &&
        file.exists(file.path(ve.runtime,"VisionEval.R"))
      ) {
        message("setwd(ve.root) to return to Git root")
        setwd(ve.runtime)
        source("VisionEval.R")
      } else {
        message("Runtime not built. Run ve.build()")
      }
    }
    message("ve.make() to (re)build VisionEval")
    message("ve.build() is experimental R re-implementation")
    message("ve.run() to run VisionEval (once it is built)")
  }
)
