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
    env.loc <- "ve.bld"
    attach(NULL,name=env.loc)
  }

  # Establish the VisionEval development tree root
  assign("ve.root",getwd(),pos=env.loc)
})

evalq(
  envir=as.environment("ve.bld"),
  expr={
    ve.config.step <- "configure"

    # Write scripts to reset each build step based on the config
    ve.build.steps <- list(
      configure=list(   # name "configure" needs to match ve.config.step
        Path="build-config.R",
        Deps=character(0),
        Clean=""
        ),
      repository=list(
        Path="build-repository.R",
        Deps="configure",
        Clean=""
        ),
      external=list(
        Path="build-external.R",
        Deps="repository",
        Clean=""
        ),
      velib=list(
        Path="build-velib.R",
        Deps="repository",
        Clean=""
        ),
      modules=list(
        Path="build-modules.R",
        Deps="velib",
        Clean=""
        ),
      runtime=list(
        Path="build-runtime.R",
        Deps="modules",
        Clean=""
        ),
      docs=list(
        Path="build-docs.R",
        Deps="modules",
        Clean=""
        ),
      inventory=list(
        Path="build-inventory.R",
        Deps=c("modules"),
        Clean=""
        ),
      runtime.pkg.bin=list(
        Path="build-runtime-packages-bin.R",
        Deps="modules",
        Clean=""
        ),
      installer.base=list(
        Path="build-installer-base.R",
        Deps=c("runtime","docs"),
        Clean=""
      ),
      installer.bin=list(
        Path="build-installer-bin.R",
        Deps=c("inventory", "runtime.pkg.bin","installer.base"),
        Clean=""
        ),
      runtime.pkg.src=list(
        Path="build-runtime-packages-full.R",
        Deps=c("inventory"),
        Clean=""
        ),
      installer.full=list(
        Path="build-installer-full.R",
        Deps=c("installer.base","runtime.pkg.src"),
        Clean=""
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
      "installer.bin",
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

    ve.outdated <- function(step)
    {
      # check step for its artifacts (appeal to VE-Components.yml for
      # the ones configured there)
      # Explain in build steps above how to deal with non-components
      # (e.g. installers)
    }

    # ve.build : Helper function to run the build process
    # Default "runtime" step will build a working runtime (but not docs, inventory, installer)

    ve.build <- function(
      steps="runtime",   # list of build steps (no special order); explicit names get automatically reset
      build=TRUE,        # if FALSE, just reset steps and process clean lists (don't build)
      reset=FALSE,       # if TRUE, automatically add "configure" step
      clean=steps,       # list of build steps whose artifacts we should remove (implies reset)
      dependencies=TRUE) # add dependencies to build steps (otherwise just rebuild the explicit step)
      {
        # run this function from ve.root
        # build and clean scripts may change to another location
        setwd(ve.root)

        # ========================= CONSISTENCY CHECKS =========================

        # Check for errors in steps and clean vectors
        bad.steps <- ! steps %in% ve.build.sequence
        if ( any(bad.steps) ) stop("Build steps provided but not in sequence: ",steps[bad.steps])
        bad.clean <- ! clean %in% ve.build.sequence
        if ( any(bad.clean) ) stop("Clean steps provided but not in sequence: ",clean[bad.clean])
        bad.clean <- ! clean %in% steps
        if ( any(bad.clean) ) message("Steps will be cleaned but not rebuilt: ",clean[bad.clean])

        # ========================= BUILD ENVIRONMENT =========================

        # Create the builder environment for VisionEval
        # We will re-create this when we build different branches or R versions
        if ( length(grep("^ve.builder$",search()))>0 ) {
          cat("Using existing build environment: 've.builder'\n")
        } else {
          cat("Creating new build environment.\n")
          attach(NULL,name="ve.builder")
          ve.runtime.config <- Sys.getenv("VE_RUNTIME_CONFIG","")
          if (
            length(ve.runtime.config)==1 &&
            nzchar(ve.runtime.config) &&
            file.exists(normalizePath(ve.runtime.config,winslash="/"))
          ) {
            load(ve.runtime.config,envir=as.environment("ve.builder"))
          }
        }

        # Establish the root and build folders
        ve.build.dir <- file.path(ve.root,"build")
        assign("ve.build.dir",ve.build.dir,envir=as.environment("ve.builder"))
        assign("ve.root",ve.root,envir=as.environment("ve.builder"))

        # Set up the "developer" elements of the builder environment
        # These elements are not part of "configure" step because they depend
        # on the running R environment, not on the particular
        # VisionEval we're building

        evalq(
          envir=as.environment("ve.builder"),
          expr={

            if ( ! exists("this.R") ) {
              this.R <- paste(R.version[c("major","minor")],collapse=".")
            }
            if ( ! exists("ve.dev") ) {
              ve.dev <- normalizePath(file.path(ve.root,"dev"),winslash="/",mustWork=FALSE)
            }
            if ( ! exists("dev.lib") ) {
              dev.lib <- normalizePath(file.path(ve.dev,"lib",this.R),winslash="/",mustWork=FALSE)
              dir.create(dev.lib, recursive=TRUE, showWarnings=FALSE )
            } else {
              if ( ! dir.exists(dev.lib) ) dir.create(dev.lib, recursive=TRUE, showWarnings=FALSE )
            }
            .libPaths(dev.lib)
          })

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

        # ========================= CLEAN STEPS =========================

        # Perform the clean operations (remove artifacts by running script)
        clean.set <- unique(clean)
        clean.set <- ve.build.sequence[ ve.build.sequence %in% clean.set ]
        if ( any( nzchar(clean.set) ) ) { # use 'any' to force logical if nzchar returns nothing
          # Perform reset 
          to.reset <- getBuiltSteps(clean.set)
          if (length(to.reset)>0) rm(list=getBuiltSteps(clean.set),envir=as.environment("ve.builder"))
          for ( m in clean.set ) {
            m.config <- ve.build.steps[[m]]
            if ( "Clean" %in% names(m.config) ) {
              for ( cln in m.config$Clean ) { # Allow for multiple scripts, but one or zero
                if ( nzchar(cln) ) {
                  cat("Would run:",cln,"in ve.builder\n")
                  # Would run: clean.result <- try(sys.source(cln,envir="ve.builder") )
                }
              }
            }
          }
        } else {
          message("No steps to clean.")
        }

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

        # clean steps were automatically reset (above)
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
    # Environment variable set in .Renviron if we have run build successfully
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
        message("Runtime not built. Run ve.build()\n")
      }
    }
    message("ve.build() to (re)build VisionEval")
    message("ve.run() to run VisionEval (once it is built)")
  }
)
