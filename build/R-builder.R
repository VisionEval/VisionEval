# Replace the Makefile with an R builder function
# Transitional to putting the build system back into VisionEval(-dev)

local({
  # Better repository setting for non-interactive installs
  # Replace with your preferred CRAN mirror
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos=r)
})

ve.config.step <- "configure"

# Write scripts to reset each build step based on the config
ve.build.steps <- list(
  configure=list(   # name "configure" needs to match ve.config.step
    Path="scripts/build-config.R",
    Deps=character(0),
    Clean=""
    ),
  repository=list(
    Path="scripts/build-repository.R",
    Deps=character(0),
    Clean=""
    ),
  external=list(
    Path="scripts/build-external.R",
    Deps="repository",
    Clean=""
    ),
  velib=list(
    Path="scripts/build-velib.R",
    Deps="repository",
    Clean=""
    )
  modules=list(
    Path="scripts/build-modules.R",
    Deps=c("velib"),
    Clean=""
    ),
  runtime=list(
    Path="scripts/build-runtime.R",
    Deps=character(0),
    Clean=""
    ),
  docs=list(
    Path="scripts/build-docs.R",
    Deps="modules",
    Clean=""
    ),
  inventory=list(
    Path="scripts/build-inventory.R",
    Deps=c("modules"),
    Clean=""
    ),
  runtime.pkg.bin=list(
    Path="scripts/build-runtime-packages-bin.R",
    Deps="modules",
    Clean=""
    ),
  installer.base=list(
    Path="scripts/build-installer-base.R",
    Deps=c("runtime","docs"),
    Clean=""
  ),
  installer.bin=list(
    Path="scripts/build-installer-bin.R",
    Deps=c("inventory", "runtime.pkg.bin","installer.base"),
    Clean=""
    ),
  runtime.pkg.src=list(
    Path="scripts/build-runtime-packages-full.R",
    Deps=c("inventory"),
    Clean=""
    ),
  installer.full=list(
    Path="scripts/build-installer-full.R",
    Deps=c("installer.base","runtime.pkg.src"),
    Clean=""
    ),
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
if ( length( setdiff(ve.build.steps,ve.build.sequence) ) ) stop("Build steps configured that are not in ve.build.sequence")
if ( length( setdiff(ve.build.sequence,ve.build.steps) ) ) stop("Sequence steps listed with no module configuration")

ve.build <- function(
  steps="runtime",   # list of build steps (no special order)
  build=TRUE,        # if FALSE, do reset if requested but do not rebuild
  reset=FALSE,       # if TRUE, reset shortcut build flags for listed steps + dependencies
  clean=steps,       # list of build steps whose artifacts we should remove
                     # use character(0) or "" not to visit anything marked "ve.built"
  dependencies=TRUE) # add dependencies to build steps
{
  # ========================= CONSISTENCY CHECKS =========================

  # Check for errors in steps and clean vectors
  bad.steps <- ! steps %in% ve.build.sequence
  if ( any(bad.steps) ) stop("Build steps provided but not in sequence: ",steps[bad.steps])
  bad.clean <- ! clean %in% ve.build.sequence
  if ( any(bad.clean) ) stop("Clean steps provided but not in sequence: ",clean[bad.clean])
  bad.clean <- ! clean %in% steps
  if ( any(bad.clean) ) message("Steps will be cleaned but not rebuilt: ",clean[bad.clean])

  # ========================= BUILD ENVIRONMENT =========================

  # Create the build environment for VisionEval
  env.loc <- grep("^ve.env$",search(),value=TRUE)
  if ( length(env.loc)==0 ) ) {
    cat("Using existing build environment",env.loc[1],"\n")
    as.environment(env.loc[1])
  } else {
    cat("Creating new build environment.\n")
    attach(NULL,name="ve.env")
  }
  rm(env.loc)

  # Set up the "developer" elements of the built environment
  # These elements are not part of "configure" step because they depend
  # on the running R environment, not on the particular VisionEval we're building
  evalq({
    this.R <- paste(R.version[c("major","minor")],collapse=".")
    cat("Building for R Version",this.R,"\n")

    # Keep development packages separate from ve-lib (runtime packages)
    ve.dev <- file.path(getwd(),"dev") # Used in "configure" to set log file location  
    dev.lib <- file.path(ve.dev,"lib",this.R)
    if ( ! dir.exists(dev.lib) ) dir.create( dev.lib, recursive=TRUE, showWarnings=FALSE )
    .libPaths(c(dev.lib,.libPaths()))
  },envir="ve.env")

  # Get list of currently build steps
  built.prefix <- "ve.built."
  built.pattern <- gsub("\\.","\\\\.",built.prefix)
  getBuiltSteps <- function(steps="") {
    built.steps <- ls("ve.env",pattern=built.pattern)
    if ( nzchar(built.steps) ) {
      names(built.steps) <- gsub(built.pattern,"",built.steps)
      if ( nzchar(steps) ) built.steps <- built.steps[ names(built.steps) %in% steps ]
    }
    return(built.steps)
  }
  isBuilt <- function(step) nzchar(getBuiltSteps(step))

  # ========================= CLEAN STEPS =========================

  # Perform the clean operations
  clean.set <- unique(clean)
  clean.set <- ve.build.sequence[ names(ve.build.sequence) %in% clean.set ]
  if ( any( nzchar(clean.set) ) ) { # use 'any' to force logical if nzchar returns nothing
    cat("Cleaning steps:\n")
    print(clean.set)
    # Cleaning implies a reset
    rm(list=getBuiltSteps(clean.set),envir="ve.env") # Reset clean steps
    for ( m in clean.set ) {
      m.config <- ve.build.steps[[m]]
      if ( "Clean" %in% names(m.config) ) {
        for ( cln in m.config$Clean ) { # Allow for multiple scripts, but one or zero
          if ( nzchar(cln) ) {
            cat("Would run:",cln,"in ve.env\n")
            # Would run: clean.result <- try( sys.source(cln,envir="ve.env") )
          }
        }
      }
    }
  } else {
    cat("No steps to clean.")
  }

  # ========================= CONSTRUCT BUILD STEPS =========================

  # Set up build order
  build.set <- unique(steps)
  # Reset requires us to rebuild config, even if not explicitly requested
  if ( reset ) {
    if ( ! ve.config.step %in% build.set ) {
      build.set <- c(build.set,ve.config.step)
    } else {
      # reset forces config rebuild
      rm(getBuiltSet(ve.config.step),envir="ve.env")
    }
  }
  add.module <- function(build.set,module) {
    if ( dependencies ) {
      for ( m in module$Deps ) {
        # Add dependency to list if is not built and not already requested
        # Also add it if we've requested full reset
        if ( reset ||
             ( ! "m" %in% build.set && ! isBuilt(m) )
           ) {
          build.set <- add.module(build.set,m)
        }
      }
    }
    return(build.set)
  }
  for ( m in steps ) build.set <- add.module(build.set,m)

  build.set <- unique(build.set)
  to.build <- names(ve.build.sequence) %in% build.set
  build.set <- ve.build.sequence[ to.build ]
  cat("Build Set (whether built yet or not):\n")
  print(build.set)

  # ========================= PERFORM RESET =========================

  # clean set is automatically reset (above)
  if ( reset ) {
    cat("Resetting build status for Build Set.\n")
    rm(list=getBuiltSteps(build.set),envir="ve.env")
  } else {
    # Remove from build.set anything that remains "ve.built"
    build.set <- build.set [ ! ( build.set %in% getBuiltSteps(build.set) ) ]
    cat("Build Set (skipping built steps):\n")
    print(build.set)
  }

  # ========================= PERFORM BUILD STEPS =========================

  for ( step in build.set ) {
    # Get build step configuration
    s.config <- ve.build.steps[[step]]
    if ( ! file.exists(s.config$Path) ) {
      stop("Missing build script: ",s.config$Path)
      next
    }
    build.success <- FALSE
    try( {
      # Would run: sys.source(s.config$Path,envir="ve.env")
      cat("Would run:",s.config$Path,"in ve.env\n")
      build.success <- TRUE
      }
    )
    if ( ! build.success ) {
      cat("Build step failure in",step," - Stopping")
      break
    } else {
      assign(paste0(built.prefix,step), Sys.time(), envir="ve.env")
    }
  }
}

# # Expect to have VE_CONFIG set in the environment, or sought in standard' places:
# # "build/config/VE-config.yml" or "config/VE-config.yml"
# # VE_R_VERSION is determined by the R running this builder script.
# 
# # The output includes the branch:
# 
# # += /built
# #    += /<branch>
# #       +- /<r.version>
# #          +- models
# #          +- VEGUI
# #          ... [This IS the runtime]
# #          +- docs
# #          +- src
# #          +- ve.lib
# #          +- ve-pkgs (was: pkg-dependencies)
# #          +- disney
# pkg-repository (keep changes in ve-pkgs too)
# 
# # Rather than keep the build steps all separate, we should have an easy configuration based on
# # intended application.  What do you want to build?
# #    - Runtime: Run it locally for debug/development?
# #    - Documentation: (Re)build documentation PDFs? (make it bulletproof in case they don't have the wiki; don't clean)
# #    - Platform Installer: Build installer zip file for my platform?
# #    - Package Source Installer: Build pacakage source zip file? (also builds manifest)
# #    - "Pure source" installer (compile everything for any platform)?
# # All of those check artifact time stamps and report "no changes" if artifact is newer
# # Any of those can have a "reset" flag, which will ignore the existing artifact(s) and rebuild
# 
# # The configuration of a build target includes the list of scripts that will be run
# # Each script has a list of sources and artifacts that need to be checked (VE-config/VE-components
# # will identify those) to decide if it is "up to date" - that list is ignored if we reset, and
# # we delete all the output from that step.  Each step is associated (currently, implicitly) with
# # a specific output location (possibly just certain files there). We can make that association
# # explicit.
# 
# # In the build-repository step, don't pull down the sources unless that's what our platform needs
# # (then don't do binaries for it), or unless we're making a "pure source" installer. Otherwise we
# # pull binary dependencies (and build just that for the externals).
# 
# # In addition to the structural configuration, each script needs information on the build target
# # status so it can adjust what it does. So launching the build probably should ALWAYS rebuild the
# # config and report on out-of-date targets.
# 
