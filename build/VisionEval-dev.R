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
      ve.runtime = NA
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
      return(ve.runtime)
    }


    ve.run <- function() {
      ve.runtime <- get.ve.runtime()
      if (
        ! is.na(ve.runtime) &&
        dir.exists(ve.runtime) &&
        file.exists(file.path(ve.runtime,"VisionEval.R"))
      ) {
        message("setwd(ve.root) to return to Git root")
        setwd(ve.runtime)
        source("VisionEval.R")
      } else {
        message("Could not locate runtime. Run ve.build()")
      }
    }
    message("ve.build() to (re)build VisionEval\n\t(see build/Building.md for advanced configuration)")
    message("ve.run() to run VisionEval (once it is built)")
  }
)
