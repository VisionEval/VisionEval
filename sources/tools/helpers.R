# Author: Jeremy Raw

# VisionEval helper tools
# NOTE: these all depend on having set up the VisionEval environment
# See VisionEval.R in the installation runtime (VE-Installer/boilerplate)
# In particular, functions call models at locations relative to ve.root

tool.contents <- c("vegui","verpat","verspm","vestate")

# Function starts the VEGUI
vegui <- function(ve.root=ve.runtime) {
  library("shiny")
  full.path <- file.path(ve.root,"VEGUI")
  owd <- setwd(full.path) 
  runApp('../VEGUI')
  setwd(owd)
}

# The following two functions run the command line model versions per the
# Getting Started document.  Optional "scenarios" argument, if TRUE, will
# run the scenarios version of the test models.
verpat <- function(scenarios=FALSE,baseyear=FALSE,ve.root=ve.runtime) {
  if ( ! scenarios ) {
    if ( ! baseyear ) {
      full.path <- file.path(ve.root,"models/VERPAT")
    } else {
      full.path <- file.path(ve.root,"models/BaseYearVERPAT")
    }
  } else {
    full.path <- file.path(ve.root,"models/VERPAT_Scenarios")
  }
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}

verspm <- function(scenarios=FALSE,ve.root=ve.runtime) {
  if ( ! scenarios ) {
    full.path <- file.path(ve.root,"models/VERSPM")
    test.dir <- file.path(full.path,"Test1") # Older structure for VERSPM
    if ( dir.exists(test.dir) ) full.path <- test.dir
  } else {
    full.path <- file.path(ve.root,"models/VERSPM_Scenarios")
  }
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}

verspm_mm <- function(ve.root=ve.runtime) {
  full.path <- file.path(ve.root,"models/VERSPM_MM")
  test.dir <- file.path(full.path,"Test1") # Older structure for VERSPM
  if ( dir.exists(test.dir) ) full.path <- test.dir
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}

vestate <- function(ve.root=ve.runtime) {
  full.path <- file.path(ve.root,"models/VE-State")
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}
