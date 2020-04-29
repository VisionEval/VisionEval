# Author: Jeremy Raw

# VisionEval helper tools
# NOTE: these all depend on having set up the VisionEval environment
# See VisionEval.R in the installation runtime (VE-Installer/boilerplate)
# In particular, functions call models at locations relative to ve.root

tool.contents <- c("vegui","verpat","verspm","vestate")

# Function starts the VEGUI
vegui <- function(root=ve.root) {
  library("shiny")
  full.path <- file.path(root,"VEGUI")
  owd <- setwd(full.path) 
  runApp('../VEGUI')
  setwd(owd)
}

# The following two functions run the command line model versions per the
# Getting Started document.  Optional "scenarios" argument, if TRUE, will
# run the scenarios version of the test models.
verpat <- function(scenarios=FALSE,baseyear=FALSE,root=ve.root) {
  if ( ! scenarios ) {
    if ( ! baseyear ) {
      full.path <- file.path(root,"models/VERPAT")
    } else {
      full.path <- file.path(root,"models/BaseYearVERPAT")
    }
  } else {
    full.path <- file.path(root,"models/VERPAT_Scenarios")
  }
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}

verspm <- function(scenarios=FALSE,root=ve.root) {
  if ( ! scenarios ) {
    full.path <- file.path(root,"models/VERSPM")
    test.dir <- file.path(full.path,"Test1") # Older structure for VERSPM
    if ( dir.exists(test.dir) ) full.path <- test.dir
  } else {
    full.path <- file.path(root,"models/VERSPM_Scenarios")
  }
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}

vestate <- function(root=ve.root) {
  full.path <- file.path(root,"models/VE-State")
  owd <- setwd(full.path)
  source("run_model.R")
  setwd(owd)
}
