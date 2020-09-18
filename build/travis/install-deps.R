#!/bin/env Rscript

# Load and install R package dependencies
# These get put into ve-lib on Travis (since we don't have a separate
# dev-lib there). They also become part of the package cache.

if ( ! suppressWarnings(require("yaml")) ) {
  cat("Installing YAML package\n")
  install.packages("yaml", repos="https://cloud.r-project.org", dependencies=NA)
}
if ( ! suppressWarnings(require("roxygen2")) ) {
  cat("Installing roxygen2 package\n")
  install.packages("roxygen2", repos="https://cloud.r-project.org", dependencies=NA)
}

cat("Installing to:",.libPaths()[1],"\n")

# Parse the Module and Source Listings for dependencies
# Include their dependencies:
#   CRAN
#   BioConductor
#   Github

# Note: the VE-config-travis.yml is a special abbreviated version that won't work with ve.build
# It is only used to be able to exclude components that we don't want to test with Travis-CI

ve.components <- yaml::yaml.load_file(Sys.getenv("VE_COMPONENTS",unset="build/config/VE-components.yml"))
ve.config     <- yaml::yaml.load_file(Sys.getenv("VE_CONFIG",unset="build/config/VE-config-travis.yml"))
rversions     <- yaml::yaml.load_file(file.path("build/R-versions.yml"))
this.R        <- paste(R.version[c("major","minor")],collapse=".")
CRAN          <- rversions[[this.R]]$CRAN
BioC.version  <- basename(dirname(rversions[[this.R]]$BioC)) # looks like "https://www.bioconductor.org/packages/3.11/bioc"

exclude <- character(0)
if ( "Components" %in% ve.config ) {
  if ( "Exclude" %in% ve.config$Components ) {
    exclude <- ve.config$Components$Exclude
  }
} 

pkgs.db <- data.frame(Type="Type",Package="Package",Source="Source")

items <- ve.components[["Components"]]
items <- setdiff(items,exclude)
for ( pkg in names(items) ) {
  it <- items[[pkg]]
  if ( "CRAN" %in% names(it) ) {
    for ( dep in it$CRAN ) {
      dep.db <- data.frame(Type="CRAN",Package=dep,Source=pkg)
      pkgs.db <- rbind(pkgs.db,dep.db)
    }
  }
  if ( "BioC" %in% names(it) ) {
    for ( dep in it$BioC ) {
      dep.db <- data.frame(Type="BioC",Package=dep,Source=pkg)
      pkgs.db <- rbind(pkgs.db,dep.db)
    }
  }
  if ( "Github" %in% names(it) ) {
    for ( dep in it$Github ) {
      dep.db <- data.frame(Type="Github",Package=dep,Source=pkg)
      pkgs.db <- rbind(pkgs.db,dep.db)
    }
  }
}
for ( d in names(pkgs.db)) { pkgs.db[,d] <- as.character(pkgs.db[,d]) } # Otherwise we get factors, which can get weird
pkgs.db <- pkgs.db[-1,]

# cat("Packages DB:\n")
# print(pkgs.db)
# quit()

# Acknowledge the cache for the top-level packages
sought.pkgs <- unique(pkgs.db[pkgs.db$Type=="CRAN","Package"])
new.pkgs <- sought.pkgs[ ! (sought.pkgs %in% installed.packages()[,"Package"]) ]
if( length(new.pkgs) > 0 ) {
  cat("Installing new packages:\n")
  print(new.pkgs)
  install.packages(new.pkgs,repos="https://cloud.r-project.org")
}
BioC <- unique(pkgs.db[pkgs.db$Type=="BioC","Package"])
if ( length(BioC)>0 ) devtools::install_bioc(paste(BioC.version,BioC,sep="/"))
ghub <- unique(pkgs.db[pkgs.db$Type=="Github","Package"])
if ( length(ghub)>0 ) devtools::install_github(ghub)
