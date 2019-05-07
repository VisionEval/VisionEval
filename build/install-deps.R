#!/bin/env Rscript

# Load and install R package dependencies

if ( ! suppressWarnings(require(yaml)) ) {
  cat("Installing YAML package\n")
  install.packages("yaml", repos="https://cloud.r-project.org", dependencies=NA)
}

cat("Installing to:",.libPaths()[1],"\n")

# Parse the Module and Source Listings for dependencies
# Include their dependencies:
#   CRAN
#   BioConductor
#   Github

ve.components <- yaml::yaml.load_file(Sys.getenv("VE_COMPONENTS",unset="build/VE-components.yml"))

# cat("Components:\n")
# print(ve.components)
# cat("\n")
pkgs.db <- data.frame(Type="Type",Package="Package",Source="Source")

items <- ve.components[["Components"]]
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
  install.packages(new.pkgs)
}
devtools::install_bioc(paste("3.6",unique(pkgs.db[pkgs.db$Type=="BioC","Package"]),sep="/"))
devtools::install_github(unique(pkgs.db[pkgs.db$Type=="Github","Package"]))
