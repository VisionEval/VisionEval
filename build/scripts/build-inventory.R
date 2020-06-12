#!/bin/env Rscript

# Author: Jeremy Raw

# Build the package inventory and model usage tables (in ve.src)

# Load runtime configuration
source(file.path(getwd(),"scripts/get-runtime-config.R"))

if ( ! dir.exists(ve.src) ) {
  stop("Need to make modules before building inventory.\n")
}

cat("========================= BUILDING PACKAGE SPEC INVENTORY =========================\n")

# Reach for ve.lib first when seeking packages used by the ones we're building
.libPaths( c(ve.lib, .libPaths()) ) # push runtime library onto path stack

# Libraries from ve.lib:
require(visioneval)
require(jsonlite)

# Need to work in ve.src to support visioneval structure
setwd(ve.src)

# Find module packages and models

ve.packages <- pkgs.db[pkgs.module,]$Package
ve.models <- pkgs.db[pkgs.model,]$Package

# For each module package, find the list of data elements and extract those ending with
# "Specifications"

ve.modules <- data.frame(Item=character(0),Package=character(0))
for ( p in ve.packages ) {
  d <- data(package=p,lib.loc=ve.lib)
  items <- d$results[,"Item"]
  items <- items[grep("Specifications$",items)] # d$results, and thus "items", is an R matrix
  if ( length(items) < 1 ) {
    cat("No registry items for package '",p,"'\n")
    next
  }
  items <- unlist(strsplit(items,"Specifications"))
  ve.modules <- rbind(ve.modules,data.frame(MODULE=items,PACKAGE=p))
}

# writeVENameRegistry weirdly requires the registry file already to exist
NameRegistry_ls <- list(Inp=list(),Set=list())

cat("Registry calls:\n")
registry <- apply(ve.modules,1,function(x) {
    cat("Item=",x["MODULE"],"; Package=",x["PACKAGE"],"\n")
    visioneval::writeVENameRegistry(x["MODULE"],x["PACKAGE"],NameRegistryList=TRUE)
  }
)

for ( m in registry ) {
  NameRegistry_ls$Inp <- c(NameRegistry_ls$Inp,m$Inp)
  NameRegistry_ls$Set <- c(NameRegistry_ls$Set,m$Set)
}

NameRegistryFile <- file.path(ve.src,"VENameRegistry.json")
json <- toJSON(NameRegistry_ls,pretty=TRUE)
writeLines(json,NameRegistryFile)
json <- readLines(NameRegistryFile) # Can't use it directly since it won't make Inp or Set into data.frames otherwise
reg <- fromJSON(json)

model.path <- file.path(ve.runtime,"models")
model.mods <- list()
models <- lapply(ve.models,FUN=function(model) {
  script <- parseModelScript(file.path(model.path,model,"run_model.R"),TestMode=TRUE)
  df <- data.frame(MODULE=script$ModuleName,PACKAGE=script$PackageName,MODEL=TRUE)
  names(df)[3] <- model
  return(df)
})

for ( m in models ) {
  ve.modules <- merge(ve.modules,m,by=c("MODULE","PACKAGE"),all=TRUE)
  col <- names(ve.modules)[length(ve.modules)]
  ve.modules[[col]][which(is.na(ve.modules[[col]]))] <- FALSE
}

ModelApplicationFile <- file.path(ve.src,"VEModelPackages.csv")
write.csv(ve.modules[order(ve.modules$PACKAGE,ve.modules$MODULE),],row.names=FALSE,file=ModelApplicationFile)
