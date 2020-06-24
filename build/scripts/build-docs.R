#!/bin/env Rscript

# Author: Jeremy Raw

# Assemble .md documentation files and compile to PDF in docs folder

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

if ( ! suppressWarnings(require("rmarkdown",quietly=TRUE)) ) {
  install.packages("rmarkdown", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}

message("========== BUILD DOCS ==========")

doc.file.pattern <- "\\.[Mm]d$"

# We'll find docs in the following types of components:
#    framework
#    module
#    model
#    script
# There's a default location for each type

# Apart from the modules (whose build creates the documents in ve.src), all the
# document source .md files are found in ve.root locations suitable for the type

# Documentation for modules is constructed in build-modules.R using a
# framework function, and placed in src/<module>/inst/module_docs for the module.

# Documents for framework are prebuilt and found in ve.root/api

# Documents for models are found in ve.root/models/<model_name>
# "tools" are just a special kind of model (model_name == "tools")

# All the documents are .md files and we'll use rmarkdown or knitr to render them
# into PDF format (also HTML?)

def.inputs <- c(framework="inst/framework_docs",module="inst/module_docs",model=".",script=".",docs="")

ve.getdocs <-                  pkgs.db[pkgs.docs,]
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.framework,])
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.module,])
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.model,])
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.script,])

multi.dir.re <- "\\|\\|" # regular expression to find delimiter for multiple doc locations

# those are suffixes.
#   Append to the indicated folder (ve.src/modulename, ve.root
#   model name, or tools folder)

for ( i in 1:nrow(ve.getdocs) ) {
  docs <- ve.getdocs[i,]
  type <- docs$Type
  root <- docs$Root
  input <- def.inputs[type]

  if ( type=='module' || type=='framework' ) {
    root <- ve.src # Use intermediate build
  }

  ############ Input Directories
  # Set doc.dir (where to search for .md files for this set of docs
  # May end up as a vector of input locations (e.g. for framework)

  # Override input if docs$Docs is not an empty string
  if ( "Docs" %in% names(docs) && !is.na(docs$Docs) && nchar(docs$Docs)>0 ) {
    input <- docs$Docs
  }

  # Construct doc.dir vector of locations based on the component type
  doc.dir = character(0)
  # Where to locate docs for other component types (module, model, script)
  # Need to use root + docs$Path + docs$Package + input for model, script and docs
  # Need to use just root + docs$Package + input for module
  if ( type=='module' || type=='framework' ) {
    doc.dir = file.path(root,docs$Package,input)
  } else if ( type=='docs' ) { 
    doc.dir = file.path(root,docs$Path,docs$Package)
  } else {
    doc.dir = file.path(root,docs$Path,docs$Package,input)
  }

  ############ Output Directory
  # For the "docs" type, output use ve.docs + docs$Target (Target may be "", which resolves to ve.docs)
  # For modules, models and script use ve.docs + docs$Type + docs$Package
  #   Thus "docs/module/modulename...", "docs/model/modelname", "docs/script/scriptname [VEGUI,tools,VEScenarioViewer]
  # For framework use ve.docs + docs$Package, thus "docs/visioneval",

  if ( type=="docs" ) {
    out.dir <- file.path(ve.docs,docs$Target)
  } else if ( type %in% c("model","module") ) {
    out.dir <- file.path(ve.docs,docs$Type,docs$Package)
  } else if ( type %in% c("script","framework") ) {
    out.dir <- file.path(ve.docs,docs$Package)
  } else {
    stop("Don't know how to make output directory for ",type)
  }

  doc.dir <- normalizePath(doc.dir,winslash="/",mustWork=FALSE)
  out.dir <- normalizePath(out.dir,winslash="/",mustWork=FALSE)

  doc.files <- character(0)
  if ( type=='docs' && ! dir.exists(doc.dir) ) {
    # if doc.dir is a specific file (presumed .md), skip directory processing
    if ( !file.exists(doc.dir) || length(grep(doc.file.pattern,doc.dir))==0 ) {
      stop("Could not find .md file on 'docs' Path (",doc.dir,") for ",docs$Package)
    } else {
      doc.files = doc.dir
    }
  } else { 
    for (d in doc.dir) {
      these.files <- dir(d,pattern=doc.file.pattern,full.names=TRUE)
      if ( length(these.files)>0 ) {
        doc.files <- c(doc.files,these.files)
      }
    }
  }
  if ( length(doc.files)>0 ) {
    cat("=== Rendering docs for '",docs$Package,"' (as '",type,"')...",sep="")
    up.to.date <- TRUE
    for ( f in doc.files ) {
      # Note that rmarkdown will create output_dir and its components if they
      # do not already exist, so we don't need to
      expected.of <- file.path(out.dir,sub(doc.file.pattern,".pdf",basename(f)))
      if ( newerThan(f,expected.of) ) {
        up.to.date <- FALSE
        cat("\nRendering",sub(root,"",f),"...")
        of <- rmarkdown::render(
          f
          , output_dir=out.dir
          , output_format="pdf_document"
          , quiet=TRUE
          # , param=value # do it like this and you can drop options in and out with a single #
        )
        cat("\nDone as",sub(file.path(ve.docs),"",of))
        if ( of != expected.of ) {
          cat("\nDIFFERENT NAME")
          cat("\n",of)
          cat("\n",expected.of)
        }
      }
    }
    if ( up.to.date ) {
      cat("Up to date.\n")
    } else cat("\n")
  }
}
