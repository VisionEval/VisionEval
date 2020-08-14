#!/bin/env Rscript

# Author: Jeremy Raw

# Assemble .md documentation files into ve.docs and compile top level to PDF in docs folder

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

if ( ! suppressWarnings(require("rmarkdown",quietly=TRUE)) ) {
  install.packages("rmarkdown", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}

message("========== BUILD DOCS ==========")

# See this Rstudio forum thread on getting set up for rendering to PDF:
# https://github.com/rstudio/rmarkdown/issues/1285#issuecomment-374340175
# Despite their "allergy" to MikTeX, I find it a far more functional and
# easy to manage system than tinytex. Either should work, though you may
# still need to add rsvg-convert to your Pandoc installation (see the
# instructions for building VisionEval).

# Consider this page for information on controlling the render environment
# for .Rmd files: https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html

# We'll look for Markdown (.md), and R Markdown (.Rmd) files (case insensitive)
# In principle, could handle anything we can throw at Pandoc.

doc.file.pattern <- "\\.[Rr]*[Mm]d$"  # Include .Rmd files
# doc.file.pattern <- "\\.[Mm]d$"    # Only .md files

.libPaths( c(ve.lib,.libPaths()) ) # Add visioneval to process R inside .Rmd rendering

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

# The framework has its docs (build in a separate install step) in visioneval/framework_docs

# Documents for models are found in ve.root/models/<model_name>
# "tools" are just a special kind of model (model_name == "tools")

# All the documents are .md files and we'll use rmarkdown or knitr to render them
# into PDF format (also HTML?)

doc.type.names <- c("framework","module","model","script","docs")

# Folders in which to seek documentation
def.inputs <- c(framework="inst/framework_docs",module="inst/module_docs",model="Tutorial",script=".",docs="")

# Patterns to match for documentation file to include
def.patterns <- rep(doc.file.pattern,length(doc.type.names))
names(def.patterns) <- doc.type.names
def.patterns["module"] <- "\\.*"   # any file in inst/module_docs
def.patterns["model"] <- "\\.*"    # any file of any type in the Tutorial directory

# Whether to hunt recursively or confine search to root documentation folder for the type
def.recursive <- rep(TRUE,length(doc.type.names))
names(def.recursive) <- doc.type.names
# def.recursive["model"] <- FALSE

# Specs for documentation to seek
ve.getdocs <-                  pkgs.db[pkgs.docs,]
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.framework,])
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.module,])
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.model,])
ve.getdocs <- rbind(ve.getdocs,pkgs.db[pkgs.script,])

# those are suffixes.
#   Append to the indicated folder (ve.src/modulename, ve.root
#   model name, or tools folder)

for ( i in 1:nrow(ve.getdocs) ) {
  docs <- ve.getdocs[i,]
  type <- docs$Type
  root <- docs$Root
  input <- def.inputs[type]

  if ( type=='module' || type=='framework' ) {
     # Use intermediate build for module and framework
    root <- ve.src
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
    if ( !file.exists(doc.dir) || length(grep(def.patterns['docs'],doc.dir))==0 ) {
      stop("Could not find .md file on 'docs' Path (",doc.dir,") for ",docs$Package)
    } else {
      doc.files = doc.dir
    }
  } else { 
    for (d in doc.dir) {
      these.files <- dir(d,pattern=def.patterns[type],full.names=TRUE, recursive=def.recursive[type])
      if ( length(these.files)>0 ) {
        doc.files <- c(doc.files,these.files)
      }
    }
  }
  if ( length(doc.files)>0 ) { # Don't do anything if there are no docs of this type
    cat("=== Copying docs for '",docs$Package,"' (as '",type,"')...",sep="")
    up.to.date <- TRUE
    if ( ! dir.exists(out.dir) ) dir.create(out.dir,recursive=TRUE)
    for ( f in doc.files ) {
      expected.of <- file.path(out.dir,basename(f))
      if ( newerThan(f,expected.of) ) {
        up.to.date <- FALSE
        file.copy(f, expected.of, overwrite=TRUE)
        cat("Copied",f,"to",sub(ve.docs,basename(ve.docs),expected.of),"\n")
      }
    }
    if ( up.to.date ) {
      cat("Up to date.\n")
    } else cat("\n")
  }
}

# As of VisionEval 2.0, only render to PDFs and HTML the top level .md files in the docs folder
# Only pdf's from that folder will be copied into the installer.
pandoc_formats <- list(
  "html"=rmarkdown::html_document(pandoc_args="--metadata=title:VisionEval Getting Started"),
  "pdf"=rmarkdown::pdf_document()
)
    
for ( f in dir(pattern=doc.file.pattern,ve.docs,full.names=TRUE) ) {
  for ( ext in c("html","pdf") ) {
    expected.of <- file.path(ve.docs,sub(doc.file.pattern,paste0(".",ext),basename(f)))
    if ( ! file.exists(expected.of) || newerThan(f,expected.of) ) {
      cat("Rendering",sub(root,"",f),"...")
      of <- rmarkdown::render(
        f
        , output_dir=ve.docs
        , output_format=pandoc_formats[[ext]]
        , quiet=TRUE
        , param=list(eval=FALSE)
        # , param=value # do it like this and you can drop options in and out with a single #
      )
      cat("\nDone as",sub(ve.docs,"",of))
      if ( of != expected.of ) {
        cat("\nDIFFERENT NAME")
        cat("\n",of)
        cat("\n",expected.of)
      } else {
        cat("\n")
      }
    } else {
      cat("Up to date:",sub(dirname(ve.docs),"",expected.of),"\n")
    }
  }
}
