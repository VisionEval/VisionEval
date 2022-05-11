#!/bin/env Rscript

# Author: Jeremy Raw

# Build a "book" by running bookdown::render_book() in the configured book directory
# (the book directory may live in a different repository ("root") -- see the standard
# config/VE-config.yml for pointing at the ve.docbook root to include the getting
# started file.

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

if ( ! suppressWarnings(require("bookdown",quietly=TRUE)) ) {
  install.packages("bookdown", lib=dev.lib, repos=CRAN.mirror, dependencies=NA, type=.Platform$pkgType )
}
# Following is required to use bs4_book() function in bookdown
if ( ! suppressWarnings(require("downlit",quietly=TRUE)) ) {
  install.packages("downlit", lib=dev.lib, repos=CRAN.mirror, dependencies=NA, type=.Platform$pkgType )
}

# Following are required for the user_guide files (as of 5/6/2022)

# TODO: for packages used internally by the render_book function, we should specify the packages in
# VE-Components.yml using the DevPkg list to load them into dev/lib
# Only hard-code things here that are required generically to run render_book versus
# the packages required to process the book's .Rmd files

if ( ! suppressWarnings(require("kableExtra",quietly=TRUE)) ) {
  install.packages("kableExtra", lib=dev.lib, repos=CRAN.mirror, dependencies=NA, type=.Platform$pkgType )
}
if ( ! suppressWarnings(require("openxlsx",quietly=TRUE)) ) {
  install.packages("openxlsx", lib=dev.lib, repos=CRAN.mirror, dependencies=NA, type=.Platform$pkgType )
}

message("========== BUILD VISIONEVAL BOOK ==========")

# Need the Sys.setenv hack to make sure that .libPaths() gets set correctly
# when bookdown starts a new session for each chapter.
.libPaths(c(dev.lib,ve.lib,.libPaths()))
Sys.setenv(R_LIBS_USER=paste(.libPaths(),collapse=";"))

ve.book.def <- pkgs.db[pkgs.book,] # Typically one folder locating the "user_guide"

# Probably moot if running via "make" at command line, but probably needed for ve.build()...
owd <- getwd()
on.exit(setwd(owd))

for ( i in 1:nrow(ve.book.def) ) {
  book <- ve.book.def[i,]
  book.dir <- normalizePath(file.path(book$Root,book$Path,book$Package),winslash="/",mustWork=TRUE)
  cat(".libPaths():\n")
  print(.libPaths())
  cat("Building ",book.dir,", which contains:\n",sep="")
  setwd(book.dir)
  print(dir())
  ve.bookdir <- file.path(
    ve.docs,
    if ( ! is.na(book$Target) && nzchar(book$Target) ) book$Target else book$Package
  )
  if ( ! dir.exists(ve.bookdir) ) dir.create(ve.bookdir,recursive=TRUE)
  cat("Rendering into",ve.bookdir,"\n")
  cat("Using a new R session for each chapter.\n")
  cat("Running bookdown::render_book()\n",sep="")
  bookdown::render_book(new_session=TRUE,output_dir=ve.bookdir)
  cat("Bookdown rendering complete.\n\n")
}

