# Building and Installing VisionEval

This document describes how to launch VisionEval from a copy of the Github repository (either the development branch
from VisionEval/VisionEval-dev.git) or the master branch after the Summer 2020 release (from either
VisionEval/VisionEval.git or VisionEval/VisionEval-dev.git).

To develop VisionEval scenarios and apply the models, you will need a runtime version. You can construct that from
this repository if you like, but most people will find it easier to download the runtime installer from the VisionEval
website ([https://visioneval.org/category/download.html](https://visioneval.org/category/download.html)).

<hr/>

The build process is integrated into the VisionEval soure code tree as of the Summer 2020 release (previously, the
installer was a separate project, **VE-Installer**). **VE-Installer is now OBSOLETE.**

Using the build proces, you can build a runtime or create a redistributable installer (or rebuild any of the parts
of VisionEval, for example if you update a module).

## Prerequisites

To build VisionEval, you will need the following tools in addition to a copy or clone of the repository itself:

- [RStudio][getRStudio], preferably of recent vintage
- [R, at least version 3.6.x][getR], preferably the latest R version\
  Windows: [RTools40][getRTools40]\
  Macintosh: No extra dependencies\
  Linux: [R development package for your distribution][getRLinux]
- [Git for Windows][Git4W] or git for your own platform if not Windows
- Optionally, the [MikTeX][getMikTeX] program*

\* MikTeX is more flexible than tinytex, which comes with RStudio; both will work for the docs in VisionEval

[getR]:        https://cran.r-project.org
[getRstudio]:  https://www.rstudio.com/products/rstudio/download/ "Download RStudio"
[getRTools40]: https://cran.r-project.org/bin/windows/Rtools/ "Using RTools40 on Windows"
[Git4W]:       https://gitforwindows.org "Git for Windows"
[getMikTeX]:   https://miktex.org/download
[getRLinux]:   https://cran.r-project.org/bin/linux/
[VE-dev]:      https://github.com/visioneval/VisionEval-dev "VisionEval Development"

Full instructions and reference materials are included below and in various referenced files.

## Quick Start

Here is a sequence of steps that will get you started:

1. Install [Git for Windows][Git4W] or equivalent (optional: makes it easier to access the repositories)
1. Install R\
   (usually the most recent version works; must be 3.6.0 or later)
2. Install RTools40
3. Install RStudio\
   (a version compatible with RTools40, and with the version of R you're using)
4. Copy or clone the [VisionEval](https://github.com/VisionEval/VisionEval) (or [VisionEval-dev](https://github.com/VisionEval/VisionEval-dev) repository
5. Start RStudio and open the VisionEval-dev.Rproj project file\
   (On most graphic display machines, just double-click it)
6. Execute the ve.build() function:\
   (A full build from scratch takes from 45 minutes to an hour and a
   half on a typical Windows machine. A build on other architectures may
   take longer as many of the dependency packages will have to be built
   from source code.)\

   ```
   ve.build()
   ```\
7. Once the build is done, you can get into the runtime environment in several ways:
   1. By executing ve.run() from the VisionEval-dev RStudio project.
   2. By entering your file manager program, navigating to the runtime directory and
      double-clicking the VisionEval.Rproj RStudio project file down in the
      runtime directory (as you would for the standard binary release installer).
   3. Starting the R GUI (or R terminal) for the version of R that you used to build
      VisionEval, setting the working directory (`setwd`) to the runtime folder you
      just built, and then `source`-ing `Visioneval.R`
8. If the build is successful, you can make an installer .zip file by doing this:\
   ```
   ve.build("installer")
   ```

### Troubleshooting

- Administrator permissions are required to install RStudio, but not the rest of it.
- You must put RTools40
  into a directory you can write (i.e. NOT the default location it suggests unless you
  have especially strong administrative permissions). Putting it in %HOME%
  or your Documents folder are workable options.
- On Windows, make sure RTools40 is on your path, and in particular that RStudio can find
  the `make` program and the associated environment.  The `RTOOOLS40_HOME` variable
  should be set to the same directory you installed into (should happen by default), and you
  should make sure your `PATH` includes `%RTOOLS40_HOME%\usr\bin`.
- Make sure your R version is 3.6.0 or later. Earlier versions don't work.
- If a new R version has come out since you downloaded the repository, make
  sure you pull (or recopy) the new repository version so VisionEval will know
  about the new R. The VisionEval development and master branches are updated with a new
  r-versions.yml file in between releases (there should also be a new binary installer in
  the Downloads area for VisionEval/VisionEval.git).

## `ve.build()` Usage

`ve.build <- function(targets=character(0), r.version=paste(R.version[c("major","minor")],collapse="."),
config=character(0), flags=character(0), express=TRUE)`

`targets` - a character vector of build targets (see build/Makefile.md for a list and explanation); the
default will build the targets needed for a local runtime.

`r.version` - a character string (if a vector, only the first element will be used) in the format '4.0.1'
identifying the R major and minor version. The default is to use the version of R that is running in RStudio.

`use.git` - a logical flag (`TRUE` or (default) `FALSE`) that says whether to name the built branch after the
git branch. The default is to always use the "visioneval" branch. If you are checking out and building different
branches in the same worktree, or using the command-line `make` build process, you should set it to `TRUE`.

`config` - a character string (if a vecotr, only the first element will be used) with a relative path (to the
'build' directory) or absolute path to the alternate `VE-config.yml` file. See the 'build/config' directory for
instructions about how to build or modify that file. The default should be good for development purposes. The
release version (which includes documentation from the wiki) is build from "build/config/VE-config-release.yml"

`flags` - a character vector flags that can be passed to the `make` program. In general, the only one you might
consider using is "-n", which does a "dry run" (it will build the configuration file).

`express` - a boolean value (default is TRUE): the devtools::check step will be skipped, and package /data directories
will not be rebuilt if the needed files already exist.  **Warning** If you are developing a new module that has data
requirements (e.g. it estimates a model during the package build), you should run `ve.build(express=FALSE)`. It will
take longer to build, but you'll be sure to get updated data.

### `ve.build()` Additional Notes

You can build for any supported version of R on Windows, provided it is installed and can be located on your system.
Just give `ve.build` a different `r.version` parameter (e.g. `ve.build(r.version='3.6.3')`

r.version will not look for other versions of R on Linux or the Mac; on those systems you should set RStudio (via
its global options) to use an R installed some place other than the default R path.

If you want to reset the build products (e.g. if you are using the default 'visioneval' build branch and need to
build a different branch in the same place), you should clean your output:

```
ve.build(c("clean")
```

You can build an installer by doing this:\
```
ve.build("installer")
```\
If you ask for an installer and you have not already built the runtime, it will go ahead and do that for you anyway.

The complete set of build targets is documented in build/Makefile.md. Some of the "-clean" targets can be helpful for
doing a complete rebuild of certain targets (e.g. ve-lib, or the runtime models, or the documentation)
