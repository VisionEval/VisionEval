# Building and Installing VisionEval

`VE-Installer` is a build system that makes a VisionEval Githb source tree "installable", and that
facilitates development and testing of VisionEval.

<hr/>

# Critical Update June, 2020

## Winter is Coming!
### (for the old ways of installing and hacking VisionEval)

As of the June 2020 release of VisionEval, the only supported way to build a running instance of VisionEval from the
Github canonical source code is with VE-Installer. Other methods for turning the VisionEval source code tree into a
runtime environment are deprecated and will fail to work with the next release.

However, all is not lost! We have also introduced a new optional binary download of the "almost-built" VisionEval source
packages to accompany the VisionEval runtime installer. If you install those packages (creating a "src" directory within
your runtime) you have full access to the VisionEval package source code in a form that will allow you to inspect, step
through, debug, and rebuild the binary packages in your installation.

In practice, that means that even power users of VisionEval will not have to interact with Github. The only people who
will need Github are those who are working on or contributing to the VisionEval core release. It is possible to modify
VisionEval locally (including adding new module packages) just with the runtime distribution and the "almost-built"
sources. If you want to contribute those back to Github, you will need to clone the VisionEval or VisionEval-dev
repository and copy your changes over to the relevant places on Github. The next release of VisionEval will include
tools for synchronizing loal package source code and data changes with the repository.

For information on debugging and developing VisionEval using the "almost-built" sources, see the separate RStudio.md
file here in VE-Installer (that document should be subtitled "How to Work on VisionEval without Setting Up VE-Installer").

<hr/>

# Why use VE-Installer

This installer addresses two VisionEval use cases:

1. Enable developers to develop, test and run VisionEval from the Github source repository
1. Enable developers to prepare and test contributions to the Github source repository (source code, documentation)
1. Create the VisionEval installers for all supported architectures (Windows, Max OSX, Linux), including
    * Binary installation .zip file
    * Package Source .zip file
    * Full Source Installation (rebuild dependencies from source) .zip file 

## What Comes out of VE-Installer

The following back-end outputs are available:

* Self-contained VisionEval "**runtime**" installation that will run locally on your development machine
* (optional) Windows or Macintosh "**offline**" installer (or the equivalent for your development system architecture) - a `.zip` file. VE-Installer builds that installer for the architecture it is running on, either Windows or Macintosh
* (optional) Multi-Platform "**offline**" (or "source") installer that will build VisionEval from source packages - also delivered as a `.zip` file. (_Warning_: this file requires a full binary R development environment on the end-user's machine)
* [Under Construction] **Docker images** for any system running Docker. This will eventually be the preferred mode of delivery for other architectures besides Windows and Macintosh 

## Overview

To use VE-Installer, do these steps:

- Install the required development environment:
  - [RStudio][getRStudio]
  - [R, at least version 3.6.x][getR]\
    Windows: [RTools40][getRTools]\
    Macintosh: No extra dependencies\
    Linux: [R development package for your distribution][getRLinux]
  - [Pandoc][getPandoc] (if building documentation)
  - [MikTeX][getMikTeX] (if building documentation)
- Clone the repositories
  - Clone the [VE-Installer][VE-Installer]
  - Clone the [Vision Eval Repository][VE-dev]
  - Setup a configuration file (`VE-config*.yml`) to point to your clone of the VisionEval repository.
    THe default VE-config.yml may work for you
  - Start the development environment shell and change to your VE-Installer directory
  - Build whatever `make` targets you want, such as the runtime environment or the installers

[Git4W]:       https://gitforwindows.org "Git for Windows"
[getPandoc]:   https://pandoc.org/installing.html "Install Pandoc"
[getMikTeX]:   https://miktex.org/download
[getR]:        https://cran.r-project.org
[getRLinux]:   https://cran.r-project.org/bin/linux/
[getRstudio]:  https://www.rstudio.com/products/rstudio/download/ "Download RStudio"
[VE-dev]:      https://github.com/visioneval/VisionEval-dev "VisionEval Development"
[VEInstaller]: https://github.com/visioneval/VE-Installer "VisionEval Installer"

Full instructions and reference materials are included below and in various referenced files.

## Telling VE-Installer What to Build

You can use VE-Installer with any version of VisionEval (with appropriate changes to configuration files describing that
version). For older versions, you will need to construct a suitable `VE-components.yml` file. Newer versions already
have a suitable `VE-components.yml`. The process for setting up the configuration is described in detail in the `build`
directory of any code branch in the [VisionEval-dev repository][VE-dev], and in the `config/ReadMe.md` here in
VE-Installer.

VE-Installer will work "out of the box" (with the standard `config/VE-config.yml`) if you clone the current
VisionEval-dev development branch so it is in a "sibling" directory of VE-Installer (i.e. they both have the same parent
directory).

The configuration file which describes what to build is documented in its own ReadMe.md file in the `config`
subdirectory (along with a primer on the required `VE-components.yml` file, which typically exists in the `build`
directory of the VisionEval source tree).

# Getting Started

To use the VE-Installer, install a development environment, clone VE-Installer, setup the configuration file to point at
the VisionEval source tree you would like to build from, and run suitable `make` targets to build (for example) a
runtime environment and an installer.

* Install [R][getR] and [RStudio][getRStudio]
  * If you have already installed the binary relase of VisionEval from the .zip files and have that running, you're good
  to go.
  * Just make sure that the version of R you use for RStudio and for your build is the same
  * The version matters more on Windows than Mac or Linux, since Windows lets you have lots of R versions installed side-by-side.
* Install [RTools40][getRTools]
    * As of the June 2020 release of VisionEval and VE-Installer, RTools40 is required and RTools 3.5 is no longer
    supported on Windows
    * There are a couple of configuration steps (and shortcuts) for RTools40 to get VE-Installer to work nicely; those
    are covered below.
* Install [Git for Windows][Git4W]
    * Git is actually optional; VE-Installer works fine if you just copy down a snapshot of the Github repository.
    * Having Git is necessary for contributing back to the VisionEval repositories
    * Using Git to track your own changes and updates is recommended
    * Manipulating the git repositories (other than checking them out) is beyond the scope of this document.
* Install [pandoc][getPandoc]
    * Pandoc will be installed automatically if you install Rstudio (you may need to adjust your PATH so
    VE-Installer can find it when you run `make`
    * Pandoc is used by `rmarkdown` and `knitr` to build R package documentation and vignettes
    * You can install pandoc without administrative permissions
    * No configuration beyond installation is required
* Install [MikTex][MixTex] *(Optional)*
    * You only need MikTex if you plan to render documents as PDF files
    * RStudio installs TinyTeX, but in our experience MikTex works better
    * No configuration beyond installation is required (though you may find yourself waiting for package downloads the first times you use it).
* Clone [VE-Installer][VEInstaller] and [VisionEval-dev][VE-dev]
    * You should check out either the "master" or the "development" branch of VisionEval-dev
      * "master" is always the same as the released VisionEval repository master and is the version used to create the `.zip` installers available from the [Download Page](https://visioneval.org/category/download.html)
      * "development" contains the current accepted changes for the next release
      * There is mostly very little difference in VE-Installer between "development" and "master"
      * Use other branches in VisionEval-dev at your peril.
    * Configuration will be simplest if VE-Installer and VisionEval-dev are subdirectories of the same parent directory
      * "simplest" in this case means you might not even need to change VE-Config.yml (see below)
* Configure the `bash` shell
    * The minimal adjustment is to fix the RTools40/mingw64.ini (or mingw32.ini) fie so your Windows PATH is included.
      * Uncomment the line that says `MSYS2_PATH_TYPE=inherit`
      * That way you get access to Pandoc and MikTeX
    * There is a section on dusting up the `bash` profile later on in this document
* Edit the `VE-Installer/config/VE-config.yml` file so it points at your copy of VisionEval-dev
    * This step is optional if you have cloned the VisionEval-dev repository so it is in a sibling directory of
    VE-Installer.
    * Check the following
      * ve.root : full path in which to look for VisionEval itself (defaults to "../VisionEval.dev") (".." relative to home of VE-Installer)
      * ve.output : Path in which to create gigabytes of output files (in subdirectories named after the version of R
      you used to build them, e.g. 4.0.0). (Default is "../VE-Built" relative to home of VE-Installer)
      * The installer will make a subfolder based on the current full R version number and put its output in that subfolder
      * So for example, subdirectory "3.6.1" will hold the built files if you choose VE_R_VERSION=3.6.1
* Run the build from a MinGW64 Bash window
    * Open a bash window
    * Change to the VE-Installer root folder
        * `cd /path/to/VE-Installer`
    * Run this command from the Bash command prompt
        * `./mk`
    * If you want to build something other than the local runtime, use a different version of R than the default, or use a
    configuration file other than the default VE-config.yml, there are instructions later on.
    * Documentation is not built by default (so you don't have to set up Pandoc or MikTeX right away)
* Once the build is done, you can start VisionEval from `../VE-Built/4.0.0/runtime` just as you would from the standard
installer
    * Either click VisionEval.bat (on Windows) or VisionEval.Rproj (on Windows or on any other system)
      * On Linux, you can run VisionEval.sh to work on a command line
    * When you build an installer, you'll find the zip files in the root of the build (e.g. `../VE-Build/4.0.0`).

Additional make targets and fine-tuning your setup are discussed below.

# Development Environment Details

On Windows, you will need to install the [RTools][getRTools] suite, as well as [Git for
Windows][Git4W] so you can use `bash` scripts. On Linux, installing the R development
package and "development essentials" will typically do, though some of the R dependency
packages may require you to install additional Linux OS packages. Mac OSX appears to already
contain everything you need for R development.

## Pre-Requisite: A supported version of `R`

We recommend the [current release version of R][currentR] for your development platform.

[currentR]: https://cran.r-project.org "Download and Install R"

You do need to have access to the Internet in order to run VE-Installer (unlike VisionEval itself).

You will also need quite a few R packages, and the build scripts will install the suitable versions of those for your
version of R (though you must have access to the internet). Packages needed by VE-Installer are placed in a their own
library to avoid contaminating VisionEval's own dependencies. Look in the "dev-lib" subdirectory of VE-Installer after
you've run the build, in the subdirectory (e.g. 4.0.0) corresponding to your version of R.

## Pre-Requisite: Bash Shell Environment and GNU tools

The build process is driven by the GNU `make` program available in RTools40, on the Mac, and in Linux.

You do NOT need the R Mac build environment (which only matters if you are compiling C++ or Fortran code, none of which
you need to do for VisionEval on Windows or Mac). The Mac uses the 'zsh' command interpreter (not 'bash'); mostly that's
not a problem. Please report an issue on VE-Installer if there is a shell problem on your Mac - the scripts have been
set up so key differences between 'bash' and 'zsh' are accounted for.

On a Linux machine, GNU bash is the standard command line interpreter, and you just need the "build-essentials" Linux
package for your distribution.

## Pre-Requisite: RTools40 on Windows

Installing [RTools40][getRTools40] does not require administrator rights to install or use. But you will need to point
the installer at a writable directory (i.e. one that you as a user have write permission for, which means "not the
default proposed by RTools40").

[getRTools40]: https://cran.r-project.org/bin/windows/Rtools/ "Using RTools40 on Windows"

### Linux environment

A standard R installation with `r-base-dev` on Linux (whether from a package repository or directly from the R project)
will include all the development tools needed to install source packages.

However, some additional system-level dependencies exist (required libraries that some of the binary dependency packages
use). You can find these in the `apt` section of `.travis.yml` at the root of the VisionEval source tree (the Travis CI
online build at Github runs a source installation under Linux, although Travis does not currently use VE-Installer to
construct VisionEval).

You can look at the the `docker/Dockerfile` (experimental, currently broken as of June 2020) for system dependencies
needed (beyond those already included in the base Docker image, `rocker/r-ver`). A future version of VisionEval will
include the required non-standard Linux libraries as explicit dependencies.

## Getting VisionEval code to build

The simplest way to obtain a VisionEval source tree to install is to clone it from Github.
If you have Git for Windows (or just `git` on Linux), you can do this:

```
git clone --depth=1 -b master https://github.com/VisionEval/VisionEval-dev.git My-VisionEval
```

Using `depth=1` saves you copying gigabytes of binary files that were produced in earlier test runs and committed
unnecessarily to the repository. Using the `-b` option (replace `master` with your chosen branch such as `development`)
selects a specific branch, which may often be necessary if the default is some obsolete version languishing in `master`
or if the repository offers a documentation tree as the default branch (as does VisionEval-dev).

Naming the folder in which to put the clone (`My-VisionEval` in the example above) makes it simple to clone different
branches (or VisionEval repositories) to different places. But if you do rename the location, you will have to adjust
the `ve.root` setting in `config/VE-config.yml` accordingly.

Note that cloning with `--depth=1` only gets one branch, so you can't change branches within a repository clone created
that way. Look at `git clone` documentation on --depth for further information.

If you don't want to mess with Git, you can just retrieve a source zip file by (for example) [going
here](https://github.com/VisionEval/VisionEval-Dev/tree/master) and looking for the "Clone or download" button. Then
just download a .zip file.

## Building, Rebuilding and Updating dependencies

In this version of VE-Installer, dependencies are managed directly from the VisionEval github tree via the
`build/VE-components.yml` file. If you need to put `VE-components.yml` somewhere else (e.g. if you're building an older
version of VisionEval and had to make `VE-components.yml` yourself), just create a location called `ve.components` in
your `VE-Config.yml` (relative to one of the "roots"). See the `ReadMe.md` for `VE-config.yml` for further instructions.

## Running the build process

You can use `make` (with various command line options described below) to build VisionEval. A simple bash script (`mk`)
is also provided to run a "one line" build without having to address the options directly.

Learning to use make will give you much greater flexibility in selecting build targets, rebuilding things, or using
different R versions.

Here is an overview of how to use make (command lines to construct various elements of VisionEval). A detailed analysis
of the `Makefile` (make configuration script) can be found in `Makefile.md`.

* `make`
  Just do it!  Defaults to `make configure; make repository; make binary; make modules; make runtime`
  see below. You get a built VisionEval that you can run on your local machine.
* `make configure`
  This reads your VE-config.yml and turns it into something the other scripts can use.
  Nothing else can happen until this step completes successfully, so if you're having
  trouble, start here until your configuration is working.
* `make repository`
  Builds a local package repository with all the VisionEval dependencies (and their dependencies, and their
  dependencies, and so on all the way down). At the end of this step, you will have source and Windows binary packages for
  all the dependencies, plus built source packages (only) for any Github packages. Packages can come from CRAN,
  BioConductor, or Github.
* `make binary`
  Builds VisionEval dependency binaries and installs them for the machine architecture of your development environment.  If you want Windows binaries, run this on a Windows machine.  Likewise, you can use it to create a runtime for Linux or Macintosh if that's where you're developing.  After running this step, you'll have installed copies of all VisionEval's binary dependency packages.
* `make modules`
  Here's where the action is. This will build source and binary versions of the VisionEval framework and modules.  If you set the variable VE_RUNTESTS=TRUE, the builder will run the development tests for each module.  To set the variable, you can do one of these three things:
    * in the bash shell, before calling `make` or `build-VE.sh`, do `export VE_RUNTESTS=TRUE`
    * Add VE_RUNTESTS to the make command line: `make VE_RUNTESTS=TRUE ...`
* `make runtime`
  Copy non-package source files and test data to the runtime folder (basis for
  the installers, and also for a local runtime test environment for the developer)
* `make installer` or `make installer-bin`
  Packages up a binary installer as a .zip file, as well as the 'intermediate source' zip file
  that you can use to put the package sources on your machine in a form you can use with RStudio for
  debugging or further development. You can also make a source
  installer (twice the size) that would work on Linux using `make installer-src`,
  but that is a slow process that generates a gigantic file so you probably don't
  want to do it.
* `make docs`
  Regenerates the configured documentation into the 'docs' output directory as PDFs.
* `make module-list`
  Generates two files into the "src" folder (where packages are built) by (1) extracting the names of modules available in each package, listing
  their Inp (input) and Set (output) specifications, and (2) identifying which models use which packages from which
  modules.
* `make docker`
  This target builds a docker image; see the `docker` subdirectory and its `ReadMe.md` for details. The docker target is
  broken and won't run right now (June 2020)

### Sitting back and watching the build

Rather than just running `make` we recommend that you give yourself and your computer some
freedom.  If you run `./mk` to build your VisionEval, you'll see a line like this one at the end of its
output:

```bash
tail -f logs/VE-4.0.0-make.out
```

### Rebuilding after you change the VisionEval sources

The `make` process is set up to do as little repetitive work as possible. There are several `make` targets that will undo some of what you have built so as to check and rebuild things that would otherwise be untouched.  Here is a list of those targets that tell `make` to take a closer look at what is built.  It will still try to do as little as possible:

* `make build-clean`
  This target will remove all the log file output (in the `logs` subdirectory of
  VE-installer). It will also remove the parsed configuration file data.  Use this
  target if you change `VE-config.yml` or if any of the VisionEval package dependencies
  have been changed, or if you want to use a different value for VE_RUNTESTS. It is
  pretty harmless - everything will be checked and the rebuild will be quick when you do
  'make' again.
* `make module-clean`
  Clear all the built VisionEval modules so they will get completely rebuilt (and optionally
  tested).
* `make lib-clean`
  Clear the installed package library for the runtime.  Will force complete rebuild
  of the binary packages for the local environment.
* `make runtime-clean`
  Removes the model scripts, VEGUI and other source files from the runtime.  They will be
  copied again when the runtime is next built.  Use this if you have deleted or renamed
  files for the runtime.  New or modified files will always be copied when the runtime is
  built.
* `make test-clean`
  Removes test data, test results and the copies of the module sources used for testiong
* `make installer-clean`
  Removes any installer `.zip` files and supporting data in the package source repository
* `make docs-clean`
  Removes all the built documentation (PDFs) from the 'docs' directory
* `make depends-clean`
  Removes the downloaded package dependencies so the local repository can be rebuilt.
* `make dev-clean`
  Removes the packages that were downloaded into the development library. Those are
  packages used by the installer, but not required by VisionEval itself.
  They will be downloaded again as needed when you subsequently run `make`.
* `make clean`
  Remove the standard output files, but leave behind things like dependencies if
  they have been stored in an alternate output root location. See documentation of
  VE-Config.yml.  Same as running `build-clean` and `test-clean` and then removing
  all the other files in the configured output location.
* `make really-clean`
  This target combines `clean`, `depends-clean` and `dev-clean`

### Remember to build fresh to make an installer

If you are planning to distribute one of the .zip installers, you should build from scratch (run `make really-clean`,
then `make all`, then `make docs`, then `make module-list` and finally `make installer`). Otherwise you risk including
obsolete dependencies. That's probably harmless; it just makes the installer bigger than necessary.

### Using the Installer to develop VisionEval packages and models

`VE-Installer` is designed not to do a lot of time-consuming redundant work. It uses file time stamps to manage most of
that. So if you're developing a model and you change `Run_Model.R`, you can just run `make` and that file (only) will
get recopied to your runtime.

The same thing happens with modules. So if you're working (for example) on VEScenario (a module which, as of this
writing, needs work), you can just go and edit the package code, `DESCRIPTION` or whatever and when you next run `make`,
VE-Installer will recognize that files were changed and will (only) rebuild VEScenario.

The result is an easy workflow:

1. Edit something in your VisionEval git clone directory
2. Run `make build-clean` from a bash window to force an update status on all VisionEval files
3. Run `make` from a bash window
4. Edit any file that shows up with errors
5. Lather, rinse, repeat until you've got it to build without errors
    * Extra credit: run `make lib-clean; make VE_RUNTESTS=TRUE`
5. Run `VisionEval.bat` from your runtime output folder and try out the modules and models
6. Repeat from the top until it all does what you want without errors.

There's also an easy workflow just using the binary and source installation .zip files. See RStudio.md for details.  You
only need to do VE-Installer work if you are planning to contribute to VisionEval, or if you want to use a version of
VisionEval code that has not yet been released.

# Key outputs of the build process

Inside the `ve.output\4.0.0` folder, you will find the following:

Item | Contents
--------- | --------
docs | PDF versions of the documentation (if you built them with `make docs`
external | Location where Github dependency packages are cloned and built from
pkg-dependencies | CRAN and BioConductor package repository (source and Windows binary\*) for dependencies
pkg-repository | Repository of built VisionEval packages (source and Windows binary\*)
runtime | the VisionEval folder with the installed / installable elements (see below)
src | Copies of the VisionEval TestData and package folders used for building, running tests and saving test output
ve-lib | Library of installed packages for the local machine architecture
ve-pkg | Repository of packages (VisionEval plus dependencies) used in Macintosh or Linux installers
VE-Runtime-Rx.x.x_(Date).zip | Just the runtime folder (for the local architecture and R version x.x.x) zipped up
VE-Installer-Windows-Rx.x.x_(Date).zip | the offline Windows installer for end users using R version x.x.x
VE-PackageSources-Rx.x.x_(Date) | the packages sources for RStudio development
VE-Installer-Full-Rx.x.x_(Date)x.x.x.zip | the offline source installer for end users using R version x.x.x

Remember that logs from the `R CMD check` tests go into sub-directories of each module's copy in the output `tests`
folder, in case you need to find them. They are not included in the PackageSources .zip file.

Inside the VE-Installer hierarchy, you will find the following new (built) directories (with
sub-directories for each R version you have used to perform the build) after you have run any build:

Item | Contents
--------- | --------
dev-lib | Package library for dependencies used by VE-Installer itself (but not necessarily by VisionEval)
logs | Log files and tracking files constructed during the build.

The driver script, `./mk`, will put its log output in the top of the logs directory (with the config and R version in
the file name). Other logged artifacts will go in a sub-directory named after the R version and the config file.

`dev-lib` contains packages that VE-Installer needs to build VisionEval, but that are (mostly) not needed to run
VisionEval itself. It is there principally as a convenience, so you can do `make clean` and not have to download them
again when you do the next `make`.

# Running VisionEval locally

The "runtime" directory in your install output directory is a ready-to-run installation of VisionEval. Just change to
the runtime directory and run the `VisionEval.bat` (on Windows), or `VisionEval.Rproj` (on Windows or Mac), or
`VisionEval.sh` (on Linux) to start RStudio with VisionEval loaded. Do that every time you want to start VisionEval.
Make a shortcut if you prefer.

# Hacking VisionEval

If `VE-Installer` feels too hard and all you want to do is make some tweaks to existing packages (such as rebuilding
some of them using local data rather than what comes in the installer), it is possible to develop package updates simply
by using [the current VisionEvalInstaller][getVisionEval] and the associated PackageSources. See `RStudio.md` here in
VE-Installer for details.

# Navigating the `VisionEval-dev` Repository Organization

The [repository](https://github.com/visioneval/VisionEval-dev) is organized into two directories:
- The **sources** directory contains the following directories:
  - [visioneval framework](https://github.com/visioneval/VisionEval-dev/tree/master/sources/framework/visioneval) package
  - [VE modules](https://github.com/visioneval/VisionEval-dev/tree/master/sources/modules) such as VESimHouseholds and VESyntheticFirms
  - VE models such as [VERPAT](https://github.com/visioneval/VisionEval-dev/tree/master/sources/models/VERPAT), [VERSPM](https://github.com/visioneval/VisionEval-dev/tree/master/sources/models/VERSPM), and [VE-State](https://github.com/visioneval/VisionEval-dev/tree/master/sources/models/VE-State).
  - [VEGUI](https://github.com/visioneval/VisionEval-dev/tree/master/sources/VEGUI) graphical user interface for running and viewing tabular results
  - [VEScenarioViewer](https://github.com/visioneval/VisionEval-dev/tree/master/sources/VEScenarioViewer) interactive multiple-run scenario viewer / visualizer for viewing results
- The **api** directory contains documentation of the model system. The [model system design](https://github.com/visioneval/VisionEval-dev/blob/master/api/model_system_design.md) document is the most complete at the present time. VisionEval framework functions are documented in a [network visualization](https://gregorbj.github.io/VisionEval/website/visioneval_functions.html) of the functions and their call relationships.

## Develop Branch

The current release version of VisionEval is on the **master** branch.  The current development version is on
the **development** branch.  When working on develop (or a branch other than master), make sure to install the correct branch version of the packages and to use the branch example data.  To download, install, and test the develop branch resources, do the following:
  1. Git clone (i.e. copy) the develop branch to your computer. 
  ```
  git clone --branch development --depth 1 git@github.com:visioneval/VisionEval-dev.git 
  ```
  2. Run the same R commands above

A zipped version of the development branch is available [here](https://github.com/visioneval/VisionEval-dev/archive/development.zip).
