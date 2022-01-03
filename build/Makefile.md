## Makefile ##

This file provides explanation and commentary on the Makefile used to build
VisionEval with `ve.build()`

You'll be most interested in the Makefile Build Targets section below.

### Makefile Environment

The first part of the Makefile sets up necessary environment variables. Here is the list
of environment configuration variables, all of which have useful defaults. The Makefile
will do error checking to see if a suitable R version is installed. The configuration
variables VE_CONFIG, VE_EXPRESS and VE_RUNTESTS work in their default form, but you can
override them by doing something like

`Sys.setenv(VE_CONFIG="config/VE-config-release.yml")`.

You can also set them (or any other environment variable) permanently to non-standard
values in your operating system or your .Renviron file.

(Inside R Studio, VE_R_VERSION can also set up by something like
`ve.build(r.version='3.6.1')`). By default, R Studio builds VisionEval for whatever
version of R it starts up with. By setting VE_R_VERSION, you can build VisionEval for a
different version of R (as long as it is installed on your system). That works because the
build process starts independent R processes for each step, and VE_R_VERSION tells the
Makefile which R to use for those processes.

<dl>
    <dt>VE_CONFIG</dt>
    <dd>Location of the configuration file for this run. Defaults to
       `config/VE-config.yml` whose default version will build the branch of visioneval
       that it is checked out into (or a pseudo-branch called "visioneval" if you just
       copied rather than cloned the repository). You can override it by setting it as an
       environment variable or putting a flag into ve.build (it would say
       `ve.build(flags="VE_CONFIG=/path/to/my/VE-config.yml". See the "config" folder
       for documentation of the config format.
    </dd>
    <dt>VE_RUNTESTS</dt>
    <dd>Generally, just leave this as "Default".  If TRUE, tests will
       be run when the VE modules are built. If FALSE tests will be
       skipped.  The default is pulled from VE-Config.yml, but may be
       overridden by setting an environment variable, or by specifying it
       on the make command line. **Warning: Nobody has tried the tests recently
       because it is easier to just run test models, so this may not build
       successfully. Re-doing the tests is already in the issues list.**
    </dd>
    <dt>VE_EXPRESS</dt>
    <dd>If set to YES, skip the R CMD CHECK operations in build-modules, and do not
       rebuild data files if they already exist. If you are developing packages that
       change their `/data` directory, you should set VE_EXPRESS to anything other
       than `YES`.
    </dd>
    <dt>VE_BRANCH</dt>
    <dd>This defaults to "visioneval". If you're checking out different
       branches, you can set this to the currently checked out branch (or
       leave it blank and the Makefile will fill it in). If you're using
       ve.build() from within RStudio, there is a flag ("use.git") which you
       can set to true to use the Git branch; otherwise it will still use
       "visioneval".
    </dd>
    <dt>VE_R_VERSION</dt>
    <dd>The R Version for which to build VisionEval. On Linux or the Mac,
       this variable is forced to match whatever R is installed. On Windows, it uses a
       helper batch file to find (and optionally install) the requested version of R. This
       must be one of the versions identifed in r-versions.yml in the build directory
       (that file is easy to extend yourself if needed - just open it and make a similar
       entry with the numbers adjusted).
    </dd>
    <dt>VE_DELETE</dt>
    <dd>See the rm-module target below. If you set this environment variable to the name
       of a module, rm-module will remove every trace of that module (only) so it gets
       completely rebuilt the next time. Usually you won't need it; it crept into the
       Makefile while we were debugging a failure of package data to rebuild. Generally,
       it will be enough to set VE_EXPRESS=NO, or to use VE_MBUILD.
    </dd>
    <dt>VE_MBUILD</dt>
    <dd>This environment variable is used within the build-modules.R script and does not
       appear in the Makefile. You can set it to a comma-separated list (**no spaces**),
       and the named modules will be cleared (similar to using rm-module with VE_DELETE)
       and rebuilt from scratch. But modules that you do not name will **not** be rebuilt,
       even if they are out of date. Do something like this:
       ```
       make modules VE_MBUILD=VEHouseholdVehicles,VETravelPerformance
       ```
     </dd>
     <dt>VE_MCLEAR</dt>
     <dd>This environment variable is used in the build-modules.R script, and does
        not appear in the Makefile. Set it to "yes" or "no" (without the quotes). If set to
        "yes", it will remove the package source folder prior to building (unlike
        VE_DELETE, it leaves behind the built packages). Useful if you are just removing
        some folders from the package build source.
     </dd>
     <dt>VE_MCLEAR</dt>
     <dd>This environment variable is used in the build-modules.R script, and does
        not appear in the Makefile. Set it to "yes" or "no" (without the quotes). If set to
        "yes", it will remove the package source folder prior to building (unlike
        VE_DELETE, it leaves behind the built packages). Useful if you are just removing
        some folders from the package build source.
      </dd>
      <dt>VE_MDOCS</dt>
      <dd> Mutually exclusive options (strip everything from a comma on) that can be set
         to "all" (generate all documents), "init" (regenerate only NAMESPACE and
         Collation, not the function docs), or "docs" - (regenerate .Rd files using
         Roxygen).
      </dd>
      <dt>VE_MTEST</dt>
      <dd> Control which tests will run. Can have different values. If not set, only do R
         CMD check. Otherwise: "all" says do all tests (run, chk, pkg); "chk" says run R
         CMD check but with `--no-tests` (the default); "pkg" says do package tests (run R
         CMD check with tests enabled) (unit testing); "run" says do runtime tests after
         the package has been built (good for developers); or "none"- do no tests (usually
         it is simplest to set `VE_MTEST="none"` indirectly by setting `VE_EXPRESS=yes`)
      </dd>
</dl>

There are some variables in the Makefile that are set automatically, and you won't
want to change them. They are documented here so you won't worry.

<dl>
    <dt>VE_LOGS, VE_RUNTIME_CONFIG, VE_MAKEVARS</dt>
    <dd>These are file names constructed automatically during the build
       to keep track of information passed from one step to another.
       VE_LOGS is used to record log and status information on the build.
       VE_RUNTIME_CONFIG is used to keep R variables describing what
       is being built, dependencies, build locations, etc.
       VE_MAKEVARS names a file containing additional variables used in this make
       process that are generated from VE-Config.yml.</dd>
</dl>

~~~
# This is VisionEval Makefile Version 3.0 ("NextGen")
# You can override VE_CONFIG, VE_RUNTESTS, VE_EXPRESS, VE_BRANCH and VE_R_VERSION
# on the command line or export them from your environment
# ve.build() handles useful defaults
VE_CONFIG?=config/VE-config.yml
VE_VERSION?=3.0
VE_RUNTESTS?=Default
VE_EXPRESS?=YES # should be NO, or unset, for standard use
VE_BRANCH?=$(shell git branch --show-current 2>/dev/null || echo visioneval)
ifeq ($(OS),Windows_NT)
  VE_R_VERSION?=4.1.2
  RTERM:="$(shell scripts/find-R.bat $(VE_R_VERSION))"
  WINDOWS=TRUE
else
  # If you change the PATH to point at a different Rscript, you can
  # support multiple R versions on Linux, but it's done outside the
  # Makefile. If you're running in RStudio, just set a different R
  # version there using its Global Options.
  RTERM:="$(shell which R)"
  override VE_R_VERSION:=$(shell $(RTERM) --vanilla --slave --quiet -f scripts/find-R.R)
  WINDOWS=FALSE
endif
ifeq ($(RTERM),"")
  $(error R Version $(VE_R_VERSION) not found)
else
  $(info R Version     : R $(VE_R_VERSION) [using $(RTERM)])
  $(info Configuration : $(VE_CONFIG))
  $(info Git Branch    : $(VE_BRANCH))
endif
RSCRIPT:=$(RTERM) --vanilla --slave --quiet --no-init-file -f
# Must follow $(RSCRIPT) with the name of the file to run

# Must export to have available for runR
export VE_R_VERSION VE_CONFIG VE_RUNTESTS RSCRIPT

# If the correct installation is in place, VE_BRANCH will be set
# automaticall when the VE_MAKEVARS file gets built
VE_MAKEVARS?=ve-output-$(VE_BRANCH)-$(VE_R_VERSION).make
export VE_BRANCH VE_MAKEVARS
~~~

The "include" directive forces the file identified by VE_MAKEVARS to be rebuilt if it is
out of date compared to config/VE-Config.yml. The makefile is then reloaded. VE_MAKEVARS
includes definitions of lots of important locations that are set up in the configuration
so the makefile can inspect and build them suitably.

~~~
include $(VE_MAKEVARS)
# $(VE_MAKEVARS) gets rebuilt (see below) if it is out of date, using build-config.R
# Make then auto-restarts to read:
#   VE_OUTPUT, VE_CACHE, VE_LIB, VE_INSTALLER, VE_PLATFORM, VE_SRC
#   and others
export VE_RUNTIME_CONFIG
~~~

.PHONY make targets are things you can ask to build that do not directly generate a file.
They will always get "built", but make won't bother to see if there's an up-to-date target
(though if they depend on other things that _do_ build targets, those will only get built
if they are out of date).

~~~
.PHONY: list-targets help show-defaults\
	configure repository lib modules runtime docs\
	build all dev\
	installer installers installer-bin installer-full\
	configure-build repository-build lib-build modules-build runtime-build docs-build\
	clean build-build all-build dev-build clean-build\
	installer-build installer-bin-build installer-full-build\
	configure-clean repository-clean lib-clean modules-clean runtime-clean docs-clean\
	clean build-clean all-clean dev-clean clean-clean\
	installer-clean installer-bin-clean installer-full-clean\
	configure-reset repository-reset lib-reset modules-reset runtime-reset docs-reset\
	reset build-reset all-reset dev-reset clean-reset docs-reset\
	installer-reset installer-bin-reset installer-full-reset
~~~

all is a target that builds the basic runtime by building each of the steps (or verifying
that they are up to date). See the subsequent targets to understand what happens

~~~
all build dev: reset configure-build repository-build lib-build modules-build runtime-build
	@echo Build complete
~~~
Use `list-targets` to dump the summary of targets you can use. Use `make show-defaults` to dump some of make's key variables. Use to
debug environment variables and command line definitions.
~~~
list-targets help:
	@echo 'VisionEval Build System (using "make" or "ve.build()")' 
	@echo 'The following build targets are available'
	@echo
	@echo 'all            = build a standard runtime (a.k.a. "build" or "dev")'
	@echo '                   (can also use "build" or "dev", or not provide a target)'
	@echo '    \'all\' has the following sub-steps:
	@echo '     configure = parse the configuration file and set up output directories'
	@echo '    repository = download the required runtime package dependencies'
	@echo '           lib = create runtime package library and install the dependencies'
	@echo '       modules = build the VisionEval framework and module packages'
	@echo '       runtime = set up static files in the runtime directory'
	@echo 'docs           = build the documentation'
	@echo 'installer      = binary installer for the current machine architecture'
	@echo 'installer-full = installer using only source packages (including dependencies)'
	@echo 'clean          = remove ALL the build artifacts and re-do (almost) everything'
	@echo 'reset          = remove all the success flags from previous builds'
	@echo
	@echo '    suffix any target with "-reset" to ignore previous builds'
	@echo '    suffix any target with "-clean" to remove previous build artifacts'

show-defaults: $(VE_MAKEVARS)
	: Make defaults:
	: VE_MAKEVARS  $(VE_MAKEVARS)   # File containing runtime variables
	: VE_RUNTIME_CONFIG $(VE_RUNTIME_CONFIG) # File of R built configuration
	: WINDOWS      $(WINDOWS)       # Running on Windows?
	: VE_BRANCH    $(VE_BRANCH)     # Current branch
	: VE_PLATFORM  $(VE_PLATFORM)   # Then what platform ARE we running on?
	: VE_R_VERSION $(VE_R_VERSION)  # R Version for build
	: RSCRIPT      $(RSCRIPT)       # Rscript (should match R Version)
	: VE_LOGS      $(VE_LOGS)       # Location of log files
	: VE_CONFIG    $(VE_CONFIG)     # Location of VE-Config.yml
	: VE_MAKEVARS  $(VE_MAKEVARS)   # Make's version of VE-Config.yml
	: VE_OUTPUT    $(VE_OUTPUT)     # Root of build output
	: VE_DEPS      $(VE_DEPS)       # Location of dependency repository
	: VE_REPOS     $(VE_REPOS)      # Location of build VE packages (repo)
	: VE_LIB       $(VE_LIB)        # Location of installed packages
	: VE_RUNTIME   $(VE_RUNTIME)    # Location of local runtime
	: VE_SRC       $(VE_SRC)        # Location of source folder
	: VE_PKGS      $(VE_PKGS)       # Location of runtime packages (for installer/docker)
~~~

The following 'reset' targets will force the build process to re-evaluate the adequacy of
the targets. Usually you will want to do that before you update a build (and the default
targets will call them).

~~~
# reset targets to force a "visit" by the builder script for that element.
# The builder scripts do the traditional "make" work of checking for out-of-date/up-to-date targets.

reset: build-reset

configure-reset build-reset all-reset dev-reset clean-reset repository-reset:
	[[ -n "$(VE_LOGS)" ]] && rm -rf "$(VE_LOGS)"/*
	rm -f "$(VE_MAKEVARS)"

lib-reset:
	[[ -n "$(VE_LOGS)" ]] && rm -f "$(VE_LOGS)/velib.built"

modules-reset:
	[[ -n "$(VE_LOGS)" ]] && rm -f "$(VE_LOGS)"/modules.built

runtime-reset:
	[[ -n "$(VE_LOGS)" ]] && rm -f "$(VE_LOGS)"/runtime.built

docs-reset:
	[[ -n "$(VE_LOGS)" ]] && rm -f "$(VE_LOGS)"/docs.built

installer-reset installer-bin-reset installer-full-reset:
	[[ -n "$(VE_LOGS)" ]] && rm -f "$(VE_LOGS)"/installer*.built
~~~

The following 'clean' targets will blow away various artifacts of previous builds and
force make to start again. In general, the only one of these you'll need is `build-clean`.

~~~
# clean targets perform a reset and remove built artifacts, triggering full rebuild

configure-clean build-clean: build-reset # Resets the built status of all the targets (but doesn't touch outputs)

modules-clean: $(VE_MAKEVARS) modules-reset # Reset all VE modules for complete rebuild
	[[ -n "$(VE_REPOS)" ]] && rm -rf "$(VE_REPOS)"/*
	[[ -n "$(VE_SRC)" ]] && rm -rf "$(VE_SRC)"/VE*
	[[ -n "$(VE_SRC)" ]] && rm -rf "$(VE_SRC)"/visioneval
	[[ -n "$(VE_LIB)" ]] && rm -rf "$(VE_LIB)"/VE*
	[[ -n "$(VE_LIB)" ]] && rm -rf "$(VE_LIB)"/visioneval

lib-clean: $(VE_MAKEVARS) lib-reset # Reset installed package library for complete rebuild
	[[ -n "$(VE_LIB)" ]] && rm -rf "$(VE_LIB)"/*

runtime-clean: $(VE_MAKEVARS) runtime-reset # Reset all models and scripts for complete rebuild
	[[ -n "$(VE_RUNTIME)" ]] && rm -rf "$(VE_RUNTIME)"/* && rm -rf "$(VE_RUNTIME)"/.Rprofile "$(VE_RUNTIME)"/.Rprof.user

docs-clean: $(VE_MAKEVARS) docs-reset # Clear the docs
	[[ -n "$(VE_DOCS)" ]] && rm -rf "$(VE_DOCS)"/*

installer-clean: $(VE_MAKEVARS) installer-reset # remove the installers for rebuild (installers always imply "clean")
	# installers have the R version coded in their .zip name
	[[ -n "$(VE_ZIPOUT)" ]] && rm -f "$(VE_ZIPOUT)"/*.zip
	[[ -n "$(VE_LOGS)" ]] && rm -f "$(VE_LOGS)"/installer*.built

repository-clean: # Reset the CRAN, BioConductor and Github dependency packages for fresh download
	[[ -n "$(VE_DEPS)" ]] && rm -rf "$(VE_DEPS)"/*

dev-clean: $(VE_MAKEVARS) build-reset # Reset the developer packages for VE-Installer itself
	[[ -n "$(VE_DEVLIB)" ]] && [[ -n "$(VE_R_VERSION)" ]] && rm -rf "$(VE_DEVLIB)"/*

clean all-clean: $(VE_MAKEVARS) build-clean docs-clean # Reset everything except developer packages
	[[ -n "$(VE_OUTPUT)" ]] && [[ -n "$(VE_BRANCH)" ]] && [[ -n "$(VE_R_VERSION)" ]] && rm -rf "$(VE_OUTPUT)/$(VE_BRANCH)/$(VE_R_VERSION)"

clean-clean: all-clean dev-clean # Scorched earth: reset all installer artifacts

rm-module:
# By default,VE_DELETE is not set, and this target will just print a message
# Use 'VE_DELETE=VEHouseholdVehicles make rm-module' to delete all traces of VEHouseholdVehicles
	$(RSCRIPT) scripts/clean-module.R --args $(VE_DELETE)
~~~

Finally, we get down to the targets that do real work:

<dl>
   <dt>configure</dt><dd>Parses VE-config.yml into R and `make`
      variables and structures.</dd>
   <dt>repository</dt><dd>Downloads the dependencies from CRAN,
      BioConductor and Github into a single local repository</dd>
   <dt>lib</dt><dd>Installs dependencies into the local library,
      ve-lib </dd>
   <dt>modules</dt><dd>Builds source and binary packages from the VE
      frameowrk modules and installs them into the local library, ve-lib</dd>
   <dt>runtime</dt><dd>Copies non-package modules into the runtime -
      the startup scripts will locate ve-lib to complete the local installation.</dd>
   <dt>docs</dt><dd>Builds PDF documentation from configured locations</dd>
   <dt>installer</dt><dd>Builds a binary installer for the development
      machine architecture. On Windows, the installer includes installed versions of the
      packages (which just become an R library). On a Mac, the installer includes
      installable packages - we can't pre-install a library due to Mac security
      constraints. On Linux, the installer is identical to the "full" installer and
      includes source packages, which means the installation will be _slow_ as everything
      needs to get recompiled from the ground up. We may one day create installable
      packages for common Linux variants (`.deb`, `.rpm`, etc.) and different
      architectures, but we're not bothering yet since the market is still so small for
      Linux.</dd>
   <dt>installer-full</dt><dd>Builds a source installer that will work
      on any architecture with R and a development environment. This is only needed if the
      system does not have RStudio and a graphic environment.</dd>
   <dt>installers</dt><dd>Builds installer-bin and installer-full</dd>
</dl>

~~~
# The remaining targets build the various VisionEval pieces

# Make sure configuration is up to date
configure: reset configure-build

configure-build: $(VE_MAKEVARS) $(VE_RUNTIME_CONFIG)

# This rule reads the configuration about what to build. Use build-clean to reset
# Note: build-config.R identifies VE_CONFIG via the exported environment variable
$(VE_MAKEVARS) $(VE_RUNTIME_CONFIG): scripts/build-config.R $(VE_CONFIG) R-versions.yml
	: Build Environment:
	:     R Version = $(VE_R_VERSION)
	: Configuration = $(VE_CONFIG)
	:    Git branch = $(VE_BRANCH)
	$(RSCRIPT) scripts/build-config.R

# This rule and the following one rebuild the repository of dependencies for VE_R_VERSION
# Will skip if repository.built is up to date with VE_Config and the scripts themselves
repository: repository-reset repository-build

repository-build: $(VE_LOGS)/repository.built

$(VE_LOGS)/repository.built: $(VE_RUNTIME_CONFIG) scripts/build-repository.R scripts/build-external.R
	$(RSCRIPT) scripts/build-repository.R
	$(RSCRIPT) scripts/build-external.R
	@touch $(VE_LOGS)/repository.built

# This rule and the following one rebuild the installed library of dependencies and VE packages
# Will skip if velib.built is up to date with the repository build and with the script itself
# Use build-clean to reset velib.built and lib-clean to force complete rebuild of library
lib: lib-reset lib-build

lib-build: $(VE_LOGS)/velib.built

$(VE_LOGS)/velib.built: $(VE_LOGS)/repository.built scripts/build-velib.R
	$(RSCRIPT) scripts/build-velib.R
	@touch $(VE_LOGS)/velib.built

# This rule and the following one will check the VE modules and rebuild as needed
# We'll almost always "build" the modules but only out-of-date stuff gets built
# (File time stamps are checked in the R scripts)
modules: modules-reset modules-build

modules-build: $(VE_LOGS)/modules.built

$(VE_LOGS)/modules.built: $(VE_LOGS)/repository.built $(VE_LOGS)/velib.built scripts/build-modules.R
	$(RSCRIPT) scripts/build-modules.R
	@touch $(VE_LOGS)/modules.built

# This rule and the following one will (re-)copy out of date scripts and models to the runtime
# We'll almost always "build" the runtime, but only out-of-date stuff gets built
# (File time stamps are checked in the R scripts)
runtime: runtime-reset runtime-build

runtime-build: $(VE_LOGS)/runtime.built

$(VE_LOGS)/runtime.built: scripts/build-runtime.R $(VE_INSTALLER)/boilerplate/* $(VE_RUNTIME_CONFIG)
	$(RSCRIPT) scripts/build-runtime.R
	@touch $(VE_LOGS)/runtime.built

# This rule and the following one will assemble the documentation
docs: docs-reset docs-build

docs-build: $(VE_LOGS)/docs.built

$(VE_LOGS)/docs.built: $(VE_LOGS)/modules.built $(VE_LOGS)/runtime.built\
        $(VE_LOGS)/velib.built scripts/build-docs.R scripts/build-inventory.R
	$(RSCRIPT) scripts/build-docs-framework.R
	$(RSCRIPT) scripts/build-docs-visual.R
	$(RSCRIPT) scripts/build-inventory.R
	$(RSCRIPT) scripts/build-docs.R
	@touch $(VE_LOGS)/docs.built

# The next rules build the installer .zip files
# 'bin' is the binary installer for the local architecture (e.g. Windows or MacOSX)
#     (also package source as a separate zip file)
# 'src' is install-from-source installer (source packages for everything, including dependencies)
installers: installers-reset installers-build

installers-build: installer-bin installer-full

installer installer-bin: installer-reset installer-build

installer-build installer-bin-build: $(VE_LOGS)/installer-bin.built

$(VE_LOGS)/installer-bin.built: $(VE_RUNTIME_CONFIG) $(VE_LOGS)/runtime.built\
            scripts/build-runtime-packages-bin.R scripts/build-installer-base.R scripts/build-installer-bin.R
	$(RSCRIPT) scripts/build-runtime-packages-bin.R
	$(RSCRIPT) scripts/build-installer-base.R
	$(RSCRIPT) scripts/build-installer-bin.R
	@touch $(VE_LOGS)/installer-bin.built

installer-full: installer-full-reset installer-full-build

installer-full-build: $(VE_LOGS)/installer-full.built

$(VE_LOGS)/installer-full.built: $(VE_RUNTIME_CONFIG) $(VE_LOGS)/installer-bin.built \
            scripts/build-runtime-packages-full.R scripts/build-installer-full.R
	$(RSCRIPT) scripts/build-runtime-packages-full.R
	$(RSCRIPT) scripts/build-installer-full.R
	@touch $(VE_LOGS)/installer-full.built
~~~
