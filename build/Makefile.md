## Makefile ##

This file provides extensive explanation and commentary on the Makefile used to build
VisionEval with the VE-Installer.

The first part sets up necessary environment variables.  Here are the list of key
environment configuration variables, all of which have useful defaults. The
Makefile will do error checking to see if a suitable R version is installed. The
configuration variables VE_CONFIG, VE_RUNTESTS, VE_R_VERSION can either be
exported from your shell or system environment, or set on the make command
line (e.g. via `make VE_CONFIG=config/MyVE-Config.yml`).

<dl>
    <dt>VE_CONFIG</dt>
    <dd>Location of the configuration file for this run. Defaults to
       `config/VE-config.yml` which does not exist until you create it.
       You can override it by setting it as an environment variable, or
       specifying it on the make command line.</dd>
    <dt>VE_RUNTESTS</dt>
    <dd>Generally, just leave this as "Default".  If TRUE, tests will
       be run when the VE modules are built. If FALSE tests will be
       skipped.  The default is pulled from VE-Config.yml, but may be
       overridden by setting an environment variable, or by specifying it
       on the make command line.</dd>
    <dt>VE_R_VERSION</dt>
    <dd>The R Version for which to build VisionEval. On Linux, this
       variable is forced to match whatever R is installed.  On
       Windows, it uses a helper batch file to find (and optionally
       install) the requested version of R.  This must be one of the
       versions identifed in r-versions.yml in the root of the
       VE-Installer (though that file is easy to extend).</dd>
    <dt>VE_LOGS, VE_RUNTIME_CONFIG, VE_MAKEVARS</dt>
    <dd>These are files constructed during the build to keep track of
       information passed from one step to another.  VE_LOGS is used
       to record log and status information on the build.
       VE_RUNTIME_CONFIG is used to keep R variables describing what
       is being built, dependencies, build locations, etc.
       VE_MAKEVARS contains additional variables used in this make
       process that are generated from VE-Config.yml.</dd>
    <dt>VE_BOILERPLATE</dt>
    <dd>Contains the path to the boilerplate configuration file;
       used to keep from recopying the boilerplate if it is already up
       to date</dd>
</dl>

~~~
# You can override VE_CONFIG, VE_LOGS, VE_RUNTESTS and VE_R_VERSION on the command line
# Or export them from your environment
VE_CONFIG?=config/VE-config.yml
VE_RUNTESTS?=Default
ifeq ($(OS),Windows_NT)
  VE_R_VERSION?=3.6.3
  RSCRIPT:="$(shell scripts/find-R.bat $(VE_R_VERSION))"
  WINDOWS=TRUE
else
  # If you change the PATH to point at a different Rscript, you can
  # support multiple R versions on Linux, but it's done outside the Makefile
  RSCRIPT:="$(shell which Rscript)"
  override VE_R_VERSION:=$(shell $(RSCRIPT) --no-init-file scripts/find-R.R)
  WINDOWS=FALSE
endif
ifeq ($(RSCRIPT),"")
  $(error R Version $(VE_R_VERSION) not found)
else
  $(info VE_R_VERSION is $(VE_R_VERSION), using [$(RSCRIPT)])
endif

export VE_R_VERSION VE_CONFIG VE_RUNTESTS RSCRIPT

VE_BRANCH?=$(shell basename $(VE_CONFIG) | cut -d'.' -f 1 | cut -d'-' -f 2-3)
VE_LOGS?=logs/VE-$(VE_R_VERSION)-$(VE_BRANCH)
VE_RUNTIME_CONFIG:=$(VE_LOGS)/dependencies.RData
VE_MAKEVARS:=$(VE_LOGS)/ve-output.make
export VE_LOGS VE_RUNTIME_CONFIG VE_MAKEVARS

VE_BOILERPLATE:=$(wildcard boilerplate/boilerplate*.lst)
~~~

The "include" directive forces the file identified by VE_MAKEVARS to
be rebuilt if it is out of date compared to VE-Config.yml. The
makefile is then reloaded.  VE_MAKEVARS includes definitions of lots
of important locations that are set up in the configuration so the
makefile can inspect and build them suitably.

~~~
include $(VE_MAKEVARS)
# $(VE_MAKEVARS) gets rebuilt (see below) if it is out of date, using build-config.R
# Make then auto-restarts to read:
#   VE_OUTPUT, VE_CACHE, VE_LIB, VE_INSTALLER, VE_PLATFORM, VE_SRC
#   and others
~~~

.PHONY make targets are things you can ask to build that do not
directly generate a file.  They will always get "built", but make
won't bother to see if there's an up-to-date target.

~~~
.PHONY: configure repository modules binary runtime installers docs all\
	clean lib-clean module-clean runtime-clean build-clean test-clean\
	dev-clean really-clean docs-clean installer-clean\
        module-list runtime-packages\
	docker-clean docker-output-clean docker
~~~

all is a target that builds the basic runtime by building each of the
steps (or verifying that they are up to date).  See the subsequent
targets for configure, repository, binary, modules and runtime

~~~
all: configure repository binary modules runtime # docs - need that for installer, but not for local runtime
~~~

Use `make show-defaults` to dump some of make's key variables. Use to
debug environment variables and command line definitions.

~~~
show-defaults: $(VE_MAKEVARS)
	: Make defaults:
	: WINDOWS      $(WINDOWS)       # Running on Windows?
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

The following 'clean' targets will blow away various artifacts of
previous builds and force make to start again.

~~~
# Should have the "clean" target depend on $(VE_MAKEVARS) if it uses
# any of the folders like VE_OUTPUT that are read in from there.
build-clean: # Resets the built status of all the targets
	[[ -n "$(VE_LOGS)" ]] && rm -rf $(VE_LOGS)/*
	rm -f *.out

module-clean: $(VE_MAKEVARS) test-clean # Reset all VE modules for complete rebuild
	[[ -n "$(VE_REPOS)" ]] && rm -rf $(VE_REPOS)/*
	rm -rf $(VE_LIB)/visioneval $(VE_LIB)/VE*
	rm -f $(VE_LOGS)/modules.built

lib-clean: $(VE_MAKEVARS) # Reset installed package library for complete rebuild
	[[ -n "$(VE_LIB)" ]] && rm -rf $(VE_LIB)/*

runtime-clean: $(VE_MAKEVARS) # Reset all models and scripts for complete rebuild
	[[ -n "$(VE_RUNTIME)" ]] && rm -rf $(VE_RUNTIME)/*
	[[ -n "$(VE_LOGS)" ]] && rm -f $(VE_LOGS)/runtime.built

src-clean: $(VE_MAKEVARS) # Reset the build source directory for the packages
	[[ -n "$(VE_SRC)" ]] && rm -rf $(VE_SRC)/*

docs-clean: $(VE_MAKEVARS) # Clear the docs
	[[ -n "$(VE_DOCS)" ]] && rm -rf $(VE_DOCS)/*
	[[ -n "$(VE_LOGS)" ]] && rm -f $(VE_LOGS)/docs.built

installer-clean: $(VE_MAKEVARS) # Reset the installers for rebuild
	# installers have the R version coded in their .zip name
	[[ -n "$(VE_OUTPUT)" ]] && [[ -n "$(VE_R_VERSION)" ]] && rm -f $(VE_OUTPUT)/$(VE_R_VERSION)/*.zip
	[[ -n "$(VE_PKGS)" ]] && rm -rf $(VE_PKGS)/*
	[[ -n "$(VE_LOGS)" ]] && rm -f $(VE_LOGS)/installer*.built

depends-clean: clean # Reset the CRAN, BioConductor and Github dependency packages for fresh download
	[[ -n "$(VE_DEPS)" ]] && rm -rf $(VE_DEPS)/*

dev-clean: $(VE_MAKEVARS) # Reset the developer packages for VE-Installer itself
	[[ -n "$(VE_R_VERSION)" ]] && rm -rf dev-lib/$(VE_R_VERSION)

clean: $(VE_MAKEVARS) build-clean test-clean # Reset everything except developer packages
	[[ -n "$(VE_OUTPUT)" ]] && [[ -n "$(VE_R_VERSION)" ]] && rm -rf $(VE_OUTPUT)/$(VE_R_VERSION)

really-clean: clean depends-clean dev-clean # Scorched earth: reset all VE-Installer artifacts
~~~

Finally, we get down to the targets that do real work:

<dl>
   <dt>configure</dt><dd>Parses VE-config.yml into R and `make`
      variables and structures.</dd>
   <dt>repository</dt><dd>Downloads the dependencies from CRAN,
      BioConductor and Github into a single local repository</dd>
   <dt>binary</dt><dd>Installs dependencies into the local library,
      ve-lib </dd>
   <dt>modules</dt><dd>Builds source and binary packages from the VE
      modules and installs them into the local library, ve-lib</dd>
   <dt>docs</dt><dd>Builds PDF documentation from configured locations</dd>
   <dt>runtime</dt><dd>Copies non-package modules into the runtime -
      the startup scripts will locate ve-lib to complete the
      local installation.</dd>
   <dt>module-list</dt><dd>Analyzes the source tree and identifies all
      the modules in each package, and what models they are used in</dd>
   <dt>installer or installer-bin</dt><dd>Builds a binary installer
      for the development machine architecture (typically Windows)</dd>
   <dt>installer-src</dt><dd>Buils a source installer that will work
      on any architecture with R and a development environment -
      currently the only way to bundla an install for Mac or Linux.</dd>
   <dt>installers</dt><dd>Builds installer-bin and installer-src</dd>
</dl>

~~~
# The remaining targets build the various VisionEval pieces

# Make sure configuration is up to date
configure: $(VE_RUNTIME_CONFIG) $(VE_MAKEVARS)

# This rule reads the configuration about what to build. Use build-clean to reset
# Note: build-config.R identifies VE_CONFIG via the exported environment variable
$(VE_MAKEVARS) $(VE_RUNTIME_CONFIG): scripts/build-config.R $(VE_CONFIG) R-versions.yml
	mkdir -p $(VE_LOGS)
	mkdir -p dev-lib/$(VE_R_VERSION)
	[[ $(RSCRIPT) != "" ]] && $(RSCRIPT) scripts/build-config.R

# This rule and the following one rebuild the repository of dependencies for VE_R_VERSION
# Will skip if repository.built is up to date with VE_Config and the scripts themselves
repository: $(VE_LOGS)/repository.built

$(VE_LOGS)/repository.built: $(VE_RUNTIME_CONFIG) scripts/build-repository.R scripts/build-external.R
	$(RSCRIPT) scripts/build-repository.R
	$(RSCRIPT) scripts/build-external.R
	touch $(VE_LOGS)/repository.built

# This rule and the following one rebuild the installed library of dependencies and VE packages
# Will skip if velib.built is up to date with the repository build and with the script itself
# Use build-clean to reset velib.built and lib-clean to force complete rebuild of library
binary: $(VE_LOGS)/velib.built

$(VE_LOGS)/velib.built: $(VE_LOGS)/repository.built scripts/build-velib.R
	$(RSCRIPT) scripts/build-velib.R
	touch $(VE_LOGS)/velib.built

# This rule and the following one will check the VE modules and rebuild as needed
# We'll almost always "build" the modules but only out-of-date stuff gets built
# (File time stamps are checked in the R scripts)
modules: $(VE_LOGS)/modules.built

$(VE_LOGS)/modules.built: $(VE_LOGS)/repository.built $(VE_LOGS)/velib.built scripts/build-modules.R
	$(RSCRIPT) scripts/build-modules.R
	touch $(VE_LOGS)/modules.built

# This rule and the following one will assemble the documentation
docs: $(VE_LOGS)/docs.built

$(VE_LOGS)/docs.built: $(VE_LOGS)/modules.built $(VE_LOGS)/velib.built scripts/build-docs.R
	$(RSCRIPT) scripts/build-docs.R
	touch $(VE_LOGS)/docs.built

# This rule and the following one will (re-)copy out of date scripts and models to the runtime
# We'll almost always "build" the runtime, but only out-of-date stuff gets built
# (File time stamps are checked in the R scripts)
runtime: $(VE_LOGS)/runtime.built

$(VE_LOGS)/runtime.built: scripts/build-runtime.R $(VE_INSTALLER)/boilerplate/*
	$(RSCRIPT) scripts/build-runtime.R
	touch $(VE_LOGS)/runtime.built

# The next two rules will build the VisionEval Name Registry (listing
# all the modules and packages, and what their Inp and Set
# specifications are), and the Model Packages (listing which models
# use which modules from which packages). These are constructed
# programmatically from the VisionEval source tree.
# Results are place in VE_SRC

module-list: $(VE_SRC)/VENameRegistry.json $(VE_SRC)/VEModelPackages.csv #  $(VE_SRC)/module_status.csv

$(VE_SRC)/VENameRegistry.json $(VE_SRC)/VEModelPackages.csv: scripts/build-inventory.R $(VE_LOGS)/modules.built
	$(RSCRIPT) scripts/build-inventory.R

# The next rules build the installer .zip files
# 'bin' is the binary installer for the local architecture (e.g. Windows or MacOSX)
#     (also package source as a separate zip file)
# 'src' is install-from-source installer (source packages for everything, including dependencies)
installers: installer-bin installer-src

installer installer-bin: $(VE_LOGS)/installer-bin.built

$(VE_LOGS)/installer-bin.built: $(VE_RUNTIME_CONFIG) $(VE_LOGS)/runtime.built  \
            scripts/build-runtime-packages.R scripts/build-installers.sh
	$(RSCRIPT) scripts/build-runtime-packages.R
	bash scripts/build-installers.sh BINARY
	touch $(VE_LOGS)/installer-bin.built

installer-src: $(VE_LOGS)/installer-src.built

$(VE_LOGS)/installer-src.built: $(VE_RUNTIME_CONFIG) $(VE_LOGS)/installer-bin.built \
            scripts/build-runtime-packages.R scripts/build-installers.sh
	$(RSCRIPT) scripts/build-runtime-packages.R source
	bash scripts/build-installers.sh SOURCE
	touch $(VE_LOGS)/installer-src.built
~~~

Finally, there are some Docker targets that work to build images,
but that are still under development.  Those are not documented here.
