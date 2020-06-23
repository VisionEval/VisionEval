# You can override VE_CONFIG, VE_LOGS, VE_RUNTESTS and VE_R_VERSION on the command line
# Or export them from your environment
VE_CONFIG?=config/VE-config.yml
VE_RUNTESTS?=Default
ifeq ($(OS),Windows_NT)
  VE_R_VERSION?=4.0.1
  RSCRIPT:="$(shell scripts/find-R.bat $(VE_R_VERSION))"
  WINDOWS=TRUE
else
  # If you change the PATH to point at a different Rscript, you can
  # support multiple R versions on Linux, but it's done outside the
  # Makefile. If you're running in RStudio, just set a different R
  # version there using its Global Options.
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

# If the correct installation is in place, VE_BRANCH will be set
# automaticall when the VE_MAKEVARS file gets built
VE_BRANCH?=$(shell git branch --show-current)
VE_MAKEVARS?=ve-output-$(VE_BRANCH)-$(VE_R_VERSION).make
export VE_BRANCH VE_MAKEVARS

VE_BOILERPLATE:=$(wildcard boilerplate/boilerplate*.lst)

include $(VE_MAKEVARS)
# $(VE_MAKEVARS) gets rebuilt (see below) if it is out of date, using build-config.R
# Make then auto-restarts to read:
#   VE_OUTPUT, VE_CACHE, VE_LIB, VE_INSTALLER, VE_PLATFORM, VE_SRC
#   and others
export VE_RUNTIME_CONFIG

.PHONY: configure repository modules binary runtime installers docs all\
	clean lib-clean module-clean runtime-clean build-clean\
	dev-clean really-clean docs-clean installer-clean inventory-clean\
        inventory runtime-packages\
	docker-clean docker-output-clean docker

all: configure repository binary modules runtime # docs - need that for installer, but not for local runtime

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

# clean targets to remove built artifacts, triggering full rebuild

build-clean: # Resets the built status of all the targets (but doesn't touch outputs)
	[[ -n "$(VE_LOGS)" ]] && rm -rf $(VE_LOGS)/*
	rm -f $(VE_MAKEVARS)

module-clean: $(VE_MAKEVARS) # Reset all VE modules for complete rebuild
	[[ -n "$(VE_REPOS)" ]] && rm -rf $(VE_REPOS)/*
	rm -rf $(VE_LIB)/visioneval $(VE_LIB)/VE*
	rm -f $(VE_LOGS)/modules.built

lib-clean: $(VE_MAKEVARS) # Reset installed package library for complete rebuild
	[[ -n "$(VE_LIB)" ]] && rm -rf $(VE_LIB)/*
	rm -f $(VE_LOGS)/velib.built

runtime-clean: $(VE_MAKEVARS) # Reset all models and scripts for complete rebuild
	[[ -n "$(VE_RUNTIME)" ]] && rm -rf $(VE_RUNTIME)/* && rm -rf $(VE_RUNTIME)/.Rprofile $(VE_RUNTIME)/.Rprof.user
	[[ -n "$(VE_LOGS)" ]] && rm -f $(VE_LOGS)/runtime.built

src-clean: $(VE_MAKEVARS) # Reset the build source directory for the packages
	[[ -n "$(VE_SRC)" ]] && rm -rf $(VE_SRC)/*

docs-clean: $(VE_MAKEVARS) # Clear the docs
	[[ -n "$(VE_DOCS)" ]] && rm -rf $(VE_DOCS)/*
	[[ -n "$(VE_LOGS)" ]] && rm -f $(VE_LOGS)/docs.built

installer-clean: $(VE_MAKEVARS) # Reset the installers for rebuild
	# installers have the R version coded in their .zip name
	[[ -n "$(VE_ZIPOUT)" ]] && rm -f $(VE_ZIPOUT)/*.zip
	[[ -n "$(VE_LOGS)" ]] && rm -f $(VE_LOGS)/installer*.built

inventory-clean:
	rm -f $(VE_SRC)/VENameRegistry.json $(VE_SRC)/VEModelPackages.csv

repository-clean: clean # Reset the CRAN, BioConductor and Github dependency packages for fresh download
	[[ -n "$(VE_DEPS)" ]] && rm -rf $(VE_DEPS)/*

dev-clean: $(VE_MAKEVARS) # Reset the developer packages for VE-Installer itself
	[[ -n "$(VE_DEVLIB)" ]] && [[ -n "$(VE_R_VERSION)" ]] && rm -rf $(VE_DEVLIB)/*

clean: $(VE_MAKEVARS) build-clean # Reset everything except developer packages; won't clean docs
	[[ -n "$(VE_OUTPUT)" ]] && [[ -n "$(VE_BRANCH)" ]] && [[ -n "$(VE_R_VERSION)" ]] && rm -rf $(VE_OUTPUT)/$(VE_BRANCH)/$(VE_R_VERSION)

really-clean: clean depends-clean dev-clean # Scorched earth: reset all VE-Installer artifacts

# The remaining targets build the various VisionEval pieces

# Make sure configuration is up to date
configure: $(VE_MAKEVARS) $(VE_RUNTIME_CONFIG)
	@echo Configuration complete

# This rule reads the configuration about what to build. Use build-clean to reset
# Note: build-config.R identifies VE_CONFIG via the exported environment variable
$(VE_MAKEVARS) $(VE_RUNTIME_CONFIG): scripts/build-config.R $(VE_CONFIG) R-versions.yml
	[[ $(RSCRIPT) != "" ]] && $(RSCRIPT) scripts/build-config.R

# This rule and the following one rebuild the repository of dependencies for VE_R_VERSION
# Will skip if repository.built is up to date with VE_Config and the scripts themselves
repository: $(VE_LOGS)/repository.built

$(VE_LOGS)/repository.built: $(VE_RUNTIME_CONFIG) scripts/build-repository.R scripts/build-external.R
	$(RSCRIPT) scripts/build-repository.R
	$(RSCRIPT) scripts/build-external.R
	@touch $(VE_LOGS)/repository.built

# This rule and the following one rebuild the installed library of dependencies and VE packages
# Will skip if velib.built is up to date with the repository build and with the script itself
# Use build-clean to reset velib.built and lib-clean to force complete rebuild of library
binary: $(VE_LOGS)/velib.built

$(VE_LOGS)/velib.built: $(VE_LOGS)/repository.built scripts/build-velib.R
	$(RSCRIPT) scripts/build-velib.R
	@touch $(VE_LOGS)/velib.built

# This rule and the following one will check the VE modules and rebuild as needed
# We'll almost always "build" the modules but only out-of-date stuff gets built
# (File time stamps are checked in the R scripts)
modules: $(VE_LOGS)/modules.built

$(VE_LOGS)/modules.built: $(VE_LOGS)/repository.built $(VE_LOGS)/velib.built \
        scripts/build-modules.R
	$(RSCRIPT) scripts/build-modules.R
	@touch $(VE_LOGS)/modules.built

# Framework docs
framework-docs: $(VE_LOGS)/framework-docs.built

$(VE_LOGS)/framework-docs.built: $(VE_LOGS)/modules.built scripts/build-docs-framework.R
	$(RSCRIPT) scripts/build-docs-framework.R
	@touch $(VE_LOGS)/framework-docs.built

# Visual docs (HTML page with graph of function call structures)
visual-docs: $(VE_LOGS)/visual-docs.built

$(VE_LOGS)/visual-docs.built: $(VE_LOGS)/framework-docs.built scripts/build-docs-visual.R
	$(RSCRIPT) scripts/build-docs-visual.R
	@touch $(VE_LOGS)/visual-docs.built

# This rule and the following one will assemble the documentation
docs: $(VE_LOGS)/docs.built

$(VE_LOGS)/docs.built: $(VE_LOGS)/modules.built $(VE_LOGS)/framework-docs.built \
	$(VE_LOGS)/velib.built scripts/build-docs.R
	$(RSCRIPT) scripts/build-docs.R
	@touch $(VE_LOGS)/docs.built

# This rule and the following one will (re-)copy out of date scripts and models to the runtime
# We'll almost always "build" the runtime, but only out-of-date stuff gets built
# (File time stamps are checked in the R scripts)
runtime: $(VE_LOGS)/runtime.built

$(VE_LOGS)/runtime.built: scripts/build-runtime.R $(VE_INSTALLER)/boilerplate/* $(VE_RUNTIME_CONFIG)
	$(RSCRIPT) scripts/build-runtime.R
	@touch $(VE_LOGS)/runtime.built

# The next two rules will build the VisionEval Name Registry (listing
# all the modules and packages, and what their Inp and Set
# specifications are), and the Model Packages (listing which models
# use which modules from which packages). These are constructed
# programmatically from the VisionEval source tree.
# Results are place in VE_SRC

inventory: $(VE_SRC)/VENameRegistry.json $(VE_SRC)/VEModelPackages.csv #  $(VE_SRC)/module_status.csv

$(VE_SRC)/VENameRegistry.json $(VE_SRC)/VEModelPackages.csv: scripts/build-inventory.R $(VE_LOGS)/modules.built
	$(RSCRIPT) scripts/build-inventory.R

# The next rules build the installer .zip files
# 'bin' is the binary installer for the local architecture (e.g. Windows or MacOSX)
#     (also package source as a separate zip file)
# 'src' is install-from-source installer (source packages for everything, including dependencies)
installers: installer-bin installer-full

installer installer-bin: $(VE_LOGS)/installer-bin.built

$(VE_LOGS)/installer-bin.built: $(VE_RUNTIME_CONFIG) $(VE_LOGS)/docs.built $(VE_LOGS)/runtime.built  \
            scripts/build-runtime-packages-bin.R scripts/build-installer-base.R scripts/build-installer-bin.R
	$(RSCRIPT) scripts/build-runtime-packages-bin.R
	$(RSCRIPT) scripts/build-installer-base.R
	$(RSCRIPT) scripts/build-installer-bin.R
	@touch $(VE_LOGS)/installer-bin.built

installer-full: $(VE_LOGS)/installer-full.built

$(VE_LOGS)/installer-full.built: $(VE_RUNTIME_CONFIG) $(VE_LOGS)/installer-bin.built \
            scripts/build-runtime-packages-full.R scripts/build-installer-full.R
	$(RSCRIPT) scripts/build-runtime-packages-full.R
	$(RSCRIPT) scripts/build-installer-full.R
	@touch $(VE_LOGS)/installer-full.built
