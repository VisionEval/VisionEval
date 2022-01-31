# This is VisionEval Makefile Version 3.0 ("NextGen")
# You can override VE_CONFIG, VE_RUNTESTS, VE_EXPRESS, VE_BRANCH and VE_R_VERSION
# on the command line or export them from your environment
# ve.build() handles useful defaults
# Check the SHELL setup to ensure it is set correctly
SHELL?=/bin/bash
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

include $(VE_MAKEVARS)
# $(VE_MAKEVARS) gets rebuilt (see below) if it is out of date, using build-config.R
# Make then auto-restarts to read:
#   VE_OUTPUT, VE_CACHE, VE_LIB, VE_INSTALLER, VE_PLATFORM, VE_SRC
#   and others
export VE_RUNTIME_CONFIG

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

all build dev: reset configure-build repository-build lib-build modules-build runtime-build
	@echo Build complete

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
