# Build and (optionally) Check the VE packages
# paramter "ve.root" should be set to the root folder of the current repository clone
# Set up an .Rprofile in this folder and source it (e.g. by starting a fresh R)

# This should really happen through a build system (Make, Maven, etc.) rather
# than the all-or-nothing script here.

if (!exists("ve.root") || !file.exists(ve.root) )
	ve.root <- choose.dir(getwd(),caption="Locate Repository Root Directory")
if (!exists("ve.root") || is.na(ve.root) || !file.exists(ve.root) ) # NA generated if user cancels choose.dir
	stop("Can't build packages without knowing the repository")

# Where to find the package sources
framework.dir <- file.path(ve.root,"sources","framework")
modules.dir <- file.path(ve.root,"sources","modules")

# Where to put the built results (these are not version controlled, so create if missing)
dir.create( built.path <- file.path(ve.root,"install","built"), showWarnings=FALSE )
dir.create( built.path.src <- file.path(built.path,"src"), showWarnings=FALSE )
dir.create( built.path.binary <- file.path(built.path,"bin"), showWarnings=FALSE )

# Create module manifest
# NOTE: there is an order dependency for building/checking modules
# NOTE: list formatted here to make it easy to add another module
# TODO: create a manifest file in "sources/modules" with all module names in build order
framework <- file.path(framework.dir,"visioneval")
modules <- c(
	 "VE2001NHTS"
	,"VESyntheticFirms"
	,"VESimHouseholds"
	,"VELandUse"
	,"VETransportSupply"
	,"VETransportSupplyUse"
	,"VEHouseholdTravel"
	,"VEHouseholdVehicles"
	,"VEPowertrainsAndFuels"
	,"VETravelPerformance"
	,"VEReports"
)
build.modules <- file.path(modules.dir,modules)

require(devtools)

# Check the packages
if (askYesNo("Comprehensively check packages (Warning: PAINFUL)",default=FALSE)) {
	devtools::check(framework)
	for (module in build.modules) devtools::check(module)
}

# Build the framework and modules as source packages
if (askYesNo("Build source packages)",default=TRUE)) {
	devtools::build(framework,path=built.path.src)
	for (module in build.modules) devtools::build(module,path=built.path.src)
}

# Build the framework and modules as binary packages (note - side effect is to install them)
if (askYesNo("Build binary packages (Warning: SLOW)",default=FALSE)) {
	devtools::build(framework,path=built.path.binary,binary=TRUE)
	for (module in build.modules) devtools::build(module,path=built.path.binary,binary=TRUE)
}
