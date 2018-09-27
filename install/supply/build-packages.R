# Build and (optionally) Check the VE packages
# paramter "repo.root" should be set to the root folder of the current repository clone
# Set up an .Rprofile in this folder and source it (e.g. by starting a fresh R)

#TODO: consider building source into "built/src" and binary into "built/contrib/windows/..."
if (!is.defined(repo.root))
	repo.root <- choose.dir(getwd(),caption="Locate Repository Root Directory")
if (is.na(repo.root))
	stop("Can't build packages without knowing the repository")
build.dir <- file.path(repo.root,"install","built")
framework.dir <- file.path(repo.root,"sources","framework")
modules.dir <- file.path(repo.root,"sources","modules")
built.path <- file.path(repo.root,"install","built")

# Create module manifest
# NOTE: there is an order dependency for building/checking modules
# NOTE: list formatted here to make it easy to stuff in another module
framework <- c("visioneval")
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
if (askYesNo("Comprehensively check packages (Warning: PAINFUL)")) {
	devtools::check(file.path(framework.dir,framework),path=built.path,binary=TRUE)
	for (module in build.modules) devtools::check(module,path=built.path,binary=TRUE)
}

# Build the framework and modules as source packages
devtools::build(file.path(framework.dir,framework),path=built.path)
for (module in build.modules) devtools::build(module,path=built.path)

# Build the framework and modules as binary packages (note - side effect is to install them)
if (askYesNo("Build binary packages (Warning: SLOW)")) {
	devtools::build(file.path(framework.dir,framework),path=built.path,binary=TRUE)
	for (module in build.modules) devtools::build(module,path=built.path,binary=TRUE)
}
