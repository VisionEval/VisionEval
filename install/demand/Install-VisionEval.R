# VisionEval initialization script
# Run once to install everything and build VisionEval.Rdata

# Put the current directory into ve.root
ve.root <- getwd()

# Configure depending on installation source
ve.remote <- "https://visioneval.jeremyraw.com/R/"
ve.local  <- file.path(ve.root,"R")
if ( ! dir.exists(ve.local) )
	ve.local <- normalizePath(file.path(ve.local,"../../www/R"),winslash="/")# see if the development environment is available

# Put the library directory into ve.library
ve.lib <- file.path(ve.root,"lib")
if ( ! dir.exists(ve.lib) ) dir.create(ve.lib)

# Install the VE packages and dependencies
# If it looks like we have suitable 'src' and 'bin' in a local 'R' folder, use that for packages
# Otherwise, reach for the default online server

ve.repos <- ifelse( dir.exists(ve.local), paste("file:",ve.local,sep=""), ve.remote )
VE.pkgs <-
c(	 "visioneval"
	,"VE2001NHTS"
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
# Also need dependencies for VEGUI
	,"data.table"
	,"DT"
	,"envDocument"
	,"future"
	,"jsonlite"
	,"namedCapture"
	,"rhandsontable"
	,"rhdf5"
	,"shiny"
	,"shinyAce"
	,"shinyBS"
	,"shinyFiles"
	,"shinyjs"
	,"shinyTree"
	,"testit"
)
ls()

install.packages(
	VE.pkgs,
	lib=ve.lib,
	repos=ve.repos,
	dependencies=TRUE
)
