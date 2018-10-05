# VisionEval initialization script
# Run once to install everything and build VisionEval.Rdata

# Set an option to force end-user style installation on Windows
# (force "binary" even if a newer source package is available for compilation).
# Note that on Linux or MacOS, source packages are always used.
# Compiling the sources can be beastly for big packages.

options(install.packages.compile.from.source="never")

# As configured, if a newer source is available but consists only of
# R code, the newer source package will still be used.
# To force only binary packages in all cases, uncomment the following

# options(install.packages.check.source="no")

# Put the current directory into ve.root
ve.root <- getwd()

# Put the library directory into ve.lib
ve.lib <- file.path(ve.root,"lib")
if ( ! dir.exists(ve.lib) ) {
	# We'll presume that if it's there, it has what we need (if not, delete it and re-run installation)
	# That allows a "pure windows installer" with the lib directory pre-loaded

	# Configure depending on installation source
	ve.remote <- "https://visioneval.jeremyraw.com/R/"
	ve.local  <- file.path(ve.root,"R") # This is where the miniCRAN gets placed by the offline installer
	if ( ! dir.exists(ve.local) )
		ve.local <- normalizePath(file.path(ve.local,"../../www/R"),winslash="/")# see if the development environment is available


	dir.create(ve.lib)

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

	install.packages(
		VE.pkgs,
		lib=ve.lib,
		repos=ve.repos,
		dependencies=TRUE
	)
}

# Construct "RunVisionEval.Rdata"
# In principle, we could do this in .Rprofile below, but this
# gives us something to "double-click" for a rapid happy start
# in RGui.

# TODO: We'll want to do some better library-finding
# I'd recommend blowing away all the library paths except the
# one that has the base packages, then forcing ve.lib to the
# front of the list as is done here

.First <- function() {
	.libPaths(c(ve.lib,.libPaths()))
	require(visioneval)
	setwd(ve.root)
	cat("Welcome to VisionEval!\n")
}

# The following convenience functions have not been well-tested
vegui <- function() {
	require("shiny")
	full_path <- file.path(ve.root,"VEGUI")
	cat(full_path,"\n")
	setwd(full_path)
	runApp('../VEGUI')
	setwd(ve.root)
}

# The following two functions run the command line version per the
# Getting Started document

verpat <- function() {
	full_path = file.path(ve.root,"models/VERPAT")
	setwd(full_path)
	source("run_model.R")
	setwd(ve.root)
}

verspm <- function() {
	full_path = file.path(ve.root,"models/VERSPM")
	setwd(full_path)
	source("run_model.R")
	setwd(ve.root)
}

save(ve.root,ve.lib,.First,vegui, verpat, verspm, file="RunVisionEval.RData")
