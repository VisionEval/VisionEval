# Developer Install VisionEval Resources from within the repository

#library(httr)
#If working within a proxy server, run the following commands to enable install from GitHub
#set_config(use_proxy(url="proxynew.odot.state.or.us", port=8080))
#set_config( config( ssl_verifypeer = 0L ) )

#Set repository
local({currentRepo <- getOption("repos")
currentRepo["CRAN"] <- "https://cran.cnr.berkeley.edu/"
options(repos = currentRepo)
})

# Download and install the required libraries and their dependencies
cat("\nInstalling dependencies\n")
install.packages(c("curl","devtools", "roxygen2", "stringr", "knitr", "digest"), dependencies = TRUE, quiet=TRUE)
install.packages(c("shiny", "shinyjs", "shinyFiles", "data.table", "DT", "shinyBS", "future", "testit", "jsonlite", "shinyAce", "envDocument", "rhandsontable","shinyTree"), dependencies = TRUE, quiet=TRUE)
devtools::install_github("tdhock/namedCapture", quiet=TRUE)
source("https://bioconductor.org/biocLite.R")
biocLite(c("rhdf5","zlibbioc"), suppressUpdates=TRUE, quiet=TRUE)

# VE framework and modules
VE_framework <- "visioneval"
VE_modules <- c(
	"VE2001NHTS",
	"VESyntheticFirms",
	"VESimHouseholds",
	"VELandUse",
	"VETransportSupply",
	"VETransportSupplyUse",
	"VEHouseholdTravel",
	"VEHouseholdVehicles",
	"VEPowertrainsAndFuels",
	"VETravelPerformance",
	"VEReports"
)

### TODO:
### install VE packages from "install/built"
### offer to check and (re-)build packages prior to installation
### Optional: look at installed.packages() and using update instead of install
### (I think it works just to install over existing if the package is not actually loaded)

#Install the required VE framework package
cat("\nInstalling VE framework\n")
devtools::install_local(normalizePath(file.path(destdir, "sources", "framework", VE_framework)))

#Download and install the required VE modules for VERPAT and VERSPM
for(module in VE_modules){
	cat(paste("\nInstalling Module:", module,"\n"))
	devtools::install_local(normalizePath(file.path(destdir, "sources", "modules", module)))
	if(!module %in% rownames(installed.packages())){
		stop(paste0(module, " cannot be installed."))
	}
}