The "supply" folder contains R scripts and resources necessary to
build the packages necessary to install VisionEval.

The following operations are performed (from a clone of the repository
on your local machine).  The results (in the folders documented below)
will generate the resources required for a rapid-as-possible binary
installation of VisionEval.

1. Build VisionEval packages (source and, if running on Windows, binaries)
	* Output from this step is placed into the "built" subfolder here
	* Packages are built (R CMD build) and checked (R CMD check)
	* The built packages are used to populate the miniCRAN (next step)
	* The build process is scripted for the Git for Windows bash shell
2. Create a miniCRAN repository, and a zipped version
	* Output from miniCRAN is placed into the "repository" subfolder here
	* Consider two versions of miniCRAN:
		1. Only contains the VE packages (for online installation of dependencies)
		2. Contains VE packages plus all dependencies
3. Update the Install.R script (located next door in the "demand" subfolder) as necessary
	* Build process places Install.R into an Rdata file, which will
	  allow double-click starting from Windows and thus automatically make
	  sure that correct directories are used for the miniCRAN and for
	  VE install itself.
	* Install.Rdata gets zipped up along with the local package repository
4. Prepare the "Online" and "Offline" download files (zipped separately into "../demand/installers" subfolder)
	* Online contains the Install.R script plus the VE miniCRAN
	* Offline contains the Install.R script plus the full miniCRAN
	* These are packaged in such a way that the user can follow these instructions:
		1. Install R
		2. Download either the "Offline" or "Online" installer zip file
		3. Unzip into an empty directory
		4. Start RGui by double clicking "Install.Rdata" (or loading it within an
		   existing R session) from that folder (contains functions from Install.R
		   script)
		5. Run the function "install_visioneval" and respond to the prompts/popups