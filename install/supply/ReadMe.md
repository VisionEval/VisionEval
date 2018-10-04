The "supply" folder contains R scripts and resources necessary to
build the packages necessary to install VisionEval.

Prerequisites include R and preferably R-Studio (there's also
supply.RProj for RStudio).  You should install RTools if you're planning
to build Windows binaries.  You will also need devtools and miniCRAN,
plus dependencies - you can enable packrat to store those.

The following operations are performed (from a clone of the repository
on your local machine).  The result will be binary installers, plus
a web-accessible repository of all necessary VisionEval packages.

0. Development Environment for the "supply" operation (build installer)
	* Edit .RProfile to identify ve.root (parent of "install")
1. Build VisionEval packages (source and, if running on Windows, binaries)
	* Run R script: build-packages.R
	* Output from this step is placed into the "built" subfolder here (src and bin)
	* Packages are optionally checked (R CMD check) and then built
	* The built packages are used to populate the miniCRAN (next step)
2. Build the namedCapture package from Github
	* Used only - and probably gratuitously - in VEGUI)
	* Clone https://github.com/tdhock/nameCapture into install/external/namedCapture
	* Run bash script in "install/external" to build: buildNamedCapture.sh
	* Output packages are added to the built VE packages (source, binary)
3. Create a miniCRAN repository
	* Update dependencies in R script build-miniCRAN.R
	* Then run the R script
	* Output is placed in "../built/miniCRAN"
	* If you want to update the web site, run shell script "publish-miniCRAN.sh"
		* The script uses rsync and ssh to upload files to the remote web server
4. Update the Install.R script (located next door in the "demand" subfolder) as necessary
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
