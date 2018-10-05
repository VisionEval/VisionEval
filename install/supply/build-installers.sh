#/bin/bash

# This builds (and publishes) the .zip installers - they go straight
# into the www folder

# First the online installer, which will pull the R packages from the
# visioneval installation server

VE_INSTALLER=../www/VE-installer.zip
VE_WINDOWS=../www/VE-installer-windows-R3.5.1.zip

cd ../demand
[ -f $VE_INSTALLER ] && rm $VE_INSTALLER

zip --recurse-paths ${VE_INSTALLER} .Rprofile Install-VisionEval.bat Install-VisionEval.R RunVisionEval.R models vegui

# Windows installer
zip --recurse-paths --output-file=${VE_WINDOWS} ${VE_INSTALLER} lib

# Second, copy the first .zip file and add the miniCRAN to it so
# all the gigabytes of packages can be downloaded at once.
# But that produces a 1.1 Gb archive, so we won't go there...

# cd ../www/R # Grab the miniCRAN and add it
# zip --recurse-paths --output-file=../${VE_FULL} ../${VE_INSTALLER} bin src

# TODO; review the insanely huge number of dependencies and streamline
# them.  There's probably a lot of cruft that some careful thinking
# might be able to sidestep
