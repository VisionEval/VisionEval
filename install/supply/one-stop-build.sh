#!/bin/bash
# (expecting Git for Windows or Git on Linux)

# It's probably safer, less resource intensive, and more informative to
# do these steps manually one at a time, but here's the full train ready
# to roll, and secondarily serving as documentation of what needs to
# happen.

# This expects to find a clean clone of the install tree from git Visit
# 'external' to make sure you have 'rsync' in Git for Windows

# pushd ../external
# bash install-rsync.sh
# popd

Rscript build-miniCRAN.R    # the miniCRAN lives in a web-ready location under install/www/R
Rscript build-packages.R    # Prepare installable visioneval; has a number of annoying user dependencies like rhdf5
			    # Also, it won't currently work as a script, because the cross-dependencies in the
			    # modules require them to be installed into the current session as they are compiled,
			    # but batch operation puts them "out of the way".  Running it interactively should be fine
bash unstage-built-files.sh # a lot of crud is checked in that never should have been
pushd ../external
bash buildNamedCapture.sh   # Prepare installable namedCapture Github package
popd
bash setup-sources.sh
bash build-installers.sh    # these also landi n a web-ready location

# Website is currently set up as a clone of my git repository, so only the miniCRAN gets rsync'ed
# bash publish-installers.R # Loads the "R" directory out to the website

