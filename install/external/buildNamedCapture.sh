#!/bin/bash -x
# Run in Git for Windows Bash shell
# You can just clone https://github.com:/tdhock/namedCapture into this
# external folder in order to have something to build!

# Establish a temporary library in preparation for binary build
TMP_LIBRARY="../tmp"
echo $TMP_LIBRARY
if [ ! -d $TMP_LIBRARY ]
then
	mkdir "$TMP_LIBRARY"
fi
echo "Installing into $TMP_LIBRARY"

# Build source and binary packages for namedCapture
cd ../built
R CMD build ../external/namedCapture
mv namedCapture*.tar.gz src
R CMD INSTALL --build --library=$TMP_LIBRARY ../external/namedCapture
mv namedCapture*.zip bin


