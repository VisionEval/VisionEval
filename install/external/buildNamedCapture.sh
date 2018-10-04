#!/bin/bash -x
# Run in Git for Windows Bash shell or equivalent
# You should do this under Windows or you'll be stuck with a "source-only" build

BUILT_DIR=../built
EXT_DIR=$(pwd)
TMP_LIBRARY="../tmp" # Same one used to hold products of building VisionEval packages

# Lightly check that we have something to build and places to put it
[ ! -d namedCapture ]   && git clone https://github.com:/tdhock/namedCapture.git "${EXT_DIR}/namedCapture"
[ ! -d $BUILT_DIR/src ] && mkdir $BUILT_DIR/src
[ ! -d $BUILT_DIR/bin ] && mkdir $BUILT_DIR/bin
[ ! -d $TMP_LIBRARY ]   && mkdir $TMP_LIBRARY

# Build source and binary packages for namedCapture
cd $BUILT_DIR

R CMD build "${EXT_DIR}/namedCapture"
[ -f namedCapture*.tar.gz ] && mv namedCapture*.tar.gz src

R CMD INSTALL --build --library=$TMP_LIBRARY "${EXT_DIR}/namedCapture"
[ -f namedCapture*.zip ] && mv namedCapture*.zip bin
