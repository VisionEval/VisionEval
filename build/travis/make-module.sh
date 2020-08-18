#!/bin/bash

# Updated 2020-08-11 to remove module unit tests
# Those simulate a model environment, and we haven't kept up with
# suitable input/target data in the Test_Data folder under sources/modules
# Next major release will include new tests and a new testing architecture.

set -ev

BUILD_LIB=${VE_LIBRARY:-$TRAVIS_BUILD_DIR/ve-lib}
[ -d ${BUILD_LIB} ] || mkdir -p ${BUILD_LIB}

if [ -n "${BUILD_LIB}" ]
then
  export R_LIBS_USER=${BUILD_LIB}:${R_LIBS_USER}
fi

pushd $1
mkdir -p data  # No error if already exists, but must exist to document/install

# TEST_SCRIPT=${2:-${VE_SCRIPT:-tests/scripts/test.R}}
# echo TEST_SCRIPT=${TEST_SCRIPT}
Rscript -e "roxygen2::roxygenise()" # estimates models, creates NAMESPACE, help docs, data
echo Installing "$(basename $(pwd))" to ${BUILD_LIB}
R CMD INSTALL -l "${BUILD_LIB}" . # Save the installed package for later use
popd
