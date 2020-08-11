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
rm -rf inst/module_docs # Don't need these for testing; just confuses devtools::check

# TEST_SCRIPT=${2:-${VE_SCRIPT:-tests/scripts/test.R}}
# echo TEST_SCRIPT=${TEST_SCRIPT}
Rscript -e "devtools::check('.',cran=FALSE,error_on='error')"
# if [ -f ${TEST_SCRIPT} ]
# then
# 	echo Executing ${TEST_SCRIPT} in $(pwd)
# 	Rscript -e "tryCatch( source('${TEST_SCRIPT}') )"
# else
# 	echo ${TEST_SCRIPT} does not exist in $(pwd)
# fi
echo Installing "$(basename $(pwd))" to ${BUILD_LIB}
R CMD INSTALL -l "${BUILD_LIB}" . # Save the installed package for later use
popd
