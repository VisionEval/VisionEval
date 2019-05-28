#!/bin/bash
set -ev

BUILD_LIB=${VE_LIBRARY:-$TRAVIS_BUILD_DIR/ve-lib}
[ -d ${BUILD_LIB} ] || mkdir -p ${BUILD_LIB}

if [ -n "${BUILD_LIB}" ]
then
  export R_LIBS_USER=${BUILD_LIB}:${R_LIBS_USER}
fi

pushd $1
TEST_SCRIPT=${2:-${VE_SCRIPT:-tests/scripts/test.R}}
echo TEST_SCRIPT=${TEST_SCRIPT}
Rscript -e "devtools::check('.',cran=FALSE,error_on='error')"
if [ -f ${TEST_SCRIPT} ]
then
	echo Executing ${TEST_SCRIPT} in $(pwd)
	Rscript -e "tryCatch( source('${TEST_SCRIPT}') )"
else
	echo ${TEST_SCRIPT} does not exist in $(pwd)
fi
echo Installing "$(basename $(pwd))" to ${BUILD_LIB}
R CMD INSTALL -l "${BUILD_LIB}" . # Save the installed package for later use
popd
