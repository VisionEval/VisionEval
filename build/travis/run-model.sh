#! /bin/bash
set -ev

BUILD_LIB=${VE_LIBRARY:-$TRAVIS_BUILD_DIR/ve-lib}
[ -d ${BUILD_LIB} ] || mkdir -p ${BUILD_LIB}

if [ -n "${BUILD_LIB}" ]
then
  export R_LIBS_USER=${BUILD_LIB}:${R_LIBS_USER}
fi

MODEL_PATH=$1
MODEL_DIR=$(dirname $MODEL_PATH)
MODEL_DIR=${MODEL_DIR:-.}

RUNNER=/sources/tools/models.R

echo Running R to run the model...
Rscript -e << RUNSCRIPT
tryCatch( {
  require(visioneval,quietly=TRUE)
  cat("Importing model API\n")
  import::here(openModel,.from='$RUNNER')
  ve.runtime <- '/sources'
  model <- openModel('$MODEL_PATH')
  cat("Running model",model\$modelName,"\n")
  model\$run()
  }
)
RUNSCRIPT
