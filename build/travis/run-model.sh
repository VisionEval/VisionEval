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

RUNNER=sources/tools/models.R

echo Running the model...
echo Model Path ${MODEL_PATH}
cat >run-script.R <<RUNSCRIPT
tryCatch( {
  require(visioneval,quietly=TRUE)
  cat("Importing model API\n")
  import::here(openModel,.from='${RUNNER}')
  ve.runtime <- 'sources'
  model <- openModel('${MODEL_PATH}')
  cat("Running model",model\$modelName,"\n")
  model\$run()
  },
  error=function(e) { print(e); quit(status=1) }
)
RUNSCRIPT
echo Processed Run Script:
R --vanilla --slave --quiet -f run-script.R
