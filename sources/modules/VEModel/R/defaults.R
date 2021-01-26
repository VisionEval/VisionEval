# Set up package defaults for VE getRunParameter

default.parameters.table = list(
  ModelRoot           = "models",
  ModelScript         = "run_model\\.R",
  ModelScriptFile     = "run_model.R",
  QueryFileName       = "New-Query.VEqry",
  QueryDir            = "queries",
  QueryOutputTemplate = "Measures_%scenario%_%years%_%geography%.csv"
)

#GET DEFAULT PARAMETERS
#======================
#' Report useful default values for key parameters
#'
#' \code{defaultVERuntimeParameters} extends \code{visioneval::defaultVERuntimeParameters} to
#' provide additional Parameters and Defaults for VEModel functions that can be accessed '
#' seamlessly. You can call this function directly (e.g. to see what parameters are defined and '
#' defaulted in VEModel). Internally VEModel uses \code{visioneval::defaultVERuntimeParameters},
#' so it doesn't have to remember the place specific defaults are defined.
#'
#' @param Param_ls a list (possibly empty) of already-defined parameters
#' @return a named list for parameters not present in Param_ls containing default values for those
#'   parameters
#' @export
VEPackageRunParameters <- function(Param_ls=list()) {
  defaultParams_ls <- default.parameters.table[
    which( ! names(default.parameters.table) %in% names(Param_ls) )
  ]
  if ( length(defaultParams_ls)>0 ) {
    defaultParams_ls <- visioneval::addParameterSource(defaultParams_ls,"Package VEModel Default")
    Param_ls <- visioneval::mergeParameters(defaultParams_ls,Param_ls) # Param_ls will override
  }
  return(Param_ls)
}

