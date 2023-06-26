# Parameters.R
# Define visioneval.cnf parameter defaults for parameters used in this package.

# Add default for the SnapshotDir
default.parameters.table = list(
  SnapshotDir = "snapshot",
  DynamicDir  = "dynamic"
)

# Hook to present too getRunParameter (boilerplate)
#' Hook to add default values for key parameters in the VEModel package
#'
#' \code{VEPackageRunParameters} extends \code{visioneval::defaultVERuntimeParameters} to provide
#' additional Parameters and Defaults for VEModel functions that can be accessed seamlessly. You can
#' call this function directly (e.g. to see what parameters are defined and defaulted in
#' VESnapshot). See \code{visioneval::defaultVERuntimeParameters} for details.
#'
#' @return a named list for parameter default values for parameters used in this package
#' @import visioneval
#' @export
VEPackageRunParameters <- function() {
  return ( visioneval::addParameterSource(default.parameters.table,"VESnapshot Default") )
}

