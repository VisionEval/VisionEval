#' @include AdjustVehicleOwnership.R AssignDrivers.R AssignVehicleAge.R AssignVehicleFeatures.R AssignVehicleFeaturesFuture.R AssignVehicleOwnership.R AssignVehicleType.R CalculateVehicleOwnCost.R CreateVehicleTable.R
NULL

if ( exists("Hh_df",inherits=FALSE) ) rm(Hh_df)
if ( exists("Veh_df",inherits=FALSE) ) rm(Veh_df)
if ( exists("Per_df",inherits=FALSE) ) rm(Per_df)  

cat("Object Sizes (Collated Last)\n")
obj.sizes <- (sapply(ls(), function(x) object.size(get(x))))
obj.sizes <- data.frame(Name=names(obj.sizes),Size=obj.sizes)
obj.sizes <- obj.sizes[order(obj.sizes$Size),]
print(obj.sizes)
