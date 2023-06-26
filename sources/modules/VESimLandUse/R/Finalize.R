#' @include     CreateSimBzoneModels.R Initialize.R CreateSimBzones.R SimulateHousing.R SimulateEmployment.R Simulate4DMeasures.R SimulateUrbanMixMeasure.R AssignCarSvcAvailability.R AssignDemandManagement.R AssignParkingRestrictions.R
NULL

if ( toupper(Sys.getenv("VE_BUILD_PHASE","SAVE"))!="SAVE" ) {
  rm(SimD4c_,SimPredD4c_,Tn_df,D4_Ua_df,D4_df,Ru_df,Pt_df,Ua_df,D_df)
}

# cat("Object Sizes (Collated Last)\n")
# obj.sizes <- (sapply(ls(), function(x) object.size(get(x))))
# obj.sizes <- data.frame(Name=names(obj.sizes),Size=obj.sizes)
# obj.sizes <- obj.sizes[order(obj.sizes$Size),]
# print(obj.sizes)

