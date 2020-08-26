#============
#Initialize.R
#============

#<doc>
## Initialize Module
#### June 1, 2020
#
#Modules in the VESimLandUse package synthesize Bzones and their land use attributes as a function of Azone characteristics as well as data derived from the US Environmental Protection Agency's Smart Location Database (SLD) augmented with US Census housing and household income data, and data from the National Transit Database. Details on these data are included in the VESimLandUseData package. The combined dataset contains a number of land use attributes at the US Census block group level. The goal of Bzone synthesis to generate a set of SimBzones in each Azone that reasonably represent block group land use characteristics given the characteristics of the Azone, the Marea that the Azone is a part of, and scenario inputs provided by the user.
#
#Many of the models and procedures used in Bzone synthesis pivot from profiles developed from these data sources for specific urbanized areas, as well as more general profiles for different urbanized area population size categories, towns, and rural areas. Using these specific and general profiles enables the simulated Bzones (SimBzones) to better represent the areas being modeled and the variety of conditions found in different states. Following is a listing of the urbanized areas for which profiles have been developed. Note that urbanized areas that cross state lines are split into the individual state components. This is done to faciliate the development of state models and to better reflect the characteristics of the urbanized area characteristics in each state.
#
#It is incumbent on the model user to identify the name of the urbanized area profile that will be used for each of the Mareas in the model. This module reads in the names assigned in the "marea_uza_profile_names.csv" file and checks their validity. If any are invalid, input processing will stop and error messages will be written to the log identifying the problem names. The following table identifies the names that may be used.
#
#<tab:UzaProfileNames_ls$Names_df>
#
#Note that at the bottom of the table are 6 generic names for urbanized areas of different sizes. If an urbanized area being modeled is not listed in the table, the user may substitute one of these generic names, or may use the name of a different urbanized area that the user believes has similar characteristics. The generic categories represent urbanized areas of different sizes measured by the total numbers of households and jobs in the area as follows:
#
#* **small**: 0 - 50,000 households and jobs
#
#* **medium-small**: 50,001 - 100,000 households and jobs
#
#* **medium**: 100,001 - 500,000 households and jobs
#
#* **medium-large**: 500,001 - 1,000,000 households and jobs
#
#* **large**: 1,000,001 - 5,000,000 households and jobs
#
#* **very-large**: More than 5,000,000 households and jobs
#
#This module includes a number of input checks to avoid data inconsistencies that could cause the model run to fail. Errors and warnings are produced to identify these errors and warnings:
#* The proportions of households by location type for each Azone ('PropMetroHh', 'PropTownHh', 'PropRuralHh' in the 'azone_hh_loc_type_prop.csv' file) are checked to confirm that they add up to 1. If the sum is off by more than 1%, then an error is identified. The error message identifies the Azones and Years that the data is incorrect. If the sum is off by less than 1% the proportions are rescaled to sum to 1 and a warning is identified. The warning message identifies the Azones and Years that the data doesn't sum to 1.
#* The proportions of workers by location type for each Azone ('PropWkrInMetroJobs', 'PropWkrInTownJobs', 'PropWkrInRuralJobs' in 'azone_wkr_loc_type_prop.csv' file) are checked to confirm that they add up to 1. If the sum is off by more than 1%, then an error is identified. The error message identifies the Azones and Years that the data is incorrect. If the sum is off by less than 1% the proportions are rescaled to sum to 1 and a warning is identified. The warning message identifies the Azones and Years that the data doesn't sum to 1.
#* The household and worker proportions ('PropMetroHh' in the 'azone_hh_loc_type_prop.csv' file and 'PropWkrInMetroJobs' in the 'azone_wkr_loc_type_prop.csv' file) are checked to confirm that the value is 0 if the Marea is 'None'. If any values are not 0 then an error is identified. The error messages identify the Azones and Years where the errors occur.
#* The split of metropolitan jobs (i.e. jobs in the urbanized area) among Azones in the Marea ('PropMetroJobs' in the 'azone_wkr_loc_type_prop.csv') is checked to confirm that they sum to 1. If the sum is off by more than 1%, then an error is identified. The error message identifies the Azones and Years that the data is incorrect. If the sum is off by less than 1% the proportions are rescaled to sum to 1 and a warning is identified. The warning message identifies the Azones and Years that the data doesn't sum to 1.
#* The data are check to confirm that there is Town land area each each Azone ('TownLandArea' in the 'azone_loc_type_land_area.csv' file) where activity (households and/or jobs) will be assigned to the the Town location type. Likewise the Metro land area ('MetroLandArea' in the 'azone_loc_type_land_area.csv' file) and Rural average density ('RuralAveDensity' in the 'azone_loc_type_land_area.csv' file) will be checked for consistency with respective activity assignments. If any inconsistencies in the land area are identified, errors will be identified and the error messages will identify the Azones and Years that have the errors.
#* If data are checked to confirm that the average activity densities (households and jobs per acre) for each Azone, location type, and year are sensible. An average activity density is sensible if a Bzone activity density distribution can be created using model data that will reproduce the average activity density. This is done by comparing the density with the lowest allowable Bzone density and with the highest allowable Bzone density identified in the estimated SimBzone model data.
#* The identified 'UzaProfileName' in the 'marea_uza_profile_names.csv' for each Marea is a name included in the SimBzone model data (or 'None' if the Marea is 'None'). If the name is not included, an error is identified.
#* If the 'azone_gq_pop-prop_by_area-type.csv' input file is present (this is an optional input) the 'PropGQPopCenter', 'PropGQPopInner', 'PropGQPopOuter', and 'PropGQPopFringe' are checked to confirm that they sum to 1 for all Azones and Years. If not, then errors are identified and the error messages identify the Azones and Years for which the errors occur.
#</doc>


#=================================
#Packages used in code development
#=================================
#Uncomment following lines during code development. Recomment when done.
#library(visioneval)
#library(VESimLandUseData)


#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================

#' @import visioneval
#' @import VESimLandUseData

UzaProfileNames_ls <- list()

#Make a vector of acceptable urbanized area profile names
#--------------------------------------------------------
AllNames_ <- VESimLandUseData::SimLandUseData_df$UZA_NAME
LocType_ <- VESimLandUseData::SimLandUseData_df$LocType
UzaNames_ <- sort(unique(AllNames_[LocType_ == "Urban"]))
UzaNames_ <-
  c(UzaNames_,
    "small", "medium-small", "medium", "medium-large", "large", "very-large")
UzaProfileNames_ls$Names <- UzaNames_

#Create a table of urbanized area names to use for documentation
#---------------------------------------------------------------
#Make a matrix having 3 columns
UzaNamesPad_ <- c(UzaNames_, rep("", 3 - (length(UzaNames_) %% 3)))
UzaNames_mx <- matrix(UzaNamesPad_, ncol = 3, byrow = TRUE)
#Make a data frame having 3 columns of the names
UzaProfileNames_ls$Names_df <- data.frame(UzaNames_mx)
names(UzaProfileNames_ls$Names_df) <- c("Column 1", "Column 2", "Column 3")
rm(AllNames_, LocType_, UzaNames_, UzaNamesPad_, UzaNames_mx)

#Save the urbanized area profile names list
#------------------------------------------
#' Urbanized area profile names list
#'
#' A list containing the names of urbanized areas included in the profiles of
#' urbanized area land use characteristics.
#'
#' @format A list containing 2 components:
#' \describe{
#'  \item{Names}{a sorted vector of urbanized area names}
#'  \item{Inp}{a 3-column data frame of the names to include in documentation}
#' }
#' @source Initialize.R script.
"UzaProfileNames_ls"
visioneval::savePackageDataset(UzaProfileNames_ls, overwrite = TRUE)


#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
InitializeSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify new tables to be created by Inp if any
  #Specify new tables to be created by Set if any
  #Specify input data
  Inp = items(
    item(
      NAME = "UzaProfileName",
      FILE = "marea_uza_profile_names.csv",
      TABLE = "Marea",
      GROUP = "Global",
      TYPE = "character",
      UNITS = "ID",
      NAVALUE = -1,
      SIZE = 100,
      PROHIBIT = "",
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        "Name of a specific urbanized area for the urbanized area profile to use in SimBzone creation or one of the following: small, medium-small, medium, medium-large, large, very-large"
    ),
    item(
      NAME = items(
        "PropMetroHh",
        "PropTownHh",
        "PropRuralHh"
      ),
      FILE = "azone_hh_loc_type_prop.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0", "> 1"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = items(
        "Proportion of households residing in the metropolitan (i.e. urbanized) part of the Azone",
        "Proportion of households residing in towns (i.e. urban-like but not urbanized) in the Azone",
        "Proportion of households residing in rural (i.e. not urbanized or town) parts of the Azone"
      )
    ),
    item(
      NAME = items(
        "PropWkrInMetroJobs",
        "PropWkrInTownJobs",
        "PropWkrInRuralJobs",
        "PropMetroJobs"
      ),
      FILE = "azone_wkr_loc_type_prop.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0", "> 1"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = items(
        "Proportion of workers residing in the Azone who work at jobs in the metropolitan (i.e. urbanized) area associated with the Azone",
        "Proportion of workers residing in the Azone who work at jobs in towns (i.e. urban-like but not urbanized) in the Azone",
        "Proportion of workers residing in the Azone who work at jobs in rural (i.e. not urbanized or town) parts of the Azone",
        "Proportion of the jobs of the metropolitan area that the Azone is associated with that are located in the metropolitan portion of the Azone"
      )
    ),
    item(
      NAME = "TownJobWkrRatio",
      FILE = "azone_wkr_loc_type_prop.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        "Ratio of jobs located in towns in the Azone to workers residing in the Azone who work at jobs located in towns (inside or outside the Azone"
    ),
    item(
      NAME = items(
        "MetroLandArea",
        "TownLandArea"
      ),
      FILE = "azone_loc_type_land_area.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "area",
      UNITS = "SQMI",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = items(
        "Land area (excluding large water bodies and large tracts of undevelopable land) in the metropolitan (i.e. urbanized) portion of the Azone",
        "Land area (excluding large water bodies and large tracts of undevelopable land) in towns (i.e. urban-like but not urbanized) in the Azone"
      )
    ),
    item(
      NAME = "RuralAveDensity",
      FILE = "azone_loc_type_land_area.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "HHJOB/ACRE",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = items(
        "Average activity density (households and jobs per acre) of rural (i.e. not metropolitan or town) portions of the Azone not including large waterbodies or large tracts of agricultural lands, forest lands, or otherwise protected lands"
      )
    ),
    item(
      NAME = items(
        "PropGQPopCenter",
        "PropGQPopInner",
        "PropGQPopOuter",
        "PropGQPopFringe"),
      FILE = "azone_gq_pop-prop_by_area-type.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0", "> 1"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = items(
        "Proportion of Azone non-institutional group quarters population located in center area type",
        "Proportion of Azone non-institutional group quarters population located in inner area type",
        "Proportion of Azone non-institutional group quarters population located in outer area type",
        "Proportion of Azone non-institutional group quarters population located in fringe area type"
      ),
      OPTIONAL = TRUE
    ),
    item(
      NAME =
        items("Age0to14",
              "Age15to19",
              "Age20to29",
              "Age30to54",
              "Age55to64",
              "Age65Plus"),
      FILE = "azone_hh_pop_by_age.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        items(
          "Household (non-group quarters) population in 0 to 14 year old age group",
          "Household (non-group quarters) population in 15 to 19 year old age group",
          "Household (non-group quarters) population in 20 to 29 year old age group",
          "Household (non-group quarters) population in 30 to 54 year old age group",
          "Household (non-group quarters) population in 55 to 64 year old age group",
          "Household (non-group quarters) population in 65 or older age group")
    ),
    item(
      NAME = "AveHhSize",
      FILE = "azone_hhsize_targets.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "PRSN/HH",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = "Average household size of households (non-group quarters)"
    ),
    item(
      NAME = "Prop1PerHh",
      FILE = "azone_hhsize_targets.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion of households",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = "Proportion of households (non-group quarters) having only one person"
    ),
    item(
      NAME =
        items("GrpAge0to14",
              "GrpAge15to19",
              "GrpAge20to29",
              "GrpAge30to54",
              "GrpAge55to64",
              "GrpAge65Plus"),
      FILE = "azone_gq_pop_by_age.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        items("Group quarters population in 0 to 14 year old age group",
              "Group quarters population in 15 to 19 year old age group",
              "Group quarters population in 20 to 29 year old age group",
              "Group quarters population in 30 to 54 year old age group",
              "Group quarters population in 55 to 64 year old age group",
              "Group quarters population in 65 or older age group")
    ),
    item(
      NAME =
        items("RelEmp15to19",
              "RelEmp20to29",
              "RelEmp30to54",
              "RelEmp55to64",
              "RelEmp65Plus"),
      FILE = "azone_relative_employment.csv",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        items("Ratio of workers to persons age 15 to 19 in model year vs. in estimation data year",
              "Ratio of workers to persons age 20 to 29 in model year vs. in estimation data year",
              "Ratio of workers to persons age 30 to 54 in model year vs. in estimation data year",
              "Ratio of workers to persons age 55 to 64 in model year vs. in estimation data year",
              "Ratio of workers to persons age 65 or older in model year vs. in estimation data year"),
      OPTIONAL = TRUE
    )
  )
)
#Save the data specifications list
#---------------------------------
#' Specifications list for Initialize module
#'
#' A list containing specifications for the Initialize module.
#'
#' @format A list containing 2 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Inp}{scenario input data to be loaded into the datastore for this
#'  module}
#' }
#' @source Initialize.R script.
"InitializeSpecifications"
visioneval::savePackageDataset(InitializeSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================

#Define function to allocate integer quantities among categories
#---------------------------------------------------------------
#' Allocate integer quantities among categories
#'
#' \code{splitIntegers} splits a total value into a vector of whole numbers to
#' reflect input vector of proportions
#'
#' This function splits an input total into a vector of whole numbers to reflect
#' an input vector of proportions. If the input total is not an integer, the
#' value is rounded and converted to an integer.
#'
#' @param Tot a number that is the total value to be split into a vector of
#' whole numbers corresponding to the input proportions. If Tot is not an
#' integer, its value is rounded and converted to an integer.
#' @param Props_ a numeric vector of proportions used to split the total value.
#' The values should add up to approximately 1. The function will adjust so that
#' the proportions do add to 1.
#' @return a numeric vector of whole numbers corresponding to the Props_
#' argument which sums to the Tot.
#' @export
splitIntegers <- function(Tot, Props_) {
  #Convert Tot into an integer
  if (!is.integer(Tot)) {
    Tot <- as.integer(round(Tot))
  }
  #If Tot is 0, return vector of zeros
  if (Tot == 0) {
    integer(length(Props_))
  } else {
    #Make sure that Props_ sums exactly to 1
    Props_ <- Props_ / sum(Props_)
    #Make initial whole number split
    Ints_ <- round(Tot * Props_)
    #Determine the difference between the initial split and the total
    Diff <- Tot - sum(Ints_)
    #Allocate the difference
    if (Diff != 0) {
      for (i in 1:abs(Diff)) {
        IdxToChg <- sample(1:length(Props_), 1, prob = Props_)
        Ints_[IdxToChg] <- Ints_[IdxToChg] + sign(Diff)
      }
    }
    unname(Ints_)
  }
}

#Define function to calculate number of households by location type
#------------------------------------------------------------------
#' Calculate number of households by location type
#'
#' \code{calcNumHhByLocType} calculates the number of households by location
#' type for a set of Azones.
#'
#' This function calculates the number of households by location type for a
#' set of Azones as a function of the total number of households by Azone and
#' user inputs on the proportions of households by location type and Azone.
#' Location types are metropolitan (i.e. urbanized area), town (i.e. urban areas
#' that are not urbanized), and rural.
#'
#' @param NumHh_Az A numeric vector of the total number of households in each
#' Azone.
#' @param PropRuralHh_Az A numeric vector identifying the proportion of
#'   households in each Azone that are located in rural locations.
#' @param PropTownHh_Az A numeric vector identifying the proportion of
#'   households in each Azone that are located in town locations.
#' @param PropMetroHh_Az A numeric vector identifying the proportion of
#'   households in each Azone that are located in metropolitan locations.
#' @param Az A character vector of Azone names.
#' @return A list having 3 named components (Rural, Town, Metropolitan) where
#' each component is a numeric vector identifying the number of households in
#' the respective location type in each Azone.
#' @export
calcNumHhByLocType <-
  function(NumHh_Az, PropRuralHh_Az, PropTownHh_Az, PropMetroHh_Az, Az) {
    HhProp_AzLt <- cbind(
      Rural = PropRuralHh_Az,
      Town = PropTownHh_Az,
      Urban = PropMetroHh_Az)
    Hh_AzLt <- t(apply(cbind(NumHh_Az, HhProp_AzLt), 1, function(x) {
      splitIntegers(x[1], x[2:4])}))
    colnames(Hh_AzLt) <- colnames(HhProp_AzLt)
    rownames(Hh_AzLt) <- Az
    #Return as list
    #--------------
    list(
      Rural = Hh_AzLt[,"Rural"],
      Town = Hh_AzLt[,"Town"],
      Urban = Hh_AzLt[,"Urban"]
    )
  }

#Define function to calculate the number of jobs by Azone location type
#----------------------------------------------------------------------
#' Calculate number of jobs by location type
#'
#' \code{calcNumJobsByLocType} calculates the number of jobs by location type
#' for a set of Azones
#'
#' This function calculates the number of jobs by location type for a set of
#' Azones as a function of the total number of workers by Azone and user inputs
#' on the proportions of jobs by location type (metropolitan, town, rural) and
#' Azone. In addition, the user specifies the proportional allocation of jobs
#' among the metropolitan portions of Azones that make up an Marea. The function
#' logic is based on the assumption that Azone workers having jobs in town and
#' rural locations, will work within the Azone where they reside but that
#' workers having metropolitan jobs may work in a different Azone portion of the
#' metropolitan area which includes their Azone.
#'
#' @param NumWkr_Az A numeric vector of the total number of workers residing in
#' each Azone.
#' @param PropWkrInRuralJobs_Az A numeric vector of the proportions of workers
#'   in each Azone that have jobs located in rural locations.
#' @param PropWkrInTownJobs_Az A numeric vector of the proportions of workers in
#'   each Azone that have jobs located in town locations.
#' @param PropWkrInMetroJobs_Az A numeric vector of the proportions of workers
#'   in each Azone that have jobs located in metropolitan locations.
#' @param PropMetroJobs_Az A numeric vector identifying the proportion of
#' metropolitan jobs for the Marea that the Azone is a part of that are located
#' in the metropolitan portion of the Azone.
#' @param TownJobWkrRatio_Az A numeric vector identifying the ratio of the
#' number of jobs in the town portion of the Azone to the number of Azone
#' workers who work in towns (inside and outside the Azone)
#' @param Marea_Az A character vector identifying the Marea associated with each
#' Azone.
#' @param Az A character vector of Azone names.
#' @return A list having 3 named components (Rural, Town, Metropolitan) where
#'   each component is a numeric vector identifying the number of jobs in the
#'   respective location type in each Azone.
#' @export
calcNumJobsByLocType <-
  function(NumWkr_Az, PropWkrInRuralJobs_Az, PropWkrInTownJobs_Az,
           PropWkrInMetroJobs_Az, PropMetroJobs_Az,
           TownJobWkrRatio_Az, Marea_Az, Az) {

    #Initial allocation of jobs within Azones
    #----------------------------------------
    JobProp_AzLt <- cbind(
      Rural = PropWkrInRuralJobs_Az,
      Town = PropWkrInTownJobs_Az,
      Metropolitan = PropWkrInMetroJobs_Az)
    rownames(JobProp_AzLt) <- Az
    Jobs_AzLt <- t(apply(cbind(NumWkr_Az, JobProp_AzLt), 1, function(x) {
      splitIntegers(x[1], x[2:4])
    }))
    colnames(Jobs_AzLt) <- colnames(JobProp_AzLt)
    rownames(Jobs_AzLt) <- Az

    #Adjust the town jobs to reflect proportion of town workers who work in Azone
    #----------------------------------------------------------------------------
    TownWkr_Az <- Jobs_AzLt[,"Town"]
    TownJobs_Az <- round(TownWkr_Az * TownJobWkrRatio_Az)

    #Reallocate metropolitan jobs among Azones in the Marea
    #------------------------------------------------------
    #Create data frame of metropolitan data
    Metro_df <- data.frame(
      Jobs = Jobs_AzLt[,"Metropolitan"],
      PropMetroJobs = PropMetroJobs_Az,
      Marea = Marea_Az,
      Azone = Az
    )
    #Split by metropolitan area
    Metro_Ma_df <- split(Metro_df, Metro_df$Marea)
    #Allocate metropolitan jobs among Azones in Marea
    MetroJobs_Az <- unlist(lapply(Metro_Ma_df, function(x) {
      splitIntegers(sum(x$Jobs), x$PropMetroJobs)
    }), use.names = FALSE)
    names(MetroJobs_Az) <- unlist(lapply(Metro_Ma_df, function(x) x$Azone))
    MetroJobs_Az <- MetroJobs_Az[Az]

    #Return as list
    #--------------
    list(
      Rural = Jobs_AzLt[,"Rural"],
      Town = TownJobs_Az,
      Urban = MetroJobs_Az,
      TownWkr = TownWkr_Az,
      UrbanWkr = Jobs_AzLt[,"Metropolitan"]
    )
  }

#Main module function that checks whether urbanized area name is correct
#-----------------------------------------------------------------------
#' Check and adjust fuel and powertrain proportions inputs.
#'
#' \code{Initialize} checks optional fuel and powertrains proportions datasets
#' to determine whether they each sum to 1, creates error and warning messages,
#' and makes adjustments if necessary.
#'
#' This function processes optional user energy and emissions inputs that have
#' been preprocessed by the processModuleInputs function. It checks datasets
#' that specify fuel type or powertrain type proportions to determine whether
#' they sum to 1. If the sum for a dataset differs from 1 by more than 1%, then
#' the function returns an error message identifying the problem dataset. If the
#' sum differs from 1 but the difference is 1% or less it is assumed that the
#' difference is due to rounding errors and function adjusts the proportions so
#' that they equal 1. In this case, a warning message is returned as well that
#' the framework will write to the log.
#'
#' @param L A list containing data from preprocessing supplied optional input
#' files returned by the processModuleInputs function. This list has two
#' components: Errors and Data.
#' @return A list that is the same as the input list with an additional
#' Warnings component.
#' @import visioneval
#' @export
Initialize <- function(L) {

  #Set up
  #------
  #Retrieve the model state
  G <- getModelState()
  #Initialize error and warnings message vectors
  Errors_ <- character(0)
  Warnings_ <- character(0)
  #Add the Marea identification in the Azone list
  L$Data$Year$Azone$Marea <-
    G$Geo_df$Marea[match(L$Data$Year$Azone$Geo, G$Geo_df$Azone)]
  #Initialize output list with input values
  AzoneVars_ <- names(L$Data$Year$Azone)
  NotSaveVars_ <-
    c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64",
      "Age65Plus", "AveHhSize", "Prop1PerHh", "GrpAge0to14", "GrpAge15to19",
      "GrpAge20to29", "GrpAge30to54", "GrpAge55to64", "GrpAge65Plus",
      "RelEmp15to19", "RelEmp20to29", "RelEmp30to54", "RelEmp55to64",
      "RelEmp65Plus", "Marea")
  OutAzoneVars_ <- AzoneVars_[-which(AzoneVars_ %in% NotSaveVars_)]
  Out_ls <- L
  Out_ls$Data$Year$Azone <- Out_ls$Data$Year$Azone[OutAzoneVars_]

  #Define function to check whether proportions add to 1 and adjust
  #----------------------------------------------------------------
  checkProps <- function(Names_, Geo, File) {
    Values_df <- data.frame(L$Data$Year[[Geo]][c("Year", "Geo", Names_)])
    Values_df$Geo <- as.character(Values_df$Geo)
    Values_df$Year <- as.character(Values_df$Year)
    Yr <- unique(Values_df$Year)
    for (yr in Yr) {
      V_df <- Values_df[Values_df$Year == yr,]
      SumDiff_ <- abs(1 - rowSums(V_df[,Names_]))
      HasErr_ <- SumDiff_ > 0.01
      HasWarn_ <- SumDiff_ > 0 & SumDiff_ < 0.01
      if (any(HasErr_)) {
        ErrAzones_ <- V_df$Geo[HasErr_]
        Msg <- paste0(
          "Error in input file '", File, "' for year ", yr,
          " and the following Azones: ",
          paste(ErrAzones_, collapse = ", "), ". ",
          "The sum of values for (", paste(Names_, collapse = ", "),
          ") are off by more than 1%. They should add up to 1."
        )
        Errors_ <<- c(Errors_, Msg)
      }
      if (any(HasWarn_)) {
        WarnAzones_ <- V_df$Geo[HasWarn_]
        Msg <- paste0(
          "Warnings for input file '", File, "' for year '", yr,
          "' and the following Azones: ",
          paste(WarnAzones_, collapse = ", "), ". ",
          "The sum of values for (", paste(Names_, collapse = ", "),
          ") are not equal to 1 but are off by 1% or less. ",
          "They have been adjusted to add up to 1."
        )
        Warnings_ <<- c(Warnings_, Msg)
        for (az in WarnAzones_) {
          Values_ <- V_df[V_df$Geo == az, Names_]
          AdjValues_ <- Values_ / sum(Values_)
          Values_df[Values_df$Year == yr & Values_df$Geo == az, Names_] <-
            AdjValues_
        }
      }
    }
    as.list(Values_df[,Names_])
  }

  #Check and adjust household location type proportions
  #----------------------------------------------------
  Names_ <- c("PropMetroHh", "PropTownHh", "PropRuralHh")
  if (all(Names_ %in% names(Out_ls$Data$Year$Azone))) {
    Out_ls$Data$Year$Azone[Names_] <-
      checkProps(Names_, "Azone", "azone_hh_loc_type_prop.csv")
  } else {
    Msg <- paste0(
      "azone_hh_loc_type_prop.csv input file is present but not complete. ",
      "Not all the required fields are present. The required fields are: ",
      paste(Names_, collapse = ", ")
    )
    Errors_ <- c(Errors_, Msg)
  }
  rm(Names_)

  #Check and adjust worker location type proportions
  #-------------------------------------------------
  Names_ <- c("PropWkrInMetroJobs", "PropWkrInTownJobs", "PropWkrInRuralJobs")
  if (all(Names_ %in% names(Out_ls$Data$Year$Azone))) {
    Out_ls$Data$Year$Azone[Names_] <-
      checkProps(Names_, "Azone", "azone_wkr_loc_type_prop.csv")
  } else {
    Msg <- paste0(
      "azone_wkr_loc_type_prop.csv input file is present but not complete. ",
      "Not all the required fields are present. The required fields are: ",
      paste(Names_, collapse = ", ")
    )
    Errors_ <- c(Errors_, Msg)
  }
  rm(Names_)

  #Check that there are no metropolitan jobs or households in Marea "None"
  #-----------------------------------------------------------------------
  Yr <- unique(L$Data$Year$Azone$Year)
  for (yr in Yr) {
    IsYearAndNone <-
      L$Data$Year$Azone$Year == yr & L$Data$Year$Azone$Marea == "None"
    AzNone_ <- L$Data$Year$Azone$Geo[IsYearAndNone]
    MetroHhProp_ <- L$Data$Year$Azone$PropMetroHh[IsYearAndNone]
    MetroJobProp_ <- L$Data$Year$Azone$PropWkrInMetroJobs[IsYearAndNone]
    if (any(MetroHhProp_ != 0)) {
      ErrAzones_ <- AzNone_[which(MetroHhProp_ != 0)]
      Msg <- paste0(
        "Error in 'azone_hh_loc_type_prop.csv' input file for year ", yr, ". ",
        "The values for the 'PropMetroHh' field for one or more Azones ",
        "that are identified as being in Marea 'None' are not 0. ",
        "By definition, Marea 'None' means that it is not a metropolitan area. ",
        "Therefore, the value of 'PropMetroHh' must be 0. ",
        "Correct the entries for the following Azones: ",
        paste(ErrAzones_, collapse = ", ")
      )
      Errors_ <- c(Errors_, Msg)
    }
    if (any(MetroJobProp_ != 0)) {
      ErrAzones_ <- AzNone_[which(MetroJobProp_ != 0)]
      Msg <- paste0(
        "Error in 'azone_wkr_loc_type_prop.csv' input file for year ", yr, ". ",
        "The values for the 'PropWkrInMetroJobs' field for one or more Azones ",
        "that are identified as being in Marea 'None' are not 0. ",
        "By definition, Marea 'None' means that it is not a metropolitan area. ",
        "Therefore, the value of 'PropWkrInMetroJobs' must be 0. ",
        "Correct the entries for the following Azones: ",
        paste(ErrAzones_, collapse = ", ")
      )
      Errors_ <- c(Errors_, Msg)
    }
  }

  #Check that split of jobs among Azones in each Marea add to 1
  #------------------------------------------------------------
  AzonesByMarea_ls <- with(G$Geo_df, split(Azone, Marea))
  Ma <- names(AzonesByMarea_ls)
  Ma <- Ma[Ma != "None"]
  PropMetroJobs_df <-
    data.frame(Out_ls$Data$Year$Azone[c("Year", "Geo", "PropMetroJobs")])
  PropMetroJobs_df$Year <- as.character(PropMetroJobs_df$Year)
  Yr <- unique(PropMetroJobs_df$Year)
  for (yr in Yr) {
    for (ma in Ma) {
      Az <- AzonesByMarea_ls[[ma]]
      IsYearAndMarea <- with(PropMetroJobs_df, Year == yr & Geo %in% Az)
      PropMetroJobs_ <- with(PropMetroJobs_df, PropMetroJobs[IsYearAndMarea])
      names(PropMetroJobs_) <- with(PropMetroJobs_df, Geo[IsYearAndMarea])
      TotProp <- sum(PropMetroJobs_)
      Diff <- abs(1 - TotProp)
      if (Diff >= 0.01) {
        Msg <- paste0(
          "Error in input values for 'PropMetroJobs' for Marea ",
          ma, " and Year ", Year,
          ". The sum of values for Azones in the Marea is off by more than 1%. ",
          "They should add up to 1."
        )
        Errors_ <- c(Errors_, Msg)
      } else {
        if (Diff != 0) {
          Msg <- paste0(
            "Warning regarding input values for 'PropMetroJobs' for Marea ",
            ma, " and Year ", Year,
            ". The sum of values for Azones in the Marea do not add up to 1 ",
            "but are off by less than 1%. ",
            "They have been adjusted to add up to 1."
          )
          Warnings_ <- c(Warnings_, Msg)
          PropMetroJobs_ <- PropMetroJobs_ / sum(PropMetroJobs_)
          Out_ls$Data$Year$Azone$PropMetroJobs[IsYearAndMarea] <- PropMetroJobs_
        }
      }
      rm(Az, IsYearAndMarea, PropMetroJobs_, TotProp, Diff)
    }
    rm(yr)
  }
  rm(AzonesByMarea_ls, Ma, PropMetroJobs_df, Yr)

  #Check consistency of location type area and activity
  #----------------------------------------------------
  #Only check if no other errors identified
  if (length(Errors_) == 0) {
    #Calculate average density limits
    SimBzone_ls <- VESimLandUse::SimBzone_ls
    RuralActDenRng_ <- range(SimBzone_ls$RuProfiles$D1DGrp_ls$AveDensity)
    TownActDenRng_ <- range(SimBzone_ls$TnProfiles$D1DGrp_ls$AveDensity)
    UrbanActDenRng_ <- local({
      UrbanActDenRng_mx <-
        do.call(rbind, lapply(SimBzone_ls$UaProfiles$D1DGrp_Ua_ls, function(x) {
          x$AveDensity[c(1,20)]
        }))
      apply(UrbanActDenRng_mx, 2, function(x) max(x[!is.nan(x)]))
    })
    #Load the household model data and calculate default worker rates
    WkrProp_Ag <- local({
      Hh_df <- loadPackageDataset("Hh_df", DefaultPackage = "VESimHouseholds")
      Ag <- c("Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
      Wk <- c("Wkr15to19", "Wkr20to29", "Wkr30to54", "Wkr55to64", "Wkr65Plus")
      WkrProp_Ag <- colSums(Hh_df[,Wk]) / colSums(Hh_df[,Ag])
      names(WkrProp_Ag) <- Ag
      WkrProp_Ag
    })
    #Make data frame of population data
    Ag <- c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
    Gq <- c("GrpAge0to14", "GrpAge15to19", "GrpAge20to29", "GrpAge30to54", "GrpAge55to64", "GrpAge65Plus")
    PopData_df <- data.frame(L$Data$Year$Azone[c("Year", "Geo", Ag, Gq, "AveHhSize")])
    PopData_df$Year <- as.character(PopData_df$Year)
    Re <- c("RelEmp15to19", "RelEmp20to29", "RelEmp30to54", "RelEmp55to64", "RelEmp65Plus")
    for (re in Re) {
      if (!is.null(L$Data$Year$Azone[[re]])) {
        PopData_df[[re]] <- L$Data$Year$Azone[[re]]
      } else {
        PopData_df[[re]] <- 1
      }
    }
    #Iterate through years and check values
    Yr <- unique(L$Data$Year$Azone$Year)
    for (yr in Yr) {
      IsYear <- L$Data$Year$Azone$Year == yr
      #Estimate number of households
      HhPop_AzAg <- as.matrix(data.frame(L$Data$Year$Azone)[IsYear, Ag])
      rownames(HhPop_AzAg) <- L$Data$Year$Azone$Geo[IsYear]
      GqPop_AzAg <- as.matrix(data.frame(L$Data$Year$Azone)[IsYear, Gq])
      rownames(GqPop_AzAg) <- L$Data$Year$Azone$Geo[IsYear]
      AveHhSize_Az <- L$Data$Year$Azone$AveHhSize[IsYear]
      AveHhSize_Az[is.na(AveHhSize_Az)] <- 2.5
      NumHh_Az <- round(
        rowSums(HhPop_AzAg) / AveHhSize_Az + rowSums(GqPop_AzAg),
        0
      )
      #Estimate number of workers
      WkrPopAdj_AzRe <- as.matrix(data.frame(L$Data$Year$Azone)[IsYear, Re])
      rownames(WkrPopAdj_AzRe) <- L$Data$Year$Azone$Geo[IsYear]
      WkrProp_AzAg <- sweep(WkrPopAdj_AzRe, 2, WkrProp_Ag, "*")
      NumWkr_Az <- round(
        rowSums((HhPop_AzAg[,-1] + GqPop_AzAg[,-1]) * WkrProp_AzAg),
        0
      )
      #Allocate households to location types
      Hh_AzLt <- do.call(cbind, calcNumHhByLocType(
        NumHh_Az = NumHh_Az,
        PropRuralHh_Az = L$Data$Year$Azone$PropRuralHh[IsYear],
        PropTownHh_Az = L$Data$Year$Azone$PropTownHh[IsYear],
        PropMetroHh_Az = L$Data$Year$Azone$PropMetroHh[IsYear],
        Az = L$Data$Year$Azone$Geo[IsYear]
      ))
      #Allocate jobs to location types and Azones
      Jobs_AzLt <- do.call(cbind, calcNumJobsByLocType(
        NumWkr_Az = NumWkr_Az,
        PropWkrInRuralJobs_Az = L$Data$Year$Azone$PropWkrInRuralJobs[IsYear],
        PropWkrInTownJobs_Az = L$Data$Year$Azone$PropWkrInTownJobs[IsYear],
        PropWkrInMetroJobs_Az = L$Data$Year$Azone$PropWkrInMetroJobs[IsYear],
        PropMetroJobs_Az = L$Data$Year$Azone$PropMetroJobs[IsYear],
        TownJobWkrRatio_Az = L$Data$Year$Azone$TownJobWkrRatio[IsYear],
        Marea_Az = L$Data$Year$Azone$Marea[IsYear],
        Az =  L$Data$Year$Azone$Geo[IsYear]
      ))
      #Calculate total households and jobs by location type and Azone
      Activity_AzLt <- Hh_AzLt + Jobs_AzLt[,c("Rural", "Town", "Urban")]
      #Check that if there is activity in Azone LocType there is land area
      MetroLandArea_Az <- L$Data$Year$Azone$MetroLandArea[IsYear]
      attributes(MetroLandArea_Az) <- list(
        names = L$Data$Year$Azone$Geo[IsYear],
        UNITS = attributes(L$Data$Year$Azone$MetroLandArea)$UNITS
      )
      TownLandArea_Az <- L$Data$Year$Azone$TownLandArea[IsYear]
      attributes(TownLandArea_Az) <- list(
        names = L$Data$Year$Azone$Geo[IsYear],
        UNITS = attributes(L$Data$Year$Azone$TownLandArea)$UNITS
      )
      RuralAveDensity_Az <- L$Data$Year$Azone$RuralAveDensity[IsYear]
      attributes(RuralAveDensity_Az) <- list(
        names = L$Data$Year$Azone$Geo[IsYear],
        UNITS = attributes(L$Data$Year$Azone$RuralAveDensity)$UNITS
      )
      MetroWrong_Az <- Activity_AzLt[,"Urban"] != 0 & MetroLandArea_Az == 0
      TownWrong_Az <- Activity_AzLt[,"Town"] != 0 & TownLandArea_Az == 0
      RuralWrong_Az <- Activity_AzLt[,"Rural"] != 0 & RuralAveDensity_Az == 0
      if (any(MetroWrong_Az)) {
        ErrAzones_ <- names(MetroWrong_Az)[MetroWrong_Az]
        Msg <- paste0(
          "Error in 'azone_loc_type_land_area.csv' input file for year ", yr, ". ",
          "The following Azones are identified as having 0 value for 'MetroLandArea' ",
          "but will have households and/or jobs assigned to them based on the ",
          "values of 'PropMetroHh' in the 'azone_hh_loc_type_prop.csv' input file ",
          "and/or values of 'PropWkrInMetroJobs' and 'PropMetroJobs' in the ",
          "'azone_wkr_loc_type_prop.csv' input file: ",
          paste(ErrAzones_, collapse = ", ")
        )
        Errors_ <- c(Errors_, Msg)
        rm(ErrAzones_, Msg)
      }
      if (any(TownWrong_Az)) {
        ErrAzones_ <- names(TownWrong_Az)[TownWrong_Az]
        Msg <- paste0(
          "Error in 'azone_loc_type_land_area.csv' input file for year ", yr, ". ",
          "The following Azones are identified as having 0 value for 'TownLandArea' ",
          "but will have households and/or jobs assigned to them based on the ",
          "values of 'PropTownHh' in the 'azone_hh_loc_type_prop.csv' input file ",
          "and/or values of 'PropWkrInTownJobs' in the ",
          "'azone_wkr_loc_type_prop.csv' input file: ",
          paste(ErrAzones_, collapse = ", ")
        )
        Errors_ <- c(Errors_, Msg)
        rm(ErrAzones_, Msg)
      }
      if (any(RuralWrong_Az)) {
        ErrAzones_ <- names(RuralWrong_Az)[RuralWrong_Az]
        Msg <- paste0(
          "Error in 'azone_loc_type_land_area.csv' input file for year ", yr, ". ",
          "The following Azones are identified as having 0 value for 'RuralAveDensity' ",
          "but will have households and/or jobs assigned to them based on the ",
          "values of 'PropRuralHh' in the 'azone_hh_loc_type_prop.csv' input file ",
          "and/or values of 'PropWkrInRuralJobs' in the ",
          "'azone_wkr_loc_type_prop.csv' input file: ",
          paste(ErrAzones_, collapse = ", ")
        )
        Errors_ <- c(Errors_, Msg)
        rm(ErrAzones_, Msg)
      }
      #If there are no missing land area values, check reasonableness of density
      if (!any(MetroWrong_Az) & !any(TownWrong_Az) & !any(RuralWrong_Az)) {
        #Convert land area to acres and density to activity per acre if needed
        if (attributes(MetroLandArea_Az)$UNITS != "ACRE") {
          FromUnits <- attributes(MetroLandArea_Az)$UNITS
          MetroLandArea_Az <-
            convertUnits(MetroLandArea_Az, "area",FromUnits, "ACRE")$Values
        }
        if (attributes(TownLandArea_Az)$UNITS != "ACRE") {
          FromUnits <- attributes(TownLandArea_Az)$UNITS
          TownLandArea_Az <-
            convertUnits(TownLandArea_Az, "area",FromUnits, "ACRE")$Values
        }
        if (attributes(RuralAveDensity_Az)$UNITS != "HHJOB/ACRE") {
          FromUnits <- attributes(RuralAveDensity_Az)$UNITS
          RuralAveDensity_Az <-
            convertUnits(RuralAveDensity_Az, "compound", FromUnits, "HHJOB/ACRE")$Values
        }
        #Check values for metropolitan average density
        IsMetro_ <- L$Data$Year$Azone$Marea[IsYear] != "None"
        names(IsMetro_) <- L$Data$Year$Azone$Geo[IsYear]
        MetroActDen_Az <- Activity_AzLt[IsMetro_,"Urban"] / MetroLandArea_Az[IsMetro_]
        IsLowDen_Az <- MetroActDen_Az < UrbanActDenRng_[1]
        IsLowDen_Az[is.nan(MetroActDen_Az)] <- FALSE
        IsHighDen_Az <- MetroActDen_Az > UrbanActDenRng_[2]
        IsHighDen_Az[is.nan(MetroActDen_Az)] <- FALSE
        if (any(IsLowDen_Az)) {
          ErrAzones_ <- names(IsLowDen_Az)[IsLowDen_Az]
          Msg <- paste0(
            "Metropolitan portions of the following Azones for year ", yr,
            " are estimated to have very low average activity densities: ",
            paste(ErrAzones_, collapse = ", "),
            "The respective average activity densities are: ",
            paste(MetroActDen_Az[ErrAzones_], collapse = ", "),
            "The lowest allowed Bzone activity density is ",
            round(UrbanActDenRng_[1], 3)," households and jobs per acre. ",
            "Modify relevant values in the 'azone_hh_loc_type_prop.csv', ",
            "'azone_wkr_loc_type_prop.csv', and/or 'azone_loc_type_land_area.csv' ",
            "files to correct the error."
          )
          Errors_ <- c(Errors_, Msg)
          rm(ErrAzones_, Msg)
        }
        if (any(IsHighDen_Az)) {
          ErrAzones_ <- names(IsHighDen_Az)[IsHighDen_Az]
          Msg <- paste0(
            "Metropolitan portions of the following Azones for year ", yr,
            " are estimated to have very high average activity densities: ",
            paste(ErrAzones_, collapse = ", "),
            "The respective average activity densities are: ",
            paste(MetroActDen_Az[ErrAzones_], collapse = ", "),
            "The highest allowed Bzone activity density is ",
            round(UrbanActDenRng_[2])," households and jobs per acre. ",
            "Modify relevant values in the 'azone_hh_loc_type_prop.csv', ",
            "'azone_wkr_loc_type_prop.csv', and/or 'azone_loc_type_land_area.csv' ",
            "files to correct the error."
          )
          Errors_ <- c(Errors_, Msg)
          rm(ErrAzones_, Msg)
        }
        rm(IsMetro_, MetroActDen_Az, IsLowDen_Az, IsHighDen_Az)
        #Check values for town average density
        IsTown_ <- Activity_AzLt[,"Town"] != 0
        TownActDen_Az <- Activity_AzLt[IsTown_,"Town"] / TownLandArea_Az[IsTown_]
        IsLowDen_Az <- TownActDen_Az < TownActDenRng_[1]
        IsHighDen_Az <- TownActDen_Az > TownActDenRng_[2]
        if (any(IsLowDen_Az)) {
          ErrAzones_ <- names(IsLowDen_Az)[IsLowDen_Az]
          Msg <- paste0(
            "Town portions of the following Azones for year ", yr,
            " are estimated to have very low average activity densities: ",
            paste(ErrAzones_, collapse = ", "),
            "The respective average activity densities are: ",
            paste(TownActDen_Az[ErrAzones_], collapse = ", "),
            "The lowest allowed Bzone activity density is ",
            round(TownActDenRng_[1], 3)," households and jobs per acre. ",
            "Modify relevant values in the 'azone_hh_loc_type_prop.csv', ",
            "'azone_wkr_loc_type_prop.csv', and/or 'azone_loc_type_land_area.csv' ",
            "files to correct the error."
          )
          Errors_ <- c(Errors_, Msg)
          rm(ErrAzones_, Msg)
        }
        if (any(IsHighDen_Az)) {
          ErrAzones_ <- names(IsHighDen_Az)[IsHighDen_Az]
          Msg <- paste0(
            "Town portions of the following Azones for year ", yr,
            " are estimated to have very high average activity densities: ",
            paste(ErrAzones_, collapse = ", "),
            "The respective average activity densities are: ",
            paste(TownActDen_Az[ErrAzones_], collapse = ", "),
            "The highest allowed Bzone activity density is ",
            round(TownActDenRng_[2])," households and jobs per acre. ",
            "Modify relevant values in the 'azone_hh_loc_type_prop.csv', ",
            "'azone_wkr_loc_type_prop.csv', and/or 'azone_loc_type_land_area.csv' ",
            "files to correct the error."
          )
          Errors_ <- c(Errors_, Msg)
          rm(ErrAzones_, Msg)
        }
        rm(IsTown_, TownActDen_Az, IsLowDen_Az, IsHighDen_Az)
        #Check values for rural average density
        IsRural_ <- Activity_AzLt[,"Rural"] != 0
        RuralActDen_Az <- RuralAveDensity_Az[IsRural_]
        IsLowDen_Az <- RuralActDen_Az < RuralActDenRng_[1]
        IsHighDen_Az <- RuralActDen_Az > RuralActDenRng_[2]
        if (any(IsLowDen_Az)) {
          ErrAzones_ <- names(IsLowDen_Az)[IsLowDen_Az]
          Msg <- paste0(
            "Rural portions of the following Azones for year ", yr,
            " are estimated to have very low average activity densities: ",
            paste(ErrAzones_, collapse = ", "),
            "The respective average activity densities are: ",
            paste(RuralActDen_Az[ErrAzones_], collapse = ", "),
            "The lowest allowed Bzone activity density is ",
            round(RuralActDenRng_[1], 3)," households and jobs per acre. ",
            "Modify relevant values in the 'azone_hh_loc_type_prop.csv', ",
            "'azone_wkr_loc_type_prop.csv', and/or 'azone_loc_type_land_area.csv' ",
            "files to correct the error."
          )
          Errors_ <- c(Errors_, Msg)
          rm(ErrAzones_, Msg)
        }
        if (any(IsHighDen_Az)) {
          ErrAzones_ <- names(IsHighDen_Az)[IsHighDen_Az]
          Msg <- paste0(
            "Rural portions of the following Azones for year ", yr,
            " are estimated to have very high average activity densities: ",
            paste(ErrAzones_, collapse = ", "),
            "The respective average activity densities are: ",
            paste(RuralActDen_Az[ErrAzones_], collapse = ", "),
            "The highest allowed Bzone activity density is ",
            round(RuralActDenRng_[2])," households and jobs per acre. ",
            "Modify relevant values in the 'azone_hh_loc_type_prop.csv', ",
            "'azone_wkr_loc_type_prop.csv', and/or 'azone_loc_type_land_area.csv' ",
            "files to correct the error."
          )
          Errors_ <- c(Errors_, Msg)
          rm(ErrAzones_, Msg)
        }
        rm(IsRural_, RuralActDen_Az, IsLowDen_Az, IsHighDen_Az)
      }
    }
  }

  #Check each assigned UzaProfileName for consistency
  #--------------------------------------------------
  UzaProfileNames_ls <- VESimLandUse::UzaProfileNames_ls
  UzaNames_ <- UzaProfileNames_ls$Names
  Marea_ <- L$Data$Global$Marea$Geo
  UzaProfileName_ <- L$Data$Global$Marea$UzaProfileName
  for (i in 1:length(Marea_)) {
    Marea <- Marea_[i]
    UzaProfileName <- UzaProfileName_[i]
    if (Marea != "None") {
      if (!(UzaProfileName %in% UzaNames_)) {
        ErrMsg <- paste0(
          "The urbanized area profile name - ", UzaProfileName,
          " - assigned to Marea - ", Marea, "does not exist. ",
          "Read the documentation for this (Initialize) module to see a list."
        )
        Errors_ <- c(Errors_, ErrMsg)
      }
    }
  }

  #Check and adjust group quarters area type proportions
  #-----------------------------------------------------
  Names_ <- c(
    "PropGQPopCenter",
    "PropGQPopInner",
    "PropGQPopOuter",
    "PropGQPopFringe")
  if (any(Names_ %in% names(Out_ls$Data$Year$Azone))) {
    if (all(Names_ %in% names(Out_ls$Data$Year$Azone))) {
      if (all(is.na(unlist(Out_ls$Data$Year$Azone[Names_])))) {
        Out_ls$Data$Year$Azone[Names_] <- NULL
      } else {
        Out_ls$Data$Year$Azone[Names_] <- checkProps(Names_, "Azone", "azone_gq_pop-prop_by_area-type.csv")
      }
    } else {
      Msg <- paste0(
        "azone_gq_pop-prop_by_area-type.csv input file is present but not complete"
      )
      Errors_ <- c(Errors_, Msg)
    }
  }
  rm(Names_)

  #Add Errors and Warnings to Out_ls and return
  #--------------------------------------------
  Out_ls$Errors <- Errors_
  Out_ls$Warnings <- Warnings_
  Out_ls
}


#===============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
#===============================================================
#Run module automatic documentation
#----------------------------------
documentModule("Initialize")

#Test code to perform additional checks on input files. Return input list
#(TestDat_) to use for developing the Initialize function.
#-------------------------------------------------------------------------------
# source("tests/scripts/test_functions.R")
# #Set up test data
# setUpTests(list(
#   TestDataRepo = "../Test_Data/VE-State",
#   DatastoreName = "Datastore.tar",
#   LoadDatastore = TRUE,
#   TestDocsDir = "vestate",
#   ClearLogs = TRUE
# ))
# #Return test dataset
# TestDat_ <- testModule(
#   ModuleName = "Initialize",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = FALSE
# )
# L <- TestDat_
# R <- Initialize(TestDat_)

#Test code to check everything including running the module and checking whether
#the code runs completely and produces desired results
#-------------------------------------------------------------------------------
# source("tests/scripts/test_functions.R")
#Set up test data
# setUpTests(list(
#   TestDataRepo = "../Test_Data/VE-State",
#   DatastoreName = "Datastore.tar",
#   LoadDatastore = TRUE,
#   TestDocsDir = "vestate",
#   ClearLogs = TRUE
# ))
# TestDat_ <- testModule(
#   ModuleName = "Initialize",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
