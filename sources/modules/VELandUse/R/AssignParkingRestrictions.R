#===========================
#AssignParkingRestrictions.R
#===========================
#
#<doc>
#
## AssignParkingRestrictions Module
#### February 13, 2020
#
#This module identifies parking restrictions and prices affecting households at their residences, workplaces, and other places they are likely to visit in the urban area. The module takes user inputs on parking restrictions and prices by Bzone and calculates for each household the number of free parking spaces available at the household's residence, which workers pay for parking and whether their payment is part of a *cash-out-buy-back* program, the cost of residential parking for household vehicles that can't be parked in a free space, the cost for workplace parking, and the cost of parking for other activities such as shopping. The parking restriction/cost information is used by other modules in calculating the cost of vehicle ownership and the cost of vehicle use.
#
### Model Parameter Estimation
#
#This module has no estimated parameters.
#
### How the Module Works
#
#The user provides inputs by Bzone which provide the basis for calculating
#parking restrictions/costs. These include:
#
#- Average number of free parking spaces per single-family dwelling unit
#
#- Average number of free parking spaces per multifamily dwelling unit
#
#- Average number of free parking spaces per group quarters resident
#
#- Proportion of non-work trips to the Bzone which pay for parking
#
#- Proportion of workers working at jobs in the Bzone who pay for parking
#
#- Proportion of worker paid parking in *cash-out_buy-back* program
#
#- Average daily parking cost
#
#Residential Bzone parking restrictions are applied to each household based on the Bzone and dwelling type of the household and the supplied inputs on the average number of parking spaces per household by Bzone and dwelling type. If the average number of parking spaces is not an integer, the household is assigned the integer amount of spaces and a possible additional space determined through a random draw with the decimal portion serving as the probability of success. For example, if the average is 1.75 spaces, all households would be assigned at least 1 space and 75% of the households would be assigned 2 spaces. The daily parking cost assigned to the Bzone is assigned to the household to use in vehicle ownership cost calculations.
#
#A worker is assigned as paying or not paying for parking through a random draw with the probability of paying equal to the proportion of paying workers that is input for the Bzone of the worker's job location. A worker identified as paying for parking is identified as being in a *cash-out-buy-back* program through a random draw with the participation probability being the input value for the Bzone of the worker's job location. The daily parking cost assigned to the worker's job site Bzone is assigned to the work to use in vehicle use calculations.
#
#Other household parking costs (e.g. shopping) are assigned to households based on the daily parking cost assigned to each Bzone and the assumption that the likelihood that a household would visit the Bzone is directly proportional to the relative number of activities in the Bzone and the inverse of the distance to the Bzone from the household residence Bzone. The activity in the Bzone is measured with the total number of retail and service jobs in the Bzone. As with the LocateEmployment and Calculate4DMeasures modules, a centroid-to-centroid distance matrix is calculated from user supplied data on the latitude and longitude of each Bzone centroid. Next, the number of Bzone attractions is scaled to equal the number of households. Then an iterative proportional fitting process (IPF) is used to allocate households to attractions where the margins are the numbers of households by Bzone and the scaled attractions by Bzone, and the seed matrix is the inverse of the values of the distance matrix. After a balanced matrix has been computed, the proportions of attractions from each residence Bzone to each attraction Bzone is calculated such that the total for each residence Bzone adds to 1. Finally, the average daily parking cost for residents of a Bzone is the sum of the product of the attraction proportion to each Bzone, the daily parking cost in each Bzone, and the proportion of non-work trips to the Bzone that pay for parking. Households are assigned the cost calculated for their Bzone of residence. This cost is adjusted to account for the number of household vehicle trips when the household's vehicle use costs are calculated.
#
#</doc>


#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================
#This module has no model parameters.


#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
AssignParkingRestrictionsSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify new tables to be created by Inp if any
  #Specify new tables to be created by Set if any
  #Specify input data
  Inp = items(
    item(
      NAME =
        items(
          "PkgSpacesPerSFDU",
          "PkgSpacesPerMFDU",
          "PkgSpacesPerGQ"),
      FILE = "bzone_parking.csv",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "parking spaces",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        items(
          "Average number of free parking spaces available to residents of single-family dwelling units",
          "Average number of free parking spaces available to residents of multifamily dwelling units",
          "Average number of free parking spaces available to group quarters residents"
        )
    ),
    item(
      NAME =
        items(
          "PropWkrPay",
          "PropCashOut",
          "PropNonWrkTripPay"),
      FILE = "bzone_parking.csv",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0", "> 1"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        items(
          "Proportion of workers who pay for parking",
          "Proportions of workers paying for parking in a cash-out-buy-back program",
          "Proportion of non-work trips who pay for parking"
        )
    ),
    item(
      NAME = "PkgCost",
      FILE = "bzone_parking.csv",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION = "Average daily cost for long-term parking (e.g. paid on monthly basis)"
    )
  ),
  #Specify data to be loaded from data store
  Get = items(
    item(
      NAME = "Bzone",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    item(
      NAME =
        items(
          "PkgSpacesPerSFDU",
          "PkgSpacesPerMFDU",
          "PkgSpacesPerGQ"),
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "parking spaces",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME =
        items(
          "PropWkrPay",
          "PropCashOut",
          "PropNonWrkTripPay"),
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "proportion",
      PROHIBIT = c("NA", "< 0", "> 1"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = "PkgCost",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = "NumHh",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "households",
      UNITS = "HH",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = "RetEmp",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = "SvcEmp",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME =
        items(
          "Latitude",
          "Longitude"),
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "NA",
      NAVALUE = -9999,
      PROHIBIT = "NA",
      ISELEMENTOF = ""
    ),
    item(
      NAME = "HouseType",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      PROHIBIT = "",
      ISELEMENTOF = c("SF", "MF", "GQ")
    ),
    item(
      NAME = "Bzone",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    item(
      NAME = "Bzone",
      TABLE = "Worker",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    )
  ),
  #Specify data to saved in the data store
  Set = items(
    item(
      NAME = "FreeParkingSpaces",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      UNITS = "parking spaces",
      NAVALUE = "NA",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = "Number of free parking spaces available to the household"
    ),
    item(
      NAME = "ParkingUnitCost",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD",
      NAVALUE = "NA",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = "Daily cost for long-term parking (e.g. paid on monthly basis)"
    ),
    item(
      NAME = "OtherParkingCost",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD",
      NAVALUE = "NA",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = "Daily cost for parking at shopping locations or other locations of paid parking not including work (not adjusted for number of vehicle trips)"
    ),
    item(
      NAME = "PaysForParking",
      TABLE = "Worker",
      GROUP = "Year",
      TYPE = "integer",
      UNITS = "binary",
      NAVALUE = "NA",
      PROHIBIT = "",
      ISELEMENTOF = c(0, 1),
      SIZE = 0,
      DESCRIPTION = "Does worker pay for parking: 1 = yes, 0 = no"
    ),
    item(
      NAME = "IsCashOut",
      TABLE = "Worker",
      GROUP = "Year",
      TYPE = "integer",
      UNITS = "binary",
      NAVALUE = "NA",
      PROHIBIT = "",
      ISELEMENTOF = c(0, 1),
      SIZE = 0,
      DESCRIPTION = "Is worker paid parking in cash-out-buy-back program: 1 = yes, 0 = no"
    ),
    item(
      NAME = "ParkingCost",
      TABLE = "Worker",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD",
      NAVALUE = "NA",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = "Daily cost for long-term parking (e.g. paid on monthly basis)"
    )
  )
)

#Save the data specifications list
#---------------------------------
#' Specifications list for AssignParkingRestrictions module
#'
#' A list containing specifications for the AssignParkingRestrictions module.
#'
#' @format A list containing 4 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Inp}{scenario input data to be loaded into the datastore for this
#'  module}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
#' @source AssignParkingRestrictions.R script.
"AssignParkingRestrictionsSpecifications"
visioneval::savePackageDataset(AssignParkingRestrictionsSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#This function assigns parking restrictions to households. This includes
#restrictions on parking household vehicles and restrictions on parking vehicles
#at the household workers places of work. The function identifies the number of
#free parking spaces the household may use, the charge for household parking per
#vehicle if they exceed the number of free spaces, how many household workers
#pay for parking, the charge they have to pay, and the amount of the work
#parking charges that are 'cash-out-buy-back'. The module also calculates a
#simple placeholder value for other daily parking charges (e.g. paying for
#parking at shopping). These are calculated as a weighted average of daily
#parking cost in each Bzone weighted by the portion of total aggregate activity
#in the region that is in each Bzone (D1D measure calculated by the
#Calculate4DMeasures module)

#Main module function that assigns parking restrictions to each household
#------------------------------------------------------------------------
#' Main module function to assign parking restrictions to each household.
#'
#' \code{AssignParkingRestrictions} assigns parking restrictions to each
#' household including residential parking restrictions and worker parking
#' restrictions.
#'
#' This function assigns parking restrictions to households. This includes
#' restrictions on parking household vehicles and restrictions on parking
#' vehicles at the household workers places of work. The function identifies the
#' number of free parking spaces the household may use, the charge for household
#' parking per vehicle if they exceed the number of free spaces, how many
#' household workers pay for parking, the charge they have to pay, and the
#' amount of the work parking charges that are 'cash-out-buy-back'.
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @name AssignParkingRestrictions
#' @import visioneval stats fields
#' @export
AssignParkingRestrictions <- function(L) {
  #Set up
  #------
  #Fix seed as synthesis involves sampling
  set.seed(L$G$Seed)
  BzToHh_ <- match(L$Year$Household$Bzone, L$Year$Bzone$Bzone)
  BzToWk_ <- match(L$Year$Worker$Bzone, L$Year$Bzone$Bzone)
  NumHh <- length(BzToHh_)
  NumWkr <- length(BzToWk_)

  #Calculate the number of number of residential parking spaces and cost
  #---------------------------------------------------------------------
  #Calculate number of free parking spaces available to each household
  PkgSp_Hh <- local({
    HouseType_Hh <- L$Year$Household$HouseType
    AvePkgSp_Hh <- numeric(NumHh)
    AvePkgSp_Hh[HouseType_Hh == "SF"] <-
      L$Year$Bzone$PkgSpacesPerSFDU[BzToHh_][HouseType_Hh == "SF"]
    AvePkgSp_Hh[HouseType_Hh == "MF"] <-
      L$Year$Bzone$PkgSpacesPerMFDU[BzToHh_][HouseType_Hh == "MF"]
    AvePkgSp_Hh[HouseType_Hh == "GQ"] <-
      L$Year$Bzone$PkgSpacesPerGQ[BzToHh_][HouseType_Hh == "GQ"]
    BasePkgSp_Hh <- floor(AvePkgSp_Hh)
    AddSpProb_Hh <- AvePkgSp_Hh - BasePkgSp_Hh
    BasePkgSp_Hh + as.numeric(runif(NumHh) < AddSpProb_Hh)
  })
  #Identify how much household would have to pay for a parking space
  CostPerSpace_Hh <- L$Year$Bzone$PkgCost[BzToHh_]

  #Calculate parking costs for workers
  #-----------------------------------
  #Identify which workers pay for parking
  PropPay_Wk <- L$Year$Bzone$PropWkrPay[BzToWk_]
  DoesPay_Wk <- runif(NumWkr) < PropPay_Wk
  #Identify which workers whose parking is cash out buy back
  PropCashOut_Wk <- L$Year$Bzone$PropCashOut[BzToWk_]
  IsCashOut_Wk <- (runif(NumWkr) < PropCashOut_Wk) & DoesPay_Wk
  #Identify the cost per space
  CostPerSpace_Wk <- L$Year$Bzone$PkgCost[BzToWk_]
  CostPerSpace_Wk[!DoesPay_Wk] <- 0
  #Clean up
  rm(PropPay_Wk, PropCashOut_Wk)

  #Other household parking cost
  #----------------------------
  #Calculated as function of inverse of distance to attractions from home and
  #amount of retail and service employment
  OtherPkgCost_Bz <- local({
    #Calculate distances between Bzones
    LngLat_df <-
      data.frame(
        lng = L$Year$Bzone$Longitude,
        lat = L$Year$Bzone$Latitude)
    Dist_BzBz <- rdist.earth(LngLat_df, LngLat_df, miles = TRUE, R = 6371)
    diag(Dist_BzBz) <- 0
    diag(Dist_BzBz) <- apply(Dist_BzBz, 1, function(x) min(x[x != 0]) / 2)
    #Create attraction term to determine relative attractiveness to non-work trips
    Attr_Bz <- L$Year$Bzone$RetEmp + L$Year$Bzone$SvcEmp
    Attr_Bz[Attr_Bz == 0] <- 1
    #Create production term
    NumHh_Bz <- L$Year$Bzone$NumHh
    #Scale relative attractions to equal number of households
    Attr_Bz <- sum(NumHh_Bz) * Attr_Bz / sum(Attr_Bz)
    #Calculate relative attractiveness
    Attr_BzBz <-
      ipf(1 / Dist_BzBz, list(NumHh_Bz, Attr_Bz), list(1, 2))$Units_ar
    #Calculate attraction probabilities
    AttrProb_BzBz <- sweep(Attr_BzBz, 1, rowSums(Attr_BzBz), "/")
    #Calculate the weighted parking cost
    NonWorkPkgCost_Bz <- with(L$Year$Bzone, PkgCost * PropNonWrkTripPay)
    rowSums(sweep(AttrProb_BzBz, 2, NonWorkPkgCost_Bz, "*"))
  })
  #Assign other parking cost to households
  OtherPkgCost_Hh <- OtherPkgCost_Bz[BzToHh_]

  #Return list of results
  #----------------------
  Out_ls <- initDataList()
  Out_ls$Year$Household <- list(
    FreeParkingSpaces = as.integer(PkgSp_Hh),
    ParkingUnitCost = CostPerSpace_Hh,
    OtherParkingCost = OtherPkgCost_Hh
  )
  Out_ls$Year$Worker <- list(
    PaysForParking = as.integer(DoesPay_Wk),
    IsCashOut = as.integer(IsCashOut_Wk),
    ParkingCost = CostPerSpace_Wk
  )
  Out_ls
}

#===============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
#===============================================================
#Run module automatic documentation
#----------------------------------
documentModule("AssignParkingRestrictions")

#Test code to check specifications, loading inputs, and whether datastore
#contains data needed to run module. Return input list (L) to use for developing
#module functions
#-------------------------------------------------------------------------------
# #Load packages and test functions
# library(filesstrings)
# library(visioneval)
# library(fields)
# source("tests/scripts/test_functions.R")
# #Set up test environment
# TestSetup_ls <- list(
#   TestDataRepo = "../Test_Data/VE-RSPM",
#   DatastoreName = "Datastore.tar",
#   LoadDatastore = TRUE,
#   TestDocsDir = "verspm",
#   ClearLogs = TRUE,
#   # SaveDatastore = TRUE
#   SaveDatastore = FALSE
# )
# setUpTests(TestSetup_ls)
# #Run test module
# TestDat_ <- testModule(
#   ModuleName = "AssignParkingRestrictions",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = FALSE
# )
# L <- TestDat_$L
# R <- AssignParkingRestrictions(L)
