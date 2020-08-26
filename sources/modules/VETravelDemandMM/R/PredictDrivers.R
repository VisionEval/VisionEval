#==========================
#PredictDrivers.R
#==========================
#
#<doc>
#
## PredictDrivers Module
#### January 4, 2019
#
#This module predicts number of licensed drivers for households. It uses the model object in data/DriversModel_df.rda (ordered logit model by default) and variables and coefficients therein to predict Drivers.
#
### Model Parameter Estimation
#
#See data-raw/DriversModel_df.R.
#
### How the Module Works
#
#The user specifies the model in data-raw/DriversModel_df.R and saves the estimation results in data/DriversModel_df.rda. If no model re-estimation is desired, the estimation process can be skipped. The module assigns Drivers to each household using household characteristics.
#
#</doc>

#=================================
#Packages used in code development
#=================================
#Uncomment following lines during code development. Recomment when done.
# library(visioneval)


#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================
# See data-raw/DriversModel_df.R

#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
PredictDriversSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify data to be loaded from data store
  Get = visioneval::items(
    visioneval::item(
      NAME =
        items("Age0to14",
              "HhSize",
              "Workers"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "Income",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD.2009",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "Vehicles",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "vehicles",
      UNITS = "VEH",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "LifeCycle",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      NAVALUE = -1,
      PROHIBIT = "",
      ISELEMENTOF = c("00", "01", "02", "03", "04", "09", "10"),
      SIZE = 2
    ),
    visioneval::item(
      NAME = "HhId",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "LocType",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      NAVALUE = "NA",
      PROHIBIT = "NA",
      ISELEMENTOF = c("Urban", "Town", "Rural"),
      SIZE = 5,
      DESCRIPTION = "Location type (Urban, Town, Rural) of the place where the household resides"
    ),
    visioneval::item(
      NAME = "Azone",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    )
  ),
  #Specify data to saved in the data store
  Set = visioneval::items(
    visioneval::item(
      NAME = "Drivers",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = "Number of drivers in the household"
    )
  )
)

#Save the data specifications list
#---------------------------------
#' Specifications list for PredictDrivers module
#'
#' A list containing specifications for the PredictDrivers module.
#'
#' @format A list containing 3 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
"PredictDriversSpecifications"
visioneval::savePackageDataset(PredictDriversSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#This function predicts the number of Drivers in each
#household and tallies the total number of Drivers in the household. It uses
#the model object in data/DriversModel_df.rda.

#Main module function that predicts Drivers by age for each household
#--------------------------------------------------------------------
#' Main module function to predict Drivers for each household
#'
#' \code{PredictDrivers} predicts the number of Drivers for each
#' household and tallies the total number of Drivers for each household. It uses
#' the model specifications in the model object in data/DriversModel_df.rda.
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @import visioneval
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom MASS polr
#' @export
PredictDrivers <- function(L) {
  
   ## change the old nest and unnest function to be compatible with new tidyr
  nest <- nest_legacy
  unnest <- unnest_legacy
  
  dataset_name <- "Household"
  id_name <- "HhId"
  y_name <- "Drivers"

  D_df <- data.frame(L$Year[[dataset_name]])
  stopifnot("data.frame" %in% class(D_df))

  D_df <- D_df %>%
    mutate(metro=ifelse(LocType=="Urban", "metro", "non_metro"),
           LogIncome=log1p(Income),
           DrvAgePop=HhSize - Age0to14,
           LifeCycle = as.character(LifeCycle),
           LifeCycle = ifelse(LifeCycle=="01", "Single", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("02"), "Couple w/o children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("00", "03", "04", "05", "06", "07", "08"), "Couple w/ children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("09", "10"), "Empty Nester", LifeCycle)
           )

  #load("data/DriversModel_df.rda")
  Preds_lcdf <- DriversModel_df

  Preds_lcdf$data <- list(D_df)

  Preds_lcdf <- Preds_lcdf %>%
    mutate(y = map2(model, data, ~predict(.x, .y)))

  # call post_func(y) if post_func column exists
  if ("post_func" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      mutate(y=map2(y, post_func, `.y(.x)`))
  }

  if ("bias_adj" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      mutate(y=map2(y, bias_adj, `*`))
  }

  Preds_df <- Preds_lcdf %>%
    mutate(id=map(data, id_name)) %>%
    unnest(id, y)

  Out_ls <- initDataList()
  Out_ls$Year$Household <-
    list(
      Drivers = -1
    )
  Out_ls$Year$Household$Drivers <- Preds_df[["y"]]

  #Return the list
  Out_ls
}

#===============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
#===============================================================
#Run module automatic documentation
#----------------------------------
visioneval::documentModule("PredictDrivers")

#====================
#SECTION 5: TEST CODE
#====================
# model test code is in tests/scripts/test.R
