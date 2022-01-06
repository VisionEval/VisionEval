#==========================
#CalculateHouseholdDvmt.R
#==========================
#
#<doc>
#
## CalculateHouseholdDvmt Module
#### January 4, 2019
#
#This module predicts AADVMT for households. It uses the model object in data/AADVMTModel_df.rda and variables and coefficients therein to predict AADVMT.
#
### Model Parameter Estimation
#
#See data-raw/AADVMTModel_df.R.
#
### How the Module Works
#
#The user specifies the model in data-raw/AADVMTModel_df.R and saves the estimation results in data/AADVMTModel_df.rda. If no model re-estimation is desired, the estimation process can be skipped. The module assigns AADVMT to each household using variables including household characteristics, built environment, and transportation supply.
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
#See data-raw/AADVMTModel_df.R






#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
CalculateHouseholdDvmtSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  ##Specify input data
  Inp = visioneval::items(
    visioneval::item(
      NAME = "CENSUS_R",
      FILE = "marea_census_r.csv",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      PROHIBIT = "",
      ISELEMENTOF = c("NE", "S", "W", "MW"),
      SIZE = 2,
      UNLIKELY = "",
      DESCRIPTION = "CENSUS_R"       
    )
  ),
  #   visioneval::item(
  #     NAME = "metro",
  #     FILE = "marea_metro.csv",
  #     TABLE = "Marea",
  #     GROUP = "Year",
  #     TYPE = "character",
  #     UNITS = "category",
  #     PROHIBIT = "",
  #     ISELEMENTOF = c("metro", "non_metro"),
  #     SIZE = 9
  #   )
  # ),
  #Specify data to be loaded from data store
  Get = visioneval::items(
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
      NAME =
        items("HhSize",
              "Workers",
              "Drivers",
              "Age0to14",
              "Age65Plus"),
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
      NAME = "Bzone",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "none",
      NAVALUE = "NA",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "Bzone",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "none",
      NAVALUE = "NA",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "D1B",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "PRSN/SQM",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "D2A_WRKEMP",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "PRSN/JOB",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "D2A_EPHHM",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "employment & household entropy",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "D3bpo4",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "pedestrian-oriented intersections per square mile",
      NAVALUE = -9999,
      SIZE = 0,
      PROHIBIT = "NA",
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "D4c",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "aggregate peak period transit service",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "Marea",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "none",
      NAVALUE = "NA",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    visioneval::item(
      NAME = "CENSUS_R",
      #FILE = "marea_census_r.csv",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      PROHIBIT = "",
      ISELEMENTOF = c("NE", "S", "W", "MW"),
      SIZE = 2
    ),
    visioneval::item(
      NAME = "FwyLaneMiPC",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/PRSN",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "TranRevMiPC",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/PRSN",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    visioneval::item(
      NAME = "Marea",
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
      PROHIBIT = "NA",
      ISELEMENTOF = c("Urban", "Town", "Rural")
    )

  ),
  
  #Specify data to saved in the data store
  Set = visioneval::items(
    visioneval::item(
      NAME = "Dvmt",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/DAY",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = "Average daily vehicle miles traveled by the household in autos or light trucks"
    ),
    visioneval::item(
      NAME = items(
        "UrbanHhDvmt",
        "TownHhDvmt",
        "RuralHhDvmt"),
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/DAY",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = items(
        "Average daily vehicle miles traveled in autos or light trucks by households residing in the urbanized portion of the Marea",
        "Average daily vehicle miles traveled in autos or light trucks by households residing in town (urban but not urbanized) portion of the Marea",
        "Average daily vehicle miles traveled in autos or light trucks by households residing in the rural (non-urban) portion of the Marea")

  )
)
)

#Save the data specifications list
#---------------------------------
#' Specifications list for CalculateHouseholdDvmt module
#'
#' A list containing specifications for the CalculateHouseholdDvmt module.
#'
#' @format A list containing 4 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Inp}{scenario input data to be loaded into the datastore for this
#'  module}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
"CalculateHouseholdDvmtSpecifications"
visioneval::savePackageDataset(CalculateHouseholdDvmtSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================

#Main module function that predicts AADVMT for households
#------------------------------------------------------
#' Main module function
#'
#' \code{CalculateHouseholdDvmt} predicts AADVMT for each household in the households
#' dataset using independent variables including household characteristics
#' and 5D built environment variables.
#'
#' This function predicts AADVMT for each hosuehold in the model region where
#' each household is assigned an AADVMT. The model objects as a part of the
#' inputs are stored in data frame with two columns: a column for segmentation
#' (e.g., metro, non-metro) and a 'model' column for model object (list-column
#' data structure). The function "nests" the households data frame into a
#' list-column data frame by segments and applies the generic predict() function
#' for each segment to predict AADVMT for each household. The vectors of HhId
#' and AADVMT produced by the CalculateHouseholdDvmt function are to be stored in the
#' "Household" table.
#'
#' If this table does not exist, the function calculates a LENGTH value for
#' the table and returns that as well. The framework uses this information to
#' initialize the Households table. The function also computes the maximum
#' numbers of characters in the HhId and Azone datasets and assigns these to a
#' SIZE vector. This is necessary so that the framework can initialize these
#' datasets in the datastore. All the results are returned in a list.
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module along with:
#' LENGTH: A named integer vector having a single named element, "Household",
#' which identifies the length (number of rows) of the Household table to be
#' created in the datastore.
#' SIZE: A named integer vector having two elements. The first element, "Azone",
#' identifies the size of the longest Azone name. The second element, "HhId",
#' identifies the size of the longest HhId.
#' @import visioneval dplyr purrr tidyr pscl
#' @importFrom splines ns
#' @export
CalculateHouseholdDvmt <- function(L) {
  #TODO: get id_name from L or specification?
  dataset_name <- "Household"
  id_name <- "HhId"
  y_name <- "AADVMT"
  
  if(!exists("DvmtModel_ls")){
    DvmtModel_ls <- loadPackageDataset("DvmtModel_ls")
  }
  
  Ma <- L$Year$Marea$Marea
  
  Bzone_df <- data.frame(L$Year[["Bzone"]])
  stopifnot("data.frame" %in% class(Bzone_df))
  
  Marea_df <- data.frame(L$Year[["Marea"]])
  stopifnot("data.frame" %in% class(Marea_df))
  
  D_df <- data.frame(L$Year[[dataset_name]])
  stopifnot("data.frame" %in% class(D_df))
  IsUr_ <- D_df$LocType == "Urban"
  
  D_df <- D_df %>%
    mutate(metro=ifelse(LocType=="Urban", "metro", "non_metro"),
           LogIncome=log1p(Income),
           DrvAgePop=HhSize - Age0to14,
           VehPerDriver=ifelse(Drivers==0 | is.na(Drivers), 0, Vehicles/Drivers),
           LifeCycle = as.character(LifeCycle),
           LifeCycle = ifelse(LifeCycle=="01", "Single", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("02"), "Couple w/o children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("00", "03", "04", "05", "06", "07", "08"), "Couple w/ children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("09", "10"), "Empty Nester", LifeCycle)
    ) %>%
    left_join(Bzone_df, by="Bzone") %>%
    left_join(Marea_df, by="Marea")
  
  D_df <- D_df %>% mutate_if(is.factor, as.character)
 
   
  #D_df <- D_df %>%
  #  crossing(Marea_df, by="Marea")
  
  #load("data/AADVMTModel_df.rda")
  Model_df <- loadPackageDataset("AADVMTModel_df")
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc)
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0.01, y))
  #Apply the 95th percentile model
  #-------------------------------
  D_df$Dvmt <- Preds[["y"]]
  D_df$DvmtSq <- Preds[["y"]] ^ 2
  D_df$DvmtCu <- Preds[["y"]] ^ 3
  D_df$Intercept <- 1
  NumHh <- length(L$Year$Household[[1]])
  Dvmt95th_ <- numeric(NumHh)
  Dvmt95th_[IsUr_] <-
    as.vector(eval(parse(text = DvmtModel_ls$Metro$Pctl95),
                   envir =  D_df[IsUr_,]))
  Dvmt95th_[!IsUr_] <-
    as.vector(eval(parse(text = DvmtModel_ls$NonMetro$Pctl95),
                   envir = D_df[!IsUr_,])) 
  
  #Sum the DVMT by Marea
  #--------------------
  tabulateMareaDvmt <- function(LocType) {
    IsType <- D_df$LocType == LocType
    Dvmt_Ma <- setNames(numeric(length(Ma)), Ma)
    if (any(IsType)) {
      Dvmt_Mx <- tapply(D_df$Dvmt[IsType], D_df$Marea[IsType], sum)
      Dvmt_Ma[names(Dvmt_Mx)] <- Dvmt_Mx
      Dvmt_Ma[is.na(Dvmt_Ma)] <- 0
      Dvmt_Ma
    } else {
      Dvmt_Ma
    }
  }
  UrbanDvmt_Ma <- tabulateMareaDvmt("Urban")
  TownDvmt_Ma <- tabulateMareaDvmt("Town") 
  RuralDvmt_Ma <- tabulateMareaDvmt("Rural")
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation

  
  Out_ls <- initDataList()
  Out_ls$Year$Household <-
    list(
      Dvmt = Preds[["y"]],
      Dvmt95th = Dvmt95th_)
  Out_ls$Year$Marea <-
    list(UrbanHhDvmt = UrbanDvmt_Ma,
         TownHhDvmt = TownDvmt_Ma,
         RuralHhDvmt = RuralDvmt_Ma)
  #Return the outputs list
  Out_ls
}
#===============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
#===============================================================
#Run module automatic documentation
#----------------------------------
visioneval::documentModule("CalculateHouseholdDvmt")

#====================
#SECTION 5: TEST CODE
#====================
# model test code is in tests/scripts/test.R
