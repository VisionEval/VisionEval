#==========================
#CalculateAltModeTrips.R
#==========================

#<doc>
#
## CalculateAltModeTrips Module
#### March 2020
#
#This module predicts:
# 1- transit PMT for households. It uses the model object in data/TransitPMTModel_df.rda and variables and coefficients therein to predict TransitPMT.
# 2- transit trip frequency and average trip length for households. It uses the model object in data/TransitTFLModel_df.rda and variables and coefficients therein to predict TransitTFL.
# 3- walking PMT for households. It uses the model object in data/WalkPMTModel_df.rda and variables and coefficients therein to predict WalkPMT.
# 4- walking trip frequency and average walking trip length for households. It uses the model object in data/WalkTFLModel_df.rda and variables and coefficients therein to predict WalkTFL.
# 5- biking PMT for households. It uses the model object in data/BikePMTModel_df.rda and variables and coefficients therein to predict BikePMT.
# 6- trip frequency (BikeTrips) and average trip length (BikeAvgTripDist) for households. It uses the model object in data/BikeTFLModel_df.rda and variables and coefficients therein to predict BikeTFL.

#
### Model Parameter Estimation
#See:
# data-raw/TransitPMTModel_df.R
# data-raw/TransitTFLModel_df.R
# data-raw/WalkPMTModel_df.R
# data-raw/WalkTFLModel_df.R
# data-raw/BikePMTModel_df.R
# data-raw/BikeTFLModel_df.R

### How the Module Works
#
#The user specifies the model in the estimation scripts in data-raw folder and saves the estimation results in data folder. If no model re-estimation is desired, the estimation process can be skipped and the default model specification is then used. The module assigns the alternative mode PMT and TFL to each household using variables including household characteristics, built environment, and transportation supply.
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
#See:
# data/TransitPMTModel_df.R
# data/TransitTFLModel_df.R
# data/WalkPMTModel_df.R
# data/WalkTFLModel_df.R
# data/BikePMTModel_df.R
# data/BikeTFLModel_df.R
#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

require(visioneval,quietly=TRUE)

#Define the data specifications
#------------------------------
CalculateAltModeTripsSpecifications <- list(
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
    # visioneval::item(
    #   NAME = "D3apo",
    #   FILE = "bzone_network_design2.csv",
    #   TABLE = "Bzone",
    #   GROUP = "Year",
    #   TYPE = "double",
    #   UNITS = "pedestrian-oriented intersections per square mile",
    #   NAVALUE = -9999,
    #   SIZE = 0,
    #   PROHIBIT = "NA",
    #   ISELEMENTOF = "",
    #   UNLIKELY = "",
    #   TOTAL = "",
    #   DESCRIPTION = "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile (Ref: EPA 2010 Smart Location Database)"
    # ),
    # visioneval::item(
    #   NAME = "D3bmm4",
    #   FILE = "bzone_network_design3.csv",
    #   TABLE = "Bzone",
    #   GROUP = "Year",
    #   TYPE = "double",
    #   UNITS = "pedestrian-oriented intersections per square mile",
    #   NAVALUE = -9999,
    #   SIZE = 0,
    #   PROHIBIT = "NA",
    #   ISELEMENTOF = "",
    #   UNLIKELY = "",
    #   TOTAL = "",
    #   DESCRIPTION = "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile (Ref: EPA 2010 Smart Location Database)"
    # )
    # visioneval::item(
    #   NAME =
    #     items(
    #       "D3apo",
    #       "D3bpo4",
    #       "D3bmm4"),
    #   FILE = "bzone_network_design.csv",
    #   TABLE = "Bzone",
    #   GROUP = "Year",
    #   TYPE = "double",
    #   UNITS = "pedestrian-oriented intersections per square mile",
    #   NAVALUE = -9999,
    #   SIZE = 0,
    #   PROHIBIT = "NA",
    #   ISELEMENTOF = "",
    #   UNLIKELY = "",
    #   TOTAL = "",
    #   DESCRIPTION =
    #     items(
    #       "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile (Ref: EPA 2010 Smart Location Database)",
    #       "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile (Ref: EPA 2010 Smart Location Database)",
    #       "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile (Ref: EPA 2010 Smart Location Database)"
    #     )
    # )
    
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
      NAME =
        list("HhSize",
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
      UNITS = "PRSN/ACRE",
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
    # visioneval::item(
    #   NAME = "D3apo",
    #   TABLE = "Bzone",
    #   GROUP = "Year",
    #   TYPE = "double",
    #   UNITS = "pedestrian-oriented intersections per square mile",
    #   NAVALUE = -9999,
    #   SIZE = 0,
    #   PROHIBIT = "NA",
    #   ISELEMENTOF = ""
    # ),

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
    # visioneval::item(
    #   NAME = "D3bmm4",
    #   TABLE = "Bzone",
    #   GROUP = "Year",
    #   TYPE = "double",
    #   UNITS = "multimodal intersections per square mile",
    #   NAVALUE = -9999,
    #   SIZE = 0,
    #   PROHIBIT = "NA",
    #   ISELEMENTOF = ""
    # ),
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
      NAME = "D5",
      TABLE = "Bzone",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "NA",
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
    # visioneval::item(
    #   NAME = "TRPOPDEN",
    #   TABLE = "Bzone",
    #   GROUP = "Year",
    #   TYPE = "compound",
    #   UNITS = "PRSN/SQM",
    #   NAVALUE = -1,
    #   PROHIBIT = c("NA", "< 0"),
    #   ISELEMENTOF = "",
    #   SIZE = 0
    # ),    
    # # visioneval::item(
    # #   NAME = "ZeroVeh",
    # #   TABLE = "Household",
    # #   GROUP = "Year",
    # #   TYPE = "integer",
    # #   UNITS = "none",
    # #   PROHIBIT = c("NA", "< 0"),
    # #   ISELEMENTOF = ""
    # # ),
    # 
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
    )
  ),
  
  #Specify data to saved in the data store
 
  Set = visioneval::items(
    visioneval::item(
      NAME =
        list("WalkTrips",
              "BikeTrips",
              "TransitTrips"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "TRIP/YR",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = list(
        "Average number of walk trips per year by household members",
        "Average number of bicycle trips per year by household members",
        "Average number of public transit trips per year by household members"
      )
    ),    
    visioneval::item(
      NAME =
        list("WalkAvgTripDist",
              "BikeAvgTripDist",
              "TransitAvgTripDist"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "MI",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = list(
        "Daily walking average trip length",
        "Daily biking average trip length",
        "Daily transit average trip length"
      )
    ),
    visioneval::item(
      NAME =
         list("WalkPMT",
              "BikePMT",
              "TransitPMT"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "distance",
      UNITS = "MI",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = list(
        "Daily walking person miles traveled by all members of the household",
        "Daily biking person miles traveled by all members of the household",
        "Daily transit person miles traveled by all members of the household"
      )   
      )
    
  
    
    
    
    
    ),
  #Make module callable
  Call = TRUE
  
  
)

#Save the data specifications list
#---------------------------------
#' Specifications list for PredictTransitPMT module
#'
#' A list containing specifications for the PredictTransitPMT module.
#'
#' @format A list containing 4 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Inp}{scenario input data to be loaded into the datastore for this
#'  module}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
"CalculateAltModeTripsSpecifications"
visioneval::savePackageDataset(CalculateAltModeTripsSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#This function calculates alternative mode trips for each household.

#Main module function that calculates household alternative mode trips
#---------------------------------------------------------------------
#' Main module function to calculate alternative mode trips
#'
#' \code{CalculateAltModeTrips} Function first predicts PMT for Walk, Bike and Transit modes 
#' for each household in the households dataset using independent variables including household 
#' characteristics and 5D built environment variables.
#' 
#' This function predicts PMT for alterantive modes for each hosuehold in the model region where
#' each household is assigned a PMT value for each mdoe. The model objects as a part of the
#' inputs are stored in data frame with two columns: a column for segmentation
#' (e.g., metro, non-metro) and a 'model' column for model object (list-column
#' data structure). The function "nests" the households data frame into a
#' list-column data frame by segments and applies the generic predict() function
#' for each segment to predict PMT for each household. This is done for all modes and 
#' vectors of HhId and PMT values are produced and stored in the
#' "Household" table.
#' 
#' If this table does not exist, the function calculates a LENGTH value for
#' the table and returns that as well. The framework uses this information to
#' initialize the Households table. The function also computes the maximum
#' numbers of characters in the HhId and Azone datasets and assigns these to a
#' SIZE vector. This is necessary so that the framework can initialize these
#' datasets in the datastore. All the results are returned in a list.
#'
#' The second part of function predicts the the daily average trip length and 
#' annual averae number of trips generated by each alternative modes in households
#' and return the values to the datastore
#'   
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @name CalculateAltModeTrips
#' @import visioneval
#' @export
CalculateAltModeTrips <- function(L) {
  
  #TODO: get id_name from L or specification?
  
  
  dataset_name <- "Household"
  id_name <- "HhId"
  
  
  Bzone_df <- data.frame(L$Year[["Bzone"]])
  if ("D5" %in% colnames(Bzone_df)) {
    Bzone_df$D5 = Bzone_df$D5 / 10000
  }
  stopifnot("data.frame" %in% class(Bzone_df))
  
  Marea_df <- data.frame(L$Year[["Marea"]])
  stopifnot("data.frame" %in% class(Marea_df))
  
  D_df <- data.frame(L$Year[[dataset_name]])
  stopifnot("data.frame" %in% class(D_df))
  D_df <- D_df %>%
    mutate(metro=ifelse(LocType=="Urban", "metro", "non_metro"),
           AADVMT = Dvmt,
           LogIncome=log1p(Income),
           DrvAgePop=HhSize - Age0to14,
           VehPerDriver=ifelse(Drivers==0 || is.na(Drivers), 0, Vehicles/Drivers),
           LifeCycle = as.character(LifeCycle),
           LifeCycle = ifelse(LifeCycle=="01", "Single", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("02"), "Couple w/o children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("00", "03", "04", "05", "06", "07", "08"), "Parents w/ children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("09", "10"), "Empty Nester", LifeCycle)
    ) %>%
    left_join(Bzone_df, by="Bzone") %>%
    crossing(Marea_df)
  
  #D_df <- D_df %>%
  #  crossing(Marea_df, by="Marea")
  
  # WalkPMT prediction
  
  #load("data/WalkPMTModel_df.rda")
  Model_df <- loadPackageDataset("WalkPMTModel_df")
  y_name <- "WalkPMT"
  
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "Step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc)
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0, y))
  
  Out_ls <- initDataList()
  Out_ls$Year$Household$WalkPMT <- Preds[["y"]]
  
  
    # BikePMT
  
  #load("data/WalkPMTModel_df.rda")
  Model_df <- loadPackageDataset("BikePMTModel_df")
  y_name <- "BikePMT"
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc)
  
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0, y))
  
  # Out_ls$Year$Household <-
  #   list(
  #     BikePMT = -1
  #   )
  Out_ls$Year$Household$BikePMT <- Preds[["y"]]
  
  # TransitPMT
  
  #load("data/TransitPMTModel_df.rda")
  Model_df <- loadPackageDataset("TransitPMTModel_df")
  y_name <- "TransitPMT"
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc)
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0, y))
  
  # Out_ls <- initDataList()
  # Out_ls$Year$Household <-
  #   list(
  #     TransitPMT = -1
  #   )
  Out_ls$Year$Household$TransitPMT <- Preds[["y"]]
  
  #change the dataframe to be compatible with TFL models
 
    # WalkTFL
  
  #load("data/WalkTFLModel_df.rda")
  Model_df <- loadPackageDataset("WalkTFLModel_df")
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "Step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc, combine_preds=FALSE)
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0, y))
  
  # Out_ls <- initDataList()
  # Out_ls$Year$Household <-
  #   list(
  #     WalkTrips = -1,
  #     WalkAvgTripDist = -1
  #   )
  Out_ls$Year$Household$WalkTrips       <- (Preds %>% filter(Step==1) %>% pull(y)) *365
  Out_ls$Year$Household$WalkAvgTripDist <- Preds %>% filter(Step==2) %>% pull(y)
  
  # BikeTFL
  
  #load("data/BikeTFLModel_df.rda")
  Model_df <- loadPackageDataset("BikeTFLModel_df")
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "Step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc, combine_preds=FALSE)
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0, y))
  
  # Out_ls <- initDataList()
  # Out_ls$Year$Household <-
  #   list(
  #     BikeTrips = -1,
  #     BikeAvgTripDist = -1
  #   )
  Out_ls$Year$Household$BikeTrips       <- (Preds %>% filter(Step==1) %>% pull(y)) *365
  Out_ls$Year$Household$BikeAvgTripDist <- Preds %>% filter(Step==2) %>% pull(y)
  
  # TransitTFL
  
  #load("data/TransitTFLModel_df.rda")
  Model_df <- loadPackageDataset("TransitTFLModel_df")
  
  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "Step", "post_func", "bias_adj"))
  
  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))
  
  Preds <- DoPredictions(Model_df, D_df,
                         dataset_name, id_name, y_name, SegmentCol_vc, combine_preds=FALSE)
  
  # fill NA with 0s - produced with negative predictions before inversing power transformation
  Preds <- Preds %>%
    mutate(y=ifelse(is.na(y) | y < 0, 0, y))
  
  # Out_ls <- initDataList()
  # Out_ls$Year$Household <-
  #   list(
  #     TransitTrips = -1,
  #     TransitAvgTripDist = -1
  #   )
  Out_ls$Year$Household$TransitTrips       <- (Preds %>% filter(Step==1) %>% pull(y)) *365
  Out_ls$Year$Household$TransitAvgTripDist <- Preds %>% filter(Step==2) %>% pull(y)
  
  #Return the list
  Out_ls
  
}


#===============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
#===============================================================
#Run module automatic documentation
#----------------------------------
visioneval::documentModule("CalculateAltModeTrips")

#====================
#SECTION 5: TEST CODE
#====================
#model test code is in tests/scripts/test.R
