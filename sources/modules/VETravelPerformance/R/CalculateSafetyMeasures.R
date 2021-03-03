#===================
#CalculateSafetyMeasures.R
#===================

#<doc>
#
## CalculateSafetyMeasures Module
#### June 02, 2020
#
# This module aims to calculate safety measures for the model area. It uses the fatality and injury crash rates for different modes
# All these rates are per miles traveled. Therefore this module simply uses calculated VMT from model to estimate the 
# fatality and injury rates for Marea
### Model Parameter Estimation
#
#This module has no estimated parameters.
#
### How the Module Works
#
#This module calculates following metrics:
#
#* * Auto daily fataility and injuries for Urban, Rural and Town areas
#* * Bike daily fataility and injuries for Marea
#* * Walk daily fataility and injuries for Marea
#* * Bus daily fataility and injuries for Marea
#* * Rail daily fataility and injuries for Marea
#* * Van daily fataility and injuries for Marea

#
#</doc>


#=================================
#Packages used in code development
#=================================
#Uncomment following lines during code development. Recomment when done.
# library(visioneval)
# library(stringr)


#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================
#The estimated model parameters for this module are created in the


#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
CalculateSafetyMeasuresSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify new tables to be created by Inp if any
  #Specify input data
  #Specify data to be loaded from data store
  ##Specify input data
  Inp = items(
    item(
      NAME =
        items(
          "AutoFatal",
          "AutoInjur",
          "BikeFatal",
          "BikeInjur",
          "WalkFatal",
          "WalkInjur",
          "BusFatal",
          "BusInjur",
          "RailFatal",
          "RailInjur"
        ),
      FILE = "marea_safety_factors.csv",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "CRASH/MI",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = "< 0",
      ISELEMENTOF = "",
      UNLIKELY = "",
      TOTAL = "",
      DESCRIPTION =
        items(
          "Number of fatal auto crashes per 100 millions of mile traveled",
          "Number of injury auto crashes per 100 millions of mile traveled",
          "Number of fatal bike crashes per 100 millions of mile traveled",
          "Number of injury bike crashes per 100 millions of mile traveled",
          "Number of fatal walk crashes per 100 millions of mile traveled",
          "Number of injuy walk crashes per 100 millions of mile traveled",
          "Number of fatal bus crashes per 1 millions of mile traveled",
          "Number of injury bus crashes per 1 millions of mile traveled",
          "Number of fatal rail crashes per 1 millions of mile traveled",
          "Number of injury rail crashes per 1 millions of mile traveled"
        )
    )
  ),
  Get = items(
    item(
      NAME = "Marea",
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    item(
      NAME = items(
        "VanDvmt",
        "BusDvmt",
        "RailDvmt"),
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/DAY",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = items(
        "UrbanHhDvmt",
        "RuralHhDvmt",
        "TownHhDvmt"),
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/DAY",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
   ),
    item(
      NAME = items(
        "AutoFatal",
        "AutoInjur",
        "BikeFatal",
        "BikeInjur",
        "WalkFatal",
        "WalkInjur",
        "BusFatal",
        "BusInjur",
        "RailFatal",
        "RailInjur"),
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "CRASH/MI",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ), 
    item(
      NAME = "Marea",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    item(
      NAME = "LocType",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      PROHIBIT = "NA",
      ISELEMENTOF = c("Urban", "Town", "Rural")
    ),
    item(
      NAME = "Dvmt",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/DAY",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME =
        items("WalkTrips",
              "BikeTrips",
              "TransitTrips"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "TRIP/DAY",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
   item(
     NAME =
       items("WalkAvgTripDist",
             "BikeAvgTripDist",
             "TransitAvgTripDist"),
     TABLE = "Household",
     GROUP = "Year",
     TYPE = "double",
     UNITS = "MI",
     PROHIBIT = c("NA", "< 0"),
     ISELEMENTOF = ""
   )
    
    ),
  #Specify data to saved in the data store
  Set = items(
    item(
      NAME = 
        items(
          "AutoFatalCrashRural",
          "AutoFatalCrashUrban",
          "AutoFatalCrashTown",
          
          "AutoInjuryCrashRural",
          "AutoInjuryCrashUrban",
          "AutoInjuryCrashTown",
          
          "WalkFatalCrash",
          "WalkInjuryCrash",
          "BikeFatalCrash",
          "BikeInjuryCrash",
          "BusFatalCrash",
          "BusInjuryCrash",
          "RailFatalCrash",
          "RailInjuryCrash",
          "VanFatalCrash",
          "VanInjuryCrash"
        ),
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "double",
      UNITS = "CRASH",
      NAVALUE = -1,
      SIZE = 0,
      PROHIBIT = "< 0",
      ISELEMENTOF = "",
      DESCRIPTION = items(
        "Number of yearly atuo fatal crashes in Rural area.",
        "Number of yearly auto injury crashes in Rural area",
        "Number of yearly atuo fatal crashes in Urban area.",
        "Number of yearly auto injury crashes in Urabn area",
        "Number of yearly atuo fatal crashes in Town area.",
        "Number of yearly auto injury crashes in Town area",
        "Number of yearly walk fatal crashes in Marea.",
        "Number of yearly walk injury crashes in Marea.",
        "Number of yearly bike fatal crashes in Marea.",
        "Number of yearly bike injury crashes in Marea.",
        "Number of yearly bus fatal crashes in Marea.",
        "Number of yearly bus injury crashes in Marea.",
        "Number of yearly rail fatal crashes in Marea.",
        "Number of yearly rail injury crashes in Marea.",
        "Number of yearly van fatal crashes in Marea.",
        "Number of yearly van injury crashes in Marea."
      )
    )
  )
)
#Save the data specifications list
#---------------------------------
#' Specifications list for CalculateRoadDvmt module
#'
#' A list containing specifications for the CalculateRoadDvmt module.
#'
#' @format A list containing 3 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
#' @source CalculateSafetyMeasures.R script.
"CalculateSafetyMeasuresSpecifications"
visioneval::savePackageDataset(CalculateSafetyMeasuresSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================

#Main module function that assigns base year DVMT
#------------------------------------------------
#' Assign base year DVMT by vehicle type and road class for Mareas
#'
#' \code{CalculateSafetyMeasures} Calculates number of yearly fatal and injury crashes
#' for Auto, Bike, Walk, Rail, Bus and Van modes
#'
#'This function uses estimated VMT and PMT for all the modes and apply the 
#'crash rates which are the input of the moduls to get the total number of crashes
#' @param L A list containing data defined by the module specification.
#' @return A list containing data produced by the function consistent with the
#' module specifications.
#' @name CalculateSafetyMeasures
#' @export
CalculateSafetyMeasures <- function(L) {
  
  `%>%` <- magrittr::`%>%`
  
  HH_df= data.frame(L$Year$Household)
  Marea_df= data.frame(L$Year$Marea)

# calcualte Bike and Walk PMT from Houshold table and aggregate all housholds to geth the total Marea PMTs
# apply the injury and fatal rates to estiamte the crashes by type.  
    Crashes_HH <- HH_df %>%
      dplyr::mutate(BikeMT = (rlang::.data$BikeTrips) * (rlang::.data$BikeAvgTripDist),
             WalkMT = (rlang::.data$WalkTrips) * (rlang::.data$WalkAvgTripDist)) %>%
      dplyr::group_by((rlang::.data$Marea)) %>%
      dplyr::summarise(BikePMT = sum((rlang::.data$BikeMT)),
              WalkPMT = sum((rlang::.data$WalkMT))) %>%
      dplyr::mutate(walkfatal = Marea_df$WalkFatal[[1]] *(rlang::.data$WalkPMT) / (10^8),
             walkinjury = Marea_df$WalkInjur[[1]] *(rlang::.data$WalkPMT) / (10^8),
             bikefatal = Marea_df$BikeFatal[[1]] *(rlang::.data$BikePMT) / (10^8),
             bikeinjury = Marea_df$BikeInjur[[1]] *(rlang::.data$BikePMT) / (10^8)
           )
  
    Crashes_Marea <- Marea_df %>%
      dplyr::mutate(AutoFatalCrashRural = (rlang::.data$AutoFatal)[[1]] * (rlang::.data$RuralHhDvmt)[[1]] / (10^8),
             AutoInjuryCrashRural = (rlang::.data$AutoInjur)[[1]] * (rlang::.data$RuralHhDvmt)[[1]] / (10^8) ,
             AutoFatalCrashUrban = (rlang::.data$AutoFatal)[[1]] * (rlang::.data$UrbanHhDvmt)[[1]] / (10^8),
             AutoInjuryCrashUrban= (rlang::.data$AutoInjur)[[1]] * (rlang::.data$UrbanHhDvmt)[[1]] / (10^8),
             AutoFatalCrashTown = (rlang::.data$AutoFatal)[[1]] * (rlang::.data$TownHhDvmt)[[1]] / (10^8),
             AutoInjuryCrashTown = (rlang::.data$AutoInjur)[[1]] * (rlang::.data$TownHhDvmt)[[1]] / (10^8),
             busfatal = (rlang::.data$BusFatal)[[1]] * (rlang::.data$BusDvmt)[[1]] / (10^6), 
             businjury = (rlang::.data$BusInjur)[[1]] *(rlang::.data$BusDvmt)[[1]] / (10^6),
             railfatal = (rlang::.data$RailFatal)[[1]] * (rlang::.data$RailDvmt)[[1]] / (10^6), 
             railinjury = (rlang::.data$RailInjur)[[1]] *(rlang::.data$RailDvmt)[[1]] / (10^6),
             vanfatal = (rlang::.data$BusFatal)[[1]] * (rlang::.data$VanDvmt)[[1]] / (10^6), 
             vaninjury = (rlang::.data$BusInjur)[[1]] *(rlang::.data$VanDvmt)[[1]] / (10^6) )
    
      
    Out_ls <- initDataList()
    Out_ls$Year$Marea$AutoFatalCrashRural <- Crashes_Marea$AutoFatalCrashRural *365
    Out_ls$Year$Marea$AutoInjuryCrashRural <- Crashes_Marea$AutoInjuryCrashRural *365
    Out_ls$Year$Marea$AutoFatalCrashUrban <- Crashes_Marea$AutoFatalCrashUrban *365
    Out_ls$Year$Marea$AutoInjuryCrashUrban <- Crashes_Marea$AutoInjuryCrashUrban *365
    Out_ls$Year$Marea$AutoFatalCrashTown <- Crashes_Marea$AutoFatalCrashTown *365
    Out_ls$Year$Marea$AutoInjuryCrashTown <- Crashes_Marea$AutoInjuryCrashTown *365
    Out_ls$Year$Marea$WalkFatalCrash <- Crashes_HH$walkfatal *365
    Out_ls$Year$Marea$WalkInjuryCrash <- Crashes_HH$walkinjury *365
    Out_ls$Year$Marea$BikeFatalCrash <- Crashes_HH$bikefatal *365
    Out_ls$Year$Marea$BikeInjuryCrash <- Crashes_HH$bikeinjury *365
    Out_ls$Year$Marea$RailFatalCrash <- Crashes_Marea$railfatal *365
    Out_ls$Year$Marea$RailInjuryCrash <- Crashes_Marea$railinjury *365
    Out_ls$Year$Marea$BusFatalCrash <- Crashes_Marea$busfatal *365
    Out_ls$Year$Marea$BusInjuryCrash <- Crashes_Marea$businjury *365
    Out_ls$Year$Marea$VanFatalCrash <- Crashes_Marea$vanfatal *365
    Out_ls$Year$Marea$VanInjuryCrash <- Crashes_Marea$vaninjury *365

    Out_ls
}


#===============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
#===============================================================
#Run module automatic documentation
#----------------------------------
documentModule("CalculateSafetyMeasures")
