# #===================
# #TravelTimeReliability.R
# #===================
# 
# #<doc>
# #
# ## TravelTimeReliability Module
# #### October 01, 2020
# #
# # This module calculates travel time reliability measures for the model area. Average speeds at different congestion levels (None, Mod, Hvy, Sev, Ext) on freeways and arterials by marea calculated in the CalculateRoadPerformance  
# # Module are used as inputs to calculate travel time index
# # to calculate Travel Time Index 
# # for Marea
# ### Model Parameter Estimation
# #
# #This module has no estimated parameters.
# #
# ### How the Module Works
# 
# #
# #This module calculates following metrics:
# #
# #* * Travel Time Reliability metric during moderate congestion on arterials for Marea
# #* * Travel Time Reliability metric during heavy congestion on arterials for Marea
# #* * Travel Time Reliability metric during severe congestion on arterials for Marea
# #* * Travel Time Reliability metric during extreme congestion on arterials for Marea
# #* * Travel Time Reliability metric during moderate congestion on freeways for Marea
# #* * Travel Time Reliability metric during heavy congestion on freeways for Marea
# #* * Travel Time Reliability metric during severe congestion on freeways for Marea
# #* * Travel Time Reliability metric during extreme congestion on freeways for Marea
# #
# #</doc>
# 
# 
# #=================================
# #Packages used in code development
# #=================================
# #Uncomment following lines during code development. Recomment when done.
# # library(visioneval)
# # library(stringr)
# 
# #SECTION 1A: LOAD SPEED DATA
# #==========================================
# #---------------------
# #Load congested speeds 
# #---------------------
# UmsInp_ls <- items(
#   item(
#     NAME = "Area",
#     TYPE = "character",
#     PROHIBIT = "",
#     ISELEMENTOF = "",
#     UNLIKELY = "",
#     TOTAL = ""
#   ),
#   item(
#     NAME =
#       items("FwyAveModSpd",
#             "FwyAveHvySpd",
#             "FwyAveSevSpd",
#             "FwyAveExtSpd",
#             "ArtAveModSpd",
#             "ArtAveHvySpd",
#             "ArtAveSevSpd",
#             "ArtAveExtSpd"),
#     TYPE = "double",
#     PROHIBIT = c("< 0"),
#     ISELEMENTOF = "",
#     UNLIKELY = "",
#     TOTAL = ""
#   )
# )
# 
# 
# 
# #Read in Urban Mobility Study datasets
# #-------------------------------------
# UmsInp_df <-
#   processEstimationInputs(
#     UmsInp_ls,
#     "ums_2009.csv",
#     "CalculateRoadPerformance.R")
# rm(UmsInp_ls)
# 
# #========================================================================
# #SECTION 1B: LOAD AND SAVE METROPOLITAN SPEED PARAMETERS
# #========================================================================
# # Base speeds used in the CalculateRoadPerformance module are 
# #levels relative to average levels, estimated maximum reductions possible by
# #congestion category, facility type, congestion type (recurring vs. incident),
# #and assumed deployment relative to the average for a metropolitan area of the
# #size.
# 
# #----------------------------------------------------------
# #Load and base speeds (assuming no operations improvements)
# #----------------------------------------------------------
# BaseSpeedInp_ls <- items(
#   item(
#     NAME = "Level",
#     TYPE = "character",
#     PROHIBIT = "",
#     ISELEMENTOF = "",
#     UNLIKELY = "",
#     TOTAL = ""
#   ),
#   item(
#     NAME =
#       items("Fwy",
#             "Art",
#             "Fwy_Rcr",
#             "Art_Rcr"),
#     TYPE = "double",
#     PROHIBIT = c("< 0"),
#     ISELEMENTOF = "",
#     UNLIKELY = "",
#     TOTAL = ""
#   )
# )
# BaseSpeeds_df <-   processEstimationInputs(
#   BaseSpeedInp_ls,
#   "base_speeds.csv",
#   "CalculateSpeeds.R")
# row.names(BaseSpeeds_df) <- BaseSpeeds_df$Level
# BaseSpeeds_df <- BaseSpeeds_df[,1]
# rm(BaseSpeedInp_ls)
# 

#================================================
#SECTION 1C: CALCULATE TRAVEL TIME INDEX
#================================================

TravelTimeReliabilitySpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify new tables to be created by Inp if any
  #Specify new tables to be created by Set if any
  #Specify input data
  Get = items(
    item(
      NAME = "StateAbbrLookup",
      TABLE = "Region",
      GROUP = "Global",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
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
        "Art_Rcr",
        "Art_NonRcr",
        "Fwy_Rcr",
        "Fwy_NonRcr"),
      TABLE = "OtherOpsEffectiveness",
      GROUP = "Global",
      TYPE = "double",
      UNITS = "proportion",
      PROHIBIT = c("< 0", "> 100"),
      ISELEMENTOF = "",
      OPTIONAL = TRUE
    ),
    item(
      NAME = items(
        "FwyNoneCongSpeed",
        "FwyModCongSpeed",
        "FwyHvyCongSpeed",
        "FwySevCongSpeed",
        "FwyExtCongSpeed",
        "ArtNoneCongSpeed",
        "ArtModCongSpeed",
        "ArtHvyCongSpeed",
        "ArtSevCongSpeed",
        "ArtExtCongSpeed"),
      TABLE = "Marea",
      GROUP = "Year",
      TYPE = "compound",
      UNITS = "MI/HR",
      NAVALUE = -1,
      PROHIBIT = "< 0",
      ISELEMENTOF = "",
      SIZE = 0,
      DESCRIPTION = items(
        "Average freeway speed (miles per hour) when there is no congestion",
        "Average freeway speed (miles per hour) when congestion is moderate",
        "Average freeway speed (miles per hour) when congestion is heavy",
        "Average freeway speed (miles per hour) when congestion is severe",
        "Average freeway speed (miles per hour) when congestion is extreme",
        "Average arterial speed (miles per hour) when there is no congestion",
        "Average arterial speed (miles per hour) when congestion is moderate",
        "Average arterial speed (miles per hour) when congestion is heavy",
        "Average arterial speed (miles per hour) when congestion is severe",
        "Average arterial speed (miles per hour) when congestion is extreme")
      
    )),
    Set = items(
      item(
        NAME = items(
          "FwyModCong_TTI",
          "FwyHvyCong_TTI",
          "FwySevCong_TTI",
          "FwyExtCong_TTI",
          "ArtModCong_TTI",
          "ArtHvyCong_TTI",
          "ArtSevCong_TTI",
          "ArtExtCong_TTI"),
        TABLE = "Marea",
        GROUP = "Year",
        TYPE = "double",
        UNITS = "proportion",
        NAVALUE = -1,
        PROHIBIT = "< 0",
        ISELEMENTOF = "",
        SIZE = 0,
        DESCRIPTION = items(
          "Average freeway travel time index when congestion is moderate",
          "Average freeway travel time index when congestion is heavy",
          "Average freeway travel time index when congestion is severe",
          "Average freeway travel time index when congestion is extreme",
          "Average arterial travel time index when congestion is moderate",
          "Average arterial travel time index when congestion is heavy",
          "Average arterial travel time index when congestion is severe",
          "Average arterial travel time index when congestion is extreme")
      ),
      item(
        NAME = items(
          "FwyModCong_PTI",
          "FwyHvyCong_PTI",
          "FwySevCong_PTI",
          "FwyExtCong_PTI",
          "ArtModCong_PTI",
          "ArtHvyCong_PTI",
          "ArtSevCong_PTI",
          "ArtExtCong_PTI"),
        TABLE = "Marea",
        GROUP = "Year",
        TYPE = "double",
        UNITS = "proportion",
        NAVALUE = -1,
        PROHIBIT = "< 0",
        ISELEMENTOF = "",
        SIZE = 0,
        DESCRIPTION = items(
          "Average freeway planning time index when congestion is moderate",
          "Average freeway planning time index when congestion is heavy",
          "Average freeway planning time index when congestion is severe",
          "Average freeway planning time index when congestion is extreme",
          "Average arterial planning time index when congestion is moderate",
          "Average arterial planning time index when congestion is heavy",
          "Average arterial planning time index when congestion is severe",
          "Average arterial planning time index when congestion is extreme")
      ),
      item(
        NAME = items(
          "FwyModCong_BTI",
          "FwyHvyCong_BTI",
          "FwySevCong_BTI",
          "FwyExtCong_BTI",
          "ArtModCong_BTI",
          "ArtHvyCong_BTI",
          "ArtSevCong_BTI",
          "ArtExtCong_BTI"),
        TABLE = "Marea",
        GROUP = "Year",
        TYPE = "double",
        UNITS = "proportion",
        NAVALUE = -1,
        PROHIBIT = "< 0",
        ISELEMENTOF = "",
        SIZE = 0,
        DESCRIPTION = items(
          "Average freeway buffer time index when congestion is moderate",
          "Average freeway buffer time index when congestion is heavy",
          "Average freeway buffer time index when congestion is severe",
          "Average freeway buffer time index when congestion is extreme",
          "Average arterial buffer time index when congestion is moderate",
          "Average arterial buffer time index when congestion is heavy",
          "Average arterial buffer time index when congestion is severe",
          "Average arterial buffer time index when congestion is extreme")
      )
    )
  )

#Save the data specifications list
#---------------------------------
#' Specifications list for TravelTimeReliability module
#'
#' A list containing specifications for the TravelTimeReliability module.
#'
#' @format A list containing 3 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
#' @source TravelTimeReliability.R script.
"TravelTimeReliabilitySpecifications"
visioneval::savePackageDataset(TravelTimeReliabilitySpecifications, overwrite = TRUE)
    
    
#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
    
#' Calculate travel time reliability measures
#' \code{TravelTimeReliability} calculates freeway and arterial travel time index
#' by congestion level
#' 
#' This function uses the average free flow speed (congestion level none) and the
#' average speeds during the remaining four congestion levels (mod, hvy, sev, ext)
#' and calculates travel time index as a ratio of the congested speed and free flow
#' speed
#' @param L A list containing data defined by the module specification.
#' @return A list containing data produced by the function consistent with the
#' module specifications.
#' @name TravelTimeReliability
#' @export
TravelTimeReliability <- function(L) {
      
  `%>%` <- magrittr::`%>%`
  
    #------
  #Define naming vectors for Mareas and congestion levels
  Ma <- L$Year$Marea$Marea
  Marea_df= data.frame(L$Year$Marea)  
  Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")
  Vt <- c("Ldv", "HvyTrk", "Bus")
 Rc <- c("Fwy", "Art", "Oth")
 #Initialize outputs list
 Out_ls <- initDataList()
 Out_ls$Global$Marea <- list()
 Out_ls$Year$Marea <- list()
  #FwyModCong_BTI <- FwyModCongSpeed / FwyNoneCongSpeed
  #FwyHvyCong_BTI <- FwyHvyCongSpeed / FwyNoneCongSpeed
  #FwySevCong_BTI <- FwySevCongSpeed / FwyNoneCongSpeed
  #FwyExtCong_BTI <- FwyExtCongSpeed / FwyNoneCongSpeed
  #ArtModCong_BTI <- ArtModCongSpeed / ArtNoneCongSpeed
  #ArtHvyCong_BTI <- ArtHvyCongSpeed / ArtNoneCongSpeed
  #ArtSevCong_BTI <- ArtSevCongSpeed / ArtNoneCongSpeed
  #ArtExtCong_BTI <- ArtExtCongSpeed / ArtNoneCongSpeed  
 #Function to remove attributes
 unattr <- function(X_) {
   attributes(X_) <- NULL
   X_
 }
      
  #Calculate Travel Time Index by congestion level for metropolitan areas
  #---------------------------------------------
  TTR_df <- Marea_df %>%
  dplyr::mutate(FwyModCong_TTI = (rlang::.data$FwyNoneCongSpeed)[[1]]/(rlang::.data$FwyModCongSpeed)[[1]],
    FwyHvyCong_TTI = (rlang::.data$FwyNoneCongSpeed)[[1]]/(rlang::.data$FwyHvyCongSpeed)[[1]],
    FwySevCong_TTI = (rlang::.data$FwyNoneCongSpeed)[[1]]/(rlang::.data$FwySevCongSpeed)[[1]],
    FwyExtCong_TTI = (rlang::.data$FwyNoneCongSpeed)[[1]]/(rlang::.data$FwyExtCongSpeed)[[1]],
    ArtModCong_TTI = (rlang::.data$ArtNoneCongSpeed)[[1]]/(rlang::.data$ArtModCongSpeed)[[1]],
    ArtHvyCong_TTI = (rlang::.data$ArtNoneCongSpeed)[[1]]/(rlang::.data$ArtHvyCongSpeed)[[1]],
    ArtSevCong_TTI = (rlang::.data$ArtNoneCongSpeed)[[1]]/(rlang::.data$ArtSevCongSpeed)[[1]],
    ArtExtCong_TTI = (rlang::.data$ArtNoneCongSpeed)[[1]]/(rlang::.data$ArtExtCongSpeed)[[1]])

  TTR_df <- TTR_df %>%
  dplyr::mutate(FwyModCong_PTI = 1+3.67*log(mean((rlang::.data$FwyModCong_TTI))),
    FwyHvyCong_PTI = 1+3.67*log(mean((rlang::.data$FwyHvyCong_TTI))),
    FwySevCong_PTI = 1+3.67*log(mean((rlang::.data$FwySevCong_TTI))),
    FwyExtCong_PTI = 1+3.67*log(mean((rlang::.data$FwyExtCong_TTI))),
    ArtModCong_PTI = 1+3.67*log(mean((rlang::.data$ArtModCong_TTI))),
    ArtHvyCong_PTI = 1+3.67*log(mean((rlang::.data$ArtHvyCong_TTI))),
    ArtSevCong_PTI = 1+3.67*log(mean((rlang::.data$ArtSevCong_TTI))),
    ArtExtCong_PTI = 1+3.67*log(mean((rlang::.data$ArtExtCong_TTI))))

  TTR_df <- TTR_df %>%
  dplyr::mutate(
    FwyModCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$FwyModCong_TTI))))^(1/0.04953),
    FwyHvyCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$FwyHvyCong_TTI))))^(1/0.04953),
    FwySevCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$FwySevCong_TTI))))^(1/0.04953),
    FwyExtCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$FwyExtCong_TTI))))^(1/0.04953),
    ArtModCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$ArtModCong_TTI))))^(1/0.04953),
    ArtHvyCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$ArtHvyCong_TTI))))^(1/0.04953),
    ArtSevCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$ArtSevCong_TTI))))^(1/0.04953),
    ArtExtCong_BTI = 5.3746 / (1 + exp(- 1.5782 - 0.85867*mean((rlang::.data$ArtExtCong_TTI))))^(1/0.04953)
  )
 
 # FwyModCong_TTI <- L$Year$Marea$FwyModCongSpeed / L$Year$Marea$FwyNoneCongSpeed
 # FwyHvyCong_TTI <- L$Year$Marea$FwyHvyCongSpeed / L$Year$Marea$FwyNoneCongSpeed
 # FwySevCong_TTI <- L$Year$Marea$FwySevCongSpeed / L$Year$Marea$FwyNoneCongSpeed
 # FwyExtCong_TTI <- L$Year$Marea$FwyExtCongSpeed / L$Year$Marea$FwyNoneCongSpeed
 # ArtModCong_TTI <- L$Year$Marea$ArtModCongSpeed / L$Year$Marea$ArtNoneCongSpeed
 # ArtHvyCong_TTI <- L$Year$Marea$ArtHvyCongSpeed / L$Year$Marea$ArtNoneCongSpeed
 # ArtSevCong_TTI <- L$Year$Marea$ArtSevCongSpeed / L$Year$Marea$ArtNoneCongSpeed
 # ArtExtCong_TTI <- L$Year$Marea$ArtExtCongSpeed / L$Year$Marea$ArtNoneCongSpeed
      
 #Save performance measures
 # L$Year$Marea$FwyModCong_TTI <- FwyModCong_TTI 
 # L$Year$Marea$FwyHvyCong_TTI <- FwyHvyCong_TTI 
 # L$Year$Marea$FwySevCong_TTI <- FwySevCong_TTI 
 # L$Year$Marea$FwyExtCong_TTI <- FwyExtCong_TTI  
 # L$Year$Marea$ArtModCong_TTI <- ArtModCong_TTI
 # L$Year$Marea$ArtHvyCong_TTI <- ArtHvyCong_TTI 
 # L$Year$Marea$ArtExtCong_TTI <- ArtSevCong_TTI 
 # L$Year$Marea$ArtSevCong_TTI <- ArtExtCong_TTI 
  
 Out_ls$Year$Marea$FwyModCong_TTI <- TTR_df$FwyModCong_TTI 
 Out_ls$Year$Marea$FwyHvyCong_TTI <- TTR_df$FwyHvyCong_TTI 
 Out_ls$Year$Marea$FwySevCong_TTI <- TTR_df$FwySevCong_TTI 
 Out_ls$Year$Marea$FwyExtCong_TTI <- TTR_df$FwyExtCong_TTI  
 Out_ls$Year$Marea$ArtModCong_TTI <- TTR_df$ArtModCong_TTI
 Out_ls$Year$Marea$ArtHvyCong_TTI <- TTR_df$ArtHvyCong_TTI 
 Out_ls$Year$Marea$ArtExtCong_TTI <- TTR_df$ArtSevCong_TTI 
 Out_ls$Year$Marea$ArtSevCong_TTI <- TTR_df$ArtExtCong_TTI   
 
 Out_ls$Year$Marea$FwyModCong_PTI <- TTR_df$FwyModCong_PTI 
 Out_ls$Year$Marea$FwyHvyCong_PTI <- TTR_df$FwyHvyCong_PTI 
 Out_ls$Year$Marea$FwySevCong_PTI <- TTR_df$FwySevCong_PTI 
 Out_ls$Year$Marea$FwyExtCong_PTI <- TTR_df$FwyExtCong_PTI  
 Out_ls$Year$Marea$ArtModCong_PTI <- TTR_df$ArtModCong_PTI
 Out_ls$Year$Marea$ArtHvyCong_PTI <- TTR_df$ArtHvyCong_PTI 
 Out_ls$Year$Marea$ArtExtCong_PTI <- TTR_df$ArtSevCong_PTI 
 Out_ls$Year$Marea$ArtSevCong_PTI <- TTR_df$ArtExtCong_PTI  
 
 Out_ls$Year$Marea$FwyModCong_BTI <- TTR_df$FwyModCong_BTI 
 Out_ls$Year$Marea$FwyHvyCong_BTI <- TTR_df$FwyHvyCong_BTI 
 Out_ls$Year$Marea$FwySevCong_BTI <- TTR_df$FwySevCong_BTI 
 Out_ls$Year$Marea$FwyExtCong_BTI <- TTR_df$FwyExtCong_BTI  
 Out_ls$Year$Marea$ArtModCong_BTI <- TTR_df$ArtModCong_BTI
 Out_ls$Year$Marea$ArtHvyCong_BTI <- TTR_df$ArtHvyCong_BTI 
 Out_ls$Year$Marea$ArtExtCong_BTI <- TTR_df$ArtSevCong_BTI 
 Out_ls$Year$Marea$ArtSevCong_BTI <- TTR_df$ArtExtCong_BTI  
 
 
 
 #Save performance measures
 #-------------------------
 
 #Return the result
 Out_ls
}
      
 #===============================================================
 #SECTION 4: MODULE DOCUMENTATION AND AUXILLIARY DEVELOPMENT CODE
 #===============================================================
 #Run module automatic documentation
 #----------------------------------
 #documentModule("TravelTimeReliability")

