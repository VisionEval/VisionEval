#==============================================================
#Define function to calculate metropolitan performance measures
#==============================================================
calcMetropolitanMeasures <- 
  function(Year, Ma, DstoreLocs_ = c("Datastore"), DstoreType = "RD") {
    
    #Prepare for datastore queries
    #-----------------------------
    QPrep_ls <- prepareForDatastoreQuery(
      DstoreLocs_ = DstoreLocs_,
      DstoreType = DstoreType
    )
    
    #Define function to create a data frame of measures
    #--------------------------------------------------
    makeMeasureDataFrame <- function(DataNames_, Ma) {
      if (length(Ma) > 1) {
        Data_XMa <- t(sapply(DataNames_, function(x) get(x)))
      } else {
        Data_XMa <- t(t(sapply(DataNames_, function(x) get(x))))
      }
      colnames(Data_XMa) <- Ma
      Measures_ <- gsub("_Ma", "", DataNames_)
      Units_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Units))
      Description_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Description))
      Data_df <- cbind(
        Measure = Measures_,
        data.frame(Data_XMa),
        Units = Units_,
        Description = Description_
      )
      rownames(Data_df) <- NULL
      Data_df
    }

    
    #=========================    
    #HOUSEHOLD CHARACTERISTICS
    #=========================

    #Number of households in Marea
    #-----------------------------
    MareaHhNum_Ma <- summarizeDatasets(
      Expr = "count(HhSize)",
      Units_ = c(
        HhSize = "",
        Marea = ""),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhNum_Ma) <- 
      list(Units = "Households",
           Description = "Number of households residing in Marea")

    #Population of Marea
    #-------------------
    MareaHhPop_Ma <- summarizeDatasets(
      Expr = "sum(HhSize)",
      Units_ = c(
        HhSize = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhPop_Ma) <- 
      list(Units = "Persons",
           Description = "Number of persons residing in Marea")
    
    #Number of workers in Marea
    #--------------------------
    MareaHhWorkers_Ma <- summarizeDatasets(
      Expr = "sum(Workers)",
      Units_ = c(
        Workers = "PRSN",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhWorkers_Ma) <- 
      list(Units = "Workers",
           Description = "Number of workers residing in Marea")
    
    #Total household income of Marea
    #-------------------------------
    MareaHhIncome_Ma <- summarizeDatasets(
      Expr = "sum(Income)",
      Units_ = c(
        Income = "USD",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhIncome_Ma) <- 
      list(Units = "Base year dollars",
           Description = "Total annual income of households residing in Marea")
    
    #Number of drivers in Marea
    #--------------------------
    MareaHhDrivers_Ma <- summarizeDatasets(
      Expr = "sum(Drivers)",
      Units_ = c(
        Drivers = "PRSN",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhDrivers_Ma) <- 
      list(Units = "Drivers",
           Description = "Number of drivers residing in Marea")
    
    #Number of vehicles owned by households in Marea
    #-----------------------------------------------
    MareaHhVehicles_Ma <- summarizeDatasets(
      Expr = "sum(NumAuto) + sum(NumLtTrk)",
      Units_ = c(
        NumAuto = "VEH",
        NumLtTrk = "VEH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhVehicles_Ma) <- 
      list(Units = "Household light-duty vehicles",
           Description = "Total number of light-duty vehicles owned/leased by households residing in Marea")

    #Marea number of light trucks
    #----------------------------
    MareaHhLightTrucks_Ma <- summarizeDatasets(
      Expr = "sum(NumLtTrk)",
      Units_ = c(
        NumLtTrk = "VEH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    
    #Marea light-truck vehicle proportion
    #------------------------------------
    MareaHhLtTrkProp_Ma <- MareaHhLightTrucks_Ma / MareaHhVehicles_Ma
    attributes(MareaHhLtTrkProp_Ma) <- 
      list(Units = "Light truck proportion of household vehicles",
           Description = "Light truck proportion of light-duty vehicles owned/leased by households residing in the Marea")
    
    #Average household vehicle age for Marea
    #---------------------------------------
    MareaHhAveVehAge_Ma <- summarizeDatasets(
      Expr = "mean(Age[VehicleAccess == 'Own'])",
      Units_ = c(
        Age = "YR",
        VehicleAccess = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Vehicle",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhAveVehAge_Ma) <- 
      list(Units = "Years",
           Description = "Average age of vehicles owned/leased by households residing in the Marea")
    
    #Average car service light truck proportion of car service DVMT
    #--------------------------------------------------------------
    MareaCarSvcLtTrkDvmtProp_Ma <- summarizeDatasets(
      Expr = "sum(DvmtProp[VehicleAccess != 'Own' & Type == 'LtTrk']) / sum(DvmtProp[VehicleAccess != 'Own'])",
      Units_ = c(
        DvmtProp = "",
        VehicleAccess = "",
        Type = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Vehicle",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaCarSvcLtTrkDvmtProp_Ma) <- 
      list(Units = "Proportion",
           Description = "Average proportion car service vehicle DVMT in light trucks used by households residing in the Marea")
    
    #Average car service vehicle age for Marea
    #-----------------------------------------
    MareaCarSvcAveVehAge_Ma <- summarizeDatasets(
      Expr = "mean(Age[VehicleAccess != 'Own'])",
      Units_ = c(
        Age = "YR",
        VehicleAccess = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Vehicle",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaCarSvcAveVehAge_Ma) <- 
      list(Units = "Years",
           Description = "Average age of car service vehicles used by households residing in the Marea")
    
    #Average commercial service vehicle light truck proportion
    #---------------------------------------------------------
    MareaComSvcLtTrkDvmtProp <- summarizeDatasets(
      Expr = "ComSvcLtTrkProp",
      Units_ = c(
        ComSvcLtTrkProp = ""
      ),
      Table = "Region",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcLtTrkDvmtProp_Ma <- rep(MareaComSvcLtTrkDvmtProp, length(Ma))
    attributes(MareaComSvcLtTrkDvmtProp_Ma) <- 
      list(Units = "Proportion",
           Description = "Light truck proportion of commercial service vehicle DVMT in the Marea")
    rm(MareaComSvcLtTrkDvmtProp)

    #Average commercial service vehicle age
    #--------------------------------------
    MareaComSvcAveVehAge <- summarizeDatasets(
      Expr = "AveComSvcVehicleAge",
      Units_ = c(
        AveComSvcVehicleAge = "YR"
      ),
      Table = "Region",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcAveVehAge_Ma <- rep(MareaComSvcAveVehAge, length(Ma))
    attributes(MareaComSvcAveVehAge_Ma) <- 
      list(Units = "Years",
           Description = "Average age of commercial service vehicles used in the Marea")
    rm(MareaComSvcAveVehAge)
        
    #Number of households in urbanized area
    #--------------------------------------
    HhNum_Ma <- summarizeDatasets(
      Expr = "count(HhSize[LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhNum_Ma) <- 
      list(Units = "Households",
           Description = "Number of households residing in urbanized area")
    
    #Population in urbanized area
    #----------------------------
    HhPop_Ma <- summarizeDatasets(
      Expr = "sum(HhSize[LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhPop_Ma) <- 
      list(Units = "Persons",
           Description = "Number of persons residing in urbanized area")
    
    #Average household size of urbanized area households
    #---------------------------------------------------
    HhAveSize_Ma <- HhPop_Ma / HhNum_Ma
    
    #Number of workers
    #-----------------
    HhWorkers_Ma <- summarizeDatasets(
      Expr = "sum(Workers[LocType == 'Urban'])",
      Units_ = c(
        Workers = "PRSN",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhWorkers_Ma) <- 
      list(Units = "Workers",
           Description = "Number of workers residing in urbanized area")
    
    #Average workers per household
    #-----------------------------
    HhAveNumWkr_Ma <- HhWorkers_Ma / HhNum_Ma
    
    #Total household income
    #----------------------
    HhIncome_Ma <- summarizeDatasets(
      Expr = "sum(Income[LocType == 'Urban'])",
      Units_ = c(
        Income = "USD",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhIncome_Ma) <- 
      list(Units = "Base year dollars",
           Description = "Total annual income of households residing in urbanized area")
    
    #Average income per household
    #----------------------------
    HhAveIncPerHh_Ma <- HhIncome_Ma / HhNum_Ma
    attributes(HhAveIncPerHh_Ma) <- 
      list(Units = "Base year dollars per household",
           Description = "Average annual income of households residing in urbanized area")
    
    #Average income per person
    #-------------------------
    HhAveIncPerPrsn_Ma <- HhIncome_Ma / HhPop_Ma
    attributes(HhAveIncPerPrsn_Ma) <- 
      list(Units = "Base year dollars per person",
           Description = "Average annual income per person of households residing in urbanized area")
    
    #Average income per worker
    #-------------------------
    HhAveIncPerWkr_Ma <- HhIncome_Ma / HhWorkers_Ma
    attributes(HhAveIncPerWkr_Ma) <- 
      list(Units = "Base year dollars per worker",
           Description = "Average annual income per worker of households residing in urbanized area")
    
    #Number of drivers
    #-----------------
    HhDrivers_Ma <- summarizeDatasets(
      Expr = "sum(Drivers[LocType == 'Urban'])",
      Units_ = c(
        Drivers = "PRSN",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhDrivers_Ma) <- 
      list(Units = "Drivers",
           Description = "Number of drivers residing in urbanized area")
    
    #Average number of drivers per household
    #---------------------------------------
    HhAveDvrPerHh_Ma <- HhDrivers_Ma / HhNum_Ma
    attributes(HhAveDvrPerHh_Ma) <- 
      list(Units = "Drivers per household",
           Description = "Average number of drivers in households residing in urbanized area")
    
    #Average number of drivers per person
    #------------------------------------
    HhAveDvrPerPrsn_Ma <- HhDrivers_Ma / HhPop_Ma
    attributes(HhAveDvrPerPrsn_Ma) <- 
      list(Units = "Drivers per person",
           Description = "Average number of drivers per person residing in urbanized area")
    
    #Average number of drivers per worker
    #------------------------------------
    HhAveDvrPerWkr_Ma <- HhDrivers_Ma / HhWorkers_Ma
    attributes(HhAveDvrPerWkr_Ma) <- 
      list(Units = "Drivers per worker",
           Description = "Average number of drivers per worker residing in urbanized area")
    
    #Number of vehicles
    #------------------
    HhVehicles_Ma <- summarizeDatasets(
      Expr = "sum(NumAuto[LocType == 'Urban']) + sum(NumLtTrk[LocType == 'Urban'])",
      Units_ = c(
        NumAuto = "VEH",
        NumLtTrk = "VEH",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhVehicles_Ma) <- 
      list(Units = "Household light-duty vehicles",
           Description = "Total number of light-duty vehicles owned/leased by households residing in urbanized area")
    
    #Average number of vehicles per household
    #----------------------------------------
    HhAveVehPerHh_Ma <- HhVehicles_Ma / HhNum_Ma
    attributes(HhAveVehPerHh_Ma) <- 
      list(Units = "Household light-duty vehicles per household",
           Description = "Average number of light-duty vehicles owned/leased by households residing in urbanized area")
    
    #Average number of vehicles per person
    #-------------------------------------
    HhAveVehPerPrsn_Ma <- HhVehicles_Ma / HhPop_Ma
    attributes(HhAveVehPerPrsn_Ma) <- 
      list(Units = "Household light-duty vehicles per person",
           Description = "Average number of household light-duty vehicles per person residing in urbanized area")
    
    #Average number of vehicles per worker
    #-------------------------------------
    HhAveVehPerWkr_Ma <- HhVehicles_Ma / HhWorkers_Ma
    attributes(HhAveVehPerWkr_Ma) <- 
      list(Units = "Household light-duty vehicles per worker",
           Description = "Average number of household light-duty vehicles per worker residing in urbanized area")
    
    #Average number of vehicles per driver
    #-------------------------------------
    HhAveVehPerDvr_Ma <- HhVehicles_Ma / HhDrivers_Ma
    attributes(HhAveVehPerDvr_Ma) <- 
      list(Units = "Household light-duty vehicles per driver",
           Description = "Average number of household light-duty vehicles per driver residing in urbanized area")
    
    #Number of light trucks
    #----------------------
    HhLightTrucks_Ma <- summarizeDatasets(
      Expr = "sum(NumLtTrk[LocType == 'Urban'])",
      Units_ = c(
        NumLtTrk = "VEH",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    
    #Light-truck vehicle proportion
    #------------------------------
    HhLtTrkProp_Ma <- HhLightTrucks_Ma / HhVehicles_Ma
    attributes(HhLtTrkProp_Ma) <- 
      list(Units = "Light truck proportion of household vehicles",
           Description = "Light truck proportion of light-duty vehicles owned/leased by households residing in urbanized area")
    
    #Total daily work parking cost
    #-----------------------------
    HhTotDailyWkrParkingCost_Ma <- summarizeDatasets(
      Expr = "sum(ParkingCost[LocType == 'Urban'])",
      Units_ = c(
        ParkingCost = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = list(
        Worker = c("ParkingCost"),
        Household = c("Marea", "LocType")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhTotDailyWkrParkingCost_Ma) <-
      list(Units = "USD per day",
           Description = "Total daily work parking expenditures by households living in the urbanized portion of the Marea")
    
    #Total daily non-work parking cost
    #---------------------------------
    HhTotDailyOthParkingCost_Ma <- summarizeDatasets(
      Expr = "sum(OtherParkingCost[LocType == 'Urban'])",
      Units = c(
        OtherParkingCost = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhTotDailyOthParkingCost_Ma) <-
      list(Units = "USD per day",
           Description = "Total daily non-work parking expenditures by households living in the urbanized portion of the Marea")
    
    #Average daily household parking cost
    #------------------------------------
    HhAveDailyParkingCost_Ma <- 
      (HhTotDailyWkrParkingCost_Ma + HhTotDailyOthParkingCost_Ma) / HhNum_Ma
    attributes(HhAveDailyParkingCost_Ma) <-
      list(Units = "USD per day",
           Description = "Average daily parking expenditures by households living in the urbanized portion of the Marea")
    
    #Proportion of households that have reduced car ownership due to use of car services
    #-----------------------------------------------------------------------------------
    #All households
    PropHhReduceVehicleOwnership_Ma <- summarizeDatasets(
      Expr = "sum(OwnCostSavings > 0 & LocType == 'Urban') / count(OwnCostSavings[LocType == 'Urban'])",
      Units = c(
        OwnCostSavings = "",
        LocType = "",
        Marea = ""
      ),
      Table = list(
        Household = c("OwnCostSavings", "Marea"),
        Bzone = c("LocType")
      ),
      By_ = "Marea",
      Key = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(PropHhReduceVehicleOwnership_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in the urbanized portion of the Marea that reduce vehicle ownership due to car service availability")
    #Households in high density (>= 10,000 persons per square mile)
    PropHhHiDenReduceVehicleOwnership_Ma <- summarizeDatasets(
      Expr = "sum(OwnCostSavings > 0 & LocType == 'Urban' & D1B >= 10000) / count(OwnCostSavings[LocType == 'Urban' & D1B >= 10000])",
      Units = c(
        OwnCostSavings = "",
        LocType = "",
        Marea = "",
        D1B = "PRSN/SQMI"
      ),
      Table = list(
        Household = c("OwnCostSavings", "Marea"),
        Bzone = c("LocType", "D1B")
      ),
      By_ = "Marea",
      Key = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(PropHhHiDenReduceVehicleOwnership_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in the high density urbanized portion of the Marea that reduce vehicle ownership due to car service availability")
    #Households in medium density (>= 4,000 & < 10,000 persons per square mile)
    PropHhMedDenReduceVehicleOwnership_Ma <- summarizeDatasets(
      Expr = "sum(OwnCostSavings > 0 & LocType == 'Urban' & D1B >= 4000 & D1B < 10000) / count(OwnCostSavings[LocType == 'Urban' & D1B >= 4000 & D1B < 10000])",
      Units = c(
        OwnCostSavings = "",
        LocType = "",
        Marea = "",
        D1B = "PRSN/SQMI"
      ),
      Table = list(
        Household = c("OwnCostSavings", "Marea"),
        Bzone = c("LocType", "D1B")
      ),
      By_ = "Marea",
      Key = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(PropHhMedDenReduceVehicleOwnership_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in the medium density urbanized portion of the Marea that reduce vehicle ownership due to car service availability")
    
    #Proportion of households that have high level car service available to them
    #---------------------------------------------------------------------------
    PropHhWithHiCarSvc_Ma <- summarizeDatasets(
      Expr = "sum(NumHh[CarSvcLevel == 'High' & LocType == 'Urban']) / sum(NumHh[LocType == 'Urban'])",
      Units = c(
        NumHh = "",
        CarSvcLevel = "",
        LocType = "",
        Marea = ""
      ),
      Table = "Bzone",
      By_ = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(PropHhWithHiCarSvc_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in Bzones in the urbanized portion of the Marea that have high car service availability")
    
    #Data frame of household characteristics
    #---------------------------------------
    HhCharacteristics_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "MareaHhNum_Ma",
        "MareaHhPop_Ma",
        "MareaHhWorkers_Ma",
        "MareaHhIncome_Ma",
        "MareaHhDrivers_Ma",
        "MareaHhVehicles_Ma",
        "MareaHhLtTrkProp_Ma",
        "MareaHhAveVehAge_Ma",
        "MareaCarSvcLtTrkDvmtProp_Ma",
        "MareaCarSvcAveVehAge_Ma",
        "MareaComSvcLtTrkDvmtProp_Ma",
        "MareaComSvcAveVehAge_Ma",
        "HhNum_Ma",
        "HhPop_Ma",
        "HhAveSize_Ma",
        "HhWorkers_Ma",
        "HhAveNumWkr_Ma",
        "HhIncome_Ma",
        "HhAveIncPerHh_Ma",
        "HhAveIncPerPrsn_Ma",
        "HhAveIncPerWkr_Ma",
        "HhVehicles_Ma",
        "HhAveVehPerHh_Ma",
        "HhAveVehPerPrsn_Ma",
        "HhAveVehPerWkr_Ma",
        "HhAveVehPerDvr_Ma",
        "HhLtTrkProp_Ma",
        "HhTotDailyWkrParkingCost_Ma",
        "HhTotDailyOthParkingCost_Ma",
        "HhAveDailyParkingCost_Ma",
        "PropHhReduceVehicleOwnership_Ma",
        "PropHhHiDenReduceVehicleOwnership_Ma",
        "PropHhMedDenReduceVehicleOwnership_Ma",
        "PropHhWithHiCarSvc_Ma"
      ),
      Ma = Ma
    )

    
    #========================
    #LAND USE CHARACTERISTICS
    #========================

    #Average population density
    #--------------------------
    UrbanAvePopDen_Ma <- summarizeDatasets(
      Expr = "sum(UrbanPop) / sum(UrbanArea)",
      Units_ = c(
        UrbanArea = "ACRE",
        UrbanPop = "PRSN",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]	
    attributes(UrbanAvePopDen_Ma) <- list(
      Units = "Persons per acre",
      Description = "Average number of persons per acre in the urbanized area"
    )
    
    #Average population in towns
    #---------------------------
    TownAvePopDen_Ma <- summarizeDatasets(
      Expr = "sum(TownPop) / sum(TownArea)",
      Units_ = c(
        TownArea = "ACRE",
        TownPop = "PRSN",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]	
    attributes(TownAvePopDen_Ma) <- list(
      Units = "Persons per acre",
      Description = "Average number of persons per acre in towns"
    )
    
    #Mean Bzone population density
    #-----------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)){
      MeanBzonePopDen_Ma <- summarizeDatasets(
        Expr = "mean(D1B[LocType == 'Urban'])",
        Units_ = c(
          D1B = "PRSN/ACRE",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MeanBzonePopDen_Ma <- summarizeDatasets(
        Expr = "mean(UrbanPop / UrbanArea)",
        Units_ = c(
          UrbanPop = "PRSN",
          UrbanArea = "ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MeanBzonePopDen_Ma) <- list(
      Units = "Persons per acre",
      Description = "Mean Bzone population density in the urbanized area"
    )
    
    #Maximum Bzone population density
    #--------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MaxBzonePopDen_Ma <- summarizeDatasets(
        Expr = "max(D1B[LocType == 'Urban'])",
        Units_ = c(
          D1B = "PRSN/ACRE",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MaxBzonePopDen_Ma <- summarizeDatasets(
        Expr = "max(UrbanPop / UrbanArea)",
        Units_ = c(
          UrbanPop = "PRSN",
          UrbanArea = "ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MaxBzonePopDen_Ma) <- list(
      Units = "Persons per acre",
      Description = "Maximum Bzone population density in urbanized area"
    )
    
    #Median Bzone population density
    #-------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MedianBzonePopDen_Ma <- summarizeDatasets(
        Expr = "median(D1B[LocType == 'Urban'])",
        Units_ = c(
          D1B = "PRSN/ACRE",
          LocType = "Category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MedianBzonePopDen_Ma <- summarizeDatasets(
        Expr = "median(UrbanPop / UrbanArea)",
        Units_ = c(
          UrbanPop = "PRSN",
          UrbanArea = "ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MedianBzonePopDen_Ma) <- list(
      Units = "Persons per acre",
      Description = "Median Bzone population density in urbanized area"
    )
    
    #Average activity density
    #------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      AveActivityDen_Ma <- summarizeDatasets(
        Expr = "sum(NumHh[LocType == 'Urban'] + TotEmp[LocType == 'Urban']) / sum(UrbanArea)",
        Units_ = c(
          NumHh = "HH",
          TotEmp = "PRSN",
          UrbanArea = "ACRE",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]	
    } else {
      AveActivityDen_Ma <- summarizeDatasets(
        Expr = "sum(NumHh + TotEmp) / sum(UrbanArea)",
        Units_ = c(
          NumHh = "HH",
          TotEmp = "PRSN",
          UrbanArea = "ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(AveActivityDen_Ma) <- list(
      Units = "Households and jobs per acre",
      Description = "Average number of households and jobs per acre in the urbanized area"
    )
    
    #Mean Bzone activity density
    #---------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MeanBzoneActivityDen_Ma <- summarizeDatasets(
        Expr = "mean(D1D[LocType == 'Urban'])",
        Units_ = c(
          D1D = "HHJOB/ACRE",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MeanBzoneActivityDen_Ma <- summarizeDatasets(
        Expr = "mean(D1D)",
        Units_ = c(
          D1D = "HHJOB/ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MeanBzoneActivityDen_Ma) <- list(
      Units = "Households and Jobs per acre",
      Description = "Mean Bzone activity density in the urbanized area"
    )
    
    #Maximum Bzone activity density
    #------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MaxBzoneActivityDen_Ma <- summarizeDatasets(
        Expr = "max(D1D[LocType == 'Urban'])",
        Units_ = c(
          D1D = "HHJOB/ACRE",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MaxBzoneActivityDen_Ma <- summarizeDatasets(
        Expr = "max(D1D)",
        Units_ = c(
          D1D = "HHJOB/ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MaxBzoneActivityDen_Ma) <- list(
      Units = "Households and Jobs per acre",
      Description = "Maximum Bzone activity density in the urbanized area"
    )
    
    #Median Bzone activity density
    #-----------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MedianBzoneActivityDen_Ma <- summarizeDatasets(
        Expr = "median(D1D[LocType == 'Urban'])",
        Units_ = c(
          D1D = "HHJOB/ACRE",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma] 
    } else {
      MedianBzoneActivityDen_Ma <- summarizeDatasets(
        Expr = "median(D1D)",
        Units_ = c(
          D1D = "HHJOB/ACRE",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma] 
    }
    attributes(MedianBzoneActivityDen_Ma) <- list(
      Units = "Households and Jobs per acre",
      Description = "Median Bzone activity density in the urbanized area"
    )
    
    #Number of households in urban-mixed neighborhoods
    #-------------------------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      NumUrbanMixHh_Ma <- summarizeDatasets(
        Expr = "sum(IsUrbanMixNbrhd[LocType == 'Urban'])",
        Units_ = c(
          IsUrbanMixNbrhd = "",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      NumUrbanMixHh_Ma <- summarizeDatasets(
        Expr = "sum(IsUrbanMixNbrhd)",
        Units_ = c(
          IsUrbanMixNbrhd = "",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(NumUrbanMixHh_Ma) <- list(
      Units = "Households",
      Description = "Number of households residing in urban-mixed neighborhoods in urbanized area"
    )
    
    #Proportion of households in urban-mixed neighborhoods
    #-----------------------------------------------------
    PropUrbanMixHh_Ma <- NumUrbanMixHh_Ma / HhNum_Ma
    attributes(PropUrbanMixHh_Ma) <- list(
      Units = "Proportion of Households",
      Description = "Proportion of urbanized area households that reside in urban-mixed neighborhoods"
    )
    
    #Proportion of single-family dwelling units
    #------------------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      PropSFDU_Ma <- summarizeDatasets(
        Expr = "sum(SFDU[LocType == 'Urban']) / (sum(NumHh[LocType == 'Urban']))",
        Units = c(
          SFDU = "DU",
          NumHh = "HH",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      PropSFDU_Ma <- summarizeDatasets(
        Expr = "sum(SFDU) / (sum(NumHh))",
        Units = c(
          SFDU = "DU",
          NumHh = "HH",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(PropSFDU_Ma) <- list(
      Units = "Proportion of Households",
      Description = "Proportion of urbanized area households that reside in single-family dwellings"
    )
    
    #Mean TDM DVMT reduction proportion
    #----------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MeanTdmPropDvmtReduction_Ma <- summarizeDatasets(
        Expr = "mean(PropTdmDvmtReduction[LocType == 'Urban'])",
        Units = c(
          PropTdmDvmtReduction = "proportion",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MeanTdmPropDvmtReduction_Ma <- summarizeDatasets(
        Expr = "mean(PropTdmDvmtReduction)",
        Units = c(
          PropTdmDvmtReduction = "proportion",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MeanTdmPropDvmtReduction_Ma) <- list(
      Units = "Proportion of household DVMT",
      Description = "Average proportional reduction of DVMT of urbanized households due to travel demand management"
    )
    
    #Maximum TDM DVMT reduction proportion
    #-------------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      MaxTdmPropDvmtReduction_Ma <- summarizeDatasets(
        Expr = "max(PropTdmDvmtReduction[LocType == 'Urban'])",
        Units = c(
          PropTdmDvmtReduction = "proportion",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      MaxTdmPropDvmtReduction_Ma <- summarizeDatasets(
        Expr = "max(PropTdmDvmtReduction)",
        Units = c(
          PropTdmDvmtReduction = "proportion",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(MaxTdmPropDvmtReduction_Ma) <- list(
      Units = "Proportion of household DVMT",
      Description = "Maximum proportional reduction of DVMT of Marea households due to travel demand management"
    )
    
    #Proportion of households participating in individualized marketing TDM
    #----------------------------------------------------------------------
    if (isDatasetPresent("LocType", "Bzone", Year, QPrep_ls)) {
      PropImpHouseholds_Ma <- summarizeDatasets(
        Expr = "sum(IsIMP[LocType == 'Urban']) / count(IsIMP[LocType == 'Urban'])",
        Units = c(
          IsIMP = "binary",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    } else {
      PropImpHouseholds_Ma <- summarizeDatasets(
        Expr = "sum(IsIMP) / count(IsIMP)",
        Units = c(
          IsIMP = "binary",
          LocType = "category",
          Marea = ""
        ),
        By_ = "Marea",
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma]
    }
    attributes(PropImpHouseholds_Ma) <- list(
      Units = "Proportion of households",
      Description = "Proportion of Marea households that participate in individualized marketing program for travel demand management"
    )
    
    #Proportion of workers participating in employee commute options
    #---------------------------------------------------------------
    MareaPropEcoWorkers_Ma <- summarizeDatasets(
      Expr = "sum(IsECO) / count(IsECO)",
      Units = c(
        IsECO = "binary",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Worker",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaPropEcoWorkers_Ma) <- list(
      Units = "Proportion of workers",
      Description = "Proportion of Marea workers in employee commute options program"
    )
    
    #Proportion of workers paying for parking
    #----------------------------------------
    MareaPropWkrPayForPkg_Ma <- summarizeDatasets(
      Expr = "sum(PaysForParking) / count(PaysForParking)",
      Units = c(
        PaysForParking = "binary",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Worker",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaPropWkrPayForPkg_Ma) <- list(
      Units = "Proportion of workers",
      Description = "Proportion of Marea workers who pay for parking"
    )
    
    #Proportion of workers participating in cashout parking program
    #--------------------------------------------------------------
    MareaPropWkrCashoutPkg_Ma <- summarizeDatasets(
      Expr = "sum(IsCashOut) / count(IsCashOut)",
      Units = c(
        IsCashOut = "binary",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Worker",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaPropWkrCashoutPkg_Ma) <- list(
      Units = "Proportion of workers",
      Description = "Proportion of Marea workers in cashout parking program"
    )
    
    #Calculate households and workers by area type if VE-State model
    #---------------------------------------------------------------    
    if (checkDataset("AreaType", "Bzone", Year, QPrep_ls$Listing$Datastore$Datastore)) {
      
      #Proportion of Marea households by area type
      #-------------------------------------------
      MareaHhByAreaType_MaAt <- summarizeDatasets(
        Expr = "count(AreaType)",
        Units_ = c(
          AreaType =  "",
          Marea = ""
        ),
        By_ = c("Marea", "AreaType"),
        Table = list(
          Bzone = "AreaType",
          Household = "Marea"
        ),
        Key = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma,]
      MareaPropHhByAreaType_MaAt <-
        sweep(MareaHhByAreaType_MaAt, 1, rowSums(MareaHhByAreaType_MaAt), "/")
      MareaCenterHhProp_Ma <- MareaPropHhByAreaType_MaAt[,"center"]
      attributes(MareaCenterHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of Marea households living in center area type"
      )
      MareaInnerHhProp_Ma <- MareaPropHhByAreaType_MaAt[,"inner"]
      attributes(MareaInnerHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of Marea households living in inner area type"
      )
      MareaOuterHhProp_Ma <- MareaPropHhByAreaType_MaAt[,"outer"]
      attributes(MareaOuterHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of Marea households living in outer area type"
      )
      MareaFringeHhProp_Ma <- MareaPropHhByAreaType_MaAt[,"fringe"]
      attributes(MareaFringeHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of Marea households living in fringe area type"
      )
      
      #Proportion of urbanized area households by area type
      #----------------------------------------------------
      HhByAreaType_MaAt <- summarizeDatasets(
        Expr = "count(AreaType[LocType == 'Urban'])",
        Units_ = c(
          AreaType =  "",
          Marea = "",
          LocType = ""
        ),
        By_ = c("Marea", "AreaType"),
        Table = list(
          Bzone = "AreaType",
          Household = c("Marea", "LocType")
        ),
        Key = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma,]
      PropHhByAreaType_MaAt <-
        sweep(HhByAreaType_MaAt, 1, rowSums(HhByAreaType_MaAt), "/")
      CenterHhProp_Ma <- PropHhByAreaType_MaAt[,"center"]
      attributes(CenterHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of urbanized area households living in center area type"
      )
      InnerHhProp_Ma <- PropHhByAreaType_MaAt[,"inner"]
      attributes(InnerHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of urbanized area households living in inner area type"
      )
      OuterHhProp_Ma <- PropHhByAreaType_MaAt[,"outer"]
      attributes(OuterHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of urbanized area households living in outer area type"
      )
      FringeHhProp_Ma <- PropHhByAreaType_MaAt[,"fringe"]
      attributes(FringeHhProp_Ma) <- list(
        Units = "Proportion of households",
        Description = "Proportion of households living in fringe area type"
      )
      
      #Proportion of Marea workers by area type
      #----------------------------------------
      MareaWkrByAreaType_MaAt <- summarizeDatasets(
        Expr = "count(AreaType)",
        Units_ = c(
          AreaType =  "",
          Marea = ""
        ),
        By_ = c("Marea", "AreaType"),
        Table = list(
          Bzone = "AreaType",
          Worker = "Marea"
        ),
        Key = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma,]
      MareaPropWkrByAreaType_MaAt <-
        sweep(MareaWkrByAreaType_MaAt, 1, rowSums(MareaWkrByAreaType_MaAt), "/")
      MareaCenterWkrProp_Ma <- MareaPropWkrByAreaType_MaAt[,"center"]
      attributes(MareaCenterWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of Marea workers working in center area type"
      )
      MareaInnerWkrProp_Ma <- MareaPropWkrByAreaType_MaAt[,"inner"]
      attributes(MareaInnerWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of Marea workers working in inner area type"
      )
      MareaOuterWkrProp_Ma <- MareaPropWkrByAreaType_MaAt[,"outer"]
      attributes(MareaOuterWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of Marea workers working in outer area type"
      )
      MareaFringeWkrProp_Ma <- MareaPropWkrByAreaType_MaAt[,"fringe"]
      attributes(MareaFringeWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of Marea workers working in fringe area type"
      )
      
      #Proportion of urbanized area workers by area type
      #-------------------------------------------------
      WkrByAreaType_MaAt <- summarizeDatasets(
        Expr = "count(AreaType[LocType == 'Urban'])",
        Units_ = c(
          AreaType =  "",
          LocType = "",
          Marea = ""
        ),
        By_ = c("Marea", "AreaType"),
        Table = list(
          Bzone = c("AreaType", "LocType"),
          Worker = "Marea"
        ),
        Key = "Bzone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )[Ma,]
      PropWkrByAreaType_MaAt <-
        sweep(WkrByAreaType_MaAt, 1, rowSums(WkrByAreaType_MaAt), "/")
      CenterWkrProp_Ma <- PropWkrByAreaType_MaAt[,"center"]
      attributes(CenterWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of urbanized area workers working in center area type"
      )
      InnerWkrProp_Ma <- PropWkrByAreaType_MaAt[,"inner"]
      attributes(InnerWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of urbanized area workers working in inner area type"
      )
      OuterWkrProp_Ma <- PropWkrByAreaType_MaAt[,"outer"]
      attributes(OuterWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of urbanized area workers working in outer area type"
      )
      FringeWkrProp_Ma <- PropWkrByAreaType_MaAt[,"fringe"]
      attributes(FringeWkrProp_Ma) <- list(
        Units = "Proportion of workers",
        Description = "Proportion of urbanized area workers working in fringe area type"
      )
      
    }
    
    #Data frame of land use characteristics
    #--------------------------------------
    if (checkDataset("AreaType", "Bzone", Year, QPrep_ls$Listing$Datastore$Datastore)) {
      LuCharacteristics_df <- makeMeasureDataFrame(
        DataNames_ = c(
          "UrbanAvePopDen_Ma",
          "TownAvePopDen_Ma",
          "MeanBzonePopDen_Ma",
          "MaxBzonePopDen_Ma",
          "MedianBzonePopDen_Ma",
          "AveActivityDen_Ma",
          "MeanBzoneActivityDen_Ma",
          "MaxBzoneActivityDen_Ma",
          "MedianBzoneActivityDen_Ma",
          "PropUrbanMixHh_Ma",
          "PropSFDU_Ma",
          "MeanTdmPropDvmtReduction_Ma",
          "MaxTdmPropDvmtReduction_Ma",
          "PropImpHouseholds_Ma",
          "CenterHhProp_Ma",
          "InnerHhProp_Ma",
          "OuterHhProp_Ma",
          "FringeHhProp_Ma",
          "CenterWkrProp_Ma",
          "InnerWkrProp_Ma",
          "OuterWkrProp_Ma",
          "FringeWkrProp_Ma",
          "MareaPropEcoWorkers_Ma",
          "MareaPropWkrPayForPkg_Ma",
          "MareaPropWkrCashoutPkg_Ma",
          "MareaCenterHhProp_Ma",
          "MareaInnerHhProp_Ma",
          "MareaOuterHhProp_Ma",
          "MareaFringeHhProp_Ma",
          "MareaCenterWkrProp_Ma",
          "MareaInnerWkrProp_Ma",
          "MareaOuterWkrProp_Ma",
          "MareaFringeWkrProp_Ma"
        ),
        Ma = Ma
      )
    } else {
      LuCharacteristics_df <- makeMeasureDataFrame(
        DataNames_ = c(
          "UrbanAvePopDen_Ma",
          "TownAvePopDen_Ma",
          "MeanBzonePopDen_Ma",
          "MaxBzonePopDen_Ma",
          "MedianBzonePopDen_Ma",
          "AveActivityDen_Ma",
          "MeanBzoneActivityDen_Ma",
          "MaxBzoneActivityDen_Ma",
          "MedianBzoneActivityDen_Ma",
          "PropUrbanMixHh_Ma",
          "PropSFDU_Ma",
          "MeanTdmPropDvmtReduction_Ma",
          "MaxTdmPropDvmtReduction_Ma",
          "PropImpHouseholds_Ma",
          "MareaPropEcoWorkers_Ma",
          "MareaPropWkrPayForPkg_Ma",
          "MareaPropWkrCashoutPkg_Ma"
        ),
        Ma = Ma
      )
    }
    
    
    #============================    
    #Daily Vehicle Miles Traveled
    #============================
    
    #Marea commercial service vehicle DVMT
    #-------------------------------------
    MareaComSvcDvmt_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanDvmt + ComSvcTownDvmt + ComSvcRuralDvmt)",
      Units = c(
        ComSvcUrbanDvmt = "MI/DAY",
        ComSvcTownDvmt = "MI/DAY",
        ComSvcRuralDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaComSvcDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the Marea"
    )
    
    #Marea public transit 'van' DVMT
    #-------------------------------
    MareaVanDvmt_Ma <- summarizeDatasets(
      Expr = "sum(VanDvmt)",
      Units = c(
        VanDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaVanDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by on-demand transit vans in the Marea."
    )
    
    #Marea household DVMT
    #--------------------
    MareaHhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(UrbanHhDvmt + TownHhDvmt + RuralHhDvmt)",
      Units_ = c(
        UrbanHhDvmt = "MI/DAY",
        TownHhDvmt = "MI/DAY",
        RuralHhDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in the Marea"
    )
    
    #Marea light-duty vehicle DVMT
    #-----------------------------
    MareaLdvDvmt_Ma <- MareaHhDvmt_Ma + MareaVanDvmt_Ma + MareaComSvcDvmt_Ma
    attributes(MareaLdvDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Sum of daily vehicle miles traveled by households residing in the Marea, commercial service travel attributable to the demand of Marea households and businesses, and on-demand transit van travel in the Marea."
    )
    
    #Marea car service proportion of household DVMT
    #----------------------------------------------
    MareaCarSvcPropHhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(Dvmt[VehicleAccess != 'Own'] * DvmtProp[VehicleAccess != 'Own']) / sum(Dvmt * DvmtProp)",
      Units_ = c(
        Dvmt = "",
        VehicleAccess = "",
        DvmtProp = "",
        Marea = ""
      ),
      By_ <- "Marea",
      Table = list(
        Household = c("Dvmt", "Marea"),
        Vehicle = c("VehicleAccess", "DvmtProp")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaCarSvcPropHhDvmt_Ma) <- list(
      Units = "Proportion of DVMT",
      Description = "Proportion of the DVMT of households residing in the Marea using car services"
    )

    #Urbanized area car service proportion of household DVMT
    #-------------------------------------------------------
    CarSvcPropHhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(Dvmt[VehicleAccess != 'Own' & LocType == 'Urban'] * DvmtProp[VehicleAccess != 'Own' & LocType == 'Urban']) / sum(Dvmt[LocType == 'Urban'] * DvmtProp[LocType == 'Urban'])",
      Units_ = c(
        Dvmt = "",
        VehicleAccess = "",
        DvmtProp = "",
        Marea = "",
        LocType = ""
      ),
      By_ <- "Marea",
      Table = list(
        Household = c("Dvmt", "Marea", "LocType"),
        Vehicle = c("VehicleAccess", "DvmtProp")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(CarSvcPropHhDvmt_Ma) <- list(
      Units = "Proportion of DVMT",
      Description = "Proportion of the DVMT of households residing in the urbanized area using car services"
    )
    
    #Urbanized area commercial service vehicle DVMT
    #----------------------------------------------
    ComSvcDvmt_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanDvmt)",
      Units = c(
        ComSvcUrbanDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(ComSvcDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the urbanized area"
    )
    
    #Urbanized area public transit 'van' DVMT
    #----------------------------------------
    VanDvmt_Ma <- summarizeDatasets(
      Expr = "sum(VanDvmt)",
      Units = c(
        VanDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(VanDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by on-demand transit vans in the urbanized area."
    )
    
    #Urbanized area household DVMT
    #-----------------------------
    HhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(UrbanHhDvmt)",
      Units_ = c(
        UrbanHhDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in the urbanized area"
    )
    
    #Urbanized area light-duty vehicle DVMT
    #--------------------------------------
    LdvDvmt_Ma <- HhDvmt_Ma + VanDvmt_Ma + ComSvcDvmt_Ma
    attributes(LdvDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Sum of daily vehicle miles traveled by households residing in the urbanized area, commercial service travel attributable to the demand of urbanized area households and businesses, and on-demand transit van travel in the urbanized area."
    )
    
    #Urban roadway light-duty vehicle DVMT
    #-------------------------------------
    LdvRoadDvmt_Ma <- summarizeDatasets(
      Expr = "sum(LdvFwyDvmt + LdvArtDvmt + LdvOthDvmt)",
      Units = c(
        LdvFwyDvmt = "MI/DAY",
        LdvArtDvmt = "MI/DAY",
        LdvOthDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(LdvRoadDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by light-duty vehicles on roadways within the urbanized area"
    )
    
    #Urbanized area household DVMT per household
    #-------------------------------------------
    AveHhDvmtPerHh_Ma <- HhDvmt_Ma / HhNum_Ma
    attributes(AveHhDvmtPerHh_Ma) <- list(
      Units = "Miles per day per household",
      Description = "Average daily vehicle miles traveled per household residing within the urbanized area"
    )
    
    #Urbanized area household DVMT per person
    #----------------------------------------
    AveHhDvmtPerPrsn_Ma <- HhDvmt_Ma / HhPop_Ma
    attributes(AveHhDvmtPerHh_Ma) <- list(
      Units = "Miles per day per person",
      Description = "Average daily household vehicle miles of households residing within the urbanized area per person"
    )
    
    #Urbanized area household DVMT per driver
    #----------------------------------------
    AveHhDvmtPerDvr_Ma <- HhDvmt_Ma / HhDrivers_Ma
    attributes(AveHhDvmtPerDvr_Ma) <- list(
      Units = "Miles per day per driver",
      Description = "Average daily household vehicle miles of households residing within the urbanized area per driver"
    )
    
    #Urbanized area household DVMT per vehicle
    #-----------------------------------------
    AveHhDvmtPerVeh_Ma <- HhDvmt_Ma / HhVehicles_Ma
    attributes(AveHhDvmtPerVeh_Ma) <- list(
      Units = "Miles per day per vehicle",
      Description = "Average daily household vehicle miles of households residing within the urbanized area per household vehicle"
    )
    
    #Urbanized area light-duty vehicle DVMT per household
    #----------------------------------------------------
    AveLdvDvmtPerHh_Ma <- LdvDvmt_Ma / HhNum_Ma
    attributes(AveLdvDvmtPerHh_Ma) <- list(
      Units = "Miles per day per household",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per household"
    )
    
    #Urbanized area light-duty vehicle DVMT per person
    #-------------------------------------------------
    AveLdvDvmtPerPrsn_Ma <- LdvDvmt_Ma / HhPop_Ma
    attributes(AveLdvDvmtPerPrsn_Ma) <- list(
      Units = "Miles per day per person",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per person"
    )
    
    #Urbanized area light-duty vehicle DVMT per driver
    #-------------------------------------------------
    AveLdvDvmtPerDvr_Ma <- LdvDvmt_Ma / HhDrivers_Ma
    attributes(AveLdvDvmtPerDvr_Ma) <- list(
      Units = "Miles per day per driver",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per driver"
    )
    
    #Urbanized area light-duty vehicle DVMT per vehicle
    #--------------------------------------------------
    AveLdvDvmtPerVeh_Ma <- LdvDvmt_Ma / HhVehicles_Ma
    attributes(AveLdvDvmtPerVeh_Ma) <- list(
      Units = "Miles per day per vehicle",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per vehicle"
    )
    
    #Ratio of urbanized area household DVMT to light-duty DVMT
    #---------------------------------------------------------
    PropHhDvmt_Ma <- HhDvmt_Ma / LdvDvmt_Ma
    attributes(PropHhDvmt_Ma) <- list(
      Units = "Proportion of LDV DVMT",
      Description = "Household DVMT proportion of light-duty vehicle DVMT attributable to urbanized area households and businesses"
    )
    
    #Ratio of urbanized area commercial service DVMT to light-duty DVMT
    #------------------------------------------------------------------
    PropComSvcDvmt_Ma <- ComSvcDvmt_Ma / LdvDvmt_Ma
    attributes(PropComSvcDvmt_Ma) <- list(
      Units = "Proportion of LDV DVMT",
      Description = "Commercial service DVMT proportion of light-duty vehicle DVMT attributable to urbanized area households and businesses"
    )
    
    #Ratio of urbanized area public transit van DVMT to light-duty DVMT
    #------------------------------------------------------------------
    PropVanDvmt_Ma <- VanDvmt_Ma / LdvDvmt_Ma
    PropComSvcDvmt_Ma <- ComSvcDvmt_Ma / LdvDvmt_Ma
    attributes(PropVanDvmt_Ma) <- list(
      Units = "Proportion of LDV DVMT",
      Description = "Public transit van DVMT proportion of light-duty vehicle DVMT attributable to urbanized area households and businesses"
    )
    
    #Data frame of DVMT values
    #-------------------------
    Dvmt_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "MareaHhDvmt_Ma",
        "MareaComSvcDvmt_Ma",
        "MareaVanDvmt_Ma",
        "MareaLdvDvmt_Ma",
        "MareaCarSvcPropHhDvmt_Ma",
        "HhDvmt_Ma",
        "ComSvcDvmt_Ma",
        "VanDvmt_Ma",
        "LdvDvmt_Ma",
        "LdvRoadDvmt_Ma",
        "AveHhDvmtPerHh_Ma",
        "AveHhDvmtPerPrsn_Ma",
        "AveHhDvmtPerDvr_Ma",
        "AveHhDvmtPerVeh_Ma",
        "AveLdvDvmtPerHh_Ma",
        "AveLdvDvmtPerPrsn_Ma",
        "AveLdvDvmtPerDvr_Ma",
        "AveLdvDvmtPerVeh_Ma",
        "PropHhDvmt_Ma",
        "PropComSvcDvmt_Ma",
        "PropVanDvmt_Ma",
        "CarSvcPropHhDvmt_Ma"
      ),
      Ma = Ma
    )


    #===========================================================        
    #FUEL CONSUMPTION AND CO2E PRODUCTION OF LIGHT-DUTY VEHICLES
    #===========================================================

    #Household fuel consumption for Marea
    #------------------------------------
    MareaHhGGE_Ma <- summarizeDatasets(
      Expr = "sum(DailyGGE)",
      Units = c(
        DailyGGE = "GGE/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for the travel of households residing in the Marea"
    )
    
    #Commercial service fuel consumption for Marea
    #---------------------------------------------
    MareaComSvcGGE_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanGGE + ComSvcNonUrbanGGE)",
      Units = c(
        ComSvcUrbanGGE = "GGE/DAY",
        ComSvcNonUrbanGGE = "GGE/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaComSvcGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for commercial services vehicle travel arising from households and businesses located in the Marea"
    )
    
    #Public transit van fuel consumption for Marea
    #---------------------------------------------
    MareaVanGGE_Ma <- summarizeDatasets(
      Expr = "sum(VanGGE)",
      Units = c(
        VanGGE = "GGE/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaVanGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for public transit van in the Marea"
    )
    
    #Light-duty vehicle fuel consumption for Marea
    #---------------------------------------------
    MareaLdvGGE_Ma <- MareaHhGGE_Ma + MareaComSvcGGE_Ma + MareaVanGGE_Ma
    attributes(MareaVanGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for light-duty vehicle travel attributable to households and businesses in the Marea"
    )
    
    #Household CO2e for Marea
    #------------------------
    MareaHhCO2e_Ma <- summarizeDatasets(
      Expr = "sum(DailyCO2e)",
      Units = c(
        DailyCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaHhCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel by households residing in the Marea"
    )
    
    #Commercial service CO2e for Marea
    #---------------------------------
    MareaComSvcCO2e_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanCO2e + ComSvcNonUrbanCO2e)",
      Units = c(
        ComSvcUrbanCO2e = "MT/YR",
        ComSvcNonUrbanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaComSvcCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from commercial service light-duty vehicle travel attributable to households and businesses in the Marea"
    )
    
    #Van CO2e for Marea
    #------------------
    MareaVanCO2e_Ma <- summarizeDatasets(
      Expr = "sum(VanCO2e)",
      Units = c(
        VanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(MareaVanCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from public transit van travel in the Marea"
    )
    
    #Light-duty vehicle CO2e for Marea
    #---------------------------------
    MareaLdvCO2e_Ma <- MareaHhCO2e_Ma + MareaVanCO2e_Ma + MareaComSvcCO2e_Ma
    attributes(MareaLdvCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the Marea"
    )
    
    #Light-duty vehicle CO2e per person for Marea
    #--------------------------------------------
    MareaLdvCO2ePerPrsn_Ma <- MareaLdvCO2e_Ma / MareaHhPop_Ma
    attributes(MareaLdvCO2ePerPrsn_Ma) <- list(
      Units = "Metric tons CO2e per year per person",
      Description = "Average per capita annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the Marea"
    )

    #Household CO2e for urbanized area
    #---------------------------------
    HhCO2e_Ma <- summarizeDatasets(
      Expr = "sum(DailyCO2e[LocType == 'Urban'])",
      Units = c(
        DailyCO2e = "MT/YR",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(HhCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel by households residing in the urbanized area"
    )
    
    #Commercial service CO2e for urbanized area
    #------------------------------------------
    ComSvcCO2e_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanCO2e)",
      Units = c(
        ComSvcUrbanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(ComSvcCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from commercial service light-duty vehicle travel attributable to households and businesses in the urbanized area"
    )
    
    #Van CO2e for urbanized area
    #---------------------------
    VanCO2e_Ma <- summarizeDatasets(
      Expr = "sum(VanCO2e)",
      Units = c(
        VanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )[Ma]
    attributes(VanCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from public transit van travel in the urbanized area"
    )
    
    #Light-duty vehicle CO2e for urbanized area
    #------------------------------------------
    LdvCO2e_Ma <- HhCO2e_Ma + VanCO2e_Ma + ComSvcCO2e_Ma
    attributes(LdvCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the urbanized area"
    )
    
    #Light-duty vehicle CO2e per capita for urbanized area
    #-----------------------------------------------------
    LdvCO2ePerPrsn_Ma <- LdvCO2e_Ma / HhPop_Ma
    attributes(LdvCO2ePerPrsn_Ma) <- list(
      Units = "Metric tons CO2e per year per person",
      Description = "Average per capita annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the urbanized area"
    )
    
    #Household CO2e rate for urbanized area
    #--------------------------------------
    HhCO2eRate_Ma <- (1e6 * HhCO2e_Ma) / (365 * HhDvmt_Ma)
    attributes(HhCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of vehicle travel by households residing in the urbanized area"
    )
    
    #Commercial service CO2e rate for urbanized area
    #-----------------------------------------------
    ComSvcCO2eRate_Ma <- (1e6 * ComSvcCO2e_Ma) / (365 * ComSvcDvmt_Ma)
    attributes(ComSvcCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of commercial service vehicle travel attributable to households and businesses in the urbanized area"
    )
    
    #Van CO2e rate for urbanized area
    #--------------------------------
    VanCO2eRate_Ma <- (1e6 * VanCO2e_Ma) / (365 * VanDvmt_Ma)
    attributes(VanCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of public transit van travel in the urbanized area"
    )
    
    #Light-duty vehicle CO2e rate
    #----------------------------
    LdvCO2eRate_Ma <- (1e6 * LdvCO2e_Ma) / (365 * LdvDvmt_Ma)
    attributes(LdvCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of light-duty vehicle travel attributable to households and businesses in the urbanized area"
    )
    
    #Data frame of fuel and CO2e values
    #----------------------------------
    CO2e_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "MareaHhGGE_Ma",
        "MareaComSvcGGE_Ma",
        "MareaVanGGE_Ma",
        "MareaLdvGGE_Ma",
        "MareaHhCO2e_Ma",
        "MareaComSvcCO2e_Ma",
        "MareaVanCO2e_Ma",
        "MareaLdvCO2e_Ma",
        "MareaLdvCO2ePerPrsn_Ma",
        "HhCO2e_Ma",
        "ComSvcCO2e_Ma",
        "VanCO2e_Ma",
        "LdvCO2e_Ma",
        "LdvCO2ePerPrsn_Ma",
        "HhCO2eRate_Ma",
        "ComSvcCO2eRate_Ma",
        "VanCO2eRate_Ma",
        "LdvCO2eRate_Ma"
        ),
      Ma = Ma
      )
    

    #Return data frame of all results
    #--------------------------
    rbind(
      HhCharacteristics_df,
      LuCharacteristics_df,
      Dvmt_df,
      CO2e_df
    )
  }
