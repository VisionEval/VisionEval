getModuleL <- function(ModuleName, PackageName, RunFor, RunYear, Year, StopOnErr = TRUE) {
  #Check whether the module should be run for the current run year
  #---------------------------------------------------------------
  BaseYear <- getModelState()$BaseYear
  if (RunYear == BaseYear & RunFor == "NotBaseYear") {
    return()
  }
  if (RunYear != BaseYear & RunFor == "BaseYear") {
    return()
  }
  #Log and print starting message
  #------------------------------
  Msg <-
    paste0(Sys.time(), " -- Starting module '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)
  #Load the package and module
  #---------------------------
  Function <- paste0(PackageName, "::", ModuleName)
  Specs <- paste0(PackageName, "::", ModuleName, "Specifications")
  M <- list()
  M$Func <- eval(parse(text = Function))
  M$Specs <- processModuleSpecs(eval(parse(text = Specs)))
  #Load any modules identified by 'Call' spec if any
  if (is.list(M$Specs$Call)) {
    Call <- list(
      Func = list(),
      Specs = list()
    )
    for (Alias in names(M$Specs$Call)) {
      #Called module function when specified as package::module
      Function <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(Function, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == Function) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      Specs <- paste0(Function, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = Function))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
      Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
    }
  }
  #Initialize vectors to store module errors and warnings
  #------------------------------------------------------
  Errors_ <- character(0)
  Warnings_ <- character(0)
  #Get module data
  #---------------
  if (M$Specs$RunBy == "Region") {
    #Get data from datastore
    L <- getFromDatastore(M$Specs, RunYear = Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
      }
    }
  } else {
    #Identify the units of geography to iterate over
    GeoCategory <- M$Specs$RunBy
    #Create the geographic index list
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
      }
    }
    #Run module for each geographic area
    Geo_ <- readFromTable(GeoCategory, GeoCategory, RunYear)
    L <- list()
    for (Geo in Geo_) {
      #Get data from datastore for geographic area
      L$Geo <-
        getFromDatastore(M$Specs, RunYear, Geo, GeoIndex_ls)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
            getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
        }
      }
    }
  }
  #Return L
  #--------
  L
}
