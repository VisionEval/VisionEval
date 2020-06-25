# Define a single object (list), not nessarily named PMSpecifications
# that contains a series of lists that are specifications

PMSpecifications <- list(

  #### DVMT
  #Urban area household DVMT
  #--------------------
  list(
    Name = "UrbanHhDvmt_Ma",
    Summarize = list(
      Expr = "sum(UrbanHhDvmt )",
      Units_ = c(
        UrbanHhDvmt = "MI/DAY",
        Marea = ""
      ),
      By = "Marea",
      Table = "Marea"
    ),
    Units = "Miles per day",
    Description = "Daily vehicle miles traveled by households residing in the urban area"
  )
  ,

  #Urban area household DVMT in Multnomah county
  list(
    Name = "UrbanHhDvmt_MaAz",
    Summarize = list(
      Expr = "sum(Dvmt[Azone == 'Multnomah' & LocType == 'Urban'] )",
      Units_ = c(
        Dvmt = "MI/DAY",
        LocType = "",
        Azone = "",
        Marea = ""
      ),
      By = "Marea",
      Table = "Household"
    ),
    Units = "Miles per day",
    Description = "Daily vehicle miles traveled by households residing in the urban area in Multnomah county"
  ),

  #Urban area household DVMT in mix used in Multnomah county
  list(
    Name = "UrbanHhDvmt_MaAzMx",
    Summarize = list(
      Expr = "sum(Dvmt[Azone == 'Multnomah'& LocType == 'Urban'&  IsUrbanMixNbrhd == '1'] )",
      Units_ = c(
        Dvmt = "MI/DAY",
        LocType = "",
        Azone = "",
        IsUrbanMixNbrhd = "",
        Marea = ""
      ),
      By = "Marea",
      Table = "Household"
    ),
    Units = "Miles per day",
    Description = "Daily vehicle miles traveled by households residing in mixed use in the urban area in Multnomah county"
  ),

  #Urban area public transit 'van' DVMT
  #-------------------------------
  list(
    Name = "UrbanVanDvmt_Ma",
    Summarize = list(
      Expr = "sum(VanDvmt)",
      Units = c(
        VanDvmt = "MI/DAY",
        Marea = ""
      ),
      By = "Marea",
      Table = "Marea"
    ),
    Units = "Miles per day",
    Description = "Daily vehicle miles traveled by on-demand transit vans in the Urban area."
  ),

  #Urban area commercial service vehicle DVMT
  #-------------------------------------
  list(
    Name = "UrbanComSvcDvmt_Ma",
    Summarize = list(
      Expr = "sum(ComSvcUrbanDvmt)",
      Units = c(
        ComSvcUrbanDvmt = "MI/DAY",
        Marea = ""
      ),
      By = "Marea",
      Table = "Marea"
    ),
    Units = "Miles per day",
    Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the urban area"
  ),

  #Urban area light-duty vehicle DVMT
  list(
    Name = "UrbanLdvDvmt_Ma",
    Function = "UrbanHhDvmt_Ma + UrbanVanDvmt_Ma + UrbanComSvcDvmt_Ma",
    Units = "Miles per day",
    Description = "Sum of daily vehicle miles traveled by households residing in the urban area, commercial service travel attributable to the demand of urban area households and businesses, and on-demand transit van travel in the urban area."
  ),

  #Urban Population in Marea
  #-------------------
  list(
    Name = "UrbanHhPop_Ma",
    Summarize = list(
      Expr = "sum(HhSize[LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Marea = ""
      ),
      By = "Marea",
      Table = "Household"
    ),
    Units = "Persons",
    Description = "Number of persons residing in urban area"
  ),

  #Urban Population in Multnomah county
  #-------------------
  list(
    Name = "UrbanHhPop_MaAz",
    Summarize = list(
      Expr = "sum(HhSize[Azone == 'Multnomah' & LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Azone = "",
        Marea = ""
      ),
      By = "Marea",
      Table = "Household"
    ),
    Units = "Persons",
    Description = "Number of persons residing in urban area in Multnomah county"
  ),

  #Urban Population in mixed use in Multnomah county
  #-------------------
  list(
    Name = "UrbanHhPop_MaAzMx",
    Summarize = list(
      Expr = "sum(HhSize[Azone == 'Multnomah'& LocType == 'Urban'& IsUrbanMixNbrhd == '1'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Azone = "",
        IsUrbanMixNbrhd = "",
        Marea = ""
      ),
      By = "Marea",
      Table = "Household"
    ),
    Units = "Persons",
    Description = "Number of persons residing in mixed use in urban area in Multnomah county"
  ),

  #Urban Population in mixed use in Multnomah county, broken out by income
  #-------------------
  list(
    Name = "UrbanHhPopLowMFI_Ma",
    Summarize = list(
      Expr = "sum(HhSize[LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        PctMedianIncome = "",
        Marea = ""
      ),
      By = c(
        "PctMedianIncome",
        "Marea"),
      Breaks = list(
        PctMedianIncome = c(0.30, 0.50, 0.80, 1.00, 1.20, 2.00)
      ),
      BreakNames = list(
        PctMedianIncome = c("30pct", "50pct", "80pct", "100pct", "120pct","200pct")
      ),
      Table = "Household"
    ),
    Units = "Persons",
    Description = "Number of persons in households residing in urban area by Percent Median Family Income"
  ),

  #DVMT per Capita in Marea
  list(
    Name = "UrbanLdvDmvtPerCap_Ma",
    Function = "UrbanLdvDvmt_Ma / UrbanHhPop_Ma",
    Units = "Dvmt per person",
    Description = "daily vehicle miles traveled per person residing in the urban area."
  ),

  #urban DVMT per Capita in Multnomah county
  list(
    Name = "UrbanLdvDmvtPerCap_MaAz",
    Function = "UrbanHhDvmt_MaAz / UrbanHhPop_MaAz",
    Units = "Dvmt per person",
    Description = "daily vehicle miles traveled per person residing in the urban area in Multnomah county."
  ),

  #urban DVMT per Capita in mixed use in Multnomah county
  list(
    Name = "UrbanLdvDmvtPerCap_MaAzMx",
    Function = "UrbanHhDvmt_MaAzMx / UrbanHhPop_MaAzMx",
    Units = "Dvmt per person",
    Description = "daily vehicle miles traveled per person residing in mixed use in the urban area in Multnomah county."
  )

)
