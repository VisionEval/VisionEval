# Sample_Measures_Script.R
# This script will process a list of measure specifications across the Years in a Scenario Datastore
# and generate a .csv file that contains the measures.

# Default/Example measures in Test-Spec.R will work with output from the sample VE_RSPM. Adjust the
# Test-Spec.R file for your own MArea and Azone geography and it should work with your local version
# of VE_RSPM.

# CONFIGURE THE FOLLOWING PARAMETERS FOR THE SCRIPT RUN:

# SpecFile - (optional) file name containing measure specifications
#               default/examples: PM-Spec.R in folder with this script
# ScenarioRoot - the directory relative to which to look for scenarios
# Scenarios - vector of directories to find the Datastore from which to extract the measures
#            (each Scenario is sought as subdirectory of ScenarioRoot)
# OutputFile - (optional) file name in which to put the measures (default is long but informative)
#              Can be a string or it can contain a "%s" two letter parameter that gets replaced
#              with "basename(Scenario)
# Years - which years in the Datastore to process measures for
# Geography - which geography (Type/Value) to create measures for
# Median Income - (optional) value from which to build PctMedianIncome for each Household to use in making measures

# SpecFile contains a list of specifications for the measures to generate (see the sample file for format)
SpecFile <- "Sample-VERSPM-Spec.R"
#   (default is "PM-Spec.R" in the same folder as this script)
# SpecFile contains a definition of PMSpecifications (a list of specifications; see the sample file for format)
# SpecFile should contain measure specs for whatever Geography you specified abaove (in the By and Units Summarize fields)
# For different geography types, make another PM-Spec-Azone.R (e.g.) and in the summarize blocks,
# replace 'By = "Marea"' with 'By="Azone"' and replace 'Marea = ""' with 'Azone = ""' in the Summarize Units

# ScenarioRoot is a path (absolute or relative) to a directory that contains the Scenarios
ScenarioRoot <- file.path(ve.root,"ModelState")

# 'Scenarios' is a vector of names of subfolders of 'ScenarioRoot' to be a directory containing a
# "Datastore" and "ModelState.Rda" that is a subfolder of where you are running the script
Scenarios <- c("VERSPM") # could be many
# Each scenario gets expanded using file.path(normalizePath(ScenarioRoot),Scenarios[x]) before
# processing. You can fake it by making Scenarios a vector of relative paths (relative to
# ScenarioRoot), so this mechanism should be general purpose.

outputFile <- "Measures_%scenario%_%years%_%geography%.csv"
#   (default is a long-but-informative filename in each Scenario directory; suggest not changing it)

Years <- c("2010","2038")
# Which Years to summarize from the Datastore for this Scenario

Geography <- c(Type="Marea",Value="RVMPO") # Can set type to "Azone" or "Bzone" and it probably works
# What Geography to perform the summary for
#    Type can be Marea, Azone or Bzone
#    Value should be the label of one of those geographies)
# The Geography should be consistent with the "By" field in PMSpecifications
# TODO: Type="Region"

# Comment out the following line if you're not doing measures that use PctMedianIncome

MedianIncome <- 92100
# If defined at all, add a "PctMedianIncome" Dataset to the Datastore, based on Income and HhSize
# You can use a single value as here, or a vector of different values, indexed by HhSize, or NULL
# MedianIncome <- c(44400, 60200, 71300)
#                 44400 is MFI for 1 person HH, 60200 for 2 person HH, 71300 for 3 person HH
# Do the following to generate the MedianIncome for each HhSize from the Datastore
# MedianIncome <- NULL # use Datastore by HhSize
# If there are fewer MFI's than Household Sizes, the last value is used for all the larger sizes
# so providing a single value gives every HH Size the same MFI
# 92100 default is from
# https://www.huduser.gov/portal/datasets/il/il2020/2020MedCalc.odn?inputname=METRO38900M38900*Portland-Vancouver-Hillsboro%2C+OR-WA+MSA&selection_type=hmfa&year=2020&wherefrom=mfi&incpath=%24incpath%24

# Change the line below if all your scenarios are using the hdf5 Datastore_Type (verify in
# defs/run_parameters.json). We don't just read that file, since the model setup files might be
# somewhere else than where the Datastore is.
DatastoreType <- "RD"
# DatastoreType <- "HD"

# The rest of the script should not need to be customized.
################################################################################################

###########################################################################
# Required libraries
# Need to affix namespace resolution operator to use functions

requireNamespace(stringr)
requireNamespace(visioneval)

###########################################################################
# Set up Input and Output files

# Gather the specifications, if they're not already there
# We will read SpecFile if it exists, and if not, use PMSpecifications
# already defined in the current R environment.

if ( ! exists("SpecFile") && ! exists("PMSpecifications") ) {
  SpecFile <- "PM-Spec.R"
}
if ( exists("SpecFile") && file.exists(SpecFile) ) {
  specEnv <- new.env()
  sys.source(SpecFile,envir=specEnv)
  specs <- objects(specEnv)
  if ( length(specs) != 1 ) {
    print(specs)
    stop("Must define a single specification list in",SpecFile)
  }
  PMSpecifications <- get(specs,envir=specEnv)
  cat("Loaded specification object ",specs," from ",SpecFile,"\n",sep="'")
  rm(specEnv)
}
if ( ! exists("PMSpecifications") ) {
  stop(paste0("Need to put a single specification list in '",SpecFile,"' before running this script."))
}

# Name the output files, if the default is being used
if ( !exists("outputFile") || nchar("outputFile")==0 ) {
  outputFile <- "Measures_%scenario%_%years%_%geography%.csv"
}

# Construct and test the scenario directories

Scenarios <- normalizePath(file.path(ScenarioRoot,Scenarios),mustWork=FALSE)
if ( any( ! dir.exists(Scenarios) ) ) {
  # check that requested scenario directories exist
  message("Missing Scenarios")
  print(Scenarios[!dir.exists(Scenarios)])
  stop("Scenarios must be sub-directories of Scenario Root",call.=FALSE)
} else {
  # check that there is a datastore in the Scenarios directory
  for ( scen.dir in Scenarios ) {
    sd <- dir(Scenarios)
    if ( length(grep("Datastore",sd))==0 ||
         length(grep("ModelState.Rda"))== 0 ) {
      stop("No Datastore or ModelState.Rda: Did you run the model yet?",call.=FALSE)
    }
  }
}

###########################################################################
# FUNCTION DEFINITIONS
###########################################################################

###########################################################################
# FUNCTION: addPctMFI
#
# Add PctMedianIncome (percent of Meaian Family Income (MFI)) to Datastore
# MedianIncome can be:
#    NULL (compute MedianIncome by HhSize)
#    A single numeric value (used for all HhSizes)
#    A vector of Medians, indexed by HhSize
#       (if more HhSizes in Datastore than in list, just recyle the last value)
#
addPctMFI <- function(thisYear,MedianIncome,QPrep_ls,DstoreType="RD") {
  HhSizeInc <- visioneval::readDatastoreTables(
    Tables_ls = list(Household=c(HhSize="PRSN",Income="USD")),
    Group = thisYear,
    QueryPrep_ls = QPrep_ls
  )$Data$Household
  HhSize <- HhSizeInc$HhSize
  Income <- HhSizeInc$Income

  # Make sure we have enough median income bins
  if ( ! is.null(MedianIncome)  &&
     ( maxHhSize <- max(unique(HhSize)) ) > ( mil <- length(MedianIncome) ) ) {
    MedianIncome[(mil+1):maxHhSize] <- MedianIncome[mil]
  } else {
    # generate MFI by Household Size from synthetic incomes
    MedianIncome <- aggregate(
      list(MedianIncome=Income),
      by=list(HhSize=HhSize),
      FUN=median,
      drop=FALSE
    )$MedianIncome
  }

  # Compute PctMedInc
  MedHhIncome <- MedianIncome[HhSize] # make vector length of HhSize w/ MedianIncome for that size
  PctMedHhInc <- Income / round(MedHhIncome,2)

  # Prepare data list for datastore
  # Group = Year, Table= Household, Dataset = PctMedianIncome
  # Use literal "Year" (actual year is supplied with setInDatastore
  # -----------------------------------
  PctHhMedInc_ls <- list(Year=list(Household=list(PctMedianIncome=PctMedHhInc)))

  # Construct a Datastore specification for PctMedHhInc
  # -----------------------------------
  Specs <- list(
    #Level of geography module is applied at
    RunBy = "Region",
    #Specify data to saved in the data store
    Set = items(
      item(
        NAME = "PctMedianIncome",
        TABLE = "Household",
        GROUP = "Year",
        TYPE = "double",
        UNITS = "proportion",
        NAVALUE = -1,
        PROHIBIT = c("NA", "< 0"),
        ISELEMENTOF = "",
        DESCRIPTION = "HH Income as fraction of median income for households of this size"
      )
    )
  )

  # Write the Dataset out to the Year Group using
  # the specification + framework functions
  # -----------------------------------
  Loc <- QPrep_ls$Dir[1]
  assign("ModelState_ls",QPrep_ls$Listing[[Loc]],pos=1)

  visioneval::assignDatastoreFunctions(DstoreType) 
  Specs <- visioneval::processModuleSpecs(Specs)
  visioneval::setInDatastore( PctHhMedInc_ls, Specs, "PBOT-PctMedianIncome", thisYear )
  rm(ModelState_ls, pos=1)

  # Read back for test

  #   PctMedIncome <- visioneval::readDatastoreTables(
  #     Tables_ls = list(Household=c(PctMedianIncome="none")),
  #     Group = Year,
  #     QueryPrep_ls = QPrep_ls
  #   )$Data$Household

  # Return MedianIncome table, in case we had to construct it here
  invisible(MedianIncome)
}

###########################################################################
# FUNCTION: makeMeasure
#
# Process a measureSpec for a Year and Geography
# Return the measureName, with the side effect that the value(s) of the measure
# are placed into measureEnv (whence they will later be summarized)
#
makeMeasure <- function(measureSpec,thisYear,Geography,QPrep_ls,measureEnv) {
  GeoValue <- Geography["Value"]
  measureName <- measureSpec$Name

  # Skip or include measures based on presence of required Dataset
  if ( "Require" %in% names(measureSpec) ) {
    if ( ! visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      return(paste(measureName,"(SKIPPED due to Require:)"))
    }
  } else
  if ( "RequireNot" %in% names(measureSpec) ) {
    if ( visioneval::isDatasetPresent(measureSpec$Require["Dataset"], measureSpec$Require["Table"], thisYear, QPrep_ls) ) {
      return(paste(measureName,"(SKIPPED due to RequireNot:)"))
    }
  }

  # Compute the measure based on the measureSpec     
  if ( "Function" %in% names(measureSpec) ) {
    measure <- eval(parse(text=measureSpec$Function), envir=measureEnv)
    names(measure) <- measureName
  } else
  if ( "Summarize" %in% names(measureSpec) ) {
    sumSpec <- measureSpec$Summarize
    if ( ! "By" %in% names(sumSpec) ||
         ! Geography["Type"] %in% sumSpec$By ) {
      stop(paste("Script wants Geography Type ",Geography["Type"]," in 'By' but got ",sumSpec$By,"",sep="'"))
    }
    usingBreaks <- "Breaks" %in% names(sumSpec) && ! is.null(sumSpec$Breaks)
    usingKey <- "Key" %in% names(sumSpec) && ! is.null(sumSpec$Key)
    measure <- visioneval::summarizeDatasets(
        Expr = sumSpec$Expr,
        Units = sumSpec$Units,
        By_ = sumSpec$By,
        Breaks_ls = if ( usingBreaks) sumSpec$Breaks else NULL,
        Table = sumSpec$Table,
        Key = if ( usingKey ) sumSpec$Key else NULL,
        Group = thisYear,
        QueryPrep_ls = QPrep_ls
      )
    if ( ! usingBreaks ) {
      measure <- measure[GeoValue]
      names(measure) <- measureName
    } else {
      # WARNING: even though possible in theory, as written this
      # won't work with breaks from more than one Dataset
      measure <- measure[,GeoValue]
      if ( "BreakNames" %in% names(sumSpec) ) {
        breakNames <- sumSpec$BreakNames[[sumSpec$By[1]]]
      } else {
        breakNames <- as.character(sumSpec$Breaks[[sumSpec$By[1]]])
      }
      names(measure) <- paste(measureName,c("min",breakNames),sep=".")
    }
  } else
  {
    stop("Invalid Measure Specification")
  }
  
  # Stash the measure results in measureEnv
  for ( nm in names(measure) ) {
    msr <- measure[nm]
    attributes(msr) <- list(
      Units = measureSpec$Units,
      Description = measureSpec$Description
    )
    assign(nm,msr,env=measureEnv)
  }

  # Return the name(s), for output tracking
  return(names(measure))
}

###########################################################################
# FUNCTION: makeMeasureDataFrame
#
# Extract the measures made by makeMeasure from measureEnv and put them in a
# data.frame suitable for writing to the output file
#
makeMeasureDataFrame <- function(measureEnv) {
  Measures_     <- objects(measureEnv)
  Values_       <- sapply(Measures_, get, envir=measureEnv)
  Units_        <- unname(sapply(Measures_, function(x) attributes(get(x,envir=measureEnv))$Units))
  Description_  <- unname(sapply(Measures_, function(x) attributes(get(x,envir=measureEnv))$Description))
  Data_df       <- data.frame(
    Measure     = Measures_,
    thisYear    = Values_,
    Units       = Units_,
    Description = Description_
  )
  # The following addresses a unique naming standard in original PBOT script
  Data_df$Measure <- gsub("_Ma", "_", Data_df$Measure)
  Data_df$Measure <- gsub("_$", "", Data_df$Measure)
  Data_df$Measure <- gsub("_\\.", ".", Data_df$Measure)
  rownames(Data_df) <- NULL
  return(Data_df)
}

###########################################################################
# Finally, Process the Specification list
###########################################################################

# Get down to business

old.wd <- getwd()

tryCatch(
  {
    for ( scenario in Scenarios ) { # scenario contains a path to a working directory with a Datastore in it

      # Move to scenario directory
      setwd(scenario)

      # Scenario Name for reporting / outputFile
      scenarioName <- basename(scenario)

      # Confirm what we're working on
      catYears <- paste(Years,collapse=",")
      catGeography <- paste(Geography["Type"],"=",paste0("'",Geography["Value"],"'"))
      catSpecNum <- paste("(",length(PMSpecifications)," Measure Specs)",sep="")
      cat(
        "Specification File:",SpecFile,catSpecNum,"\n",
        "Building measures for:\n",
        "Scenario:",scenarioName,"\n",
        "Years:",catYears,"\n",
        "Geography:",catGeography,"\n"
      )

      # Build the outputFile name using the just reported specifications
      outputFileToWrite <- stringr::str_replace(outputFile,"%scenario%",scenarioName)
      outputFileToWrite <- stringr::str_replace(outputFileToWrite,"%years%",catYears)
      outputFileToWrite <- stringr::str_replace(outputFileToWrite,"%geography%",stringr::str_remove_all(catGeography,"[ ']"))
      outputFileToWrite <- normalizePath(file.path(scenario,outputFileToWrite),mustWork=FALSE)

      # Prepare for datastore queries
      #------------------------------
      prepForQuery <- function() {
        visioneval::prepareForDatastoreQuery(
          DstoreLocs_ = c("Datastore"),
          DstoreType = DatastoreType
        )
      }
      QPrep_ls <- prepForQuery()

      # Create the name of the data.frame that will collect the results
      Measures_df <- NULL

      # Iterate across the Years in the scenario
      for ( thisYear in Years ) {

        cat("Working on Year",thisYear,"\n")
        results <- new.env()

        # Build PctMedianIncome for Households, if requested
        if ( exists("MedianIncome") ) {
          if ( is.null(MedianIncome) ) {
            cat("Using default Median Incomes from Datastore\n")
          } else {
            cat("Using Median Income:\n")
            print(MedianIncome)
          }
          cat("Adding PctMedianIncome...")
          addPctMFI(thisYear,MedianIncome,QPrep_ls)
          cat("PctMedianIncome Processed\n")
          # Need to refresh QPrep_ls to re-read the updated Datastore contents
          QPrep_ls <- prepForQuery()
        }

        # Iterate over the measures
        for ( measureSpec in PMSpecifications ) {
          cat("Processing ",measureSpec$Name,"...",sep="")
          measure <- makeMeasure(measureSpec,thisYear,Geography,QPrep_ls,results)
          if ( length(measure)>1 ) for ( m in measure ) cat( paste0(m,"||") )
          cat("..Processed\n")
        }

        # Add this Year's measures to the output data.frame
        temp <- makeMeasureDataFrame(results)
        if ( is.null(Measures_df) ) {
          Measures_df<-temp[,c("Measure","Units","Description","thisYear")]
        } else {
          Measures_df<-cbind(Measures_df,thisYear=temp$thisYear )
        }
        names(Measures_df)[names(Measures_df)=="thisYear"]<-thisYear
      }

      # Write the measures for all the Years to the output file
      cat("Saving measures in",basename(dirname(outputFileToWrite)),"as",basename(outputFileToWrite),"...")
      write.csv(Measures_df, row.names = FALSE, file = outputFileToWrite)
      cat("Saved\n")
    }
  },
  error=function(e)cat(geterrmessage(),"\n"),
  finally=setwd(old.wd)
)
