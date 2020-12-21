#========
#module.R
#========

#This script defines functions related to the development and use of modules.

utils::globalVariables(c("readFromTable","initDatastore"))

#DEFINE LIST ALIAS
#=================
#' Alias for list function.
#'
#' \code{item} a visioneval framework module developer function that is an alias
#' for the list function whose purpose is to make module specifications easier
#' to read.
#'
#' This function defines an alternate name for list. It is used in module
#' specifications to identify data items in the Inp, Get, and Set portions of
#' the specifications.
#'
#' @return a list.
#' @export
item <- list

#' Alias for list function.
#'
#' \code{items} a visioneval framework module developer function that is
#' an alias for the list function whose purpose is to make module specifications
#' easier to read.
#'
#' This function defines an alternate name for list. It is used in module
#' specifications to identify a group of data items in the Inp, Get, and Set
#' portions of the specifications.
#'
#' @return a list.
#' @export
items <- list


#INITIALIZE DATA LIST
#====================
#' Initialize a list for data transferred to and from datastore
#'
#' \code{initDataList} a visioneval framework module developer function that
#' creates a list to be used for transferring data to and from the datastore.
#'
#' This function initializes a list to store data that is transferred from
#' the datastore to a module or returned from a module to be saved in the
#' datastore. The list has 3 named components (Global, Year, and BaseYear). This
#' is the standard structure for data being passed to and from a module and the
#' datastore.
#'
#' @return A list that has 3 named list components: Global, Year, BaseYear
#' @export
initDataList <- function() {
  list(Global = list(),
       Year = list(),
       BaseYear = list())
}


#ADD ERROR MESSAGE TO RESULTS LIST
#=================================
#' Add an error message to the results list
#'
#' \code{addErrorMsg} a visioneval framework module developer function that adds
#' an error message to the Errors component of the module results list that is
#' passed back to the framework.
#'
#' This function is a convenience function for module developers for passing
#' error messages back to the framework. The preferred method for handling
#' errors in module execution is for the module to handle the error by passing
#' one or more error messages back to the framework. The framework will then
#' write error messages to the log and stop execution. Error messages are
#' stored in a component of the returned list called Errors. This component is
#' a string vector where each element is an error message. The addErrorMsg will
#' create the Error component if it does not already exist and will add an error
#' message to the vector.
#'
#' @param ResultsListName the name of the results list given as a character
#' string
#' @param ErrMsg a character string that contains the error message
#' @return None. The function modifies the results list by adding an error
#' message to the Errors component of the results list. It creates the Errors
#' component if it does not already exist.
#' @export
addErrorMsg <- function(ResultsListName, ErrMsg) {
  Results_ls <- get(ResultsListName, envir = parent.frame())
  Results_ls$Errors <- c(Results_ls$Errors, ErrMsg)
  assign(ResultsListName, Results_ls, envir = parent.frame())
}


#ADD WARNING MESSAGE TO RESULTS LIST
#===================================
#' Add a warning message to the results list
#'
#' \code{addWarningMsg} a visioneval framework module developer function that
#' adds an warning message to the Warnings component of the module results list
#' that is passed back to the framework.
#'
#' This function is a convenience function for module developers for passing
#' warning messages back to the framework. The preferred method for handling
#' warnings in module execution is for the module to handle the warning by
#' passing one or more warning messages back to the framework. The framework
#' will then write warning messages to the log. Warning
#' messages are stored in a component of the returned list called Warnings. This
#' component is a string vector where each element is an warning message. The
#' addWarningMsg will create the Warning component if it does not already exist
#' and will add a warning message to the vector.
#'
#' @param ResultsListName the name of the results list given as a character
#' string
#' @param WarnMsg a character string that contains the warning message
#' @return None. The function modifies the results list by adding a warning
#' message to the Warnings component of the results list. It creates the
#' Warnings component if it does not already exist.
#' @export
addWarningMsg <- function(ResultsListName, WarnMsg) {
  Results_ls <- get(ResultsListName, envir = parent.frame())
  Results_ls$Warnings <- c(Results_ls$Warnings, WarnMsg)
  assign(ResultsListName, Results_ls, envir = parent.frame())
}


#LOAD ESTIMATION DATA
#====================
#' Load estimation data
#'
#' \code{processEstimationInputs} a visioneval framework module developer
#' function that checks whether specified model estimation data meets
#' specifications and returns the data in a data frame.
#'
#' This function is used to check whether a specified CSV-formatted data file
#' used in model estimation is correctly formatted and contains acceptable
#' values for all the datasets contained within. The function checks whether the
#' specified file exists in the "inst/extdata" directory. If the file does not
#' exist, the function stops and transmits a standard error message that the
#' file does not exist. If the file does exist, the function reads the file into
#' the data frame and then checks whether it contains the specified columns and
#' that the data meets all specifications. If any of the specifications are not
#' met, the function stops and transmits an error message. If there are no
#' data errors the function returns a data frame containing the data in the
#' file.
#'
#' @param Inp_ls A list that describes the specifications for the estimation
#' file. This list must meet the framework standards for specification
#' description.
#' @param FileName A string identifying the file name. This is the file name
#' without any path information. The file must located in the "inst/extdata"
#' directory of the package.
#' @param ModuleName A string identifying the name of the module the estimation
#' data is being used in.
#' @return A data frame containing the estimation data according to
#' specifications with data types consistent with specifications and columns
#' not specified removed. Execution stops if any errors are found. Error
#' messages are printed to the console. Warnings are also printed to the console.
#' @export
processEstimationInputs <- function(Inp_ls, FileName, ModuleName) {
  #Define a function which expands a specification with multiple NAME items
  expandSpec <- function(SpecToExpand_ls) {
    Names_ <- unlist(SpecToExpand_ls$NAME)
    Expanded_ls <- list()
    for (i in 1:length(Names_)) {
      Temp_ls <- SpecToExpand_ls
      Temp_ls$NAME <- Names_[i]
      Expanded_ls <- c(Expanded_ls, list(Temp_ls))
    }
    Expanded_ls
  }
  #Define a function to process a component of a specifications list
  processComponent <- function(Component_ls) {
    Result_ls <- list()
    for (i in 1:length(Component_ls)) {
      Temp_ls <- Component_ls[[i]]
      if (length(Temp_ls$NAME) == 1) {
        Result_ls <- c(Result_ls, list(Temp_ls))
      } else {
        Result_ls <- c(Result_ls, expandSpec(Temp_ls))
      }
    }
    Result_ls
  }
  #Expand the specifications
  Inp_ls <- processComponent(Inp_ls)
  #Try to load the estimation file
  FilePath <- paste0("inst/extdata/", FileName)
  if (!file.exists(FilePath)) {
    Message <- paste("File", FilePath,
                     "required to estimate parameters for module", ModuleName,
                     "is missing.")
    stop(Message)
  } else {
    Data_df <- read.csv(FilePath, as.is = TRUE)
  }
  #Check whether all the necessary columns exist and remove unnecessary ones
  Names <- unlist(lapply(Inp_ls, function(x) x$NAME))
  if (!all(Names %in% names(Data_df))) {
    Message <- paste("Some required columns are missing from", FileName,
                     "required to estimate parameters for module", ModuleName)
    stop(Message)
  }
  Data_df <- Data_df[, Names]
  #Iterate through each column and check whether data meets specifications
  Errors_ <- character(0)
  Warnings_ <- character(0)
  for (i in 1:length(Inp_ls)) {
    Spec_ls <- Inp_ls[[i]]
    DatasetName <- Spec_ls$NAME
    Data_ <- Data_df[[DatasetName]]
    #Calculate SIZE of data if character data
    #This is only necessary because checkDataConsistency requires a SIZE attribute
    if (typeof(Data_) == "character") {
      Spec_ls$SIZE <- max(nchar(Data_))
    } else {
      Spec_ls$SIZE <- 0
    }
    #Check the dataset consistency with the specs
    DataCheck_ls <- checkDataConsistency(DatasetName, Data_, Spec_ls)
    if (length(DataCheck_ls$Errors) != 0) {
      Errors_ <- c(Errors_, DataCheck_ls$Errors)
    }
    if (length(DataCheck_ls$Warnings) != 0) {
      Warnings_ <- c(Warnings_, DataCheck_ls$Warnings)
    }
  }
  #Stop and list any errors if there are any
  if (length(Errors_) != 0) {
    Message <- paste(
      paste("Estimation file", FileName, "contains the following errors:"),
      paste(Errors_, collapse = "\n"),
      paste("Check data specifications in module", ModuleName, "script."),
      sep = "\n"
    )
    stop(Message)
  }
  #Print any warnings if there are any
  if (length(Warnings_) != 0) {
    print("The following data items match data conditions that are UNLIKELY:")
    for (i in length(Warnings_)) {
      print(Warnings_[i])
    }
  }
  #Identify the data class for each input file field
  ColClasses_ <- unlist(lapply(Inp_ls, function(x) {
    Type <- x$TYPE
    Class <- Types()[[Type]]$mode
    if (Class == "double") Class <- "numeric"
    Class
  }))
  names(ColClasses_) <- unlist(lapply(Inp_ls, function(x) {
    x$NAME
  }))
  #Match the classes with order of field names in the input file
  ColClasses_ <- ColClasses_[names(Data_df)]
  #Convert NA values into "NULL" (columns in data not to be read in)
  ColClasses_[is.na(ColClasses_)] <- "NULL"
  #Read the data file with the assigned column classes
  read.csv(FilePath, colClasses = ColClasses_)[, Names]
}


#LOAD A VE PACKAGE DATASET
#=========================
#' Load a VisionEval package dataset
#'
#' \code{loadPackageDataset} a visioneval framework module developer function
#' which loads a dataset identified by name from the VisionEval package
#' containing the dataset.
#'
#' This function is used to load a dataset identified by name from the
#' VisionEval package which contains the dataset. Using this function is the
#' preferred alternative to hard-wiring the loading a dataset using the
#' 'package::dataset' notation because it enables users to switch between module
#' versions contained in different packages. For example, there may be different
#' versions of the VEPowertrainsAndFuels package which have different default
#' assumptions about light-duty vehicle powertrain mix and characteristics by
#' model year. Using this function, the module developer only needs to identify
#' the dataset name. The module developer should also specify a 'DefaultPackage'
#' name to simplify module development and testing. For example, the default
#' 'VEPowertrainsAndFuels' package can be specified. During module development
#' and testing the dataset will be loaded from that package, but during a model
#' run the dataset will be loaded from the package that is specified to be used
#' in the model run. The function uses 'DatasetsByPackage_df' data frame in the
#' model state list to identify the package which contains the dataset. If
#' 'DatasetsByPackage_df' is not present in the model state list, as is the case
#' during module development and testing, the the dataset is retrieved from the
#' 'DefaultPackage'.
#'
#' @param DatasetName A string identifying the name of the dataset.
#' @param DefaultPackage A string identifying the name of the package to
#'   retrieve the dataset from during module development and testing.
#' @return The identified dataset.
#' @export
loadPackageDataset <- function(DatasetName, DefaultPackage = NULL) {
  if (exists(DatasetName)) {
    return(eval(parse(text = DatasetName)))
  } else {
    if (!is.null(getModelState()$DatasetsByPackage_df)) {
      Dat_df <- getModelState()$DatasetsByPackage_df
      PkgName <- with(Dat_df, Package[Dataset == DatasetName])
    } else {
      PkgName <- DefaultPackage
    }
    FullName <- paste(PkgName, DatasetName, sep = "::")
    return(eval(parse(text = FullName)))
  }
}

#SAVE A VE PACKAGE DATASET
#=========================
#' Save a VisionEval package dataset to the data/ folder during package build
#'
#' \code{savePackageDataset} a visioneval framework module developer function
#' which saves a dataset to the data/ directory during package build.
#'
#' This function is used to save a dataset during module estimation and when
#' building module specifications. Using this function is the
#' preferred alternative to hard-wiring saving a dataset using usthis::use_data
#' or other means as it does suitable error-checking.
#'
#' @param dataset A string identifying the name of the object containing
#' the dataset.
#' @param overwrite During the SAVE phase and if FALSE, do not overwrite an existing file in data/ space
#' @param keep  During the BUILD phase and if TRUE, do not reomve the object from the R/ space
#' @param compress Optionally specify a different compression mode
#' @return The dataset name if it was saved successfully, otherwise an empty character vector
#' @export
savePackageDataset <- function(dataset,overwrite=TRUE,keep=FALSE,compress="xz") {
  dsname <- deparse(substitute(dataset))
  if ( length(dsname)!=1 ) stop("Unable to deparse dataset name.")
  if ( ! dir.exists("data") ) stop("Data directory not found in ",getwd())
  file <- file.path("data",paste0(dsname,".rda"))
  build.phase <- toupper(Sys.getenv("VE_BUILD_PHASE","SAVE"))
  if ( build.phase == "SAVE" ) {
    if ( file.exists(file) ) {
      if ( ! overwrite ) {
        message("Existing '",dsname,"' not overwritten due to overwrite=FALSE.")
      } else if ( Sys.getenv("VE_EXPRESS","NO")!="NO" ) {
        message("Existing '",dsname,"' skipped for VE_EXPRESS.")
        overwrite = FALSE
      }
    }
    if ( overwrite ) {
      cat("Saving '",dsname,"' to '",file,"' ... ",sep="")
      save(list=dsname,file=file,compress=compress,envir=parent.frame())
      if ( ! file.exists(file) ) { traceback(); stop("File NOT saved!\n") } else cat("Saved\n")
    }
  } else if (build.phase == "BUILD" ) {
    # During the BUILD phase, we won't keep the dataset unless explicitly requested.
    # Some modules will break (e.g. using variable as an exported function default
    # parameter) if the object is removed. The "Undefined Global Variable" message
    # from R CMD check will identify variables that are used in that wrong way.
    # Currently, they won't get deleted if they hadn't already been saved to a data/ set.
    # The right way is to save the variable to data/ and then reload it inside the
    # function if the variable default is not a usable value (e.g. NULL)
    Msg_ <- paste(dsname,"in R/ space")
    if ( ! keep ) {
      rm(list=dsname,envir=parent.frame())
      if ( dsname %in% ls(parent.frame()) ) {
        stop("Failed to remove ",Msg_)
      } else {
        message("Removed ",Msg_)
      }
    } else {
      message("Keeping",Msg_)
    }
  } else {
    message("Unknown VE_BUILD_PHASE for savePackageDataset: ",build.phase)
    stop('VE_BUILD_PHASE = one.of("SAVE","BUILD")')
  }
  
  return(dsname)
}

#CHECK MODULE OUTPUTS FOR CONSISTENCY WITH MODULE SPECIFICATIONS
#===============================================================
#' Check module outputs for consistency with specifications
#'
#' \code{checkModuleOutputs} a visioneval framework module developer function
#' that checks output list produced by a module for consistency with the
#' module's specifications.
#'
#' This function is used to check whether the output list produced by a module
#' is consistent with the module's specifications. If there are any
#' specifications for creating tables, the function checks whether the output
#' list contains the table(s), if the LENGTH attribute of the table(s) are
#' present, and if the LENGTH attribute(s) are consistent with the length of the
#' datasets to be saved in the table(s). Each of the datasets in the output list
#' are checked against the specifications. These include checking that the
#' data type is consistent with the specified type and whether all values are
#' consistent with PROHIBIT and ISELEMENTOF conditions. For character types,
#' a check is made to ensure that a SIZE attribute exists and that the size
#' is sufficient to store all characters.
#'
#' @param Data_ls A list of all the datasets returned by a module in the
#' standard list form required by the VisionEval model system.
#' @param ModuleSpec_ls A list of module specifications in the standard list
#' form required by the VisionEval model system.
#' @param ModuleName A string identifying the name of the module.
#' @return A character vector containing a list of error messages or having a
#' length of 0 if there are no error messages.
#' @export
checkModuleOutputs <-
  function(Data_ls, ModuleSpec_ls, ModuleName) {
    #Initialize Errors vector
    Errors_ <- character(0)
    #Check that required info for specified tables is present in the outputs
    #-----------------------------------------------------------------------
    if (!is.null(ModuleSpec_ls$NewSetTable)) {
      TableSpec_ls <- ModuleSpec_ls$NewSetTable
      for (i in 1:length(TableSpec_ls)) {
        Table <- TableSpec_ls[[i]]$TABLE
        Group <- TableSpec_ls[[i]]$GROUP
        if (is.null(Data_ls[[Group]][[Table]])) {
          Msg <-
            paste0("Table ", Table, " in group ", Group, " is not defined in outputs.")
          Errors_ <- c(Errors_, Msg)
        } else {
          TableLength <- attributes(Data_ls[[Group]][[Table]])$LENGTH
          if (is.null(TableLength)) {
            Msg <-
              paste0("Table ", Table, " in group ", Group, " does not have a LENGTH attribute.")
            Errors_ <- c(Errors_, Msg)
          } else {
            DSetLengths_ <- unlist(lapply(Data_ls[[Group]][[Table]], length))
            if (!all(DSetLengths_ == TableLength)) {
              Msg <-
                paste0("The LENGTH attribute of table, ", Table, " in group ",
                       Group, " does not match length of datasets to be ",
                       "stored in the table.")
              Errors_ <- c(Errors_, Msg)
            }
            rm(DSetLengths_)
          }
          rm(TableLength)
        }
        rm(Table, Group)
      }
    }
    #Check that all Set specifications are satisfied
    #-----------------------------------------------
    SetSpec_ls <- processModuleSpecs(ModuleSpec_ls)$Set
    for (i in 1:length(SetSpec_ls)) {
      #Identify the Group, Table, and Name
      Spec_ls <- SetSpec_ls[[i]]
      Spec_ls$MODULE <- ModuleName
      Group <- Spec_ls$GROUP
      Table <- Spec_ls$TABLE
      Name <- Spec_ls$NAME
      #Check that the dataset exists and if so whether it correct
      if (is.null(Data_ls[[Group]][[Table]][[Name]])) {
        Msg <-
          paste0("Specified dataset ", Name, " in table ", Table,
                 " and group ", Group, " is not present in outputs.")
        Errors_ <- c(Errors_, Msg)
      } else {
        if (Spec_ls$TYPE == "character" & is.null(Spec_ls$SIZE)) {
          SizeSpec <- attributes(Data_ls[[Group]][[Table]][[Name]])$SIZE
          if (is.null(SizeSpec)) {
            Msg <-
              paste0("Specified dataset ", Name, " in table ", Table,
                     " and group ", Group, " has no SIZE attribute.")
            Errors_ <- c(Errors_, Msg)
          } else {
            Spec_ls$SIZE <- SizeSpec
          }
          rm(SizeSpec)
        }
        DSet_ <- Data_ls[[Group]][[Table]][[Name]]
        DataCheck_ls <- checkDataConsistency(Name, DSet_, Spec_ls)
        Errors_ <- c(Errors_, DataCheck_ls$Errors)
      }
      rm(Spec_ls, Group, Table, Name)
    }
    Errors_
  }

#BINARY SEARCH FUNCTION
#======================
#' Binary search function to find a parameter which achieves a target value.
#'
#' \code{binarySearch} a visioneval framework module developer function that
#' uses a binary search algorithm to find the value of a function parameter for
#' which the function achieves a target value.
#'
#' A binary search algorithm is used by several modules to calibrate the
#' intercept of a binary logit model to match a specified proportion or to
#' calibrate a dispersion parameter for a linear model to match a mean value.
#' This function implements a binary search algorithm in a consistent manner to
#' be used in all modules that need it. It is written to work with stochastic
#' models which by their nature don't produce the same outputs given the same
#' inputs and so will not converge reliably. To deal with the stochasticity,
#' this function uses a successive averaging  approach to smooth out the effect
#' of stochastic variation on reliable convergence. Rather than use the results
#' of a single search iteration to determine the next value range to use in the
#' search, a weighted average of previous values is used with the more recent
#' values being weighted more heavily.
#'
#' @param Function a function which returns a value which is compared to the
#' 'Target' argument. The function must take as its first argument a value which
#' from the 'SearchRange_'. It must return a value that may be compared to the
#' 'Target' value.
#' @param SearchRange_ a two element numeric vector which has the lowest and
#' highest values of the parameter range within which the search will be carried
#' out.
#' @param ... one or more optional arguments for the 'Function'.
#' @param Target a numeric value that is compared with the return value of the
#' 'Function'.
#' @param DoWtAve a logical indicating whether successive weighted averaging is
#' to be done. This is useful for getting stable results for stochastic
#' calculations.
#' @param MaxIter an integer specifying the maximum number of iterations
#' to all the search to attempt.
#' @param Tolerance a numeric value specifying the proportional difference
#' between the 'Target' and the return value of the 'Function' to determine
#' when the search is complete.
#' @return the value in the 'SearchRange_' for the function parameter which
#' matches the target value.
#' @export
binarySearch <-
  function(Function,
           SearchRange_,
           ...,
           Target = 0,
           DoWtAve = TRUE,
           MaxIter = 100,
           Tolerance = 0.0001) {
    #Initialize vectors of low, middle and high values
    Lo_ <- SearchRange_[1]
    Mid_ <- mean(SearchRange_)
    Hi_ <- SearchRange_[2]
    #Initialize vector to store weighted average of middle value
    WtMid_ <- numeric(0)
    #Define function to calculate weighted average value of a vector
    calcWtAve <- function(Values_) {
      Wts_ <- (1:length(Values_))^2 / sum((1:length(Values_))^2)
      sum(Values_ * Wts_)
    }
    #Iterate to find best fit
    for (i in 1:MaxIter) {
      Lo <- calcWtAve(Lo_)
      Hi <- calcWtAve(Hi_)
      #Range of input values to test
      InValues_ <- c(Lo, (Lo + Hi) / 2, Hi)
      #Apply Function to calculate results for the InValues_ vector
      Result_ <-
        sapply(InValues_, Function, ...)
      #Check whether any values of Result_ are NA
      if (any(is.na(Result_))) {
        Msg <-
          paste0("Error in 'binarySearch' function to match target value. ",
                 "The low and/or high values of the search range produce ",
                 "NA results. The low result is ", Result_[1], ". ",
                 "The high result is ", Result_[2], ". ",
                 "Modify the search range to avoid NA values.")
        stop(Msg)
      }
      #Determine which two Result_ values bracket the target value
      GT_ <- Result_ > Target
      Idx <- which(diff(GT_) != 0)
      #Calculate new low and high values
      if (length(Idx) > 0) {
        Lo_ <- c(Lo_, InValues_[Idx])
        Hi_ <- c(Hi_, InValues_[Idx + 1])
        Mid_ <- c(Mid_, mean(c(InValues_[Idx], InValues_[Idx + 1])))
      } else {
        MinIdx <- which(abs(Result_ - Target) == min(abs(Result_ - Target)))
        Lo_ <- c(Lo_, tail(Lo_, 1))
        Hi_ <- c(Hi_, tail(Hi_, 1))
        Mid_ <- c(Mid_, tail(Mid_, 1))
      }
      WtMid_ <- c(WtMid_, calcWtAve(Mid_))
      #Break out of loop if change in weighted mean of midpoint is less than tolerance
      if (length(Mid_) > 10) {
        Chg <- abs(diff(tail(Mid_, 4)) / tail(Mid_, 3))
        if (all(Chg < Tolerance)) break()
      }
    }
    #Return the weighted average of the midpoint value
    if (DoWtAve) {
      Result <- tail(WtMid_, 1)
    } else {
      Result <- tail(Mid_, 1)
    }
    Result
  }


#MAKE A MODEL FORMULA STRING
#===========================
#' Makes a string representation of a model equation.
#'
#' \code{makeModelFormulaString} a visioneval framework module developer
#' function that creates a string equivalent of a model equation.
#'
#' The return values of model estimation functions such as 'lm' and 'glm'
#' contain a large amount of information in addition to the parameter estimates
#' for the specified model. This is particularly the case when the estimation
#' dataset is large. Most of this information is not needed to apply the model
#' and including it can add substantially to the size of a package that includes
#' several estimated models. All that is really needed to implement an estimated
#' model is an equation of the model terms and estimated coefficients. This
#' function creates a string representation of the model equation.
#'
#' @param EstimatedModel the return value of the 'lm' or 'glm' functions.
#' @return a string expression of the model equation.
#' @export
makeModelFormulaString <- function (EstimatedModel) {
  # Extract the model coefficients
  Coeff. <- coefficients( EstimatedModel )
  # Make the model formula
  FormulaString <- gsub( ":", " * ", names( Coeff. ) )
  FormulaString[ 1 ] <- "Intercept"
  FormulaString <- paste( Coeff., FormulaString, sep=" * " )
  FormulaString <- paste( FormulaString, collapse= " + " )
  return(FormulaString)
}


#APPLY A BINOMIAL MODEL
#======================
#' Applies an estimated binomial model to a set of input values.
#'
#' \code{applyBinomialModel} a visioneval framework module developer function
#' that applies an estimated binomial model to a set of input data.
#'
#' The function calculates the result of applying a binomial logit model to a
#' set of input data. If a target proportion (TargetProp) is specified, the
#' function calls the 'binarySearch' function to calculate an adjustment to
#' the constant of the model equation so that the population proportion matches
#' the target proportion. The function will also test whether the target search
#' range specified for the model will produce acceptable values.
#'
#' @param Model_ls a list which contains the following components:
#' 'Type' which has a value of 'binomial';
#' 'Formula' a string representation of the model equation;
#' 'Choices' a two-element vector listing the choice set. The first element is
#' the choice that the binary logit model equation predicts the odds of;
#' 'PrepFun' a function which prepares the input data frame for the model
#' application. If no preparation, this element of the list should not be
#' present or should be set equal to NULL;
#' 'SearchRange' a two-element numeric vector which specifies the acceptable
#' search range to use when determining the factor for adjusting the model
#' constant.
#' 'RepeatVar' a string which identifies the name of a field to use for
#' repeated draws of the model. This is used in the case where for example the
#' input data is households and the output is vehicles and the repeat variable
#' is the number of vehicles in the household.
#' 'ApplyRandom' a logical identifying whether the results will be affected by
#' random draws (i.e. if a random number in range 0 - 1 is less than the
#' computed probability) or if a probability cutoff is used (i.e. if the
#' computed probability is greater then 0.5). This is an optional component. If
#' it isn't present, the function runs with ApplyRandom = TRUE.
#'
#' @param Data_df a data frame containing the data required for applying the
#' model.
#' @param TargetProp a number identifying a target proportion for the default
#' choice to be achieved for the input data or NULL if there is no target
#' proportion to be achieved.
#' @param CheckTargetSearchRange a logical identifying whether the function
#' is to only check whether the specified 'SearchRange' for the model will
#' produce acceptable values (i.e. no NA or NaN values). If FALSE (the default),
#' the function will run the model and will not check the target search range.
#' @param ApplyRandom a logical identifying whether the outcome will be
#' be affected by random draws (i.e. if a random number in range 0 - 1 is less
#' than the computed probability) or if a probability cutoff is used (i.e. if
#' the computed probability is greater than 0.5)
#' @param ReturnProbs a logical identifying whether to return the calculated
#' probabilities rather than the assigned results. The default value is FALSE.
#' @return a vector of choice values for each record of the input data frame if
#' the model is being run, or if the function is run to only check the target
#' search range, a two-element vector identifying if the search range produces
#' NA or NaN values.
#' @export
applyBinomialModel <-
  function(Model_ls,
           Data_df,
           TargetProp = NULL,
           CheckTargetSearchRange = FALSE,
           ApplyRandom = TRUE,
           ReturnProbs = FALSE) {
    #Check that model is 'binomial' type
    if (Model_ls$Type != "binomial") {
      Msg <- paste0("Wrong model type. ",
                    "Model is identified as Type = ", Model_ls$Type, ". ",
                    "Function only works with 'binomial' type models.")
      stop(Msg)
    }
    #Check whether Model_ls has ApplyRandom component and assign value if so
    if (!is.null(Model_ls$ApplyRandom)) {
      ApplyRandom <- Model_ls$ApplyRandom
    }
    #Prepare data
    if (!is.null(Model_ls$PrepFun)) {
      Data_df <- Model_ls$PrepFun(Data_df)
    }
    #Define function to calculate probabilities
    calcProbs <- function(x) {
      Results_ <- x + eval(parse(text = Model_ls$Formula), envir = Data_df)
      if (!is.null(Model_ls$RepeatVar)) {
        Results_ <- rep(Results_, Data_df[[Model_ls$RepeatVar]])
      }
      Odds_ <- exp(Results_)
      Odds_ / (1 + Odds_)
    }
    #Define function to calculate factor to match target proportion
    checkProportionMatch <- function(TestValue) {
      Probs_ <- calcProbs(TestValue)
      sum(Probs_) / length(Probs_)
    }
    #Define a function to assign results
    if (ApplyRandom) {
      assignResults <- function(Probs_) {
        N <- length(Probs_)
        Result_ <- rep(Model_ls$Choices[2], N)
        Result_[runif(N) <= Probs_] <- Model_ls$Choices[1]
        Result_
      }
    } else {
      assignResults <- function(Probs_) {
        ifelse(Probs_ > 0.5, Model_ls$Choices[1], Model_ls$Choices[2])
      }
    }
    #Apply the model
    if (CheckTargetSearchRange) {
      Result_ <- c(
        Lo = checkProportionMatch(Model_ls$SearchRange[1]),
        Hi = checkProportionMatch(Model_ls$SearchRange[2])
      )
    } else {
      if (is.null(TargetProp)) {
        Probs_ <- calcProbs(0)
        Result_ <- assignResults(Probs_)
      } else {
        if (TargetProp == 0 | TargetProp == 1) {
          if (TargetProp == 0) Result_ <- rep(Model_ls$Choices[2], nrow(Data_df))
          if (TargetProp == 1) Result_ <- rep(Model_ls$Choices[1], nrow(Data_df))
        } else {
          Factor <- binarySearch(checkProportionMatch, Model_ls$SearchRange, Target = TargetProp)
          Probs_ <- calcProbs(Factor)
          Result_ <- assignResults(Probs_)
        }
      }
    }
    #Return values
    if (ReturnProbs) {
      Probs_
    } else {
      Result_
    }
  }


#APPLY A LINEAR MODEL
#====================
#' Applies an estimated linear model to a set of input values.
#'
#' \code{applyLinearModel} a visioneval framework module developer function that
#' applies an estimated linear model to a set of input data.
#'
#' The function calculates the result of applying a linear regression model to a
#' set of input data. If a target mean value (TargetMean) is specified, the
#' function calculates a standard deviation of a sampling distribution which
#' is applied to linear model results. For each value returned by the linear
#' model, a sample is drawn from a normal distribution where the mean value of
#' the distribution is the linear model result and the standard deviation of the
#' distibution is calculated by the binary search to match the population mean
#' value to the target mean value. This process is meant to be applied to linear
#' model where the dependent variable is power transformed. Applying the
#' sampling distribution to the linear model results increases the dispersion
#' of results to match the observed dispersion and also matches the mean values
#' of the untransformed results. This also enables the model to be applied to
#' situations where the mean value is different than the observed mean value.
#'
#' @param Model_ls a list which contains the following components:
#' 'Type' which has a value of 'linear';
#' 'Formula' a string representation of the model equation;
#' 'PrepFun' a function which prepares the input data frame for the model
#' application. If no preparation, this element of the list should not be
#' present or should be set equal to NULL;
#' 'SearchRange' a two-element numeric vector which specifies the acceptable
#' search range to use when determining the dispersion factor.
#' 'OutFun' a function that is applied to transform the results of applying the
#' linear model. For example to untransform a power-transformed variable. If
#' no transformation is necessary, this element of the list should not be
#' present or should be set equal to NULL.
#' @param Data_df a data frame containing the data required for applying the
#' model.
#' @param TargetMean a number identifying a target mean value to be achieved  or
#' NULL if there is no target.
#' @param CheckTargetSearchRange a logical identifying whether the function
#' is to only check whether the specified 'SearchRange' for the model will
#' produce acceptable values (i.e. no NA or NaN values). If FALSE (the default),
#' the function will run the model and will not check the target search range.
#' @return a vector of numeric values for each record of the input data frame if
#' the model is being run, or if the function is run to only check the target
#' search range, a summary of predicted values when the model is run with
#' dispersion set at the high value of the search range.
#' @export
applyLinearModel <-
  function(Model_ls,
           Data_df,
           TargetMean = NULL,
           CheckTargetSearchRange = FALSE) {
    #Prepare data
    if (!is.null(Model_ls$PrepFun)) {
      Data_df <- Model_ls$PrepFun(Data_df)
    }
    #Define function for applying linear model
    calcValues <- function() {
      eval(parse(text = Model_ls$Formula), envir = Data_df)
    }
    #Define function to test match with TargetMean
    testModelMean <- function(SD) {
      Values_ <- calcValues()
      Est_ <- Values_ + rnorm(length(Values_), 0, sd = SD)
      if (!is.null(Model_ls$OutFun)) Est_ <- Model_ls$OutFun(Est_)
      TargetMean - mean(Est_)
    }
    #Define function for checking target search range
    testSearchRange <- function(Range_) {
      Values_ <- calcValues()
      Est_ <- Values_ + rnorm(length(Values_), 0, sd = Range_[2])
      if (!is.null(Model_ls$OutFun)) Est_ <- Model_ls$OutFun(Est_)
      Est_
    }
    #Calculate result
    if (CheckTargetSearchRange) {
      Result_ <- summary(testSearchRange(Model_ls$SearchRange))
    } else {
      if (is.null(TargetMean)) {
        Result_ <- calcValues()
        if (!is.null(Model_ls$OutFun)) Result_ <- Model_ls$OutFun(Result_)
      } else {
        SD <- binarySearch(testModelMean, Model_ls$SearchRange)
        Values_ <- calcValues()
        Result_ <- Values_ + rnorm(length(Values_), 0, sd = SD)
        if (!is.null(Model_ls$OutFun)) Result_ <- Model_ls$OutFun(Result_)
        attributes(Result_) <- list(SD = SD)
      }
    }
    Result_
  }


#WRITE TO THE VISIONEVAL NAME REGISTRY
#=====================================
#' Writes module Inp and Set specifications to the VisionEval name registry.
#'
#' \code{writeVENameRegistry} a visioneval framework control function that
#' writes module Inp and Set specifications to the VisionEval name registry.
#'
#' The VisionEval name registry (VENameRegistry.json) keeps track of the
#' dataset names created by all registered modules by reading in datasets
#' specified in the module Inp specifications or by returning calculated
#' datasets as specified in the module Set specifications. This functions adds
#' the Inp and Set specifications for a module to the registry. It removes any
#' existing entries for the module first.
#'
#' @param ModuleName a string identifying the module name.
#' @param PackageName a string identifying the package name.
#' @param NameRegistryList, if FALSE read and write the NameRegistryFile, if TRUE just compose a list for this module
#' @param NameRegistryDir a string identifying the path to the directory
#' where the name registry file is located.
#' @return TRUE if successful. Has a side effect of updating the VisionEval
#' name registry.
#' @export
writeVENameRegistry <-
  function(ModuleName, PackageName, NameRegistryList = FALSE, NameRegistryDir = NULL) {
    if ( ! NameRegistryList ) {
      #Check whether the name registry file exists
      if (is.null(NameRegistryDir)) {
        NameRegistryFile <- "VENameRegistry.json"
      } else {
        NameRegistryFile <- file.path(NameRegistryDir, "VENameRegistry.json")
      }
      if (!file.exists(NameRegistryFile)) {
        cat("NameRegistryDir: ")
        if ( ! is.null(NameRegistryDir) ) {
          cat(NameRegistryDir)
        } else {
          cat(getwd())
        }
        cat("\n")
        stop("VENameRegistry.json file is missing: cannot update.")
      }
      #Read in the name registry file as a list
      NameRegistry_ls <-
        jsonlite::fromJSON(readLines(NameRegistryFile), simplifyDataFrame = FALSE)
      #Remove any existing registry entries for the module
      for (x in c("Inp", "Set")) {
        NameRegistry_df <- readVENameRegistry(NameRegistryDir)
        ExistingModuleEntries_ <-
          NameRegistry_df[[x]]$MODULE == ModuleName &
          NameRegistry_df[[x]]$PACKAGE == PackageName
        NameRegistry_ls[[x]] <- NameRegistry_ls[[x]][ -ExistingModuleEntries_ ]
      }
    }

    #Define function to process module specifications
    processModuleSpecs <- function(Spec_ls) {
      #Define a function to expand a specification having multiple NAMEs
      expandSpec <- function(SpecToExpand_ls, ComponentName) {
        Names_ <- unlist(SpecToExpand_ls$NAME)
        Descriptions_ <- unlist(SpecToExpand_ls$DESCRIPTION)
        Expanded_ls <- list()
        for (i in 1:length(Names_)) {
          Temp_ls <- SpecToExpand_ls
          Temp_ls$NAME <- Names_[i]
          Temp_ls$DESCRIPTION <- Descriptions_[i]
          Expanded_ls <- c(Expanded_ls, list(Temp_ls))
        }
        Expanded_ls
      }
      #Define a function to process a component of a specifications list
      processComponent <- function(Component_ls, ComponentName) {
        Result_ls <- list()
        for (i in 1:length(Component_ls)) {
          Temp_ls <- Component_ls[[i]]
          Result_ls <- c(Result_ls, expandSpec(Temp_ls, ComponentName))
        }
        Result_ls
      }
      #Process the list components and return the results
      Out_ls <- list()
      Out_ls$RunBy <- Spec_ls$RunBy
      if (!is.null(Spec_ls$NewInpTable)) {
        Out_ls$NewInpTable <- Spec_ls$NewInpTable
      }
      if (!is.null(Spec_ls$NewSetTable)) {
        Out_ls$NewSetTable <- Spec_ls$NewSetTable
      }
      if (!is.null(Spec_ls$Inp)) {
        Out_ls$Inp <- processComponent(Spec_ls$Inp, "Inp")
      }
      if (!is.null(Spec_ls$Get)) {
        Out_ls$Get <- processComponent(Spec_ls$Get, "Get")
      }
      if (!is.null(Spec_ls$Set)) {
        Out_ls$Set <- processComponent(Spec_ls$Set, "Set")
      }
      if (!is.null(Spec_ls$Call)) {
        Out_ls$Call <- Spec_ls$Call
      }
      Out_ls
    }
    #Process the Inp and Set specifications
    ModuleSpecs_ls <-
      processModuleSpecs(getModuleSpecs(ModuleName, PackageName))
    Inp_ls <-
      lapply(ModuleSpecs_ls$Inp, function(x) {
        x$PACKAGE <- PackageName
        x$MODULE <- ModuleName
        x
      })
    Set_ls <-
      lapply(ModuleSpecs_ls$Set, function(x) {
        x$PACKAGE <- PackageName
        x$MODULE <- ModuleName
        x
      })
    #Save the revised name registry
    if ( ! NameRegistryList ) {
      #Add the the module specifications to the registry
      NameRegistry_ls$Inp <- c(NameRegistry_ls$Inp, Inp_ls)
      NameRegistry_ls$Set <- c(NameRegistry_ls$Set, Set_ls)
      writeLines(jsonlite::toJSON(NameRegistry_ls,pretty=TRUE), NameRegistryFile)
    }
    return(list(Inp=Inp_ls,Set=Set_ls))
  }


#READ THE VISIONEVAL NAME REGISTRY
#=================================
#' Reads the VisionEval name registry.
#'
#' \code{readVENameRegistry} a visioneval framework module developer function
#' that reads the VisionEval name registry and returns a list of data frames
#' containing the Inp and Set specifications.
#'
#' The VisionEval name registry (VENameRegistry.json) keeps track of the
#' dataset names created by all registered modules by reading in datasets
#' specified in the module Inp specifications or by returning calculated
#' datasets as specified in the module Set specifications. This function reads
#' the VisionEval name registry and returns a list of data frames containing the
#' registered Inp and Set specifications.
#'
#' @param NameRegistryDir a string identifying the path to the directory
#' where the name registry file is located.
#' @return A list having two components: Inp and Set. Each component is a data
#' frame containing the respective Inp and Set specifications of registered
#' modules.
#' @export
readVENameRegistry <-
  function(NameRegistryDir = NULL) {
    #Check whether the name registry file exists
    if (is.null(NameRegistryDir)) {
      NameRegistryFile <- "VENameRegistry.json"
    } else {
      NameRegistryFile <- file.path(NameRegistryDir, "VENameRegistry.json")
    }
    if (!file.exists(NameRegistryFile)) {
      stop("VENameRegistry.json file is not present in the identified directory.")
    }
    #Read in the name registry file
    jsonlite::fromJSON(readLines(NameRegistryFile))
  }


#GET REGISTERED GET SPECIFICATIONS
#=================================
#' Returns Get specifications for registered datasets.
#'
#' \code{getRegisteredGetSpecs} a visioneval framework module developer function
#' that returns a data frame of Get specifications for datasets in the
#' VisionEval name registry.
#'
#' The VisionEval name registry (VENameRegistry.json) keeps track of the
#' dataset names created by all registered modules by reading in datasets
#' specified in the module Inp specifications or by returning calculated
#' datasets as specified in the module Set specifications. This function
#' reads in the name registry and returns Get specifications for identified
#' datasets.
#'
#' @param Names_ A character vector of the dataset names to get specifications
#' for.
#' @param Tables_ A character vector of the tables that the datasets are a part
#' of.
#' @param Groups_ A character vector of the groups that the tables are a part of.
#' @param NameRegistryDir a string identifying the path to the directory
#' where the name registry file is located.
#' @return A data frame containing the Get specifications for the identified
#' datasets.
#' @export
getRegisteredGetSpecs <-
  function(Names_, Tables_, Groups_, NameRegistryDir = NULL) {
    #Put Names_, Tables_, Groups_ into data frame
    Datasets_df <-
      data.frame(
        NAME = Names_,
        TABLE = Tables_,
        GROUP = Groups_
      )
    #Check whether the name registry file exists
    if (is.null(NameRegistryDir)) {
      NameRegistryFile <- "VENameRegistry.json"
    } else {
      NameRegistryFile <- file.path(NameRegistryDir, "VENameRegistry.json")
    }
    if (!file.exists(NameRegistryFile)) {
      stop("VENameRegistry.json file is not present in the identified directory.")
    }
    #Read in the name registry file
    NameRegistry_df <- jsonlite::fromJSON(readLines(NameRegistryFile))
    #Identify attributes to return
    AttrNames_ <-
      c("NAME", "TABLE", "GROUP", "TYPE", "UNITS", "PROHIBIT", "ISELEMENTOF")
    #Define function to return records matching criteria
    extractRecords <- function(Data_df) {
      ToGetIdxNames_ <- apply(Datasets_df, 1, paste, collapse = "-")
      DataIdxNames_ <- apply(Data_df[,c("NAME", "TABLE", "GROUP")], 1, paste, collapse = "-")
      Data_df <- Data_df[DataIdxNames_ %in% ToGetIdxNames_, AttrNames_]
    }
    #Extract the specifications to be returned
    Specs_df <-
      rbind(extractRecords(NameRegistry_df$Inp),
            extractRecords(NameRegistry_df$Set))
    #Return data frame of identified Get specifications
    Specs_df
  }


#FETCH MODULE DATASETS FROM DATASTORE
#====================================
#' Returns the datasets that a module requires.
#'
#' \code{fetchModuleData} a visioneval framework module developer function
#' that fetches from the datastore a complete list of all the data required
#' by a module to run.
#'
#' The purpose of this function is to help module developers with debugging
#' modules during a model run. It is not uncommon for a new module to fail
#' during a module run due to an edge case that was not thought of during
#' module development. In such circumstances, it can be difficult to determine
#' the cause of the error without stepping though the module code; and to do
#' that requires creating from the datastore the datasets which cause the error
#' to occur. This function fetches the datasets from the datastore and returns
#' them in the form they are required to be in to run the module.
#'
#' Armed with the return from this function, you can manually source
#' a module and then step through its function call with the debugger...
#'
#' @param ModuleName a string identifying the name of the module.
#' @param PackageName a string identifying the name of the package that the
#' module is in.
#' @param Year a string identifying the model run year to retrieve the data
#' for.
#' @param Geo a string identifying the geography to retrieve the data for if
#' the module's 'RunBy' specification is not 'Region'. This argument is
#' omitted if the 'RunBy' specification is 'Region'.
#' @return A list in standardized form containing all the datasets required by
#' a module to run.
#' @export
fetchModuleData <- function(ModuleName, PackageName, Year, Geo = NULL) {

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
        Pkg_df <- getModelState()$ModulesByPackage_df
        Function <-
          paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
        rm(Pkg_df)
      }
      #Called module specifications
      Specs <- paste0(Function, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = Function))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
      Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
    }
  }

  #Get data from datastore
  #-----------------------
  #If RunBy is 'Region' get all data
  if (M$Specs$RunBy == "Region") {
    #Get data from datastore
    L <- getFromDatastore(M$Specs, RunYear = Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
      }
    }
  #If RunBy is not 'Region' get data for Geo
  } else {
    #Check that Geo has been specified
    if (is.null(Geo)) {
      Msg <- paste0(
        "The RunBy specification for module ", ModuleName, " is ",
        M$Specs$RunBy, ".", "You must specify the name of the ", M$Specs$RunBy,
        " you want to retrieve the datasets for using the 'Geo' argument."
      )
    }
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
    #Get data from datastore for Geo
    L <-
      getFromDatastore(M$Specs, RunYear = Year, Geo, GeoIndex_ls)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
      }
    }
  }

  #Return the Results
  #------------------
  if (exists("Call")) {
    return(list(L = L, M = Call$Func))
  } else {
    return(list(L = L))
  }

}


#CHECK MODULE AVAILABILITY
#=========================
#' Check whether a module required to run a model is present
#'
#' \code{checkModuleExists} a visioneval framework control function that checks
#' whether a module required to run a model is present.
#'
#' This function takes a specified module and package, checks whether the
#' package has been installed and whether the module is in the package. The
#' function returns an error message if the package is not installed or if
#' the module is not present in the package. If the module has been called by
#' another module the value of the 'CalledBy' argument will be used to identify
#' the calling module as well so that the user understands where the call is
#' coming from.
#'
#' @param ModuleName A string identifying the module name.
#' @param PackageName A string identifying the package name.
#' @param InstalledPkgs_ A string vector identifying the names of packages that
#' are installed.
#' @param CalledBy A string vector having two named elements. The value of the
#' 'Module' element is the name of the calling module. The value of the
#' 'Package' element is the name of the package that the calling module is in.
#' @return TRUE if all packages and modules are present and FALSE if not.
#' @export
checkModuleExists <-
  function(ModuleName,
           PackageName,
           InstalledPkgs_ = rownames(installed.packages()),
           CalledBy = NA) {
    ErrorMsg <- character(0)
    PackageMissing <- FALSE
    ModuleMissing <- FALSE
    #Check whether the package is installed and module is present in package
    PackageMissing <- !(PackageName %in% InstalledPkgs_)
    if (!PackageMissing) {
      PkgData_ <- data(package=PackageName)$results[,"Item"]
      PkgModules_ <- PkgData_[grep("Specifications", PkgData_)]
      PkgModules_ <- gsub("Specifications", "", PkgModules_)
      ModuleMissing <- !(ModuleName %in% PkgModules_)
    }
    #Compose error messages if any
    if (PackageMissing) {
      if (all(is.na(CalledBy))) {
        ErrorMsg <-
          paste0("Error in runModule call for module ", ModuleName,
                 " in package ", PackageName, ". Package ", PackageName,
                 " is not installed.")
      } else {
        ErrorMsg <-
          paste0("Error in runModule call for module ", CalledBy["Module"],
                 " in package ", CalledBy["Package"], ". This module calls Module ",
                 ModuleName, " in package ", PackageName, ". Package ", PackageName,
                 " is not installed.")
      }
    }
    if (ModuleMissing) {
      if (all(is.na(CalledBy))) {
        ErrorMsg <-
          paste0("Error in runModule call for module ", ModuleName,
                 " in package ", PackageName, ". Module ", ModuleName,
                 " is not present in package.")
      } else {
        ErrorMsg <-
          paste0("Error in runModule call for module ", CalledBy["Module"],
                 " in package ", CalledBy["Package"], ". This module calls Module ",
                 ModuleName, " in package ", PackageName, ". Module ", ModuleName,
                 " is not present in package.")
      }
    }
    #Return the error message
    ErrorMsg
  }


#GET MODULE SPECIFICATIONS
#=========================
#' Retrieve module specifications from a package
#'
#' \code{getModuleSpecs} a visioneval framework control function that retrieves
#' the specifications list for a module and returns the specifications list.
#'
#' This function loads the specifications for a module in a package. It returns
#' the specifications list.
#'
#' @param ModuleName A string identifying the name of the module.
#' @param PackageName A string identifying the name of the package that the
#' module is in.
#' @return A specifications list that is the same as the specifications list
#' defined for the module in the package.
#' @export
getModuleSpecs <- function(ModuleName, PackageName) {
  eval(parse(text = paste0(PackageName, "::", ModuleName, "Specifications")))
}
# Test_ls <-
#   getModuleSpecs(ModuleName = "CreateBzones", PackageName = "vedemo1")
# rm(Test_ls)


#EXPAND SPECIFICATION
#====================
#' Expand a Inp, Get, or Set specification so that is can be used by other
#' functions to process inputs and to read from or write to the datastore.
#'
#' \code{expandSpec} a visioneval framework control function that takes a Inp,
#' Get, or Set specification and processes it to be in a form that can be used
#' by other functions which use the specification in processing inputs or
#' reading from or writing to the datastore. The parseUnitsSpec function is
#' called to parse the UNITS attribute to extract name, multiplier, and year
#' values. When the specification has multiple values for the NAME attribute,
#' the function creates a specification for each name value.
#'
#' The VisionEval design allows module developers to assign multiple values to
#' the NAME attributes of a Inp, Get, or Set specification where the other
#' attributes for those named datasets (or fields) are the same. This greatly
#' reduces duplication and the potential for error in writing module
#' specifications. However, other functions that check or use the specifications
#' are not capable of handling specifications which have NAME attributes
#' containing multiple values. This function expands a specification with
#' multiple values for a  NAME attribute into multiple specifications, each with
#' a single value for the NAME attribute. In addition, the function calls the
#' parseUnitsSpec function to extract multiplier and year information from the
#' value of the UNITS attribute. See that function for details.
#'
#' @param SpecToExpand_ls A standard specifications list for a specification
#' whose NAME attribute has multiple values.
#' @param ComponentName A string that is the name of the specifications
#' that the specification is a part of (e.g. "Inp", "Get", "Set").
#' @return A list of standard specifications lists which has a component for
#' each value in the NAME attribute of the input specifications list.
#' @export
#Define a function which expands a specification with multiple NAME items
expandSpec <- function(SpecToExpand_ls, ComponentName) {
  SpecToExpand_ls <- parseUnitsSpec(SpecToExpand_ls, ComponentName)
  Names_ <- unlist(SpecToExpand_ls$NAME)
  Descriptions_ <- unlist(SpecToExpand_ls$DESCRIPTION)
  Expanded_ls <- list()
  for (i in 1:length(Names_)) {
    Temp_ls <- SpecToExpand_ls
    Temp_ls$NAME <- Names_[i]
    Temp_ls$DESCRIPTION <- Descriptions_[i]
    Expanded_ls <- c(Expanded_ls, list(Temp_ls))
  }
  Expanded_ls
}


#FILTER INP SPECIFICATIONS BASED ON WHETHER SPECIFICATION IS OPTIONAL
#====================================================================
#' Filters Inp specifications list based on OPTIONAL specification attributes.
#'
#' \code{doProcessInpSpec} a visioneval framework control function that filters
#' out Inp specifications whose OPTIONAL specification attribute is TRUE but the
#' specified input file is not present.
#'
#' An Inp specification component may have an OPTIONAL specification whose value
#' is TRUE. If so, and if the specified input file is present, then the input
#' specification needs to be processed. This function checks whether the
#' OPTIONAL specification is present, whether its value is TRUE, and whether the
#' file exists. If all of these are true, then the input specification needs to
#' be processed. The input specification also needs to be processed if it is
#' not optional. A specification is not optional if the OPTIONAL attribute is
#' not present or if it is present and the value is not TRUE. The function
#' returns a list of all the Inp specifications that meet these criteria.
#'
#' @param InpSpecs_ls A standard specifications list for Inp specifications.
#' @param InputDir A vector of paths in which to seek input files;
#'   the first path containing the named file will be used.
#' @return A list containing the Inp specification components that meet the
#'   criteria of being optional and present or being not optional. If
#'   the file is not optional and not present, throw an error and stop.
#'   The FILE element will be expanded to file.path(InputDir,$FILE) in
#'   the spec that is returned.
#' @export
doProcessInpSpec <- function(InpSpecs_ls, InputDir) {
  #Define function to check an individual specification
  #Return TRUE if missing file is not an error
  checkOptional <- function(SpecToCheck_ls) {
    IsOptional <- FALSE
    if (!is.null(SpecToCheck_ls$OPTIONAL)) {
      if (SpecToCheck_ls$OPTIONAL == TRUE) {
        IsOptional <- TRUE
      }
    }
    IsOptional
  }
  #Return all input specifications that must be processed
  Out_ls <- list()
  j <- 1
  for (i in 1:length(InpSpecs_ls)) {
    Spec_ls <- InpSpecs_ls[[i]]
    File <- file.path(InputDir, Spec_ls$FILE) # might be a vector
    FileExists <- file.exists(File)
    if ( ! any(FileExists) ) {
      if ( checkOptional(Spec_ls) ) {
        next # Do not add to Out_ls; continue to next InpSpec
      } else {
        Spec_ls$INPUTDIR <- NA # Required, but missing; trap later
      }
    } else {
      Spec_ls$INPUTDIR <- InputDir[FileExists][1]
    }
    Out_ls[[j]] <- Spec_ls
    j <- j + 1
  }
  Out_ls
}
#Test code
# setwd("tests")
# source("data/TestOptionalSpecs.R")
# doProcessInpSpec(TestOptionalSpecs$Inp)
# setwd("..")
# rm(TestOptionalSpecs)


#PROCESS MODULE SPECIFICATIONS
#=============================
#' Process module specifications to expand items with multiple names.
#'
#' \code{processModuleSpecs} a visioneval framework control function that
#' processes a full module specifications list, expanding all elements in the
#' Inp, Get, and Set components by parsing the UNITS attributes and duplicating
#' every specification which has multiple values for the NAME attribute.
#'
#' This function process a module specification list. If any of the
#' specifications include multiple listings of data sets (i.e. fields) in a
#' table, this function expands the listing to establish a separate
#' specification for each data set.
#'
#' @param Spec_ls A specifications list.
#' @return A standard specifications list with expansion of the multiple item
#' specifications.
#' @export
processModuleSpecs <- function(Spec_ls) {
  G <- getModelState()
  #Define a function to process a component of a specifications list
  processComponent <- function(Component_ls, ComponentName) {
    Result_ls <- list()
    for (i in 1:length(Component_ls)) {
      Temp_ls <- Component_ls[[i]]
      Result_ls <- c(Result_ls, expandSpec(Temp_ls, ComponentName))
    }
    Result_ls
  }
  #Process the list components and return the results
  Out_ls <- list()
  Out_ls$RunBy <- Spec_ls$RunBy
  if (!is.null(Spec_ls$NewInpTable)) {
    Out_ls$NewInpTable <- Spec_ls$NewInpTable
  }
  if (!is.null(Spec_ls$NewSetTable)) {
    Out_ls$NewSetTable <- Spec_ls$NewSetTable
  }
  if (!is.null(Spec_ls$Inp)) {
    InputDir <- if ( "InputDir" %in% names(G$RunParam_ls) ) {
      (G$RunParam_ls$InputDir)  # May be a vector; first matching file will be used
    } else { # backward compatible
      normalizePath("inputs",winslash="/",mustWork=FALSE)
    }
    FilteredInpSpec_ls <- doProcessInpSpec(Spec_ls$Inp, InputDir)
    if (length(FilteredInpSpec_ls) > 0) {
      Out_ls$Inp <- processComponent(FilteredInpSpec_ls, "Inp")
    }
  }
  if (!is.null(Spec_ls$Get)) {
    Out_ls$Get <- processComponent(Spec_ls$Get, "Get")
  }
  if (!is.null(Spec_ls$Set)) {
    Out_ls$Set <- processComponent(Spec_ls$Set, "Set")
  }
  if (!is.null(Spec_ls$Call)) {
    Out_ls$Call <- Spec_ls$Call
  }
  Out_ls
}
