#' @include parameters.R
#==================
#Dynamic.R
#==================
#
#<doc>
#
## Dynamic Module
#### March 14, 2023
#
#This module simply prints a message into the model run log file when it appears in a runModule directive.
#It illustrates the basic structure of a VE module and specifically shows how to use visioneval.cnf to locate
#module configuration information, and how to generate module Specifications dynamically using a function.
#
### How the Module Works
#
#The user configures the visioneval.cnf script through the Dynamic: block. The block can be in the model's
#main visioneval.cnf or anywhere else in the configuration tree for the current model stage (e.g. the model, or
#just the stage). It can also be in a visioneval.cnf located in the parameter directory DynamicDir, which will
#be sought in the model root, in the defs folder, or along the input path (so basically anywhere you like).
#
#Dynamic is configured and used in the the VESnap test model, along with the Snapshot module, so you can see
#how they work.
#
### Configuring Dynamic
#
#The Dynamic configuration is placed in visioneval.cnf, where it
#has these elements
#
#'''
#DynamicDir: mydynamic # optional directory with a Dynamic specification in its visioneval.cnf (default: "dynamic")
#Dynamic: # located somewhere in the model stage's configuration tree
#  Message:     "An optional message printed to the model run Log"
#  RunBy:       Region     # the default; you can also specify Marea, Azone or Bzone to see the difference at runtime
#  LogLevel:    warn       # the default; you can specify any of "error","warn","info","debug" or "trace"
#  Field:                  # optional: creates a "Get" specification for an existing field and logs its summary
#    Group:     Year       # replaced with current run year, or you can specify "Global" to look there
#    Table:     Household  # or whatever
#    Name:      HhSize     # or whatever
#    Operation: mean       # a function applied to the Field values for the logging (default: length); must yield a scalar
#'''
#
#If you change the Field configuration, any Group/Table/Name object must already have been specified in an earlier
#module call in the ModelScript (i.e. before the `runModule("Dynamic",...)` step). You'll get a framework error If Field
#is specified, but there is no matching field when the Dynamic module runs.
#
#See the VESnap test model for a working example.
#
#</doc>

#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================

#This module has no parameters to estimate.

#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

# Dynamic builds its specifications dynamically. It will generate an empty specification list
# unless the Field block appears in its configuration
DynamicSpecifications <- list(
  Function="getDynamicField", # name of the function (no need to export) that returns the specification
  Specs=TRUE # Should the specifications from previous modules in the run script be passed to this function?
             # Specs is only needed to support the Fields directive in Dynamic. Real-world dynamic
             # specifications should make this TRUE if they are planning to "Get" fields from the Datastore

             # If this Dynamic module is only doing "Set" or "Inp" specifications, then Specs here can be left
             # out, or explicity set to its default value of FALSE.
)

#Save the data specifications list
#---------------------------------
# "DynamicSpecifications"
#' Specifications list for the Dynamic module (wrapper around dynamic specification function)
#'
#' Dynamic module illustrates dynamic model specification (through a function). See the Dynamic
#' entry in VESnapshot/model_docs, or R help for \code{VESnapshot::Dynamic}.
#' @format a list with one or two elements (Function and Specs)
#' @source Dynamic.R script
#' @name DynamicSpecifications
visioneval::savePackageDataset(DynamicSpecifications, overwrite = TRUE)

# An environment constructed within the package that will hold the Dynamic message and configuration
# loaded during the specification creation. Shows how to save "session" details during a model run.
dynamic.env <- new.env()

#' Function to generate module specifications for Dynamic from visioneval.cnf
#'
#' Module Specification function, returning a list of "Get" and "Set" elements (and possibly "Inp") that will
#' be supplied to, and retrieved from, this module.
#'
#' This function is called internally and need not be exported from the VESnapshot Namespace. The actual
#' module function \code{Dynamic} (below) is exported. The AllSpecs_ls is required as a parameter if "Specs"
#' is true in the DynamicSpecifications above (and in any case where you expect to "Get" existing fields from
#' the Datastore). The Cache parameter can be used to avoid recomputing the statistics twice: the VisionEval
#' framework calls the specification function twice and expects both results to be the same. The first call
#' has Cache==FALSE (asking for the specification to be built) and the second has Cache==TRUE, which grants
#' permission (but does not required) that the previously-built specifications be returned without recomputing
#' them.
#' 
#' Even though this function is exported from this example, other modules implementing a dynamic
#' specification function do not not need to export it: all that needs to be present is the specification
#' stub tha names the function (see DynamicSpecifications above).
#' 
#' @param AllSpecs_ls a list of specifications for all known packages and modules defined up to this point
#'    in the model run. Furnished by the framework if "Specs" is true (see DynamicSpecifications above), and
#'    furnished as NA otherwise.
#' @param Cache a logical provided by the framework. FALSE when first building the specification; TRUE when
#'    the specification is accessed during a model run.
#' @return a "Get" specification if there is valid "Field:" block in the Dynamic configuration, otherwise
#'    just return an empty list.
#' @export
getDynamicField <- function(AllSpecs_ls=NA,Cache=FALSE) {
  # Used cached specification when calling from runModule
  if ( Cache && "Specs_ls" %in% names(dynamic.env) ) {
    # Use cache
    return( dynamic.env$Specs_ls )
  } else {
    # Clear cache
    rm(list=ls(dynamic.env),envir=dynamic.env)
  }

  # General instructions for building a dynamic Specification function:
  
  # If Specs is TRUE in the module specification structure, then AllSpecs_ls will be passed into the function
  # so this module can see what other modules have specified. It is good practice to default AllSpecs_ls when
  # defining the specification function so it can be called without AllSpecs_ls. If the function truly
  # requires AllSpecs_ls (like this one), it needs to throw an error if AllSpecs_ls is not available.

  # The Instance parameter can safely be ignored in most cases. It is there to support the Snapshot function
  #   (allowing multiple calls to Snapshot in a single model run, with possibly different fields being snapshotted
  #   each time. If you leave out "Specs" in the specification function description (see above in this file), you
  #   won't get AllSpecs_ls passed at all. Likewise, if there is no explicit "Instance" in the runModule call (see
  #   the VESnap sample model script) then Instance also won't be passed, so you could just define a function
  #   without parameters.
  # The Cache parameter is set by the framework: when the Specification is requested during model initialization
  #   Cache is FALSE and the dynamic.env will be rebuilt; when the Specification is requested again as runModule
  #   happens, Cache is TRUE. The specification function can ignore Cache (and just regenerate each time).

  # Look for the "Dynamic" specification
  config <- visioneval::getRunParameter("Dynamic") # looking in the RunParam_ls for the current model stage
  Message <- if ( ! is.list(config) ) {
    # Could add any other directories you like
    DynamicDir <- visioneval::getRunParameter("DynamicDir",Default=NA)
    if ( is.character(DynamicDir) ) {
      ModelDir   <- visioneval::getRunParameter("ModelDir")
      ParamPath  <- visioneval::getRunParameter("ParamPath") # already expanded from ParamDir to absolute path
      InputPath  <- visioneval::getRunParameter("InputPath") # already expanded to all the places to look for inputs
      configDir  <- findFileOnPath( DynamicDir, c(ModelDir,ParamPath,InputPath) )
      if ( !is.na(configDir) ) {
        dynamicParam_ls <- readConfigurationFile(ParamDir=configDir) # look for visioneval.cnf or equivalent
        config <- getRunParameter("Dynamic",Param_ls=dynamicParam_ls)
        paste("Loaded:",configDir)
      } else {
        paste("Could not open Dynamic configuration file in",configDir)
      }
    } else {
      paste("Dynamic or DynamicDir not provided in visioneval.cnf")
    }
  } else {
    if ( "Message" %in% names(config) ) config$Message else "No message supplied"
  }

  # If still no configuration specified, do a default configuration
  if ( ! is.list(config) ) {
    Message = c("Unconfigured Dynamic module",Message)
    dynamic.env$Message <- Message
    return( list() ) # No specifications for this module; let framework inject RunBy="Region"
  }

  # Process configuration
  Specs_ls <- if ( "Field" %in% names(config) ) {
    field <- config$Field
    if (!is.character(field$Operation)) field$Operation <- "length"
    Message <- c(Message,paste("Operation is",field$Operation))

    dynamic.env$Field <- field
    if ( ! all(nzchar(field$Group),nzchar(field$Table),nzchar(field$Name)) ) {
      Message <- c(Message,"Missing Group, Table or Name")
      list()
    } else {
      Message <- c(
        Message,paste("Field specified:",GTN<-file.path(field$Group,field$Table,field$Name))
      )
      # Find the first occurrence of Group/Table/Name in AllSpecs_ls
      # Pull out the TYPE, UNITS, PROHIBIT, ISELEMENTOF fron its attributes
      fieldSpec <- NA
      for ( p in AllSpecs_ls ) {
        for ( spec in p$Specs$Set ) {
          if ( all
            (
              isTRUE(spec$GROUP==field$Group) &&
              isTRUE(spec$TABLE==field$Table) &&
              isTRUE(spec$NAME==field$Name)
            ) ) {
            fieldSpec <- spec
            break
          }
        }
        if ( is.list(fieldSpec) ) break
      }
      if ( !is.list(fieldSpec) ) {
        Message <- c(Message,paste("No existing field spec found for",GTN))
        list()
      } else {
        list( Get = list(fieldSpec) )
      }
    }
  } else {
    list()
  }
  Specs_ls$RunBy <- if ( "RunBy" %in% names(config) ) config$RunBy else "Region"
  Specs_ls$Inp <- NULL
  Specs_ls$Set <- NULL

  dynamic.env$Specs_ls <- Specs_ls
  dynamic.env$Message <- Message

  return( Specs_ls )
}

#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
# This module constructs an interesting message (possibly including statistics about a Datastore field)
# and prints it to the model run log.
#' Illustrates use of Dynamic specification generator
#'
#' This module doesn't do anything particularly useful. It just makes some messages and can generate
#' a summary statistic for one field in the Datastore. It does provide a simple code example for
#' using dynamic specifications.
#'
#' @param L A list tracking the getSpecification request
#' @return An empty list (a real module would "Set" some data in the Datastore - see Snapshot module)
#' @export
Dynamic <- function( L ) {
  # L contains a master tree of nested lists: Year, Global, BaseYear, plus the ModelState ("G")
  # The only items populated in that tree are what was requested in the Specification
  # This function does not "Set" parameters, but if there is a "Set" specification, the list
  # that is returned should include those elements (which do NOT have to be in a nested list
  # format - see the framework documentation).
  logLevel <- if ( "LogLevel" %in% names(dynamic.env) ) dynamic.env$LogLevel else "warn"
  
  Summary <- if ( "Field" %in% names(dynamic.env) ) {
    if ( length(L)>0 ) {
      operation <- if ( "Operation" %in% names(dynamic.env$Field) ) dynamic.env$Field$Operation else "length"
      obj <- L[[dynamic.env$Field$Group]][[dynamic.env$Field$Table]][[dynamic.env$Field$Name]]
      summ <- with (dynamic.env, paste("Summarizing:",file.path(Field$Group,Field$Table,Field$Name)))
      summValue <- eval(parse(text=paste(operation,"(obj)")))
      c(summ,paste("Operation",operation),paste("Results:",summValue))
    } else {
      "Field requested for Dynamic, but no Specification"
    }
  } else "No Field to summarize"

  Message <- if ( "Message" %in% names(dynamic.env) ) {
    dynamic.env$Message
  } else "Specification not processed for Dynamic module"

  visioneval::writeLog(paste("Dynamic: ",c(Message,Summary)),Level=logLevel)

  list() # Dynamic never Sets any fields in the Datastore
}

#==============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILIARY DEVELOPMENT CODE
#==============================================================
#Run module automatic documentation
#----------------------------------
#' @importFrom visioneval documentModule
visioneval::documentModule("Snapshot")
