#' @include parameters.R
#==================
#Snapshot.R
#==================
#
#<doc>
#
## Snapshot Module
#### March 14, 2023
#
#This module illustrates the basic structure of a VE module and implements a useful test facility that can copy an
#arbitrary field (or more than one) present in the Datastore into a new field. Useful for grabbing a picture of a field
#that is revised at different points in the model stream. Run 'installModel("VESnap")' to install a test module that
#illustrates the application of the Snapshot module, including some spiffy features that allow you to take multiple
#snapshots of the same field and keep them separate in the Datastore.
#
### Model Parameter Estimation
#
#This module has no parameters and nothing is estimated.
#
### How the Module Works
#
#The user specifies fields to copy in their visioneval.cnf script in the Snapshot: block. In the model's run
#script, add a series of runModule("Shapshot",...) commands, including the Instance parameter to distinguish
#which entry in visioneval.cnf will apply to that Snapshot.
#
#See the VESnap test model that can be installed from this package. When the model runs and the
#runModule("Snapshot",...) instruction is encountered, the specified fields are copied into new fields in the
#Datastore where they can later be extraced, queried or otherwise used.
#
### Configuring Snapshot
#
# The snapshot configuration is placed in the main visioneval.cnf, or within SnapshotDir (system
# parameter) which defaults to "snapshot" and which will be sought in the Model root directory, or
# in the Parameer directory ('defs'). The Snapshot configuration will look like this in its fully
# developed form:
#
# '''
# Snapshot:
#   - Instance: "First"
#     Group:        # The Datastore group (shortcut "Year" is acceptable)
#     Table:        # The Datastore table within Group
#     Name:         # The Datastore field with Table
#     SnapshotName: # The name of the field that will be created (in the same Group/Table as the input)
#   - Instance: "Second"
#     Group:
#     Table:
#     Name:
#     SnapshotName:
#     LoopIndex: 1
#       # If LoopIndex is missing, snapshot each time through the loop, overwriting to the same place.
#       # Otherwise only take the snapshot if the LoopIndex passed to runModule matches this number
# '''
#
# The Instance is optional. If it is not specified, it will be ignored if it is passed from the
# runModule instruction, and referred to as "NA" (literal character string, not an R NA value). A
# minimal Snapshot will look like the following. Note the dash ahead of 'Group'. Snapshot must
# define a list of objects (that leading dash), even if there is only one Snapshot
#
# '''
# Snapshot:
#   - Group:
#     Table:
#     Name:
#     SnapshotName:
# '''
#
# Any Group/Table/Name object must already have been specified in an earlier module call in the
# ModelScript (i.e. before the `runModule("Snapshot",...)` step), and it will be looked up in the
# AllSpecs_ls parameter for the properties required to construct a Get or Set specification.
#
# Finally, you can use Instance and LoopIndex together with this configuration:
#
# '''
# # In visioneval.cnf:
# Snapshot:
#   - Instance:     "FirstLoop"
#     Group:        "Year"
#     Table:        "Household"
#     Name:         "DVMT"
#     SnapshotName: "HhDVMTFirstLoop"
#     LoopIndex:    1
#   - Instance:     "SecondLoop"
#     Group:        "Year"
#     Table:        "Household"
#     Name:         "DVMT"
#     SnapshotName: :HhDVMTSecondLoop
#     LoopIndex:    2
#
# # In model run script (run_model.R):
# ... # Earlier script lines
# for (i in 1:2) {
#   runModule("Snapshot", "VESnapshot", RunFor = "AllYear", RunYear = Year, Instance="FirstLoop", LoopIndex=i)
#   runModule("Snapshot", "VESnapshot", RunFor = "AllYear", RunYear = Year, Instance="SecondLoop", LoopIndex=i)
#   ... # Remainder of loop
# }
# ... # Remainder of run script
# '''
#
# See the VESnap test model, variant snapshot, for a working example.
#
#</doc>

#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================

#This module has no parameters to estimate.

#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

# An environment constructed within the package that will hold the Snapshot configuration
# loaded during the specification creation. Shows how to save "session" details during a model run.
snapshot.env <- new.env()

# Snapshot builds its specifications dynamically by locating fields in the Datastore that
# the model developer identifies in their visioneval.cnf and returning renamed copies of
# whatever is in those fields at runtime.
SnapshotSpecifications <- list(
  Function="getSnapshotFields",
  Specs=TRUE # Specs is only needed if the specification Function wants to see AllSpecs_ls
             # Snapshot does need it (since we are copying a spec from another module)
             # Other modules usually won't, and their Get or Set or Inp will be generated
             # internally); if it's not needed, you can leave it out entirely (default is FALSE)
)

#Save the data specifications list
#---------------------------------
# "SnapshotSpecifications"
#' Specifications list for the Snapshot module
#'
#' Snapshot illustrates dynamic model specification (through a function). See the Snapshot entry in
#' VESnapshot/model_docs, or R help for \code{VESnapshot::Snapshot}
#' @format a list with one or two elements (Function and Specs)
#' @source Snapshot.R script
#' @name SnapshotSpecifications
visioneval::savePackageDataset(SnapshotSpecifications, overwrite = TRUE)

# Module Specification function, returning a list of "Get" and "Set" elements (and possibly "Inp") that will
# be supplied to, and retrieved from, this module. This function is called internally and not
# exported in the VESnapshot Namespace. The module function \code{Snapshot} is exported.
#' Function to generate module specifications for Snapshot from visioneval.cnf
#' @param AllSpecs_ls a list of specifications for all known packages and modules in the model run
#' @param Instance is the name (possibly an empty character vector if there is just one Instance) of
#'   the Snapshot instance, defined via Instance="SnapshotInstance" in the runModule("Snapshot",...)
#'   instruction in the model script.
#' @param Cache if TRUE, use the locally cached parameters rather than regenerating (Cache=TRUE is
#'   passed from the runModule command when the module is being run).
#' @return a list of specifications (Inp, Get and/or Set) descripbing fields to inject into the Datastore
#' @export
getSnapshotFields <- function(AllSpecs_ls=NA,Instance=character(0), Cache=FALSE) {

  # Check AllSpecs_ls
  if ( ! is.list(AllSpecs_ls) || length(AllSpecs_ls)==0 ) {
    stop(
      visioneval::writeLog( "Snapshot can only be run if items are created first by other modules",Level="error")
    )
  }

  # Make sure Instance can be used as a list element name
  # Don't just want to default it - also want to head off "non-Instance"
  if ( length(Instance)==0 || Instance=="" || is.na(Instance) ) {
    Instance = "NA"
  }

  # Returned cached specifications if they are available
  if ( Cache ) {
    return( snapshot.env$Spec_ls[[Instance]] )
  } else {
    # Clear cache
    rm( list=ls(snapshot.env), envir=snapshot.env )
  }

  # Set up configurations in instanceList in snapshot.env
  snapConfig <- visioneval::getRunParameter("Snapshot") # Will search modelEnvironment()$RunParam_ls
  if ( !is.list(snapConfig) ) {
    # Conduct directory search
    snapDir   <- visioneval::getRunParameter("SnapshotDir")
    ModelDir  <- visioneval::getRunParameter("ModelDir")
    ParamPath <- visioneval::getRunParameter("ParamPath") # already expanded from ParamDir to absolute path
    configDir  <- findFileOnPath( snapDir, c(ModelDir,ParamPath) )
    if ( !is.na(configDir) ) {
      snapshotParam_ls <- readConfigurationFile(ParamDir=configDir) # look for visioneval.cnf or equivalent
      snapConfig <- getRunParameter("Snapshot",Param_ls=snapshotParam_ls)
      visioneval::writeLog( paste("Loaded Snapshot configuration:",configDir), Level="info" )
    } else {
      visioneval::writeLog( paste("Could not locate Snapshot configuration file"), Level="info" )
    }
  }

  # If Snapshot config is not found, write a warning to the Log and return an empty list
  if ( ! is.list(snapConfig) ) {
    # Not fatal to be unconfigured, but will generate a message
    snapshot.env$Spec_ls <- list()
    if ( length(Instance) == 0 ) {
      Instance <- "NA"
      visioneval::writeLog(
        c (
          "No configuration found for default Snapshot",
          "No Snapshot taken."
        ) , Level="error"
      )
    } else {
      visioneval::writeLog(
        c (
          paste("No configuration found for Snapshot Instance",Instance[1]),
          "No Snapshot taken."
        ) , Level="error"
      )
    }
    snapshot.env$Spec_ls[Instance] <- list( RunBy = "Region") # could just return empty; framework will provide
    return( snapshot.env$Spec_ls[[Instance]] )
  }

  # Set up list of snapshot instance configurations
  # The actual spec will be built below Instance-by-Instance while initializing the model
  # script. Specs will only be built for Instances that appear in runModule, but the
  # configuration can include lots more Instances (it's no problem to define them and not
  # use them.
  visioneval::writeLog("Processing snapshot instances",Level="info")
  snapshot.env$instanceList <- list()

  # Now dig into the configuration and pull out the Instances (we'll process the requested one
  # below)
  if ( length(snapConfig)==1 && ! "Instance" %in% names(snapConfig[[1]]) ) {
    # Add dummy name for Instance if there is only one and "Instance" element is missing
    snapConfig[[1]]$Instance <- "NA"
  }
  for ( instanceSpec in snapConfig ) {
    if ( ! "Instance" %in% names(instanceSpec) ) {
      # Snapshot and model will die if we have an Instance without a name
      stop (
        visioneval::writeLog("More than one instance configured, but Instance name is missing",Level="error")
      )
    }
    # Move "Instance" from being a list element to name of the list of other elements
    snapshot.env$instanceList[[instanceSpec$Instance]] <- instanceSpec[ ! names(instanceSpec) %in% "Instance" ]
  }

  # snapshot.env$instanceList contains all the Instance specifications

  # Now build the specification for this specific Instance
  if ( ! "Spec_ls" %in% names(snapshot.env) ) snapshot.env$Spec_ls <- list()
  visioneval::writeLog("Building Snapshot Spec_ls",Level="info")

  # Create (and cache) just the snapshot specifications for the current instance
  instanceSpec <- snapshot.env$instanceList[[Instance]]

  Group <- instanceSpec$Group
  Table <- instanceSpec$Table
  Name  <- instanceSpec$Name
  SnapshotName <- if ( "SnapshotName" %in% names(instanceSpec) ) {
    instanceSpec$SnapshotName
  } else if ( Instance != "NA" ) {
    paste0(Instance,"Snapshot")
  } else {
    paste0(instanceSpec$Name,"Snapshot")
  }

  # Find where the field was originally Set
  if ( any(is.na(c(Group,Table,Name))) || ! all(nzchar(c(Group,Table,Name))) ) {
    stop( visioneval::writeLog("Missing Group, Table or Name from Snapshot Specification",Level="error") )
  } else {
    # Find the LATEST occurrence of Group/Table/Name in AllSpecs_ls
    # (most recently altered)
    # Pull out the TYPE, UNITS, PROHIBIT, ISELEMENTOF fron its attributes
    sourceSpec <- NA
    for ( pidx in rev(seq_along(AllSpecs_ls)) ) {
      for ( spec in AllSpecs_ls[[pidx]]$Specs$Set ) {
        if ( all
          (
            isTRUE(spec$GROUP==Group) &&
            isTRUE(spec$TABLE==Table) &&
            isTRUE(spec$NAME ==Name)
          ) ) {
          sourceSpec <- spec
          break
        }
      }
      if ( is.list(sourceSpec) ) break
    }
    if ( !is.list(sourceSpec) ) {
      stop( visioneval::writeLog( paste("No existing field spec found for",file.path(Group,Table,Name)), Level="error") )
    }
  }
  visioneval::writeLog(paste("Snapshotting",file.path(Group,Table,Name),"from",names(AllSpecs_ls[pidx])),Level="info")

  # Construct the getSpec, which will just be the sourceSpec with the new SnapshotName
  snapshotSpec      <- sourceSpec
  snapshotSpec$NAME <- SnapshotName

  # "Get" and "Set" are lists of individual specs
  # Snapshot just puts one list element into each
  snapshot.env$Spec_ls[[Instance]] <- list(
    RunBy = "Region",
    Get = list(sourceSpec), # presuming we can just ignore Set-only parameters like SIZE
    Set = list(snapshotSpec)
  )
  
  return( snapshot.env$Spec_ls[[Instance]] )
}

#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
# This module takes a specified list of Datastore elements (defined in visioneval.cnf) and makes copies of them under
# new names in the Datastore. See the VESnap sample model from the VEModel package for examples and documentation of how
# to configure the fields.

#' Main Module function that copies datastore fields to new fields with a different name
#' \code{Snapshot} takes a snapshot of Datastore fields. Use it as runModuleInstruction. See the
#' VESnap sample model in VEModel for detailed instructions on setup and use. @param L A list
#' containing the components listed in the Get specifications for the module.
#'
#' Using LoopIndex and Instance in the runModule instruction (they are optional and may be left
#' out) will append those to the output field names so multiple snapshots of the same value can be
#' taken at different points in the model script. They also identify the particular instance of
#' Shapshot so different fields can be snapshotted at different points in the script. Each instance
#' of Snapshot can be identifed by Instance in visioneval.cnf, and the fields to snapshot will be
#' selected using identifiers the Loop/Instance block. See the VESnap sample model for more
#' information.
#'
#' To configure a Snapshot, you need to add a "Snapshot" element to visioneval.cnf (somewhere in
#' what is being built for the running model stage). Alternatively, you can put a visioneval.cnf
#' with a "Snapshot" element in SnapshotDir (default is "snapshot", located either in the model's
#' root directory, or in the model's ParamDir (which defaults to "defs" - so "defs/snapshot").
#'
#' The snapshot configuration is placed in visioneval.cnf, where it
#' will look like this in its fully developed form:
#' 
#' \code{
#' Snapshot:
#'   - Instance: "First"
#'     Group:
#'     Table:
#'     Name:
#'     SnapshotName:
#'   - Instance: "Second"
#'     Group:
#'     Table:
#'     Name:
#'     LoopIndex: 1
#'       #If LoopIndex is missing, snapshot each time through the loop to the same place. Otherwise
#'       #only during this loop index.
#'     SnapshotName:
#' }
#' 
#' A minimal Snapshot will look like the following (Instance is implicitly
#' `""`). Note the dash ahead of Group - Snapshot must define a list
#' of objects, even if there is only one.
#' 
#' \code{
#' Snapshot:
#'   - Group:
#'     Table:
#'     Name:
#'     SnapshotName:
#' }
#'
#' Any Group/Table/Name object must already have been specified in an earlier module call in the
#' ModelScript (i.e. before the `runModule("Snapshot",...)` step).
#'
#' @param L A list following the getSpecification structure
#' @param LoopIndex A numeric value to use in distinguishing Snapshot output field names within a model run script loop
#' @param Instance A string value that identifies this instance of Snapshot in visioneval.cnf. Different Snapshot instances
#' @return a list of data elements to be added to the Datastore (copies of the L parameter inputs)
#' @export
Snapshot <- function( L, LoopIndex=0, Instance=character(0) ) {
  # L will contain the data to snapshot
  # LoopIndex can be non-zero if specified in the runModule call, and will be compared to LoopIndex in the
  #  the Snapshot configuration (if present). If LoopIndex here matches the one in the configuration, or if no
  #  LoopIndex is configured for the Instance, the Snapshot will be performed. If loopIndex does not match,
  #  the Snapshot will be skipped.
  # Instance will default in runModule to character(0), so only the first defined Snapshot will be used.
  # Otherwise Instance is a character string that will match the "Instance" element value in the Snapshot configuration
  # At runtime, the Snapshot configuration for the Instance will be loaded, and LoopIndex, if
  # non-zero, will be compared to the configuration and used to determine if an output field will
  # be generated. The default with LoopIndex==0 will be to overwrite the output field each time
  # through the loop.

  # Look up the Instance specification so we know what to pull out of L
  instance <- snapshot.env$instanceList[[Instance]]
  if ( ! is.list(instance) ) {
    stop( visioneval::writeLog(paste("No Snapshot defined for",Instance),Level="error") )
  }

  # Copy the input field data to the output
  # I always marvel at how much work must be done just to run one
  # trivial line of code!
  Out_ls <- list()
  Out_ls[[instance$Group]][[instance$Table]][[instance$SnapshotName]] <- L[[instance$Group]][[instance$Table]][[instance$Name]]
  return( Out_ls )
}

#==============================================================
#SECTION 4: MODULE DOCUMENTATION AND AUXILIARY DEVELOPMENT CODE
#==============================================================
#Run module automatic documentation
#----------------------------------
#' @importFrom visioneval documentModule
visioneval::documentModule("Snapshot")
