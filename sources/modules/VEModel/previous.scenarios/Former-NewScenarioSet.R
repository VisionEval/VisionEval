#=================
#NewScenarioSet.R
#=================
# This module builds scenarios from the combinations scenario input levels.

library(visioneval)
library(jsonlite)

#=================================================
#SECTION 1: ESTIMATE AND SAVE Scenario PARAMETERS
#=================================================

# No estimation required

#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

NewScenarioSetSpecifications <- list(
  # Level of geography module is applied at
  RunBy = "Region",
  # Specify new tables to be created by Inp if any
  # Specify new tables to be created by Set if any
  # NewSetTable
  # Specify Input data
  # Specify data to be loaded from the datastore
  Get = items(
    
    item(
      NAME = "ModelFolder",
      TABLE = "Model",
      GROUP = "Global",
      TYPE = "character",
      UNITS = "NA",
      SIZE = 20,
      PROHIBIT = "NA",
      ISELEMENTOF = ""
    )
    ,
    
    item(
      NAME = "ScenarioInputFolder",
      TABLE = "Model",
      GROUP = "Global",
      TYPE = "character",
      UNITS = "NA",
      PROHIBIT = "NA",
      ISELEMENTOF = "",
      DESCRIPTION = "Scenario Input folder"
      
    )
    ,
    item(
      NAME = "ScenarioManagerFile",
      TABLE = "Model",
      GROUP = "Global",
      TYPE = "character",
      UNITS = "NA",
      PROHIBIT = "NA",
      ISELEMENTOF = "",
      DESCRIPTION = "name of scenario management csv database"
      
    )
  ),
  Set = items(
    item(
      NAME = "Scenarioset",
      TABLE = "Model",
      GROUP = "Global",
      TYPE = "integer",
      UNITS = "NA",
      PROHIBIT = "NA",
      ISELEMENTOF = "",
      DESCRIPTION = "Returns 1 if scenario set build is complete"
    )
  )
)

#Save the data specifications list
#---------------------------------
#' Specifications list for NewScenarioSet module
#'
#' A list containing specifications for the NewScenarioSet module.
#'
#' @format A list containing 4 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Inp}{scenario input data to be loaded into the datastore for this
#'  module}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
#' @source NewScenarioSet.R script.
"NewScenarioSetSpecifications"
visioneval::savePackageDataset(NewScenarioSetSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#Main module function builds scenarios
#------------------------------------------------------------------
#' Interior Function to create scenario set.
#'
#' \code{NewScenarioSetInterior} interior function that builds structure to run mulitple scenarios.
#'
#'Check that each of the input_filenames exist in the ScenarioBaselineInputFolder.
#'Populate scenario folders with the input_filename files into the appropriate folders described in 
#'the ScenaroManagerFile. If the policy_label is Base or Baseline, copy the file without modifying 
#'its name. If the policy_label is not Base or Baseline, then copy the file with a prefix of UNEDITED-, 
#'to signal to the user that this file needs to be edited. This method should have an overwrite boolean 
#'argument that defaults to False to avoid overwriting existing input.  
#'
#' @param ScenarioInputFolder a file path identifying the location on the computer hard disk where the inputs for the scenarios will be located
#' @param ScenarioManagerFile the file path of the CSV file described above, which is expected to be in the ScenarioInputFolder
#' @param ModelFolder The (preferably normalized) path to the folder containing the Base Model run_model.R  script that will run each of the constructed scenarios plus its other elements 
#' @param ScenarioBaselineInputFolder ScenarioBaselineInputFolder — the location on the computer of the hard disk where the inputs used for the baseline scenarios are located
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @name NewScenarioSetInterior
#' @import jsonlite 
#' @export
   NewScenarioSetInterior <- function(ScenarioInputFolder=NULL, ScenarioManagerFile, ModelFolder, ScenarioBaselineInputFolder=NULL){
     assign("%>%",getFromNamespace("%>%","magrittr"))
     
         if (is.null( ScenarioInputFolder)) {
      if (is.null( ScenarioManagerFile)) {
        stop("ScenarioInputFolder is missing")
        
      } else {
        ScenarioInputFolder = dirname(normalizePath(ScenarioManagerFile))
      }
    }
    
    if (is.null( ScenarioBaselineInputFolder)) {
      if (is.null( ModelFolder)) {
        stop("Base scenario folder directory is missing")
        
      } else {
        ScenarioBaselineInputFolder = file.path(ModelFolder, "inputs")
      }
    }
    
    
    # private methods --------------------------------------------------------------
    .ReadScenarioFormFromCSV <- function(input_file_name) {
      
      return_df <- readr::read_csv(input_file_name, 
                                   col_types = readr::cols(.default = readr::col_character())) %>%
        dplyr::mutate(category_description = tidyr::replace_na(category_description, ""))
      
      return(return_df)
      
    }
    
    .MakeScenarioJsonFromForm <- function(input_form_df, input_json_file_name) {
      
      output_df <- input_form_df %>%
        dplyr::distinct(category_name, category_label, category_description, category_instructions,
                        policy_name, policy_label, policy_description) %>%
        dplyr::rename(NAME = policy_name, 
                      LABEL = policy_label, 
                      DESCRIPTION = policy_description) %>%
        dplyr::group_by(category_name, category_label, category_description, category_instructions) %>%
        tidyr::nest(LEVELS = c(NAME, LABEL, DESCRIPTION)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(NAME = category_name, 
                      LABEL = category_label, 
                      DESCRIPTION = category_description,
                      INSTRUCTIONS = category_instructions)
      
      jsonlite::write_json(output_df, path = input_json_file_name, force = TRUE)
      
    }
    
    .MakeCategoryJsonFromForm <- function(input_form_df, input_json_file_name) {
      
      output_df <- input_form_df %>%
        dplyr::select(top_NAME = strategy_label, 
                      DESCRIPTION = strategy_description,
                      middle_NAME = strategy_level,
                      NAME = category_name,
                      LEVEL = policy_name) %>%
        dplyr::group_by(top_NAME, DESCRIPTION, middle_NAME) %>%
        tidyr::nest(INPUTS = c(NAME, LEVEL)) %>%
        dplyr::rename(NAME = middle_NAME) %>%
        tidyr::nest(LEVELS = c(NAME, INPUTS)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(NAME = top_NAME)
      
      jsonlite::write_json(output_df, path = input_json_file_name, force = TRUE)
      
    }
    
    ve.scenario_management.make_form_csv_from_json <- function(input_dir ,
                                                               output_file_name ) {
      
      input_scenario_json <- file.path(input_dir, "scenario_config.json")
      input_category_json <- file.path(input_dir, "category_config.json")
      
      input_scenario_df <- jsonlite::read_json(input_scenario_json, simplifyVector = TRUE)
      input_category_df <- jsonlite::read_json(input_category_json, simplifyVector = TRUE)
      
      scenario_df <- input_scenario_df %>%
        dplyr::rename(category_name = NAME, 
                      category_label = LABEL, 
                      category_description = DESCRIPTION,
                      category_instructions = INSTRUCTIONS) %>%
        tidyr::unnest(., cols = c(LEVELS)) %>%
        dplyr::rename(policy_name = NAME,
                      policy_label = LABEL, 
                      policy_description = DESCRIPTION)
      
      category_df <- input_category_df %>%
        dplyr::rename(strategy_label = NAME,
                      strategy_description = DESCRIPTION) %>%
        tidyr::unnest(., cols = c(LEVELS)) %>%
        dplyr::rename(strategy_level = NAME) %>%
        tidyr::unnest(., cols = c(INPUTS)) %>%
        dplyr::rename(category_name = NAME) %>%
        dplyr::rename(policy_name = LEVEL)
      
      write_df <- dplyr::left_join(category_df, scenario_df, 
                                   by = c("category_name","policy_name")) %>%
        dplyr::select(strategy_label, strategy_description, strategy_level,
                      category_name, category_label, category_description, category_instructions,
                      policy_name, policy_label, policy_description)
      
      readr::write_csv(write_df, path = output_file_name)
      
    }
    
    ve.scenario_management.make_json_from_form_csv <- function(input_form_file_name, 
                                                               output_scenario_dir) 
    {
      
      input_form_df <- .ReadScenarioFormFromCSV(input_form_file_name)
      
      output_scenario_file_name <- file.path(output_scenario_dir, "scenario_config.json")
      output_category_file_name <- file.path(output_scenario_dir, "category_config.json")
      
      .MakeScenarioJsonFromForm(input_form_df, output_scenario_file_name)
      .MakeCategoryJsonFromForm(input_form_df, output_category_file_name)
      
    }
    
    # ve.scenario_management.make_directory_structure
    # Create a VisionEval Scenario structure from the category and level definitions
    # in the standard scenario CSV file.
    #
    # Parameters:
    #   target_root_dir: 
    #      The file path of the location where you want the folder structure to be 
    #      constructed. 
    #   input_form_file_name:
    #     Standard VisionEval scenario configuration CSV file, which can have any name and
    #     file location that you wish. 
    ve.scenario_management.make_directory_structure <- function(target_root_dir, input_form_csv){
      dir = grepl(basename(ScenarioManagerFile),list.files(target_root_dir, full.names = TRUE))
      unlink(list.files(target_root_dir, full.names = TRUE)[!dir], recursive = TRUE)
      form_df <- .ReadScenarioFormFromCSV(input_form_csv)
      
      for (row_index in 1:nrow(form_df)) {
        
        relevant_row_df <- dplyr::slice(form_df, row_index:row_index)
        level_one_dir <- file.path(target_root_dir, relevant_row_df$category_name)
        level_two_dir <- file.path(level_one_dir, relevant_row_df$policy_name)
        inputs_requierd <- relevant_row_df$inputs
        
        if (!dir.exists(level_one_dir)) dir.create(level_one_dir)
        if (!dir.exists(level_two_dir)) dir.create(level_two_dir)
        inputs_list <-  unlist(strsplit(inputs_requierd, ","))
        #check if inputs exist in parent directory
        for (i in 1:length(inputs_list)) {
          
          if( !file.exists(file.path(ScenarioBaselineInputFolder , inputs_list[i])) ) {
            stop(paste0(inputs_list[i] , " does not exist in the main input directory "))
            
          } else {
            
            file.copy(file.path(ScenarioBaselineInputFolder ,inputs_list[i]), level_two_dir)
            file.rename(file.path(level_two_dir,inputs_list[i]),
                        file.path(level_two_dir,paste0("Unedited_",inputs_list[i])))
            
          }
          
        } 
      }
    }
    
    #ve.scenario_management.make_json_from_form_csv(ScenarioManagerFile, ScenarioInputFolder)
    ve.scenario_management.make_directory_structure(ScenarioInputFolder, ScenarioManagerFile)
    ve.scenario_management.make_json_from_form_csv(ScenarioManagerFile, ScenarioInputFolder)
    cat('NewScenarioSet run is complete')
    
    
  }  


#Main module function builds scenarios
#------------------------------------------------------------------
#' Function to build scenarios.
#'
#' \code{NewScenarioSet} builds structure to run mulitple scenarios.
#'
#' This function reads the scenario management database and creates
#' the related configuration json files for defining scenario sets.
#' It also creates all the scenario folders with their required inputs 
#' in the root directory 
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @name NewScenarioSet
#' @import jsonlite 
#' @export
NewScenarioSet <- function(L){
  # Setup
  # -------------

  RunDir = getwd()
  ScenarioInputFolder <- file.path(RunDir, L$Global$Model$ScenarioInputFolder)
  ScenarioManagerFile <- file.path(RunDir, L$Global$Model$ModelFolder,L$Global$Model$ScenarioManagerFile)
  ModelFolder <- file.path(RunDir, L$Global$Model$ModelFolder)
  ScenarioBaselineInputFolder <- file.path(ModelFolder,"inputs")
  
  NewScenarioSetInterior(ScenarioInputFolder , ScenarioManagerFile, ModelFolder, ScenarioBaselineInputFolder)   
  Out_ls <- initDataList()
  Out_ls$Global$Model <- list(Scenarioset = 1L)
  return(Out_ls)  
 
}
# change to VeScenario
