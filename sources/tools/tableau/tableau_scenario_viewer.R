requireNamespace("magrittr")
requireNamespace("dplyr")
requireNamespace("tidyr")
requireNamespace("readr")
requireNamespace("jsonlite")

assign("%>%",getFromNamespace("%>%","magrittr"))

tool.contents <- c(
  "ve.tableau_scenario_viewer.MakeCSV"
)

# private methods --------------------------------------------------------------
.MakeDataframeFromJavaScript <- function(input_js_var_file_name, var_name_string) {
  
  temp_out_name <- "temp.JSON"
  input_line  <- readLines(input_js_var_file_name)
  step_1 <- stringr::str_replace(input_line, paste0("var ", var_name_string, " ="), "")
  step_2 <- stringr::str_replace(step_1, ";", "")
  writeLines(step_2, con = temp_out_name)
  
  return_df <- jsonlite::fromJSON(temp_out_name, simplifyVector = TRUE, flatten = TRUE)
  
  file.remove(temp_out_name)
  
  return(return_df)
  
}

.MakeScenarioDataframes <- function(input_scenario_dir, model_name = "verspm") {
  
  if (!(model_name %in% c("verspm", "verpat"))) {
    stop("model_name must be 'verspm' or 'verpat'")
    return("Error")
  }
  
  file_name <-  paste0(input_scenario_dir, model_name, ".js")
  results_df <- .MakeDataframeFromJavaScript(file_name, "data") 
  
  file_name <-  paste0(input_scenario_dir, "output-cfg.js")
  output_config_df <- .MakeDataframeFromJavaScript(file_name, "outputcfg") 
  
  file_name <-  paste0(input_scenario_dir, "scenario-cfg.js")
  scenario_config_df <- .MakeDataframeFromJavaScript(file_name, "scenconfig") %>%
    dplyr::rename(category_name = NAME, 
                  category_label = LABEL, 
                  category_description = DESCRIPTION,
                  category_instructions = INSTRUCTIONS) %>%
    tidyr::unnest(.,cols = c(category_name, category_label, category_description, 
                             category_instructions, LEVELS)) %>%
    dplyr::bind_cols(., tibble::tibble(level_name = unlist(.$NAME))) %>%
    dplyr::bind_cols(., tibble::tibble(level_label = unlist(.$LABEL))) %>%
    dplyr::bind_cols(., tibble::tibble(level_description = unlist(.$DESCRIPTION))) %>%
    dplyr::select(-NAME, -LABEL, -DESCRIPTION) 
  
  file_name <-  paste0(input_scenario_dir, "category-cfg.js")
  category_config_df <- .MakeDataframeFromJavaScript(file_name, "catconfig") %>%
    dplyr::rename(strategy_label = NAME,
                  strategy_description = DESCRIPTION) %>%
    tidyr::unnest(., cols = c(strategy_label, strategy_description, LEVELS)) %>%
    dplyr::rename(strategy_bundle = NAME) %>%
    tidyr::unnest(., cols = c(strategy_bundle, INPUTS)) %>%
    dplyr::bind_cols(., tibble::tibble(category_name = unlist(.$NAME))) %>%
    dplyr::bind_cols(., tibble::tibble(level_name = unlist(.$LEVEL))) %>%
    dplyr::select(-LEVEL, -NAME)
  
  return_list <- list("results" = results_df,
                      "output_config" = output_config_df,
                      "scenario_config" = scenario_config_df,
                      "category_config" = category_config_df)
  
  return(return_list)
}

.notAllNA <- function(x) {
  
  return(any(!is.na(x)))
  
} 


# public methods ---------------------------------------------------------------
# ve.tableau_scenario_viewer.MakeCSV
# Create a Tableau-ready CSV database from the HTML Scenario Viewer inputs
#
# Parameters:
#   input_scenario_dir:
#     File directory of the Standard VisionEval scenario viewer output files, which
#     are assumed to be named 'output-cgf.js', 'scenario-cfg.js', 'category-cfg.js',
#     and either 'verspm.js' or 'verpat.js'.
#   model_name:
#     Either 'verspm' or 'verpat'. 
ve.tableau_scenario_viewer.MakeCSV <- function(input_scenario_dir, model_name = "verspm") 
{
  
  df_list <- .MakeScenarioDataframes(input_scenario_dir, model_name)
  
  measure_names_vector <- df_list$output_config$COLUMN
  
  outcomes_df <- df_list$results %>%
    dplyr::mutate_at(dplyr::vars(all_of(measure_names_vector)), as.numeric)
  
  category_name_dict_df <- df_list$scenario_config %>%
    dplyr::distinct(category_name, category_label) %>%
    dplyr::mutate(category_label = stringr::str_replace_all(category_label, " ", "_")) %>%
    dplyr::select(short = category_name, long = category_label)
  
  level_name_dict_df <- df_list$scenario_config %>%
    dplyr::distinct(category_name, level_name, level_label)
  
  working_df <- outcomes_df
  
  for (category in unique(level_name_dict_df$category_name)) {
    
    level_dict_df <- level_name_dict_df %>%
      dplyr::filter(category_name == category) %>%
      dplyr::distinct(level_name, level_label) %>%
      dplyr::mutate(level_name = paste0(level_name))
    
    new_category_name <- paste0(category, "_NAME")
    
    working_df <- working_df %>%
      dplyr::rename(subject := !!category) %>%
      dplyr::mutate(subject = paste0(subject)) %>%
      dplyr::left_join(., level_dict_df, by = c("subject" = "level_name")) %>%
      dplyr::mutate(subject_copy = level_label) %>%
      dplyr::rename(!!new_category_name := subject_copy) %>%
      dplyr::rename(!!category := subject) %>%
      dplyr::select(-level_label)
    
  }
  
  for (category in category_name_dict_df$short) {
    
    long_name <- dplyr::filter(category_name_dict_df, short == category)$long
    
    new_category_name <- paste0(category, "_NAME")
    
    working_df <- working_df %>%
      dplyr::rename(!!long_name := !!new_category_name) 
  }
  
  strategy_name_df <- df_list$category_config %>%
    tidyr::pivot_wider(., names_from = category_name, values_from = level_name)
  
  for (strategy in unique(strategy_name_df$strategy_label)) {
    
    join_df <- dplyr::filter(strategy_name_df, strategy_label == strategy) %>%
      dplyr::select(-strategy_description) %>%
      dplyr::select_if(.notAllNA)
    
    join_vector <- colnames(dplyr::select(join_df, -strategy_label, -strategy_bundle))
    
    strategy <- stringr::str_replace_all(strategy, " ", "_")
    strategy <- stringr::str_replace_all(strategy, "\\/", "_")
    strategy <- paste0("Strategy_", strategy)
    
    working_df <- dplyr::left_join(working_df, join_df, by = join_vector) %>%
      dplyr::rename(!!strategy := strategy_bundle) %>%
      dplyr::select(-strategy_label)
    
  }
  
  return_df <- working_df %>%
    dplyr::mutate(scenario_index = dplyr::row_number())
  
  return(return_df)
  
}

