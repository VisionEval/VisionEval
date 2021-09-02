# VE-Tools

This tool are designed to be used with the downloadable version of VisionEval. To use, download the VE-Tools folder and place into the /tools folder of your local copy of VisionEval.

## Folders:
1. <b>Data Prep</b>: This folder contains files and scripts that help with the data preparation. 
2. <b>Model Run</b>: This folder will support running the VERSPM module. 

# Data Prep

### [VERSPM File Summary Tracker.xlsx](https://github.com/eric-englin-volpe/VE-Tools/raw/main/Data%20Prep/VERSPM%20File%20Summary%20Tracker.xlsx)

This Excel file has information on all 68 input files which can be used by VE-RSPM (not all files are mandatory). It also contains information on the run order and specific module that each input is used by. There is an additional column, "Included in VE Tools Data Prep?" that will have the specific Data Prep file that will create and save each particular input. 

Typically, this Excel file is used while preparing the input files and shared across a team that is working together to compile all files. The file contains a status folder that can be used to track this progress. 

### Config.R

Users should start by changing the initial settings in this text file. Other data prep scripts will reference the variables and file locations in this text file. These main variables include: directory paths, base and future years, names and geometries for metro area, bzones, and azones, and Census county codes. 

**Note:** At this time, the prep files are written for bzones using TAZs. If a user has their own geometric shapefile for the bzones then this may be substituted in with no issues. Future versions of this tool will allow for using Census block groups as a default bzone option as well. 

### Other Data Prep Files

The fully developed data prep files are as follows: 
- Make_bzone_dwelling_units.R
- Make_bzone_hh_inc_qrtl_prop.R
- Make_bzone_lat_lon.R
- Make_marea_lane_miles.R
- Make_placetypes_USA.R

# Model Run

VisionEval can be used to run distinctive user defined combinations of scenarios as opposed to the full factorial combination.
The following scripts when run with Vision Eval can be used with the `VERSPM_Scenarios` model provided in the as an example by default.

### Create_Single_Scenarios.R

###### <b> Description: </b>
* Creates unique model run folders based on specified base model and swaps out distinct files for each run folder. All other files will remain the same as the base model. These run folders will be created in the `VERSPM_Scenarios` directory.

###### <b> User Required Inputs: </b>
* The user can alter the default `base_model_path` and `scenario_inputs_path` to specify where the base model to be used and 
files inputs for the scenarios can be found.

###### <b> Outputs: </b>
* Generates 15 model run folders B2-V2
* Generates `Single_Scenarios_Status.csv` in `VERSPM_Scenarios` directory which tracks model names, model path, modified input files, and model run status.


### Run_Single_Scenarios.R

###### <b> Description: </b>
* This script runs all the model folders including the base case created by the `Create_Single_Scenarios.R` script.

###### <b> User Required Inputs: </b>
* Requires `Create_Single_Scenarios.R` script to be run for model folders and CSV information.

###### <b> Outputs: </b>
* Generates complete model runs for each scenario including the base case
* Updates `Single_Scenarios_Status.csv` in `VERSPM_Scenarios` directory with model status

### Extract_Single_Scenarios.R

###### <b> Description: </b>
* This script extracts key metrics from each of the run models and compiles the data into a household and marea file.

###### <b> User Required Inputs: </b>
* User can change/specify future year which is being explored in scenario.
* Expects models to be run with prior run script above

###### <b> Outputs: </b>
* Generates `Extracted_Metric_Units.csv` in `VERSPM_Scenarios` detailing units for the model run
* Generates `Single_Scenarios_Metrics_Marea.csv` in `VERSPM_Scenarios` with consolidated Marea stats
* Generates `Single_Scenarios_Metrics_Hh.csv` in `VERSPM_Scenarios` with consolidated household stats
* Generates `Single_Scenarios_Complete.RData` in `VERSPM_Scenarios`

### Single_Scenarios_Results.Rmd

###### <b>Description: </b>
* This RMarkdown file generates a summarized interactive report of the key statistics comparing the multiple model runs.

###### <b> User Required Inputs:</b>
* User must specify where VisionEval is located
* User must place the Single_Scenarios_Overview_Template.xlsx from this repository into the VERSPM_Scenarios directory


###### <b> Outputs:</b>
* Generates HTML summarized report for viewing the results
* Generates figures of key findings found in Single_Scenarios_Figures directory in VERSPM_Scenarios




