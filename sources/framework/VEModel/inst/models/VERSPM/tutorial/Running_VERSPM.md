# VERSPM Software Execution
----

This section describes the installation and use of VERSPM.

## Installation of VisionEval and VERSPM

VERSPM and VisionEval framework are implemented in R, a statistical programming language and environment.  Both R and VisionEval are open source and freely available. For running VERSPM you need to follow these steps:

1. [Install R](https://cran.r-project.org/) (users are encouraged to also install [RStudio](https://www.rstudio.com/products/rstudio/download/), a free and open-source integrated development environment for R)
2. [Install VisionEval](https://download.visioneval.org/)
3. Run VERSPM

The VisionEval installer is available [here](https://download.visioneval.org/). Follow the instructions on this page carefully to install the Visioneval on your system.

## Running VERSPM Base Scenario

### Preparing inputs
Once VisionEval and VERSPM have been installed, a directory with sample data will be available at `../models/VERSPM/Test1`. (Note `..` refers to the parent directory of the unzipped installer file). 

The `VERSPM` directory serves the dual purposes of providing sample data and serving as a template for local modification to other locations. 

The `../models/VERSPM/Test1` directory contains sample input files for the Rogue Valley region in Oregon. These inputs can be modified or replaced to investigate the impacts of policy changes or to model a different region.  The folder contains multiple files and subfolders:

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/VERSPM_Folder.PNG">

`run_model.R` is the core script for running the model. It consists of calls to the modules that make up the model. The user may modify the script to call the  desired modules.

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/run_model.PNG">

The `defs` directory contains five model definition files which is introduced in [Model Definition Files](Inputs_and_Parameters.md)

<img align="center" width="700" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/model_definitions.PNG">

The `inputs` directory contains a number of `CSV` and `JSON` files that provide inputs for the modules. Each module specifies what input files it needs. If you would like to know 
the description of each input file and how you can change those files for your desired testing see [Inputs_and_Parameters](Inputs_and_Parameters.md/#input-files)
<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/inputs.PNG">

There is complete set of test data for RVMPO which you will find out in the Test1 folder. You can use those data to test VERRSPM run.

 the `../models/VERSPM/Test1` directory contains sample input files for RV MPO, Oregon.  These can be modified or replaced to investigate the impacts of policy changes or to model a different region.  

To modify a scenario, the appropriate input files are edited.  For example, to change the flat rate tax of vehicles for future [`azone_hh_veh_own_taxes.csv`](link) would be modified in Excel, LibreOffice, or a text editor to change the `VehOwnFlatRateFee` of year 2038

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/modify_input.PNG">

### Running the Model

There are multiple ways to run VisionEval models. VisionEval models can be run via the command line or via the GUI, and can be run for one scenario or multiple scenarios in parallel. Results can be viewed in tabular form or with the interactive VEScenarioViewer.

#### Running VERSPM from the R command line

1. Start R (or Rstudio) and make sure your directory is set to the installer folder
2. Run the following commands:

```
source("Install-VisionEval.R")
load("RunVisionEval.Rdata")
verspm()
```

By defult this will run the model in `../models/VERSPM/Test1` directory. It will be dicussed how to run scnerios in another folder later in this tutorial.

After running the scipt you will see how the modules will be running in order.

#### Running VERSPM from GUI

1. Start R (or Rstudio) and make sure your directory is set to the installer folder
2. Run the following commands:

```
source("Install-VisionEval.R")
load("RunVisionEval.Rdata")
vegui()
```

By defult this will run the model in `../models/VERSPM/Test1` directory. It will be dicussed how to run scnerios in another folder later in this tutorial.

After running the script a new window will pop up: 

<img align="center" width="500" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/GUI.PNG">

click the `Select Scenario run script` and go to `../models/VERSPM/Test1/run_model.R` . After clicking on the script you will see a list of all running modules in `Scenario tab`

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/GUI2.PNG">

Go to `Inputs` tab to view all the inputs.

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/GUI3.PNG">

Finally go to `Run` and click `Run Model` button to start running the model

<img align="center" width="500" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/GUI4.PNG">

Once run is finished output are exported to `../models/VERSPM/Test1/outputs` in 3 different zone levels

## Running VERSPM Multiple Scenarios

Strategic planning often requires the assessment of large numbers of future scenarios, each assessing a different combination of prioritizations, policy decisions, and constraints. 
The VEScenarios module provides the capability to quickly set up, run, and visualize large numbers of VERSPM scenarios using a baseline scenario combined with multiple changes to model inputs. 
After going to `../models/VERSPM_Scenarios` folder, you will see multiple subfolders and scripts.

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/multiple_scenarios.PNG">


#### Test1

 This directory contains the inputs and R script necessary to run the base scenario, as described above.
 
#### defs

`VERSPM_Scenarios/defs` directory contains the same files as the `Test1/defs` directory, but the `model_parameters.json` file differs between `Test1/defs` and`VERSPM_Scenarios/defs` diffiers in that the latter 
version contains just four parameters specifying the locations of inputs and outputs, as well as the number of processors (NWorkers) to use. The default is 4, but be sure to set this to a number appropriate to your machine.

```
[
  {
    "NAME": "ModelFolder",
    "VALUE": "Test1",
    "TYPE": "character",
    "UNITS": "NA",
    "PROHIBIT": "NA",
    "SIZE": 20,
    "ISELEMENTOF": ""
  },
  {
    "NAME": "ScenarioInputFolder",
    "VALUE": "scenario_inputs",
    "TYPE": "character",
    "UNITS": "NA",
    "PROHIBIT": "NA",
    "SIZE": 20,
    "ISELEMENTOF": ""
  },
  {
    "NAME" : "ScenarioOutputFolder",
    "VALUE": "scenarios",
   "TYPE": "character",
   "UNITS": "NA",
   "PROHIBIT": "NA",
   "SIZE": 20,
   "ISELEMENTOF": ""
  },
  {
    "NAME" : "NWorkers",
    "VALUE": "6",
    "TYPE" : "integer",
    "UNITS" : "NA",
    "PROHIBIT" : "c('NA', '< 0')",
    "ISELEMENTOF" : ""
  }
]
```

#### Inputs

The `inputs` folder in `VERRSPM_Scenarios` also differs from that in `Test1`.  In this case, there is only a single file specifying the output data tables that should be exported as CSV files in the `outputs` directory.
<img align="center" width="300" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/VERSPM_scenarios.PNG">

#### scenario_inputs

Model scenarios are defined in terms of combinations of individual model input parameters and policy choices.  The various inputs are defined in the `scenario_inputs` folder.

Scenario inputs consist of eleven folders, one for each of a particular category of input, as described below.  All eleven folders are required.

Within each folder, there are subfolders containing input files, one per specific input. Each of the folders must contain at least one subfolder named "1", defining the input for the base scenario.  Subsequent numbered folders contain input files modifying parameters of interest, as shown in the screenshots below:

Model inputs not otherwise specified in the `scenario_inputs` directory are drawn from files in `Test1/defs` and `Test1/inputs`
<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/VERSPM_scenarios2.PNG">

The subfolder names and scenario inputs are defined as follows (input file to modify given in parentheses):

  - B - Bicycles (`azone_prop_sov_dvmt_diverted.csv`) : Network improvements, incentives, and technologies that encourage bicycling and other light-weight vehicle travel
    - 1 - Base bicyling percentage of SOV tours less than 20 miles (9.75%)
    - 2 - Increase diversion of SOV tours to 20%
  - C - Vehicle Travel Cost (`azone_hh_veh_own_taxes.csv`) and (`region_prop_externalities_paid.csv`) : Combination of fuel prices and charges to pay for roadway costs and possibly externalities
    - 1 - No change in fuel prices or increase in roadway or externality charges
    - 2 - Keep the vehicle ownerhsip cost the same
    - 3 - Higher climate cost and pay as you drive insurance
  - D - DemandManagement (`bzone_travel_demand_mgt.csv`) : Programs to encourage less private vehicle travel
    - 1 - Baseline implementation of ITS
    - 2 - Increase the effectiveness of implementation of ITS
  - E - Driving Efficiency (`marea_operations_deployment.csv`) and (`marea_speed_smooth_ecodrive.csv`) and (`other_ops_effectiveness.csv`) : Driving efficiency by increasing implementation of ITS
    - 1 - Base
    - 2 - Increased the proportion by 10%	
  - F - Technology Mix and CI (`marea_transit_powertrain_prop.csv`) and (`region_carsvc_powertrain_prop.csv`) and (region_comsvc_powertrain_prop.csv`) : Vehicle technology mix and carbon intensity of fuels.
    - 1 - Baseline vehicle technology mix
    - 2 - Increased percentage of electric vehicles in household and commercial setting by 20%	
  - G - Fuel Price (`azone_fuel_power_cost.csv`) : Real fuel price in 2010 USD
    - 1 - Baseline fuel price
    - 2 - Double fuel price
	- 3 - Quadruple fuel price
  - I - Income (`azone_fuel_power_cost.csv`) : Real average household income in 2010 USD
    - 1 - Baseline household income
    - 2 - Income growth of 7% w.r.t reference
	- 3 - Income growth of 14% w.r.t reference
  - L - LandUse (`azone_hhsize_targets.csv`) and (`bzone_urban_du_proportions.csv`) : Distribution of population and employment by place type
    - 1 - Base, Maintain current distribution
    - 2 - LU overlaps with HHsize + Population
  - P - Parking (`bzone_parking.csv`) : The extent of paid parking and its price
    - 1 - Current extent and daily fee
    - 2 - Increase parking cost by 100% and proportion charted by 10%.
  - T - Transit (`marea_transit_service.csv`) : The extent and frequency of transit service
    - 1 - Current public transit service level
    - 2 - Double public transit service level
    - 3 - Quadruple public transit service level
  - V - Vehicle Characteristics (`azone_hh_veh_mean_age.csv`) and (`azone_lttrk_prop`): The combination of fuel prices and vehicle travel charges to pay for roadways and to pay for externalities such as carbon pricing
    - 1 - Base
    - 2 - Light truck proportion at 35% of the fleet and the average vehicle age at 8 years

Running all of these input values will result in 10368 total scenarios, which would take days to run.  User usallly does not need all the possible combinations of scnerios. 
VERSPM Scenarios are grouped in 5 different categories which you can modify in `category_config.json` :

* **Community Design** :  Policies that seek to enable shorter trips and alternate modes such as promotion of mixed use land use, transit service, bicycling, and parking management. ( group of L, B, P , T ) 
* **Marketing/Incentive** : Policies that improve driving efficiency such as ecodriving,and Intelligent Transportation System efforts, as well as programs that reduce auto demand such as carsharing, and home or work-based transportation demand management.( group of D, E ) 
* **Pricing : Policies** that move towards true cost pricing such as road user fees to pay for the cost of operating, maintaining and improving roads, pay-as-you-drive (PAYD) insurance, and environmental impact fees such as a carbon tax. ( C )
* **Vehicles/Fuels** : Factors representing changes to future vehicles and fuels ( group of V, F ) 
* **Income** : Context factor on the assumed growth of statewide average per capita income ( I )

Depending on how many scenarios exist in each of these five groups, total number of scenariso will be determined.

To test the multi-scenario capability in less time, reduce the number of scenario inputs by deleting some of the numbered folders, making sure to retain the "1" folder for each of the six options.
Note that if you change the directory structure in scenario_inputs, you will have to change `category_config.json` and `scenario_config.json` to match the new directory structure.

To modify the scenario inputs, the numbered subdirectories can be created or deleted. Each directory contains a single input file containing the modified model parameter.

#### run_model.R
The `run_model.R` script in this case runs four modules that create the scenarios from the inputs, runs each and combines the results.
<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/VERSPM_scenarios3.PNG">

Similar to base case the model can be run using R or GUI. To run the model using R, run the following commands:

```
source("Install-VisionEval.R")
load("RunVisionEval.Rdata")
verspm(scenarios=TRUE)
```

After starting the run,model automatically builds scenarios by creating all possible combinations of settings found in scenario_inputs. The scenarios to run are found in the newly created scenarios directory.

<img align="center" width="800" border=1 src="https://github.com/wsp-sag/client_fhwa_vision_eval/blob/rspm_tutorial_AA/documentation/tutorials/verspm/pics/scenarios.PNG">

Each directory is essentially a copy of Test1, with inputs modified as specified in the scenario_inputs directory. Each scenario directory contains the results of a model run with its own inputs, datastore, and log file.

When finished, the VERSPM Scenario Viewer will automatically open to display the results. To find out more on exported metrics see [Performance Metrics](Performance.md)

Return to [Tutorial](Main.md). 