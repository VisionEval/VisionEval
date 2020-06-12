
# VETravelDemandMM
This is the Multi-Modal Travel Demand which simulates multi-modal travel demand for individual households including:

- Annual Average Daily VMT
- Transit trips and PMT
- Biking trips and PMT
- Walking trips and PMT

Previously in VERSPM, VEHouseholdTravel package [] predicts VMT and non-driving trips for households. In VERSPM_MM add-on, new VETravelDemandMM package
now estimates VMT and non-driving trips alongside PMT and non-driving trip length.

The motivations of developing the new package includes better policy sensitivities for non-driving modes and taking advantage of newer and better data sources available since the implementation of the RSPM/GreenSTEP model. More specifically, the objectives of the new module include the following:

### Better Representation of Multi-Modal Travel
Since the primary focus of GreenSTEP is green-house gas emission, its travel demand module has minimum representation of non-driving modes. As more non-driving travel and its associated benefits attracts more attention from the public and policy-makers, there is need to understand the key drivers of multi-modal transportation choice and how non-driving travel responds to policies and investment decisions and to develop models that better represent the multi-modal travel for strategic planning. This module is developed in response to this demand.

### Updating Models with the Latest and Best Data Available 
The current implementation of travel demand module uses for model estimation the latest 2009 NHTS data joined with EPA's Smart Location Database (SLD) for built environment information, the National Transit Database (NTD) for region-level transit supply, and HPMS for region-level road network. Access to the confidential block group of household's residential location allow these nationwide datasets to be joined at a very high resolution. In addition to refresh the model estimation with the latest nationwide datasets, this new data provides a rich set of high-resolution built environment variables (the SLD includes more than a hundred block group-level built environment measures covering most of US).

Since NHTS2009 have Annual VMT data for most households surveyed (more than half of them missing in NHTS2001), we took advantage of the data and model the AADVMT for household, instead of the VMT in the survey day as what the GreenSTEP used.

### Rigorous Benchmark and Selection of Different Model Structures
There are various model structures used in the research literature to model non-driving travel. We reviewed the various model structures and used theoretical vigorousness and cross-validation to benchmark and select model structures. More details of the cross-validation and model selection can be found in [a manuscript currently under review](https://www.dropbox.com/s/y594fz44achoqkq/jtlu_rspm.pdf?dl=0). 

### Taking advantage of the R infrastructure and new packages
The current implementation of the module takes advantage of [the `tidyverse` suite of R packages](http://www.tidyverse.org/), in particular, `dplyr`, for efficiency, concision and code readability. It also uses the `purrr` package for functional programming where feasible. Comparing with  RSPM/GreenSTEP, the package uses model objects and method dispatch for `predict` calls, which eliminates the need to implement different model structures in the package.

## Methods and Model Structure

A paper on the VETravelDemandMM models structure can be found [here] (https://www.dropbox.com/s/y594fz44achoqkq/jtlu_rspm.pdf?dl=0). Here is a summary of existing and selected model structures:
A broader detail-oriented on all the models alongside performance and sensitivity test can be found [here] (https://www.oregon.gov/ODOT/Programs/ResearchDocuments/SPR788RSPMTool.pdf)

Here is a summary of existing and selected model structures:

- GreenSTEP Daily VMT (DVMT) Models (2-step models)
    1. binomial logit ZeroDVMT
    2. power-transformed linear regression of DVMT (for DVMT > 0)
- AADVMT Model for Annual Average Daily VMT (AADVMT)
    - power-transformed linear regression of AADVMT
- TFL models for non-driving modes (2-step models)
    1. hurdle model of trip frequncies by modes (transit, walk, and bike)
    2. power-transformed linear regression of average trip length
- Daily person mile traveled (PMT) by (non-driving) modes models
    - hurdle models of DPMT by modes (transit, walk, and bike)

Technical details of the model structures can be found in the estimation script for corresponding model in `data-raw`. The actual functions doing the prediction for the module in `R` is model structure agnostic - it is determined by the model objects saved in the model data frame in the `data` directory.

### Variables Used in Models

Plese see the abovementioned [report] (https://www.oregon.gov/ODOT/Programs/ResearchDocuments/SPR788RSPMTool.pdf) for a list of all variables and 
their defintions.

Also, there is a full list of all the VisionEval variables used as the input for VETravelDemandMM models in the models document.

See the document for [CalculateHouseholdDvmt] (inst/module_docs/CalculateHouseholdDVMT.md) and [CalculateAltModeTrips] (inst/module_docs/CalculateAltModeTrips.md)

[A Cheat Sheet](https://github.com/gregorbj/VisionEval/wiki/documents/RSPM-TFLmodelVariables_May2017.pdf) summarizes the estimated functions, independent and dependent variables in each model.

## Data

This module provides default model parameters estimated with US nationwide data, and it is also possible to re-estimate model parameters with region-specific data. 
The main estimation data are drawn from two external data package ([NHTS2009](https://github.com/cities-lab/NHTS2009) and [SLD](https://github.com/cities-lab/SLD), documented therein;  `data-raw/LoadDataforEstimation.R` provides code and comments needed to replace the estimation data with region specific data. However, since the residential block group information for households in the 2009 NHTS (essentially providing an additional block group id column to the households data frame and allowing NHTS to be joined with SLD) used in the estimation of the nationwide models is confidential and cannot be shared, users will not be able to directly run the estimation scripts in `data-raw`.

## Input

See [model documents] (inst/module_docs) for user input requiremnet. Note that there is no additional land use data needed comparing to VERSPM model inputs as all the 
data used by VETravelDemandMM models had already been processed by VERSPM upstream modules.

## VERSPM VS. VERSPM_MM

VERSPM_MM is an add-on to VERSPM model. Running and installation of the model is the same procedure. The main difference of these models are in run_model.R script . VERSPM uses VEHouseholdTravel: CalculateHouseholdDvmt and VEHouseholdTravel: CalculateAltModeTrips
whereas VERSPM_MM runs with VETravelDemandMM: CalculateHouseholdDvmt and VETravelDemandMM : CalculateAltModeTrips.

The Integration of VETravelDemandMM package to visioneval framework has been done in a way to minimize the structural
changes to other visioneval packages. All the VETravelDemandMM modules specifications are consistent with the previous version
so all the downstream and upstream modules remain the same. 

# Build Status

[![Travis-CI Build Status](https://travis-ci.org/cities-lab/VETravelDemandMM.svg)](https://travis-ci.org/cities-lab/VETravelDemandMM)
