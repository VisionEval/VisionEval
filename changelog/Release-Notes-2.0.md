# VisionEval Version 2.0

Released: 2020-07-31

## Supported R versions

Version 2.0 can be built with R versions 3.6.0 through 4.0.2

Binary installers are available for R versions 3.6.1, 3.6.3, and 4.0.2 on Windows.

## Changes and enhancements:

Relative to the previous release of VisionEval (Version 1.1, 2019-09-18), the following significant changes have been made:

- A **multi-modal** alternative implementation of the metropolitan model VE-RSPM is now available. This is an alternative to the `VETravelDemand` which allows users to assess impacts on multimodal trips. For more information on the `VETravelDemandMM` module, implemented through the new `VERSPM_MM` model, see this reference ([vignette](https://github.com/VisionEval/VisionEval-Dev/blob/mm_debug/sources/modules/VETravelDemandMM/vignettes.hide/Intro.Rmd)) and [this publication from Prof. Liming Wang](https://www.dropbox.com/s/y594fz44achoqkq/jtlu_rspm.pdf?dl=0) for more details.

- **Safety Module** has been added to `VETravelPerformance`, which adds functionality for users to calculate the safety impacts of the scenarios being evaluated. See [ !! Update this link after PR closed !! ] [here](https://github.com/arashasadabadi/VisionEval-Dev/blob/patch-12/sources/modules/VETravelPerformance/inst/module_docs/CalculateSafetyMeasures.md) for more details. This has been implemented for both VE-RSPM (TODO) and `VERSPM_MM`.


- **VE-Reports**

- **VE-State**



## Instructions for Installing

- Install one of the supported R versions using its default options (R **3.6.1 to 4.0.1**). Make sure to leave the box checked that offers to add R to the Windows registry.
- Download the corresponding installer .zip file
- Unzip the downloaded installer into an empty folder of your choice
- Double-click “VisionEval.Rproj” (this is the standard entry point – use it every time to start VisionEval)

a. If VisionEval has not run before, the batch file and associated script will set up a few pointers and the convenience functions (see below)
b. Otherwise, it will move straight to step 5.a below

A command window will briefly appear followed by one of these:
a. RGUI with the “Welcome to VisionEval!” message (you’re good to go)
b. Your default browser pointing at the R download page (if you forgot to do step 1 or if you picked the wrong version) – just redo step 1 or steps 2/3 to get them consistent

The previous instructions for launching VEGUI, VERPAT, and VERSPM. remain the same.

a. vegui() starts VEGUI
b. verpat() runs VERPAT 
c. verspm() runs VERSPM 
d. verspm_mm() runs VERSPM_MM (new multi-modal version of VERSPM)
d. Note that there’s no helper function to run VESTATE from the command line yet – that will be added to the installation in a future release. You can just setwd() to the VE-State folder under “models” and then source(‘Run_Model.R’).

## Changelog

| Version | Changes         |
| --------|-----------------|
| 0.1     | Initial release |
| 0.1.1   |  Numerous updates to VELandUse and other modules              |
| 1.0     |  Added `ve.export` <br> Fixed bugs in unit conversions <br> Fixed issue with Azone file order dependence <br> |
| 1.1     |   Major updates to framework code, adding VERSPM_MM, safety module, updates to VEReports |
|         |                 |
