# Overview 

The VisionEval software framework is written in the R programming language for statistical computing
and graphics. The purpose of the model system and framework is to enable models be created in a
plug-and-play fashion from modules that are distributed as R packages. A simple R script is
used to implement a model by initializing the model environment and then calling modules
successively.

Model users need to develop input files to represent their base conditions and any future
scenarios they would like to evaluate. Once the models have run for each scenario, the results
can be inspected and extracted for further analysis either in R or in some other data analysis
environment.

## Installation and Setup

There are two paths to getting VisionEval running:

1. **Install from stand-alone Windows installer**\
   Download a zipped folder with all dependencies include, for a specific version of R. This is the
   simplest way to quickly get VisionEval on your computer. This uses the installers on the
   [Releases](https://github.com/VisionEval/VisionEval/releases) page and further details are available
   on the [Downloads page of visioneval.org](https://visioneval.org/category/download.html). Installers
   are currently only available for the Windows operating system. That path is described in this 
   document.

2. **Clone or fork repository**\
   If you area a Mac/Linux user, or if you are interested in contributing to the development of
   VisionEval modules, models, framework, or visualizer, choose this path. This release is hosted
   at [VisionEval on Github](https://VisionEval/VisionEval).  Development releases are available
   at [VisionEval-dev](https://VisionEval/VisionEval). Once you have downloaded or cloned one
   of the VisionEval repositories, complete instructions for building a runtime are found in
   `build/Building.md` (or you can locate that file in the repository).

# Install for Windows

### Pre-requisites

You will need:
- [R, at least version 3.6.x][getR]
- [RStudio][getRStudio]

[getR]:        https://cran.r-project.org
[getRstudio]:  https://www.rstudio.com/products/rstudio/download/ "Download RStudio"

Once you have R and RStudio installed, you can retrieve the VisionEval installer itself:

#### [Get VisionEval Here](https://github.com/VisionEval/VisionEval/releases/download/v2.0.0/VE-installer-Windows-R4.0.2-latest.zip)

*Note: Almost 650 Mb download! Packaged for R 4.0.2*

The link above will download a .zip file containing the following:
 - The VisionEval framework code
 - Sample models including VE-RSPM, VE-RPAT, VE-State and VE-ScenarioViewer 
 - All necessary R packages
 - Documentation

[See this page][GetVersions] for other R versions and eventually other operating systems, 3.6.1 - 4.0.2

[GetVersions]:  https://github.com/VisionEval/VisionEval/releases/tag/v2.0.0

### Completing the Installation

After installing R 4.0.2 (or the version of R corresponding to the installer you are retrieving) and
Rstudio, unzip the VisionEval installer into an empty destination folder of your choice.

To complete the installation and start VisionEval, simply:
   - Double-click **<tt>VisionEval.Rproj</tt>**

RStudio will start, and the VisionEval will load.

If an unusual RStudio configuration prevents that from working, just start RStudio (or even plain
R), change into your installation folder, and run this R command:

```
source("VisionEval.R")
```

## Getting Started

This section should explain:

1. Where to find the tutorials for each model (docs/models/ModelName/tutorial).
1. How to run the sample models.
1. How to duplicate a sample model so you can make local changes.
1. How to configure alternate scenarios based on your local model copy
1. How to run alternate scenarios
1. How to summarize performance measures for a single model run
1. How to summarize performance measures for multiple scenarios
1. How to export the data from a model run so you can examine it with other tools
1. How to export data from multiple scenarios

### Debugging and Customizing VisionEval

If you are comfortable with using R, you can debug and customize VisionEval either by retrieving
the source repository (best if you are creating extensions to VisionEval) or by using the
Package Sources to debug or re-estimate the VisionEval modules with local data.

A separate zip file (also very large) containing Package Sources is available for each R
version at the same location as the Installers. You can use the Package Sources to debug or
customize VisionEval for local data using RStudio without having to retrieve the Git repository or
do a full build of the system. See the file `docs\visioneval\RStudio.pdf` for information on using
the Package Sources.

## Requirements

If the above installation steps did not succeed, ensure that you have
downloaded the appropriate version of VisionEval to match the version
of R that you have installed.

#### R The current version of VisionEval is built for the latest version of R, 4.0.2.

If you currently have another version of R installed, you can go to the [GitHub release
page](https://github.com/VisionEval/VisionEval/releases) to download VisionEval for R. 

You can find the [R 4.0.2 installer for Windows here](https://cran.r-project.org/bin/windows/base/").
