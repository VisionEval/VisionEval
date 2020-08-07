---
title: Getting Started with VisionEval
---

<a name='overview'>Overview</a>
-------------------------------

The VisionEval strategic modeling system allows rapid evaluation of land use and transportation
scenarios to support scenario planning, and to develop strategies to manage transportation system
performance.

Configuring and running VisionEval scenarios is simpler and faster than using traditional travel
demand models, and more flexible and detailed than using traditional spreadsheet sketch models.
While VisionEval outputs are limited in spatial resolution, the system is very detailed in its
analysis of demographics and behavioral responses to a variety of transportation policies and system
improvements (captured in a broad range of computed performance metrics).

More detailed information on how VisionEval works and what it can do is contained in the [Concepts
Primer](https://github.com/VisionEval/VisionEval/wiki/VisionEval-Primer).

The remainder of this document explains the mechanics of VisionEval: how to install it, and what to
do with it after it is installed.

-   [Overview](#overview)
-   [Installation and Setup](#installation)
-   [Workflow](#workflow)
-   [Editing and Running Models](#editrun)
-   [Getting Results](#results)

<a name='installation'>Installation and Setup</a>
-------------------------------------------------

VisionEval runs within the R Statistical Environment on any system for
which R is available. There are two paths to installing VisionEval:

1.  **Install from stand-alone Windows installer**\\ [Download a zipped
    folder](https://visioneval.org/category/download.html) from the
    VisionEval website for a specific version of R.

    This is the simplest way to quickly get VisionEval on your computer.
    You will first need to install

    -   [R, at least version 3.6.x](https://cran.r-project.org), and
    -   [RStudio](https://www.rstudio.com/products/rstudio/download/ "Download RStudio")
        (a widely-used independent R Visual Environment)
<br/><br/>

2.  **Copy, clone or 'fork' the system code repository**\\ If you area a
    Mac/Linux user, or if you are interested in contributing to the
    development of VisionEval modules, models, framework, or visualizer,
    choose this path

    The current releaseis hosted at [VisionEval on
    Github](https://VisionEval/VisionEval). Development releases are
    available at [VisionEval-dev](https://VisionEval/VisionEval). Once
    you have downloaded or cloned one of the VisionEval repositories,
    instructions for building a runtime are found in `build/Building.md`
    (or you can locate that file in the repository).

### Install for Windows

#### Pre-requisites

You will need:
   - [R, at least version 3.6.x](https://cran.r-project.org)
   - [RStudio](https://www.rstudio.com/products/rstudio/download/ "Download RStudio")

Once you have R and RStudio installed, you can retrieve the VisionEval
installer itself:

#### Installer

***[Get VisionEval Here](https://github.com/VisionEval/VisionEval/releases/download/v2.0.0/VE-installer-Windows-R4.0.2-latest.zip)***

*Note: Almost 650 Mb download! Packaged for R 4.0.2*

The link above will download a .zip file containing the following:
  - The VisionEval framework code
  - Sample models including VE-RSPM, VE-RPAT, VE-State and VE-ScenarioViewer
  - All necessary R packages
  - Documentation (this Getting Started document)

[See this page](https://github.com/VisionEval/VisionEval/releases/tag/v2.0.0) for other R versions
(3.6.1 - 4.0.2) and eventually for other operating systems.

#### Completing the Installation

After installing R 4.0.2 (or the version of R corresponding to the
installer you are retrieving) and Rstudio, unzip the VisionEval
installer into an empty destination folder of your choice.

To complete the installation and start VisionEval, do this: 1. navigate
to the folder into which you unzipped the installer: 2. Double-click
**<tt>VisionEval.Rproj</tt>**

RStudio will start, and the VisionEval will load. You should see this
message:

<code style='color: red'><strong>Loading VisionEval for R 4.0.2
Welcome to VisionEval!</strong></code><code>
&gt; _</code> 

#### Starting VisionEval Manually<a name='changedir'></a>

If you need to start VisionEval manually for some reason, just start
RStudio (or even plain R), change into your installation folder using
  - RStudio's `Session / Set Working Directory...` menu option, or
  - In plain R, the `File / Change dir...` menu option or `setwd` command.

Then run this instruction to start VisionEval:

    source("VisionEval.R")

<a name='workflow'>Workflow of VisionEval</a>
---------------------------------------------

VisionEval models and the underlying software framework are written in
the [R programming language](https://www.r-project.org) for statistical
computing and graphics. The purpose of the model system and framework is
to enable models be created in a plug-and-play fashion from modules that
are distributed as R packages. A simple R script is used to implement a
model by initializing the model environment and then calling modules
successively. Scenarios are then constructed through a set of files that
provide variant model inputs for evaluation and comparison.

To use VisionEval to evaluate scenarios, there are several elements that
users need to set up:

1.  Select one of the [VisionEval
    models](https://github.com/VisionEval/VisionEval/wiki/VisionEval-Models),
    customizing it as needed:

    -   **[VERSPM](https://github.com/VisionEval/VisionEval-Dev/blob/readme-dev/docs/tutorials/verspm/Main.md)**
        – Regional Strategic Planning Model
    -   **[VERPAT](https://github.com/VisionEval/VisionEval/wiki/VERPAT-Tutorial-Overview)**
        – Rapid Policy Analysis Tool
    -   **[VE-State](https://github.com/VisionEval/VisionEval/wiki/VE-State-Status)**
        – VisionEval State-Level Model

    Instructions and tutorials for configuring these models are
    presented in the [VisionEval
    Wiki](https://github.com/VisionEval/VisionEval/wiki), which is your
    entry point to a wealth of documentation on the VisionEval models.

2.  Develop a *Base Scenario* for the region under analysis. The Base
    Scenario specifies:
    -   **Model Geography** (zone structure), reported as Marea
        (metropolitan area), AZones (county-sized), and BZones (census-tract-sized)
    -   **Base and Future Years** to be evaluated for each scenario
        (e.g. 2015 and 2040)
    -   **Local Data Files** describing Base Scenario conditions in the
        region (including both observed base year data, and estimates of
        future year conditions with no scenario policies applied)
3.  Develop variant *Future Actions* and *Scenarios*, by adjusting
    specific input elements for the Future Years. The [`VEScenario`
    sub-system](https://github.com/VisionEval/VisionEval/wiki/VERPAT-Tutorial-Multiple-Scenarios)
    (described for VERPAT, but available for any of the VisionEval
    models) contains functions to help build and run scenarios composed
    of combinations of future actions.
4.  [Run the scenarios](#editrun), either manually, or using the
    `VEScenario` sub-system
5.  [Extract the results](#results) for summarization and further
    analysis in R or export tabular data files to other data analysis
    systems.

<a name='editrun'>Editing and Running Models</a>
------------------------------------------------

As described in the model tutorials [on the
Wiki](https://github.com/VisionEval/VisionEval/wiki), a VisionEval
Scenario contains the following components:

-   The model intiialization and description file, `run_model.R`
-   Global parameters in the `/defs` folder
-   Scenario Input data in the `/inputs` folder

The [documentation on the
Wiki](https://github.com/VisionEval/VisionEval/wiki) for each model
describes what needs to go into the input files and configuration
options, which of the default scenario files you need to change for your
model area, and where to obtain the data you need.

### End User Interface

VisionEval includes simple R command-line instructions for running
models and their outputs. Once you have received the
<code style="color: red">Welcome to VisionEval!</code> message, you can
perform these operations from the R console (either in R or RStudio):

To list the available models (including those you may create as
described below, provided you put them in the standard place), do this:

<code style='color: blue'> dir("models") </code>

You'll see a list of available models:

    [1] "VE-State"         "VE-State-Staged"  "VERPAT"          
    [4] "VERPAT_Scenarios" "VERSPM"           "VERSPM-VehAdj"   
    [7] "VERSPM_MM"        "VERSPM_Scenarios"

To open a model from that list, just do this (remember to save the
result into an R variable, using the assignment operator `<-`):

<code style='color: blue'> rspm &lt;- openModel("VERSPM") </code>

The resulting R object `rspm` now lets you work with the VERSPM model
and its sample scenario. You can customize the inputs to the VERSPM
model (in the "models/VERSPM" directory), or you can use this command to
copy the model and test scenario into a new model directory (which we
are calling `MY-RSPM`):

<code style='color: blue'> myrspm &lt;- rspm$copy("MY-RSPM") </code>

After you run that, VisionEval will tell you the directory that contains
the copy of the model and its test scenario (the exact directory will
depend on your Windows user account and where you installed VisionEval):

    "C:/Users/MyVisionEval/Documents/VisionEval/models/MY-RSPM"

You can run the sample scenario like this:

<code style='color: blue'> myrspm$run() </code>

The model will give you progress updates as it runs:

~~~
  Running model stage:
  C:/Users/MyVisionEval/Documents/VisionEval/models/MY-RSPM
  run_model.R: script entered
  run_model.R: library visioneval loaded
  [1] "2020-08-15 10:50:20 -- Initializing Model. This may take a while."
  [1] "2020-08-15 10:50:41 -- Model successfully initialized."
  run_model.R: initializeModel completed 
  [1] "2020-08-15 10:50:41 -- Starting module 'CreateHouseholds' for year '2010'."
  [1] "2020-08-15 10:50:44 -- Finish module 'CreateHouseholds' for year '2010'."
  ...
  [... More messages ... ]
  ...
  [1] "2020-08-15 10:56:13 -- Starting module 'CalculatePtranEnergyAndEmissions' for year '2038'."
  [1] "2020-08-15 10:56:15 -- Finish module 'CalculatePtranEnergyAndEmissions' for year '2038'."
  run_model.R: run complete.
  Model stage C:/Users/MyVisionEval/Documents/VisionEval/models/VERSPM complete
  Model Stage: /models/MY-RSPM
  Status: Complete
~~~

If errors are reported and you would like to see the log, you can open
this file from RStudio's file pane (in the directory in which your model
scenario is located):

    "C:/Users/MyVisionEval/Documents/VisionEval/models/MY-RSPM/Log_<date>.txt"

where `<date>` will be the `year-day-month_hour_minute_second` at which
the scenario run was started.

If you make changes to your scenario inputs and would like to run the
model again, you should clear the results like this:

<code style='color: blue'> myrspm$clear() </code>

VisionEval will ask you for confirmation, and if you type 'y' and press
`<Enter>`, the output files will be erased. You'll see something this:

<code> [1] "/models/MY-RSPM/ModelState.Rda"  
[2] "/models/MY-RSPM/Datastore"  
[3] "/models/MY-RSPM/Log\_2020-08-15\_10\_50\_20.txt"
</code><code style="color: blue"> Clear ALL prior model results? (y/n/c)
y</code><code> Model results cleared. </code>

Then you can `run` your scenario again.

### Running Scenarios by Hand or from Batch Files

Rather than use the command line interface, it is easy to manually run a
VisionEval scenario by [changing to the directory](#changedir) that
contains the scenario components and then `source`'ing the run\_model.R
script, with this R command line (or the RStudio `Code / Source File...`
menu option):

    source("run_model.R")

You can use that instruction as the basis for R scripts to automate
running a lot of scenarios, or use the [`VEScenario`
sub-system](https://github.com/VisionEval/VisionEval/wiki/VERPAT-Tutorial-Multiple-Scenarios)
to set them up and run them.

To run a VisionEval model from within an operating system batch or shell
script, you should change to your VisionEval model directory, then use
the `Rscript` program to run the model (note the directory separator
will be different on a Macintosh or on Linux):

    cd VisionEval\models\MY-RSPM
    Rscript run_model.R

<a name='results'>Extracting Model Results</a>
----------------------------------------------

Extracting information from a completed model run should be done with
the VisionEval command line interface. First, make sure your model has
run to completion, for example by doing this:

  myrspm &lt;- openModel("MY-RSPM")
  myrspm$status
  [1] "Complete"

If it doesn't say "Complete", please review the instructions above for
setting up and running a scenario.

Outputs from VisionEval are saved in the scenario "Datastore", which may be an [HDF5 file](https://www.hdfgroup.org/solutions/hdf5), or a
directory hierarchy containing saved R objects. The default model
scenarios all use R object storage. Both of the output file formats are
complex and difficult to manipulate manually, so you should use the VisionEval command line interface.

The Datastore is organized hierarchically into "Groups" and "Tables", with each Table containing "Fields" (a.k.a. "Datasets"), each of which is a vector of values organized by model geography, household ID or vehicle ID.  You can list the available Groups by doing this:

    rspm$groups
    Group    Stage Selected
    1   2010     1      Yes
    2 Global     1      Yes
    3   2038     1      Yes

You can list Tables by doing this:

    rspm$tables
    Group         Table Stage Selected
    1    2010     Azone     1      Yes
    2    2010 Household     1      Yes
    3    2010     Bzone     1      Yes
    4    2010    Worker     1      Yes
    5    2010     Marea     1      Yes
    6    2010   Vehicle     1      Yes
    7    2010    Region     1      Yes
    8  Global    Region     1      Yes
    9  Global     Marea     1      Yes
    10   2038     Azone     1      Yes
    11   2038 Household     1      Yes
    12   2038     Bzone     1      Yes
    13   2038    Worker     1      Yes
    14   2038     Marea     1      Yes
    15   2038   Vehicle     1      Yes
    16   2038    Region     1      Yes

You can get the available Fields by doing this:

    flds <- rspm$fields

We have in this example saved the result (an R `data.frame`) into a variable, since it has a lot of rows (the default RSPM model and scenario geography generates 473 rows). You can inspect that using R data.frame operations, or you can save it out to an external comma-separated values (CSV) file like this:

    write.csv(flds,file="Field-List.csv")

To extract all the data from a scenario Datastore, you can execute this instruction:

    rspm$extract()

That will create a directory called `output` adjacent to the scenario `inputs` folder where it will write one comma-separated file for each Table, naming the files after the Group, Table and the date and time at which the model was run (_NOT_ the date and time at which the data was extracted).

The default is to extract all Groups and Tables, and all the Fields within them.  You can select a subset just by pushing a list of names into the Groups, Tables or Fields.  For example, the following commands will generate output files containing only the scenario years 2010 and 2038 from the sample model, and leaving out the Global group:

    rspm$groups <- c("2010","2038")

The groups will now look like this:

    rspm$groups
    Group    Stage Selected
    1   2010     1      Yes
    2 Global     1      No
    3   2038     1      Yes

To clear a selection of `groups` (or `tables` or `fields`), you can just push an empty list, like this:

    rspm$groups <- ""

And then the groups will again have everything selected.

    rspm$groups
    Group    Stage Selected
    1   2010     1      Yes
    2 Global     1      Yes
    3   2038     1      Yes

If you want to manipulate the model results within R, using R functions and packages, you can do the following:

    results <- rspm$extract(saveTo=FALSE)

That instruction creates an R list of R data.frames, each of which contains a single Datastore
table. Be aware that extract loads the entire Datastore into memory. So if your model is large, you
may want to select a subset of groups or tables. Or you can just save the outputs into files and
then load those back into R one at a time for further processing.

The `saveTo` option can be used to specify an alternate directory, so you can do something like the
following to create an alternate output directory with only selected groups or tables:

    rspm$groups <- c("2010","2038")
    rspm$extract(saveTo="years-only")

You will then find a directory called `years-only` next to the model scenario `inputs` containing the extracted subset.

Finally, if you try to do a new Datastore extract into an existing folder (`outputs` or whatever),
VisionEval will tell you that the directory already has things in it. You should specify the
`overwrite` parameter like this to replace the existing files:

    rspm$extract(saveTo="years-only",overwrite=TRUE)
