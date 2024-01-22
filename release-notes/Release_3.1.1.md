Version 3.1.1 Release
=====================

This release (VE-3.1.1) includes VisionEval 3.0, the "Next Generation" framework,
referred to here as VE-3. It includes updates from the previous releases (documented below)

Please post any issues you encounter on the VisionEval/VisionEval-dev Github repository.

Emerging documentation for this release is located at [docs.visioneval.org](docs.visioneval.org).
You can build the documentation "book" using `ve.build('book')` from the Github development
environment (start VisionEval-dev.Rproj from the VisionEval git root).

Interactive quick start documentation is available by running the "walkthrough()" function and
stepping through the various scripts provided there. The walkthrough provides a rudimentary view of
VE-3 operations.

Release 3.1.1 is the first major public release in a long time, and will appear on the `main` branch
of the VisionEval/VisionEval repository as well as VisionEval/VisionEval-dev. There are no new
functionality changes compared to VE-3.0.4 below, but a variety of bug fixes and operational
improvements have been included and the overall system should be stable and buildable with
current recent versions of R 4.x (2024-01-22: R 4.1.3, R 4.2.3, and R 4.3.2).

Version 3.0.4 Release
=====================

# Updates in VE-3.0.4

This release introduces a simplified export process that is also more configurable. Output is
available to CSV files, SQLite, and any data format supported by the R DBI database interface
(including MySQL/MariaDB, PostgreSQL, SQL Service, or Microsoft Access). It is also straightforward
to write results to Excel spreadsheets. The walkthrough and test functions have been updated and
provide working examples of the core functionality.

Additional documentation on VE 3 will be forthcoming as the next major development effort. Also note
that while VisionEval 3 supports earlier models in that it can run them, the greatest benefit will
accrue to models that have been restructured into VE 3 model stages and scenarios (with different
input sets and scripts appearing as alternatives within a single model).

This release is still somewhat "experimental". Please try it out (and reach out to me -
jeremy.raw@dot.gov - if you need help converting your models to the new structure).

# Updates from "VE-3.0.0 Release 1" to "VE-3.0.3" (prior releases)

A new "BaseScenario" parameter was added for the scenarios sub-folder "visioneval.cnf". This
parameter is preferred over "StartFrom" since it only looks at the base stage setup, not the
Datastore that results from it

  - BaseScenario resolves a bug where some modules that re-write fields in the Datastore
    during the "for 1:2" loop in standard VERSPM always presume the field won't exist on the
    first iteration. Using a StartFrom stage makes the previous stage Datastore available, and
    the first iteration finds an existing field. BaseScenario just points into the stage setup
    and its inputs (not the Datastore). Since runModule now includes an iteration tracking
    variable, we can eventually change the module implementation to use that iteration
    variable in deciding whether to create or overwrite the field so there won't be confusion
    if a StartFrom stage happens to have the same forecast year. StartFrom still works, and
    the test models have been updated: for future year scenarios, StartFrom should point at
    the Base Year model stage (and BaseScenario should be set to the Future Year default model
    stage to pick up the unaltered inptus, forecast years and related parameters).

Additional small changes were also made in Release 2:

  - [UPDATE] Added build support for R 4.3.0
  - [FEATURE] Added a file-level input validation function to the AssignLocTypes module to ensure that housing
  proportions were not out of range.
  - [FEATURE] Added a quickQuery function to the VEQuery object to build a query specification for
  geography-based summary of any variable in the Year output Tables. See VEModel test function
  test_06_quickquery() for working examples
  - [FEATURE] Added the ability to set package-level runtime configuration parameters. Just create a block
  in `visioneval.cnf` named after the package and then set named parameter within it. The most
  useful application is to set for your model:
  ```
  VELandUse:
    HighDensityThreshold: 250 # Default is 100
  ```
  That way you can relax the (sometimes extensive) list of high-density Bzones marked as "high density"
  - [EXPERIMENTAL] You can also use a package configuration block to change the package you want to use (without
  editing the `run_model.R` script). Use the AliasFor instruction like this:
  ```
  VEPowertrainsAndFuels:
    AliasFor: VEPowertrainsAndFuels_aP
  ```
  The package listed as "AliasFor" will be swapped in during the runModule call when the model runs.
  You can configure the AliasFor within specific stages, or for the overall model, so you can use the
  VE 3.0 query mechanism to compare different scenarios without writng a different script (though
  you're also able to do that if you prefer, just set ModelScript=other_run_module.R for the stage).
  - [BUG] Fix in "Function" query specifications to properly handle functions on measures that use
  breakpoints.
  - [BUG] viewSetup function (for displaying the parameters built for a model or a model stage from the
  cascade of `visioneval.cnf` files) was fixed to ensure that the YAML package (used for formatting
  output) was correctly referred to.

Version 3.0.2 Release
=====================

This release (VE-3.0.2) includes VisionEval 3.0, the "Next Generation" framework,
referred to here as VE-3. It includes updates from the previous release.

Installers for R 4.1.3, R 4.2.3 and R 4.3.0 are available below for the end user version in the
"assets" section of this release. Alternatively, to get the latest and greatest, you may clone the
Github "development" branch and build VE-3 using the standard VisionEval build tools.

Please post any issues you encounter on the VisionEval/VisionEval-dev Github repository. Numerous
previous issues have been resolved in this release.

Emerging documentation for this release is located at [docs.visioneval.org](docs.visioneval.org).
You can build the documentation "book" using `ve.build('book')` from the Github development
environment (start VisionEval-dev.Rproj from the VisionEval git root).

Interactive quick start documentation is available by running the "walkthrough()" function and
stepping through the various scripts provided there. The walkthrough provides a rudimentary view of
VE-3 operations.

# Updates from "VE-3.0.0 Release 1" to "VE-3.0.2" (this release)

A new "BaseScenario" parameter was added for the scenarios sub-folder "visioneval.cnf". This
parameter is preferred over "StartFrom" since it only looks at the base stage setup, not the
Datastore that results from it

  - BaseScenario resolves a bug where some modules that re-write fields in the Datastore
    during the "for 1:2" loop in standard VERSPM always presume the field won't exist on the
    first iteration. Using a StartFrom stage makes the previous stage Datastore available, and
    the first iteration finds an existing field. BaseScenario just points into the stage setup
    and its inputs (not the Datastore). Since runModule now includes an iteration tracking
    variable, we can eventually change the module implementation to use that iteration
    variable in deciding whether to create or overwrite the field so there won't be confusion
    if a StartFrom stage happens to have the same forecast year. StartFrom still works, and
    the test models have been updated: for future year scenarios, StartFrom should point at
    the Base Year model stage (and BaseScenario should be set to the Future Year default model
    stage to pick up the unaltered inptus, forecast years and related parameters).

Additional small changes were also made in Release 2:

  - [UPDATE] Added build support for R 4.3.0
  - [FEATURE] Added a file-level input validation function to the AssignLocTypes module to ensure that housing
  proportions were not out of range.
  - [FEATURE] Added a quickQuery function to the VEQuery object to build a query specification for
  geography-based summary of any variable in the Year output Tables. See VEModel test function
  test_06_quickquery() for working examples
  - [FEATURE] Added the ability to set package-level runtime configuration parameters. Just create a block
  in `visioneval.cnf` named after the package and then set named parameter within it. The most
  useful application is to set for your model:
  ```
  VELandUse:
    HighDensityThreshold: 250 # Default is 100
  ```
  That way you can relax the (sometimes extensive) list of high-density Bzones marked as "high density"
  - [EXPERIMENTAL] You can also use a package configuration block to change the package you want to use (without
  editing the `run_model.R` script). Use the AliasFor instruction like this:
  ```
  VEPowertrainsAndFuels:
    AliasFor: VEPowertrainsAndFuels_aP
  ```
  The package listed as "AliasFor" will be swapped in during the runModule call when the model runs.
  You can configure the AliasFor within specific stages, or for the overall model, so you can use the
  VE 3.0 query mechanism to compare different scenarios without writng a different script (though
  you're also able to do that if you prefer, just set ModelScript=other_run_module.R for the stage).
  - [BUG] Fix in "Function" query specifications to properly handle functions on measures that use
  breakpoints.
  - [BUG] viewSetup function (for displaying the parameters built for a model or a model stage from the
  cascade of `visioneval.cnf` files) was fixed to ensure that the YAML package (used for formatting
  output) was correctly referred to.

VisionEval 3.0.0 Release (VE-3)
===============================

## Setting up a runtime environment

The VE-3 runtime environment has three parts:

1. The "load directory" containing the startup scripts
    - By default, the installation location of VisionEval
    - VisionEval should be started from this folder (but it need not run models there; see "runtime directory" below)
3. The directory containing the VisionEval R package library (`ve-lib`)
    - Usually located within the load directory (end user installation)
    - May instead be located adjacent to the load directory (as in the `built` artifacts of the Github)
5. The "runtime directory" which contains models to be run and their results
    - You can change the runtime directory at startup by setting the VE_RUNTIME operating system environment variable
    - This scheme allows you to install new VisionEval runtimes and not overwrite (or have to copy) your previous models. You may still want to copy those models within the VE-3 environment before running them again (or just use the `run("save")` function the first time you run them).
    - In the development environment, what is called the "runtime" directory is used as the "load directory" and if you use "ve.run()" to launch a VisionEval run, the development environment will create a separate "runtime.test" directory where it does its work. That is to avoid accidentally overwriting models you may be developing within the Github tree.

## Sample / Prototype Models

The VisionEval sample models are stored within the VisionEval packages and copies of those standard
models, with a small sample dataset, can be installed using the VE-3 "installModel" function. You
can access help for that function by loading the VEModel package and running
`?help("installModel")`. A default name is provided for the model when it is installed, and you can
use that name later to re-open the model (e.g. in a new VisionEval session). The default name is
constructed from the model (VERSPM, VE-State, others) and the model "variant", which describes a
specific set of sample files that illustrate model structures and operations that you can adapt for
your own models.

Installed models are set up in subdirectories of the "models" subdirectory of the VisionEval runtime
directory. The name of the model subdirectory is the model name used by VE-3. So if your "models"
directory contains a subdirectory called "myRSPM", you can use the `mod <- openModel('myRSPM')`
function to open the model. See the R help for `openModel`.

Both `installModel` and `openModel` functions return model objects that contain the functions and
data needed to configure, run and report results from the model. See below and the walkthrough for
more information.

## Setting Up and Running Models

A typical VE-3 model development process would be to install a prototype model that does what you
want, and then adjust the model structure by creating your own model geography (zones), setting the
model base year and run years (and adjusting the deflators.csv file), creating all the module inputs
for base and future years in your baseline scenario, and once that is all running, creating
scenarios (subsets of the main model that make changes to the inputs for the scenario). See
[docs.visioneval.org](https://docs.visioneval.org) for more information.

**Running existing VisionEval models...** If you already have a VisionEval model, you can still run
it the old way, using `source("run_model.R")` from the directory containing the inputs, defs, and
run_model.R script. However that approach does not put full information into the model results so
some of the advanced VE-3 functions for examining the model will not be available.

A better way to get your old models into VE-3 is to create a subdirectory of "models" in the runtime
directory with the name of your model (say `myOldModel`), then use `mod <- openModel('myOldModel')`
to open the model, and `mod$run()` to run it. It should (fingers crossed, file an issue if it
doesn't) "just work". You don't have to change a thing, and you can still "extract" or "query" the
results as usual (see below).

**Re-running models** VE-3 notices if you've already run your model and if you run it again, it will
do nothing and report the model status (`print(mod)` will show you the status at any time). If the
run failed for some reason, it will try to run again. To force it to throw away the previous run, do
`mod$run('reset')`. If you want to save the previous run, do `mod$run('save')` and the previous
results will be moved to a new timestamped directory prior to starting the whole model over again.

**Parallel Processing** VE-3 has a basic implementation of parallel processing. You enable it by
calling the R function `mod$plan(workers=3)` before you do `mod$run()`. VE-3 will group the model
stages (see below) into sets that have the same starting point (see below, StartFrom) and
distributed them across the number of "workers" (CPU's) available on your computer. Note that a
large VisionEval model will consume up to 7 or 8 gigabytes of memory per running stage, so the
limiting factor on parallel processing is more likely to be how much RAM you have and not how many
processors. The parallel processing is implemented through R's "future" package (and support from
"parallely") and any type of connection you can set up that way (including clusters of machines)
shoudl work "in principle" and if you'd like to figure out how to set that up, [get in touch with
us](mailto:info@visioneval.org). The stock parallell processing implementation uses the "callr"
package to set up multiple R sessions.

## VE-3 Model Configuration

To enable the new VE-3 features, particularly model stage and scenario management as well as
efficient extraction of results, VE-3 uses a new configuration scheme. Parameters that used to be
kept separately in "defs/run_parameters.json" and in the arguments to the initializeModel function
in the run_model.R script are now maintained in one or more YAML files called "visioneval.cnf"
(which, if you're seriously into nostalgia, could still be a JSON file called "run_parameters.json";
it should still work). Plus there are a lot more parameters to control other features of VE-3.

You can have a global "visioneval.cnf" (where you might set your preferred DatastoreType or a common
random Seed), each model *must* have a "visioneval.cnf" in its root directory (i.e. inside
"models\myModel") that describes the base model, and if the model has stages or scenarios defined in
subdirectories of the the model (see below), those can also have a "visioneval.cnf" describing how
they differ from the base scenario. However, you can also configure scenarios directly in the main
model "visioneval.cnf", but that gets inconvenient if you have lots of them, or if they change
frequently; see below on setting up scenarios.

## Model Stages

The most radical change in VE-3 (which is fully backward compatible with old models) is that models
are reconceived as a series of "model stages" - units that can be run to generate output in a
Datastore. A model stage can be part of a model (e.g. population synthesis, or just the base year
run of the model, or whatever). A stage can have its own run_model.R script (which can have some
informative name if you prefer), its own inputs, and its own outputs. All the stages share the basic
model structural information: everything that is in the "defs" directory, notably "geo.csv",
"units.csv" and "deflators.csv".

Stages can be connected to each other, using the "StartFrom" parameter in the stage's
"visioneval.cnf" file. When the stage is run, any information that is not present in that stage will
be sought in the "StartFrom" stage (and if that stage also has a StartFrom, the run will keep
looking up the "ladder" of StartFrom stages until it has found everything it needs). A stage without
a StartFrom needs to have all the inputs and scripts it needs in its own directories. If you have
multiple stages that share the same StartFrom (e.g. future year scenarios that StartFrom the default
future scenario), those can easily be run in parallel (see above); the default is to run them
sequentially, which is better the first time through, since it is a bit harder to find the murder
weapon if one or more of the stages comes to an untimely demise.

It is possible to start a model from another model (in effect, turning the other model into a
"stage") but that is intended mostly for use in debugging a large model that has crashed many
minutes (or hours) into a run, and the key difference is that it _copies_ the previous model's
Datastore, rather than just accessing it in place. To use that, you can set up another model to load
the partially-formed carcass with a script that starts just before the old model crashed and then
play around with the inputs and not have to wait hours to find out if it worked or not. Use the
LoadModel configuration parameter in the subsequent model's "visioneval.cnf" to copy over the
datastore (and the LoadStage if you need to load a stage other than the very last one in the
previous model).

VE-3 puts its results in a subdirectory of the model called "results". Each stage, if any are
defined, will go in a subdirectory of "results" named after the stage.

One downside of the stages is that each stage has its own separate Datastore which VE-3 links
internally to the StartFrom stage Datastores (without copying them). So to get at the results from
your full model run, you'll either need to use the VE-3 extraction and query mechanims (see below).
If you have R scripts already that run on one big Datastore, you can merge the stage datastores into
a single datastore through a process called "flattening" the Datastore. You can just copy the
results from your stage and add the Flatten parameter, like this:

```
rs <- mod$results("stageToFlatten")   # stageToFlatten is the name of the final stage you would like all the data from
rs$copy("OtherDirectory",Flatten=TRUE)  # Generates a Datastore with all data available to the stage from its StartFrom stages
```

Note that you can open that "OtherDirectory" using the `openResults` function (see its R help) that
you can then extract or query using the machinery described below.

## Building scenarios

Scenarios are just model stages that have the special property of being "Reportable" (that is, they
will automatically be included in extracts or in queries). You can manually mark a stage as
"Reportable" in its visioneval.cnf, but VE-3 marks any stage Reportable that does not have another
stage starting from it (i.e. a terminal stage).

So to make a scenario, you define a model stage. If the changes do not involve different inputs
(e.g. separating base and future years into different stages), you can just define the model stage
in the model's visioneval.cnf.

For scenario-type stages, you might want to define different inputs, or perhaps even a different
run_model.R script (though be careful with that - if the same data doesn't emerge from each
Reportable stage, the query process may leave you with "NA's" in some of your metrics). That's most
easily done by creating a sub-directory and putting a visioneval.cnf with the stage particulars into
that directory. It's probably better to put run-model.R scripts under different names in the single
"scripts" directory for the overall model than to bury them in the scenario stage. The stage
(scenario) sub-directory should just contain input files that are different from what is available
to its StartFrom stages.

So to set up scenarios efficiently, you'll create your default future year (with complete inputs),
then just create a few altered input files in the scenario subdirectory of the model (the
subdirectory is named after the "stage" according to its - or the model's - visioneval.cnf). When
the stage runs, it looks for input files locally, and anything it doesn't find it searches for in
its StartFrom stage (and on up the ladder, as described earlier). Building scenarios as stages in
that way makes it very easy to keep track of what's different in each scenario. Plus, when you run
the model, you just run the model - each scenario (stage) gets run in its turn automatically. And
when you extract or query the results, you get the results for all the (Reportable) scenarios. -
though you can also get at the results for stages that are not reportable; you just have to ask for
them explicitly by name or index.

Instead of defining individual subdirectories for your stages, you can push them all down into a
single subdirectory of your model called "scenarios". Inside that scenarios, you can construct a set
of manual scenarios (and they will all be reportable by default, even if some of the scenarios you
define start from others - that's the key behavioral difference). Or you can construct variant
inputs and have VE-3 combine them into all possible permutations and combinations. That reproduces
in essence the behavior of the old VEScenario package that no longer exists. If you do combination
scenarios, you can visualize them easily with the (now long-in-the-tooth) R HTML visualizer. More
modern approaches to dumping VisionEval outputs into Access, Excel, SQL generally or using various
Tableau or Power BI templates are under development and will go into another minor update release
shortly.

## Extracting model results

Extracting results is pretty simple. You run the model, you get a "VEResults" object by calling `rs
<- mod$results()`, and then you do `rs$export()`. Be default, that creates .CSV files. You can use
`rs$export("sql")` to generate a SQLite database. The 3.1 export mechanism is configurable to use
any RDBI interface.

## Querying model results

A spiffy mechanism for generating summary queries was built a few years back by Brian Gregor (the
original author of VIsionEval). That mechanism was reworked into VE-3 to query model results and
generate tables of outputs for multiple scenarios within a model (all the "Reportable" ones). The
idea is to generate summary metrics from simple one-line computations (e.g. household DVMT per
capita) and build a table of all the metrics for each scenario. It's more easily shown in examples
than explained briefly in text, so check out the "walkthrough" and also the "queries" subdirectory
in many of the sample models (see "installModel" above).

The metrics can be split out by grouping variables (e.g. Income or some adjacent characteristic,
such as Households in urbanized areas). In VE-3, Bzones can be tagged with new properties (e.g.
identifying EJ zones) and those can also be used to subdivide the query metrics - just add your tags
as columns in the model's geo.csv file before you run the model.

Queries can generate two types of output format: "wide format" which produces one column for each
year of each scenario and one row for each metric, and "long format" which produces one row for each
scenario for each year for each metric (so there is only one column of metric values in the
resulting output). See the walkthrough for examples of how to generate those outputs.

## Interior changes

As noted above, model stages can have different run_model.R scripts. That will support using (for
example) different versions of the PowertrainsAndFuels module in different scenarios.

The framework now supports modules with "dynamic" specifications (generated by a function call at
runtime, rather than being built as static data into an R package). See the VESnapshot package and
help for its functions "Dynamic" and "Snapshot" for more details.

### Earlier pre-release versions

The following notes were written at the time of earlier pre-releases and contain additional
information about VE-3.

In beta-release-0.8: Updated 01-getting-started.md in VisionEval-docs (and configuration to build it
into the installer) - currently a pull request. Fixed a variety of problems with queries and
indexing model contents. Fixed runtime and development startup (including functional access to the
walkthrough's in their own independent runtime folder). See the getting started document for some
instructions.

Updated tests and walkthrough, and in the process fixed a bunch of bugs (beta-release-0.7).

In beta-release-0.7: Updated with a new test architecture (see the changelog, test-architecfture.md,
for description) (beta-release-0.6). Also fixed some bugs in the VEModel results extraction code
where earlier stages in a staged model were not being included in the results.

Updated to improve walkthrough and test access and clean up runtime build process
(beta-release-0.4). This release (which skips beta-release-0.3) includes the patched MultiModal
module, restructuring of the tutorials and vignettes, some fixes to the build process, the locate
employment bug patch, and (most exciting) the fully functional dynamic visualizer that uses the
VEModel scenario and query features to let you configure exactly what scenario categories and
metrics you want to display.

Updated to beta-release-0.5 which makes a variety of minor fixes (including updates the scenario
functionality to work better, added new scheme for managing pull requests and the changelog.

The internal VEModel test.R script, which I've used to develop the framework, is also included in
(and will run from) the installed runtime. Just `source("tools/tests/VEModel/test.R")` once you have
launched the runtime. I will be updating walkthrough.R to demonstrate the test features in a more
tutorial fashion.

The revised walkthrough will be part of beta-release-0.5, which will also include updating the
scenarios and visualizer so you can visualize an arbitrary set of manually constructed scenarios,
rather than just the category-permutation scenarios that track the old VEScenario functionality.
That will make the visualizer a fully-supported element of VisionEval.

The installers included as assets below have been updated to tag beta-release-0.4.

Key changes include the following:

- New framework VEModel package to manage the User Interface
- Extensive changes to the visioneval framework package itself to support the new UI
- An extensive runtime "walkthrough" describing and running most of the new User Interface features, plus access to the VEModel tests.R script, which is the definitive statement of "what works".

Though it is still feasible to run a "classic" VisionEval model by doing `source("run_model.R")`,
the full power of the new framework requires a few simple modifications to the model structures. We
should probably have a vignette on converting an old model. The basic strategy is simple: move the
model into a subdirectory of the "models" directory in the runtime (callint it, say, "myModel"), and
create a visioneval.cnf file that reproduces key elements from defs/run_parameters.json and the
InitializeModel function in run_model.R. You can remove intializeModel from the run_model.R script
(though you can also leave it - it will be ignored if you use the VEModel run function). Then just
use `mod <- openModel("myModel")` and then `mod$run()`. You'll also eventually want to restructure
the various scenarios you created by varying inputs to that model, and there will soon be a tutorial
on how to do that.

Aside from the above, here are the key user-visible changes in this VisionEval version (there are
probably many more, but they'll be intuitive or optional for basic model setup and runs).

- Pre-packaged sample models are delivered in packages either as standalones (see the new VEState package) or as add-ons to a package that also delivers modules (see the updated VETravelDemandMM package). Use `installModel()` to list available models, `installModel("VERSPM",var="")` to list "variants" (the same sample model set up with different staging and configuration strategies), or pick a variant and do `installModel("VERSPM",var="base")` to copy one of the models into the runtime "models" folder.
- The new model packaging scheme provides a new recommended way to generate a model: just create an R package like what you see in VEState, VERPAT, or VETravelDemandMM that has a suitably configured "inst/models" sub-directory. Once you install such a package (using standard R install.packages), the model automatically becomes visible and available to `installModel`. 
- As you will see VERPAT and VETravelDemandMM, an inst/model package can also include modules specific to that model (and the model thus functions like a vignette that shows how the package is used and what it does).
- Models can be organized into "stages", or they can be "loaded" from another model. Stages can start from another stage, so you can run a base model then run future scenarios without re-running the base.  Try "installModel("VERSPM",var="pop")" to look at a fully-staged sample.
- Stages are "cascading" in the sense that key features like the defs/geo.csv file, any of the inputs, or the model script are passed from earlier stages to later stages (unless the later stage explicitly redefines it).
- Stages sound complicated, but they greatly simplify scenario management and documentation: your new future scenario may only change three input files, so you just set the new scenario StartFrom to the existing future scenario and then drop just the three changed input files into the new scenario's sub-directory. Then you can run the model as usual.
- And when you run a model, if any of its stages have already run to completion, they won't get re-run unless you "reset" the run. New scenario stages you may have added will get run.
- Before or after running a model, you can use its "list" function to inspect all the inputs and outputs of each stage of the model, identifying which exact file was used on input, and which module wrote the various output datasets. That function is poorly documented still, but see the walkthrough and also "tests/test.r" in the VEModel package source code for more walkthrough-like testing.
- You can use multiprocessing without any special changes. At a minimum you can do `model$plan()` just before doing `model$run()` which will use all available cores, or you can do `model$plan(workers=3)` to pick a specific number (smaller is sometimes better). When multiprocessing is enabled, all the model stages that StartFrom the same place will run in parallel. See the walkthrough and VEmodel/tests.R. Be aware however that the limiting factor on how useful multiprocessing will be is often the amount of RAM you have, not the number of cores - VE models that have many zones (or that simulate a lot of zones, like VE-State) use a LOT of memory. If you exceed available RAM, the memory will get swapped onto your hard disk, leading to a condition called "thrashing" that is ultimately just as bad (or worse) than it sounds.
- In principle (that's what "beta" means in Greek) the multiprocessing should be trivial to extend across multiple servers or into the cloud using standard R future "plans".
- Once you have run a model, you can get at its results easily using the "extract" and "export" functions to generate data frames and save the results into tabular formats. Right now, only .csv files are supported, but the forthcoming ViEIO package will allow the output formats to be readily extended to any tabular environment (SQLite, Excel, Access, MySQL, Apache feather).
- An extensible query facility can also be applied to model results to generate performance metrics either for the entire region, broken out by Geography (Marea, Azone, Bzone), or set up by breakpoints (e.g. Household income brackets). You have to write the queries using framework tools or just by editing an R script file, but there are working examples included for most of the models that you can easily adapt.
- The Query specification supports some extra fields that help control the visualizer display so it looks nicer. See the "scenarios-cat" or "scenarios-ms" variants of VERSPM and look in the "queries" sub-folder for a fully-fleshed-out example.
- Query results are cached (but can be regenerated if you like), and can be extracted into a data.frame (`query$extract()`)or exported into a tabular file (like the raw model results), again eventually using ViEIO.
- When a model is run in stages, each stage has its own Datastore. No data is duplicated (new data from a stage is always written into its own Datastore). If a dataset is not present in the current stage, each StartFrom stage is searched until the dataset is located.
- A Datastore can be manually copied to a new location (so can a model, with or without its results). A copied Datastore will always be associated with a single stage, but you can request that the copy operation "flatten" the Datastore, so that all the datasets are in one place.
- When a Datastore is copied or accessed, the Datastore type (currently just "RD" or "H5" for hdf5, but be on the lookout for ViEIO version 2) can be changed. You can specific different Datastore types for each stage of a model if you like, or load a Datastore from another model with a different type and (in beta/principle) it "just works".
- You can change the names of pretty much any of the standard files or directory locations. In the new version, model results go into a "Results" sub-directory, but you can set that back to "." to get the classic behavior. Such changes are managed in various places by files called "visioneval.cnf" that can be either in YAML (simple new style) or in JSON (like the old run_parameters.json file).  See the visioneval.cnf.sample file in the root of the runtime folder for details (though there are currently a few more configurable parameters than appear there, mostly related to configuring scenarios).
- A first cut at a tableau prototype is included in the "tools" directory (which can be extended with new scripts if you like). The "visuzalizer.js" file that is created if you save out the visualizer setup should eventually form the basis for tableau visualization - we'll come back to that.
- The old VEScenario functionality, now (as of beta-release-0.4) including the R visualizer, is fully integrated into the framework. You can create model stages in a sub-directory of the model, either by explicitly putting them there (and remember you only have to write the parts that differ from the base scenario), or by using the category combination approach that VEScenario made available.  `installModel("VERSPM",var="scenario-ms")` to see examples of setting up manual scenarios as model stages (visualizer may not yet be working here), and `installModel("VERSPM",var="scenario-cat") (visualizer definitely works here) to see an example of setting up combination categories (as well as an example of how to comment out some categories so you can try out such a model in a finite amount of time without having to run 637 scenarios - though you could if you wanted to).
- The old R visualizer is now integrated into the framework. Currently, you get there by running a visualizer-compatible query (which is really any query, except it will look nicer if you set some of the "Export" fields in the query specification; see the scenario sample models) and then doing `query$visual()`. By default it will launch a browser window, load the visualizer, and populate the it with scenario categories and results.  If you set SaveTo=NULL, it will dump a working set of files (like the old VEScenario visualizer) into a sub-directory of results/outputs. The walkthrough will get updated to demonstrate all of that. The upshot is that you will have full control over the input categories as well as what metrics appear in the output histograms just by configuring the model and its query (the visualizer HTML, Javascript and CSS need never be touched)

