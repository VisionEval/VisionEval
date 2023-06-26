# VisionEval


**IMPORTANT**: the `development` branch is no more! Yes, it looks like it is still here,
but that is an **illusion**. This branch (aside from this one commit changing the
README.md file) is identical to `master` at the `public-2.0` branch.

All new development should be based on the `development-next` branch.

Thanks for your cooperation. If you have questions or complaints, please contact
[info@visioneval.org](mailto:info@visioneval.org).

VisionEval is a model system and supporting software framework for building collaborative
disaggregate strategic planning models. 

## Documentation

Documentation for VisionEval is online at
[https://github.com/VisionEval/VisionEval/wiki](https://github.com/VisionEval/VisionEval/wiki)

## Release

You can retrieve the current binary release of VisionEval from
[https://visioneval.org/category/download.html](https://visioneval.org/category/download.html)

Detailed [Release Notes](#release-notes) for VisionEval 3.0 are available below.

## VisionEval Repositories

There are five repositories in the VisionEval organization to serve different purposes:

 - **[VisionEval](https://github.com/VisionEval/VisionEval)**: Public release version of VisionEval. There is one master branch only. This wiki is associated with the VisionEval repository. If you have a bug report or other issue, create an issue instead in the VisionEval-Dev repository ([here](https://github.com/VisionEval/VisionEval-Dev/issues)).
 
 - **[VisionEval-Dev](https://github.com/VisionEval/VisionEval-Dev)**: Main repository for developers and power-users who want to contribute code improvements. There are multiple branches, including `master` (which is can be considered the beta release) and `development` where active code development happens. Additional branches can be used to evaluate new features or pull requests. Developers / power-users should [create issues](https://github.com/VisionEval/VisionEval-Dev/issues) and [pull requests](https://github.com/VisionEval/VisionEval-Dev/pulls) to this repository.
 
 - **[VisionEval.org](https://github.com/VisionEval/VisionEval.org)**: Website repository. You can [create issues](https://github.com/VisionEval/VisionEval.org/issues) here for website-related change requests.
 
 - **[VisionEval-Docs](https://github.com/VisionEval/VisionEval-Docs)**: Documentation respository. 

 - **[VisionEval-Extras](https://github.com/VisionEval/VisionEval-Extras)**: Additional tools and module packages that may be added to core VisionEval.

See the [Repository Structure page](https://github.com/VisionEval/VisionEval/wiki/Repository-Structure) in the wiki for more details.

## Issues

Please submit issues, bugs, or feature requests about VisionEval on the [VisionEval-Dev issues
page](https://github.com/VisionEval/VisionEval-Dev/issues). 

Please submit issues or content change requests about the VisionEval.org website on the
[VisionEval.org issues page](https://github.com/VisionEval/VisionEval.org/issues).

## For Developers: Building 

To modify and rebuild the released VisionEval system, you can clone a suitable branch
(either "master" or "development") from the "development" repository:
[VisionEval-Dev repository](https://github.com/VisionEval/VisionEval-dev). 

Here are the build steps:

1. Clone the Github
2. Start VisionEval-dev.Rproj in the root directory
3. Run ve.build() to construct the packages
4. Run ve.run() to launch the runtime (note that the built "runtime" is only used indirectly
    1. VisionEval runs in the new "runtime.test" directory
    2. You can set a directory of your choice selected either by passing it as a parameter
       (`ve.run('myRuntimeDirectory")`) or by setting the VE_RUNTIME system environment variable.
       A complete working runtime will be created at that destination if it does not already exist
5. Once running, do `walkthrough()` or run `ve.test()` (with no parameters) to get a list of
   sample scripts illustrating basic functions (all to run in an additional temporary runtime to
   avoid confusing them with real work).
    1. `walkthrough()` will also work for ordinary users in the distributed runtime
    2. Run `ve.test("VEModel")` to load more detailed API test functions (a comprehensive exercise of what works and how).

If you intend to submit changes back to the VisionEval project, please clone the [VisionEval-Dev
repository, `development` branch](https://github.com/VisionEval/VisionEval-Dev/tree/development).
Pull requests against this branch are welcome (but make sure you have rebased the pull request on
the current HEAD of `development-next`).

Pre-built binary installers of recently released versions (the "master" branch) are available at
[https://visioneval.org](https://visioneval.org) and as "releases" in either the VisionEval
repository (the `master` branch) or in VisionEval-dev (the `development-next` branch).

You can install directly from a copy (.zip) or clone of this VisionEval repository branch, using the
instructions in the `build/Building.md` file on a VisionEval code branch.

## Release Notes

VisionEval 3.0 is a major update to VisionEval that addes many new features, including a simple
set of commands to manage models, scenarios, results and exports through a simple end-user API.

[https://docs.visioneval.org](https://docs.visioneval.org) does address VisionEval 3.0, and
additional documentation will be appearing there.

## Release Notes

VisionEval 3.0 includes a great many changes from previous versions:

- Numerous internal changes
  - Builds and runs correctly with R 4.2.2 (the release of R 4.2.x required significant internal framework changes)
  - Efficiency improvements for running models in stages (and not re-running the base year in every scenario)
  - All the module packages have been fixed to pass R CMD check with no messages apart from being grossly oversized
  - Dynamic module specifications
  - Row-level module input checking (optional function)
- End User API for managing and running models and scenarios
  - Model management functions
  - Improved scenario management within a single model
  - R-level interrogation of Datastore contents, model structure, and model outputs
- Many standard model examples illustrating features of VisionEval 3.0, including a walkthrough (run `walkthrough()`
  from the VisionEval command line).
  - Model samples can be installed using the `installModel` function (see the walkthrough)
  - Sample models can be used as the basis for developing a local model
- Functions to extract data from the VisionEval Datastore into standard tabular formats
  - R data.frame, .CSV, SQL
  - Optional unit conversions from internal units (e.g. so speeds can be reported in MILES/HR instead of MILES/DAY)
- Simple query facility for extracting summary performance measures
  - Can generate tabular formats for direct display (R data.frame, CSV) or post-processing (SQL, Tableau)


