# Developing VisionEval using RStudio

As of the July 2020 release of VisionEval, [RStudio Desktop](https://rstudio.com/products/rstudio/download/#download) is supported as the preferred environment for developing and running VisionEval software and scenarios.

If you are interested in developing or debugging [VisionEval](https://visioneval.org "VisionEval"), you can get started in one of two ways:

1. **From the Github**
    * _When to use this method:_
      * To work on the VisionEval development branch and its unreleased features (as opposed to the `master` branch which corresponds to the current binary release).
      * You _must_ use this method if you are developing something (like a new module) that you think you might submit back to the VisionEval project.
    * _How to use this method:_
    1. Change to the parent directory where you would like to put the VisionEval Github source
    1. Clone the Github development branch from [https://github.com/VisionEval/VisionEval-dev](https://github.com/VisionEval/VisionEval-dev), like this:
      ```
      git clone --branch development https://github.com/VisionEval/VisionEval-dev.git MyVisionEval-dev
      ```
      * Change the final argument to give the repository a useful name in your local environment.
      * This instruction will still clone all the other branches as well.  Use the `--single-branch` option to limit the clone just to the `development` branch
      * Use the `--depth=1` argument to forego all the branch history. That will speed the clone, but is inappropriate if you are planning to re-submit your changes.
    2. Follow the setup and build instructions in the build/Building.md file for the development branch of [VisionEval-dev](https://github.com/VisionEval/VisionEval-dev).
      * You will need [RTools40][getRTools40] plus [Git for Windows](<https://gitforwindows.org/>) and additional software if you plan to re-build the runtime documentation. 
    
2. **From the Binary Installer**
    * _When to use this method:_
      * To make local modifications to the current runtime release of VisionEval (e.g. to rebuild one of the packages with local data)
      * To introduce debugging statements or set breakpoints in the R code as a way of finding problems when you try to set up your own data for one of the models.
    * _How to use this method:_
      1. Download the runtime installer (zip file) from [the VisionEval Download Page](https://visioneval.org/category/download.html "VisionEval Download")
        * Pick the installer for your version of R and your Operating System
        * Windows and Mac OSX are supported for binary installations
      2. Install the PackageSources (zip file) for the same release date, also from [the VisionEval Download Page](https://visioneval.org/category/download.html "VisionEval Download")
        * Just unzip the PackageSources into the exact same folder you put the VisionEval runtime
        * It will create a new `src` sub-directory next to the other VisionEval folders
      3. Make sure you have [RTools40][getRTools40]
        * RStudio itself will help you get it, or you can download and install it manually.
      
[getRTools40]: https://cran.rstudio.com/bin/windows/Rtools/

## Configuring RStudio

After you have completed one of the installation steps above (either building VisionEval from scratch using the VE-Installer, or installing the runtime plus the corresponding source repository), you need to configure RStudio.

These are the things you need to set or verify:

1. Check that RStudio is configured to use the same version of R as VisionEval
  * Open RStudio
  * Go to "Tools / Global Options / General"*  and confirm that the R shown in "R Sessions / R Version" is consistent with what VisionEval will use
2. Complete the VisionEval runtime installation
  * From the runtime directory where you either (a) built VisionEval with VE-Installer, or (b) unzipped the VisionEval runtime, double-click (run) the file `VisionEval.Rproj`
    * If you're on a Macintosh, run `ve.install()` in the RStudio console to install the VisionEval packages into the R library.
    * It may take a few minutes on the Mac to complete the installation
  * You should see the message `Welcome to VisionEval!`
3. Check the R library paths, by running `.libPaths()` in the RStudio console
  * The first element should point to ve-lib, where the VisionEval packages and dependencies are installed 
  * If you did the full build with VE-Installer, a second path will be included to the development library (where R packages that are needed to build, but not to run, VisionEval are installed).
  
## Debugging VisionEval with RStudio

If you're having trouble getting a VisionEval model to run with your data, you can easily use the package sources to set breakpoints, inspect local R variables, and examine how individual VisionEval modules are running. _You do need to be familiar with R scripts  and the R / RStudio environment to make this work._

To do this kind of debugging, here are the key steps:

1. Start RStudio through `VisionEval.Rproj` as usual
2. Identify the module you would like to debug
  * For example, look at the console output while the model runs and seeing what it was doing when it crashed
3. Figure out which VisionEval package contains that module by looking into the models `run_model.R` script and finding the corresponding `RunModule` statement
  * We'll use `VETravelDemand` as a sample here
4. Set RStudio to work on that package
  * Open the "Build / Project Options"
  * Set "Project build tools" to "Package"
  * Browse to the "Package Directory" you're interested in (e.g. `src/VETravelDemand`)
  * Push "OK"
5. Link the source and binary package by rebuilding it in RStudio
  * On the build menu, chose "Build / Install and Restart"
7. Now you are ready to examine or set breakpoints in the code:
  * Navigate to the "R" folder in the RStudio files pane
  * Find the R script corresponding to the module you're interested in
  * Set your breakpoints
  * Run your model
8. Look here for [help using RStudio to debug code](<https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-RStudio>)

## Hacking VisionEval with RStudio
  
__Warning:__ _If you change any of the VisionEval packages and something breaks, you will have to re-install it (binary and source)._ __Proceed with Caution!__

Once you have everything set up, you can use RStudio to do the following things with VisionEval. Exact procedures are beyond the scope of this document.

1. Rebuild any of the packages, which lets you:
   * Re-run the model estimations using updated data files
   * Adjust or fix any of the model code
2. Add a new package (by copying or revising an existing one, or from scratch)

If you ever plan to contribute those changes back to the VisionEval project, you will need to make corresponding changes in your clone of the Github repository and then make a Github pull request. That implies that you will need the full VE-Installer setup to be sure it will all work.     
