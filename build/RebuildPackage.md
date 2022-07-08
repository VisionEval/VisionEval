---
output:
  pdf_document: default
  html_document: default
---
# Rebuilding a VisionEval Package Locally using RStudio

A common use case mentioned in the [VisionEval
documentation](https://visioneval.org/docs/ve-buildprocess.html#case-study-1---pums-data-in-vesimhouseholds) is to
replace PUMS data in the VESimHouseholds package.

The current documentation (as of 7/8/2022) is rather obtuse as to the procedures. Here's what you need to do:

1. Make sure you have a running VisionEval that you can start with RStudio.
    * Install R (ideally Version 4.1.3)
    * Install RStudio and start it directly. Verify that it is using the R 4.1.3 version
    * Download the [VisionEval for R 4.1.3
    Installer](https://github.com/VisionEval/VisionEval-Dev/releases/download/beta-release-0.9/VE-3.0-Installer-Windows-R4.1.3_2022-05-27.zip).
    Unzip it into a convenient directory (we'll call that the "VisionEval directory")
    * Check that VisionEval starts by double-clicking VisionEval.Rpro in the VisionEval directory
2. Install the VisionEval package source code
    * Download the [VisionEval for R 4.1.3 Source Code](). **WARNING** This file is huge (over a Gigabyte)
    * Unzip it into the VisionEval directory from the last step. You should then have a "src" folder inside the VisionEval
    directory.
3. Start VisionEval by clicking the VisionEval.Rproj file as you did at the end of Step 1 above (or you may just have
   left RStudio running).
4. Gather the new PUMS .csv files and replace the .csv files in the VisionEval/src/VESimHouseholds/inst/extdata
   directory
    * Follow the steps in the online documentation to [pre-process the PUMS
    data](https://visioneval.org/docs/ve-buildprocess.html#preprocessing-for-visioneval-input-data)
    * Those steps may be hard to understand, but you'll want to end up with two .csv files in the extdata folder:
        1. pums_persons.csv
        2. pums_households.csv
    * Ignore the step about recreating the R datasets in the documentation. Just proceed to the next step here.
4. Configure the RStudio Build Tools
    * pick "Build" from the RStudio top menu then "Configure Build Tools..."
    * choose "Package" in the "Project Build Tools" dropdown.
    * Browse to the package directory: VisionEval/src/VESimHouseholds, where "VisionEval" is the full path of your VisionEval
    directory.
    * Leave all the other defaults as they are.
5. Install the package two times.
    * From the "Build" menu, choose "Install Package" (that will rebuild the R data files)
    * Then do it again: "Build" menu, then "Install Package" (that will put the rebuilt files into the R package)
    * The resulting new version of VESimHouseholds will then be installed with your PUMS data rather than the Oregon
      defaults and it will be used when you next run VERSPM

