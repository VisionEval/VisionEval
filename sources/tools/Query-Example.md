### Query Example

The file `Sample_Measures_Script.R` will process a list of measure specifications across the Years
in a Scenario Datastore and generate a .csv file that contains the measures.

Default/Example measures in Test-Spec.R will work with output from the sample VE_RSPM. Adjust the
Test-Spec.R file for your own MArea and Azone geography and it should work with your local version
of VE_RSPM.

To run this query, start Rstudio, make sure there's a `PM-Spec.R` file in the same location as the
script (or in the same directory next to this script) and that it points to the scenarios you would like to
summarize.  Then just do this (from your VisionEval runtime directory):

source("tools/Query-Example/Sample-Measures-Script.R")

CONFIGURE THE FOLLOWING PARAMETERS FOR THE SCRIPT RUN:

**SpecFile** - (optional) file name containing measure specifications; examples/default: PM-Spec.R
in the same folder as the script

**ScenarioRoot** - the directory under which to look for scenarios

**Scenarios** - vector of relative paths to scenario ModelState.Rda and Datastore files from which
to extract the measures(each Scenario is sought as subdirectory of ScenarioRoot)

**OutputFile** - (optional) file name in which to put the measures (default is long but informative)
Can be a string or it can contain a "%s" two letter parameter that gets replaced with
"basename(Scenario)

**Years** - which years in the Datastore to process measures for

**Geography** - which geography (Type/Value) to create measures for

**Median Income** - (optional) value from which to build PctMedianIncome for each Household to use in making measures

SpecFile contains a list of specifications for the measures to generate (see the sample file for format)

ScenarioRoot is a path (absolute or relative) to a directory that contains the Scenarios

'Scenarios' is a vector of names of subfolders of 'ScenarioRoot' to be a directory containing a
"Datastore" and "ModelState.Rda" that is a subfolder of where you are running the script
Scenarios <- c("Sample-VERSPM") # could be many
Each scenario gets expanded using file.path(normalizePath(ScenarioRoot),Scenarios[x]) before
processing. You can fake it by making Scenarios a vector of relative paths (relative to
ScenarioRoot), so this mechanism should be general purpose.

outputFile <- "Measures_%scenario%_%years%_%geography%.csv"
  (default is a long-but-informative filename in each Scenario directory; suggest not changing it)

Years <- c("2010","2038")
Which Years to summarize from the Datastore for this Scenario

Geography <- c(Type="Marea",Value="RVMPO") # Can set type to "Azone" or "Bzone" and it probably works
What Geography to perform the summary for
   Type can be Marea, Azone or Bzone
   Value should be the label of one of those geographies)
The Geography should be consistent with the "By" field in PMSpecifications

Comment out the following line if you're not doing measures that use PctMedianIncome

MedianIncome <- 92100
If defined at all, add a "PctMedianIncome" Dataset to the Datastore, based on Income and HhSize
You can use a single value as here, or a vector of different values, indexed by HhSize, or NULL
MedianIncome <- c(44400, 60200, 71300)
                44400 is MFI for 1 person HH, 60200 for 2 person HH, 71300 for 3 person HH
Do the following to generate the MedianIncome for each HhSize from the Datastore
MedianIncome <- NULL # use Datastore by HhSize
If there are fewer MFI's than Household Sizes, the last value is used for all the larger sizes
so providing a single value gives every HH Size the same MFI
92100 default is from
https://www.huduser.gov/portal/datasets/il/il2020/2020MedCalc.odn?inputname=METRO38900M38900*Portland-Vancouver-Hillsboro%2C+OR-WA+MSA&selection_type=hmfa&year=2020&wherefrom=mfi&incpath=%24incpath%24

OPTIONAL: Uncomment the following line and define outputFile
outputFile <- "MyPBOTMeasures.csv"
  (default is a long-but-informative file name in the Scenario directory)

