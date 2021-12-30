## VisionEval: The Next Generation
### Walkthrough Instructions

The VisionEval Next Generation (VNG) `walkthrough` is an interactive tutorial and illustration of
what VNG can do. The `walkthrough.R` script is commented and illustrates how to take advantage
of various new VNG features and functions.

Steps for doing the walkthrough locally:

1. Install VNG from a binary installer, or pull down the github and build VisionEval.

1. Copy this `walkthrough` folder as a sub-directory into your runtime folder (next to `tools` and
`models`). That is the location you unpacked the binary installation, or the
`built/visioneval/4.1.1/runtime` folder in a Github build.

1. If you are putting `walkthrough` anywhere else (or running it within the cloned Github source
tree), set up `R_LIBS_USER` in your `.Renviron` to locate your local visioneval library (`ve-lib`).
If `walkthrough` is a sub-directory of your VisionEval runtime, it will find `ve-lib` automatically.

1 Start R from the walkthrough directory in one of these ways:
  1. Start RGUI from the R version of the runtime and set the working directory to your walkthrough
  1. Make sure your R_HOME variable points to the runtime R version and then run `launch.bat` to
  open a basic R terminal window.
  1. Open the walkthrough.Rproj in RStudio

Once you have your R environment of choice running, you can construct the walkthrough runtime
folder, install and run a "next generation" sample model, and be ready to do the rest of
`walkthrough.R` by running this command from within your walkthrough folder:

```
source("setup.R")
```

Note that that script respects your VE_RUNTIME setting, so it will create the temporary walkthrough
runtime as a sub-directory of whatever you have set for VE_RUNTIME (which defaults to the current
directory). It will also run a sample model so there are pre-built results to use later in the
walkthrough. Even if you're planning to walk through in RStudio, I recommend doing `setup.R` in a
more stripped-down R environment as RStudio is sometimes painfully slow running VisionEval models.

You can then explore the `walkthrough.R` script. I don't recommend running walkthrough.R all at once
- just take a line or two at a time into your console environment and watch what happens. It will
run up to a point if you just source it. But some of what happens in the walkthrough illustrates
error conditions, and you'll need either to trap those to keep going or edit them out of the script.

If you really just want to see things run, visit the "src" folder for VEModel (separate source
installer), or fix up an .Renviron in the Github sources/modules/VEModel folder to find the
built ve-lib, then with R running in the VEModel package root folder do `source("tests/test.R")` and
run one of the "test_*" functions. The `test.R` script is the basis for test-driven development: all
of those tests do what they are supposed to and provide extensive examples of what is possible
within VNG.

The test.R script (plus the corresponding test_run.R from the visioneval package itself) is also
available for inspection (though it won't run right) in the walkthrough/tests directory.
