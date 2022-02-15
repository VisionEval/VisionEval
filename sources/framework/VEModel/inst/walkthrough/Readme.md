## VisionEval 3.0: The Next Generation

### Walkthrough Instructions

The VisionEval Next Generation (VNG, aka VisionEval 3.0) "walkthrough" is an interactive tutorial
and illustration of what VNG can do. The `walkthrough.R` script is commented and illustrates how to
take advantage the new VNG features and functions.

To use the walkthrough, just start VisionEval in RStudio and navigate to your `runtime/walkthrough`
directory. Open the file called `walkthrough.R` and start working through it. This Readme will
describe the first few steps (which are also described in `walkthrough.R` itself)

### Setting up the Walkthrough

To get going, run the following command, which sets up a special runtime directory and environment
for the walkthrough so it won't accidentally trample your real models.

```
source("setup.R")
```

After doing that, you'll have a folder called something like `walkthrough/runtime409474f46004` (the
stuff after the word `runtime` will be a unique string), and your R working directory will be
set to that location.

You can make the runtime directory go somewhere else if you like. Just set the System environment
variable VE_RUNTIME to the directory of your choice. If you don't know how to do that, it means
that you don't need to! The setup script will still create a temporary subdirectory and use that.

You can then explore the `walkthrough.R` script, which is brokedn into additional topical scripts as
described below

Just take each script a line or two at a time into your console environment and watch what happens.
It will run up to a point if you just source it. But some of what happens in the walkthrough
illustrates error conditions, and you'll need either to catch those to keep going or edit them out
of the script to get it to run all the way through.

## Additional walkthrough sub-scripts to show different features:



## Module tests

If you just want to see that things run, you can explore the `runtime/tools/tests` folder. Look for
one of the packages there, and find the "test.R" script inside its folder. (If you built from Github
and are running RStudio in the Github root, you can also use "ve.test('VEModel')" to try the module
tests, which are the definitive statement of what is supposed to work.

If you are reporting a bug, the best way to do it is to find the test close to what you're trying
to accomplish and adjust the script so that it just reproduces the problem.
