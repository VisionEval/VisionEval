# Boilerplat Folder

The `boilerplate` consists of scripts that are copied into the runtime
root folder.  The scripts will install the runtime library for the current
platform and set up an .RData file the loads the VisionEval framework.

The `build-runtime.R` in the `scripts` directory simply copies these
into the runtime root where they can be used to activate the
developer's built runtime environment, or zipped up into both the
online and offline installers.
