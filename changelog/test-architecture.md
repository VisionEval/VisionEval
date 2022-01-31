---
branch: test-architecture
author: Jeremy Raw (jeremy.raw@dot.gov)
date:   2022-01-31
---

## Overview

To support unit testing, the build architecture is being revised to allow each VE package to include
a tests directory that contains a file (test.R) of test functions to exercise individual pieces of
the package. Initially, this test architecture will be applied just to the framework packages
(`visioneval` and `VEModel`). The tests can be loaded and run during package development, and can
optionally be specified in `build/config/VE-Components.yml` to be copied into the VisionEval
runtime.

Later, the test architecture will be extended to module pacakges. Note that tests are currently
never performed during the package build process (`ve.build()` or via the `build/Makefile, though
that could change in the future. It is the responsibility of the package maintainer to ensure that
their tests work, and the responsibility of the VisionEval repository maintainers to make sure that
all the tests work before accepting pull requests.

## Development notes / Instructions

Here are some instructions for running tests with the support provided here.

### During package development

Sourcing the file `build/VisionEval-dev.R` will load a function that can be invoked as
`ve.test('VEpackage')` within a fresh R session (i.e. where none of the VisionEval packages are
already loaded). The function will change the working directory to the Github root for the package.
It looks first in "sources/framework" then in "sources/modules", or if the package name contains
path separators ("/" or "\\"), then in that folder relative to the Github root (or possibly an
absolute location in a different place, though that is not recommended).

Then it uses the `pkgload` package to load that package. Other reuired packages are presumed to have
already been built and to be available in .libPaths() (either in an already-built ve-lib or the
dev/lib location for your R version).

The function will look by default for a file called "test.R" in the packages "tests" directory and
source that. An alternate file location can be provided:

```
test_setup("VEpackage",tests="tests.R")`)
test_setup("VEpackage",tests=c("tests.R","otherTests.R")`)
```

The `ve.test` function also has a parameter `localRuntime` which defaults to TRUE. If it is TRUE,
`ve.test` will create a runtime folder within `VEPackage/tests` or use an existing one in that
location. If `localRuntime` is FALSE, ve.test will attempt to use the built runtime (created by
successfully getting through `ve.build()`).

The `tests` argument can be a list of file names. Each element of the `tests` argument is either the
name of a file in the "tests" directory of the package, or if it contains path separators, the
explicitly named file in that alternate directory (relative to the root of VEPackage).

From there, any function that was loaded from `test.R` can be run (ideally exercising the features
of VEPackage).

### Building tests into the runtime

The configuration process for the VisionEval build has also been updated to make the `test.R` file
appear in the runtime (in the `tools/tests/VEPackage` folder for each VEPackage that contains tests).
By default, if the package has a `tests` folder with `test.R`, that file will get copied over to the
runtime `tools/tests`.

Extensions to the VE-Components have also been added. In particular, you can add a "Test:" key in the
new format: it is a list of test files that should be copied to the runtime `tools/tests/VEPackage`
location. Here's an example:

```
  visioneval:
    Type: framework
    Path: sources/framework
    Test:
      - test.R                       # relative to visioneval/tests directory
      - inst/OtherTests/other_test.R # relative to visioneval package root
    CRAN: 
      - futile.logger
      - jsonlite
      - yaml
      - stringr
      - knitr
    BioC:
      - rhdf5
```

If the package has no runtime tests, leave out the `Test:` tag and make sure there is no `test.R`
file in the package "tests" directory.

When you build the _runtime_ (not the package itself), the specified tests will be copied over
into the runtime `tools/tests` package directory (`visioneval` in the example above).

### Running tests at runtime.

To load or inspect tests in a built runtime, just go to the runtime directory, then `source('tools/tests/VEPackage/test.R')`.
The test functions and other objects will be loaded into the current workspace, and can be run from there. The source
code can also be inspected.

## Testing

Since this pull request is all about testing, the main way to test it is to see if we can load and
run the tests during package development and also access the test functions via the `tools` folder
in an installed VisionEval runtime.

## Files

You can generate a file summary using the following line of code. You should drop the
changelog file from the list manually (since it will always be one commit out of date).

```
git diff --compact-summary development-next
```

The following files were modified in this change:
```
```
