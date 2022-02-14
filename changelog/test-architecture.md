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

The configuration process for the VisionEval build has also been updated to make arbitrary test
scripts located (by default) in the package `tests` directory appear in the runtime (in the
`tools/tests/VEPackage` directory).

Extensions to the VE-Components have also been added. In particular, you can add a "Test:" key in the
new format: it is a list of test files that should be copied to the runtime `tools/tests/VEPackage`
location. Here's an example:

```
  visioneval:
    Type: framework
    Path: sources/framework
    Test:
      Group: 0
      Script:
        - test.R                      # bare file is relative to visioneval/tests directory
        - tests/scripts/other_test.R  # path-like file is relative to visioneval package root
    CRAN: 
      - futile.logger
      - jsonlite
      - yaml
      - stringr
      - knitr
    BioC:
      - rhdf5
```

If the package has no runtime tests, leave out the `Test:` tag.

When you build the _runtime_ (not the package itself), the specified tests will be copied over
into the runtime `tools/tests` package directory (`visioneval` in the example above).

### Running tests at runtime.

To load or inspect tests in a built runtime, you can use the new auto-generated runtime tool
function called "loadTest". With no parameters, it lists all the available module tests. With
parameters, it will source the `test.R` files associated with certain packages into an environment
called `ve.tests`. You can select a subset of tests to load for the package, add tests from
another package

Use it like this:

```
loadTest() # lists available test scripts (.R files in "tools\tests")
loadTest("VEModel") # loads all available .R file tests from tools\tests\VEModel
loadTest("VEModel","test.R") # loads just tools/tests\VEModel\test.R
loadTest("VEModel",files="test.R") # same, using named argument (files can be a vector)
loadTest("visioneval") # loads visioneval tests (in addition to VEModel)
                       # Existing tests with same name are overwritten
loadTest("visioneval",clear=TRUE) # removes contents of ve.tests and loads visioneval tests
```

## Creating installable models just for testing

To support really thorough testing and comparison, this pull request implements an update to the
model installation subsystem to allow "hidden" test models to be created and run. It's important to
hide them since in some cases they will deliberately implement "broken" behavior and we don't want
end users to accidentally use those as the basis for their own work.

See the nearby pull request for fixing the IPF bug in VELandUse. You can (and should) use the
elaborate mechanism set up there (including the development of the test.R script with functions
`test_ipf()` and `compare_ipf()`) as prototypes for building thorough tests for module changes that
have potential end-user consequences.

The architecture for installModel has been enhanced to classify models in the `inst/models`
directory of a package to be marked as `private: true` (the default is `false`). Models can also be
sought in a specific package (in addition to the end user default of matching model name and variant
name across all public models in all packages). If `private` models are requested, only models and
variants that are private will be searched, and if a package is also named then the variants will
only be sought in that package.

## Testing

Since this pull request is all about testing, the main way to test it is to see if all the things
described above do what they are supposed to.

## Files

You can generate a file summary using the following line of code.

```
git diff --compact-summary development-next
```

The following files were modified in this change:
```
 build/Makefile                                     |   2 +-
 build/Makefile.md                                  |   2 +-
 build/VisionEval-dev.R                             | 175 +++++++++++++++++---
 build/config/VE-components.yml                     | 180 ++++++++++-----------
 build/scripts/build-config.R                       |  10 +-
 build/scripts/build-modules.R                      |  52 +++---
 build/scripts/build-runtime.R                      |  48 +++++-
 changelog/changelog-sample.md                      |   3 +-
 changelog/test-architecture.md (new)               | 170 +++++++++++++++++++
 launch.bat (new)                                   |   2 +
 sources/framework/VEModel/R/environment.R          |  30 +++-
 sources/framework/VEModel/R/models.R               |  34 ++--
 sources/framework/VEModel/R/results.R              |  15 +-
 .../VEModel/inst/models/VERSPM/model-index.cnf     |   3 -
 .../VERSPM/stage-year-2010/visioneval.cnf (gone)   |   9 --
 .../VERSPM/stage-year-2038/visioneval.cnf (gone)   |  20 ---
 .../VEModel/inst/models/VERSPM/visioneval-year.cnf |  20 ++-
 sources/framework/VEModel/launch.bat               |   3 +-
 sources/framework/VEModel/tests/test.R             | 165 +++++++++++--------
 sources/framework/visioneval/R/datastore.R         |  14 +-
 sources/framework/visioneval/R/visioneval.R        | 110 +++++++++++++
 sources/runtime/VisionEval.R                       |  50 +++++-
 22 files changed, 829 insertions(+), 288 deletions(-)
```
