---
branch: compound-currency
author: Jeremy Raw (jeremy.raw@dot.gov)
date:   2022-01-28
---

## Overview

This pull request resolves Issue #19 ("Currency data can't be handled as a compound type").

The VERSPM model currently uses simple currency ("USD") even in places where the units obviously
should be USD/YR or USD/PRSN or USD/GAL.

## Items not yet resolved

The various module specifications used in VERSPM still need to be updated to use compound
currency types. A new issue will be created to address that.

## Development notes

Deeper restructuring was required to pull off the conversion successfully. deflateCurrency
was pushed down into convertUnits, which now takes an optional "Years" parameter to handle
currency conversion factors (implicitly, the currency data type has types like "USD.2008" and
"USD.2010" with conversion factors that are defined in defs/deflators.csv (instead of being
built-in for the fixed measures).

Additionally, some specification parsing was also created. Previously, specifications that included
currencies were also required to define the YEAR of currency separately. Now it should be possible
to make the MULTIPLIER and YEAR part of the specification UNITS. So instead of
```
UNITS = "USD/YR"
YEAR = 2010
MULTIPLIER = 1e6
```
you can just write this:
```
UNITS = USD.2010.1e6/YR
```
and the YEAR will become 2010 and the MULTIPLIER will be 1e6 (yielding "Millions of US Dollars per Year"),
and deflation and conversion will be applied correctly.

## Testing

These changes are tested by functions in `visioneval/tests/test.R`, which you can run by changing to
the visioneval package root, opening R (or RStudio) and source'ing visioneval/tests/test_setup.R. You'll
get a list of defined test functions. Alterntively, you can run the tests in a built runtime. Using the
visioneval test_setup.R, the visioneval package is loaded virtually with the R `pkgload` package so you
can easily browse and debug the package scripts.

Various useful hacks are illustrated: for example, setting up necessary parameters in the ModelState_ls
without having to run a model (specifically, defs/units.csv and defs/deflators.csv).

The unit conversion and deflator factors are hard-wired into the tests (not used from any particular
model), based on defaults in the sample VERSPM already

To test the specific changes made here, these tests are appropriate:

```
test_convertUnits()
test_deflateCurrency()
```

After you have run those once, you can interactively try more one-line tests of your own creation.
Remember for convertUnits that if you don't specify the target unit, the target will be the Datastore
unit defined in defs/units.csv.

## Files

 sources/framework/visioneval/R/datastore.R      |  31 +++----
 sources/framework/visioneval/R/initialization.R |   1 +
 sources/framework/visioneval/R/query.R          |   1 +
 sources/framework/visioneval/R/units.R          |  43 ++++++++--
 sources/framework/visioneval/R/validation.R     | 108 +++++++++++++++++++-----
 sources/framework/visioneval/tests/test.R       |  96 +++++++++++++++++++++
 6 files changed, 230 insertions(+), 50 deletions(-)
