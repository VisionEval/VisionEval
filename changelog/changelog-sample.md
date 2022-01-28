---
branch: changelog-sample
author: Jeremy Raw (jeremy.raw@dot.gov)
date:   2022-01-28
---

## Overview

This file illustrates a changelog that should be supplied with a pull request. The file includes
header information above, and overview of the changes. In this case, Jeremy's dev-docs was elevated
out of the VEModel package into a sub-directory of this changelog. Those files will eventually go
away in favor of Git Discussions maintained collaboratively online.

## Development notes

There aren't any for this update. But this would be information on key architectural decisions.
Don't reprodudce anything that you already put in the overview. You can leave this section out
if the Overview and Testing say it all.

You might consider including pointers here to additional documentation files that you may have
created for the package that others should be aware of.

## Testing

Describe here how the changes were tested (or should be tested by someone evaluating the pull
request). Look at the current framework packages "visioneval" and "VEModel" in their "tests"
directory for a simple idea: the test_setup.R creates a working runtime environment (based on a
built version of VisionEval development-next) then loads the framework package using the pkgload
package, then makes a set of functions from test.R available. Those tests are also currently
copied into the installed runtime under the "tools" directory where they can be run from a
standard VisionEval installation.

## Files

You can generate a file summary using the following line of code. You should drop the
changelog file from the list manually (since it will always be one commit out of date).

```
git diff --compact-summary development-next
```

The following files were modified in this change:
```
 .../dev-docs-JR}/plan-build.txt                    |  0
 .../dev-docs-JR}/plan-open-items.txt               |  0
 .../dev-docs-JR}/plan-query.txt                    |  0
 .../dev-docs-JR}/plan-scenario.txt                 |  0
 .../dev-docs-JR}/plan-stages.txt                   |  0
 .../dev-docs-JR}/plan-visualizer.txt               |  0
 7 files changed, 37 insertions(+)
```
