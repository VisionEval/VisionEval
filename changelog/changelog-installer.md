---
branch: changelog-installer
author: Jeremy Raw (jeremy.raw@dot.gov)
date:   2022-01-31
---

## Overview

Updated Makefile to force shell to /bin/bash.
Also fixed a problem finding ve-lib in the runtime installer's VisionEval.R

## Testing

The bad installer got distributed, so let that be a lesson regarding the importance
(at least) of unzipping and running any built installer prior to uploading.

## Files

The following files were modified in this change:
```
 build/Makefile                         |  2 ++
 changelog/changelog-installer.md (new) | 49 ++++++++++++++++++++++++++++++++++
 sources/runtime/VisionEval.R           | 11 +++-----
 3 files changed, 54 insertions(+), 8 deletions(-)
```
