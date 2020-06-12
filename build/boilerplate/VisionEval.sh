#!/bin/bash

if Rscript VisionEval.R
then
        exec R
else
        cat 1>&2 "Failed to install correctly (R version?)"
fi
