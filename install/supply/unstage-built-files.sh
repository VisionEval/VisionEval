#!/bin/bash
# run this script from the Repository Root
cd ../.. # presumably back to $REPO_ROOT where we might find sources/modules/packages/data/unwanted.rda
git checkout -- $(find . -name '*.rda' -print)