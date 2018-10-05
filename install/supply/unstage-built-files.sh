#!/bin/bash
cd ../.. # presumably back to $REPO_ROOT where we might find sources/modules/packages/data/unwanted.rda
git checkout -- *.rda
