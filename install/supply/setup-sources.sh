#/bin/bash
# Copy sources for models and GUIs to the "demand" staging area

# This uses rsync, which you can obtain for Git for Windows Bash
# using a script located in the "install/external" folder.

# Note that logs, etc. will be deleted from demand/models

rsync -ravzP --delete ../../sources/models ../demand/
rsync -ravzP --delete ../../sources/VEGUI  ../demand/