#!/bin/bash
# Use this in Git for Windows Bash (only - already have suitable rsync on Linux)
# This will download and install an rsync that is compatible with install/supply/publish-miniCRAN.sh

# WARNING: you will need to visit repo.msys2.org/msys and pick the correct architecture
# Almost every serious Windows developer will have a 64-bit architecture, so here goes...
RSYNC_BIN=rsync-3.1.3-1-x86_64.pkg.tar.xz

[ ! -f ${RSYNC_BIN} ] && curl -O http://repo.msys2.org/msys/x86_64/${RSYNC_BIN}
[ -f "${RSYNC_BIN}" ] && tar xvf ${RSYNC_BIN} -C . --strip-components=2 usr/bin/rsync.exe
if [ -f rsync.exe ]
then
    echo Not run: mv rsync.exe /usr/bin
    echo You can manually move "rsync.exe" to /usr/bin within Git Bash for Windows.
else
    echo Unable to extract rsync.exe.  Check error messages
fi
