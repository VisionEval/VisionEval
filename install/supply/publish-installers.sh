#!/bin/bash
# Getting rsync for Git for Windows Bash:
# https://serverfault.com/questions/310337/using-rsync-from-msysgit-for-binary-files

# You'll need to set up the $WWW_SSH_PORT (use 22 if you have no idea)
# and $VE_REPOS as the root of the installation folder
rsync -ravzP --delete -e "ssh -p $WWW_SSH_PORT" $VE_REPOS/install/www/ jeremy@jeremyraw.com:/home/jeremy/Git-Repos/VisionEval-JR/install/www/
