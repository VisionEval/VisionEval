#!/bin/env bash

# GIT_HOME is where all your repositories live

if [ ! "$(alias config 2>/dev/null)" ]
then
        if [[ $SHELL == *"zsh"* ]]
        then
          echo setting zsh alias # for Macintosh default shell
          BASH_SOURCE="${(%):-%N}"
          realpath() {
            OURPWD=$PWD
            cd "$(dirname "$1")"
            LINK=$(readlink "$(basename "$1")")
            while [ "$LINK" ]; do
              cd "$(dirname "$LINK")"
              LINK=$(readlink "$(basename "$1")")
            done
            REALPATH="$PWD/$(basename "$1")"
            cd "$OURPWD"
            echo "$REALPATH"
          }          
        fi
        THE_SCRIPT=$(realpath $BASH_SOURCE)
        echo "${THE_SCRIPT}"
        alias gitgo="source \"${THE_SCRIPT}\""
fi

unset VE_BRANCH
if [ -z "$1" ]
then
	echo Working Trees in $GIT_HOME
	ls -cd $GIT_HOME/VisionEval-dev*
else
	VE_BRANCH=$(ls -d $GIT_HOME/VisionEval-dev-*$1* 2>&1)

	if [ -d "$VE_BRANCH" ]
	then
		cd $VE_BRANCH
	else
		VE_BRANCH=$(ls -d $GIT_HOME/VisionEval-dev 2>&1)
		echo WARNING: using base branch for VisionEval-dev
		if [ -d "$VE_BRANCH" ]
		then
			cd $VE_BRANCH
		else
			echo Not Found: $VE_BRANCH
		fi
	fi
fi
