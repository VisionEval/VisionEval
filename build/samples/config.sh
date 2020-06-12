#!env bash

# Helper script. Run one time as "source config/config.sh"
# Afterwards, from VE-Installer root, you can use "config dev"
#   to pick and set VE_CONFIG for your build
# It will pick out the first matching VE-config*.yml file
# To permanently add the alias, do alias config >> ~/.profile

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
    alias config="source \"${THE_SCRIPT}\""
fi

if [ -n "$1" ]
then
    export VE_R_VERSION=$2
    echo VE_R_VERSION=${VE_R_VERSION}
else
    echo VE_R_VERSION=${VE_R_VERSION:-default}
fi
