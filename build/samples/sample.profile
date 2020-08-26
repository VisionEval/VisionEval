# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# R Locations - uncomment and set if R is not located autmoatically
export R_BASE_USER="C:/Users/Jeremy.Raw/R" # seek R-4.0.0 or whatever in that directory...

# Git configuration
export PUTTY_ROOT="C:/Users/jeremy.raw" # Expect to find "Putty" folder here
export GIT_ROOT="C:/Program Files" # Expect to find "git/cmd" folder here

export GIT_PATH=$(cygpath -w -s "${GIT_ROOT}")/git/cmd
export GIT_SSH=$(cygpath -w -s "${PUTTY_ROOT}")/Putty/Plink.exe
export GIT_PAGER=/usr/bin/less

export GIT_HOME=~/Git-Repos # Root for repository clones

# Remember to set core editor
# git config --global core.editor "C:/Users/jeremy.raw/AppData/Local/Programs/Eps14/bin/epsilon.exe -w1"
# Create aliases, if desired, for your favorite editor(s)
alias eps="${HOME}/AppData/Local/Programs/Eps14/bin/epsilon.exe -w1"
alias npp="C:/Program\ Files/Notepad++/notepad++.exe"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
    fi
fi

echo $PATH | grep -q "/git/cmd" || export PATH=$PATH:$GIT_PATH
echo $PATH | grep -iq "Putty" || export PATH=$PATH:~/Putty

ps -eW | grep -iq "pageant" || echo "Better start Pageant!"
plink git@github.com 2>&1 | grep -q successfully || echo "Putty/Plink cannot contact github"

[ -z "$COMPLETION_PATH" ] && {
	# Simple git prompt for MSYS2 standard shell
	parse_git_branch() {
		git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
	}
	PROMPT_USER="\[\033[32m\]\u@\h\[\033[00m\]"
	PROMPT_DIR="\[\033[33m\]\w\[\033[00m\]"
	PROMPT_BRANCH="\[\033[35m\]\$(parse_git_branch)\[\033[00m\]"
	export PS1="$PROMPT_USER $PROMPT_DIR$PROMPT_BRANCH"$'\n$ '
}

alias cdgit="cd \"${GIT_HOME}\""
if [ "$(pwd)" == "${HOME}" ]
then
    cdgit
fi

# Helpful aliases
if [ -d "${GIT_HOME}/VisionEval-dev" ]
then
    alias gitgo="source ${GIT_HOME}/VisionEval-dev/build/samples/gitgo.sh"
fi
  
