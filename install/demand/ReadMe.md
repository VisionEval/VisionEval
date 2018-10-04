This folder contains the VisionEval folder that is used to run the
system in "end user" mode. This is what gets zipped up into the
installers.  Because the installers are just .zip files the whole
installation is platform-independent.

This directory is almost entirely "built", so it has little in it that
goes into github.

The installer needs to set up the following things:

Start R from this folder - options include RScript, or interactively
starting R, navigating to the folder, and loading the installer .Rdata

Rscript could run a script to load .Rdata

ve.root directory (in .VE-config.Rdata; home of installed version)
ve.library directory ("library" sub-directory of installation directory)

Set up .RProfile with the installed version.  The final .Rprofile will:
	Customize .libPaths (keep the R base library), put VE first
	Set up ve.root and ve.library
	require (visioneval)

install packages (either from local repository in current directory if present or from VE website)


So what's here?

.Rprofile is the magic ticket. Its ".First" function gives the user
instructions.