# Changelog for VisionEval 3.0 "Next Generation" Beta 0.7
# 2022-03-04
# Author: Jeremy Raw <jeremy.raw@dot.gov>

## Notable changes in Beta 0.7

We're getting closer to a releaseable version, pending a bit more documentation. This version
should be usable end-to-end. And it has more useful documentation.

Most of the documentation is still focused on developers / power users (i.e. anyone who is
comfortable pulling the system from the Github). In particular, the `ve.build` world now includes
`ve.test()`. So if you grab the Github, the first thing to do (after you have installed R 4.1.2,
RStudio in some relatively recent edition, and RTools 40) is to open the RStudio project in the
Github root and run `ve.build()`. You'll need an internet connection that can download the
dependency packages. Build takes 45 minutes to an hour and a half, depending on how fast your
computer is.

Once built, you can use "ve.test()" to explore the walkthrough (it has some options - you can load
up either of the framework packages (`visioneval` or `VEModel`) and fiddle with them live, including
running their tests - the promise of the beta release is that if you do `ve.test("VEModel")` you can
run any of the listed test functions and they will work. You can read the source of the test
functions (see "sources/framework/VEModel/tests/test.R" or just edit one of the loaded functiosn) to
see how different things are accomplished.

Running `ve.test()` with no arguments loads the "walkthrough", which lists a set of scripts you can
source to run. You generally won't want to run those. They are heavily commented, and you're better
loading them up into RStudio and running a few lines at a time and then looking closely at what
happens (the comments tell you what to watch for).

The whole walkthrough (probably - this is still a beta) works. Please try it out.

The walkthrough and module test scripts for `visioneval` and `VEModel` are included in the
installer, and you can do "loadTests('VEModel')" to load the test functions and try them out in
the end user runtime. The walkthrough scripts are in the installer runtime as well, in the
"walkthrough" folder - you can source them, or as noted above, load them into RStudio's editor
and try them out. More documentation on that will ultimately be desirable.

## Specific functionality improvements

I found and squashed a number of bugs, some of which were significant. Among other things,
the H5 Datastore format now works again.

"LoadModel" (to copy in a Datastore from another model as a starting point for the current model)
also works again (it stopped working a couple of betas ago as I updated the InputPath and
DatastorePath processing). That's a great feature for development and testing: you just build the
"good stuff" into a model that stops short, then create a second model that sets LoadModel:
firstModel in its configuration. For development purposes, that means you can avoid re-running a
bunch of setup stuff and keep a populated Datastore available to feed the module you're working on.
I'm using this feature to track down some performance issues (see Issue #174 in VisionEval-dev).
See "debugging.R" in the walkthrough for how to set it up.

The model you Load does not have to have run successfully or to completion! So if your model crashes
with some obscure error deep inside one of the modules, you can still Load whatever is left of it
and either run scripts that start from there or extract and query whatever was partially created.

The query functionality now works all the way (probably - this is still a beta). Look a
"Full-Query.VEqry" in any of the test VERSPM variants (the extension is conventional - a
query file is just an R script that defines a QuerySpec object). The "queries.R" walkthrough
explains the structure of the queries and how to run them on your model results. Once you've
configured a query (and it's not hard: you just need to do some book-keeping), it is by far the
easiest way to pull useful information out of VE.

The extraction works better too. There is an example in the walkthrough "extract.R" script of how to
efficiently extract VE results into an external database using R DBI - you don't need to detour
through .csv files to put data into Access or some other database, and you can even load straight
into Excel, though I don't recommend that due to limitations in Excel on the number of rows in a
spreadsheet.

