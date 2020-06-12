# VisionEval Installer Configuration

This folder contains example configuration files that are used to build (and rebuild) versions of
VisionEval.  It is feasible to mix and match different code trees (e.g. pulling the framework from
one repository and the models plus modules from another).

This ReadMe.md file documents the structure of the configuration files.

The configuration files are YAML, consisting of Key: Value pairs.

The top-level keys can be anything (The samples contain 'Name', 'Date' and 'Description' for
example), but only the following will be examined during the build:

RunTests
Roots
Components
Locations

The structure of these

## RunTests ##

The "RunTests" key holds a single value, conventionally either TRUE or FALSE.

If TRUE, the build will run any tests configured for the VE Modules.
Any other value is considered FALSE. By default, the Makefile will consult the value set
here in this configuration .yml file. The value set here can be overridden when running `make`
either by setting an environment variable (VE_RUNTESTS) to TRUE or FALSE, or by setting it on the
make command line as `make VE_RUNTESTS=TRUE`.

## Roots ##

In the "Roots" key, each elements is a tag:value pair, where the tag is a symbolic name used
later to define sources and targets for the various VisionEval elements that are processed during
the build.  The value has one of two formats:

1. (simple) an absolute path on the machine that will run the build (in whatever local syntax is
appropriate: Windows or Linux or Macintosh).
1. (compound) a pair of key-value pairs indicated the path (same format as "simple") and the branch
(presuming that the root is a git repository)

The compound format only makes sense for "input" roots (places from which the VisionEval source is
composed). If the branch is specified, the path will be checked during the build to make sure that
it is part of a git repository, and that the currently checked out (local) branch has the indicated
name.  Note that it is the _local_ branch that is comared, not the remote, since the build process
is always relative to the user's build environment.

If you are developing from multiple branches, it is recommended that you use `git worktree` to check
out alternate branches (otherwise, you'll end up rebuilding a bunch of stuff every time you check
out a different branch) and create (as in the included examples) configurations broken out by
branch. The Makefile illustrates how the config files might conventionally be named.

In general, to use a configuration file on a different machine or with a different VisionEval
source tree, it is enough just to change the directory part of each root path to point to
the correct directory (and make sure the branch name is correct if you're using it).

While no root tags are formally required, it is conventional at a minimum to set ve.root to the
folder containing the source code for VisionEval, and ve.output to the folder that will contain the
build artifacts. If ve.root is not defined, it defaults to ".." relative to wherever VE-Installer is
located. If ve.output is not defined, it defaults to the folder "built" relative to the
VE-Installer root.

In order to define Locations, at least one Root tag must be defined (see the "Locations" section
below).

A special root, ve.installer, is always available and is set internally during the build process
to the VE-Installer directory itself.

Alternate roots can be used in the "Components" section (below) to specify different versions of
VisionEval to merge into a single build, and in the "Locations" section to identify where to put the
output. One application of an alternate output location might be to put certain build artifacts
(like the dependency repository) in a different location so they will survive "cleanup" (and thus
effectively be cached).

## Locations ##

The Locations key contains elements that are name:specification pairs identifying the output folders
for each of the build steps.  The following Locations are recognized, and they should each have a
definition (there are no defaults for these).

    - ve.dependencies
    - ve.repository
    - ve.runtime
    - ve.pkgs
    - ve.lib
    - ve.src
    - ve.docs
    - ve.external

In addition, an optional Location `ve.components` can specify an alternate location for
`VE-components.yml` (see below in the "Components" section to learn how that works).

The specification for each Location is a collection of key:value pairs.  At least two key:value pairs
must be defined:

    - root
    - path

Other keys may be defined but they will be ignored.

The `root` key has as its value one of the symbolic names defined (explicitly or implicitly) in the
`Roots` section. It defaults (if not specified) to "ve.output".

The `path` is the directory location for this location relative to the `root`.

## Components ##

The "Components" section is used to mix and match VisionEval components from alternate input root
locations (e.g. a principal VisionEval version in ve.root, and a second repository possibly called
ve.alt.root).

The entire "Components" key is optional. If it is not present, VE-Installer will search for
the file "VE-Components.yml" in a subfolder called "build" of the location specified in "ve.root".

The "Components" section, if present, contains key:value pairs, where the key is one of the root
identifiers (ve.root, ve.alt.root, or whatever) used to furnish part of what will be built into
VisionEval.  The "value" is a sequence of key:value pairs with the following keys:

    - Config (required)
    - Include (optional)
    - Exclude (optional)

The "Config" key has as its value the path (relative to the root of these components) and name of
the "VE-Components.yml" file. By default, that file is presumed to exist in the "build"
sub-directory of ve.root.

The roots are processed in order, with "ve.root" always being processed first.  The "Include"
and "Exclude" options name components found in the corresponding VE-components.yml file for that
root.  Components listed in "Include" are treated as the only elements in that root (even if others
exist).  Components listed in "Exclude" are considered not to exist in that root, and they will
be left out (so you could exclude "VEGUI", for example, to build a version that does not have the
VEGUI scripts in it).

If "Include" and "Exclude" are not used, components from subsequent roots
after ve.root will be added to the list of components to build. If the component has the same name
as a component included from an earlier root, it will replace the component from the earlier root.

So if ve.root and ve.alt.root are both listed, their VE-Component.yml files are the same, and no
"Include" or "Exclude" is provided, then only the versions from ve.alt.root will be used. It will
be as if ve.root was never mentioned! In general it is best to use "Include" and "Exclude" as
necessary to ensure that the correct component versions are built.

Finally, because older versions of VisionEval will not have a VE-Components.yml file, you can create
one yourself and use that instead.  In that case, you do not get to have multiple roots.  Only
"ve.root" is used.  To set up a local VE-Components.yml file, define ve.components in the
"Locations" section.  The "root" element will specify the root (typically "ve.installer"), and the
"path" will list the path relative to root and the filename for "VE-Components.yml". See
VE-config.skeleton.yml for an example of that.


