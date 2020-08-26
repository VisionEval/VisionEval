# VisionEval

VisionEval is a model system and supporting software framework for building collaborative disaggregate strategic planning models.  

## Documentation

[https://github.com/VisionEval/VisionEval/wiki](https://github.com/VisionEval/VisionEval/wiki)

## Building / Installing

You are looking at a code branch of VisionEval, which contains all you need to modify and rebuild
the VisionEval system.

Pre-built binary installers of recently released versions (the "master" branch) are available at
[https://visioneval.org](https://visioneval.org).

You can install directly from a copy (.zip) or clone of this VisionEval repository branch, using the
instructions in the `build/Building.md` file on a VisionEval code branch.

## Documentation

Instructions for running VisionEval once it is built or installed from a .zip installer can be found
in `sources/Getting-Started.md`.

After you have built VisionEval docs (by running `ve.build("docs")`, a full set of PDF-format
documentation is available in the `docs` folder. Because some of the documentation is generated when
VisionEval code is built, building the documentation implies building the complete runtime system.

The default location for the built docs is `built/visioneval/docs`, or if you make a binary
installer you will find `docs` in the runtime folder.


