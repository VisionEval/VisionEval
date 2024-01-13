# Interacting with VisionEval 3.0

## Introduction

The VisionEval framework interprets a set of files and directories as a model,
often with scenarios defined. VisionEval installation and configuration is
described elsewhere, as are the model structures and recommended procedures for
creating models. This document explains how to:

1.  Open and run a model

2.  Access model results

3.  Use native queries to summarize results

Those operations are currently performed from an R command line, using simple
commands documented below.

A graphical user interface is under development to perform those three types of
operations without interacting directly with R.

### Prerequisites

In order to fully understand running a model and accessing results, it is
important to understand the VisionEval environment, model structure and
configuration. (things like Model Stages, Scenarios, Groups, and Tables). Please
refer to the separate documents on

1.  Installing VisionEval

2.  Structure of a VisionEval Model

3.  Building a VisionEval Model.

## Simple Work Flow

Running a VisionEval model, retrieving results, and generating data summaries
entails the following steps, each of which is explained in detail below.

```R
myModel <- openModel(“myModel”)
myResults <- myModel$run()
myResults$export()
myQuery <- myModel$query(“myQuery”)
mySummary <- myQuery$run()
mySummary$export()
```

Note for total R newbies: the “`<-`“ character sequence is the R assignment
operator. The left had side names an object that receives the results of
performing the operation on the right hand side. In VisionEval, the right hand
side is usually of the form “`object$function()`” which says to perform the
function on the object (using parameters in parentheses).

To run those steps as written, you will need to create “myModel” and also
“myQuery” in the model’s “queries” folder (or in the runtime “queries” folder).
See [Building VisionEval Models](Building-VisionEval-Models) and [Creating and
Using Summary Queries](#using-summary-queries) respectively.

## Detailed Workflow steps

### Locating a Model

Models are opened by default from the “`models`” subdirectory of your VisionEval
runtime directory (See [VisionEval Installation](VisonEval-Installation)). When
VisionEval is first installed, there are no models in that directory.

To add a model for testing or as a basis for setting up your own model, you can
use the `installModel` function. That function installs various simple model
setups in your “models” folder ,and will give you a quick start for laying out
the structure of your own model. `installModel` is described in depth in
[VisionEval Model Setup](VisionEval-Model-Setup).

The examples below presume you have installed the “VERSPM” sample model in its
“base” variant, but you can open any model and follow the same steps. To install
the sample VERSPM model, do this.

```R
myModel <- installModel(“VERSPM”,”base”)
```

You will also need to set up a query specification as described in [Creating and
Using Summary Queries](#using-summary-queries), or you can use the “Full-Query”
delivered with the sample model.

### Opening a Model

#### Simple steps

#### openModel function arguments and alternative behaviors

### Running a Model

#### Simple steps

#### Run model function arguments and alternative behaviors

### Working with Results

#### Simple steps

#### Writing “one liners”:

It is possible to chain operations like this, which can be useful when writing a
short script to perform repeated operations on a model.

```R
openModel(“myModel”)$run()$export()
```

#### Extract and Export functions and their arguments.

#### Connections and partitions

Setting up connections in visioneval.cnf (runtime or model) or on the fly

Partitioning outputs

#### Exporting to other tools not directly supported (\$data)

#### Flattening a Datastore (talk about loading a model in the debugging section)

#### Opening and exporting raw results (e.g. flattened)

### Using Summary Queries

#### Simple Steps

#### Running Queries in detail

By stage or by selecting certain specifications

Cached versus forcing a re-run

### Creating Summary Queries

#### Setting up Queries

#### Quick query formatter function

#### How to write your own query in depth

#### Summarize queries versus Function queries

#### Details of query structure

#### Setting up queries to access extra geography fields

#### Setting Breakpoints and Sub-Categories for summary measures

#### (anything different required for VEPopulationSim fields?)

#### Tableau sample and using the R visualizer
