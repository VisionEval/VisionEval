# VisionEval Technical Documentation Map

Separate technical documents describe:

1.  VisionEval Framework Architecture

    -   The “visioneval” core

        -   Architecture

            -   Overview

            -   What happens when a model runs

            -   Support for Building and Debugging

        -   Detailed function reference

        -   Module Construction

            -   Module API

            -   Modules and Packages

            -   The “VESnapshot” module sample package

        -   Visual documentation

        -   Running a model just with visioneval (original version)

    -   The “VEModel” renovation

        -   API

            -   R function documentation

            -   Vignettes / Walkthrough

        -   How the classes and functions are implemented

2.  Building VisionEval (turning framework and module code into a runnable
    application) – documents Github creation and src builds

    -   VE_BUILT

    -   VisionEval plugins from VisionEval-Extras

3.  Installing and Configuring VisionEval

    -   VE_RUNTIME and VE_HOME

    -   Visioneval.cnf at the runtime level

    -   VisionEval GUI

4.  Constructing VisionEval models

    -   Configuring a Model from Geography, Inputs and Scenarios (complete guide
        to visioneval.cnf plus required input structures)

        -   Directory structure (and configuring it)

            -   Classic models

            -   New models

            -   Recommended structure

        -   Scripting a Model

            -   Existing Model Scripts

            -   Module-by-Module documentation

            -   Interrogating structure of a built model

                -   Reading the input files

                -   Using the VE 3.0 user interface to explore

        -   Geography and extra fields, deflators, units, display_units

        -   Inputs and InputPaths, StartFrom and BaseScenario

        -   Scenario subdirectory

        -   Results (including H5/RD, Datastore structure

        -   Queries

        -   Exports

5.  Obtaining and Using Results

    -   Running a Model

        -   Single model stage (or no model stage)

        -   Multiple model stages (Runnable, Reportable)

        -   Stages and Scenarios (accessing)

        -   Run options

    -   Retrieving Results

        -   Results structure (file-system structure, probing into result
            elements with the user interface, what you get when you export)

        -   LoadModel and debugging

        -   Copying results (flattening) and accessing standalone results

        -   Exporter configuration and exporting (+ data access)

            -   Connections

            -   Partitions

            -   Relationship to VE Datastore functions

    -   VisionEval built-in Summary Query system

        -   Queries and Query Specifications

        -   Building and testing
