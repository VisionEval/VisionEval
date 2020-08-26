# VERSPM Tutorial
----

## Strategic Planning Introduction
Strategic planning, often-referred to as scenario planning, is an important part of state and regional government policy-development activities. Recent legislation and Federal rule-making related to performance-based planning, including consideration of the impacts of transportation solutions on future outcomes such as health, mobility, and sustainability, motivates the need for a set of tools that robustly connect policies to outcomes. Plans made today must be resilient in the face of rapidly changing transportation and land use trends due to emerging technologies, social changes, and environmental constraints. The objectives of strategic planning are to consider the prospects of future trends, evaluate the potential for influencing those trends and their effects, and develop a strategic vision for the future. A strategic vision identifies desired performance goals and general strategies for achieving those goals.

## What is VisionEval?
VisionEval is an open source modeling framework intended to address these planning needs. The framework enables statewide, regional, or urban area models to be built by connecting together modules distributed in standard R language packages. A description of the impetus and vision for the framework is included in the repository [here](https://github.com/VisionEval/VisionEval/wiki/documents/VisionEval_OverallVision_20151030.pdf). The framework establishes interface standards for modules and manages the data flow between modules and a common datastore. 
 
Please see [VisionEval.org](http://VisionEval.org) for additional details. You can also go [here](https://github.com/VisionEval/VisionEval/blob/master/api/model_system_design.md) if you would like to learn more about the model framework design.

## What is RSPM?
GreenSTEP was later modified for regional transportation agencies with broader measures such as health, household costs, and other outcomes and called the Regional Strategic Planning Model (RSPM).

## VERSPM
VERSPM is the implementation of the Regional Strategic Planning Model (RSPM) in the VisionEval model system. The model run files are organized into two directories to reflect how the RSPM model has been used in strategic assessments in which hundreds of model runs are carried out to explore the decision space. Typically several land use scenarios are developed and then a number of transportation scenarios are run for each land use scenarios. While the VisionEval framework is built to support that work flow, the initial testing of VERSPM modules runs all land use and transport modules in one sequence. This is done in the `../models/VERSPM/Test1` directory.

Later testing will split apart land use and transport scenarios, splitting them into separate directories. VERSPM  operates largely at the zonal level in a manner similar to traditional travel model zones (i.e. inputs and outputs are zone-based). This results in improved spatial resolution of outputs, but which also increases the data development burden. The selection of the right tool therefore depends on a number of factors – available of data, project resources, desired spatial output detail, etc.

## Sections
This tutorial contains the following sections:

* [Model Description](Model_Overview.md): a description of the VERSPM model;
* [Modules and Outputs](Modules_and_Outputs.md): a detailed identification Modules and their inputs and outputs
* [Inputs and Parameters](Inputs_and_Parameters.md): a detailed descriptiopn of model inputs modification
* [Running the Model](Running_VERSPM.md): step by step manual to run the model;
* [Performance Metrics](Performance.md): an overview of the model outcomes;

