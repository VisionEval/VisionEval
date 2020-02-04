# VE-STATE Tutorial
----

## Strategic Planning Introduction
Strategic planning, often-referred to as scenario planning, is an important part of state and regional government policy-development activities. Recent legislation and Federal rule-making related to performance-based planning, including consideration of the impacts of transportation solutions on future outcomes such as health, mobility, and sustainability, motivates the need for a set of tools that robustly connect policies to outcomes. Plans made today must be resilient in the face of rapidly changing transportation and land use trends due to emerging technologies, social changes, and environmental constraints. The objectives of strategic planning are to consider the prospects of future trends, evaluate the potential for influencing those trends and their effects, and develop a strategic vision for the future. A strategic vision identifies desired performance goals and general strategies for achieving those goals.

## What is VisionEval?
VisionEval is an open source modeling framework intended to address these planning needs. The framework enables statewide, regional, or urban area models to be built by connecting together modules distributed in standard R language packages. A description of the impetus and vision for the framework is included in the repository [here](https://github.com/VisionEval/VisionEval/wiki/documents/VisionEval_OverallVision_20151030.pdf). The framework establishes interface standards for modules and manages the data flow between modules and a common datastore. 
 
Please see [VisionEval.org](http://VisionEval.org) for additional details. You can also go [here](https://github.com/VisionEval/VisionEval/blob/master/api/model_system_design.md) if you would like to learn more about the model framework design.


## VESTATE

VE-STATE is an extension of RSPM models, namely VERSPM, which enables users to apply the model for statewide applications. In order to develop VE-STATE, VE-RSPM modules has been tested and modified to confirm that they will work in a statewide application.
The main difference between VE-RSPM models and VE-STATE models is that a number of VE-RSPM inputs are specified at the Bzone level. Examples include numbers of dwelling units by type and numbers of jobs by sector. VE-STATE models run at a higher level of abstraction than VE-RSPM models and don’t have Bzone level inputs.

## Sections
This tutorial contains the following sections:

* [Model Description](Model_Overview.md): a description of the VESTATE model and a broad comparison with VERSPM model;
* [Modules and Outputs](Modules_and_Outputs.md): a detailed identification Modules and their inputs and outputs
* [Inputs and Parameters](Inputs_and_Parameters.md): a detailed descriptiopn of model inputs modification
* [Running the Model](Running_VESTATE.md): step by step manual to run the model;
* [Performance Metrics](Performance.md): an overview of the model outcomes;

