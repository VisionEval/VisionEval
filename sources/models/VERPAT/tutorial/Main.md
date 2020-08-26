This tutorial describes the purpose, structure, and use of VERPAT.

### Strategic Planning Introduction

Strategic planning, also known as scenario planning, is becoming increasingly important as a means to help state and metropolitan area governments select policies and actions to address pressing issues that are fraught with uncertainty. The federal government increasingly challenges state, regional and local transportation agencies to measure outcomes of decisions through performance-based planning, including consideration of the impacts of transportation solutions on future outcomes such as health, mobility, and sustainability.  Further complicating matters, plans must be resilient in the face of rapidly changing transportation and land use trends due to emerging technologies, social changes, and environmental constraints.  The objectives of strategic planning are to consider the prospects of future trends, evaluate the potential for influencing those trends and their effects, and develop a strategic vision for the future. A strategic vision identifies desired performance goals and general strategies for achieving those goals. For more background on strategic planning, see [[Scenario Planning | VERPAT-Tutorial-Scenario-Planning]].

### What is VisionEval?

VisionEval, an open source common modeling framework built on the successful GreenSTEP family of strategic planning tools, is intended to address these planning needs.  The framework enables statewide, regional, or urban area models to be built by connecting together modules distributed in standard R language packages. The framework establishes interface standards for modules and manages the data flow between modules and a common datastore. This [repository](https://github.com/visioneval/VisionEval) is the source for project documentation and code. A description of the impetus and vision for the framework is included in the repository [here](https://github.com/visioneval/VisionEval/wiki/documents/VisionEval_OverallVision_20151030.pdf).

### VERPAT development history

The family of VisionEval tools began with a tool designed to measure GHG emission outcomes in Oregon (GreenSTEP). It was operationalized and rebranded as the Energy and Emissions Reduction Policy Analysis Tool (EERPAT) with funding from the Federal Highway Administration (FHWA) for use by other state transportation agencies. GreenSTEP was later modified for regional transportation agencies with broader measures such as health, household costs, and other outcomes and called the Regional Strategic Planning Model (RSPM). GreenStep was also adapted to incorporate smart growth policies at a regional scale, called the Rapid Policy Assessment Tool (RPAT).  RPAT was developed under the 2nd Strategic Highway Research Program as part of the federal SAFETEA-LU legislation. The model was developed to help planners evaluate the potential effect of smart growth policies on regional travel. Portions of the GreenSTEP model were used in RPAT, but substantial revisions were made to the code. 

VERPAT is a further development of RPAT, implemented within the VisionEval framework. Like RPAT, VERPAT evaluates policy scenarios to identify the most promising policies that can be further tested using a more detailed project-level tool. Specifically, VERPAT is designed to evaluate transportation investment and policy strategies such as increased transit service, pricing, parking management, etc. combined with land use plans to assess how different combinations of investment and policy actions impact travel behavior and budgets and thus influence desired community outcomes and goals. The short runtimes enable thousands of scenarios to be evaluated testing plan resilience and has resulted in several web-based interactive visualization tools to explore policy/investment trade-offs with stakeholders and the public.  For more on how the VERPAT model works see [[Model Description | VERPAT-Tutorial-Model-Description]].  

## Sections
To learn how to install and run VERPAT for a single scenario, see [[Running the Model | VERPAT-Tutorial-Running-the-Model]], and to learn how to run VERPAT for multiple scenarios implementing combinations of policy options, see [[Multiple Scenarios | VERPAT-Tutorial-Multiple-Scenarios]].  Finally, the section [[Performance Metrics | VERPAT-Tutorial-Performance-Metrics]] describes the output from VERPAT.

  * [Scenario Planning](Scenario_Planning.md): a quick introduction to scenario planning.
  * [Model Description](Model_Description.md): a description of the VERPAT model.
  * [Running the Model](Running_VERPAT.md): how to install and run the model.
  * [Multiple Scenarios](Multiple_Scenarios.md): how to setup and run multiple model scenarios.
  * [Performance Metrics](Performance_Metrics.md): model outcomes of particular interest.
  * [Inputs and Outputs](Input_Outputs.md): detailed list of model inputs and outputs.

## Notes
Source for images is [documents/verpat_tutorial.pptx](documents/verpat_tutorial_images.pptx)
