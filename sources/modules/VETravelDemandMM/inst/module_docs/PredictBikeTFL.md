
# PredictBikeTFL Module
### January 4, 2019

This module predicts trip frequency (BikeTrips) and average trip length (BikeAvgTripDist) for households. It uses the model object in data/BikeTFLModel_df.rda and variables and coefficients therein to predict BikeTFL.

## Model Parameter Estimation

See data-raw/BikeTFLModel_df.R.

## How the Module Works

The user specifies the model in data-raw/BikeTFLModel_df.R and saves the estimation results in data/BikeTFLModel_df.rda. If no model re-estimation is desired, the estimation process can be skipped. The module assigns BikeTFL to each household using variables including household characteristics, built environment, and transportation supply.


## User Inputs
This module has no user input requirements.

## Datasets Used by the Module
The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:

NAME - The dataset name.

TABLE - The table in the datastore that the data is retrieved from.

GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year group. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.

TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.

|NAME      |TABLE     |GROUP |TYPE      |UNITS    |PROHIBIT |ISELEMENTOF        |
|:---------|:---------|:-----|:---------|:--------|:--------|:------------------|
|HHSIZE    |Household |Year  |integer   |PRSN     |NA, < 0  |                   |
|Workers   |Household |Year  |integer   |PRSN     |NA, < 0  |                   |
|Age65Plus |Household |Year  |integer   |PRSN     |NA, < 0  |                   |
|Income    |Household |Year  |currency  |USD.2009 |NA, < 0  |                   |
|LocType   |Household |Year  |character |category |NA       |Urban, Town, Rural |
|CENSUS_R  |Marea     |Year  |character |category |         |NE, S, W, MW       |
|TRPOPDEN  |Bzone     |Year  |compound  |PRSN/SQM |NA, < 0  |                   |
|ZeroVeh   |Household |Year  |integer   |none     |NA, < 0  |                   |

## Datasets Produced by the Module
The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:

NAME - The dataset name.

TABLE - The table in the datastore that the data is retrieved from.

GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.

TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.

DESCRIPTION - A description of the data.

|NAME            |TABLE     |GROUP |TYPE    |UNITS |PROHIBIT |ISELEMENTOF |DESCRIPTION                      |
|:---------------|:---------|:-----|:-------|:-----|:--------|:-----------|:--------------------------------|
|BikeTrips       |Household |Year  |integer |trips |NA, < 0  |            |Daily biking trip frequency      |
|BikeAvgTripDist |Household |Year  |integer |mile  |NA, < 0  |            |Daily biking average trip length |
|HhId            |Household |Year  |integer |ID    |NA, < 0  |            |HouseholdID                      |
