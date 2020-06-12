
# CalculateAltModeTrips Module
### March 2020

This module predicts:
 1- transit PMT for households. It uses the model object in data/TransitPMTModel_df.rda and variables and coefficients therein to predict TransitPMT.
 2- transit trip frequency and average trip length for households. It uses the model object in data/TransitTFLModel_df.rda and variables and coefficients therein to predict TransitTFL.
 3- walking PMT for households. It uses the model object in data/WalkPMTModel_df.rda and variables and coefficients therein to predict WalkPMT.
 4- walking trip frequency and average walking trip length for households. It uses the model object in data/WalkTFLModel_df.rda and variables and coefficients therein to predict WalkTFL.
 5- biking PMT for households. It uses the model object in data/BikePMTModel_df.rda and variables and coefficients therein to predict BikePMT.
 6- trip frequency (BikeTrips) and average trip length (BikeAvgTripDist) for households. It uses the model object in data/BikeTFLModel_df.rda and variables and coefficients therein to predict BikeTFL.


## Model Parameter Estimation
See:
 data-raw/TransitPMTModel_df.R
 data-raw/TransitTFLModel_df.R
 data-raw/WalkPMTModel_df.R
 data-raw/WalkTFLModel_df.R
 data-raw/BikePMTModel_df.R
 data-raw/BikeTFLModel_df.R

## How the Module Works

The user specifies the model in the estimation scripts in data-raw folder and saves the estimation results in data folder. If no model re-estimation is desired, the estimation process can be skipped and the default model specification is then used. The module assigns the alternative mode PMT and TFL to each household using variables including household characteristics, built environment, and transportation supply.


## User Inputs
The following table(s) document each input file that must be provided in order for the module to run correctly. User input files are comma-separated valued (csv) formatted text files. Each row in the table(s) describes a field (column) in the input file. The table names and their meanings are as follows:

NAME - The field (column) name in the input file. Note that if the 'TYPE' is 'currency' the field name must be followed by a period and the year that the currency is denominated in. For example if the NAME is 'HHIncomePC' (household per capita income) and the input values are in 2010 dollars, the field name in the file must be 'HHIncomePC.2010'. The framework uses the embedded date information to convert the currency into base year currency amounts. The user may also embed a magnitude indicator if inputs are in thousand, millions, etc. The VisionEval model system design and users guide should be consulted on how to do that.

TYPE - The data type. The framework uses the type to check units and inputs. The user can generally ignore this, but it is important to know whether the 'TYPE' is 'currency'

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values may not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Value must be one of the listed values.

UNLIKELY - Values that are unlikely. Values that meet any of the listed conditions are permitted but a warning message will be given when the input data are processed.

DESCRIPTION - A description of the data.

### marea_census_r.csv
|NAME     |TYPE      |UNITS    |PROHIBIT |ISELEMENTOF  |UNLIKELY |DESCRIPTION                                              |
|:--------|:---------|:--------|:--------|:------------|:--------|:--------------------------------------------------------|
|Geo      |          |         |         |Mareas       |         |Must contain a record for each Marea and model run year. |
|Year     |          |         |         |             |         |Must contain a record for each Marea and model run year. |
|CENSUS_R |character |category |         |NE, S, W, MW |         |CENSUS_R                                                 |

## Datasets Used by the Module
The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:

NAME - The dataset name.

TABLE - The table in the datastore that the data is retrieved from.

GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year group. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.

TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.

|NAME        |TABLE     |GROUP |TYPE      |UNITS                                             |PROHIBIT |ISELEMENTOF                |
|:-----------|:---------|:-----|:---------|:-------------------------------------------------|:--------|:--------------------------|
|HhId        |Household |Year  |character |ID                                                |         |                           |
|Dvmt        |Household |Year  |compound  |MI/DAY                                            |NA, < 0  |                           |
|HhSize      |Household |Year  |people    |PRSN                                              |NA, < 0  |                           |
|Workers     |Household |Year  |people    |PRSN                                              |NA, < 0  |                           |
|Drivers     |Household |Year  |people    |PRSN                                              |NA, < 0  |                           |
|Age0to14    |Household |Year  |people    |PRSN                                              |NA, < 0  |                           |
|Age65Plus   |Household |Year  |people    |PRSN                                              |NA, < 0  |                           |
|Income      |Household |Year  |currency  |USD.2009                                          |NA, < 0  |                           |
|LifeCycle   |Household |Year  |character |category                                          |         |00, 01, 02, 03, 04, 09, 10 |
|Vehicles    |Household |Year  |vehicles  |VEH                                               |NA, < 0  |                           |
|Bzone       |Household |Year  |character |none                                              |         |                           |
|LocType     |Household |Year  |character |category                                          |NA       |Urban, Town, Rural         |
|Bzone       |Bzone     |Year  |character |none                                              |         |                           |
|D1B         |Bzone     |Year  |compound  |PRSN/ACRE                                         |NA, < 0  |                           |
|D2A_WRKEMP  |Bzone     |Year  |compound  |PRSN/JOB                                          |NA, < 0  |                           |
|D2A_EPHHM   |Bzone     |Year  |double    |employment & household entropy                    |NA, < 0  |                           |
|D3bpo4      |Bzone     |Year  |double    |pedestrian-oriented intersections per square mile |NA       |                           |
|D4c         |Bzone     |Year  |double    |aggregate peak period transit service             |NA, < 0  |                           |
|D5          |Bzone     |Year  |double    |NA                                                |NA, < 0  |                           |
|Marea       |Marea     |Year  |character |none                                              |         |                           |
|CENSUS_R    |Marea     |Year  |character |category                                          |         |NE, S, W, MW               |
|FwyLaneMiPC |Marea     |Year  |compound  |MI/PRSN                                           |NA, < 0  |                           |
|TranRevMiPC |Marea     |Year  |compound  |MI/PRSN                                           |NA, < 0  |                           |

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

|NAME               |TABLE     |GROUP |TYPE     |UNITS     |PROHIBIT |ISELEMENTOF |DESCRIPTION                                                          |
|:------------------|:---------|:-----|:--------|:---------|:--------|:-----------|:--------------------------------------------------------------------|
|WalkTrips          |Household |Year  |compound |TRIP/YEAR |NA, < 0  |            |Average number of walk trips per year by household members           |
|BikeTrips          |Household |Year  |compound |TRIP/YEAR |NA, < 0  |            |Average number of bicycle trips per year by household members        |
|TransitTrips       |Household |Year  |compound |TRIP/YEAR |NA, < 0  |            |Average number of public transit trips per year by household members |
|WalkAvgTripDist    |Household |Year  |double   |MI        |NA, < 0  |            |Daily walking average trip length                                    |
|BikeAvgTripDist    |Household |Year  |double   |MI        |NA, < 0  |            |Daily biking average trip length                                     |
|TransitAvgTripDist |Household |Year  |double   |MI        |NA, < 0  |            |Daily transit average trip length                                    |
|WalkPMT            |Household |Year  |distance |MI        |NA, < 0  |            |Daily walking person miles traveled by all members of the household  |
|BikePMT            |Household |Year  |distance |MI        |NA, < 0  |            |Daily biking person miles traveled by all members of the household   |
|TransitPMT         |Household |Year  |distance |MI        |NA, < 0  |            |Daily transit person miles traveled by all members of the household  |
