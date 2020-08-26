
# CalculateSafetyMeasures Module
### June 02, 1992

 This module aims to calculate safety measures for the model area. It uses the fatality and injury crash rates for different modes
 All these rates are per miles traveled. Therefore this module simply uses calculated VMT from model to estimate the 
 fatality and injury rates for Marea
## Model Parameter Estimation

This module has no estimated parameters.

## How the Module Works

This module calculates following metrics:

* * Auto daily fataility and injuries for Urban, Rural and Town areas
* * Bike daily fataility and injuries for Marea
* * Walk daily fataility and injuries for Marea
* * Bus daily fataility and injuries for Marea
* * Rail daily fataility and injuries for Marea
* * Van daily fataility and injuries for Marea



## User Inputs
The following table(s) document each input file that must be provided in order for the module to run correctly. User input files are comma-separated valued (csv) formatted text files. Each row in the table(s) describes a field (column) in the input file. The table names and their meanings are as follows:

NAME - The field (column) name in the input file. Note that if the 'TYPE' is 'currency' the field name must be followed by a period and the year that the currency is denominated in. For example if the NAME is 'HHIncomePC' (household per capita income) and the input values are in 2010 dollars, the field name in the file must be 'HHIncomePC.2010'. The framework uses the embedded date information to convert the currency into base year currency amounts. The user may also embed a magnitude indicator if inputs are in thousand, millions, etc. The VisionEval model system design and users guide should be consulted on how to do that.

TYPE - The data type. The framework uses the type to check units and inputs. The user can generally ignore this, but it is important to know whether the 'TYPE' is 'currency'

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values may not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Value must be one of the listed values.

UNLIKELY - Values that are unlikely. Values that meet any of the listed conditions are permitted but a warning message will be given when the input data are processed.

DESCRIPTION - A description of the data.

### marea_safety_factors.csv
|NAME      |TYPE     |UNITS    |PROHIBIT |ISELEMENTOF |UNLIKELY |DESCRIPTION                                                     |
|:---------|:--------|:--------|:--------|:-----------|:--------|:---------------------------------------------------------------|
|Geo       |         |         |         |Mareas      |         |Must contain a record for each Marea and model run year.        |
|Year      |         |         |         |            |         |Must contain a record for each Marea and model run year.        |
|AutoFatal |compound |CRASH/MI |< 0      |            |         |Number of fatal auto crashes per 100 millions of mile traveled  |
|AutoInjur |compound |CRASH/MI |< 0      |            |         |Number of injury auto crashes per 100 millions of mile traveled |
|BikeFatal |compound |CRASH/MI |< 0      |            |         |Number of fatal bike crashes per 100 millions of mile traveled  |
|BikeInjur |compound |CRASH/MI |< 0      |            |         |Number of injury bike crashes per 100 millions of mile traveled |
|WalkFatal |compound |CRASH/MI |< 0      |            |         |Number of fatal walk crashes per 100 millions of mile traveled  |
|WalkInjur |compound |CRASH/MI |< 0      |            |         |Number of injuy walk crashes per 100 millions of mile traveled  |
|BusFatal  |compound |CRASH/MI |< 0      |            |         |Number of fatal bus crashes per 1 millions of mile traveled     |
|BusInjur  |compound |CRASH/MI |< 0      |            |         |Number of injury bus crashes per 1 millions of mile traveled    |
|RailFatal |compound |CRASH/MI |< 0      |            |         |Number of fatal rail crashes per 1 millions of mile traveled    |
|RailInjur |compound |CRASH/MI |< 0      |            |         |Number of injury rail crashes per 1 millions of mile traveled   |

## Datasets Used by the Module
The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:

NAME - The dataset name.

TABLE - The table in the datastore that the data is retrieved from.

GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year group. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.

TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.

|NAME               |TABLE     |GROUP |TYPE      |UNITS    |PROHIBIT |ISELEMENTOF        |
|:------------------|:---------|:-----|:---------|:--------|:--------|:------------------|
|Marea              |Marea     |Year  |character |ID       |         |                   |
|VanDvmt            |Marea     |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|BusDvmt            |Marea     |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|RailDvmt           |Marea     |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|UrbanHhDvmt        |Marea     |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|RuralHhDvmt        |Marea     |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|TownHhDvmt         |Marea     |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|AutoFatal          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|AutoInjur          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|BikeFatal          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|BikeInjur          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|WalkFatal          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|WalkInjur          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|BusFatal           |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|BusInjur           |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|RailFatal          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|RailInjur          |Marea     |Year  |compound  |CRASH/MI |NA, < 0  |                   |
|Marea              |Household |Year  |character |ID       |         |                   |
|LocType            |Household |Year  |character |category |NA       |Urban, Town, Rural |
|Dvmt               |Household |Year  |compound  |MI/DAY   |NA, < 0  |                   |
|WalkTrips          |Household |Year  |compound  |TRIP/DAY |NA, < 0  |                   |
|BikeTrips          |Household |Year  |compound  |TRIP/DAY |NA, < 0  |                   |
|TransitTrips       |Household |Year  |compound  |TRIP/DAY |NA, < 0  |                   |
|WalkAvgTripDist    |Household |Year  |double    |MI       |NA, < 0  |                   |
|BikeAvgTripDist    |Household |Year  |double    |MI       |NA, < 0  |                   |
|TransitAvgTripDist |Household |Year  |double    |MI       |NA, < 0  |                   |

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

|NAME                 |TABLE |GROUP |TYPE   |UNITS |PROHIBIT |ISELEMENTOF |DESCRIPTION                                        |
|:--------------------|:-----|:-----|:------|:-----|:--------|:-----------|:--------------------------------------------------|
|AutoFatalCrashRural  |Marea |Year  |double |CRASH |< 0      |            |Number of yearly atuo fatal crashes in Rural area. |
|AutoFatalCrashUrban  |Marea |Year  |double |CRASH |< 0      |            |Number of yearly auto injury crashes in Rural area |
|AutoFatalCrashTown   |Marea |Year  |double |CRASH |< 0      |            |Number of yearly atuo fatal crashes in Urban area. |
|AutoInjuryCrashRural |Marea |Year  |double |CRASH |< 0      |            |Number of yearly auto injury crashes in Urabn area |
|AutoInjuryCrashUrban |Marea |Year  |double |CRASH |< 0      |            |Number of yearly atuo fatal crashes in Town area.  |
|AutoInjuryCrashTown  |Marea |Year  |double |CRASH |< 0      |            |Number of yearly auto injury crashes in Town area  |
|WalkFatalCrash       |Marea |Year  |double |CRASH |< 0      |            |Number of yearly walk fatal crashes in Marea.      |
|WalkInjuryCrash      |Marea |Year  |double |CRASH |< 0      |            |Number of yearly walk injury crashes in Marea.     |
|BikeFatalCrash       |Marea |Year  |double |CRASH |< 0      |            |Number of yearly bike fatal crashes in Marea.      |
|BikeInjuryCrash      |Marea |Year  |double |CRASH |< 0      |            |Number of yearly bike injury crashes in Marea.     |
|BusFatalCrash        |Marea |Year  |double |CRASH |< 0      |            |Number of yearly bus fatal crashes in Marea.       |
|BusInjuryCrash       |Marea |Year  |double |CRASH |< 0      |            |Number of yearly bus injury crashes in Marea.      |
|RailFatalCrash       |Marea |Year  |double |CRASH |< 0      |            |Number of yearly rail fatal crashes in Marea.      |
|RailInjuryCrash      |Marea |Year  |double |CRASH |< 0      |            |Number of yearly rail injury crashes in Marea.     |
|VanFatalCrash        |Marea |Year  |double |CRASH |< 0      |            |Number of yearly van fatal crashes in Marea.       |
|VanInjuryCrash       |Marea |Year  |double |CRASH |< 0      |            |Number of yearly van injury crashes in Marea.      |
