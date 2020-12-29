# VERSPM Inputs and Parameters

----

VERSPM data inputs are classified into two categories: Inputs and Model Parameters. It includes five definition files and dozens of input files, some of which the user must change and others which typically remain unchanged. This page provides guidance of these files to the end user and which files must be updated to implement RSPM in a new region.


## Model Definition Files

The following five files need to be configured in the "defs" directory:

- [run_parameters.json](#run_parametersjson)
- [model_parameters.json](#model_parametersjson)
- [deflators.csv](#deflatorscsv)
- [geo.csv](#geocsv)
- [units.csv](#unitscsv)

### run_parameters.json

This file contains parameters that define key attributes of the model run and relationships to other model runs. A more detailed description of the file can be found [here](https://github.com/gregorbj/VisionEval/blob/master/api/model_system_design.md#61-model-directory-structure). The results of model run are stored in a directory with the name specified by ```"DatastoreName"```. This name should be changed when running different scenarios. For e.g. when running base scenario the output directory name can be set to *BaseScenario* by using ```"DatastoreName": "BaseScenario"``` in the file. The format of this file is as follows:

```json
{
    "Model": "VERSPM",
    "Scenario": "Test",
    "Description": "Test of VERSPM",
    "Region": "RVMPO",
    "BaseYear": "2010",
    "Years": ["2010", "2038"],
    "DatastoreName": "Datastore",
    "DatastoreType": "RD",
    "Seed": 1
}
```

[Top](#rspm-inputs-and-parameters)
___

### model_parameters.json

This file contains global parameters for a particular model configuration that may be used by multiple modules. A more detailed description of the file and its structure can be found [here](https://github.com/gregorbj/VisionEval/blob/master/api/model_system_design.md#61-model-directory-structure). The source of the default $16/hr is per a Nov 2016 ODOT Report: ["The Value of Travel-Time: Estimates of the Hourly Value of Time for Vehicles in Oregon"](https://www.oregon.gov/ODOT/Data/Documents/Value-of-Travel-Time-for-Vehicles.pdf).

The format of this file is as follows:

```json
[
  {"NAME": "ValueOfTime", 
   "VALUE": "16", 
   "TYPE": "double", 
   "UNITS": "base cost year dollars per hour"
  }
]
```

[Top](#rspm-inputs-and-parameters)
___

### deflators.csv

This file defines the annual deflator values, such as the consumer price index, that are used to convert currency values between different years for currency denomination. This file does not need to be modified unless the years for which the dollar values used in the input dataset is not contained in this file. The format of the file is as follows:

|              Year          |             Value          |
|----------------------------|----------------------------|
|              1999          |             172.6          |
|              2000          |             178.0          |
|              2001          |             182.4          |
|              ...           |             ...            |
|              2010          |             218.344        |
|              ...           |             ...            |
|              2016          |             249.426        |

[Top](#rspm-inputs-and-parameters)
___

### geo.csv

This file describes all of the geographic relationships for the model and the names of geographic entities in a CSV formatted text file. [**Azone**](https://github.com/gregorbj/VisionEval/blob/master/api/model_system_design.md#62-model-geography), [**Bzone**](https://github.com/gregorbj/VisionEval/blob/master/api/model_system_design.md#62-model-geography), and [**Marea**](https://github.com/gregorbj/VisionEval/blob/master/api/model_system_design.md#62-model-geography) should remain consistent with the input data. The format of the file is as follows:

| Azone       | Bzone          | Czone      | Marea      |
| ----------- | -------------- | ---------- | ---------- |
| RVMPO       | D410290001001  | NA         | RVMPO      |
| RVMPO       | D410290001002  | NA         | RVMPO      |
| RVMPO       | D410290002011  | NA         | RVMPO      |
| RVMPO       | D410290002012  | NA         | RVMPO      |
| RVMPO       | D410290002013  | NA         | RVMPO      |
| RVMPO       | D410290002021  | NA         | RVMPO      |
| RVMPO       | D410290002022  | NA         | RVMPO      |
| RVMPO       | D410290002023  | NA         | RVMPO      |
| RVMPO       | D410290002031  | NA         | RVMPO      |
| RVMPO       | D410290002032  | NA         | RVMPO      |
| RVMPO       | D410290002033  | NA         | RVMPO      |
| RVMPO       | D410290003001  | NA         | RVMPO      |
| RVMPO       | ...            | NA         | RVMPO      |

To learn more about VERSPM model geographic relationships see [here](docs/VisionEval_Inputs_by_Geo.docx)

[Top](#rspm-inputs-and-parameters)
___

## units.csv

This file describes the default units to be used for storing complex data types in the model. This file should NOT be modified by the user. The format of the file is as follows:

| Type                 | Units            |
| -------------------- | ---------------- |
| currency             | USD              |
| distance             | MI               |
| area                 | SQMI             |
| mass                 | KG               |
| volume               | GAL              |
| time                 | DAY              |
| energy               | GGE              |
| people               | PRSN             |
| trips                | VEH              |
| area                 | TRIP             |
| households           | HH               |
| employment           | JOB              |
| activity             | HHJOB            |

The VisionEval model system keeps track of the types and units of measure of all data that is processed. More details about the file and structure can be found [here](https://github.com/gregorbj/VisionEval/blob/master/api/model_system_design.md#63-data-types-units-and-currency-deflators).

[Top](#rspm-inputs-and-parameters)
___


## Input Files

- **azone_carsvc_characteristics.csv** This file specifies the different characteristics for high and low car service level and is used in the [CreateVehicleTable](Modules_and_Outputs.md/#createvehicletable) and [AssignVehicleAge](Modules_and_Outputs.md/#assignvehicleage) modules. 
- **azone_charging_availability.csv** This file has data on proportion of different household types who has EV charging available  and is used in the [AssignHHVehiclePowertrain](Modules_and_Outputs.md/#assignhhvehiclepowertrain) module.          
- **azone_electricity_carbon_intensity.csv** This file is used to specify the carbon intensity of electricity and is optional (only needed if user wants to modify the values). The file is used in [Initialize (VEPowertrainsAndFuels)](Modules_and_Outputs.md/#initialize-vepowertrainsandfuels
) and [CalculateCarbonIntensity](Modules_and_Outputs.md/#calculatecarbonintensity) modules.      
- **azone_fuel_power_cost.csv** This file supplies data for retail cost of fuel and electricity and is used in the [CalculateVehicleOperatingCost](Modules_and_Outputs.md/#calculatevehicleoperatingcost) module.                 
- **azone_gq_pop_by_age.csv** This file contains group quarters population estimates/forecasts by age and is used in the [CreateHouseholds](Modules_and_Outputs.md/#createhouseholds) module.                    
- **azone_hh_pop_by_age.csv** This file contains population estimates/forecasts by age and is used in the [CreateHouseholds](Modules_and_Outputs.md/#createhouseholds) module.                  
- **azone_hh_veh_mean_age.csv** This file provides inputs for mean auto age and mean light truck age and is used in the [AssignVehicleAge](Modules_and_Outputs.md/#assignvehicleage) module.                 
- **azone_hh_veh_own_taxes.csv** This file provides inputs for flat fees/taxes (i.e. annual cost per vehicle) and ad valorem taxes (i.e. percentage of vehicle value paid in taxes). The file is used in [CalculateVehicleOwnCost](Modules_and_Outputs.md/#calculatevehicleowncost) module.                 
- **azone_hhsize_targets.csv** This file contains the household specific targets and is used in [CreateHouseholds](Modules_and_Outputs.md/#createhouseholds) module.                   
- **azone_lttrk_prop.csv** This file specifies the light truck proportion of the vehicle fleet and is used in [AssignVehicleType](Modules_and_Outputs.md/#assignvehicletype) module. 
- **azone_payd_insurance_prop.csv** This file provides inputs on the proportion of households having PAYD (pay-as-you-drive) insurance and is used in the [CalculateVehicleOwnCost](Modules_and_Outputs.md/#calculatevehicleowncost) module.               
- **azone_per_cap_inc.csv** This file contains information on regional average per capita household and group quarters income in year 2010 dollars and is used in the [PredictIncome](Modules_and_Outputs.md/#predictincome) module.                 
- **azone_prop_sov_dvmt_diverted.csv** This file provides inputs for a goal for diverting a portion of SOV travel within a 20-mile tour distance and is used in the [DivertSovTravel](Modules_and_Outputs.md/#divertsovtravel) module.            
- **azone_relative_employment.csv** This file contains ratio of workers to persons by age and is used in the [PredictWorkers](Modules_and_Outputs.md/#predictworkers) module. 
- **azone_veh_use_taxes.csv** This file supplies data for vehicle related taxes and is used in the [CalculateVehicleOperatingCost](Modules_and_Outputs.md/#calculatevehicleoperatingcosts) module.         
- **azone_vehicle_access_times.csv** This file supplies data for vehicle access and egress time and is used in the [CalculateVehicleOperatingCost](Modules_and_Outputs.md/#calculatevehicleoperatingcost) module.              
- **bzone_transit_service.csv** This file supplies the data on relative public transit accessibility and is used in the [AssignTransitService](Modules_and_Outputs.md/#assigntransitservice) module.                    
- **bzone_carsvc_availability.csv** This file contains the information about level of car service availability and is used in the [AssignCarSvcAvailability](Modules_and_Outputs.md/#assigncarsvcavailability) module. 
- **bzone_dwelling_units.csv** This file contains the number single-family, multi-family and group-quarter dwelling units and is used in the [PredictHousing](Modules_and_Outputs.md/#predicthousing) module.                    
- **bzone_employment.csv** This file contains the total, retail and service employment by zone and is used in the [LocateEmployment](Modules_and_Outputs.md/#locateemployment) module.                      
- **bzone_hh_inc_qrtl_prop.csv** This file contains the proportion of households in 1st, 2nd, 3rd, and 4th quartile of household income and is used in the [PredictHousing](Modules_and_Outputs.md/#predicthousing) module.                  
- **bzone_lat_lon.csv** This file contains the latitude and longitude of the centroid of the zone and is used in the [LocateEmployment](Modules_and_Outputs.md/#locateemployment) module.                           
- **bzone_network_design.csv** This file contains the intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile and is used in the [Calculate4DMeasures](Modules_and_Outputs.md/#calculate4dmeasures) module. 
- **bzone_parking.csv** This file contains the parking information and is used in the [AssignParkingRestrictions](Modules_and_Outputs.md/#assignparkingrestrictions) module.                          
- **bzone_travel_demand_mgt.csv** This file contains the information about workers and households participating in demand management programs and is used in the [AssignDemandManagement](Modules_and_Outputs.md/#assigndemandmanagement) module. 
- **bzone_unprotected_area.csv** This file contains the information about unprotected (i.e., developable) area within the zone and is used in the [Calculate4DMeasures](Modules_and_Outputs.md/#calculate4dmeasures) module.
- **bzone_urban-mixed-use_prop.csv** This file contains the target proportion of households located in mixed-used neighborhoods in zone and is used in the [CalculateUrbanMixMeasure](Modules_and_Outputs.md/#calculateurbanmixmeasure) module.               
- **bzone_urban-town_du_proportions.csv** This file contains proportion of Single-Family, Multi-Family and Group Quarter dwelling units within the urban portion of the zone and is used in the [AssignLocTypes](Modules_and_Outputs.md/#assignloctypes) module.         
- **marea_base_year_dvmt.csv** This file is used to specify to adjust the dvmt growth factors and is optional (only needed if user wants to modify the values). The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance), [CalculateBaseRoadDvmt](Modules_and_Outputs.md/#calculatebaseroaddvmt) and [CalculateFutureRoadDvmt](Modules_and_Outputs.md/#calculatefutureroaddvmt) modules.
- **marea_congestion_charges.csv** This file is used to specify the charges of vehicle travel for different congestion levels and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateRoadPerformance](Modules_and_Outputs.md/#calculateroadperformance) modules.               
- **marea_dvmt_split_by_road_class.csv** This file is used to specify the dvmt split for different road classes and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateBaseRoadDvmt](Modules_and_Outputs.md/#calculatebaseroaddvmt) modules.         
- **marea_lane_miles.csv** This file contains inputs on the numbers of freeway lane-miles and arterial lane-miles and is used in the [AssignRoadMiles](Modules_and_Outputs.md/#assignroadmiles) module.                   
- **marea_operations_deployment.csv** This file is used to specify the proportion of dvmt affected by operations for different road classes and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateRoadPerformance](Modules_and_Outputs.md/#calculateroadperformance) modules.        
- **marea_speed_smooth_ecodrive.csv** This input file supplies information of deployment of speed smoothing and ecodriving by road class and vehicle type and is used in the [CalculateMpgMpkwhAdjustments](Modules_and_Outputs.md/#calculatempgmpkwhadjustments) module.          
- **marea_transit_ave_fuel_carbon_intensity.csv** This file is used to specify the average carbon intensity of fuel used by transit and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) module. 
- **marea_transit_biofuel_mix.csv** This file is used to specify the biofuel used by transit and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateCarbonIntensity](Modules_and_Outputs.md/#calculatecarbonintensity) modules.             
- **marea_transit_fuel.csv** This file is used to specify the transit fuel proportions and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateCarbonIntensity](Modules_and_Outputs.md/#calculatecarbonintensity) modules.                      
- **marea_transit_powertrain_prop.csv** This file is used to specify the mixes of transit vehicle powertrains and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculatePtranEnergyAndEmissions](Modules_and_Outputs.md/#calculateptranenergyandemissions) modules.     
- **marea_transit_service.csv** This file contains annual revenue-miles for different transit modes for metropolitan area and is used in the [AssignTransitService](Modules_and_Outputs.md/#assigntransitservice) module.                   
- **other_ops_effectiveness.csv** This file is used to specify the delay effects of operations in different road classes and is optional (only needed if user wants to modify the values). The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateRoadPerformance](Modules_and_Outputs.md/#calculateroadperformance) modules. 
- **region_ave_fuel_carbon_intensity.csv** This file is used to specify the average carbon density for different vehicle types and is optional (only needed if user wants to modify the values). The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance) and [CalculateCarbonIntensity](Modules_and_Outputs.md/#calculatecarbonintensity) modules.       
- **region_base_year_hvytrk_dvmt.csv** This file is used to specify the heavy truck dvmt for base year and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance), [CalculateBaseRoadDvmt](Modules_and_Outputs.md/#calculatebaseroaddvmt) and [CalculateFutureRoadDvmt](Modules_and_Outputs.md/#calculatefutureroaddvmt)  modules.            
- **region_carsvc_powertrain_prop.csv** This file is used to specify the powertrain proportion of car services and is optional. The file is used in the [Initialize (VETravelPerformance)](Modules_and_Outputs.md/#initialize-vetravelperformance), [AssignHhVehiclePowertrain](Modules_and_Outputs.md/#assignhhvehiclepowertrain) and [AdjustHhVehicleMpgMpkwh](Modules_and_Outputs.md/#adjusthhvehiclempgmpkwh) modules.                      
- **region_comsvc_lttrk_prop.csv** This file supplies data for the light truck proportion of commercial vehicles and is used in the [CalculateComEnergyAndEmissions](Modules_and_Outputs.md/#calculatecomenergyandemissions) module.              
- **region_comsvc_powertrain_prop.csv** This file is used to specify the powertrain proportion of commercial vehicles and is optional. The file is used in the [Initialize (VEPowertrainsAndFuels)](Modules_and_Outputs.md/#initialize-vepowertrainsandfuels
) and [CalculateComEnergyAndEmissions](Modules_and_Outputs.md/#calculatecomenergyandemissions) modules.        
- **region_hh_driver_adjust_prop.csv** This file specifies the relative driver licensing rate relative to the model estimation data year and is used in the [AssignDrivers](Modules_and_Outputs.md/#assigndrivers) module. 
- **region_hvytrk_powertrain_prop.csv** This file is used to specify the powertrain proportion of heavy duty trucks and is optional. The file is used in the [Initialize (VEPowertrainsAndFuels)](Modules_and_Outputs.md/#initialize-vepowertrainsandfuels
) and [CalculateComEnergyAndEmissions](Modules_and_Outputs.md/#calculatecomenergyandemissions) modules.
- **region_prop_externalities_paid.csv** This file supplies data for climate change and other social costs and is used in the [CalculateVehicleOperatingCost](Modules_and_Outputs.md/#calculatevehicleoperatingcost) module. 

[Top](#rspm-inputs-and-parameters)
___

Return to [Tutorial](Main.md). 