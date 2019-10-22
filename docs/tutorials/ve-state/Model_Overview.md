# VERSPM Model Overview
----

## Objectives
VERSPM is a strategic planning model that assists state and metropolitan area planners with the evaluation of transportation and land use policy scenarios. It considers a large number of factors which affect the performance of transportation systems and their effects on people and the environment. The Regional Strategic Planning Model (RSPM), was developed by the Oregon Department of Transportation (ODOT) for the purpose of estimatingand forecasting the effects of various policies and other influences on the amount of vehicle travel, the types of vehicles and fuels used, and the resulting greenhouse gas (GHG) emissions among other outcomes.

The RSPM model was initially developed to address the following factors:

*	Changes in population demographics (age structure);
*	Changes in personal income;
*	Relative amounts of development occurring in metropolitan, urban and rural areas;
*	Metropolitan, other urban, and rural area densities;
*	Urban form in metropolitan areas (proportion of population living in mixed use areas with a well interconnected street and walkway system);
*	Amounts of metropolitan area transit service;
*	Metropolitan freeway and arterial supplies;
*	Auto and light truck proportions by year;
*	Average vehicle fuel economy by vehicle type and year;
*	Vehicle age distribution by vehicle type;
*	Electric vehicles (EVs), plug-in hybrid electric vehicles (PHEVs)
*	Light-weight vehicles such as bicycles, electric bicycles, electric scooters, etc.;
*	Pricing – fuel, vehicle miles traveled (VMT), parking;
*	Demand management – employer-based and individual marketing;
*	Car-sharing;
*	Effects of congestion on fuel economy;
*	Effects of incident management on fuel economy;
*	Vehicle operation and maintenance – eco-driving, low rolling resistance tires, speed limits; 
*	Carbon intensity of fuels, including the well to wheels emissions; and
*	Carbon production from the electric power that is generated to run electric vehicles.

## Structure
VERSPM is a disaggregate policy model that predicts travel demand impacts at an individual household level. The  model estimates vehicle ownership, vehicle travel, fuel consumption, and GHG emissions at the individual household level. This structure accounts for the synergistic and antagonistic effects of multiple policies and factors (e.g. gas prices) on vehicle travel and emissions. For example, the battery range of electric vehicles (EVs) and plug‐in hybrid electric vehicles (PHEVs) is less of an issue for households residing in compact mixed‐use neighborhoods because those households tend to drive fewer miles each day. Modeling at the household level makes it possible to evaluate the relationships between travel, emissions and the characteristics of households, land use, transportation systems, vehicles, and other factors. In addition, household level analysis makes it possible to evaluate the equitability of the costs and benefits of different strategies.

The  model comprises sequential steps with feedback. Each calculation step is composed of a number of calculations that operate on the results of the previous calculation step and on input data that reflect inputs.

The RSPM steps are grouped as follows:
1. Define households;
2. Characterize vehicles;
3. Calculate VMT; and,
4. Balance VMT with travel costs.

The iterative process to balance the VMT with travel costs allows congestion and other costs introduced at this step influence the amount of travel. This step balances the amount of household travel with the cost of travel and recalculates household VMT, Fuel & GHG in the process. The primary outputs of the RSPM are household travel, fuel and power consumption, and GHG emissions calculations, but other information is produced for households and commercial vehicles as well. The amount of commercial (light‐duty) and freight (heavy duty) travel is calculated as well as associated fuel, power consumption and GHG emissions for those vehicles.

## Modules

_TODO_: update links to VisionEval if development branch is merged

1. **Household Modules**. - *VESimHouseholds package*

The following four modules create a set of households for each forecast year that represents each resident in the model area with the likely household mix of household and person characteristics:

  * [Create Households](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VESimHouseholds/inst/module_docs/CreateHouseholds.md);
  * [Predict Workers](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VESimHouseholds/inst/module_docs/PredictWorkers.md);
  * [Assign Life Cycle](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VESimHouseholds/inst/module_docs/AssignLifeCycle.md); and,
  * [Predict Income](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VESimHouseholds/inst/module_docs/PredictIncome.md).

2. **Land Use Modules** - *VELandUse package* 

The following five modules assigns a housing type to households (e.g. single-family, multi-family, etc.) and a development type (metropolitan, town, rural) based on available input dwelling units. Households are assigned a location in the metropolitan area based on the projected supply of housing and neighborhood affordability. Neighborhood population density and mixed-use character are calculated.
  * [Predict Housing](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/PredictHousing.md);
  * [Locate Employment](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/LocateEmployment.md);
  * [Assign Location Types](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/AssignDevTypes.md);
  * [Calculate '4D' Measures](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/Calculate4DMeasures.md);
  * [Calculate Urban Mix Measures](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/CalculateUrbanMixMeasure.md);
  
The following three modules identifies parking restrictions and prices affecting households, identifies households participating in TDM programs and assigns car service availability to zones ( including car sharing and taxis)
  * [Assign Parking Restrictions](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/AssignParkingRestrictions.md);
  * [Assign Demand Management](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/AssignDemandManagement.md); and,
  * [Assign Car Service Availability](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VELandUse/inst/module_docs/AssignCarSvcAvailability.md).

3. **Transport Supply Modules**. - *VETransportSupply package* 

The following two modules gather details about the relevant roadway and public transport systems considered in the simulation of travel:
  * [Assign Transit Service](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETransportSupply/inst/module_docs/AssignTransitService.md); and,
  * [Assign Road Miles](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETransportSupply/inst/module_docs/AssignRoadMiles.md).

4. **Household Vehicle Modules**. - *VEHouseholdVehicles package*

The following seven modules simulate details about the drivers of vehicles in the simulation and types of vehicles they drive:
  * [Assign Drivers](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/AssignDrivers.md);
  * [Assign Vehicle Ownership](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/AssignVehicleOwnership.md);
  * [Assign Vehicle Type](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/AssignVehicleType.md);
  * [Create Vehicle Table](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/CreateVehicleTable.md);
  * [Assign Vehicle Age](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/AssignVehicleAge.md);
  * [Calculate Vehicle Ownership Cost](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/CalculateVehicleOwnCost.md); and,
  * [Adjust Vehicle Ownership](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdVehicles/inst/module_docs/AdjustVehicleOwnership.md).

5. **Household Travel Modules**. - *VEHouseholdTravel package*

 The following four module provides an initial estimate of average daily vehicle miles traveled (DVMT) for each household
based on the household characteristics (e.g., demographics, income, transportation options, and land use). They also calculate the households non-motorized trips.
  * [Calculate Household Daily Vehicle Miles Traveled](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdTravel/inst/module_docs/CalculateHouseholdDvmt.md);
  * [Calculate Non-Automobile or 'Alternative' Mode Trips](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdTravel/inst/module_docs/CalculateAltModeTrips.md); 
  * [Calculate Vehicle Trips](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdTravel/inst/module_docs/CalculateVehicleTrips.md); and,
  * [Divert Single-occupant Vehicle Travel](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEHouseholdTravel/inst/module_docs/DivertSovTravel.md). 

6. **Powertrain and Fuel Modules**. - *VEPowertrainsAndFuels package*

 The following two modules identifies the vehicles powertrain as an internal combustion engine (ICE), hybrid-electric vehicle (HEV), plug-in hybrid electric vehicle (PHEV), or electric vehicle (EV) and calculates each household vehicle's emission. 
  * [Calculate Carbon Intensity](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEPowertrainsAndFuels/inst/module_docs/CalculateCarbonIntensity.md); and,
  * [Assign Household Vehicle Powertrains](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VEPowertrainsAndFuels/inst/module_docs/CalculateCarbonIntensity.md).

### Feedback

THe next step balance the amount of household travel with the cost of travel and recalculate household VMT, Fuel and GHG emissions . This is necessary because: 1) congestion calculations affect fuel economy and thus the amount and cost of fuel consumed; 2) congestion pricing affects the amount of travel and household travel costs; 3) fuel, vehicle travel, and other taxes and fees affect the amount and cost of travel; and 4) eco-driving improves fuel economy and reduces fuel cost. The effect of these adjustments to household travel costs need to be included in the total household travel costs and the adjustment to household DVMT.

7. **Travel Performance**. - *VETravelPerformance package*

The following modules run iteratively to balance the DVMT and travel costs. Total light duty vehicle (household and commercial service vehicle), truck and bus DVMT is calculated for the metropolitan area and assigned to portions of the road system (freeway, arterial, other). Congestion levels are and associated speed reductions are calculated considering the traffic loads and inputs regarding the deployment of traffic operations programs (e.g. ramp metering, traffic signal coordination) and congestion pricing. Speed-adjusted fuel economy is calculated considering variations by powertrain. Travel cost per mile due to congestion pricing is also calculated. Household travel costs are calculated from the amounts of miles driven, fuel consumed, electricity consumed, and GHG emitted. Other inputs establish the rates for fuel costs, power costs, fuel taxes, VMT taxes, PAYD insurance, and several external costs. Finally a household budget model is used to adjust household DVMT to reflect the effect of household travel costs on the amount of household travel. The adjusted household DVMT is allocated to vehicles in proportion to the previous allocation. This process can run multiple times until DVMT changes very little between iterations.

  * [Calculate Road VMT for Base Year](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculateBaseRoadDvmt.md);
  * [Calculate Road VMT for Future Year](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculateFutureRoadDvmt.md);
  * [Calculate Road Performance](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculateRoadPerformance.md);
  * [Calculate Fuel and Electric Energy Economy](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculateMpgMpkwhAdjustments.md);
  * [Adjust Fuel and Electric Energy Economy](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/AdjustHhVehicleMpgMpkwh.md);
  * [Calculate Vehicle Operating Cost](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculateVehicleOperatingCost.md); and,
  * [Adjust DVMT Based on Budget](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/BudgetHouseholdDvmt.md).


8. **Heavy Vehicles Emissions**. - *VETravelPerformance*

The following two modules calculate heavy vehicle fuel and power consumption and GHG emissions: 
 
 * [Calculate Commercial Vehicles Emissions](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculateComEnergyAndEmissions.md);
 * [Calculate Transit Vehicles Emissions](https://github.com/gregorbj/VisionEval/blob/develop/sources/modules/VETravelPerformance/inst/module_docs/CalculatePtranEnergyAndEmission.md);


Return to [Tutorial](Main.md). 