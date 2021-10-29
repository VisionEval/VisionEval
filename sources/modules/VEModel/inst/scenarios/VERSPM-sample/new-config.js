// The following is JSON

// GROUP CONFIGURATION (FOR VISUALIZER)

catconfig = [
{
  "NAME": "Marketing/Incentive",
  "DESCRIPTION": "Local programs to improve driving efficiency & reduce auto demand.",
  "LEVELS":[
  {
    "NAME": "1",
    "INPUTS":[
    {
      "NAME": "D",
      "LEVEL": "1"
    },
    {
      "NAME": "E",
      "LEVEL": "1"
    }
      ]
  },
  {
    "NAME": "2",
    "INPUTS":[
    {
      "NAME": "D",
      "LEVEL": "2"
    },
    {
      "NAME": "E",
      "LEVEL": "2"
    }
      ]
  }
      ]
},
{
  "NAME": "Pricing",
  "DESCRIPTION": "State-led policies that move towards true cost pricing.",
  "LEVELS":[
  {
    "NAME": "1",
    "INPUTS":[
    {
      "NAME": "C",
      "LEVEL": "1"
    }
    ]
  },
  {
    "NAME": "2",
    "INPUTS":[
    {
      "NAME": "C",
      "LEVEL": "2"
    }
    ]
  },
  {
    "NAME": "3",
    "INPUTS":[
    {
      "NAME": "C",
      "LEVEL": "3"
    }
    ]
  }
    ]
}
];

// SCENARIO CONFIGURATION (COMPONENTS)

scenconfig = [
{
  "NAME": "D",
  "LABEL": "Demand Management",
  "DESCRIPTION": "Programs to encourage less private vehicle travel.",
  "INSTRUCTIONS": "Programs and incentives which encourage people to drive less including ridesharing, van pooling, telecommuting, and transit subsidies.",
  "CATEGORY": "Marketing/Incentive",
  "LEVELS":[
  {
    "NAME": "1",
    "LABEL": "Base",
    "DESCRIPTION": "Existing level"
  },
  {
    "NAME": "2",
    "LABEL": "EcoProp & ImpProp",
    "DESCRIPTION": "Increased the proportion by 10%"
  } ],
  "FILES":[
           "bzone_travel_demand_mgt.csv" // (VELandUse - Assign Demand Management)
          ]
},
{
  "NAME": "E",
  "LABEL": "Driving Efficiency",
  "DESCRIPTION": "Driving efficiency by increasing implementation of ITS.",
  "CATEGORY": "Marketing/Incentive",
  "LEVELS":[
  {
    "NAME": "1",
    "LABEL": "Base",
    "DESCRIPTION": "Baseline implementation of ITS."
  },
  {
    "NAME": "2",
    "LABEL": "Fully implemented ITS",
    "DESCRIPTION": "Increase the effectiveness of implementation of ITS."
  } ],
  "FILES":[
           "marea_operations_deployment.csv", // (VETravelPerformance - Initialize),
           "marea_speed_smooth_ecodrive.csv", // (VETravelPerformance - CalculateMpgMpkwhAdjustments),
           "other_ops_effectiveness.csv" // (VETravelPerformance - Initialize),
          ]
},
{
  "NAME": C
  "LABEL": "Vehicle Travel Cost",
  "DESCRIPTION": "Combination of fuel prices and charges to pay for roadway costs and possibly externalities.",
  "CATEGORY": "Pricing",
  "LEVELS": [
  {
    "NAME": "1",
    "LABEL": "Base",
    "DESCRIPTION": "No change in fuel prices or increase in roadway or externality charges."
  },
  {
    "NAME": "2",
    "LABEL": "Steady Ownership Cost/Tax",
    "DESCRIPTION": "Keep the vehicle ownership cost the same."
  },
  {
    "NAME": "3",
    "LABEL": "Payd insurance and higher cost",
    "DESCRIPTION": "Higher climate cost and pay as you drive insurance."
  } ],
  "FILES": [
            "azone_hh_veh_own_taxes.csv", // (VEHouseholdVehics - CalculateVehicleOwnCost),
            "region_prop_externalities_paid.csv" // (VETravelPerformance - CalculateVehicleOperatingCost)
           ]
}
];
