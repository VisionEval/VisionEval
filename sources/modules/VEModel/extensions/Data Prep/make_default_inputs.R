#===========
#make_default_inputs.R
#===========

# This script will all the input files where default inputs are acceptable
# This script will make the following files:
#       - region_base_year_dvmt.csv
#       - bzone_urban-mixed-use_prop.csv
#       - region_hh_driver_adjust_prop.csv
#       - azone_hh_veh_mean_age.csv
#       - azone_carsvc_characteristics.csv
#       - azone_hh_veh_own_taxes.csv
#       - azone_payd_insurance_prop.csv
#       - region_carsvc_powertrain_prop.csv
#       - region_comsvc_powertrain_prop.csv
#       - region_ave_fuel_carbon_intensity.csv
#       - region_hvytrk_powertrain_prop.csv
#       - azone_charging_availability.csv
#       - marea_speed_smooth_ecodrive.csv
#       - marea_congestion_charges.csv
#       - azone_veh_use_taxes.csv
#       - azone_fuel_power_cost.csv
#       - azone_vehicle_access_times.csv
#       - region_co2e_costs.csv
#       - region_prop_externalities_paid.csv
#       - region_road_cost.csv
#       - region_comsvc_lttrk_prop.csv
#       - region_comsvc_veh_mean_age.csv

# Others to use defaults but not made in this script:
#       - other_ops_effectiveness.csv


# Relies on create_geo.R to be run first, to make the template files.
# Then check to see if the named file (e.g., azone_hh_veh_age_mean.csv) has already been updated (does it have the default Rogue Valley MPO geo names, or not?). If not, proceed
# Then join in the default columns, repeating for the base and future years for each zone as necessary.

# File paths
proj_dir = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study/'

input = file.path(proj_dir, 'Data_to_process')

