#===========
#Make marea_lane_miles.R
#===========
#===========
# Description: 
#     This script will create the Make marea_lane_miles.csv input file and save in the input subfolder
#     https://github.com/VisionEval/VisionEval/blob/master/sources/modules/VETransportSupply/inst/module_docs/AssignRoadMiles.md#marea_lane_milescsv
#===========


source("Data Prep/config.R")

# Install/Load libraries --------------
source("Data Prep/get_packages.R")

library(dplyr)
library(tidyverse)
library(tidycensus)
library(viridis)
library(leaflet)
library(readr)
library(sp)
library(sf)
library(rgeos)
library(rlist)
library(ggplot2)
# geo, year, FwyLaneMi, ArtLaneMi


# Check to see if geo.csv exists; read marea from geo

geo_file = file.path(proj_dir, 'defs', 'geo.csv')

if(file.exists(geo_file)){
  geo <- read.csv(geo_file)
} else {
  source('Data Prep/create_geo.R')
}

marea_name = unique(geo$Marea)

if(length(marea_name) > 1){
  stop('Only implemented for one Marea currently')
}


# Get HPMS shapefile and drop in working directory
# files available at this location: https://www.fhwa.dot.gov/policyinformation/hpms/shapefiles_2017.cfm



# Use 2017 layer
# Downloaded from HPMS website
year = 2017


hpms_zip_file = file.path(working_dir,  
                          paste0(state, year, '.zip'))


if(!file.exists(hpms_zip_file)){
  stop(paste('Get the', state, year, 'file from https://www.fhwa.dot.gov/policyinformation/hpms/shapefiles_2017.cfm'))
}


rdata_file = paste0('HPMS_', state, '_', year, '.RData')

if(!file.exists(rdata_file)){
  hpms <- st_read(file.path(proj_dir, 'Data to Process', paste0(state, year, '.shp'))) # load HPMS
  save(list = c('hpms'), file = rdata_file)
  
} else {
  load(rdata_file)
}


# Merge geometry with bzones
marea_geometry <- st_union(bzone_geometry)


#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
marea_geometry <- st_transform(marea_geometry, crs = proj.USGS)
hpms <- st_transform(hpms, crs = proj.USGS)
hpms <- st_zm(hpms) # Drop elevation geometries

# Option to make maps of the HPMS geography; default to true, but this is slow
MAKEMAPS = TRUE

if(MAKEMAPS){
  plot(hpms, col = 'red', extent = marea_geometry, max.plot = 1)
}

# Intersection
hpms_marea <- st_intersection(hpms, marea_geometry)



if(MAKEMAPS){
  ggplot() + 
    geom_sf(data = marea_geometry)  +
    geom_sf(data = hpms_marea, 
            aes(color = as.factor(F_System))) +
    ggtitle('HPMS Roads by functional class for the Marea') + 
    theme_bw()
  
  ggsave(filename = paste0(state, '_', year, '_HPMS_Marea.pdf'),
         width = 8, height = 8)
}


# Code Description
# 1 Interstate
# 2 Principal Arterial – Other Freeways and
# Expressways
# 3 Principal Arterial – Other
# 4 Minor Arterial
# 5 Major Collector
# 6 Minor Co

# For marea lane miles, sum FwyLaneMiles is F_System 1
# ArtLaneMie is F_System 2-4
# need to clip to urban areas -- here all is urban

miles_per_meter = 0.000621371

# Freeway
hpms_marea_fwy = hpms_marea[hpms_marea$F_System == 1,]

lengths = st_length(hpms_marea_fwy)
lanes = hpms_marea_fwy$Through_La

FwyLaneMi = sum(lengths * lanes) * miles_per_meter

# Arterial
hpms_marea_art = hpms_marea[hpms_marea$F_System %in% c(2, 3, 4),]

lengths = st_length(hpms_marea_art)
lanes = hpms_marea_art$Through_La

ArtLaneMi = sum(lengths * lanes) * miles_per_meter

# Write out ---

lane_miles = data.frame(Geo = rep(Marea, 2),
                         Year = years,
                         FwyLaneMi = rep(round(as.numeric(FwyLaneMi), 0), 2),
                         ArtLaneMi= rep(round(as.numeric(ArtLaneMi), 0), 2))

write.csv(lane_miles, file = file.path(proj_dir, 'inputs/marea_lane_miles.csv'),
          row.names = F)
