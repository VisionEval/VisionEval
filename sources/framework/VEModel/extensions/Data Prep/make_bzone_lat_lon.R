#===========
#make_bzone_lat_lon.R
#===========
#===========
# Description: 
#     This script will create the bzone_lat_lon.csv input file and save in the input subfolder
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


# Find centroid of bzone geometries --------------

bzone_geometry$centroids <- bzone_geometry %>%
  st_centroid() %>% 
  st_transform(., 4326) %>% # move to standard lat lon WGS84 projection
  st_geometry()


# Create input dataframe --------------

bzone_lat_lon <- bzone_geometry %>%
  mutate(Geo = Bzone,
        "Longitude" = st_coordinates(bzone_geometry$centroids)[,1] ,
         "Latitude" = st_coordinates(bzone_geometry$centroids)[,2]) %>%
  select(Geo, Latitude, Longitude ) %>%
  st_set_geometry(., NULL)
  


# Duplicate 2019 data for 2045 ----------

bzone_lat_lon_copy <- bzone_lat_lon
bzone_lat_lon$Year <- years[1]
bzone_lat_lon_copy$Year <- years[2]

# Make final input file, save  ----------
bzone_lat_lon_final <- rbind(bzone_lat_lon, bzone_lat_lon_copy)  
write.csv(bzone_lat_lon_final, file.path(proj_dir, 'inputs/bzone_lat_lon.csv'), row.names = FALSE) #save as csv in final directory



################################################################################
###################### Data Quality Checks #####################################


# This section makes a plot to check if the centroids visually look like they are in the correct spot. 
bzone_geometry <- st_transform(bzone_geometry, 4326)
plot(st_geometry(bzone_geometry))
plot(st_set_geometry(bzone_geometry, 'centroids')[, 0], add = T, col = 'red', pch = 19)

