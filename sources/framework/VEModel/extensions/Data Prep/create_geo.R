#===========
#create_geo.R
#===========
#===========
# Description: 
#     This will create geo.csv and save into a defs folder
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


write.csv(geo,
          file = file.path(proj_dir, 'defs/geo.csv'),
          row.names = F)


