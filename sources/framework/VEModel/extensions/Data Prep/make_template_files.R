#===========
#make_template_files.R
#===========
#===========
# Description: 
#     This file can be used to create template files for azone and bzone input files
#     Note that this won't make a specific input file but can be useful reference to make files and check for any errors
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


# Make file templates

years = c(2019, 2045)


azone_template = expand.grid(unique(geo$Azone), years)
colnames(azone_template) = c('Geo', 'Year')

write.csv(azone_template, 
          file = file.path(working_dir, 'azone_template.csv'),
          row.names = F)

bzone_template = expand.grid(unique(geo$Bzone), years)
colnames(bzone_template) = c('Geo', 'Year')

write.csv(bzone_template, 
          file = file.path(working_dir, 'bzone_template.csv'),
          row.names = F)

