#===========
#config.R
#===========
#===========
# Description:
#     This will create all of the reference variables for the rest of data prep scripts.
#     Default values are provided but should be updated to user specifications.
#===========

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

#set project directory folders
proj_dir <- 'C:/Users/eric.englin/Desktop/VisionEval/VDOT/'


#create folders
working_dir <- file.path(proj_dir, 'working/') #may be needed
if(!dir.exists(working_dir)){ dir.create(working_dir)}

inputs = file.path(proj_dir, 'inputs')
if(!dir.exists(inputs)){ dir.create(inputs)}

defs = file.path(proj_dir, 'defs')
if(!dir.exists(defs)){ dir.create(defs)}


#add inputs
years <- c(2019, 2045)
Marea <- "NVTA"
bzone_geometry <- st_read(file.path(working_dir, "FFXsubzone/FFX_Subzone.shp")) %>% #load TAZ dataset from working directory
                        mutate(Bzone = TAZ_N, #create standard column name bzones
                                Azone = NAME) #create standard column name azones

bzone_names <- bzone_geometry %>% st_set_geometry(NULL) %>% select(TAZ_N)#remove geometry field, select name column
azone_names <- c("Fairfax", "Fairfax City", "Falls Church")
czone<-NA

geo = bzone_geometry %>%
  st_set_geometry(NULL) %>%
  select(NAME, TAZ_N) %>%
  rename(Azone = NAME,
         Bzone = TAZ_N) %>%
  mutate(Czone = NA,
         Marea = "NVTA")


#Census information
counties <- c(059, 600, 610) #enter county codes here
counties_geoids <- c(51059, 51600, 51610) #enter county codes with state code
state <- "VA"
