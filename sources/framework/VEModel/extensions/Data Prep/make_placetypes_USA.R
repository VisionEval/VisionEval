#===========
#make_placetypes_USA.R
#===========
#===========
# Description: 

# This script will create all data inputs that use Placetypes_USA/EPA Smart Locations Database as the data source
# Input files: 
#       - bzone_employment.csv 
#       - bzone_unprotected_area.csv 
#       - bzone_network_design.csv 
#       - bzone_transit_service.csv 

# Note that 5 marea input files are also listed here but these are not necessary for the final inputs:
#       - marea_d4bpo4_adj.csv
#       - marea_mix_targets.csv 
#       - marea_travel-demand-mgt_by_area-type.csv
#       - marea_parking-avail_by_area-type.csv
#       - marea_parking-cost_by_area-type.csv
#===========

#load 2010 EPA smart location dataset
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./data/Sld_df.Rda")


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


# Load Census Data --------------
# add in census api key
fileName <- 'census_api.txt'

# check if census_api.txt is in the working directory
if(!file.exists(file.path(working_dir,fileName))){ 
  stop(paste('Census API Key needed in as a plain text file in /n', file.path(working_dir), '/n go to https://api.census.gov/data/key_signup.html /n and save the API key as `census_api.txt`'))
}

mystring <- read_file(file.path(working_dir, fileName))
api_key <- gsub('\r\n', '', mystring) #clean up in case text file has breaks

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron") # will check R environment for past api keys

# load census tract data
# Dwell units uses census table S1101
# Full table found at this link: https://api.census.gov/data/2016/acs/acs5/subject/groups/S1101.html

# Download with geography
block_groups <- get_acs(geography = "block group",variables = "B01003_001",year=2013, 
                           state = state, county = counties, geometry = TRUE) %>%
                        select(GEOID, NAME)

Sld_df_filtered <- Sld_df %>% filter(GEOID10 %in% block_groups$GEOID) %>% # filter for our Census block groups
                          select(GEOID10, EMPTOT, E5_RET10, E5_SVC10,AC_LAND, D4c, D3bpo4, AC_TOT, AC_WATER, AC_UNPR) %>% 
                          mutate(GEOID = GEOID10) %>% 
                          merge(block_groups, by = "GEOID")


block_groups_Sld <- block_groups %>% merge(Sld_df, by.x = "GEOID", by.y = "GEOID10") %>% # filter for our Census block groups
  select(GEOID, EMPTOT, E5_RET10, E5_SVC10,AC_LAND, D4c, D3bpo4, AC_TOT, AC_WATER, AC_UNPR) 
  





# Clean tract and TAZ geometries --------------
bzone_geometry_sp <- as(bzone_geometry, Class = "Spatial")  #make TAZ df into sp 
block_groups_Sld_sp = as_Spatial(block_groups_Sld) #make tract df into sp 

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
bzone_geometry_sp_newproj <- spTransform(bzone_geometry_sp, CRS = proj.USGS)
block_groups_Sld_sp_newproj <- spTransform(block_groups_Sld_sp, CRS = proj.USGS)


# Find intersection between TAZ and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(bzone_geometry_sp, block_groups_Sld_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
n<-names(gI) #grab the ids/names from the gIntersection object
n<-data.frame(t(data.frame(strsplit(n," ",fixed=TRUE)))) #ids are combined so split into separate cells
colnames(n)[1:2]<-c("id_bzone","id_blockgroup") #add id names to differentiate


#find the overlapping area for all the TAZ-Tract objects
n$area<-sapply(gI@polygons, function(x) x@area) 
a<-data.frame(id=row.names(bzone_geometry_sp_newproj), Bzone = bzone_geometry_sp_newproj$Bzone, 
                  SUB_POP15 = bzone_geometry_sp_newproj$SUB_POP15, SUB_POP40= bzone_geometry_sp_newproj$SUB_POP40, # note that these columns are needed to find urban rural classifications
                  SQMI = bzone_geometry_sp_newproj$SQMI_FXTAZ
              )#subset TAZ dataset so only joining TAZ ids
df<-merge(n,a,by.x = "id_bzone", by.y = "id", all.x=TRUE) #merge the TAZ ids into our dataset


#find the total area of every census tract
df <- df %>%   group_by(id_blockgroup)%>%
  summarise(blockgroup_area = sum(area))%>%
  right_join(df, by = "id_blockgroup") 


df_interim <- df %>% group_by(Bzone)%>%
  summarise(bzone_area = sum(area)) %>%
  right_join(df, by = "Bzone") 


block_groups_Sld$id_blockgroup <- seq.int(nrow(block_groups_Sld)) #make column so we can join census tract df with intersection df
df2<- merge(df_interim, block_groups_Sld, by = "id_blockgroup", by.y = "id_blockgroup", all.x=TRUE)


# Finalize dataframe -------------------------
df3 <- df2 %>% mutate(share_area_by_blockgroup = area/blockgroup_area, #calculate % of block group in each TAZ (use for count-based metrics)
                      share_area_by_bzone = area/bzone_area, #calculate % of block group that makes up each TAZ (use for rate-based metrics)
                      EMPTOT_this_area = EMPTOT * share_area_by_blockgroup, # bzone_employment
                      E5_RET10_this_area = E5_RET10 * share_area_by_blockgroup, # bzone_employment
                      E5_SVC10_this_area = E5_SVC10 * share_area_by_blockgroup, # bzone_employment
                      AC_LAND_this_area = AC_LAND * share_area_by_blockgroup, # bzone_unprotected_area - old method
                      AC_UNPR_this_area = AC_UNPR * share_area_by_blockgroup, # bzone_unprotected_area - new method
                      D4c_this_area = D4c * share_area_by_bzone, # bzone_transit
                      D3bpo4_this_area = D3bpo4 * share_area_by_bzone) %>% #bzone_network
  group_by(Bzone)%>%
  summarise(n = n(),
            TotEmp = sum(EMPTOT_this_area), # employment 
            RetEmp = sum(E5_RET10_this_area), # employment 
            SvcEmp = sum(E5_SVC10_this_area), # employment 
            AC_LAND = sum(AC_LAND_this_area), # unprotected areas - old method
            AC_UNPR = sum(AC_UNPR_this_area), # unprotected areas  - new method
            D4c = sum(D4c_this_area), # transit 
            D3bpo4 = sum(D3bpo4_this_area),
            SQMI = mean(SQMI),
            SUB_POP15 = mean (SUB_POP15),
            SUB_POP40 = mean(SUB_POP40)) %>%
  mutate(Geo = Bzone) 


#duplicate data for current and future years    
df3_copy <- df3

df3$Year <- years[1]
df3_copy$Year <- years[2]

df3 <- df3 %>% mutate(
  Pop_Density = SUB_POP15 / SQMI
) %>% 
  mutate(
    Rural = ifelse(Pop_Density<161, 1, 0),
    Town = ifelse(Pop_Density<1241 & Pop_Density>161, 1, 0),
    Urban = ifelse(Pop_Density>1241, 1, 0)
  )

df3_copy <- df3_copy %>% mutate(
  Pop_Density = SUB_POP40 / SQMI
) %>% 
  mutate(
    Rural = ifelse(Pop_Density<161, 1, 0),
    Town = ifelse(Pop_Density<1241 & Pop_Density>161, 1, 0),
    Urban = ifelse(Pop_Density>1241, 1, 0)
  )


#make final csv file and save to temp directory
bzone_employment <- rbind(df3, df3_copy) %>% select("Geo","Year",'TotEmp','RetEmp','SvcEmp') 
write.csv(bzone_employment, file.path(proj_dir, 'inputs/bzone_employment.csv'), row.names = FALSE) #save as csv in final directory

#Under Construction#
bzone_unprotected_area <- rbind(df3, df3_copy) %>% select("Geo","Year",'AC_UNPR', 'Rural', 'Town', 'Urban') %>% 
  mutate(UrbanArea = AC_UNPR * Urban,
         TownArea = AC_UNPR * Town,
         RuralArea = AC_UNPR * Rural) %>% select("Geo", "Year", "UrbanArea","TownArea","RuralArea")
write.csv(bzone_unprotected_area, file.path(proj_dir, 'inputs/bzone_unprotected_area.csv'), row.names = FALSE) #save as csv in final directory


bzone_transit <- rbind(df3, df3_copy) %>% select("Geo","Year",'D4c') 
write.csv(bzone_transit, file.path(proj_dir, 'inputs/bzone_transit.csv'), row.names = FALSE) #save as csv in final directory


bzone_network_design <- rbind(df3, df3_copy) %>% select("Geo","Year",'D3bpo4') 
write.csv(bzone_network_design, file.path(proj_dir, 'inputs/bzone_network_design.csv'), row.names = FALSE) #save as csv in final directory



df2_no_geo<-df2 %>% select(-"geometry")

write.csv(df2_no_geo, file.path(working_dir, 'smart_locations_database_raw.csv'), row.names = FALSE) #save as csv in final directory



################################################################################
# This section is made to save files that may be useful for checking the final output or looking at census tract information

#create final TAZ-level spatial polygon and plot output
bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
df3_geo <-st_set_geometry(df3, bzone_geometry_reordered$geometry) 

par(mfrow=c(2,1))
plot(df3_geo['D3bpo4'],
     main = 'TAZ - D3bpo4')
plot(df3_geo['D3bpo4'],
     main = 'TAZ - D3bpo4')




#final TAZ-level output saved as shapefile
df3_geo_sp = as_Spatial(df3_geo)
rgdal::writeOGR(obj = df3_geo_sp,
                dsn = file.path(working_dir, "bzone_network_design_shapefile/"),
                layer = 'bzone_network_design',
                driver = 'ESRI Shapefile')


#Tract-level output saved as shapefile
rgdal::writeOGR(obj = block_groups_Sld_sp,
                dsn = file.path(working_dir, "BlockGroup_network_design_shapefile/"),
                layer = 'BlockGroup_network_design',
                driver = 'ESRI Shapefile')




