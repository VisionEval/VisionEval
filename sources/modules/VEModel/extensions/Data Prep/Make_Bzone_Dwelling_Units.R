#===========
#Make_Bzone_Dwelling_Units.R
#===========
#===========
# Description: 
#     This script will create the bzone_dwelling_units.csv file and save in the input subfolder
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

# Load Census Data --------------
# add in census api key
fileName <- 'census_api.txt'

# check if census_api.txt is in the working directory
if(!file.exists(file.path(working_dir,fileName))){ 
  stop(paste('Census API Key needed in as a plain text file in \n', file.path(working_dir), '\n go to https://api.census.gov/data/key_signup.html \n and save the API key as `census_api.txt`'))
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
dwell_units_raw <- get_acs(geography = "tract", table = "S1101",
                           state = state, county = counties, geometry = TRUE)

#filter for our columns
dwell_units_016 <- dwell_units_raw %>% filter(variable=="S1101_C01_016") # percent one unit households
dwell_units_017 <- dwell_units_raw %>% filter(variable=="S1101_C01_017") # percent two unit households
dwell_units_018 <- dwell_units_raw %>% filter(variable=="S1101_C01_018") # percent mobile home households
dwell_units_total <- dwell_units_raw %>% filter(variable=="S1101_C01_001") # total households


# Make sure all have the same GEOID
# The tract is value 6 - 9 in the census GEOID
identical(dwell_units_016$GEOID, dwell_units_018$GEOID) &
  identical(dwell_units_016$GEOID, dwell_units_017$GEOID) &
  identical(dwell_units_016$GEOID, dwell_units_total$GEOID)

#add all variables into single table, create VisionEval columns
dwell_units_geo <- dwell_units_016 %>% 
                        mutate (one_unit = dwell_units_016$estimate, #bring in the 3 census variables
                                 mobile_home = dwell_units_018$estimate,
                                 two_unit = dwell_units_017$estimate,
                                total = dwell_units_total$estimate) %>%
                        mutate(SFDU = (one_unit + mobile_home)*total/100, #change census variables to VisionEval variables, needs to be actual number of units
                               MFDU = two_unit*total/100) %>% 
                        mutate(Geo = substr(GEOID, 6, 9)) %>%
                        select("Geo", "SFDU", "MFDU",  "GEOID") %>% #filter dataframe columns
                        replace(is.na(.), 0) #assume all NAs are 0



# Clean tract and Bzone geometries --------------
bzone_geometry_sp <- as(bzone_geometry, Class = "Spatial")  #make Bzone df into sp 
dwell_units_geo_sp = as_Spatial(dwell_units_geo) #make tract df into sp 

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
bzone_geometry_sp_newproj <- spTransform(bzone_geometry_sp, CRS = proj.USGS)
dwell_units_geo_sp_newproj <- spTransform(dwell_units_geo_sp, CRS = proj.USGS)


# Find intersection between bzone and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(bzone_geometry_sp_newproj, dwell_units_geo_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
n<-names(gI) #grab the ids/names from the gIntersection object
n<-data.frame(t(data.frame(strsplit(n," ",fixed=TRUE)))) #ids are combined so split into separate cells
colnames(n)[1:2]<-c("id_bzone","id_tract") #add id names to differentiate


#find the overlapping area for all the bzone-Tract objects
n$area<-sapply(gI@polygons, function(x) x@area) 
a<-data.frame(id=row.names(bzone_geometry_sp_newproj), Bzone = bzone_geometry_sp_newproj$Bzone, 
              SUB_POP15 = bzone_geometry_sp_newproj$SUB_POP15, SUB_POP40= bzone_geometry_sp_newproj$SUB_POP40, # note that these columns are needed to find urban rural classifications
              SQMI = bzone_geometry_sp_newproj$SQMI_FXTAZ)#subset bzone dataset so only joining bzone ids
df<-merge(n,a,by.x = "id_bzone", by.y = "id", all.x=TRUE) #merge the bzone ids into our dataset


#find the total area of every census tract
df <- df %>%   group_by(id_tract)%>%
                summarise(shape_area = sum(area))%>%
                right_join(df, by = "id_tract") 
        


dwell_units_geo$id_tract <- seq.int(nrow(dwell_units_geo)) #make column so we can join census tract df with intersection df
df2<- merge(df, dwell_units_geo, by = "id_tract", by.y = "id_tract", all.x=TRUE)

# Finalize dataframe -------------------------
df3 <- df2 %>% mutate(share.area = area/shape_area, #calculate % of tract in each Bzone
                   SFDU_this_area = SFDU * share.area, # multiply to get SFDU/MFDU in each intersected polygon
                   MFDU_this_area = MFDU * share.area) %>% 
              group_by(Bzone)%>%
              summarise(n = n(),
                        SFDU = sum(SFDU_this_area), # add up tract-level SFDU/MFDUs for each Bzone 
                        MFDU = sum(MFDU_this_area),
                        SQMI = mean(SQMI),
                        SUB_POP15 = mean (SUB_POP15),
                        SUB_POP40 = mean(SUB_POP40)) %>%
              mutate(Geo = Bzone,
                     GQDU = 1) # no census variable for GQDU. Must use outside data source or assume as 1



# quality check that total SFDUs and MFDUs are same in Bzone and census tract files
identical(sum(df3$SFDU), sum(dwell_units_geo$SFDU)) &
  identical(sum(df3$MFDU),sum(dwell_units_geo$MFDU))

#duplicate 2019 data for 2045    
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
bzone_dwelling_units_final <- rbind(df3, df3_copy) %>% select("Geo","Year",'SFDU','MFDU','GQDU') 
write.csv(bzone_dwelling_units_final, file.path(proj_dir, 'inputs/bzone_dwelling_units.csv'), row.names = FALSE) #save as csv in final directory


bzone_dwelling_units_urban_town_du <- rbind(df3, df3_copy) %>% select("Geo","Year",'SFDU','MFDU','GQDU', 'Rural', 'Town', 'Urban') %>% 
  mutate(PropUrbanSFDU = SFDU * Urban,
         PropUrbanMFDU = MFDU * Urban,
         PropUrbanGQDU = GQDU * Urban,
         PropTownSFDU = SFDU * Town,
         PropTownMFDU = MFDU * Town,
         PropTownGQDU = GQDU * Town) %>% select("Geo", "Year", "PropUrbanSFDU","PropUrbanMFDU","PropUrbanGQDU",'PropTownSFDU', 'PropTownMFDU', 'PropTownGQDU')

write.csv(bzone_dwelling_units_urban_town_du, file.path(proj_dir, 'inputs/bzone_urban-town_du_proportions.csv'), row.names = FALSE) #save as csv in final directory



################################################################################
# This section is made to save files that may be useful for checking the final output or looking at census tract information

#create final TAZ-level spatial polygon and plot output
bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
df3_geo <-st_set_geometry(df3, bzone_geometry_reordered$geometry) 

par(mfrow=c(2,1))
plot(df3_geo['MFDU'],
     main = 'TAZ - MFDU')
plot(df3_geo['SFDU'],
     main = 'TAZ - SFDU')


#final TAZ-level output saved as shapefile
df3_geo_sp = as_Spatial(df3_geo)
rgdal::writeOGR(obj = df3_geo_sp,
                dsn = file.path(working_dir, "bzone_dwell_units_shapefile/"),
                layer = 'bzone_dwelling_units',
                driver = 'ESRI Shapefile')


#Tract-level output saved as shapefile
rgdal::writeOGR(obj = dwell_units_geo_sp,
                dsn = file.path(working_dir, "tract_dwell_units_shapefile/"),
                layer = 'Tract_dwelling_units',
                driver = 'ESRI Shapefile')


