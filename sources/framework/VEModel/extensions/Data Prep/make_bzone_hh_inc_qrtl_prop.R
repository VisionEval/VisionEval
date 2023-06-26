#===========
#make_bzone_hh_inc_qrtl_prop.R
#===========
#===========
# Description: 
#     This script will create the make_bzone_hh_inc_qrtl_prop.csv file and save in the input subfolder
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
# HH income quartiles uses census table B19001
# Full table found at this link: https://api.census.gov/data/2016/acs/acs5/groups/B19001.html

# Download Census table
hh_income_county <- get_acs(geography = "county", table = "B19001",
                         state = state, geometry = FALSE) %>% filter(GEOID %in% counties_geoids)


var_list <- c("B19001_002", "B19001_003", "B19001_004", "B19001_005", "B19001_006", "B19001_007", 
              "B19001_008", "B19001_009", "B19001_010", "B19001_011","B19001_012", "B19001_013", 
              "B19001_014", "B19001_015", "B19001_016", "B19001_017")


estimate_list <- c(10000,14999,19999,24999,29999,34999,39999,44999,49999,59999,74999,99999,124999,149999,199999,200000)



percentile_q1_num_list = 0
percentile_q2_num_list = 0
percentile_q3_num_list = 0
percentile_q4_num_list = 0
total_hh_list = 0
tract_list = 0
county_list = 0


########
## The steps below outline the logic behind the loops and steps to create the final csv file
#
# **Azone Level (Steps 1-7)** 
#Step 1) Download Census data at azone level
#Step 2) Use variable B19001_001 to find total households in azone
#Step 3) Find the household number for the 25th, 50th, and 75th percentile in this azone
#Step 4) Households are bucketed into 16 income categories (B19001_02 to B19001_17). 
#        Do a running count of the households in each category. 
#Step 5) Once the running count is more than the 25th, 50th, and 75th percentile household number, 
#        flag this as the category where the household income number will be. 
#Step 6) Find the proportion of households in this bucket to calculate the exact 25th, 50th, and 75th percentile household income number. 
#           Example: 25th percentile household in this azone is flagged in the $50,000 - $59,999 income category. 
#                    The running count finds that this household is 500 out of the 712 in this income category in the azone. 
#                     In this case, the 25th percentile income is estimated as: $50,000 + [500/712]*$9,999 = $57,021.77
#Step 7) For the 25th, 50th, and 75th percentiles, record the income category and the household proportion to calculate bzone-level figures 
#           Example: Using previous example, the 25th percentile is in the $50,000-$59,999 income category (10th/17 income groups) 
#                    and the proportion would be 500/712 = 0.7022 
#
#
# **Bzone Level  (Steps 8-13)** 
#Step 8) Download Census data for all bzones in this azone
#Step 9) Use the income category and household proportions to find the number of bzone households in each quartile using the azone boundaries
#Step 10) Add this information to a series of lists that will be combined into a complete dataframe
#Step 11) Repeat for all azones
#Step 12) [OPTIONAL] Convert the bzone dataset into a different geography level if needed
#Step 13) Save as bzone_hh_inc_qrtl_prop.csv

for (geoid in counties_geoids){
  print(geoid) #print statement to track loop count
  total <- hh_income_county %>% filter(GEOID == geoid) %>% filter(variable == "B19001_001") %>% select(estimate)
  percentile_25 <- total * 0.25
  percentile_50 <- total * 0.5
  percentile_75 <- total * 0.75
  
  hh_income_one_county <- hh_income_county %>% filter(GEOID == geoid)

  spot_count <- 1
  count_last <- 0
  running_count <- 0
  percentile_25_number<- 0
  percentile_50_number<- 0
  percentile_75_number<- 0
  percentile_25_spot<- 0 
  percentile_50_spot<- 0 
  percentile_75_spot<- 0 
  
  
  for (var in var_list){
    count <- hh_income_one_county %>% filter(variable == var) %>% select(estimate)
    running_count <- running_count + count
    
    if ((percentile_25_number==0)&(running_count > percentile_25)){
      count_difference_perc_25 <- (percentile_25 - count_last) / (running_count - count_last) 
      estimate_difference <- estimate_list[spot_count] - estimate_list[spot_count-1]
      percentile_25_number <- estimate_difference*count_difference_perc_25+ estimate_list[spot_count-1]
      percentile_25_spot <- spot_count
    }
    if ((percentile_50_number==0)&(running_count > percentile_50)){
      count_difference_perc_50 <- (percentile_50 - count_last) / (running_count - count_last) 
      estimate_difference <- estimate_list[spot_count] - estimate_list[spot_count-1]
      percentile_50_number <- estimate_difference*count_difference_perc_50+ estimate_list[spot_count-1]
      percentile_50_spot <- spot_count
    }
    if ((percentile_75_number==0)&(running_count > percentile_75)){
      count_difference_perc_75 <- (percentile_75 - count_last) / (running_count - count_last) 
      estimate_difference <- estimate_list[spot_count] - estimate_list[spot_count-1]
      percentile_75_number <- estimate_difference*count_difference_perc_75+ estimate_list[spot_count-1]
      percentile_75_spot <- spot_count
    }
    spot_count <- spot_count + 1
    count_last <- running_count
    
  }
 # print(geoid)
 # print(percentile_50_number)
  
  hh_income_tract <- get_acs(geography = "tract", table = "B19001",
                             state =state, county = substring(geoid, 3), geometry = FALSE)
  
  tract_geoids <- unique(c(hh_income_tract$GEOID))
  
  for (tract_geoid in tract_geoids){
    print(tract_geoid)
    total_this_tract <- hh_income_tract %>% filter(GEOID == tract_geoid) %>% filter(variable == "B19001_001") %>% select(estimate)
    hh_income_this_tract <- hh_income_tract %>% filter(GEOID == tract_geoid) %>% filter(variable != "B19001_001") %>% select(estimate)
    hh_income_this_tract$cumsum <- cumsum(hh_income_this_tract$estimate)
    percentile_q1_this_tract <- hh_income_this_tract$cumsum[percentile_25_spot-1] + hh_income_this_tract$estimate[percentile_25_spot]* count_difference_perc_25
    percentile_q2_this_tract <- hh_income_this_tract$cumsum[percentile_50_spot-1] - percentile_q1_this_tract + hh_income_this_tract$estimate[percentile_50_spot]* count_difference_perc_50
    percentile_q3_this_tract <- hh_income_this_tract$cumsum[percentile_75_spot-1] - percentile_q2_this_tract - percentile_q1_this_tract + hh_income_this_tract$estimate[percentile_75_spot]* count_difference_perc_75
    percentile_q4_this_tract <- total_this_tract - percentile_q3_this_tract - percentile_q2_this_tract - percentile_q1_this_tract
    
    
    percentile_q1_num_list<- c(percentile_q1_num_list, percentile_q1_this_tract[[1]])
    percentile_q2_num_list<- c(percentile_q2_num_list, percentile_q2_this_tract[[1]])
    percentile_q3_num_list<- c(percentile_q3_num_list, percentile_q3_this_tract[[1]])
    percentile_q4_num_list<- c(percentile_q4_num_list, percentile_q4_this_tract[[1]])
    tract_list <- c(tract_list, tract_geoid)
    total_hh_list <- c(total_hh_list, total_this_tract[[1]])
    county_list <- c(county_list, geoid)
    
    
    
  }
  
}


final_df <- data.frame(tract_list, county_list, total_hh_list, percentile_q1_num_list, 
                       percentile_q2_num_list,percentile_q3_num_list, percentile_q4_num_list)


final_df <- final_df %>% mutate(
  HhPropIncQ1 = percentile_q1_num_list/total_hh_list,
  HhPropIncQ2 = percentile_q2_num_list/total_hh_list,
  HhPropIncQ3 = percentile_q3_num_list/total_hh_list,
  HhPropIncQ4 = percentile_q4_num_list/total_hh_list
) %>% slice(2:n())



hh_income_raw <- get_acs(geography = "tract", table = "B19001",
                            state = state, county = counties, geometry = TRUE) %>% 
                  filter(variable == "B19001_001") %>%
                  select(GEOID)
                    
hh_income_raw <- hh_income_raw %>% merge(final_df, by.x = "GEOID", by.y = 'tract_list') 


#############################################

# Clean tract and bzone geometries --------------
bzone_geometry_sp <- as(bzone_geometry, Class = "Spatial")  #make TAZ df into sp 
hh_income_raw_sp = as_Spatial(hh_income_raw)

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
bzone_geometry_sp_newproj <- spTransform(bzone_geometry_sp, CRS = proj.USGS)
hh_income_raw_sp_newproj <- spTransform(hh_income_raw_sp, CRS = proj.USGS)


# Find intersection between bzone and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(bzone_geometry_sp_newproj, hh_income_raw_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
n<-names(gI) #grab the ids/names from the gIntersection object
n<-data.frame(t(data.frame(strsplit(n," ",fixed=TRUE)))) #ids are combined so split into separate cells
colnames(n)[1:2]<-c("id_bzone","id_tract") #add id names to differentiate


#find the overlapping area for all the bzone-Tract objects
n$area<-sapply(gI@polygons, function(x) x@area) 
a<-data.frame(id=row.names(bzone_geometry_sp_newproj), Bzone = bzone_geometry_sp_newproj$Bzone)#subset bzone dataset so only joining bzone ids
df<-merge(n,a,by.x = "id_bzone", by.y = "id", all.x=TRUE) #merge the bzone ids into our dataset


#find the total area of every census tract
df <- df %>%   group_by(id_tract)%>%
  summarise(shape_area = sum(area))%>%
  right_join(df, by = "id_tract") 


hh_income_raw$id_tract <- seq.int(nrow(hh_income_raw)) #make column so we can join census tract df with intersection df
df2<- merge(df, hh_income_raw, by = "id_tract", by.y = "id_tract", all.x=TRUE)

# Finalize dataframe -------------------------
df3 <- df2 %>% mutate(share.area = area/shape_area, #calculate % of tract in each bzone
                      total_hh_list_this_area = total_hh_list * share.area,
                      percentile_q1_num_list_this_area = percentile_q1_num_list * share.area, # multiply to get SFDU/MFDU in each intersected polygon
                      percentile_q2_num_list_this_area = percentile_q2_num_list * share.area,
                      percentile_q3_num_list_this_area = percentile_q3_num_list * share.area,
                      percentile_q4_num_list_this_area = percentile_q4_num_list * share.area) %>% 
  group_by(Bzone)%>%
  summarise(n = n(),
            tot_hhs = sum(total_hh_list_this_area),
            HhPropIncQ1 = sum(percentile_q1_num_list_this_area)/sum(total_hh_list_this_area), # add up tract-level SFDU/MFDUs for each bzone 
            HhPropIncQ2 = sum(percentile_q2_num_list_this_area)/sum(total_hh_list_this_area),
            HhPropIncQ3 = sum(percentile_q3_num_list_this_area)/sum(total_hh_list_this_area),
            HhPropIncQ4 = sum(percentile_q4_num_list_this_area)/sum(total_hh_list_this_area)) %>%
  mutate(Geo = Bzone,
         GQDU = 1) # no census variable for GQDU. Must use outside data source or assume as 1



# quality check that total households are same in TAZ and census tract files
identical(sum(df3$tot_hhs), sum(hh_income_raw$total_hh_list)) 
  
#duplicate 2019 data for 2045    
df3_copy <- df3
df3$Year <- years[1]
df3_copy$Year <- years[2]

#make final csv file and save to temp directory
bzone_hh_income_quartiles_final <- rbind(df3, df3_copy) %>% select("Geo","Year",'HhPropIncQ1','HhPropIncQ2','HhPropIncQ3','HhPropIncQ4') 
write.csv(bzone_hh_income_quartiles_final, file.path(proj_dir, 'inputs/bzone_hh_inc_qrtl_prop.csv'), row.names = FALSE) #save as csv in final directory




##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############



bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
df3_geo <-st_set_geometry(df3, bzone_geometry_reordered$geometry) 

#Compare proportion of households in 1st quartile
plot(df3_geo['HhPropIncQ1'],
     main = 'Bzone - Proportion of HHs in Lowest Income Quartile')
plot(hh_income_raw['HhPropIncQ1'],
     main = 'Bzone - Proportion of HHs in Lowest Income Quartile')

#Compare proportion of households in 4th quartile
plot(df3_geo['HhPropIncQ4'],
     main = 'Bzone - Proportion of HHs in Highest Income Quartile')
plot(hh_income_raw['HhPropIncQ4'],
     main = 'Bzone - Proportion of HHs in Highest Income Quartile')


bzone_hh_income_raw_sp = as_Spatial(df3_geo)
rgdal::writeOGR(obj = bzone_hh_income_raw_sp,
                dsn = file.path(working_dir, "hh_inc_qrt_shapefile/"),
                layer = 'TAZ_bzone_hh_inc_quartile',
                driver = 'ESRI Shapefile')
