## Introduction to the sf package
# By Michael Traurig and Alys Young (mostly michael's code)



## IMPORTANT THINGS TO INCLUDE
# [X] st_write
# [] st_crs
# [] st_transform
# [X] st with %>% mutate()
# [X] st_area
# [] st_join


# Set up ------------------------------------------------------------------

## Packages
# read in required libraries
library(sf)        # spatial analysis
library(units)
library(dplyr)     # data cleaning and manipulating
library(fasterize) # turn spatial objects (e.g. polygons) into rasters quickly
library(terra)     # for rasters, newer version of the package raster

## Functions
# Custom function to erase one vector layer from another
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

## the joy of working in an r project is not having to set the working directory
#set working directory
# basedir <- "e:/temp"
# setwd(basedir)

data_dir <- paste0(file.path(getwd(), "data")) ## for a full directory
data_dir <- "data"

## Other functions that we use a lot here
?file.path
# creates a file path. You put in the names of the folders and the function fills in the / inbetween


# EVC Data --------------------------------------------------------------------

#Create extant and pre1750 maps based on selection of vegetation groups

## Look at the layers within the EVC_data geopackage
# layers are the different sets of data or maps contained within the one file
st_layers(file.path(data_dir, "EVC_data.gpkg"))


## Create extant and pre1750 maps based on selection of vegetation groups

## Option 1 - Michael's code
# MICHAEL to do - what is this first 2 lines of code doing?
unique(st_read(file.path(data_dir, "EVC_data.gpkg"), layer = "EVC_extant")$EVC)
unique(st_read(file.path(data_dir, "EVC_data.gpkg"), layer = "EVC_extant")$XGROUPNAME)

EVC_extant <- filter(st_read("EVC_data.gpkg", layer = "EVC_extant"), EVC == "1105" | EVC == "42" | EVC == "1000") %>% st_union() %>% st_as_sf()
EVC_1750   <- filter(st_read("EVC_data.gpkg", layer = "EVC_1750"), EVC == "1105" | EVC == "42" | EVC == "1000") %>% st_union() %>% st_as_sf()






## Option 2 - Alys's code
## Read in the 2 data sets which are different layers within the same file
EVC_extant_raw <- st_read(file.path(data_dir, "EVC_data.gpkg"), layer = "EVC_extant")
EVC_1750_raw <- st_read(file.path(data_dir, "EVC_data.gpkg"), layer = "EVC_1750")

class(EVC_extant_raw)
class(EVC_1750_raw)

EVC_extant_raw
head(EVC_1750_raw)

## Michael can we add st_crs in here? just to look

## Data cleaning
# Change the class
EVC_extant_raw$EVC <- as.character(EVC_extant_raw$EVC) # the class was numeric so changing it

## Data exploration
# Unique EVCs
EVC_extant_EVCs <- EVC_extant_raw %>%
  st_drop_geometry %>%
  select(EVC) %>%
  pull %>%
  unique %>%
  sort

# Unique group names
EVC_extant_GroupName <- EVC_extant_raw %>%
  st_drop_geometry %>%
  select(EVC) %>%
  pull %>%
  unique %>%
  sort

## filter the data to the EVCs of interest
EVC_extant <- EVC_extant_raw %>%
  filter(EVC %in% c("1105", "42", "1000")) %>% # filter for the EVCs of interest
  st_union() %>% # disolves the features into 1 feature with a multipolygon
  st_as_sf() # Michael - what is this doing? and why do we need it?

EVC_1750 <- EVC_1750_raw %>%
  filter(EVC %in% c("1105", "42", "1000")) %>% # filter for the EVCs of interest
  st_union() %>% # Michael - what is this doing? and why do we need it?
  st_as_sf() # Michael - what is this doing? and why do we need it?


## Ways to improve efficiency --------------------------------------------------------------------

# 1. Set the EVCs of interest as its own vector.
EVCs_of_interest <- c("1105", "42", "1000")

# 2. Make a function
filter_to_EVC <- function(data, EVCs_of_interest){
  data %>%
    filter(EVC %in% EVCs_of_interest) %>%
    st_union() %>%
    st_as_sf()

}

EVC_extant_2 <- filter_to_EVC(data = EVC_extant_raw, EVCs_of_interest = EVCs_of_interest)
EVC_1750_2 <- filter_to_EVC(data = EVC_1750_raw, EVCs_of_interest = EVCs_of_interest)

# Check the function actually returns the correct output
identical(EVC_extant, EVC_extant_2)
identical(EVC_1750, EVC_1750_2)








# Cut the data to a certain area ---------------------------------------------------------------
# Here the area is defined as above a specific elevation

# Aim: Creating an elevation polygon layer and removing lower elevation from EVC layers

## Read in the DEM raster
DEM_ras <- rast(file.path(data_dir, "vic_EVC_dem.tif"))

DEM_ras

class(DEM_ras)


## define how we want the raster reclassified
# Our area of interest is above the 1370m elevation
# Anything from -60 to 1370 elevation will we will reclassify to a NA
# Anything above the 1370m elevation will we will reclassify as a 1
classication_mat <- matrix(c(-60,  1370, NA,
                             1370, 2000, 1),
                           ncol  = 3,
                           byrow = TRUE)
# Have a look
classication_mat
# The first column in the lower bounds, the second is the upper and the 3rd is what it will be reclassified as

## Michael - whats the matrix doing?
vicElevation_1370m <- DEM_ras %>%
 	classify(classication_mat) %>% # define earlier using our elevation of interest as above 1370m
 		 as.polygons() %>% # turn the raster into polygons so we have polyogns of the high elevation areas
 		 union() %>% # make them one polygon - Michael is this correct?
 		 st_as_sf() %>%
 		 st_make_valid() # michael - clarify what this does?

vicElevation_1370m

## Michael - can we break this step up by adding in looking at the crs of the polygons? Maybe make sure the crs is different to the EVCs, plot them to show its different, then use st_tranform


## Next function to use is st_intersection
# this find the areas where the two sf object intersect and only keep the intersecting areas
?st_intersection
# this is opposed to the st_intersect function (very confusing) which find which object intersect and then keeps the entire object (doesnt cut it)
?st_intersect

EVC_extant_1370 <- st_intersection(EVC_extant, vicElevation_1370m)
EVC_1750_1370   <- st_intersection(EVC_1750, vicElevation_1370m)

st_is_valid(EVC_extant_1370)
st_is_valid(EVC_1750_1370)

# Areas of change ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Identify the areas of loss and gain for ecosystem X, Long and short method


## Method one ------------------------------------------------------------------------------------------------------------------------------------------------
# find area of similarity and remove from extant and pre1750 data

# st_erase is the function defined by us earlier
## Alys gets various errors here that look like this Error in CPL_geos_op2(op, x, y) :
# "Evaluation error: TopologyException: found non-noded intersection between"

unchanged_EVC   <- st_intersection(EVC_extant_1370, EVC_1750_1370) # find the area that is unchanged - the ecosystem is there in both 1750 and now
EVC_lost 	  	  <- st_erase(EVC_1750_1370, unchanged_EVC) # remove the unchanged area from the 1750 map - gives us what was lost
EVC_gained 		  <- st_erase(EVC_extant_1370, unchanged_EVC) # remove the unchanged area from the current map - gives us what was gained

st_is_valid(unchanged_EVC)

plot(st_geometry(unchanged_EVC), col = "purple")
plot(st_geometry(EVC_lost), add = TRUE, col = "red")
plot(st_geometry(EVC_gained), add = TRUE, col = "blue")



## Method two ------------------------------------------------------------------------------------------------------------------------------------------------
# st_difference cuts out a step
# This worked for Alys but method 1 didnt

EVC_lost2 		   <- st_difference(EVC_1750_1370, EVC_extant_1370)
EVC_gained2 	   <- st_difference(EVC_extant_1370, EVC_1750_1370)


## Michael - can we add st_write in here?
# example 1 - write 2 new files
st_write(EVC_lost2, "output/EVC_arealost.gpkg")
st_write(EVC_gained2, "output/EVC_areagained.gpkg")

# example 2 - write 1 file with 2 layers
st_write(EVC_lost2, "output/EVC_areachange.gpkg", layer = "lost")
st_write(EVC_gained2, "output/EVC_areachange.gpkg", layer = "gained")




## Using with dplyr again
# calculate the area using st_area()

# 1. as a single value
st_area(EVC_lost2)
st_area(EVC_gained2)

# 2. keeping the dataframe
EVC_lost2 %>% mutate(area = x %>% st_area() %>% as.vector) # x is the geometry column, need as.vector here to remove the units
EVC_gained2 %>% mutate(area = x %>% st_area() %>% as.vector) # x is the geometry column, need as.vector here to remove the units





# Urbanisation ---------------------------------------------------------------------------------------------------------------------------------------------------------------
## michael - can you explain whats going on here?

## Population data
# read in the raster
pop_ras <- terra::rast(file.path(data_dir, "population.tif"))

## Michael - what are these sites?
# explanation here

# st_buffer add a buffer around points
# the units is based on what units your projection is in - often meters
sites_points <- st_read(file.path(data_dir,"vic_sites.shp"))
sites <- sites_points %>% st_buffer(dist = 500, endCapStyle = "ROUND")

sites_points
sites

class(sites_points)
class(sites)

## this doesnt plot well
plot(st_geometry(unchanged_EVC), col = "purple")
plot(st_geometry(EVC_lost2), add = TRUE, col = "red")
plot(st_geometry(EVC_gained2), add = TRUE, col = "blue")

plot(st_geometry(sites), add = TRUE, pch = 17, col = "orange", size = 6)
plot(st_geometry(sites), add = TRUE, pch = 17, col = "orange", size = 6)


for (i in 1:length(sites$FID)){
  one_site <- terra::vect(sites[i,]) # select one site. Why do you want this to be a spatVector rather than an sf object?
  ras <- crop(pop_ras, one_site) %>% # crop the population raster to the site polygon - Michael is this rihgt?
    mask(one_site) %>% # keep only the values which are inside the one_site polygon
    classify(cbind(NA, 0)) # turn any NA values into a 0
  sites$urbanisation[i] <- sum(values(ras)) # count all the values and save this to a new column called urbanisation on the sites dataframe
}

# have a look at sites and the new column added on the right
sites

# lets use dplyr with an sf object for interest
# basic mutate function
sites %>% mutate(urban_round = round(urbanisation, 1)) # round the urbanisation to 1 dp

# stringing functions together
sites %>% filter(urbanisation > 1000)
sites %>% mutate(size = ifelse(urbanisation > 1000, "Yes", "No"))
sites %>% mutate(size = ifelse(urbanisation > 1000, "Yes", "No")) %>% st_drop_geometry %>%  group_by(size) %>%  summarize(n = n(),                                                                                                                       mean = mean(urbanisation))


# find the maximum value of urbanisation
## Option 1
# max(sites$urbanisation) # whats the maximum value?
# which.max(sites$urbanisation) # which site has the maximum value?
# sites[which.max(sites$urbanisation),] # show this site
# site_maxurban <- vect(sites[which.max(sites$urbanisation),])


## Option 2
site_maxurban_sf <- sites %>%
  filter(urbanisation == max(urbanisation))

site_maxurban <- sites %>%
  filter(urbanisation == max(urbanisation)) %>%  ## lets filter for the highest urban
  terra::vect(.)
urban_ras_maxsite <- crop(pop_ras, site_maxurban) %>%
  mask(site_maxurban) %>%  # to mask, it needs to be in the correct class. currently its sf and terra::vect() turns it into a SpatVector
  classify(cbind(NA, 0))

plot(urban_ras_maxsite) # the urban raster?
plot(site_maxurban, add = TRUE) # the 500m buffer area
plot(sites_points[which.max(sites$urbanisation),], add = TRUE) # the site centre point

library(tmap)
tm_map <- tm_shape(urban_ras_maxsite) + tm_raster() # plot the urban raster only
tm_map

# add the other spatial objects on top
tm_map +
  tm_shape(site_maxurban_sf) + ## add the 500m site buffer to the plot
  tm_borders() +
  tm_shape(sites_points[which.max(sites$urbanisation),]) + # the site centre
  tm_dots(size = 2)


library(ggplot2)
urban_maxsite_df <- urban_ras_maxsite %>% as.data.frame(xy = TRUE) %>%
  na.omit

## plot just the raster
ggplot() +
  geom_raster(data = urban_maxsite_df, aes(x = x, y = y, fill = population))

# Michael - Same issue as the ggplot code down the bottom. wrong coords? the plot turns into long lat
ggplot() +
  geom_sf(data = site_maxurban_sf) # plots in long lat


## Michael I think we delete this because its the same process as the maximum one
# # plot highest population site
# site_minurban <- vect(sites[which.min(sites$urbanisation),])
# urban_ras_minsite <- crop(pop_ras, site_minurban) %>%
#   mask(site_minurban) %>%
#   classify(cbind(NA, 0))
#
# plot(urban_ras_minsite) # the urban raster?
# plot(site_minurban, add = TRUE) # the 500m buffer area
# plot(sites_points[which.min(sites$urbanisation),], add = TRUE) # the site centre point
#


## st_intersect
# intersect the points with something else like the original EVC





# MICHAEL - this plotting changes the crs weird and I couldnt figu --------

library(ggplot2)

# to plot a SpatRaster in ggplot it has to be a dataframe
urban_maxsite_df <- urban_ras_maxsite %>% as.data.frame(xy = TRUE) %>% na.omit

## plot just the raster - this works well!
ggplot() +
  geom_raster(data = urban_maxsite_df, aes(x = x, y = y, fill = population))

# plot the raster together with the point - bad bad not good. points change to long lat?
ggplot() +
  geom_raster(data = urban_maxsite_df, aes(x = x, y = y, fill = population)) +
  geom_sf(data = sites_points2)

# points only - still long lat!
ggplot() +
  geom_sf(data = sites_points, aes(geometry = geometry)) # plots in long lat


## ok lets check the crs
st_crs(sites_points) # GDA94 / MGA zone 55
st_crs(urban_ras_maxsite) # GDA_1994_MGA_Zone_55
# both GDA94

# but not the same
identical(st_crs(sites_points), st_crs(urban_ras_maxsite))

# projecct the points to be the same as the urbanisation rasters
sites_points2 <- st_transform(sites_points, crs = st_crs(urban_ras_maxsite))

# and they are the same
identical(st_crs(sites_points2), st_crs(urban_ras_maxsite))


# Plot again with the points "transformed" - still bad!!
ggplot() +
  geom_sf(data = sites_points2) # plots in long lat

## ok lets change the points to a dataframe by pulling out the long and lat from the geomtery
sites_points2$long <- st_coordinates(sites_points2)[,"X"] %>% as.vector
sites_points2$lat <- st_coordinates(sites_points2)[,"Y"]%>% as.vector
sites_points_df <- sites_points2 %>%  st_drop_geometry() # its a simple dataframe now

# plot with geom_point instead because its not spatial - looks better
ggplot() +
  geom_point(data = sites_points_df, aes(x = long, y = lat))

# Still bad?????
ggplot() +
  geom_raster(data = urban_maxsite_df, aes(x = x, y = y, fill = population)) +
  geom_point(data = sites_points_df, aes(x = long, y = lat))

