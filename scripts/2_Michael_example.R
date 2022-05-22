## Introduction to the sf package

# By Michael Traurig and Alys Young (mostly michael's code)
# May 2022

## TUTE RUNNING SHEET
# 2:30 - 2:35pm start and introduce Michael
# 2:35 - 2:55pm Alys tute on sf generally including some basic live coding
# 2:55 - 3:00pm buffer time
# 3:00 - 3:20pm Michael explain the EVC and elevation ecology example, and walks through this code
# 3:20 - 3:30pm questions



# Set up ------------------------------------------------------------------

## Packages
# read in required libraries
library(sf)        # spatial analysis
library(units)
library(dplyr)     # data cleaning and manipulating
library(fasterize) # turn spatial objects (e.g. polygons) into rasters quickly
library(terra)     # for rasters, newer version of the package raster
library(tmap)      # plotting rasters
library(ggplot2)   # plotting spatial objects

## Functions
# Custom function to erase one vector layer from another
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

## Set the working directory
# dont need to do this because we are using a project



# EVC Data --------------------------------------------------------------------
# Create extant and pre1750 maps based on selection of vegetation groups

## Look at the layers within the EVC_data geopackage
# explain layers
st_layers("data/EVC_data.gpkg")


## Option 1 - Michael's code
# MICHAEL to do - what is this first 2 lines of code doing?
unique(st_read("data/EVC_data.gpkg", layer = "EVC_extant")$EVC)
unique(st_read("data/EVC_data.gpkg", layer = "EVC_extant")$XGROUPNAME)

EVC_extant <- filter(st_read("EVC_data.gpkg", layer = "EVC_extant"), EVC == "1105" | EVC == "42" | EVC == "1000") %>% st_union() %>% st_as_sf()
EVC_1750   <- filter(st_read("EVC_data.gpkg", layer = "EVC_1750"), EVC == "1105" | EVC == "42" | EVC == "1000") %>% st_union() %>% st_as_sf()






## Option 2 - Alys's code
## Read in the 2 data sets which are different layers within the same file

## Data cleaning
# Change the class - do we need this?
# EVC_extant_raw$EVC <- as.character(EVC_extant_raw$EVC) # the class was numeric so changing it

## Data exploration
# Find the unique EVCs
st_read("data/EVC_data.gpkg", layer = "EVC_extant") %>% # the data
  st_drop_geometry %>%                # extracts the dataframe (removes the polygon shapes)
  select(EVC) %>%                     # select one column
  pull %>%                            # turn from a dataframe of one column to a vector
  unique %>%                          # find the unique values
  sort                                # sort the values

# Find the unique group names - same method
st_read("data/EVC_data.gpkg", layer = "EVC_1750") %>%
  st_drop_geometry %>%
  select(EVC) %>%
  pull %>%
  unique %>%
  sort

## filter the data to the EVCs of interest
EVC_extant <- st_read("data/EVC_data.gpkg", layer = "EVC_extant") %>%
  filter(EVC %in% c("1105", "42", "1000")) %>% # filter for the EVCs of interest
  st_union() %>% # disolves the features into 1 feature with a multipolygon
  st_as_sf() # Michael - what is this doing? and why do we need it?

EVC_1750 <- st_read("data/EVC_data.gpkg", layer = "EVC_1750") %>%
  filter(EVC %in% c("1105", "42", "1000")) %>% # filter for the EVCs of interest
  st_union() %>% # Michael - what is this doing? and why do we need it?
  st_as_sf() # Michael - what is this doing? and why do we need it?



## Ways to improve efficiency --------------------------------------------------------------------

# 1. Set the EVCs of interest as its own vector.
# Alys's rule of thumb is if I use it 3 times, or it will be values that change, then turn it into a vector
EVCs_of_interest <- c("1105", "42", "1000")
filter(EVC_1750, EVC %in% EVCs_of_interest)

# 2. Make a function of the same method








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


vicElevation_1370m <- DEM_ras %>% # the data
 	classify(classication_mat) %>%  # define earlier using our elevation of interest as above 1370m
 		 as.polygons() %>%            # turn the raster into polygons so we have polyogns of the high elevation areas
 		 union() %>%                  # make them one polygon - Michael is this correct?
 		 st_as_sf() %>%               # michael - could you please explain why we need this
 		 st_make_valid()              # michael - clarify what this does?

# have a look
vicElevation_1370m


## Find the are that is both the vegetation (ethier extant of in 1750) AND above a certain elevation of 1370m
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




## Practice using dplyr
# calculate the area using st_area()

# 1. as a single value
st_area(EVC_lost2)
st_area(EVC_gained2)

# 2. keeping the dataframe
EVC_lost2 %>% mutate(area = x %>% st_area() %>% as.vector) # x is the geometry column, need as.vector here to remove the units
EVC_gained2 %>% mutate(area = x %>% st_area() %>% as.vector) # x is the geometry column, need as.vector here to remove the units





# Urbanisation - showing how the terra package and sf package interact --------------------------------------------------------------------------------------------------------------------------------------------------------------
## michael - can you explain whats going on here? What are the sites? Can we cut them down to only be like 5-20 sites?
## could these sites start as a csv, load in as a df, and then use st_as_sf() to convert? given that is probably the most popular function

## Population data
# read in the raster
pop_ras <- terra::rast(file.path(data_dir, "population.tif"))

## Michael - what are these sites?
# explanation here

# st_buffer add a buffer around points
# the units is based on what units your projection is in - often meters
sites_points <- st_read(file.path(data_dir,"vic_sites.shp"))
sites <- sites_points %>% st_buffer(dist = 500, endCapStyle = "ROUND")

## Lets have a look
sites_points # points
sites # polygons

class(sites_points)
class(sites)

## Michael - this doesnt plot well. Any thoughts on how to improve? I think zoom in on one section
plot(st_geometry(unchanged_EVC), col = "purple")
plot(st_geometry(EVC_lost2), add = TRUE, col = "red")
plot(st_geometry(EVC_gained2), add = TRUE, col = "blue")

plot(st_geometry(sites), add = TRUE, pch = 17, col = "orange", size = 6)
plot(st_geometry(sites), add = TRUE, pch = 17, col = "orange", size = 6)


for (i in 1:length(sites$FID)){
  one_site <- terra::vect(sites[i,]) # select one site. Why do you want this to be a spatVector rather than an sf object?

  ras <- crop(pop_ras, one_site) %>% # crop the population raster to the site polygon - Michael is this rihgt?
    mask(one_site) %>%               # keep only the values which are inside the one_site polygon
    classify(cbind(NA, 0))           # turn any NA values into a 0

  sites$urbanisation[i] <- sum(values(ras)) # count all the values and save this to a new column called urbanisation on the sites dataframe
}

# have a look at sites and the new column added on the right
sites

## Practice using dplyr
sites %>%
  mutate(size = ifelse(urbanisation > 1000, "Yes", "No")) %>%
  st_drop_geometry %>%
  group_by(size) %>%
  summarize(n = n(),
            mean = mean(urbanisation))


# find the maximum value of urbanisation
## Option 1 - without dplyr
# max(sites$urbanisation) # whats the maximum value?
# which.max(sites$urbanisation) # which site has the maximum value?
# sites[which.max(sites$urbanisation),] # show this site
# site_maxurban <- vect(sites[which.max(sites$urbanisation),])


## Option 2 - with dplyr
site_maxurban_sf <- sites %>%
  filter(urbanisation == max(urbanisation))

site_maxurban <- sites %>%
  filter(urbanisation == max(urbanisation)) %>%  ## lets filter for the highest urban
  terra::vect(.)
urban_ras_maxsite <- crop(pop_ras, site_maxurban) %>%
  mask(site_maxurban) %>%  # to mask, it needs to be in the correct class. currently its sf and terra::vect() turns it into a SpatVector
  classify(cbind(NA, 0))



# Plotting ----------------------------------------------------------------

## Base R - does fine, bit slow. remember to use add = TRUE
plot(urban_ras_maxsite) # the urban raster?
plot(site_maxurban, add = TRUE) # the 500m buffer area
plot(sites_points[which.max(sites$urbanisation),], add = TRUE) # the site centre point
















