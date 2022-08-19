## Introduction to the sf package
C:\Users\syntr\R
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
library(tidyverse)
library(units)
library(fasterize) # turn spatial objects (e.g. polygons) into rasters quickly
library(terra)     # for rasters, newer version of the package raster
library(tmap)      # plotting rasters

## Functions

#erases areas that overlap between x and y from x. This function simplifies y in order to reduce computational time of operation (which st_difference alone does not)
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

#Creates a polygon from a DEM that covers stipulated elevation and above. Where f is the source DEM file name, x is a value below the minimum elevation of the dem, y is the elevation cut off, z is a value above top of DEM range, and t is layer name for polygon
st_demtopoly = function(f,y,t) { rast(f) %>%
                                     classify(matrix(c(minmax(.)[1] -1, y, NA, y, minmax(.)[2]+1, 1), ncol=3, byrow=TRUE)) %>%
                                     as.polygons() %>%
                                     st_as_sf() %>%
                                     st_transform(st_crs(28355)) %>%
                                     st_make_valid() %>%
                                     dplyr::select() %>%
                                     st_write("sf_tutorial_dem.gpkg", layer = t)
}

#Extracts a subset of data from your source geopackage and, then transforms attribute data to match how map layers will store data.
st_src_extract = function(x, y, z) { filter(st_read("sf_tutorial_data.gpkg", layer = x), get(y) %in% z) %>%
                                     dplyr::select(y) %>%
                                     mutate(source = x) %>%
                                     rename(veg_id = y)
}

#Selects a single attribute from source dataset then intersects with an elevation polygon
st_src_intersect = function(x, y, z) { st_read("st_tutorial_data.gpkg", layer = x) %>%
                                       dplyr::select(y) %>%
                                       mutate(source = x) %>%
                                       rename(veg_id = y) %>%
                                       st_intersection(st_read("sf_tutorial_dem", layer = z))
}

#Stores completed outputs into a geopackage
st_store = function(x,y) {st_make_valid(x) %>%
                          st_cast("MULTIPOLYGON") %>%
                          st_write("sf_tutorial_outputs.gpkg", layer = y)
}

## Set the working directory
setwd("c:/sf_demo/")

#Creating state level digital elevation model files, full DEM is available at ga.gov.au (https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/72759)
st_layers("border_data.gpkg")

# Create state level DEM for victoria
rast("dem1sv1_0") %>% crop(ext(145.3905, 149.1324, -43.55763, -35.25134)) %>%
                      project(crs(vect("border_data.gpkg", layer = "vic_border"))) %>%
                      crop(vect("border_data.gpkg", layer = "vic_border")) %>%
                      mask(vect("border_data.gpkg", layer = "vic_border")) %>%
                      writeRaster("vic_dem.tif")

# Create state level DEM for NSW + ACT
rast("dem1sv1_0") %>% crop(ext(145.3905, 149.1324, -43.55763, -35.25134)) %>%
                      project(crs(vect("border_data.gpkg", layer = "nsw_act_border"))) %>%
                      crop(vect("border_data.gpkg", layer = "nsw_act_border")) %>%
                      mask(vect("border_data.gpkg", layer = "nsw_act_border")) %>%
                      writeRaster("nsw_dem.tif")



####Look at data, and select our vegetation groups
st_layers("sf_tutorial_data.gpkg")
#Victoria
view(head(st_read("sf_tutorial_data.gpkg", layer = "EVC_extant")))
names(st_read("sf_tutorial_data.gpkg", layer = "EVC_extant"))
unique(st_read("sf_tutorial_data.gpkg", layer = "EVC_extant")$EVC)
unique(st_read("sf_tutorial_data.gpkg", layer = "EVC_extant")$X_EVCNAME)
vic_vegclass <- c("Sub-alpine Woodland", "Sub-alpine Wet Heathland/Sub-alpine Grassland Mosaic" , "Grassy Woodland", "Sub-alpine Dry Shrubland")

#NSW
view(head(st_read("sf_tutorial_data.gpkg", layer = "NSW3858")))
names(st_read("sf_tutorial_data.gpkg", layer = "NSW3858"))
unique(st_read("sf_tutorial_data.gpkg", layer = "NSW3858")$VEG_GROUP)
NSW_vegclass <- c("Alpine Rocky Low Open Heathland", "Sub-alpine Shrub-Grass Woodland", "Sub-alpine Dry Shrub-Herb Woodland", "ACT Montane Dry Shrub-Grass Forest")

####Create elevation polygons for each state
st_demtopoly

#run line 106 and 113 before talking
#Victoria
rast("vic_dem.tif")
plot(rast("vic_dem.tif"))
st_demtopoly("vic_dem.tif", 1200, "vic_1200") #create a DEM polygon for victoria at 1200m and above
#Lets take a look at what we just created!
plot(rast("vic_dem.tif"))
plot(st_read("sf_tutorial_dem.gpkg", layer = "vic_1200"), add = TRUE)

#NSW/ACT
rast("nsw_dem.tif")
st_demtopoly("nsw_dem.tif", 1300, "nsw_1300") #create a DEM polygon for NSW at 1100m and above
plot(rast("nsw_dem.tif"))
plot(st_read("sf_tutorial_dem.gpkg", layer = "nsw_1300"), add = TRUE)


####Create present day distribution
st_src_extract
st_src_extract("EVC_extant", "X_EVCNAME", vic_vegclass) %>% st_intersection(st_read("sf_tutorial_dem.gpkg", layer = "vic_1200")) %>% #extract victorian vegetation groups, and cut to 1200m elevation and above
        rbind(st_src_extract("NSW3858", "VEG_GROUP", NSW_vegclass) %>% st_intersection(st_read("sf_tutorial_dem.gpkg", layer = "nsw_1300"))) %>% #extract NSW/ACT vegetation groups, and cut to 1300m elevation and above
        st_store("toy_eco_extant") #Store extant ecosystem into geopackage

plot(st_geometry(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_extant")))
plot(st_read("sf_tutorial_dem.gpkg", layer = "vic_1200"), col  = "green", add = TRUE)
plot(st_read("sf_tutorial_dem.gpkg", layer = "nsw_1300"), add = TRUE, col  = "purple")
plot(st_geometry(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_extant")), add = TRUE, col = "yellow")

####Create pre1750 distribution
#Copy extant code and change to pre1750!
st_layers("sf_tutorial_data.gpkg")
st_src_extract("EVC_1750", "X_EVCNAME", vic_vegclass) %>% st_intersection(st_read("sf_tutorial_dem.gpkg", layer = "vic_1200")) %>% #extract victorian vegetation groups, and cut to 1200m elevation and above
        rbind(st_src_extract("NSW3859", "VEG_GROUP", NSW_vegclass) %>% st_intersection(st_read("sf_tutorial_dem.gpkg", layer = "nsw_1300"))) %>% #extract NSW/ACT vegetation groups, and cut to 1300m elevation and above
        st_store("toy_eco_pre1750") #Store extant ecosystem into geopackage

view(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_pre1750"))

# Areas of change ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Identify the areas of loss and gain for ecosystem X, Long and short method


## Method one ------------------------------------------------------------------------------------------------------------------------------------------------
# find area of similarity and remove from extant and pre1750 data
# st_erase is a custom function we defined earlier, not automatically part of SF package

 # find the area that is unchanged - the ecosystem is there in both 1750 and now
unchanged   <- st_intersection(st_union(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_extant")), 
                                st_union(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_pre1750"))) %>% st_as_sf()

#This will take some time to calculate run and switch to second section
lost 	  	  <- st_erase(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_pre1750"), unchanged) # remove the unchanged area from the 1750 map - gives us what was lost
gained 		  <- st_erase(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_extant"), unchanged) # remove the unchanged area from the current map - gives us what was gained

## Method two ------------------------------------------------------------------------------------------------------------------------------------------------
# st_difference cuts out a step

lost2        <- st_difference(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_pre1750"), 
                              st_union(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_extant")))

gained2      <- st_difference(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_extant"),
                              st_union(st_read("sf_tutorial_outputs.gpkg", layer = "toy_eco_pre1750")))


#Not a great plot!
ggplot() + 
geom_sf(data=st_crop(lost, ext(500000, 533104.694534574, 5807326.3940318, 5940129.63613299)), col = "red") +
geom_sf(data=st_crop(gained, ext(500000, 533104.694534574, 5807326.3940318, 5940129.63613299)), col = "goldenrod")
#problems with mapping on large scales with polygons that are close to each other.. is this representative?


#Both methods produce an identical product.
sum(st_area(lost))
sum(st_area(lost2))

#lets add these layers to our output geopackage
st_write(unchanged, "sf_tutorial_outputs.gpkg", layer = "unchanged")
st_write(lost, "sf_tutorial_outputs.gpkg", layer = "lost")
st_write(gained, "sf_tutorial_outputs.gpkg", layer = "gained")

#All contained in one file!
st_layers("sf_tutorial_outputs.gpkg")

#Take a look at what data is contained in the lost file
head(lost)

# calculate the area using st_area()

# 1. as a single value
sum(st_area(st_read("sf_tutorial_outputs.gpkg", layer = "lost")))
sum(st_area(st_read("sf_tutorial_outputs.gpkg", layer = "gained")))

# 2. keeping the dataframe
lost2 %>% mutate(area = geom %>% st_area() %>% as.vector) # need as.vector here to remove the units
gained2 %>% mutate(area = geom %>% st_area() %>% as.vector) # need as.vector here to remove the units

#Calculate loss for each vegetation group
lost2 %>% mutate(area = geom %>% st_area() %>% as.vector) %>%
  as.data.frame() %>%
  group_by(veg_id) %>% 
  summarize(area_lost_km2 = sum(area)/1000) %>% view()

# Urbanisation - showing how the terra package and sf package interact --------------------------------------------------------------------------------------------------------------------------------------------------------------

## Population data
# read in the raster
pop_ras <- rast(file.path("population.tif"))


# These sites represent locations students went to do surveys for an undergraduate class. They are a good representation for both urban and remote locations

# st_buffer add a buffer around points
# the units is based on what units your projection is in - often meters
sites_points <- read.csv(file.path("sites.csv")) %>% 
                st_as_sf(coords = c("X","Y"), crs= crs(pop_ras)) %>%
                rename(FID = "X.1")

sites <- sites_points %>% st_buffer(dist = 500, endCapStyle = "ROUND")

## Lets have a look
sites_points # points
sites # polygons

##Plotting population raster 
pop_ras %>% as.data.frame(xy=TRUE) %>%
ggplot() + 
xlab("") + ylab("") +
geom_raster(aes(x = x , y = y)) +
geom_sf(data = (sites), colour = "orange", size = .5)

for (i in 1:length(sites$FID)){
  one_site <- terra::vect(sites[i,]) # select one site. Why do you want this to be a spatVector rather than an sf object?

  ras <- crop(pop_ras, one_site) %>% # crop the population raster to the site polygon - Michael is this rihgt?
    mask(one_site)                   # keep only the values which are inside the one_site polygon

  sites$urbanisation[i] <- sum(na.omit(values(ras))) # count all the values and save this to a new column called urbanisation on the sites dataframe
}
    
# have a look at sites and the new column added on the right
head(sites)

## Practice using dplyr
sites %>%
  mutate(size = ifelse(urbanisation > 1000, "Yes", "No")) %>%
  st_drop_geometry %>%
  group_by(size) %>%
  summarize(n = n(),
            mean = mean(urbanisation))

# find the maximum value of urbanisation with dplyr and then plot it!

site_maxurban <- sites %>% #Create a vector of the site with the max urbanisation
  filter(urbanisation == max(urbanisation)) %>% vect()

urban_ras_maxsite <- crop(pop_ras, site_maxurban) %>% #crop and mask the population raster by our max urbanisation site
  mask(site_maxurban) %>% 
  classify(cbind(NA, 0))

# Plotting ----------------------------------------------------------------

## Base R - does fine, remember to use add = TRUE
plot(urban_ras_maxsite) # the urban raster?
plot(site_maxurban, add = TRUE) # the 500m buffer area
plot(sites_points[which.max(sites$urbanisation),], add = TRUE) # the site centre point