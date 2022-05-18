## Introduction to the sf package
# By Michael Traurig and Alys Young (mostly michael's code)


# set up ------------------------------------------------------------------

## Packages
# read in required libraries
library("sf")
library("units")
library("dplyr")
library("fasterize")
library("terra")

## Functions
# Custom function to erase one vector layer from another
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

## the joy of working in an r project is not having to set the working directory
#set working directory
# basedir <- "e:/temp"
# setwd(basedir)

# data_dir <- paste0(file.path(getwd(), "data")) ## for a full directory
data_dir <- "data"
st_read(file.path(data_dir, "EVC_data.gpkg"))
#Create extant and pre1750 maps based on selection of vegetation groups
st_layers(file.path(data_dir, "EVC_data.gpkg"))
unique(st_read("EVC_data.gpkg", layer = "EVC_extant")$EVC)
unique(st_read("EVC_data.gpkg", layer = "EVC_extant")$XGROUPNAME)
EVC_extant <- filter(st_read("EVC_data.gpkg", layer = "EVC_extant"), EVC == "1105" | EVC == "42" | EVC == "1000") %>% st_union() %>% st_as_sf()
EVC_1750   <- filter(st_read("EVC_data.gpkg", layer = "EVC_1750"), EVC == "1105" | EVC == "42" | EVC == "1000") %>% st_union() %>% st_as_sf()

#Creating an elevation polygon layer and removing lower elevation from EVC layers
rast("vic_EVC_dem.tif")
vic_1370 <- rast("vic_EVC_dem.tif") %>%
 	classify(matrix(c(-60, 1370, NA, 1370, 2000, 1), ncol=3, byrow=TRUE)) %>%
 		 as.polygons() %>%
 		 	union() %>%
 		 		st_as_sf() %>%
 		 			st_make_valid()

EVC_extant_1370 <- st_intersection(EVC_extant, vic_1370)
EVC_1750_1370   <- st_intersection(EVC_1750, vic_1370)

#Finding areas of loss and gain for ecosystem X, Long and short method
#Method one, find area of similarity and remove from extant and pre1750 data
unchanged_EVC 	<- st_intersection(EVC_extant_1370, EVC_1750_1370)
EVC_lost 		<- st_erase(EVC_1750_1370, unchanged_EVC)
EVC_gained 		<- st_erase(EVC_extant_1370, unchanged_EVC)

#Method two, st_difference cuts out a step
EVC_lost2 		<- st_difference(EVC_1750_1370, EVC_extant_1370)
EVC_gained2 	<- st_difference(EVC_extant_1370, EVC_1750_1370)


##Section two urbanisation calculations
r <- rast("population.tif")
sites <- st_read("vic_sites.shp") %>% st_buffer(dist=500, endCapStyle="ROUND")

for (i in 1:length(sites$FID)){
  x <- vect(sites[i,])
  y <- crop(r, x) %>% mask(x) %>% classify(cbind(NA, 0))
  sites$urbanisation[i] <- sum(values(y))
}

#plot high population site
x <- vect(sites[1,])
y <- crop(r, x) %>% mask(x) %>% classify(cbind(NA, 0))
plot(y)
#plot low population site
x <- vect(sites[6,])
y <- crop(r, x) %>% mask(x) %>% classify(cbind(NA, 0))
plot(y)
