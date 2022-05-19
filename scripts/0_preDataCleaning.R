## Pre data cleaning


# set up - packages -------------------------------------------------------


library("sf")
library("units")
library("dplyr")
library("fasterize")
library("terra")


# functions ---------------------------------------------------------------
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

#set working directory
uncleanData_dir <- "e:/temp"
setwd(basedir)

#Creating source data files for presentation that are more confined for easier download
#reading cropping and storing past and present EVC files
st_read(file.path(uncleanData_dir, "NV2005_EVCBCS.shp")) %>% st_crop(ext(370000, 694000, 5176000, 6097000)) %>% st_cast("POLYGON") %>% st_write("EVC_data.gpkg", layer = "EVC_extant")
st_read(file.path(uncleanData_dir,"NV1750_EVC.shp")) %>% st_crop(ext(370000, 694000, 5176000, 6097000)) %>% st_cast("POLYGON") %>% st_write("EVC_data.gpkg", layer = "EVC_1750")
st_layers(file.path(uncleanData_dir,"EVC_data.gpkg")) #did they store correctly

#reading croping and reprojecting digital elevation raster
r	<- rast(file.path(uncleanData_dir, "dem1sv1_0")) %>% crop(ext(145.3905, 149.1324, -43.55763, -35.25134)) %>% project(vect("EVC_data.gpkg", layer = "EVC_extant")) %>% mask(vect("EVC_data.gpkg", layer = "EVC_extant")) %>% crop(ext(vect("EVC_data.gpkg", layer = "EVC_extant")))
writeRaster(r, file.path(uncleanData_dir, "vic_EVC_dem.tif"), overwrite=TRUE)

