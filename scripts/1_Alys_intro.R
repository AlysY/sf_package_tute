## Introduction to the sf package

# By Alys Young and Michael Traurig
# May 2022

# Resources:
# https://r-spatial.github.io/sf/

## setting the plotting margins - so it zooms in more
par(mar = c(0,0,0,0))

# Set up ------------------------------------------------------------------

## Read in packages - remember to install them first if you havent used them before
library(sf)        # spatial analysis
library(dplyr)     # data cleaning and manipulating
library(tmap)      # plotting - static
library(ggplot2)   # plotting spatial objects - static but many extensions
library(leaflet)   # plotting spatial objects - interactive
library(mapview)   # plotting spatial objects - interactive



# read in the data --------------------------------------------------------
## New functions:
?st_read
?st_write
?st_as_sf

## Melbourne parks - multipolygon
parks <- st_read("data/basic_intro/parks.gpkg")

## My survey sites - dataframe
my_sites_df <- read.csv("data/basic_intro/survey_sites.csv")
my_sites    <- st_as_sf(x = my_sites_df, coords = c("long", "lat"), crs = 4326) # EPSG for WGS84, what my gps was set in.

## Save the points
st_write(my_sites, "data/survey_sites_points.gpkg")

## for plotting, state boundary
vic <- st_read("data/basic_intro/vic.gpkg")





# projections --------------------------------------------------------------
## New functions:
?st_crs
?st_transform
?st_geometry


## our data
parks    # Note: Projected CRS: GDA2020 / MGA zone 55
my_sites # Geodetic CRS:  WGS 84

## Check the crs using st_crs()
st_crs(parks)
st_crs(my_sites)
# note: they are different. need to reproject my_sites from a geographic crs (WGS84) to a projected crs for melbourne (e.g. MGA2020 55)

## two methods:
  # 1. using the EPSG, or the crs
my_sites_p <- st_transform(my_sites, crs = 7855)

  # 2. extract the crs from another layer that you know is correct
my_sites_p <- st_transform(my_sites, crs = st_crs(parks))

# get the new coordinates
st_coordinates(my_sites_p)

## NOTE: transforming vs specifying the crs
# If you load a layer and no crs appear, but YOU KNOW what it is
  # then it is ok to "set" or specify the crs using
  # st_crs(my_layer) <- "the_true_crs"
  # or my_layer %>% st_set_crs("the_true_crs")
# If your layer has a crs and you want to CHANGE it
  # you MUST transform it

# plot them
plot(st_geometry(parks))                                     # to zoom to the right area
plot(st_geometry(vic),        add = TRUE, col = "lightgrey") # beacuse this is a polygon it plots over the top and covers the parks
plot(st_geometry(parks),      add = TRUE, col = "yellow")    # so add the parks again
plot(st_geometry(my_sites),   add = TRUE, col = "blue")      # dont appear - wrong projection

plot(st_geometry(my_sites_p), add = TRUE, col = "red", pch = 17)


# why use st_geometry ?
# if your data has columns, each will be plotted
# e.g. sites has 2 attributes (check the dataframe to see them) so the points plot twice
plot(my_sites)




# Normal data cleaning ----------------------------------------------------------------
park_of_interest <- parks[parks$NAME == "Albert Park",]
park_of_interest <- parks %>% filter(NAME == "Albert Park")

# plot
plot(st_geometry(parks))
plot(st_geometry(vic),              add = TRUE, col = "lightgrey")
plot(st_geometry(parks),            add = TRUE, col = "yellow")
plot(st_geometry(park_of_interest), add = TRUE, col = "green")
plot(st_geometry(my_sites_p),       add = TRUE, col = "red", pch = 17)






# Joining attributes based on geometry ------------------------------------------------------
## New functions:
?st_join

# for my sites, as the data from the park layer where the sites are
my_sites_dat <- st_join(my_sites_p, parks)
my_sites_dat

# Add the data from my sites to the parks layer
my_parks_dat <- st_join(parks, my_sites_p)
my_parks_dat # see the NAs for the park polygons where I have no sites intersecting



# Buffering ---------------------------------------------------------------
## New functions:
?st_buffer

# around a polygon:
park_oi_buf <- st_buffer(park_of_interest, dist = 2000)

# around a point:
my_sites_buf <- st_buffer(my_sites_dat, dist = 500)

# compare: the point and the buffered points (now polygons)


plot(st_geometry(parks))
plot(st_geometry(vic),              add = TRUE, col = "lightgrey")
plot(st_geometry(parks),            add = TRUE, col = "yellow")
plot(st_geometry(park_of_interest), add = TRUE, col = "green")
plot(st_geometry(my_sites_p),       add = TRUE, col = "red", pch = 17)
# Add the buffers
plot(st_geometry(park_oi_buf),      add = TRUE)
plot(st_geometry(my_sites_buf),     add = TRUE)




# Intersecting ------------------------------------------------------------
## New functions:
?st_intersects
?st_intersection

# parks that are within 2km of our park of interest, Albert Park
# Keep the parks that intersect the 2km buffer
parks_intersect_list <- st_intersects(park_oi_buf, parks)
# returns a list
parks_intersect_list
# get the polygons to keep - the ones that intersect
parks_intersect_list[[1]]

parks_intersect <- parks[parks_intersect_list[[1]],]
parks_non_intersect <- parks[!parks_intersect_list[[1]],]


plot(st_geometry(parks))
plot(st_geometry(vic),              add = TRUE, col = "lightgrey")
plot(st_geometry(parks),            add = TRUE, col = "yellow")
plot(st_geometry(park_of_interest), add = TRUE, col = "green")
plot(st_geometry(park_oi_buf),      add = TRUE)
plot(st_geometry(parks_intersect),  add = TRUE, col = "darkgreen")







# Keep parks that are within 500m of our sites
my_site_parks_intersect <- st_intersection(my_sites_buf, parks)
# returns an sf object
my_site_parks_intersect

plot(st_geometry(parks))
plot(st_geometry(vic),                     add = TRUE, col = "lightgrey")
plot(st_geometry(parks),                   add = TRUE, col = "yellow")
plot(st_geometry(my_sites_p),              add = TRUE, pch = 17, col = "red")
plot(st_geometry(my_sites_buf),            add = TRUE)
plot(st_geometry(my_site_parks_intersect), add = TRUE, col = "darkred")




# Measurements ------------------------------------------------------------
## New functions:
?st_area
?st_distance

# Area
st_area(my_site_parks_intersect)

# Distance
st_distance(my_sites_p)


# dplyr -------------------------------------------------------------------

# Calculate the area
my_parks_full <- my_site_parks_intersect %>% mutate(area = st_area(.))


## Q: how many other parks are within 2km of our park of interest (Albert Park)?
plot(st_geometry(parks))
plot(st_geometry(vic),              add = TRUE, col = "lightgrey")
plot(st_geometry(parks),            add = TRUE, col = "yellow")
plot(st_geometry(park_of_interest), add = TRUE, col = "green")
plot(st_geometry(park_oi_buf),      add = TRUE)
plot(st_geometry(parks_intersect),  add = TRUE, col = "darkgreen")

# count the number
nrow(parks_intersect)

# Make it a dataframe
my_parks_df <- my_site_parks_intersect %>%
  mutate(area = as.vector(st_area(.))) %>%
  filter(area > 300000) %>%
  select(site_name, area) %>%
  st_drop_geometry()


# Summarise, group_by, mutate
parks %>%
  group_by(NAME) %>%
  summarise(n = n()) %>%
  mutate(area = as.vector(st_area(.)))


# plot ------------------------------------------------------------------------------------------------------------------------------------------------------
## New functions:
?geom_sf

?tm_shape
?tm_polygons
?tm_borders
?tm_dots

?leaflet
?addTiles
?addPolygons
?addMarkers

?mapview

## base r
plot(my_parks_full)
plot(st_geometry(my_parks_full))



## ggplot
# good for sf objects

ggplot(parks) +
  geom_sf(fill = "lightgreen", col = "darkgreen")

ggplot(parks) +
  geom_sf(aes(fill = NAME))

ggplot() +
  geom_sf(data = parks, fill = "lightgreen") +
  geom_sf(data = my_parks_full, fill = "blue") +
  geom_sf(data = my_sites_dat, col = "purple")



## tmap
# good for rasters and sf objects
tm_shape(my_parks_full) +
  tm_polygons()

tm_shape(parks)           + tm_polygons() +
  tm_shape(vic)           + tm_borders(col = "black") +   # Vic as an outline
  tm_shape(my_parks_full) + tm_polygons(col = "blue") + # parks that overlap
  tm_shape(my_sites_dat)  + tm_dots(size = 0.1, col = "purple") # sites


## leaflet
# must be in a specific projection - '+proj=longlat +datum=WGS84'

parks %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, color = "darkgreen")

leaflet(st_transform(my_sites_p, '+proj=longlat +datum=WGS84')) %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())



## mapview
# based off leaflet
# appear in the viewer pane, not the plot
mapview(parks)
mapview(st_geometry(parks)) + mapview(my_sites_p, zcol = "site_name")
mapview(st_geometry(parks), col.regions = "lightgreen") + mapview(my_sites_p, col.regions = "red")



# Final - other useful functions ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# st_union
st_union(parks) # returns 1 polygon

st_union(parks) %>% st_area() # the total area of parks

# Validity
?st_is_valid
?st_make_valid

# empty geometry
?st_is_empty






# Plotting for the slides ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## parks locations
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4)

## my sampling sites
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4) %>%
  addMarkers(data = st_transform(my_sites_p, '+proj=longlat +datum=WGS84'))

## park of interest
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4) %>%
  addPolygons(data = st_transform(park_of_interest, '+proj=longlat +datum=WGS84'), color = "pink", weight = 1, fillOpacity = 1) %>%
  addCircleMarkers(data = st_transform(my_sites_p, '+proj=longlat +datum=WGS84'),  stroke = FALSE, fillOpacity = 1, radius = 3, color = "purple")


## buffers
# around the sites
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4) %>%
  addCircleMarkers(data = st_transform(my_sites_p, '+proj=longlat +datum=WGS84'),  stroke = FALSE, fillOpacity = 1, radius = 3, color = "purple") %>%
  addPolygons(data = st_transform(my_sites_buf, '+proj=longlat +datum=WGS84'), fillOpacity = 0, color = "black", opacity = 1, weight = 1)

# around the park of interest
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4) %>%
  addPolygons(data = st_transform(park_of_interest, '+proj=longlat +datum=WGS84'), color = "pink", weight = 1, fillOpacity = 1) %>%
  addCircleMarkers(data = st_transform(my_sites_p, '+proj=longlat +datum=WGS84'),  stroke = FALSE, fillOpacity = 1, radius = 3, color = "purple") %>%
  addPolygons(data = st_transform(park_oi_buf, '+proj=longlat +datum=WGS84'), fillOpacity = 0, color = "black", opacity = 1, weight = 1)


## intersect
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4) %>%
  # addPolygons(data = st_transform(park_of_interest, '+proj=longlat +datum=WGS84'), color = "pink", weight = 1, fillOpacity = 1) %>%
  addCircleMarkers(data = st_transform(my_sites_p, '+proj=longlat +datum=WGS84'),  stroke = FALSE, fillOpacity = 1, radius = 3, color = "purple") %>%
  addPolygons(data = st_transform(park_oi_buf, '+proj=longlat +datum=WGS84'), fillOpacity = 0, color = "black", opacity = 1, weight = 1) %>%
  addPolygons(data = st_transform(parks_intersect, '+proj=longlat +datum=WGS84'), color = "orange", weight = 1, fillOpacity = 1)


## intersection
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = st_transform(parks, '+proj=longlat +datum=WGS84'), color = "darkgreen", weight = 1, fillOpacity = 0.4) %>%
  addCircleMarkers(data = st_transform(my_sites_p, '+proj=longlat +datum=WGS84'),  stroke = FALSE, fillOpacity = 1, radius = 3, color = "purple") %>%
  addPolygons(data = st_transform(my_sites_buf, '+proj=longlat +datum=WGS84'), fillOpacity = 0, color = "black", opacity = 1, weight = 1) %>%
  addPolygons(data = st_transform(my_site_parks_intersect, '+proj=longlat +datum=WGS84'), color = "blue", weight = 1, fillOpacity = 1)

