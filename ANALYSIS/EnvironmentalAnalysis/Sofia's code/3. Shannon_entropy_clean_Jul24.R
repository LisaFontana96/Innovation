library(SpatEntropy)
library(spatstat)
library(raster)
library(rasterVis)
library(sf)
library(sp)
library(terra)
library(dplyr)

Proj <- CRS('+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

# here load the raster data provided by Sofia that merges landcover, building and roads
# to create the raster I used the same landcover layer and Building layer used for the urbanisation index
# the roads layer was downloaded from Microsoft roads open source
# Steps to merge the layers togetehr and create the raster in Qgis:
# Rasterize the building layer: Raster > Conversion> Rasterize.Parameter burnout value:5; Horizontal resolution and vertical resolution 100000
# nodata value -9999
# Rasterize the roads layer: Raster > Conversion> Rasterize.Parameter burnout value:0; Horizontal resolution and vertical resolution 10000
# nodata value -9999
# Rater> miscellaneous> merge, select landcover, roads and building raster and run
# I did it for NSW and ACT separately then merged them together. remember to assign -9999 to nodata

landcover <- raster('/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/buildings/Merged_land_roads_build_Jul24.tif')

# shannon entropy is measure for each site individually so you will need a csv file with Site, Lon and Lat for each roost (want to impove this)

# here is the code if you want to use an area of a specified radius from the roost
# if instead you want to use the Home range see below
# local_vector <- as.vector(values(local))
S1 <- read.csv('/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/CA.csv') #add directory for the csv file with your roost location 
S1_lon <- S1$Lon
S1_lat <- S1$Lat

# define the cropping extent
buffer_radius <- 2000  # change buffer (in mt) on your preference
S1_extent <- extent(S1_lon - buffer_radius / 111000, 
                    S1_lon + buffer_radius / 111000,
                    S1_lat - buffer_radius / 111000,
                    S1_lat + buffer_radius / 111000)

S1_raster <- raster(S1_extent)
S1_landcover <- crop(landcover, S1_extent)
# writeRaster(S1_landcover, "S1_landcover.tif", format = "GTiff")

# Extract values and convert to a vector
S1_vector <- as.vector(values(S1_landcover))

shannon_index <- shannon(S1_vector)
print(shannon_index)

###############################################################################
###############################################################################
# shannon entropy for home range rather than specific area from roost (still need to test this)
# load shp with home range 
S1 <- st_read('/path/to/your/shapefile.shp')
S1_extent <- extent(S1)
S1_raster <- raster(S1_extent)
S1_landcover <- crop(landcover, S1_extent)
# writeRaster(S1_landcover, "S1_landcover.tif", format = "GTiff")

# Extract values and convert to a vector
S1_vector <- as.vector(values(S1_landcover))

shannon_index <- shannon(S1_vector)
print(shannon_index)
print(shannon_index)