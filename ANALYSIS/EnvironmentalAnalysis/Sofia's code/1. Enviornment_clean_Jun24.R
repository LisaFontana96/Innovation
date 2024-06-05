# In this script I characterize each site based on Environmental characteristics, including land cover and building characheristics
# for this purpose you will need to load three files
# 1. The landcover raster that covers the entire area of interest, here I use a open source dataset from https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v100#bands
# 2. A building shapefile layer, here I use Geoscape buildings with 2 mt resolution, the file includes heights, area and classification for each building footprint
# 3. A csv file with the coordinates of each roost of interest (I made a simple file with 3 columns, Site, Lon, Lat)
# in this case I used an area of a specific radius (2km) from the roost location. The radius can be changed based on need
# and if analysis have to be performed on the Home range rather than ona fixed area from a roost you can replace file 3 with the roost home range 
# and later merge them together in a single .shp/.csv (code provided below)

library(sp)
library(sf)
library(raster)
library(dplyr)
Sys.setenv(TZ='UTC')
Proj <- CRS('+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

# load building datase, you can find the file on shared aplin lab folder>projects>scc>environmental data>geoscape)
Buildings <- st_read('~/March2019_NSW_BUILDING_POLYGON_extra_Extract_SydneyUCL2016_SHAPE/March2019_NSW_BUILDING_POLYGON_extra_Extract_SydneyUCL2016_region.shp')
Buildings <- st_transform(Buildings, Proj)
# load landcover raster 
Hab <- raster('~/Landcover_10m.tif')
# Define the legend
key <- data.frame(
  feature = c(
    'Tree cover', 
    'Shrubland',
    'Grassland',
    'Cropland',
    'Built-up',
    'Bare / sparse vegetation',
    'Snow and ice',
    'Permanent water bodies',
    'Herbaceous wetland', 
    "Mangroves",
    'Moss and lichen'
  ),
  code = c(10,20,30,40,50,60,70,80,90,95,100)
)

key$col <- c(
  rgb(0, 100, 0, max = 255),          # Tree cover
  rgb(100, 100, 100, max = 255),      # Shrubland
  rgb(0, 255, 0, max = 255),          # Grassland
  rgb(0, 255, 0, max = 255),          # Cropland
  rgb(100, 100, 100, max = 255),      # Built-up
  rgb(255, 0, 0, max = 255),          # Bare / sparse vegetation
  rgb(100, 100, 255, max = 255),      # Snow and ice
  rgb(100, 100, 0, max = 255),        # Permanent water bodies
  rgb(255, 255, 255, max = 255),      # Herbaceous wetland
  rgb(100, 100, 100, max = 255),      # Mangroves
  rgb(0, 0, 100, max = 255)           # Moss and lichen
)
key <- unique(key[order(key$code),])
key <- key[order(key$code),]


####
# load csv file with roost locations, if you want to use home ranges instead of a defined 
# area from the roost see below
slocsb <- read.csv('path/to/csv', stringsAsFactors = FALSE)
colnames(slocsb) <- c("site", "x", "y")
sf_bird <- st_as_sf(slocsb, coords = c("y", "x"), crs = 4326)
sf_bird <- st_transform(sf_bird, st_crs(Buildings))

# extract landcover proportion within the area of interest
local <- extract(Hab, sf_bird, buffer = 2000)  # Change buffer to your requirements

slocsb$L_Grass <- unlist(lapply(local, function(x)
  sum((table(x) / length(x) * 100)[c('30')], na.rm = T) ))
slocsb$L_Tree <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('10')], na.rm=T) ))
slocsb$L_Vege <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('20','40','60')], na.rm=T) ))
slocsb$L_Urban <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('50')], na.rm=T) ))

slocsb$Habitat <- c('Grass', 'Trees', 'Veget.','Urban')[
  apply(slocsb[,c('L_Grass','L_Tree', 'L_Vege','L_Urban')], 
        1,which.max)]

# Extract building characteristics within the area of interest
local <- st_buffer(sf_bird, 2000)  # Changed buffer to your requirements
local <- st_overlaps(local, Buildings)

slocsb$L_BA <- unlist(lapply(local, function(x)
  mean(Buildings$AREA[x], na.rm=T) ))
slocsb$L_BH <- unlist(lapply(local, function(x)
  mean(Buildings$HEIGHT_R1[x], na.rm=T) ))
slocsb$L_Residential <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Residential')], na.rm=T) ))
slocsb$L_Commercial <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Commercial/Business','Industrial/Utilities', 'Community Use')], na.rm=T) ))
slocsb$L_Mixed <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Mixed Use')], na.rm=T) ))
slocsb$L_Open <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Recreational/Open Space', 'Conservation/National Park')], na.rm=T) ))
slocsb$L_Urbanf <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Transport/Infrastructure','Water','Special Use')], na.rm=T) ))

# save file
save(slocsb, file = '~/site_char.R')

#####
# for analysis with the home range of multiple roost instead of a location area, follow the following 
# (still to test)
# Function to load and merge shapefiles from a directory
merge_shapefiles <- function(directory) {
  shapefiles <- list.files(directory, pattern = "\\.shp$", full.names = TRUE)
  shapefiles_list <- lapply(shapefiles, st_read)
  merged_shapefile <- do.call(st_union, shapefiles_list)
  return(merged_shapefile)
}

# Load and merge shapefiles
home_range_dir <- '~/path/to/home_ranges_folder'
merged_home_ranges <- merge_shapefiles(home_range_dir)

# extract landcover proportion within the area of interest
local <- extract(Hab, merged_home_ranges)  # Change buffer to your preferred radius

slocsb$L_Grass <- unlist(lapply(local, function(x)
  sum((table(x) / length(x) * 100)[c('30')], na.rm = T) ))
slocsb$L_Tree <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('10')], na.rm=T) ))
slocsb$L_Vege <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('20','40','60')], na.rm=T) ))
slocsb$L_Urban <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('50')], na.rm=T) ))

slocsb$Habitat <- c('Grass', 'Trees', 'Veget.','Urban')[
  apply(slocsb[,c('L_Grass','L_Tree', 'L_Vege','L_Urban')], 
        1,which.max)]

# extract buildings
local <- st_overlaps(merged_home_ranges, Buildings)

slocsb$L_BA <- unlist(lapply(local, function(x)
  mean(Buildings$AREA[x], na.rm=T) ))
slocsb$L_BH <- unlist(lapply(local, function(x)
  mean(Buildings$HEIGHT_R1[x], na.rm=T) ))
slocsb$L_Residential <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Residential')], na.rm=T) ))
slocsb$L_Commercial <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Commercial/Business','Industrial/Utilities', 'Community Use')], na.rm=T) ))
slocsb$L_Mixed <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Mixed Use')], na.rm=T) ))
slocsb$L_Open <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Recreational/Open Space', 'Conservation/National Park')], na.rm=T) ))
slocsb$L_Urbanf <- unlist(lapply(local, function(x)
  sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Transport/Infrastructure','Water','Special Use')], na.rm=T) ))


# save file
save(slocsb, file = '~/site_char.R')