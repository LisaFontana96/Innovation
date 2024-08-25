# In this script I characterize each site based on Environmental characteristics, including land cover and building characheristics
# for this purpose you will need to load three files
# 1. The landcover raster that covers the entire area of interest, here I use a open source dataset from https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v100#bands
# 2. A building shapefile layer, here I use Geoscape buildings with 2 mt resolution, the file includes heights, area and classification for each building footprint
# 3. A csv file with the coordinates of each roost of interest (I made a simple file with 3 columns, Site, Lon, Lat)
# in this case I used an area of a specific radius (2km) from the roost location. The radius can be changed based on need
# and if analysis have to be performed on the Home range rather than ona fixed area from a roost you can replace file 3 with the roost home range 
# and later merge them together in a single .shp/.csv (code provided below)

rm(list = ls())
cat("\014")
library(sp)
library(sf)
library(raster)
library(dplyr)
Sys.setenv(TZ='UTC')
Proj <- st_crs(4326)

###########################################################################
###########################################################################
# In this first part I adjusted the building layer for ACT and SYD in order to merge them together, no need to do that if you have the 
# 'Buildings.shp". In that case move straight to the next part
# # load building database, you can find the file on shared aplin lab folder>projects>scc>environmental data>geoscape)
# SYD_Buildings <- st_read('/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Envioronmental data/March2019_NSW_BUILDING_POLYGON_extra_Extract_SydneyUCL2016_SHAPE/March2019_NSW_BUILDING_POLYGON_extra_Extract_SydneyUCL2016_region.shp')
# ACT_Buildings <- st_read('/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/buildings/Buildings_JUN24_ACT_GDA94_SHP_322/Buildings/Buildings JUNE 2024/Standard/act_buildings.shp')
# 
# colnames(SYD_Buildings)
# colnames(ACT_Buildings)
# 
# # Rename columns for consistency
# SYD_Buildings <- SYD_Buildings %>%
#   rename(
#     AREA = AREA,
#     HEIGHT_R1 = HEIGHT_R1,
#     ZONE_CD_NM = ZONE_CD_NM
#   )
# 
# ACT_Buildings <- ACT_Buildings %>%
#   rename(
#     AREA = AREA,
#     HEIGHT_R1 = ROOF_HGT,
#     ZONE_CD_NM = PLAN_ZONE
#   )
# 
# # Select relevant columns
# SYD_Buildings <- SYD_Buildings %>%
#   select(AREA, HEIGHT_R1, ZONE_CD_NM, geometry)
# 
# ACT_Buildings <- ACT_Buildings %>%
#   select(AREA, HEIGHT_R1, ZONE_CD_NM, geometry)
# 
# # Check for invalid geometries in SYD_Buildings
# invalid_syd <- st_is_valid(SYD_Buildings, reason = TRUE)
# if (any(invalid_syd != "Valid Geometry")) {
#   cat("Invalid geometries in SYD_Buildings:\n")
#   print(invalid_syd[invalid_syd != "Valid Geometry"])
# }
# 
# # Check for invalid geometries in ACT_Buildings
# invalid_act <- st_is_valid(ACT_Buildings, reason = TRUE)
# if (any(invalid_act != "Valid Geometry")) {
#   cat("Invalid geometries in ACT_Buildings:\n")
#   print(invalid_act[invalid_act != "Valid Geometry"])
# }
# 
# # Fix invalid geometries if any
# SYD_Buildings <- st_make_valid(SYD_Buildings)
# ACT_Buildings <- st_make_valid(ACT_Buildings)
# 
# # Ensure both datasets have the same CRS
# if (st_crs(SYD_Buildings) != st_crs(ACT_Buildings)) {
#   ACT_Buildings <- st_transform(ACT_Buildings, st_crs(SYD_Buildings))
# }
# 
# # Combine the datasets
# Buildings <- bind_rows(SYD_Buildings, ACT_Buildings)
# 
# # Check for missing CRS and set it if needed
# if (is.na(st_crs(Buildings))) {
#   st_crs(Buildings) <- Proj
# }
# 
# # Transform the combined dataset if necessary
# Buildings <- st_transform(Buildings, Proj)
# Save as shp
# output_path <- "/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/buildings/Buildings.shp"
# 
# # Write the Buildings dataset to a shapefile
# st_write(Buildings, output_path)

Buildings<-st_read('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/Layers/Buildings/Buildings.shp')
Buildings<- st_transform(Buildings, st_crs(Proj))

# load landcover raster 
Hab <- raster('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/Layers/Landcover_10m.tif')
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

#################################################################################
#################################################################################
# To run analysis using an radius continue here, 
# to use a shp move to next section
# load csv file with roost locations, if you want to use home ranges instead of a defined 
# area from the roost see below
slocsb <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Roosts2023.csv', stringsAsFactors = FALSE)
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
slocsb$site[slocsb$site == "NA"] <- "NAR"

# save file
save(slocsb, file = '~/site_char.R')


# #################################################################################
# #################################################################################
# # for analysis with the home ranges instead of a radius around the roost, follow the following
# 
# # Function to load and merge shapefiles from a directory
# merge_shapefiles <- function(directory) {
#   shapefiles <- list.files(directory, pattern = "\\.shp$", full.names = TRUE)
#   shapefiles_list <- lapply(shapefiles, st_read)
#   merged_shapefile <- do.call(st_union, shapefiles_list)
#   return(merged_shapefile)
# }
# 
# # Read shapefiles
# # here you need to put the directory to the home range files within the 'Home ranges' inside Sofia and Lisa shared folder
# co <- st_read("/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/buildings/Home ranges/CO_suncalc_c_exc22_union.shp")
# wa <- st_read("/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/buildings/Home ranges/WA_resident_suncalc_c_union.shp")
# wm <- st_read("/Users/u7584446/Library/CloudStorage/OneDrive-AustralianNationalUniversity/Desktop/buildings/Home ranges/WM_suncalc_c_union.shp")
# 
# wa <- st_transform(wa, st_crs(Proj))
# wm <- st_transform(wm, st_crs(Proj))
# co <- st_transform(co, st_crs(Proj))
# 
# co$site_code <- "co"
# wa$site_code <- "wa"
# wm$site_code <- "wm"
# 
# combined_home_ranges <- rbind(co, wa, wm)
# 
# slocsb <- st_transform(combined_home_ranges, st_crs(Proj))
# slocsb <- slocsb %>%
#   select(-FID)
# 
# # extract landcover proportion within the area of interest
# local <- extract(Hab, slocsb)  
# 
# slocsb$L_Grass <- unlist(lapply(local, function(x)
#   sum((table(x) / length(x) * 100)[c('30')], na.rm = T) ))
# slocsb$L_Tree <- unlist(lapply(local, function(x)
#   sum((table(x)/length(x)*100)[c('10')], na.rm=T) ))
# slocsb$L_Vege <- unlist(lapply(local, function(x)
#   sum((table(x)/length(x)*100)[c('20','40','60')], na.rm=T) ))
# slocsb$L_Urban <- unlist(lapply(local, function(x)
#   sum((table(x)/length(x)*100)[c('50')], na.rm=T) ))
# 
# # Remove the geometry column
# slocsb_data <- st_drop_geometry(slocsb)
# 
# # Check column types
# str(slocsb_data)
# 
# # Apply function to determine the habitat type
# slocsb_data$Habitat <- c('Grass', 'Trees', 'Veget.','Urban')[
#   apply(slocsb_data[,c('L_Grass','L_Tree', 'L_Vege','L_Urban')], 
#         1, which.max)]
# 
# # slocsb$Habitat <- c('Grass', 'Trees', 'Veget.','Urban')[
# #   apply(slocsb[,c('L_Grass','L_Tree', 'L_Vege','L_Urban')], 
# #         1,which.max)]
# 
# # extract buildings
# local <- st_overlaps(slocsb, Buildings)
# 
# slocsb$L_BA <- unlist(lapply(local, function(x)
#   mean(Buildings$AREA[x], na.rm=T) ))
# slocsb$L_BH <- unlist(lapply(local, function(x)
#   mean(Buildings$HEIGHT_R1[x], na.rm=T) ))
# slocsb$L_Residential <- unlist(lapply(local, function(x)
#   sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Residential')], na.rm=T) ))
# slocsb$L_Commercial <- unlist(lapply(local, function(x)
#   sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Commercial/Business','Industrial/Utilities', 'Community Use')], na.rm=T) ))
# slocsb$L_Mixed <- unlist(lapply(local, function(x)
#   sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Mixed Use')], na.rm=T) ))
# slocsb$L_Open <- unlist(lapply(local, function(x)
#   sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Recreational/Open Space', 'Conservation/National Park')], na.rm=T) ))
# slocsb$L_Urbanf <- unlist(lapply(local, function(x)
#   sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Transport/Infrastructure','Water','Special Use')], na.rm=T) ))
# 
# 
# # save file
# save(slocsb, file = '~/site_char.R')