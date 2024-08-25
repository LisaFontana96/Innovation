rm(list = ls())
cat("\014")
library(sp)
library(sf)
library(raster)
library(dplyr)
Sys.setenv(TZ='UTC')
Proj <- st_crs(3577)

Buildings<-st_read('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/Layers/Buildings/Buildings.shp')
Buildings<- st_transform(Buildings, st_crs(Proj))
Hab <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/ga_ls_landcover_class_cyear_2_1-0-0_au_x15y-40_2020-01-01_level3_rgb.tif')

# Define the legend
key <- data.frame(
  feature = c(
    'Natural terrestrial vegetation', 
    'Water',
    'Cultivated terrestrial vegetation',
    'Artificial surface',
    'Natural bare vegetation'
  ),
  code = c(14, 77, 172, 218, 243)
)

key$col <- c(
  rgb(0, 100, 0, max = 255),          # Natural terrestrial vegetation 
  rgb(100, 100, 100, max = 255),      # Water
  rgb(0, 255, 0, max = 255),          # Cultivated
  rgb(0, 255, 0, max = 255),          # Artificial surface
  rgb(100, 100, 100, max = 255)       # Bare surface
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
sf_bird <- st_as_sf(slocsb, coords = c("y", "x"), crs = 4283)
sf_bird <- st_transform(sf_bird, st_crs(Buildings))

# extract landcover proportion within the area of interest
local <- extract(Hab, sf_bird, buffer = 2000)  # Change buffer to your requirements

slocsb$Natural_veg <- unlist(lapply(local, function(x)
  sum((table(x) / length(x) * 100)[c('14')], na.rm = T) ))
slocsb$Water <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('77')], na.rm=T) ))
slocsb$Cultivated <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('172')], na.rm=T) ))
slocsb$Urban <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('218')], na.rm=T) ))
slocsb$Bare <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('243')], na.rm=T) ))

slocsb$Habitat <- c('Natural terrestrial vegetation', 'Water', 'Cultivated terrestrial vegetation','Artificial surface', 'Natural bare surface')[
  apply(slocsb[,c('Natural_veg','Water', 'Cultivated','Urban', 'Bare')], 
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
save(slocsb, file = '~/site_char.R')