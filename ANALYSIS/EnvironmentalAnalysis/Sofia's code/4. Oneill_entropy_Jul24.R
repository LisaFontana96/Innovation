# O'Neill's Entropy and Contagion Indices:
# These measures are specifically mentioned in landscape ecology contexts. 
# They can be very useful for quantifying landscape heterogeneity by measuring the 
# diversity of land cover types and their spatial arrangement. The contagion index, 
# in particular, can provide insights into the degree of fragmentation or 
# aggregation in the urban landscape.
# 
rm(list = ls())
cat("\014")
# Load required libraries
library(SpatEntropy)
library(spatstat)
library(raster)
library(rasterVis)
library(sf)
library(sp)
library(terra)
library(dplyr)
library(ggplot2)

Proj <- st_crs(4326)

# Load data
landcover <- raster('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/Layers/Merged_land_roads_build_Jul24.tif')
# landcover<- raster('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/QGIS/ga_ls_landcover_class_cyear_2_1-0-0_au_x15y-40_2020-01-01_level3_rgb.tif')

###############################################################################
###############################################################################
# Oneill entropy with area for each roost

ANU <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/ANU.csv')
CK <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Cook.csv')
CP <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Corroboree.csv')
GPM <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/GarisPlace.csv')
GU <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Gungahlin.csv')
HA <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Hackett.csv')
HIG <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Higgins.csv')
LG <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/LakeGinninderra.csv')
LY <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Lyneam.csv')
OC <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/OConnor.csv')
TP <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Telopea.csv')
WA <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Watson.csv')
WM <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/WarMemorial.csv')
YA <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Yarralulma.csv')
NAR <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/Narrabundah.csv')

### ANU ###
# Check if S2 is empty
if (nrow(ANU) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  ANU_lon <- ANU$Lon
  ANU_lat <- ANU$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(ANU_lon)
  center_lat <- mean(ANU_lat)
  
  # Define cropping extent based on circular buffer
  ANU_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  ANU_landcover <- crop(landcover, ANU_extent)
  
  # Mask the cropped raster with the circular mask
  ANU_landcover <- mask(ANU_landcover, circle)

  plot(ANU_landcover)
}


# Convert raster to matrix
ANU_landcover_matrix <- as.matrix(ANU_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (ANU_landcover_matrix)
#Oneill ANU 2.38556, relative 0.573606

### Cook ###
# Check if S2 is empty
if (nrow(CK) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  CK_lon <- CK$Lon
  CK_lat <- CK$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(CK_lon)
  center_lat <- mean(CK_lat)
  
  # Define cropping extent based on circular buffer
  CK_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  CK_landcover <- crop(landcover, CK_extent)
  
  # Mask the cropped raster with the circular mask
  CK_landcover <- mask(CK_landcover, circle)
  
  plot(CK_landcover)
}


# Convert raster to matrix
CK_landcover_matrix <- as.matrix(CK_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (CK_landcover_matrix)
#Oneill Cook 1.872539, relative 0.4811473

### Corroboree ###
# Check if S2 is empty
if (nrow(CP) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  CP_lon <- CP$Lon
  CP_lat <- CP$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(CP_lon)
  center_lat <- mean(CP_lat)
  
  # Define cropping extent based on circular buffer
  CP_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  CP_landcover <- crop(landcover, CP_extent)
  
  # Mask the cropped raster with the circular mask
  CP_landcover <- mask(CP_landcover, circle)
  
  plot(CP_landcover)
}


# Convert raster to matrix
CP_landcover_matrix <- as.matrix(CP_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (CP_landcover_matrix)
#Oneill CP 2.612594, relative 0.6713038

### Garis Place McGregor ###
# Check if S2 is empty
if (nrow(GPM) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  GPM_lon <- GPM$Lon
  GPM_lat <- GPM$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(GPM_lon)
  center_lat <- mean(GPM_lat)
  
  # Define cropping extent based on circular buffer
  GPM_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  GPM_landcover <- crop(landcover, GPM_extent)
  
  # Mask the cropped raster with the circular mask
  GPM_landcover <- mask(GPM_landcover, circle)
  
  plot(GPM_landcover)
}


# Convert raster to matrix
GPM_landcover_matrix <- as.matrix(GPM_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (GPM_landcover_matrix)
#Oneill GPM 2.296369, relative 0.5521599

### Gungahlin ###
# Check if S2 is empty
if (nrow(GU) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  GU_lon <- GU$Lon
  GU_lat <- GU$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(GU_lon)
  center_lat <- mean(GU_lat)
  
  # Define cropping extent based on circular buffer
  GU_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  GU_landcover <- crop(landcover, GU_extent)
  
  # Mask the cropped raster with the circular mask
  GU_landcover <- mask(GU_landcover, circle)
  
  plot(GU_landcover)
}


# Convert raster to matrix
GU_landcover_matrix <- as.matrix(GU_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (GU_landcover_matrix)
#Oneill GU 2.431183, relative 0.6246905

### Hackett ###
# Check if S2 is empty
if (nrow(HA) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  HA_lon <- HA$Lon
  HA_lat <- HA$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(HA_lon)
  center_lat <- mean(HA_lat)
  
  # Define cropping extent based on circular buffer
  HA_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  HA_landcover <- crop(landcover, HA_extent)
  
  # Mask the cropped raster with the circular mask
  HA_landcover <- mask(HA_landcover, circle)
  
  plot(HA_landcover)
}


# Convert raster to matrix
HA_landcover_matrix <- as.matrix(HA_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (HA_landcover_matrix)
#Oneill HA 2.022477, relative 0.5196738

### Higgins ### 
# Check if S2 is empty
if (nrow(HIG) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  HIG_lon <- HIG$Lon
  HIG_lat <- HIG$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(HIG_lon)
  center_lat <- mean(HIG_lat)
  
  # Define cropping extent based on circular buffer
  HIG_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  HIG_landcover <- crop(landcover, HIG_extent)
  
  # Mask the cropped raster with the circular mask
  HIG_landcover <- mask(HIG_landcover, circle)
  
  plot(HIG_landcover)
}


# Convert raster to matrix
HIG_landcover_matrix <- as.matrix(HIG_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (HIG_landcover_matrix)
#Oneill HIG 2.56934, relative 0.6177957

### Lake Ginninderra ### 
# Check if S2 is empty
if (nrow(LG) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  LG_lon <- LG$Lon
  LG_lat <- LG$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(LG_lon)
  center_lat <- mean(LG_lat)
  
  # Define cropping extent based on circular buffer
  LG_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  LG_landcover <- crop(landcover, LG_extent)
  
  # Mask the cropped raster with the circular mask
  LG_landcover <- mask(LG_landcover, circle)
  
  plot(LG_landcover)
}


# Convert raster to matrix
LG_landcover_matrix <- as.matrix(LG_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (LG_landcover_matrix)
#Oneill LG 2.711737, relative 0.6520349

### Lyneam ### 
# Check if S2 is empty
if (nrow(LY) == 0) {
  warning("S2.csv is empty or not read correctly")
} else {
  # Extract coordinates from CSV
  LY_lon <- LY$Lon
  LY_lat <- LY$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(LY_lon)
  center_lat <- mean(LY_lat)
  
  # Define cropping extent based on circular buffer
  LY_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  LY_landcover <- crop(landcover, LY_extent)
  
  # Mask the cropped raster with the circular mask
  LY_landcover <- mask(LY_landcover, circle)
  
  plot(LY_landcover)
}


# Convert raster to matrix
LY_landcover_matrix <- as.matrix(LY_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (LY_landcover_matrix)
#Oneill LY 2.804285, relative 0.674288

### O'Connor ### 
# Check if S2 is empty
if (nrow(OC) == 0) {
  warning("S2.csv is empty or not read correctOC")
} else {
  # Extract coordinates from CSV
  OC_lon <- OC$Lon
  OC_lat <- OC$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(OC_lon)
  center_lat <- mean(OC_lat)
  
  # Define cropping extent based on circular buffer
  OC_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  OC_landcover <- crop(landcover, OC_extent)
  
  # Mask the cropped raster with the circular mask
  OC_landcover <- mask(OC_landcover, circle)
  
  plot(OC_landcover)
}


# Convert raster to matrix
OC_landcover_matrix <- as.matrix(OC_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (OC_landcover_matrix)
#Oneill OC 2.310014, relative 0.5554409

### Telopea park ### 
# Check if S2 is empty
if (nrow(TP) == 0) {
  warning("S2.csv is empty or not read correctOC")
} else {
  # Extract coordinates from CSV
  TP_lon <- TP$Lon
  TP_lat <- TP$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(TP_lon)
  center_lat <- mean(TP_lat)
  
  # Define cropping extent based on circular buffer
  TP_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  TP_landcover <- crop(landcover, TP_extent)
  
  # Mask the cropped raster with the circular mask
  TP_landcover <- mask(TP_landcover, circle)
  
  plot(TP_landcover)
}


# Convert raster to matrix
TP_landcover_matrix <- as.matrix(TP_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (TP_landcover_matrix)
#Oneill TP 2.84118, relative 0.6831593

### Watson ### 
# Check if S2 is empty
if (nrow(WA) == 0) {
  warning("S2.csv is empty or not read correctOC")
} else {
  # Extract coordinates from CSV
  WA_lon <- WA$Lon
  WA_lat <- WA$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(WA_lon)
  center_lat <- mean(WA_lat)
  
  # Define cropping extent based on circular buffer
  WA_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  WA_landcover <- crop(landcover, WA_extent)
  
  # Mask the cropped raster with the circular mask
  WA_landcover <- mask(WA_landcover, circle)
  
  plot(WA_landcover)
}


# Convert raster to matrix
WA_landcover_matrix <- as.matrix(WA_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (WA_landcover_matrix)
#Oneill WA 2.364287, relative 0.5684909

### War memorial ### 
# Check if S2 is empty
if (nrow(WM) == 0) {
  warning("S2.csv is empty or not read correctOC")
} else {
  # Extract coordinates from CSV
  WM_lon <- WM$Lon
  WM_lat <- WM$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(WM_lon)
  center_lat <- mean(WM_lat)
  
  # Define cropping extent based on circular buffer
  WM_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  WM_landcover <- crop(landcover, WM_extent)
  
  # Mask the cropped raster with the circular mask
  WM_landcover <- mask(WM_landcover, circle)
  
  plot(WM_landcover)
}


# Convert raster to matrix
WM_landcover_matrix <- as.matrix(WM_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (WM_landcover_matrix)
#Oneill WM 2.569044, relative 0.6177246

### Yarralulma ### 
# Check if S2 is empty
if (nrow(YA) == 0) {
  warning("S2.csv is empty or not read correctOC")
} else {
  # Extract coordinates from CSV
  YA_lon <- YA$Lon
  YA_lat <- YA$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(YA_lon)
  center_lat <- mean(YA_lat)
  
  # Define cropping extent based on circular buffer
  YA_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  YA_landcover <- crop(landcover, YA_extent)
  
  # Mask the cropped raster with the circular mask
  YA_landcover <- mask(YA_landcover, circle)
  
  plot(YA_landcover)
}


# Convert raster to matrix
YA_landcover_matrix <- as.matrix(YA_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (YA_landcover_matrix)
#Oneill YA 2.501617, relative 0.6427883

### Narrabundah ### 
# Check if S2 is empty
if (nrow(NAR) == 0) {
  warning("S2.csv is empty or not read correctOC")
} else {
  # Extract coordinates from CSV
  NAR_lon <- NAR$Lon
  NAR_lat <- NAR$Lat
  
  # Define cropping extent
  buffer_radius <- 2000  # 2km in meters
  buffer_degrees <- buffer_radius / 111000
  
  # Define the center of the circular mask
  center_lon <- mean(NAR_lon)
  center_lat <- mean(NAR_lat)
  
  # Define cropping extent based on circular buffer
  NAR_extent <- extent(
    center_lon - buffer_degrees,
    center_lon + buffer_degrees,
    center_lat - buffer_degrees,
    center_lat + buffer_degrees
  )
  
  # Create a circular mask
  circle <- SpatialPolygons(list(
    Polygons(list(Polygon(
      cbind(
        center_lon + buffer_degrees * cos(seq(0, 2 * pi, length.out = 100)),
        center_lat + buffer_degrees * sin(seq(0, 2 * pi, length.out = 100))
      )
    )), "circle")
  ))
  
  NAR_landcover <- crop(landcover, NAR_extent)
  
  # Mask the cropped raster with the circular mask
  NAR_landcover <- mask(NAR_landcover, circle)
  
  plot(NAR_landcover)
}


# Convert raster to matrix
NAR_landcover_matrix <- as.matrix(NAR_landcover)

# O'Neill's Entropy and Contagion Indices to get a more 
# comprehensive view of landscape heterogeneity, including both the diversity of 
# land cover types and their spatial arrangement.
oneill (NAR_landcover_matrix)
#Oneill NAR 2.439549, relative 0.5551433

#Create and save single .csv for ONeill
Site<- c('ANU', 'CK', 'CP', 'GPM', 'GU', 'HA', 'HIG', 'LG', 'LY', 'NAR', 'OC', 'TP', 'WA', 'WM', 'YA')
ONeill<- c(2.38556, 1.872539, 2.612594, 2.296363, 2.431183, 2.022477, 2.56934, 2.711737, 2.804285, 2.439549, 2.310014, 2.84118, 2.364287, 2.569044, 2.501617)
ONeill<- data.frame(Site, ONeill)

# Create a bar plot for Urbanization Index
ggplot(ONeill, aes(x = Site, y = ONeill)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "O'Neill index by Site",
       x = "Site",
       y = "O'Neill Index") +
  theme_minimal()

###############################################################################
###############################################################################
# Oneill entropy for home range rather than specific area from roost 
# Load shapefile with home range
# S1 <- st_read('/Users/u7584446/Downloads/Home ranges/WA_resident_sunS2lc_c_union.shp')
# 
# # Transform the shapefile to match the landcover CRS
# S1 <- st_transform(S1, st_crs(landcover))
# 
# # Get the extent of the shapefile
# S1_extent <- st_bbox(S1) # Use st_bbox for getting bounding box in sf
# 
# # Crop the landcover raster to the extent of the shapefile
# S1_landcover <- crop(landcover, extent(S1_extent))
# 
# # Mask the cropped raster with the shapefile
# S1_landcover <- mask(S1_landcover, as(S1, 'Spatial')) # Convert sf object to Spatial for raster masking
# 
# # Plot the masked landcover raster
# plot(S1_landcover)
# 
# # Convert raster to matrix
# S1_landcover_matrix <- as.matrix(S1_landcover)
# 
# # O'Neill's Entropy and Contagion Indices to get a more 
# # comprehensive view of landscape heterogeneity, including both the diversity of 
# # land cover types and their spatial arrangement.
# oneill (S1_landcover_matrix)
# 
# 
# ###############################################################################
# #### Plot the Entropy results with the range 
# # Create a dataframe
# data <- data.frame(
#   Site = c("AN", "HA", "LY", "OC", "WA_JP", "WM_JP", "L_TH", "WM_sb", "WA_sb", "CO_sb"),
#   ONeill = c(2.553374, 2.330625, 2.494801, 2.271436, 2.172176, 2.466872, 0.02424638, 2.771941, 2.575191, 2.431056),
#   MinRange = c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000),
#   MaxRange = c(4.158883, 4.158883, 4.394449, 4.158883, 4.394449, 4.394449, 1.386294, 4.158883, 4.158883, 4.158883)
# )
# 
# # Plot the data
# ggplot(data, aes(x = Site)) +
#   geom_point(aes(y = ONeill), color = "blue", size = 3) +
#   geom_errorbar(aes(ymin = MinRange, ymax = MaxRange), width = 0.2, color = "red") +
#   labs(title = "O'Neill Values and Ranges for Each Site",
#        x = "Site",
#        y = "O'Neill Value / Range") +
#   theme_minimal()



