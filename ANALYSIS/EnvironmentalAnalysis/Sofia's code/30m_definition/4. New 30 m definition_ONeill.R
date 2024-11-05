rm(list = ls())
cat("\014")
library(SpatEntropy)
library(spatstat)
library(raster)
library(rasterVis)
library(sf)
library(sp)
library(terra)
library(dplyr)
library(ggplot2)

Proj <- st_crs(3577)

# Load data
landcover<- raster('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/QGIS/ga_ls_landcover_class_cyear_2_1-0-0_au_x15y-40_2020-01-01_level3_rgb.tif')

###############################################################################
###############################################################################
# Oneill entropy with area for each roost
ANU <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/ANU_2.tif')
# Convert raster to matrix
ANU_landcover_matrix <- as.matrix(ANU)
#ONeill index
oneill (ANU_landcover_matrix)
ON_ANU<- 1.74215

CK <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/CK_2.tif')
CK_landcover_matrix <- as.matrix(CK)
print(oneill (CK_landcover_matrix))
ON_CK<- 1.405827

CP <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/CP_2.tif')
CP_landcover_matrix <- as.matrix(CP)
print(oneill (CP_landcover_matrix))
ON_CP<- 1.386514
  
GPM <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/GPM_2.tif')
GPM_landcover_matrix <- as.matrix(GPM)
print(oneill (GPM_landcover_matrix))
ON_GPM<- 1.982424

GU <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/GU_2.tif')
GU_landcover_matrix <- as.matrix(GU)
print(oneill (GU_landcover_matrix))
ON_GU<- 2.119615

HA <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/HA_2.tif')
HA_landcover_matrix <- as.matrix(HA)
print(oneill (HA_landcover_matrix))
ON_HA<- 0.9264335

HIG <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/HIG_2.tif')
HIG_landcover_matrix <- as.matrix(HIG)
print(oneill (HIG_landcover_matrix))
ON_HIG<- 1.781589

LG <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/LG_2.tif')
LG_landcover_matrix <- as.matrix(LG)
print(oneill (LG_landcover_matrix))
ON_LG<- 2.123897

LY <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/LY_2.tif')
LY_landcover_matrix <- as.matrix(LY)
print(oneill (LY_landcover_matrix))
ON_LY<- 1.519479

NAR <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/NAR_2.tif')
NAR_landcover_matrix <- as.matrix(NAR)
print(oneill (NAR_landcover_matrix))
ON_NAR<- 1.873134

OC <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/OC_2.tif')
OC_landcover_matrix <- as.matrix(OC)
print(oneill (OC_landcover_matrix))
ON_OC<- 1.250827

TP <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/TP_2.tif')
TP_landcover_matrix <- as.matrix(TP)
print(oneill (TP_landcover_matrix))
ON_TP<- 2.01484

WA <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/WA_2.tif')
WA_landcover_matrix <- as.matrix(WA)
print(oneill (WA_landcover_matrix))
ON_WA<- 1.416878

WM <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/WM_2.tif')
WM_landcover_matrix <- as.matrix(WM)
print(oneill (WM_landcover_matrix))
ON_WM<- 1.578677

YA <- raster('/Users/u7585399/Desktop/LISA/CCE Lab/InnovationTask/QGIS/YA_2.tif')
YA_landcover_matrix <- as.matrix(YA)
print(oneill (YA_landcover_matrix))
ON_YA<- 1.814321

## Create csv ###
Sites<- c('ANU','CK', 'CP', 'GPM', 'GU', 'HA', 'HIG', 'LG', 'LY', 'NAR', 'OC', 'TP', 'WA', 'WM', 'YA')
ONeill<- c(ON_ANU,ON_CK,ON_CP,ON_GPM,ON_GU,ON_HA,ON_HIG,ON_LG,ON_LY,ON_NAR,ON_OC,ON_TP,ON_WA,ON_WM,ON_YA)
Oneill_index<- data.frame(Site = Sites, ONeill_Index = ONeill)

file_path <- "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/ONeill_index.csv"
write.csv(Oneill_index, file = file_path, row.names = FALSE)

ggplot(Oneill_index, aes(x = Site, y = ONeill)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "O'Neill index by Site",
       x = "Site",
       y = "O'Neill Index") +
  theme_minimal()
