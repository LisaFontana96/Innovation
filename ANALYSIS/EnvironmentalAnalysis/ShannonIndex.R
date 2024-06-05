rm(list = ls())
library(dplyr)

####  ANU  ####
#Load data
data_ANU<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/ANU_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_ANU$Area.km2, by = list(Category = data_ANU$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexANU <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexANU)

#### COOK ####
data_CO<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/CO_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_CO$Area.km2, by = list(Category = data_CO$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexCO <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexCO)

#### CORROBOREE PARK ####
data_CP<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Corroboree_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_CP$Area.km2, by = list(Category = data_CP$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexCP <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexCP)

#### GARIS PLACE ####
data_GP<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/GarisPlace_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_GP$Area.km2, by = list(Category = data_GP$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexGP <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexGP)

#### GUNGHALIN ####
data_GU<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Gungahlin_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_GU$Area.km2, by = list(Category = data_GU$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexGU <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexGU)

#### HACKETT ####
data_HA<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/HA_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_HA$Area.km2, by = list(Category = data_HA$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexHA <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexHA)

#### HIGGINS ####
data_HI<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Higgins_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_HI$Area.km2, by = list(Category = data_HI$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexHI <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexHI)

#### LAKE GINNINDERRA ####
data_LG<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/LakeG_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_LG$Area.km2, by = list(Category = data_LG$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexLG <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexLG)

#### LYNEAM ####
data_LY<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Lyneam_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_LY$Area.km2, by = list(Category = data_LY$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexLY <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexLY)

#### NARRABUNDAH ####
data_NAR<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Narrabundah_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_NAR$Area.km2, by = list(Category = data_NAR$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexNAR <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexNAR)

#### O'CONNOR ####
data_OC<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/OConnor_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_OC$Area.km2, by = list(Category = data_OC$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexOC <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexOC)

#### TELOPEA PARK ####
data_TP<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/TelopeaPark_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_TP$Area.km2, by = list(Category = data_TP$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexTP <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexTP)

#### WATSON ####
data_WA<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Watson_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_WA$Area.km2, by = list(Category = data_WA$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexWA <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexWA)

#### WAR MEMORIAL ####
data_WM<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/WM_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_WM$Area.km2, by = list(Category = data_WM$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexWM <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexWM)

#### YARRALULMA ####
data_YR<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/AttributeTables_LandCover/Area_Radius1500m/Yarralulma_Landcover2.csv')
#Summarize the area of each land cover category across all pixels
summary <- aggregate(data_YR$Area.km2, by = list(Category = data_YR$DN), FUN = sum)
#Calculate the proportion of each land cover category relative to the total area
summary$Proportion <- summary$x / sum(summary$x)
#Compute the Shannon diversity index
shannon_indexYR <- -sum(summary$Proportion * log(summary$Proportion))
#Display or save the results
print(shannon_indexYR)

#### DATAFRAME ####
roosts<- c('ANU', 'CO', 'COR', 'GARP', 'GU', 'HA', 'HIG', 'LAKEG', 'LY', 'NAR', 'OC', 'TP', 'WA', 'WM', 'YM')
Shannon_index<- c(shannon_indexANU, shannon_indexCO, shannon_indexCP, shannon_indexGP, shannon_indexGU, shannon_indexHA, shannon_indexHI, shannon_indexLG, shannon_indexLY, shannon_indexNAR, shannon_indexOC, shannon_indexTP, shannon_indexWA, shannon_indexWM, shannon_indexYR)
Shannon_index<- data.frame(Roosts = roosts, Shannon_index = Shannon_index)
write.csv(Shannon_index, file = "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/Environmental_analysis/Shannon_index.csv")
