###############################################################################
############     Annotation of ARS with environment data     ##################
###############################################################################

## We characterize ARS and sites according to habitat features. We sample habitats 
## at 2 scales: local 30m radius and within 500m radius (home range =1km2)
## In the end I am only using the Local scale for analysis

#### Loading packages and data ####

load('./data/maxFPT_acc_site_45.R')
load('./data/visitedsites_45')
library(sp)
library(sf)
library(raster)
Sys.setenv(TZ='UTC')
Proj <- CRS('+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

Buildings <- st_read('./dataraw/Sydney/Building')
Buildings <- st_transform(Buildings, Proj)

Hab <- raster('./dataraw/Sydney/SURFACECOVER_2M_SYDNEY.tif')
key <- data.frame(feature = c('Bare earth', 
                            'Road',
                            'Grass',
                            'Trees',
                            'Vegetation',
                            'Built-up areas',
                            'Water',
                            'Buildings',
                            'Cloud', 
                            "Shadow",
                            'Swimming Pool'),
                  code = 2:12)
key <- unique(key[order(key$code),1:2]) ; 
key$col <- c(rgb(215,194,158, max = 255),
             rgb(156,156,156, max = 255),
             rgb(152,230,0, max = 255), 
             rgb(38,115,0, max = 255), 
             rgb(114,137,68, max = 255), 
             rgb(255,190,190, max = 255), 
             rgb(158,170,215, max = 255), 
             rgb(137,90,68, max = 255), 
             rgb(225,225,225, max = 255), 
             rgb(78,78,78, max = 255), 
             rgb(0,77,168, max = 255))
key <- key[order(key$code),]

#### Quick exploration ####
# FPT according to habitats
# sf_bird <- st_as_sf(slocs, coords = c("x", "y"), crs = Proj)
# slocs$Habitat <- extract(Hab, sf_bird)
# boxplot(slocs$fpt~slocs$Habitat)
# t <- table(slocs[which(slocs$stay==1),'Habitat'])/length(which(slocs$stay==1))*100
# barplot(t[c('3','4','5','6','7','9')])
# sum(t[c('7','9')])
# sum(t[c('4')])
# 
# Timing of visits in the different habitats
# library(lubridate)
# slocs$dates <- ymd_hms(slocs$date) 
# slocs$datestz <- with_tz(slocs$dates, tzone = 'Australia/Sydney')
# slocs$hour <- hour(round_date(slocs$datestz, 'hour'))
# ggplot(slocs[which(slocs$stay==1 & slocs$Habitat %in% c('3','4','5','6','7','9')),], aes(x=hour, colour=as.factor(Habitat)) ) + 
#   geom_density()

#### Characterize sites #### 
## revisits -----
temp <- slocsb[-which(is.na(slocsb$site) | slocsb$site=='NOTASSIGNED'),]
temp$site <- paste0(temp$site,'_', temp$Individual)

site <- data.frame(SiteID = levels(as.factor(temp$site)),
                   Revisits = as.numeric(table(temp$site)))

## habitats ----
sitehab <- data.frame()
for(x in 1:length(polybird)){
  birdsite <-  polybird[[x]][-grep('NOTASSIGNED', polybird[[x]]$id),]
  birdsite$id <- paste0(birdsite$id, '_', unique(slocsb$Individual)[x])
  
  local <- extract(Hab, birdsite)
  shab <- data.frame(SiteID = birdsite$id,
                     Lat = st_coordinates(st_centroid(birdsite))[,1],
                     Long = st_coordinates(st_centroid(birdsite))[,2],
                     L_Grass = unlist(lapply(local, function(x) sum((table(x)/length(x)*100)[c('4')], na.rm=T) )),
                     L_Tree = unlist(lapply(local, function(x) sum((table(x)/length(x)*100)[c('5')], na.rm=T) )),
                     L_Vege = unlist(lapply(local, function(x) sum((table(x)/length(x)*100)[c('6')], na.rm=T) )),
                     L_Urban = unlist(lapply(local, function(x) sum((table(x)/length(x)*100)[c('3','7')], na.rm=T) )),
                     L_Building = unlist(lapply(local, function(x)sum((table(x)/length(x)*100)[c('9')], na.rm=T) )))
  
  local <- st_overlaps(birdsite, Buildings)
  shab$L_BA <- unlist(lapply(local, function(x)
    mean(Buildings$AREA[x], na.rm=T) ))
  shab$L_BH <- unlist(lapply(local, function(x)
    mean(Buildings$HEIGHT_R1[x], na.rm=T) ))
  shab$L_Residential <- unlist(lapply(local, function(x)
    sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Residential')], na.rm=T) ))
  shab$L_Commercial <- unlist(lapply(local, function(x)
    sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Commercial/Business','Industrial/Utilities', 'Community Use')], na.rm=T) ))
  shab$L_Mixed <- unlist(lapply(local, function(x)
    sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Mixed Use')], na.rm=T) ))
  shab$L_Open <- unlist(lapply(local, function(x)
    sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Recreational/Open Space', 'Conservation/National Park')], na.rm=T) ))
  shab$L_Urbanf <- unlist(lapply(local, function(x)
    sum((table(Buildings$ZONE_CD_NM[x])/length(x)*100)[c('Transport/Infrastructure','Water','Special Use')], na.rm=T) ))
  
  sitehab <- rbind(sitehab,shab)}

site <- merge(site, sitehab, by = 'SiteID')
site$Individual <- unlist(lapply(strsplit(as.character(site$SiteID),'_'), function(x) x[2]))
site$Habitat <- c('Grass', 'Trees', 'Veget.','Urban', 'Build.')[
  apply(site[,c('L_Grass','L_Tree', 'L_Vege','L_Urban', 'L_Building')], 
        1,which.max)]

# The rocks, a neighborhood with hotels-cafes-restaurants-market of Sydney has not been classified 
# in the data set. Two frequently visited sites of Heineken are here. We annotate it manually
site$L_Mixed[which(site$Habitat=='Build.' & 
                site$L_Residential== 0 & 
                site$L_Commercial == 0 & 
                site$L_Mixed == 0 &
                site$L_Open == 0 &
                site$L_Urbanf == 0)] <- 100

# We create a gradient between 0 and 1 to specify if the buildings are rather 
# residential (0) or commercial (1)
site$Build.U <- apply(site[c('L_Residential','L_Mixed','L_Commercial')],1,
      function(x) weighted.mean(c(0,0.5,1),x,na.rm=T))


## Roost (This is checked in an other script using Net Square displacement) ----
site$Roost <- 'NO'
site <- split(site, paste0(site$Individual,'_', site$Year))
Roosts <- unlist(lapply(site, function(x) x$SiteID[which.max(x$Revisits)]))
Roosts <- as.character(Roosts)
Roosts[9] <- "AI_Limbo_Y2"  #Checked
site <- do.call(rbind,site)
site$Roost[which(site$SiteID %in% Roosts)] <- 'YES'
slocsb$Roost <- 'NO'
slocsb$Roost[which(paste0(slocsb$site,'_',slocsb$Individual) %in% Roosts)] <- 'YES'

## Fidelity through years ----
site$fidelity <- 0
slocsb$year <- 'Y1'
slocsb$year[which(slocsb$date > as.POSIXct('2017-08-01'))] <- 'Y2'
temp <- table(paste0(slocsb$site,'_',slocsb$Individual), slocsb$year)
temp <- dimnames(temp)[[1]][which(temp[,2] !=0 & temp[,1] != 0)]
site$fidelity[which(site$SiteID %in% temp)] <- 1
save(site,file='./data/sitecharac_45' )

#### Characterization of each ARS ####
## Extract habitat proportion within a buffer of 30m or 500m ##
sf_bird <- st_as_sf(slocsb, coords = c("x", "y"), crs = Proj)

local <- extract(Hab, sf_bird, buffer = 30)

slocsb$L_Grass <- unlist(lapply(local, function(x)
  sum((table(x) / length(x) * 100)[c('4')], na.rm = T) ))
slocsb$L_Tree <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('5')], na.rm=T) ))
slocsb$L_Vege <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('6')], na.rm=T) ))
slocsb$L_Urban <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('3','7')], na.rm=T) ))
slocsb$L_Building <- unlist(lapply(local, function(x)
  sum((table(x)/length(x)*100)[c('9')], na.rm=T) ))

slocsb$Habitat <- c('Grass', 'Trees', 'Veget.','Urban', 'Build.')[
  apply(slocsb[,c('L_Grass','L_Tree', 'L_Vege','L_Urban', 'L_Building')], 
        1,which.max)]

#Building characteristics
local <- st_buffer(sf_bird,30)
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

save(slocsb, file = './data/maxFPT_acc_site_45.R')
####
