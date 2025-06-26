rm(list = ls())
cat("\014")
graphics.off()
library(ggmap)
library(ggplot2)
library(readr)
library(dplyr)

#Stadia Maps API key 
register_stadiamaps(key = "fac7ca19-76cd-4221-820f-cc72c7ec3562")

# Load data
roosts <- read_csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/DATA/RoostCounts/Roosts.csv')
roost_extra<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/ARTICLE/PICTURES&GRAPHS/Map/Roost_extra.csv')

# Bounding box around the data points
bbox <- make_bbox(lon = roosts$Long, lat = roosts$Lat, f = 0.1)

#Stadia/Former Stamen basemap)
basemap <- get_stadiamap(bbox, zoom = 13, maptype = "stamen_toner_lite")

## Map with roost points ##
roosts$Rounded <- round(roosts$Count, -1)
roost_extra$Type <- "Non-focal roosts"
roosts$Type <- "Focal roosts"

breaks <- quantile(roosts$Rounded, probs = seq(0, 1, 0.25), na.rm = TRUE)
interval_labels <- paste(
  round(head(breaks, -1) / 5) * 5,
  round(breaks[-1] / 5) * 5,
  sep = "–"
)
roosts$SizeCat <- cut(
  roosts$Rounded,
  breaks = breaks,
  include.lowest = TRUE,
  labels = interval_labels
)

roost_combined <- bind_rows(
  mutate(roosts, Shape = "circle"),
  mutate(roost_extra, Shape = "star")
)

ggmap(basemap) +
  geom_point(data = roost_extra, aes(x = Long, y = Lat, shape = "Non-focal roosts"),
             color = "black", size = 3, show.legend = TRUE) +
  
  geom_point(data = roosts, aes(x = Long, y = Lat, fill = SizeCat),
             shape = 21, size = 6, alpha = 0.8, show.legend = TRUE) +
  
  scale_shape_manual(name = "", values = c("Non-focal roosts" = 8)) +
  scale_fill_brewer(palette = "YlOrRd", name = "Roost size") +
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

#With roosts labels
ggmap(basemap) +
  geom_point(data = roost_extra, aes(x = Long, y = Lat, shape = "Non-focal roosts"),
             color = "black", size = 2, show.legend = TRUE) +
  
  geom_point(data = roosts, aes(x = Long, y = Lat, fill = SizeCat),
             shape = 21, size = 6, alpha = 0.8, stroke = 0, show.legend = TRUE) +
  
  geom_text(data = roosts, aes(x = Long, y = Lat, label = CODE),
            nudge_y = 0.001, size = 2, fontface = "bold", show.legend = FALSE) +
  scale_shape_manual(name = "", values = c("Non-focal roosts" = 8)) +
  scale_fill_brewer(palette = "YlOrRd", name = "Roost size") +
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal()


