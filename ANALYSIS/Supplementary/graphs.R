rm(list = ls())
cat("\014")
graphics.off()
options(scipen = 999)
library(dplyr)
library(tidyverse)
library(ggplot2)

# dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
# dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 
environmental_indexes <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Environmental_analysis/EnvIndexes.csv')

## UI ##

ggplot(environmental_indexes, aes(x = reorder(Site, UI), y = UI)) +
  geom_col(fill = "#4D4D4D") +
  theme_minimal() +
  labs(
    title = "Urbanisation Index (UI) per Roost",
    x = "Roost",
    y = "Urbanisation Index"
  ) +
  scale_y_continuous(breaks = seq(-7, 5, by = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Entropy ##

# Absolute
ggplot(environmental_indexes %>% filter(Site != "Tidbinbilla"), aes(x = reorder(Site, -O.Neill), y = O.Neill)) +
  geom_col(fill = "#228B22") +
  labs(
    title = "Absolute Entropy (O'Neill) by Roost",
    x = "Roost",
    y = "Absolute Entropy"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relative
ggplot(environmental_indexes %>% filter(Site != "Tidbinbilla"), aes(x = reorder(Site, -ONeill_relative), y = ONeill_relative)) +
  geom_col(fill = "#6B8E23") +  
  labs(
    title = "Relative Entropy (O'Neill) by Roost",
    x = "Roost",
    y = "Relative Entropy"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



