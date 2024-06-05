# In this script I take all site characteristics and I calculate an urbanization index using Principal Component Anlaysis (PCA) 
# Positive value are for high urbanisation while negative values are for low urbanisation

library(FactoMineR)
library(factoextra)
library(corrplot)
library(viridis)
library(ggplot2)

load('~/site_char.R')

# Select habitat columns for PCA
habitat_data <- slocsb[, c("L_Grass", "L_Tree", "L_Vege", "L_Urban", "L_Residential", "L_BA", "L_BH")]

# Perform PCA
pca_result <- prcomp(habitat_data, scale. = TRUE, center = TRUE)

# Extract scores (coordinates of original data in principal component space)
scores <- pca_result$x

# Create a new column for the first principal component (urbanization index)
slocsb$Urbanization_Index <- scores[, 1]

# Display the first principal component (urbanization index)
print("Urbanization Index (First Principal Component):")
print(slocsb$Urbanization_Index)

# Create a bar plot for Urbanization Index
ggplot(slocsb, aes(x = site, y = Urbanization_Index)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Urbanization Index by Site",
       x = "Site",
       y = "Urbanization Index") +
  theme_minimal()