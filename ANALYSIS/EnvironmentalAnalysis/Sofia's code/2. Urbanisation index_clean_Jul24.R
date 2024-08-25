# In this script I take all site characteristics and I calculate an urbanization index using Principal Component Anlaysis (PCA) 
# Positive value are for high urbanisation while negative values are for low urbanisation

library(FactoMineR)
library(factoextra)
library(corrplot)
library(viridis)
library(ggplot2)

load('~/site_char.R')

#################################################################################
#################################################################################
# To run analysis using an radius from an area use the following code, to use a shp
# move to next section

# Select habitat columns for PCA
habitat_data <- slocsb[, c("L_Grass", "L_Tree", "L_Vege", "L_Urban", "L_Residential", "L_BA", "L_BH")]

habitat_data <- habitat_data
habitat_data[is.na(habitat_data)] <- 0

# Perform PCA
pca_result <- prcomp(habitat_data, scale. = TRUE, center = TRUE)

# Extract scores (coordinates of original data in principal component space)
scores <- pca_result$x

# Create a new column for the first principal component (urbanization index)
slocsb$Urbanization_Index <- scores[, 1]

# Display the first principal component (urbanization index)
print("Urbanization Index (First Principal Component):")
index_df <- data.frame(site_code = slocsb$site, Urbanization_Index = slocsb$Urbanization_Index)
print(index_df)

# Create a bar plot for Urbanization Index
ggplot(slocsb, aes(x = site, y = Urbanization_Index)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Urbanisation Index by Site",
       x = "Site",
       y = "Urbanisation Index") +
  theme_minimal()


#################################################################################
#################################################################################
# for analysis with the home range of multiple roost instead of a radius around the roost, follow the following 

# Select habitat columns for PCA
habitat_data <- slocsb[, c("L_Grass", "L_Tree", "L_Vege", "L_Urban", "L_Residential", "L_BA", "L_BH")]

habitat_data <- habitat_data
habitat_data[is.na(habitat_data)] <- 0

habitat_data_numeric <- habitat_data %>%
  select(-geometry)

habitat_data_numeric <- as.data.frame(habitat_data_numeric)

# Perform PCA
pca_result <- prcomp(habitat_data_numeric, scale. = TRUE, center = TRUE)
scores <- pca_result$x

# Flip the sign of the first principal component
# scores[, 1] <- -scores[, 1] 
# this is only optional if the PCA scores are flipped in posotove and negative 
# value and you want to flip the sign
# The sign of the principal component scores in PCA (Principal Component Analysis) 
# can be positive or negative and is not inherently meaningful by itself. It is 
# determined by the direction of the eigenvectors, which can arbitrarily be flipped. 
# This means that whether lower urbanization scores are negative or positive is not 
# inherently significant and does not affect the relative ordering or the 
# interpretation of the results.

slocsb$Urbanization_Index <- scores[, 1]

# Display the first principal component (urbanization index)
print("Urbanization Index (First Principal Component):")
index_df <- data.frame(site_code = slocsb$site_code, Urbanization_Index = slocsb$Urbanization_Index)
print(index_df)

# Results with home range
# site_code Urbanization_Index
# 1      CO_sb          0.5657270
# 2      WA_sb          0.9223497
# 3      WM_sb          1.2933476
# 4         AN          0.5895107
# 5         LY          1.1091965
# 6         HA         -0.4524586
# 7         OC          0.3717445
# 8       L_th         -6.0528800
# 9      WA_jp          0.7457606
# 10     WM_jp          0.9077020

# Create a bar plot for Urbanization Index
ggplot(slocsb, aes(x = site_code, y = Urbanization_Index)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Urbanization Index by Site",
       x = "Site",
       y = "Urbanization Index") +
  theme_minimal()
