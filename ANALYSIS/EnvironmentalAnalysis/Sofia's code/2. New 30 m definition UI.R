library(FactoMineR)
library(factoextra)
library(corrplot)
library(viridis)
library(ggplot2)

load('~/site_char.R')
# Select habitat columns for PCA
habitat_data <- slocsb[, c("Natural_veg", "Water", "Cultivated", "Urban", "Bare","L_Residential", "L_BA", "L_BH")]

habitat_data <- habitat_data
habitat_data[is.na(habitat_data)] <- 0

# Perform PCA
pca_result <- prcomp(habitat_data, scale. = TRUE, center = TRUE)

# Extract scores (coordinates of original data in principal component space)
scores <- pca_result$x

# Create a new column for the first principal component (urbanization index)
slocsb$Urbanization_Index <- -scores[, 1]

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

#Save csv
file_path <- "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/EnvironmentalAnalysis/CSVs/UI30m.csv"
write.csv(index_df, file = file_path, row.names = FALSE)

