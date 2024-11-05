rm(list = ls())
cat("\014")
library(brms)
library(survival)
library(hms)
library(RcppEigen)
library(ggplot2)
library(nortest)
library(dplyr)
library(summarytools)
library(lubridate)
library(tidyr)

#Load data
final_dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
final_dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 

#Graphs
plot1<- ggplot(final_dataset_firstapproach, aes(x = as.factor(Level), y = Latency_1stapproach, fill = as.factor(Level))) +
  geom_boxplot() +
  labs(title = "Latency to Solve Tasks by Level",
       x = "Task Level",
       y = "Latency (seconds)") +
  theme_minimal() +
  theme(legend.position = "none")

plot2<- ggplot(final_dataset_firstapproach, aes(x = Roost.size, y = Latency_1stapproach)) +
  geom_point(aes(color = as.factor(Level))) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Roost Size on Latency",
       x = "Roost Size",
       y = "Latency (seconds)",
       color = "Task Level") +
  theme_minimal()

#Latency solving= from first approach end to solving
plot3<- ggplot(final_dataset_firstapproach, aes(x = Latency_Solving, y = Roost, color = as.factor(Level), shape = as.factor(Level))) +
  geom_point(size = 3) + 
  labs(x = "Time to Solve (seconds)", y = "Roosts", title = "Time to Solve Tasks by Roost and Level") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "blue", "2" = "orange", "3" = "green")) + 
  theme(legend.title = element_blank()) 

#Latency solving= from installation time end to solving
plot4<- ggplot(final_dataset_solving, aes(x = Latency_Solving2, y = Roost, color = as.factor(Level), shape = as.factor(Level))) +
  geom_point(size = 3) + 
  labs(x = "Time to Solve (seconds)", y = "Roosts", title = "Time to Solve Tasks by Roost and Level") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "red", "2" = "violet", "3" = "black")) + 
  theme(legend.title = element_blank()) 
