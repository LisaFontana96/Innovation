rm(list = ls())
cat("\014")
library(brms)
library(hms)
library(RcppEigen)
library(ggplot2)
library(nortest)
library(dplyr)
library(summarytools)
library(lme4)  
library(lmerTest) 

final_dataset<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/ANALYSIS/Models/Dataset_ModelFinal.csv') 
approach_summary<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/DATA/approach_summary.csv')

#####Cleaning#####
final_dataset <- left_join(final_dataset, approach_summary, by = c("Roost", "Level")) %>% #Add the column with the tot approach time
  mutate(ApproachDuration = if_else(is.na(ApproachDuration), 0, ApproachDuration)) #Instead of NAs, zeros because it's still informative data
final_dataset$Position<- as.factor(final_dataset$Position)
final_dataset$Level<- as.factor(final_dataset$Level)
final_dataset$Installation.time.end. <- as.POSIXct(final_dataset$Installation.time.end., format = "%d/%m/%Y %H:%M:%S")
final_dataset$Took.down.or.Solving <- as.POSIXct(final_dataset$Took.down.or.Solving, format = "%d/%m/%Y %H:%M:%S")
final_dataset$Solving.time <- as.POSIXct(final_dataset$Solving.time, format = "%d/%m/%Y %H:%M:%S")
final_dataset$Tot_time_data <- as.numeric(difftime(final_dataset$Took.down.or.Solving, final_dataset$Installation.time.end, units = "secs")) #Total time up, until solving, or until complete data available
final_dataset <- final_dataset %>%
  mutate(Ratio = ApproachDuration / Tot_time_data)
final_dataset$Ratio <- round(final_dataset$Ratio, digits = 7)

#####Correlation testing#####
###Environmental variables
correlation_matrix_environment <- cor(cbind(final_dataset$Shannon.index.Artificial.Surface, final_dataset$ArtificialSurface., final_dataset$General.Shannon.index, final_dataset$Shannon.index.water, final_dataset$Shannon.index.natural.terrestrial.vegetation))
print(correlation_matrix_environment)

cortest1<- cor.test(final_dataset$ArtificialSurface.,final_dataset$Shannon.index.Artificial.Surface)
print(cortest1)
cortest2<-cor.test(final_dataset$Shannon.index.Artificial.Surface,final_dataset$General.Shannon.index)
print(cortest2)
cortest3<-cor.test(final_dataset$Shannon.index.water, final_dataset$General.Shannon.index)
print(cortest3)
cortest4<-cor.test(final_dataset$Shannon.index.natural.terrestrial.vegetation, final_dataset$General.Shannon.index)
print(cortest4)
#All environmental variables highly correlated between each other, so I will use in the end only the General Shannon Index

#####Variables distribution#####
###General Shannon index
ggplot(final_dataset, aes(x = General.Shannon.index)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "General Shannon index", y = "Density")
###Roost size
ggplot(final_dataset, aes(x = Roost.size)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Roost size", y = "Density")
###Latencies
ggplot(final_dataset, aes(x = Latency_1stapproach)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Latency 1st approach", y = "Density")
ggplot(final_dataset, aes(x = Latency_Solving)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Latency solving", y = "Density")
###Mean approach duration
ggplot(final_dataset, aes(x = Mean.approach.duration)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Mean approach duration", y = "Density")
###Ratio 
ggplot(final_dataset, aes(x = Ratio)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Ratio", y = "Density")

#####Bayesian GLMM#####

###First approach/ Neophobia
model_1stapproach <- brm(log(Latency_1stapproach) ~ Roost.size + General.Shannon.index + Level + Position +
                           (1 | Roost), 
                         data = final_dataset, 
                         family = gaussian(), 
                         prior = NULL, 
                         cores = 6, 
                         iter = 15000,
                         control = list(adapt_delta = 0.999))
summary(model_1stapproach)

###Ratio time spent in approaches per tot time up

