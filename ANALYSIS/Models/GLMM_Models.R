rm(list = ls())
cat("\014")
library(hms)
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

#####GLMM#####

##Change reference for the model if needed
#final_dataset$Position <- final_dataset$Position %>%
#relevel(ref="R")
#final_dataset$Level <- final_dataset$Level %>%
#relevel(ref="1")

###First approach/ Neophobia
model_1stapproacha <- lmer(log(Latency_1stapproach) ~ Level + Position + 
                                (1 | Roost), 
                              data = final_dataset)
model_1stapproachb <- lmer(log(Latency_1stapproach) ~ General.Shannon.index + (1 | Roost), 
                          data = final_dataset)

summary(model_1stapproacha) #Intercept significant, not level or position
summary(model_1stapproachb) #Intercept significant, Shannon index significant pvalue<0.0149

###Solving
model_solvinga <- glmer(Solved ~ Position + Level +
                         (1 | Roost),
                       data = final_dataset,
                       family = binomial(link = "logit"))
model_solvingb <- glmer(Solved ~ General.Shannon.index +
                          (1 | Roost),
                        data = final_dataset,
                        family = binomial(link = "logit"))

summary(model_solvinga) #Nothing significant
summary(model_solvingb) #Nothing significant
