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
library(lme4)  
library(lmerTest) 
library(pacman)

#p_update()

final_dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
final_dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 

final_dataset_firstapproach$Position<- as.factor(final_dataset_firstapproach$Position)
final_dataset_firstapproach$Level<- as.factor(final_dataset_firstapproach$Level)
final_dataset_firstapproach$Installation.time.end. <- as.POSIXct(final_dataset_firstapproach$Installation.time.end., format = "%d/%m/%Y %H:%M")
final_dataset_firstapproach$Took.down.or.Solving <- as.POSIXct(final_dataset_firstapproach$Took.down.or.Solving, format = "%d/%m/%Y %H:%M")
final_dataset_firstapproach$Solving.time <- as.POSIXct(final_dataset_firstapproach$Solving.time, format = "%d/%m/%Y %H:%M:%S")

final_dataset_solving$Position<- as.factor(final_dataset_solving$Position)
final_dataset_solving$Level<- as.factor(final_dataset_solving$Level)
final_dataset_solving$Installation.time.end. <- as.POSIXct(final_dataset_solving$Installation.time.end., format = "%d/%m/%Y %H:%M")
final_dataset_solving$Took.down.or.Solving <- as.POSIXct(final_dataset_solving$Took.down.or.Solving, format = "%d/%m/%Y %H:%M")
final_dataset_solving$Solving.time <- as.POSIXct(final_dataset_solving$Solving.time, format = "%d/%m/%Y %H:%M:%S")

#From seconds to days
final_dataset_firstapproach$Lat1stappr_days<- final_dataset_firstapproach$Latency_1stapproach/86400
final_dataset_solving$LatSolv_days<- final_dataset_solving$Latency_Solving/86400
final_dataset_solving$LatSolv_days2<- final_dataset_solving$Latency_Solving2/86400
final_dataset_solving$Latency_1stapproach_days<- final_dataset_solving$Latency_1stapproach/86400

final_dataset_firstapproach$is_censored <- ifelse(is.na(final_dataset_firstapproach$Latency_1stapproach), 1, 0) #1 Censored data, 0 uncensored
final_dataset_firstapproach$max_latency<- as.numeric(difftime(final_dataset_firstapproach$Took.down.or.Solving, final_dataset_firstapproach$Installation.time.end., units = "secs")) #Compute max latency for each level, either when solved or when uninstalled
final_dataset_firstapproach <- final_dataset_firstapproach %>%
  mutate(Latency_1stappr_censored = ifelse(is.na(Latency_1stapproach), max_latency, Latency_1stapproach)) #New columns with latency 1st approach data integrated with censored data (max latency instead of NAs)
final_dataset_firstapproach$Latency_1stappr_censored_days<- final_dataset_firstapproach$Latency_1stappr_censored / 86400

final_dataset_solving$is_censored <- ifelse(is.na(final_dataset_solving$Latency_Solving), 1, 0)
final_dataset_solving$max_latency<- as.numeric(difftime(final_dataset_solving$Took.down.or.Solving, final_dataset_solving$Installation.time.end., units = "secs"))
final_dataset_solving<- final_dataset_solving %>%
  mutate(Latency_solving_censored = ifelse(is.na(Latency_Solving), max_latency, Latency_Solving)) #New columns with latency solving data integrated with censored data (max latency instead of NAs)
final_dataset_solving$Latency_solving_censored_days<-final_dataset_solving$Latency_solving_censored/86400

#Lat 1st appr 
model_1stapproach_days<- brm(log(Latency_1stappr_censored_days) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + Level +
                          (1 | Roost),
                        data = final_dataset_firstapproach,
                        family = gaussian(),
                        cores = 6,
                        iter = 15000,
                        control = list(adapt_delta = 0.999))
summary(model_1stapproach_days)

#Latency solving
model_solving1 <- brm(
  log(Latency_solving_censored) | cens(is_censored) ~ 
    scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
    (1 | Roost), 
  data = final_dataset_solving, 
  family = gaussian(), 
  cores = 6, 
  iter = 15000,
  control = list(adapt_delta = 0.9999999),  save_pars = save_pars(all = TRUE)
)
summary(model_solving1)

#Prob solving and latency
final_dataset_solving$Latency_std_days <- final_dataset_solving$Latency_1stapproach_days / sd(final_dataset_solving$Latency_1stapproach_days, na.rm = TRUE) # Standardize Latency_1stapproach_days
model_solving2 <- brm(Solved ~  Latency_std_days + 
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
summary(model_solving2)


