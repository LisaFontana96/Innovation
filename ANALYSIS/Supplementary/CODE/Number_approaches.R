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
final_dataset<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/DATA/DATASET/Finaldataset_SCC.csv') 
final_dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
final_dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 

#Convert data in date-time format + general data cleaning
final_dataset$Position<- as.factor(final_dataset$Position)
final_dataset$Level<- as.factor(final_dataset$Level)
final_dataset$Installation.time.end.<- as.POSIXct(paste(final_dataset$Installation.date, final_dataset$Installation.time.end.),
                                         format="%Y%m%d %H:%M", tz = "Australia/Sydney")
final_dataset$Un.installation.time<- as.POSIXct(paste(final_dataset$Un.installation.date, final_dataset$Un.installation.time),
                                                  format="%Y%m%d %H:%M", tz = "Australia/Sydney")
final_dataset$Approach.start<- as.POSIXct(paste(final_dataset$Approach.date, final_dataset$Approach.start),
                                                format="%Y%m%d %H:%M", tz = "Australia/Sydney")
final_dataset$Approach.end<- as.POSIXct(paste(final_dataset$Approach.date, final_dataset$Approach.end),
                                                format="%Y%m%d %H:%M", tz = "Australia/Sydney")

final_dataset_firstapproach$Position<- as.factor(final_dataset_firstapproach$Position)
final_dataset_firstapproach$Level<- as.factor(final_dataset_firstapproach$Level)
final_dataset_firstapproach$Installation.time.end. <- as.POSIXct(final_dataset_firstapproach$Installation.time.end., format = "%d/%m/%Y %H:%M")
final_dataset_firstapproach$Took.down.or.Solving <- as.POSIXct(final_dataset_firstapproach$Took.down.or.Solving, format = "%d/%m/%Y %H:%M")
final_dataset_firstapproach$Solving.time <- as.POSIXct(final_dataset_firstapproach$Solving.time, format = "%d/%m/%Y %H:%M:%S")
final_dataset_firstapproach$is_censored <- ifelse(is.na(final_dataset_firstapproach$Latency_1stapproach), 1, 0) #1 Censored data, 0 uncensored
final_dataset_firstapproach$max_latency<- as.numeric(difftime(final_dataset_firstapproach$Took.down.or.Solving, final_dataset_firstapproach$Installation.time.end., units = "secs")) #Compute max latency for each level, either when solved or when uninstalled
final_dataset_firstapproach <- final_dataset_firstapproach %>%
  mutate(Latency_1stappr_censored = ifelse(is.na(Latency_1stapproach), max_latency, Latency_1stapproach)) #New columns with latency 1st approach data integrated with censored data (max latency instead of NAs)

final_dataset_solving$Position<- as.factor(final_dataset_solving$Position)
final_dataset_solving$Level<- as.factor(final_dataset_solving$Level)
final_dataset_solving$Installation.time.end. <- as.POSIXct(final_dataset_solving$Installation.time.end., format = "%d/%m/%Y %H:%M")
final_dataset_solving$Took.down.or.Solving <- as.POSIXct(final_dataset_solving$Took.down.or.Solving, format = "%d/%m/%Y %H:%M")
final_dataset_solving$Solving.time <- as.POSIXct(final_dataset_solving$Solving.time, format = "%d/%m/%Y %H:%M:%S")
final_dataset_solving$is_censored <- ifelse(is.na(final_dataset_solving$Latency_Solving), 1, 0)
final_dataset_solving$max_latency<- as.numeric(difftime(final_dataset_solving$Took.down.or.Solving, final_dataset_solving$Installation.time.end., units = "secs"))
final_dataset_solving<- final_dataset_solving %>%
  mutate(Latency_solving_censored = ifelse(is.na(Latency_Solving2), max_latency, Latency_Solving2)) #New columns with latency solving data integrated with censored data (max latency instead of NAs)

# Calculate 48 hours after installation end time
final_dataset <- final_dataset %>%
  mutate(end_48h = Installation.time.end. + hours(48))
# Filter for approaches within the first 48 hours
final_dataset_48h <- final_dataset %>%
  filter(Approach.start >= Installation.time.end. & Approach.start <= end_48h)

# Group by roost and level, and count approaches
approach_counts <- final_dataset_48h %>%
  group_by(Roost, Level) %>%
  summarise(approach_count = n())

#Final dataset
all_roost_levels <- final_dataset %>%
  select(Roost, Level) %>%
  distinct()

approach_counts_48h <- all_roost_levels %>%
  left_join(approach_counts, by = c("Roost", "Level")) %>%
  mutate(approach_counts = replace_na(approach_count, 0))

approach_counts_48h <- approach_counts_48h %>%
  select(-3)

#print(approach_counts_48h)

final_dataset_solving<- final_dataset_solving %>%
  left_join(approach_counts_48h, by = c("Roost", "Level")) %>%
  replace_na(list(approach_counts = 0))

final_dataset_firstapproach<- final_dataset_firstapproach %>%
  left_join(approach_counts_48h, by = c("Roost", "Level")) %>%
  replace_na(list(approach_counts = 0))

### Bayesian models ###

#Is there a relation between the number of approaches in the first 48h and the latency until first approach?
model_approaches<- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + approach_counts +
                          (1 | Roost),
                        data = final_dataset_firstapproach,
                        family = gaussian(),
                        cores = 6,
                        iter = 15000,
                        control = list(adapt_delta = 0.999))

#Does the number of approaches in the first 48 h relate with environmental and social factors?
# model_approaches2<- brm(approach_counts ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + Level +
#                           (1 | Roost),
#                         data = final_dataset_firstapproach,
#                         family = gaussian(),
#                         cores = 6,
#                         iter = 15000,
#                         control = list(adapt_delta = 0.999))
# #weird output. Rhat values are ok but pp_check looks weird

#Is there a relation between the probability of solving and the number of approaches in the first 48h?
final_dataset_solving$Latency_std <- final_dataset_solving$Latency_1stapproach / sd(final_dataset_solving$Latency_1stapproach, na.rm = TRUE) # Standardize Latency_1stapproach 
model_approaches3 <- brm(Solved ~  Latency_std + approach_counts +
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))

