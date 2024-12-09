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

final_dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
final_dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 
centrality_day<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Centrality/day_network_metrics.csv')
centrality_night<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Centrality/night_network_metrics.csv')

#####Cleaning#####
# final_dataset_firstapproach <- left_join(final_dataset_firstapproach, approach_summary, by = c("Roost", "Level")) %>% #Add the column with the tot approach time
#   mutate(ApproachDuration = if_else(is.na(ApproachDuration), 0, ApproachDuration)) #Instead of NAs, zeros because it's still informative data
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

### Prepare data for models ###
final_dataset_firstapproach$is_censored <- ifelse(is.na(final_dataset_firstapproach$Latency_1stapproach), 1, 0) #1 Censored data, 0 uncensored
final_dataset_firstapproach$max_latency<- as.numeric(difftime(final_dataset_firstapproach$Took.down.or.Solving, final_dataset_firstapproach$Installation.time.end., units = "secs")) #Compute max latency for each level, either when solved or when uninstalled
final_dataset_firstapproach <- final_dataset_firstapproach %>%
  mutate(Latency_1stappr_censored = ifelse(is.na(Latency_1stapproach), max_latency, Latency_1stapproach)) #New columns with latency 1st approach data integrated with censored data (max latency instead of NAs)

centrality_day <- centrality_day %>%
  rename(
    Roost = SITES_DAY
  )
centrality_night <- centrality_night %>%
  rename(
    Roost = SITES_NIGHT
  )

centrality_day <- centrality_day %>%
  mutate(Roost = recode(Roost,
                        "Coroboree Park" = "Corroboree Park",
                        "Hackett Oval" = "Hackett",
                        "Lyneham shops creek" = "Lyneam",
                        "MacKellar Cook" = "Cook",
                        "Narrabundah German club" = "Narrabundah",
                        "O'Connor banksia wetland" = "O'Connor",
                        "War memorial" = "War Memorial",
                        "Watson Dog Park" = "Watson",
                        "Yarralumla Primary" = "Yarralulma"))

centrality_night <- centrality_night %>%
  mutate(Roost = recode(Roost,
                        "Coroboree Park" = "Corroboree Park",
                        "Hackett Oval" = "Hackett",
                        "Lyneham shops creek" = "Lyneam",
                        "MacKellar Cook" = "Cook",
                        "Narrabundah German club" = "Narrabundah",
                        "O'Connor banksia wetland" = "O'Connor",
                        "War memorial" = "War Memorial",
                        "Watson Dog Park" = "Watson",
                        "Yarralumla Primary" = "Yarralulma"))

#x
final_dataset_firstapproach <- left_join(final_dataset_firstapproach, centrality_day, by = "Roost")
final_dataset_solving <- left_join(final_dataset_solving, centrality_day, by = "Roost")
#y
final_dataset_firstapproach <- left_join(final_dataset_firstapproach, centrality_night, by = "Roost")
final_dataset_solving <- left_join(final_dataset_solving, centrality_night, by = "Roost")

#####Bayesian GLMM#####

###LATENCY 1ST APPROACH###
## Censored model latency, weighted centrality DAY##
model_1stapproach<- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + scale(WEIGHTED_DEGREE.x) + Level +
                          (1 | Roost),
                        data = final_dataset_firstapproach,
                        family = gaussian(),
                        cores = 6,
                        iter = 15000,
                        control = list(adapt_delta = 0.999))
summary(model_1stapproach)

## Censored model latency, unweighted centrality DAY##
model_1stapproachb <- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + scale(UNWEIGHTED_DEGREE.x) + Level +
                          (1 | Roost),
                        data = final_dataset_firstapproach,
                        family = gaussian(),
                        cores = 6,
                        iter = 15000,
                        control = list(adapt_delta = 0.999))
summary(model_1stapproachb)

# Censored model latency, unweighted centrality NIGHT##
model_1stapproachc<- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + scale(WEIGHTED_DEGREE.y) + Level +
                          (1 | Roost),
                        data = final_dataset_firstapproach,
                        family = gaussian(),
                        cores = 6,
                        iter = 15000,
                        control = list(adapt_delta = 0.999, max_treedepth= 11))
summary(model_1stapproachc)

## Censored model latency, unweighted centrality NIGHT##
model_1stapproachd <- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + scale(UNWEIGHTED_DEGREE.y) + Level +
                            (1 | Roost),
                          data = final_dataset_firstapproach,
                          family = gaussian(),
                          cores = 6,
                          iter = 15000,
                          control = list(adapt_delta = 0.999, max_treedepth= 11))
summary(model_1stapproachd)

###SOLVING 1/0 AND LATENCY UNTIL SOLVING###
## Solving model, weighted centrality DAY##
model_solving <- brm(Solved ~ O.Neill + UI + Roost.size + Level + WEIGHTED_DEGREE.x +
                       (1 | Roost),
                     data = final_dataset_solving,
                     family = bernoulli(link = "logit"),
                     cores = 6, iter = 15000, warmup = 2000, control = list(adapt_delta = 0.99, max_treedepth=11))
summary(model_solving)

## Solving model, unweighted centrality DAY##
model_solvingb <- brm(Solved ~ O.Neill + UI + Roost.size + Level + UNWEIGHTED_DEGREE.x +
                       (1 | Roost),
                     data = final_dataset_solving,
                     family = bernoulli(link = "logit"),
                     cores = 6, iter = 15000, warmup = 2000, control = list(adapt_delta = 0.99, max_treedepth=12))
summary(model_solvingb)

## Solving model, weighted centrality NIGHT##
model_solvingc <- brm(Solved ~ O.Neill + UI + Roost.size + Level + WEIGHTED_DEGREE.y +
                       (1 | Roost),
                     data = final_dataset_solving,
                     family = bernoulli(link = "logit"),
                     cores = 6, iter = 15000, warmup = 2000, control = list(adapt_delta = 0.99, max_treedepth=12))
summary(model_solvingc)

## Solving model, unweighted centrality NIGHT##
model_solvingd <- brm(Solved ~ O.Neill + UI + Roost.size + Level + UNWEIGHTED_DEGREE.y +
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, warmup = 2000, control = list(adapt_delta = 0.99, max_treedepth=11))
summary(model_solvingd)

### Solved/Not solved and latency 1st approach ###
## Model solving 2, weighted centrality DAY##
final_dataset_solving$Latency_std <- final_dataset_solving$Latency_1stapproach / sd(final_dataset_solving$Latency_1stapproach, na.rm = TRUE) # Standardize Latency_1stapproach 
model_solving2 <- brm(Solved ~  Latency_std + WEIGHTED_DEGREE.x +
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
summary(model_solving2)

## Model solving 2, unweighted centrality DAY##
model_solving2b <- brm(Solved ~  Latency_std + UNWEIGHTED_DEGREE.x +
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
summary(model_solving2b)

## Model solving 2, weighted centrality NIGHT##
model_solving2c <- brm(Solved ~  Latency_std + WEIGHTED_DEGREE.y +
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
summary(model_solving2c)

## Model solving 2, unweighted centrality NIGHT##
model_solving2d <- brm(Solved ~  Latency_std + UNWEIGHTED_DEGREE.y +
                         (1 | Roost),
                       data = final_dataset_solving,
                       family = bernoulli(link = "logit"),
                       cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
summary(model_solving2d)

### Censored model Latency until solving (Solving time= Solving time - Installation time end###
## Log transformed latency, Gaussian family 
final_dataset_solving$is_censored <- ifelse(is.na(final_dataset_solving$Latency_Solving), 1, 0)
final_dataset_solving$max_latency<- as.numeric(difftime(final_dataset_solving$Took.down.or.Solving, final_dataset_solving$Installation.time.end., units = "secs"))
final_dataset_solving<- final_dataset_solving %>%
  mutate(Latency_solving_censored = ifelse(is.na(Latency_Solving2), max_latency, Latency_Solving2)) #New columns with latency solving data integrated with censored data (max latency instead of NAs)

## Model solving 3, weighted centrality DAY##
model_solving3 <- brm(
  log(Latency_solving_censored) | cens(is_censored) ~ 
    scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + scale(WEIGHTED_DEGREE.x) +
    (1 | Roost), 
  data = final_dataset_solving, 
  family = gaussian(), 
  cores = 6, 
  iter = 15000,
  control = list(adapt_delta = 0.9999999, max_treedepth=11),  save_pars = save_pars(all = TRUE)
)
summary(model_solving3)

## Model solving 3, unweighted centrality DAY##
model_solving3b <- brm(
  log(Latency_solving_censored) | cens(is_censored) ~ 
    scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + scale(UNWEIGHTED_DEGREE.x) +
    (1 | Roost), 
  data = final_dataset_solving, 
  family = gaussian(), 
  cores = 6, 
  iter = 15000,
  control = list(adapt_delta = 0.9999999, max_treedepth=11),  save_pars = save_pars(all = TRUE)
)
summary(model_solving3b)

## Model solving 3, weighted centrality NIGHT##
model_solving3c <- brm(
  log(Latency_solving_censored) | cens(is_censored) ~ 
    scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + scale(WEIGHTED_DEGREE.y) +
    (1 | Roost), 
  data = final_dataset_solving, 
  family = gaussian(), 
  cores = 6, 
  iter = 15000,
  control = list(adapt_delta = 0.9999999, max_treedepth=11),  save_pars = save_pars(all = TRUE)
)
summary(model_solving3c)

## Model solving 3, unweighted centrality NIGHT##
model_solving3d <- brm(
  log(Latency_solving_censored) | cens(is_censored) ~ 
    scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + scale(UNWEIGHTED_DEGREE.y) +
    (1 | Roost), 
  data = final_dataset_solving, 
  family = gaussian(), 
  cores = 6, 
  iter = 15000,
  control = list(adapt_delta = 0.9999999, max_treedepth=11),  save_pars = save_pars(all = TRUE)
)
summary(model_solving3b)
