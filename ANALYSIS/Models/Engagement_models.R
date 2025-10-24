rm(list = ls())
library(brms)
library(splines2)
library(survminer)
library(survival)
library(dplyr)
library(tidybayes)
library(tidyr)
library(ggplot2)
library(summarytools)
library(lubridate)
library(car)
library(broom.mixed)
library(coxme)
library(lme4)  

# dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
# dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 
summary_data <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXPERIMENT2023_102025_LF.csv')

# Model 1: What predicts engagement time (for approached tasks)?
model<- lmer(log(total_engagement_time_all) ~ LEVEL + scale(Roost.size) + 
       Degree + scale(UI) + scale(O.Neill) + (1|Roost),
     data = summary_data %>% filter(n_approaches_total > 0))
summary(model)

# Model 2: What predicts engagement time (for solved tasks)?
model2<- lmer(log(total_engagement_time_to_solve) ~ LEVEL + scale(Roost.size) + 
                Degree + scale(UI) + scale(O.Neill) + (1|Roost),
              data = summary_data %>% filter(n_approaches_total > 0))
summary(model2)

# Model 3: Does engagement predict solving?
model2<- glmer(SOLVED.SCC ~ n_approaches_total + LEVEL + scale(Roost.size) + 
        Degree + scale(UI) + scale(O.Neill) + (1|Roost),
      data = summary_data,
      family = binomial)
summary(model3)

### BAYESIAN ANALYSIS ### 

# Model 1: What predicts engagement time (for approached tasks)?
bayes_engagement_all <- brm(
  log(total_engagement_time_all) ~ LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill) + (1|Roost),
  data = summary_data %>% filter(n_approaches_total > 0),
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = Intercept),
    prior(normal(0, 2), class = b),
    prior(exponential(1), class = sd),
    prior(exponential(1), class = sigma)),
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123, control = list(adapt_delta = 0.999))

summary(bayes_engagement_all)
plot(bayes_engagement_all)

# Model 2: What predicts engagement time before solving (for approached tasks)?
bayes_engagement_before <- brm(
  log(total_engagement_time_to_solve) ~ LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill) + (1|Roost),
  data = summary_data %>% filter(n_approaches_total > 0),
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = Intercept),
    prior(normal(0, 2), class = b),
    prior(exponential(1), class = sd),
    prior(exponential(1), class = sigma)),
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123, control = list(adapt_delta = 0.999))

summary(bayes_engagement_before)
plot(bayes_engagement_before)

# Model 3: Does engagement predict solving?
bayes_solving_with_engagement <- brm(
  SOLVED.SCC ~ total_engagement_time_to_solve + LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill) + (1|Roost),
  data = summary_data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = Intercept),
    prior(normal(0, 1.5), class = b),
    prior(exponential(1), class = sd)),
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 123, control = list(adapt_delta = 0.999))

summary(bayes_solving_with_engagement)
plot(bayes_solving_with_engagement)

