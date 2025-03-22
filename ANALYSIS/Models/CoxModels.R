rm(list = ls())
cat("\014")
library(brms)
library(splines2)
library(survminer)
library(survival)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(summarytools)
library(lubridate)
library(car)

# dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
# dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 
summary<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXP_LF_EcologicalVariables.csv')

### Cleaning ###

summary <- summary %>%
  mutate(
    # Date and time objects
    datetime_1stapproach = ymd_hms(paste(as.character(X1st.APPROACH.date), as.character(X1st.APPROACH.time))),
    datetime_endpoint1 = ymd_hms(paste(as.character(END.POINT.1st.approach), as.character(END.POINT.1st.approach.time))),
    datetime_endpoint2 = ymd_hms(paste(as.character(END.POINT.solving), as.character(END.POINT.solving.time))),
    datetime_solving = ymd_hms(paste(as.character(SOLVING.date), as.character(SOLVING.time))),
    datetime_installation = ymd_hms(paste(as.character(INSTALLATION.DATE.END), as.character(INSTALLATION.TIME.END))),
    
    # Compute flag (1 for 'To compute', 0 for others, NA for NA)
    already_solved = case_when(
      X1st.APPROACH.latency == 'To compute' ~ 1,
      is.na(X1st.APPROACH.latency) ~ NA_real_,
      TRUE ~ 0 ),
    
    # Latencies and right censored data
    X1st.APPROACH.latency = if_else(X1st.APPROACH.latency == 'To compute',
                                    as.numeric(difftime(datetime_1stapproach, datetime_installation, units = "secs")),
                                    as.numeric(X1st.APPROACH.latency, na.rm = TRUE)))

# For those levels that were never approached OR there is no data, end point is represented by the datetime of un-installation (or data up to camera failure)

final_dataset <- summary %>%
  mutate(
    # Compute first approach latency if NA and based on FATE.1st.approach
    X1st.APPROACH.latency = ifelse(
      (is.na(X1st.APPROACH.latency) & FATE.1st.approach %in% c("Never approached", "No data")),
      as.numeric(difftime(datetime_endpoint1, datetime_installation, units = "secs")),
      X1st.APPROACH.latency
    ),
    
    # Compute solving latency if NA
    # From first approach to solving
    SOLVING.latency.1 = ifelse(
      is.na(SOLVING.latency.1),
      as.numeric(difftime(datetime_endpoint2, datetime_installation, units = "secs")),
      SOLVING.latency.1
    ),
    # From installation end to solving
    SOLVING.latency.2 = ifelse(
      is.na(SOLVING.latency.2),
      as.numeric(difftime(datetime_endpoint2, datetime_installation, units = "secs")),
      SOLVING.latency.2
    ),
    
    # Convert to minutes and round 
    X1st.APPROACH.latency = round(X1st.APPROACH.latency / 60),
    SOLVING.latency.1 = round(SOLVING.latency.1 / 60),
    SOLVING.latency.2 = round(SOLVING.latency.2 / 60),
    
    # Convert class objects
    Position = as.factor(Position),
    LEVEL = as.factor(LEVEL),
    Roost = as.factor(Roost),
    Degree = as.numeric(Degree),
    WTDegree = as.numeric(WTDegree),
    Roost.size = as.numeric(Roost.size))

## Frequentist approach ##

#Latency first approach
cox_1stapproach <- coxph(Surv(X1st.APPROACH.latency, SOLVED.SCC) ~ Position + scale(Roost.size) + scale(UI) + scale(O.Neill), data = final_dataset)
cox_1stapproach2 <- coxph(Surv(X1st.APPROACH.latency, APPROACHED.SCC) ~ Position + scale(Roost.size) + scale(UI) + scale(O.Neill), data = final_dataset)
#With frequentist method, the model converges only if there is no random effect

#Latency solving
cox_solving <- coxph(Surv(SOLVING.latency.1, SOLVED.SCC) ~ Position + scale(Roost.size) + scale(UI) + scale(O.Neill), data = final_dataset)
cox_solving2 <- coxph(Surv(SOLVING.latency.2, SOLVED.SCC) ~ Position + scale(Roost.size) + scale(UI) + scale(O.Neill), data = final_dataset)
#With frequentist method, the model converges only if there is no random effect

## Baeysian approach ##

#PRIORS
priors <- c(
  prior(normal(0, 1), class = "b"),  
  prior(normal(0, 1), class = "Intercept"),  
  prior(exponential(1), class = "sd")  # exponential for Roost (random effect)
)

#Change reference level if needed
#final_dataset$Position <- final_dataset$Position %>%
#relevel(ref="R")
#final_dataset$Level <- final_dataset$Level %>%
#relevel(ref="1")

# 1st approach and approached 1/0
cox_1stapproach_bayes <- brm(
  bf(X1st.APPROACH.latency | cens(1 - APPROACHED.SCC) ~ LEVEL + Position + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)), 
  data = final_dataset, 
  family = brmsfamily("cox"),
  prior= priors,
  chains = 4, iter = 20000, warmup = 5000, cores = 5,
  control = list(adapt_delta = 0.9999))
summary(cox_1stapproach_bayes)

# Solving 
cox_solving_bayes <- brm(
  bf(SOLVING.latency.1 | cens(1 - SOLVED.SCC) ~ LEVEL + Position + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)), 
  data = final_dataset, 
  family = brmsfamily("cox"),
  prior= priors,
  chains = 4, iter = 20000, warmup = 5000, cores = 5,
  control = list(adapt_delta = 0.9999))
summary(cox_solving_bayes)

## PLOTS ##
