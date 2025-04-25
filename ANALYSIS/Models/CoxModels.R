rm(list = ls())
cat("\014")
graphics.off()
options(scipen = 999)
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
refit <- FALSE
if(refit){
  # Make the model
  cox_1stapproach_bayes <- brm(
    bf(X1st.APPROACH.latency | cens(1 - APPROACHED.SCC) ~ LEVEL + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)), 
    data = final_dataset, 
    family = brmsfamily("cox"),
    prior= priors,
    chains = 4, iter = 20000, warmup = 5000, cores = 5,
    control = list(adapt_delta = 0.9999))
  # Save the model
  saveRDS(cox_1stapproach_bayes, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_1stapproach_bayes.rds")
} else {
  cox_1stapproach_bayes<- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_1stapproach_bayes.rds")
} #if refit <- FALSE, it reads the model, it TRUE, it builds and saves the model
summary(cox_1stapproach_bayes)

#With position
# cox_1stapproach_bayes_position <- brm(
#   bf(X1st.APPROACH.latency | cens(1 - APPROACHED.SCC) ~ LEVEL + Position + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)),
#   data = final_dataset,
#   family = brmsfamily("cox"),
#   prior= priors,
#   chains = 4, iter = 20000, warmup = 5000, cores = 8,
#   control = list(adapt_delta = 0.9999))

# Solving 
if(refit){
  # Make the model
  cox_solving_bayes <- brm(
    bf(SOLVING.latency.1 | cens(1 - SOLVED.SCC) ~ LEVEL + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)), 
    data = final_dataset, 
    family = brmsfamily("cox"),
    prior= priors,
    chains = 4, iter = 20000, warmup = 5000, cores = 5,
    control = list(adapt_delta = 0.9999))
  # Save the model
  saveRDS(cox_solving_bayes, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_solving_bayes.rds")
} else {
  cox_solving_bayes<- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_solving_bayes.rds")
} #if refit <- FALSE, it reads the model, it TRUE, it builds and saves the model
summary(cox_solving_bayes)

#With position
# cox_solving_bayes_position <- brm(
#   bf(SOLVING.latency.1 | cens(1 - SOLVED.SCC) ~ LEVEL + Position + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)),
#   data = final_dataset,
#   family = brmsfamily("cox"),
#   prior= priors,
#   chains = 4, iter = 20000, warmup = 5000, cores = 5,
#   control = list(adapt_delta = 0.9999))

#### PLOTS ####

## PLOTS 1st approach ##
# Load posterior draws from the Bayesian Cox model
desired_order <- rev(c("scaleUI", "Degree", "scaleO.Neill", "scaleRoost.size", "LEVEL3", "LEVEL2"))
y_labels <- c(
  "scaleUI" = "Urbanisation (scaled)",
  "Degree" = "Connectivity (Degree)",
  "scaleO.Neill" = "Entropy (scaled)",
  "scaleRoost.size" = "Roost Size (scaled)",
  "LEVEL3" = "Level 3 vs 1",
  "LEVEL2" = "Level 2 vs 1"
)

# Load posterior draws from the Bayesian Cox model
posterior_long_approach <- cox_1stapproach_bayes %>%
  spread_draws(b_LEVEL2, b_LEVEL3, b_scaleRoost.size, b_Degree, b_scaleUI, b_scaleO.Neill) %>%
  select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "log_hr") %>%
  mutate(
    hazard_ratio = exp(log_hr),
    predictor = gsub("b_", "", predictor),
    # Crea un fattore con l'ordine desiderato
    predictor_factor = factor(predictor, levels = desired_order)
  )

# Plot 1
ggplot(posterior_long_approach, aes(x = log_hr, y = predictor_factor)) +
  stat_halfeye(.width = c(0.90, 0.95), point_interval = "median_qi") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_y_discrete(labels = y_labels) +
  labs(
    x = "Posterior estimate (log hazard)",
    y = NULL,
    title = "Posterior Distributions (Bayesian Cox model – First Approach)"
  ) +
  theme_minimal(base_size = 14)

# Plot 2
ggplot(posterior_long_approach, aes(x = hazard_ratio, y = predictor_factor)) +
  stat_halfeye(.width = c(0.90, 0.95), point_interval = "median_qi") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10() +
  scale_y_discrete(labels = y_labels) +
  labs(
    x = "Hazard Ratio (log scale)",
    y = NULL,
    title = "Posterior Hazard Ratios (model first approach)"
  ) +
  theme_minimal(base_size = 14)

## PLOTS Solving ##
# Load posterior draws from the Bayesian Cox model
posterior_draws <- cox_solving_bayes %>%
  spread_draws(b_LEVEL2, b_LEVEL3,
               b_scaleRoost.size, b_Degree, b_scaleUI, b_scaleO.Neill)

# Reshape to long format and compute hazard ratios
posterior_long <- cox_solving_bayes %>%
  spread_draws(b_LEVEL2, b_LEVEL3,
               b_scaleRoost.size, b_Degree, b_scaleUI, b_scaleO.Neill) %>%
  select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "log_hr") %>%
  mutate(
    hazard_ratio = exp(log_hr),
    predictor = gsub("b_", "", predictor),
    # Crea un fattore con l'ordine desiderato
    predictor_factor = factor(predictor, levels = desired_order)
  )

# Plot 1: Posterior log-hazard estimates
ggplot(posterior_long, aes(x = log_hr, y = predictor_factor)) +
  stat_halfeye(.width = c(0.90, 0.95), point_interval = "median_qi") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_y_discrete(labels = y_labels) +
  labs(
    x = "Posterior estimate (log hazard)",
    y = NULL,
    title = "Posterior distributions (Bayesian Cox model – Solving)"
  ) +
  theme_minimal(base_size = 14)

# Plot 2: Posterior hazard ratios (log-scaled x-axis)
ggplot(posterior_long, aes(x = hazard_ratio, y = predictor_factor)) +
  stat_halfeye(.width = c(0.90, 0.95), point_interval = "median_qi") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10() +
  scale_y_discrete(labels = y_labels) +
  labs(
    x = "Hazard Ratio (log scale)",
    y = NULL,
    title = "Posterior Hazard Ratios (solving model)"
  ) +
  theme_minimal(base_size = 14)

### Posterior probability ###

## Latency until first approach
posterior <- posterior::as_draws_df(cox_1stapproach_bayes)
fixed_effects <- posterior[, grep("^b_", names(posterior))]

# Compute posterior probabilities
posterior_probs <- sapply(fixed_effects, function(x) mean(x > 0))
posterior_probs_df <- data.frame(
  Parameter = names(posterior_probs),
  Posterior_Prob_gt_0 = round(posterior_probs, 3),
  Posterior_Prob_lt_0 = round(1 - posterior_probs, 3)
)

## Latency until solving
posterior <- posterior::as_draws_df(cox_solving_bayes)
fixed_effects <- posterior[, grep("^b_", names(posterior))]

# Compute posterior probabilities
posterior_probs <- sapply(fixed_effects, function(x) mean(x > 0))
posterior_probs_df <- data.frame(
  Parameter = names(posterior_probs),
  Posterior_Prob_gt_0 = round(posterior_probs, 3),
  Posterior_Prob_lt_0 = round(1 - posterior_probs, 3)
)
