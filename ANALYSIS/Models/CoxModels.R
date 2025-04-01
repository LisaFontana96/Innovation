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
    bf(X1st.APPROACH.latency | cens(1 - APPROACHED.SCC) ~ LEVEL + Position + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)), 
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

# Solving 
if(refit){
  # Make the model
  cox_solving_bayes <- brm(
    bf(SOLVING.latency.1 | cens(1 - SOLVED.SCC) ~ LEVEL + Position + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost)), 
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

## PLOTS 1st approach ##
new_roost_data <- final_dataset %>%
  group_by(Roost) %>%
  summarise(
    LEVEL = "1",
    Position = "C",
    Roost.size = mean(Roost.size, na.rm = TRUE),
    Degree = mean(Degree, na.rm = TRUE),
    UI = mean(UI, na.rm = TRUE),
    O.Neill = mean(O.Neill, na.rm = TRUE),
    .groups = "drop"
  )

# Standardize predictors
new_roost_data <- new_roost_data %>%
  mutate(
    Roost.size = scale(Roost.size)[,1],
    UI = scale(UI)[,1],
    O.Neill = scale(O.Neill)[,1],
    X1st.APPROACH.latency = 1000  
  )


# Posterior linear predictor
lp_draws <- add_linpred_draws(
  cox_1stapproach_bayes,
  newdata = new_roost_data,
  re_formula = NULL,
  ndraws = 500,
  value = "eta"  
)


# Baseline survival estimate from frequentist Cox model
baseline_cox <- coxph(Surv(X1st.APPROACH.latency, APPROACHED.SCC) ~ 1, data = final_dataset)
baseline_fit <- survfit(baseline_cox)
baseline_surv <- data.frame(
  time = baseline_fit$time,
  S0 = baseline_fit$surv
)

surv_preds <- expand_grid(baseline_surv, lp_draws) %>%
  mutate(
    surv = S0 ^ exp(eta),
    Roost = new_roost_data$Roost[.row]
  )

# Summarise across posterior draws
plot_roost_surv <- surv_preds %>%
  group_by(time, Roost) %>%
  summarise(
    surv_prob = median(surv),
    .lower = quantile(surv, 0.025),
    .upper = quantile(surv, 0.975),
    .groups = "drop"
  )

# Plots

ggplot(plot_roost_surv, aes(x = time, y = surv_prob)) +                        
  geom_line(aes(color = Roost)) +                                    # Draw median survival curves, one per roost
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = Roost),       # Add shaded 95% credible intervals per roost
              alpha = 0.2, color = NA) +                                      
  facet_wrap(~ Roost, scales = "free_y") +                                     
  labs(                                                                        
    title = "Survival Curves by Roost (First Approach)",
    x = "Time since installation (minutes)",
    y = "Probability not yet approached"
  ) +
  theme_minimal() +                                                            
  theme(legend.position = "none")                                              

# -----------------------------------------------------------------------------

plot_roost_surv_zoom <- plot_roost_surv %>%
  filter(time <= 3000)                                               # Filter data to only include first 3000 minutes

ggplot(plot_roost_surv_zoom, aes(x = time, y = surv_prob)) +                  # Same plot setup as above, now using zoomed data
  geom_line(aes(color = Roost)) +                                            
  facet_wrap(~ Roost, scales = "free_y") +                                   
  labs(
    title = "Survival Curves by Roost (First Approach)",                       
    x = "Time since installation (minutes)",
    y = "Probability not yet approached"
  ) +
  theme_minimal() +
  theme(legend.position = "none")                 
