rm(list = ls())
cat("\014")
library(INLA)
library(splines2)
library(survminer)
library(survival)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(summarytools)
library(lubridate)
library(car)

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
    Roost.size = as.numeric(Roost.size))

## Baeysian Cox Survival Models, INLA package ##

cox_firstapproach <- inla(
  inla.surv(time = X1st.APPROACH.latency, event = APPROACHED.SCC) ~ 
    LEVEL + Position + scale(Roost.size) + scale(UI) + scale(O.Neill) + Degree +
    f(Roost, model = "iid") , 
  data = final_dataset,
  family = "coxph",
  control.fixed = list(
    mean = 0, prec = 1  # Normal(0,1) prior for fixed effects
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.inla = list(strategy = "adaptive")
)

cox_solving <- inla(
  inla.surv(time = SOLVING.latency.1, event = SOLVED.SCC) ~ 
    LEVEL + Position + scale(Roost.size) + scale(UI) + scale(O.Neill) + Degree +
    f(Roost, model = "iid") , 
  data = final_dataset,
  family = "coxph",
  control.fixed = list(
    mean = 0, prec = 0.5  # Wider normal(0,0.5) prior for fixed effects
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE,  config = TRUE),
  control.inla = list(strategy = "adaptive")
)

## PLOTS ##
