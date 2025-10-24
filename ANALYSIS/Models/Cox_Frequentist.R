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
summary<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXPERIMENT2023_102025_LF.csv')

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
    Roost.size = as.numeric(Roost.size),
    
    # Group LEVEL into binary variable
    LEVEL_grouped = factor(ifelse(LEVEL == 1, "1", "2-3"), levels = c("1", "2-3"))
  )

### MODELS ###
refit <- FALSE
## First approach model ##
if (refit) {
  cox_1stapproach <- coxme(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ 
                             LEVEL + scale(Roost.size) + 
                             Degree + scale(UI) + scale(O.Neill) + 
                             (1 | Roost),
                           data = final_dataset)
  
  saveRDS(cox_1stapproach, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_1stapproach.rds")
} else {
  cox_1stapproach <- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_1stapproach.rds")
}
summary(cox_1stapproach)
Schoenfeld.res.approach<- cox.zph(cox_1stapproach) #p value > 0.05

# if (refit) {
#   cox_1stapproach_grouped <- coxme(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ 
#                              LEVEL_grouped + scale(Roost.size) + 
#                              Degree + scale(UI) + scale(O.Neill) + 
#                              (1 | Roost),
#                            data = final_dataset)
#   
#   saveRDS(cox_1stapproach_grouped, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_1stapproach_grouped.rds")
# } else {
#   cox_1stapproach_grouped <- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_1stapproach_grouped.rds")
# }
# summary(cox_1stapproach_grouped)

## Solving model ##
if (refit) {
  cox_model_solving <- coxph(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ 
                               LEVEL + scale(Roost.size) + 
                               Degree + scale(UI) + scale(O.Neill) + 
                               frailty(Roost),
                             data = final_dataset,
                             x=TRUE)
  
  saveRDS(cox_model_solving, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_model_solving.rds")
} else {
  cox_model_solving <- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_model_solving.rds")
}
summary(cox_model_solving)
Schoenfeld.res.solving<- cox.zph(cox_model_solving) #p value > 0.05

# ## Solving model 2 ##
# if (refit) {
#   cox_model_solving2 <- coxph(Surv(SOLVING.latency.1, SOLVED.SCC == 1) ~ 
#                                LEVEL + scale(Roost.size) + 
#                                Degree + scale(UI) + scale(O.Neill) + 
#                                frailty(Roost),
#                              data = final_dataset,
#                              x=TRUE)
#   
#   saveRDS(cox_model_solving2, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_model_solving2.rds")
# } else {
#   cox_model_solving2 <- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_model_solving2.rds")
# }
# summary(cox_model_solving2)
# Schoenfeld.res.solving<- cox.zph(cox_model_solving2) #p value > 0.05

# if (refit) {
#   cox_model_solving_grouped <- coxph(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ 
#                                LEVEL_grouped + scale(Roost.size) + 
#                                Degree + scale(UI) + scale(O.Neill) + 
#                                frailty(Roost),
#                              data = final_dataset,
#                              x=TRUE)
#   
#   saveRDS(cox_model_solving_grouped, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_model_solving_grouped.rds")
# } else {
#   cox_model_solving_grouped <- readRDS("/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/Models/cox_model_solving_grouped.rds")
# }
# summary(cox_model_solving_grouped)

### GRAPHS ###
## First approach ##

#Level
fit_approach <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ LEVEL 
                        +cluster(Roost),
                        data = final_dataset)
ggsurvplot(fit_approach, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Task difficulty",
           xlab = "Time until first approach (min)",
           ylab = "Survival probability (not yet approached)",
           palette = "Dark2",
           ggtheme = theme_classic(base_size = 14),
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

# #Level grouped
# fit_approach_grouped <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ LEVEL_grouped
#                                 +cluster(Roost),
#                                 data = final_dataset)
# ggsurvplot(fit_approach_grouped, data = final_dataset,
#            conf.int = TRUE,
#            pval = FALSE,
#            legend.title = "Task difficulty",
#            legend.labs = c("Level 1 (Easy)", "Levels 2 & 3 (Harder)"),
#            xlab = "Time until first approach (min)",
#            ylab = "Survival probability (not yet approached)",
#            palette = "Dark2")

#Centrality
fit_degree <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ Degree + cluster(Roost),
                      data = final_dataset)
ggsurvplot(fit_degree, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Roost centrality",
           xlab = "Time until first approach (min)",
           ylab = "Survival probability (not yet approached)",
           palette = "Oranges",
           ggtheme = theme_classic(base_size = 14),
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Entropy
final_dataset$Entropy_cat <- ifelse(final_dataset$O.Neill > median(final_dataset$O.Neill, na.rm = TRUE),
                                    "High entropy", "Low entropy")
fit_entropy <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ Entropy_cat 
                       + cluster(Roost),
                       data = final_dataset)
ggsurvplot(fit_entropy, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Environmental entropy",
           legend.labs = c("High entropy", "Low entropy"),
           xlab = "Time until first approach (min)",
           ylab = "Survival probability (not yet approached)",
           palette = "BrBg",
           ggtheme = theme_classic(base_size = 14),
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Roost size
final_dataset$Roost_size_cat <- ifelse(final_dataset$Roost.size > median(final_dataset$Roost.size, na.rm = TRUE),
                                       "Large roost", "Small roost")
fit_roostsize <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ Roost_size_cat
                         + cluster(Roost),
                         data = final_dataset)
ggsurvplot(fit_roostsize, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Roost size",
           xlab = "Time until first approach (min)",
           ylab = "Survival probability (not yet approached)",
           palette = "Pastel1",
           ggtheme = theme_classic(base_size = 14),
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#UI
final_dataset$UI_cat <- ifelse(final_dataset$UI > median(final_dataset$UI, na.rm = TRUE),
                               "High urbanisation", "Low urbanisation")
fit_UI <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ UI_cat
                  + cluster(Roost),
                  data = final_dataset)
ggsurvplot(fit_UI, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Urbanisation",
           xlab = "Time until first approach (min)",
           ylab = 'Survival probability (not yet approached)',
           palette = "Accent",
           ggtheme = theme_classic(base_size = 14),
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Roost (random)
fit_roost <- survfit(Surv(X1st.APPROACH.latency, APPROACHED.SCC == 1) ~ Roost,
                     data = final_dataset)
my_colours <- hue_pal()(15)
Roost_code<- c('ANU', 'CK', 'CP', 'GP', 'GU', 'HA', 'HIG', 'LG', 'LY', 'NAR', 'OC', 'TP', 'WA', 'WM', 'YA')
ggsurvplot(fit_roost, data = final_dataset,
           conf.int = FALSE,
           pval = FALSE,
           legend.title = "Roost",
           ylab = 'Survival probability (not yet approached)',
           xlab = 'Time until first approach (min)',
           legend.labs = Roost_code,
           palette = my_colours,
           legend = "right", 
           legend.position = c(1, 0.5),
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

## Solving ##
#Level
fit_solving <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ LEVEL
                       +cluster(Roost),
                               data = final_dataset)
ggsurvplot(fit_solving, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Task Difficulty",
           xlab = "Time until solving (min)",
           ylab = "Survival probability (not yet solved)",
           palette = "Dark2",
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Level grouped
# fit_solving_grouped <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ LEVEL_grouped
#                                +cluster(Roost),
#                                 data = final_dataset)
# ggsurvplot(fit_solving_grouped, data = final_dataset,
#            conf.int = TRUE,
#            pval = FALSE,
#            legend.title = "Task Difficulty",
#            legend.labs = c("Level 1 (Easy)", "Levels 2 & 3 (Harder)"),
#            xlab = "Time until solving (min)",
#            ylab = "Survival probability (not yet solved)",
#            palette = "Dark2",
#            ggtheme = theme_classic(base_size = 14),  
#            font.x = c(12),
#            font.y = c(12),
#            font.tickslab = c(10),
#            font.legend = c(11))

#UI
fit_UI_2 <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ UI_cat
                    + cluster(Roost),
                  data = final_dataset)
ggsurvplot(fit_UI_2, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Urbanisation",
           xlab = "Time until solving (min)",
           ylab = "Survival probability (not yet solved)",
           palette = "Accent",
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Entropy
fit_entropy <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ Entropy_cat
                       +cluster(Roost),
                       data = final_dataset)
ggsurvplot(fit_entropy, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Environmental entropy",
           legend.labs = c("High entropy", "Low entropy"),
           xlab = "Time until solving (min)",
           ylab = "Survival probability (not yet solved)",
           palette = "BrBg",
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Roost (random)
fit_roost <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ Roost,
                     data = final_dataset)
ggsurvplot(fit_roost, data = final_dataset,
           conf.int = FALSE,
           pval = FALSE,
           legend.title = "Roost",
           ylab = 'Survival probability (not yet solved)',
           xlab = 'Time until solving (min)',
           legend.labs = Roost_code,
           palette = my_colours,
           legend = "right", 
           legend.position = c(1, 0.5),
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Centrality
fit_degree <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ Degree +
                      +cluster(Roost),
                      data = final_dataset)
ggsurvplot(fit_degree, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Roost centrality",
           xlab = "Time until solving (min)",
           ylab = "Survival probability (not yet solved)",
           palette = "Oranges",
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

#Roost size
fit_roostsize <- survfit(Surv(SOLVING.latency.2, SOLVED.SCC == 1) ~ Roost_size_cat 
                         +cluster(Roost),
                         data = final_dataset)
ggsurvplot(fit_roostsize, data = final_dataset,
           conf.int = TRUE,
           pval = FALSE,
           legend.title = "Roost size",
           xlab = "Time until solving (min)",
           ylab = "Survival probability (not yet solved)",
           palette = "Pastel1",
           ggtheme = theme_classic(base_size = 14),  
           font.x = c(12),
           font.y = c(12),
           font.tickslab = c(10),
           font.legend = c(11))

## Save model summaries ##
# summary_1stapproach <- capture.output(summary(cox_1stapproach))
# writeLines(summary_1stapproach, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/ARTICLE/Supplementary/cox_1stapproach.txt")
# 
# summary_solving <- capture.output(summary(cox_model_solving))
# writeLines(summary_solving, "/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/ARTICLE/Supplementary/cox_solving.txt")


### Model 3: Binary solving outcome (Frequentist) ###
glmm_solving_freq <- glmer(
  SOLVED.SCC ~ LEVEL + scale(Roost.size) + Degree + scale(UI) + scale(O.Neill) + (1 | Roost),
  data = final_dataset,
  family = binomial(link = "logit")
)
summary(glmm_solving_freq)
# Get odds ratios (exponentiated coefficients)
exp(fixef(glmm_solving_freq))
# Confidence intervals on odds ratio scale
exp(confint(glmm_solving_freq, method = "Wald"))

## Plots
library(ggplot2)
library(broom.mixed)  # for tidy() function with mixed models

### Plot frequentist binary GLMM results ###

# Extract coefficients and confidence intervals
freq_results <- broom.mixed::tidy(glmm_solving_freq, conf.int = TRUE, conf.method = "Wald")

# Remove intercept and format
freq_results <- freq_results %>%
  filter(effect == "fixed", term != "(Intercept)") %>%
  mutate(
    # Calculate odds ratios
    odds_ratio = exp(estimate),
    or_lower = exp(conf.low),
    or_upper = exp(conf.high),
    # Clean up term names
    term_clean = case_when(
      term == "LEVEL2" ~ "Level 2 vs 1",
      term == "LEVEL3" ~ "Level 3 vs 1",
      term == "scale(Roost.size)" ~ "Roost Size (scaled)",
      term == "Degree" ~ "Connectivity (Degree)",
      term == "scale(UI)" ~ "Urbanisation (scaled)",
      term == "scale(O.Neill)" ~ "Entropy (scaled)",
      TRUE ~ term
    ),
    # Create factor with desired order
    term_factor = factor(term_clean, 
                         levels = rev(c("Level 2 vs 1", "Level 3 vs 1", 
                                        "Roost Size (scaled)", "Connectivity (Degree)",
                                        "Urbanisation (scaled)", "Entropy (scaled)")))
  )

# Plot 1: Log-odds (coefficients)
ggplot(freq_results, aes(x = estimate, y = term_factor)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = "Coefficient estimate (log odds)",
    y = NULL,
    title = "Binomial GLMM (Probability of Solving)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Plot 2: Odds ratios
ggplot(freq_results, aes(x = odds_ratio, y = term_factor)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = or_lower, xmax = or_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Odds Ratios (Binary Model)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
