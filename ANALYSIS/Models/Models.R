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

final_dataset_firstapproach<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_1stApproach.csv') 
final_dataset_solving<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/DatasetModel_Solving.csv') 
# approach_summary<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/DATA/approach_summary.csv')


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

#####Correlation testing#####
###Environmental variables
correlation_matrix_environment <- cor(cbind(final_dataset_firstapproach$UI, final_dataset_firstapproach$O.Neill, final_dataset_firstapproach$Roost.size))
print(correlation_matrix_environment)

cortest1<- cor.test(final_dataset_firstapproach$UI,final_dataset_firstapproach$O.Neill)
print(cortest1) #Moderate correlation (0.4230468), p value 0.01015
cortest2<-cor.test(final_dataset_firstapproach$UI,final_dataset_firstapproach$Roost.size)
print(cortest2) #Cor 0.3168432, p value 0.05973
cortest3<-cor.test(final_dataset_firstapproach$O.Neill, final_dataset_firstapproach$Roost.size)
print(cortest3) #Weak correlation (0.1936932), p value 0.2577
cortest4<- cor.test(final_dataset_firstapproach$Latency_1stapproach, final_dataset_firstapproach$O.Neill, method = "pearson", use = "complete.obs")
print(cortest4) #Weak to moderate negative correlation -0.34 pvalue 0.09
cortest5<- cor.test(final_dataset_firstapproach$Latency_1stapproach, final_dataset_firstapproach$UI, method = "pearson", use = "complete.obs")
print(cortest5) #Moderate to strong negative correlation -0.61 pvalue 0.001
cortest6<- cor.test(final_dataset_firstapproach$Latency_1stapproach, final_dataset_firstapproach$Roost.size, method = "pearson", use = "complete.obs")
print(cortest6) #Weak negative correlation -0.18 not significant
#Roost size is significantly positively correlated with the O'Neill index, while the latency until first approach is significantly negatively correlated with Shannon index

#####Variables distribution#####
###O'Neill index
ggplot(final_dataset_firstapproach, aes(x = O.Neill)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "O'Neill index", y = "Density")
###Roost size
ggplot(final_dataset_firstapproach, aes(x = Roost.size)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Roost size", y = "Density")
###Latencies
ggplot(final_dataset_firstapproach, aes(x = Latency_1stapproach)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Latency 1st approach", y = "Density")
ggplot(final_dataset_firstapproach, aes(x = log(Latency_1stapproach))) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "log Latency 1st approach", y = "Density")
shapiro.test(log(final_dataset_firstapproach$Latency_1stapproach)) #Latency first approach has a normal distribution according to shapiro test

ggplot(final_dataset_firstapproach, aes(x = Latency_Solving)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Latency solving", y = "Density")
###Mean approach duration
ggplot(final_dataset_firstapproach, aes(x = Mean.approach.duration)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Mean approach duration", y = "Density")
###UI 
ggplot(final_dataset_firstapproach, aes(x = UI)) +
  geom_density() +
  labs(title = "Kernel Density Plot", x = "Urbanization index", y = "Density")

#####Bayesian GLMM#####

###LATENCY 1ST APPROACH###
##Change reference for the model if needed
#final_dataset_firstapproach$Position <- final_dataset_firstapproach$Position %>%
#relevel(ref="R")
#final_dataset_firstapproach$Level <- final_dataset_firstapproach$Level %>%
#relevel(ref="1")
#final_dataset_solving$Level <- final_dataset_solving$Level %>%
#relevel(ref="1")
#final_dataset_solving$Position <- final_dataset_solving$;Position %>%
#relevel(ref="R")

### Censored model latency ###
## Prepare data for censored model##
final_dataset_firstapproach$is_censored <- ifelse(is.na(final_dataset_firstapproach$Latency_1stapproach), 1, 0) #1 Censored data, 0 uncensored
final_dataset_firstapproach$max_latency<- as.numeric(difftime(final_dataset_firstapproach$Took.down.or.Solving, final_dataset_firstapproach$Installation.time.end., units = "secs")) #Compute max latency for each level, either when solved or when uninstalled
final_dataset_firstapproach <- final_dataset_firstapproach %>%
  mutate(Latency_1stappr_censored = ifelse(is.na(Latency_1stapproach), max_latency, Latency_1stapproach)) #New columns with latency 1st approach data integrated with censored data (max latency instead of NAs)

# model_1stapproach<- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + Position +
#                           (1 | Roost),
#                         data = final_dataset_firstapproach,
#                         family = gaussian(),
#                          cores = 6,
#                         iter = 15000,
#                        control = list(adapt_delta = 0.999))
# summary(model_1stapproach)
#POSITION has no effect, therefore I removed it (not the main variable of interest)

model_1stapproach<- brm(log(Latency_1stappr_censored) | cens(is_censored) ~ scale(O.Neill) + scale(Roost.size) + scale(UI) + Level +
                          (1 | Roost),
                        data = final_dataset_firstapproach,
                        family = gaussian(),
                        cores = 6,
                        iter = 15000,
                        control = list(adapt_delta = 0.999))
summary(model_1stapproach)

###SOLVING 1/0 AND LATENCY UNTIL SOLVING###

### Solved or not ###
# model_solving <- brm(Solved ~ UI + O.Neill + Roost.size + Position + Level +
#                        (1 | Roost),
#                      data = final_dataset_firstapproach,
#                      family = bernoulli(link = "logit"),
#                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
#POSITION has no effect, therefore I removed it (not the main variable of interest)

model_solving <- brm(Solved ~ O.Neill + UI + Roost.size + Level +
                       (1 | Roost),
                     data = final_dataset_solving,
                     family = bernoulli(link = "logit"),
                     cores = 6, iter = 15000, warmup = 2000, control = list(adapt_delta = 0.99))
summary(model_solving)

### Solved/Not solved and latency 1st approach ###
final_dataset_solving$Latency_std <- final_dataset_solving$Latency_1stapproach / sd(final_dataset_solving$Latency_1stapproach, na.rm = TRUE) # Standardize Latency_1stapproach 
model_solving2 <- brm(Solved ~  Latency_std + 
                        (1 | Roost),
                      data = final_dataset_solving,
                      family = bernoulli(link = "logit"),
                      cores = 6, iter = 15000, control = list(adapt_delta = 0.99))
summary(model_solving2)

### Censored model Latency until solving (Solving time= Solving time - Installation time end###

##Weibull##
# model_solving4 <- brm(Latency_solving_censored | cens(is_censored) ~ 
#                         scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
#                         (1 | Roost), 
#                       data = final_dataset_solving, 
#                       family = weibull(link = "log"), 
#                       cores = 6, 
#                       iter = 15000,
#                       control = list(adapt_delta = 0.99999),  save_pars = save_pars(all = TRUE))
# summary(model_solving4)
# 
# #Log transformed latency
# model_solving4b <- brm(log(Latency_solving_censored) | cens(is_censored) ~ 
#                         scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
#                         (1 | Roost), 
#                       data = final_dataset_solving, 
#                       family = weibull(link = "log"), 
#                       cores = 6, 
#                       iter = 15000,
#                       warmup = 7500,
#                       control = list(adapt_delta = 0.99999,max_treedepth=15), save_pars = save_pars(all = TRUE))
# summary(model_solving4b)
# pp_check(model_solving4b)
# conditional_effects(model_solving4b)
# 
# ##Lognormal##
# model_solving5 <- brm(
#   Latency_solving_censored | cens(is_censored) ~ 
#     scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
#     (1 | Roost), 
#   data = final_dataset_solving, 
#   family = lognormal(), 
#   cores = 6, 
#   iter = 15000,
#   control = list(adapt_delta = 0.99999),  save_pars = save_pars(all = TRUE)
# )
# summary(model_solving5)
# pp_check(model_solving5)
# 
# #Log transformed latency
# model_solving5b <- brm(
#   log(Latency_solving_censored) | cens(is_censored) ~ 
#     scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
#     (1 | Roost), 
#   data = final_dataset_solving, 
#   family = lognormal(), 
#   cores = 6, 
#   iter = 15000,
#   control = list(adapt_delta = 0.9999999),  save_pars = save_pars(all = TRUE)
# )
# summary(model_solving5b)
# pp_check(model_solving5b)

## Log transformed latency, Gaussian family ; WINNING MODEL ##
final_dataset_solving$is_censored <- ifelse(is.na(final_dataset_solving$Latency_Solving), 1, 0)
final_dataset_solving$max_latency<- as.numeric(difftime(final_dataset_solving$Took.down.or.Solving, final_dataset_solving$Installation.time.end., units = "secs"))
final_dataset_solving<- final_dataset_solving %>%
  mutate(Latency_solving_censored = ifelse(is.na(Latency_Solving2), max_latency, Latency_Solving2)) #New columns with latency solving data integrated with censored data (max latency instead of NAs)

# model_solving5c <- brm(
#   log(Latency_solving_censored) | cens(is_censored) ~ 
#     scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + Position +
#     (1 | Roost), 
#   data = final_dataset_solving, 
#   family = gaussian(), 
#   cores = 6, 
#   iter = 15000,
#   control = list(adapt_delta = 0.9999999),  save_pars = save_pars(all = TRUE)
# )
#POSITION has no effect, therefore I removed it (not the main variable of interest)

model_solving5c <- brm(
  log(Latency_solving_censored) | cens(is_censored) ~ 
    scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
    (1 | Roost), 
  data = final_dataset_solving, 
  family = gaussian(), 
  cores = 6, 
  iter = 15000,
  control = list(adapt_delta = 0.9999999),  save_pars = save_pars(all = TRUE)
)
summary(model_solving5c)
pp_check(model_solving5c)

#### Graphs #### 
conditional_effects(model_1stapproach)
conditional_effects(model_solving)
conditional_effects(model_solving2)
conditional_effects(model_solving5c)

# ### Censored model Latency until solving (Solving time= Solving time - first approach start###
# final_dataset_solving$is_censored <- ifelse(is.na(final_dataset_solving$Latency_Solving), 1, 0)
# final_dataset_solving$max_latency<- as.numeric(difftime(final_dataset_solving$Took.down.or.Solving, final_dataset_solving$Installation.time.end., units = "secs")) #Compute max latency for each level, either when solved or when uninstalled
# final_dataset_solving<- final_dataset_solving %>%
#   mutate(Latency_solving_censored = ifelse(is.na(Latency_Solving), max_latency, Latency_Solving)) #New columns with latency solving data integrated with censored data (max latency instead of NAs)
# 
# #Weibull
# model_solving3 <- brm(log(Latency_solving_censored) | cens(is_censored) ~ 
#                         scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
#                         (1 | Roost), 
#                       data = final_dataset_solving, 
#                       family = weibull(link = "log"), 
#                       cores = 6, 
#                       iter = 15000,
#                       control = list(adapt_delta = 0.99999))
# summary(model_solving3)
# 
# #Lognormal 
# model_solving3 <- brm(
#   log(Latency_solving_censored) | cens(is_censored) ~ 
#     scale(O.Neill) + scale(Roost.size) + scale(UI) + Level + 
#     (1 | Roost), 
#   data = final_dataset_solving, 
#   family = lognormal(), 
#   cores = 6, 
#   iter = 15000,
#   control = list(adapt_delta = 0.99999)
# )
# 
# # model_solving3_lognormal <- brm(
# #   bf(Latency_Solving | cens(is_censored) ~ O.Neill + Roost.size + UI + Level + (1 | Roost)), 
# #   data = final_dataset_firstapproach, 
# #   family = lognormal(),
# #   cores = 6, 
# #   iter = 15000, 
# #   control = list(adapt_delta = 0.999)
# # )
# 
