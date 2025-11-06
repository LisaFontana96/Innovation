rm(list = ls())
cat("\014")
graphics.off()
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
library(broom)
library(coxme)
library(lme4)  

summary_data <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXPERIMENT2023_102025_LF.csv')
summary_data <- summary_data %>%
  mutate(
    LEVEL = as.factor(LEVEL),
    Roost = as.factor(Roost),
    Degree = as.numeric(Degree),
    X1st.APPROACH.latency = as.numeric(X1st.APPROACH.latency),
    WTDegree = as.numeric(WTDegree),
    Roost.size = as.numeric(Roost.size)
  )

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
#With tot number of approaches before solving = engagement
model3<- glmer(SOLVED.SCC ~ scale(n_approaches_to_solve) + LEVEL + scale(Roost.size) + 
        Degree + scale(UI) + scale(O.Neill) + (1|Roost),
      data = summary_data,
      family = binomial)
summary(model3)

#With tot approaches duration before solving = engagement time
model3b <- glmer(
  SOLVED.SCC ~ scale(total_engagement_time_to_solve) + LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill) + (1|Roost),
  data = summary_data,
  family = binomial
)
summary(model3b)

#With time to first approach
#Roost as random effect has been removed because the variance was essentially 0 (1.0872e-08)
model3c <- glm(
  SOLVED.SCC ~ scale(X1st.APPROACH.latency) + LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill),
  data = summary_data,
  family = binomial
)
summary(model3c)

### GRAPHS FREQUENTIST ###
# Model 1: What predicts engagement time (all)
freq_eng1 <- broom.mixed::tidy(model, conf.int = TRUE)  
freq_eng1 <- freq_eng1 %>%
  filter(effect == "fixed", term != "(Intercept)") %>%
  mutate(
    term_clean = case_when(
      term == "LEVEL2" ~ "Level 2 vs 1",
      term == "LEVEL3" ~ "Level 3 vs 1",
      term == "scale(Roost.size)" ~ "Roost Size (scaled)",
      term == "Degree" ~ "Connectivity (Degree)",
      term == "scale(UI)" ~ "Urbanization (scaled)",
      term == "scale(O.Neill)" ~ "Entropy  (scaled)",
      TRUE ~ term
    )
  )

ggplot(freq_eng1, aes(x = estimate, y = term_clean)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Coefficient estimate (log scale)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Model 2: Engagement before solving (same structure as model 1)
freq_eng2 <- broom.mixed::tidy(model2, conf.int = TRUE)  
freq_eng2 <- freq_eng2 %>%
  filter(effect == "fixed", term != "(Intercept)") %>%
  mutate(
    term_clean = case_when(
      term == "LEVEL2" ~ "Level 2 vs 1",
      term == "LEVEL3" ~ "Level 3 vs 1",
      term == "scale(Roost.size)" ~ "Roost Size (scaled)",
      term == "Degree" ~ "Connectivity (Degree)",
      term == "scale(UI)" ~ "Urbanization (scaled)",
      term == "scale(O.Neill)" ~ "Entropy (scaled)",
      TRUE ~ term
    )
  )

ggplot(freq_eng2, aes(x = estimate, y = term_clean)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Coefficient estimate (log scale)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Model 3: Does engagement predict solving (with scaled engagement)
freq_eng3 <- broom.mixed::tidy(model3b, conf.int = TRUE, conf.method = "Wald")
freq_eng3 <- freq_eng3 %>%
  filter(effect == "fixed", term != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(estimate),
    or_lower = exp(conf.low),
    or_upper = exp(conf.high),
    term_clean = case_when(
      term == "scale(total_engagement_time_to_solve)" ~ "Engagement Time (scaled)",
      term == 'scale(n_approaches_to_solve)' ~ "N. approaches (scaled)",
      term == "LEVEL2" ~ "Level 2 vs 1",
      term == "LEVEL3" ~ "Level 3 vs 1",
      term == "scale(Roost.size)" ~ "Roost Size (scaled)",
      term == "Degree" ~ "Connectivity (Degree)",
      term == "scale(UI)" ~ "Urbanization (scaled)",
      term == "scale(O.Neill)" ~ "Entropy (scaled)",
      TRUE ~ term
    )
  )

ggplot(freq_eng3, aes(x = estimate, y = term_clean)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Coefficient estimate (log odds)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Model 3c: Does time to first approach predict solving?
freq_eng3c <- broom::tidy(model3c, conf.int = TRUE)
freq_eng3c <- freq_eng3c %>%
  filter(term != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(estimate),
    or_lower = exp(conf.low),
    or_upper = exp(conf.high),
    term_clean = case_when(
      term == "scale(X1st.APPROACH.latency)" ~ "Time to 1st approach (scaled)",
      term == "LEVEL2" ~ "Level 2 vs 1",
      term == "LEVEL3" ~ "Level 3 vs 1",
      term == "scale(Roost.size)" ~ "Roost Size (scaled)",
      term == "Degree" ~ "Connectivity (Degree)",
      term == "scale(UI)" ~ "Urbanization (scaled)",
      term == "scale(O.Neill)" ~ "Entropy (scaled)",
      TRUE ~ term
    )
  )

ggplot(freq_eng3c, aes(x = estimate, y = term_clean)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Coefficient estimate (log odds)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


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
  SOLVED.SCC ~ scale(total_engagement_time_to_solve) + LEVEL + scale(Roost.size) + #change in total_engagement_time_all for total engagement time
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

# Model 3b: Does n of approaches predict solving?
bayes_solving_with_engagementb <- brm(
  SOLVED.SCC ~ scale(n_approaches_to_solve) + LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill) + (1|Roost),
  data = summary_data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = Intercept),
    prior(normal(0, 1.5), class = b),
    prior(exponential(1), class = sd)),
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 123, control = list(adapt_delta = 0.999))

summary(bayes_solving_with_engagementb)
plot(bayes_solving_with_engagementb)

# Model 3c: Does time to first approach predict solving?
bayes_solving_with_engagementc <- brm(
  SOLVED.SCC ~ scale(X1st.APPROACH.latency) + LEVEL + scale(Roost.size) + 
    Degree + scale(UI) + scale(O.Neill) + (1|Roost),
  data = summary_data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = Intercept),
    prior(normal(0, 1.5), class = b),
    prior(exponential(1), class = sd)),
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 123, control = list(adapt_delta = 0.999))

summary(bayes_solving_with_engagementc)
plot(bayes_solving_with_engagementc)

### COMPUTE POSTERIOR PROBABILITIES ###
# Model 1: Engagement time (all)
posterior1 <- posterior::as_draws_df(bayes_engagement_all)
fixed_effects1 <- posterior1[, grep("^b_", names(posterior1))]
fixed_effects1 <- fixed_effects1[, !grepl("Intercept", names(fixed_effects1))]

posterior_probs1 <- sapply(fixed_effects1, function(x) mean(x > 0))
posterior_probs_df1 <- data.frame(
  Parameter = names(posterior_probs1),
  Posterior_Prob_gt_0 = round(posterior_probs1, 3),
  Posterior_Prob_lt_0 = round(1 - posterior_probs1, 3)
)

# Model 2: Engagement time (before solving)
posterior2 <- posterior::as_draws_df(bayes_engagement_before)
fixed_effects2 <- posterior2[, grep("^b_", names(posterior2))]
fixed_effects2 <- fixed_effects2[, !grepl("Intercept", names(fixed_effects2))]

posterior_probs2 <- sapply(fixed_effects2, function(x) mean(x > 0))
posterior_probs_df2 <- data.frame(
  Parameter = names(posterior_probs2),
  Posterior_Prob_gt_0 = round(posterior_probs2, 3),
  Posterior_Prob_lt_0 = round(1 - posterior_probs2, 3)
)

# Model 3: Does engagement predict solving
posterior3 <- posterior::as_draws_df(bayes_solving_with_engagement)
fixed_effects3 <- posterior3[, grep("^b_", names(posterior3))]
fixed_effects3 <- fixed_effects3[, !grepl("Intercept", names(fixed_effects3))]

posterior_probs_df3 <- data.frame(
  Parameter = gsub("b_", "", names(posterior_probs3)), 
  Posterior_Prob_gt_0 = round(posterior_probs3, 3),
  Posterior_Prob_lt_0 = round(1 - posterior_probs3, 3),
  row.names = NULL
)

# Model 3b: Does n approaches predict solving
posterior3 <- posterior::as_draws_df(bayes_solving_with_engagementb)
fixed_effects3 <- posterior3[, grep("^b_", names(posterior3))]
fixed_effects3 <- fixed_effects3[, !grepl("Intercept", names(fixed_effects3))]

posterior_probs3 <- colMeans(fixed_effects3 > 0)

posterior_probs_df3 <- data.frame(
  Parameter = gsub("b_", "", names(posterior_probs3)), 
  Posterior_Prob_gt_0 = round(posterior_probs3, 3),
  Posterior_Prob_lt_0 = round(1 - posterior_probs3, 3),
  row.names = NULL
)

### GRAPHS BAYESIAN ###
# Extract posteriors for all 3 models
posterior1 <- as_draws_df(bayes_engagement_all)
posterior2 <- as_draws_df(bayes_engagement_before)
posterior3 <- as_draws_df(bayes_solving_with_engagement) #change to bayes_solving_with_engagementb for engagement = n approaches

# Model 1: Total engagement time
posterior_long1 <- posterior1 %>%
  select(starts_with("b_"), -b_Intercept) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "estimate") %>%
  mutate(predictor_clean = case_when(
    predictor == "b_LEVEL" ~ "Level",
    predictor == "b_scaleRoost.size" ~ "Roost Size (scaled)",
    predictor == "b_Degree" ~ "Connectivity (Degree)",
    predictor == "b_scaleUI" ~ "Urbanization (scaled)",
    predictor == "b_scaleO.Neill" ~ "Entropy (scaled)"
  ))

ggplot(posterior_long1, aes(x = estimate, y = predictor_clean)) +
  stat_halfeye(.width = c(0.90, 0.95)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Posterior estimate (log scale)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Model 2: Engagement before solving
posterior_long2 <- posterior2 %>%
  select(starts_with("b_"), -b_Intercept) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "estimate") %>%
  mutate(predictor_clean = case_when(
    predictor == "b_LEVEL" ~ "Level",
    predictor == "b_scaleRoost.size" ~ "Roost Size (scaled)",
    predictor == "b_Degree" ~ "Connectivity (Degree)",
    predictor == "b_scaleUI" ~ "Urbanization (scaled)",
    predictor == "b_scaleO.Neill" ~ "Entropy (scaled)"
  ))

ggplot(posterior_long2, aes(x = estimate, y = predictor_clean)) +
  stat_halfeye(.width = c(0.90, 0.95)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Posterior estimate (log scale)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Model 3: Does engagement predict solving
posterior_long3 <- posterior3 %>%
  select(starts_with("b_"), -b_Intercept) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "estimate") %>%
  mutate(predictor_clean = case_when(
    predictor == "b_scaletotal_engagement_time_to_solve" ~ "Engagement Time (scaled)",  
    predictor == "b_scalen_approaches_to_solve" ~ "N approaches (scaled)", 
    predictor == "b_scaleX1st.APPROACH.latency" ~ "Time to 1st approach (scaled)",
    predictor == "b_LEVEL2" ~ "Level 2 vs 1",
    predictor == "b_LEVEL3" ~ "Level 3 vs 1",
    predictor == "b_scaleRoost.size" ~ "Roost Size (scaled)",
    predictor == "b_Degree" ~ "Connectivity (Degree)",
    predictor == "b_scaleUI" ~ "Urbanization (scaled)",
    predictor == "b_scaleO.Neill" ~ "Entropy (scaled)"
  ))

ggplot(posterior_long3, aes(x = estimate, y = predictor_clean)) +
  stat_halfeye(.width = c(0.90, 0.95)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Posterior estimate (log odds)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Get confidence intervals
ci <- confint(model2, method = "Wald", parm = "beta_")

# Get odds ratios and their CIs
exp(fixef(model))  # Odds ratios
exp(ci)  # OR confidence intervals
