rm(list = ls())
cat("\014")
graphics.off()
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)

### Load data ###
summary <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXPERIMENT2023_102025_LF.csv')

### Cleaning ###
summary <- summary %>%
  mutate(
    datetime_1stapproach = ymd_hms(paste(as.character(X1st.APPROACH.date), as.character(X1st.APPROACH.time))),
    datetime_endpoint1 = ymd_hms(paste(as.character(END.POINT.1st.approach), as.character(END.POINT.1st.approach.time))),
    datetime_endpoint2 = ymd_hms(paste(as.character(END.POINT.solving), as.character(END.POINT.solving.time))),
    datetime_solving = ymd_hms(paste(as.character(SOLVING.date), as.character(SOLVING.time))),
    datetime_installation = ymd_hms(paste(as.character(INSTALLATION.DATE.END), as.character(INSTALLATION.TIME.END))),
    already_solved = case_when(
      X1st.APPROACH.latency == 'To compute' ~ 1,
      is.na(X1st.APPROACH.latency) ~ NA_real_,
      TRUE ~ 0
    ),
    X1st.APPROACH.latency = if_else(
      X1st.APPROACH.latency == 'To compute',
      as.numeric(difftime(datetime_1stapproach, datetime_installation, units = "secs")),
      as.numeric(X1st.APPROACH.latency, na.rm = TRUE)
    )
  )

summary <- summary %>%
  mutate(
    X1st.APPROACH.latency = ifelse(
      (is.na(X1st.APPROACH.latency) & FATE.1st.approach %in% c("Never approached", "No data")),
      as.numeric(difftime(datetime_endpoint1, datetime_installation, units = "secs")),
      X1st.APPROACH.latency
    ),
    SOLVING.latency.1 = ifelse(
      is.na(SOLVING.latency.1),
      as.numeric(difftime(datetime_endpoint2, datetime_installation, units = "secs")),
      SOLVING.latency.1
    ),
    SOLVING.latency.2 = ifelse(
      is.na(SOLVING.latency.2),
      as.numeric(difftime(datetime_endpoint2, datetime_installation, units = "secs")),
      SOLVING.latency.2
    ),
    X1st.APPROACH.latency = round(X1st.APPROACH.latency / 60),
    SOLVING.latency.1 = round(SOLVING.latency.1 / 60),
    SOLVING.latency.2 = round(SOLVING.latency.2 / 60),
    Position = as.factor(Position),
    LEVEL = as.factor(LEVEL),
    Roost = as.factor(Roost),
    Degree = as.factor(Degree),
    WTDegree = as.numeric(WTDegree),
    Roost.size = as.numeric(Roost.size),
    LEVEL_grouped = factor(ifelse(LEVEL == 1, "1", "2-3"), levels = c("1", "2-3"))
  )

# Calculate success rate per roost
roost_summary <- summary %>%
  group_by(Roost) %>%
  summarise(
    success_rate = mean(SOLVED.SCC, na.rm = TRUE),
    n_solved = sum(SOLVED.SCC, na.rm = TRUE),
    n_total = n(),
    urbanization = first(UI),
    entropy = first(O.Neill),
    roost_size = first(Roost.size),
    degree = first(Degree),
    total_approaches = sum(n_approaches_total, na.rm = TRUE),
    total_engagement = sum(total_engagement_time_all, na.rm = TRUE)
  )

### --- Plot setup parameters ---
base_theme <- theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

### --- Individual plots (scatter only, smaller points) ---

p1 <- ggplot(roost_summary, aes(x = urbanization, y = success_rate)) +
  geom_point(size = 1.5, alpha = 0.8) +
  ylim(0, 1) +
  labs(x = "Urbanization Index", y = "Success Rate") +
  base_theme

p2 <- ggplot(roost_summary, aes(x = entropy, y = success_rate)) +
  geom_point(size = 1.5, alpha = 0.8) +
  ylim(0, 1) +
  labs(x = "Environmental Entropy (O’Neill)", y = "Success Rate") +
  base_theme

p3 <- ggplot(roost_summary, aes(x = roost_size, y = success_rate)) +
  geom_point(size = 1.5, alpha = 0.8) +
  ylim(0, 1) +
  labs(x = "Roost Size", y = "Success Rate") +
  base_theme

p4 <- ggplot(roost_summary, aes(x = as.factor(degree), y = success_rate)) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.8) +
  ylim(0, 1) +
  labs(x = "Connectivity class (Degree)", 
       y = "Success rate") +
  scale_x_discrete(labels = c("0" = "D0", "1" = "D1", "2" = "D2")) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

p5 <- ggplot(roost_summary, aes(x = total_engagement, y = success_rate)) +
  geom_point(size = 1.5, alpha = 0.8) +
  ylim(0, 1) +
  labs(x = "Total engagement time (minutes)", y = "Success rate") +
  base_theme

p6 <- ggplot(roost_summary, aes(x = total_approaches, y = success_rate)) +
  geom_point(size = 1.5, alpha = 0.8) +
  ylim(0, 1) +
  labs(x = "Total number of approaches", y = "Success rate") +
  base_theme

# Combine into one figure (3x2 layout)
combined_plot <- (p1 | p2) / (p3 | p4) / (p5 | p6)
combined_plot

# ## With regression line and SD
# # Plot 1: Success rate vs Urbanization
# p1 <- ggplot(roost_summary, aes(x = urbanization, y = success_rate)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_smooth(method = "loess", span = 1.2, se = TRUE, color = "blue", alpha = 0.3) +
#   ylim(0, 1) +
#   labs(x = "Urbanization Index", 
#        y = "Success Rate",
#        title = "A. Urbanization") +
#   theme_minimal(base_size = 12) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # Plot 2: Success rate vs Entropy
# p2 <- ggplot(roost_summary, aes(x = entropy, y = success_rate)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_smooth(method = "loess", span = 1.2, se = TRUE, color = "blue", alpha = 0.3) +
#   ylim(0, 1) +
#   labs(x = "Environmental Entropy (O'Neill)", 
#        y = "Success Rate",
#        title = "B. Environmental Heterogeneity") +
#   theme_minimal(base_size = 12) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # Plot 3: Success rate vs Roost size
# p3 <- ggplot(roost_summary, aes(x = roost_size, y = success_rate)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_smooth(method = "loess", span = 1.2, se = TRUE, color = "blue", alpha = 0.3) +
#   ylim(0, 1) +
#   labs(x = "Roost Size", 
#        y = "Success Rate",
#        title = "C. Roost Size") +
#   theme_minimal(base_size = 12) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # Plot 4: Success rate vs Connectivity
# p4 <- ggplot(roost_summary, aes(x = degree, y = success_rate)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_smooth(method = "loess", span = 1.2, se = TRUE, color = "blue", alpha = 0.3) +
#   ylim(0, 1) +
#   labs(x = "Connectivity (Degree)", 
#        y = "Success Rate",
#        title = "D. Connectivity") +
#   theme_minimal(base_size = 12) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # Combine into one figure (2x2 layout)
# combined_plot <- (p1 | p2) / (p3 | p4)
# combined_plot