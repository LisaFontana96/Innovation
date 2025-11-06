rm(list = ls())
cat("\014")
graphics.off()
library(dplyr)
library(lubridate)
library(tidyr)

# Load both datasets
summary_data <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXPERIMENT2023_102025_LF.csv')
detailed_data <- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/DATA/DATASET/Finaldataset_SCC.csv')

# Count approaches before solving for each Roost-Level combination
# approaches_before_solving <- detailed_data %>%
#   group_by(Roost, Level) %>%
#   summarise(
#     # Count total approaches (all rows for that Roost-Level)
#     total_approaches = n(),
#     # Find if there was a solving event
#     was_solved = any(Approach.type == "Solving" | !is.na(Solving.time)),
#     # Count approaches BEFORE solving (exclude the solving approach itself)
#     approaches_before_solve = ifelse(
#       was_solved,
#       sum(Approach.type != "Solving" & is.na(Solving.time)),  # Only count non-solving approaches
#       NA  # If never solved, return NA
#     ),
#     .groups = "drop"
#   )
# 
# # Merge with summary data
# summary_data_updated <- summary_data %>%
#   left_join(approaches_before_solving, by = c("Roost" = "Roost", "LEVEL" = "Level"))

engagement_summary <- detailed_data %>%
  mutate(
    approach_start_datetime = ymd_hms(paste(Approach.date, Approach.start)),
    approach_end_datetime = ymd_hms(paste(Approach.date, Approach.end)),
    approach_duration = as.numeric(difftime(
      approach_end_datetime,
      approach_start_datetime,
      units = "mins"
    )),
    # Flag solving approaches
    is_solving_approach = (Approach.type == "Solving" | !is.na(Solving.time))
  ) %>%
  # For each task, find if and when it was solved
  group_by(Roost, Level) %>%
  mutate(
    task_was_solved = any(is_solving_approach),
    solving_approach_number = ifelse(task_was_solved, 
                                     min(which(is_solving_approach)), 
                                     NA)
  ) %>%
  ungroup() %>%
  mutate(
    # Mark whether this approach happened before solving
    before_solving = ifelse(task_was_solved, 
                            Approach.n <= solving_approach_number, 
                            TRUE)  # All approaches count if never solved
  ) %>%
  group_by(Roost, LEVEL = Level) %>%
  summarise(
    # All approaches (including post-solving)
    n_approaches_total = n(),
    total_engagement_time_all = sum(approach_duration, na.rm = TRUE),
    
    # Only approaches up to and including solving
    n_approaches_to_solve = sum(before_solving),
    total_engagement_time_to_solve = sum(approach_duration[before_solving], na.rm = TRUE),
    
    mean_approach_duration = mean(approach_duration, na.rm = TRUE),
    .groups = "drop"
  )

summary_with_engagement <- summary_data %>%
  left_join(engagement_summary, by = c("Roost", "LEVEL")) %>%
  mutate(
    # Replace NAs with 0 for never-approached tasks
    n_approaches_total = replace_na(n_approaches_total, 0),
    n_approaches_to_solve = replace_na(n_approaches_to_solve, 0),
    total_engagement_time_all = replace_na(total_engagement_time_all, 0),
    total_engagement_time_to_solve = replace_na(total_engagement_time_to_solve, 0)
  )

# Save 
write.csv(summary_with_engagement, 
          '/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/ANU_PhD/CCE_Lab/InnovationTask/Innovation/RESULTS/DataCleaning/SUMMARY_EXPERIMENT2023_102025_LF.csv', 
          row.names = FALSE)
