rm(list = ls())
cat("\014")
library(brms)
library(hms)
library(RcppEigen)
library(ggplot2)
library(nortest)
library(dplyr)
library(summarytools)
library(lme4)  
library(lmerTest) 
 
data<- read.csv('/Users/u7585399/Library/CloudStorage/OneDrive-AustralianNationalUniversity/LISA/CCE_Lab/InnovationTask/Innovation/ANALYSIS/DataCleaning/Dataset_Neophobia2.0.csv')

#########Data cleaning###########
data$Approach.start <- as_hms(data$Approach.start)
data$Approach.end <- as_hms(data$Approach.end)
data$Approach.start <- as.POSIXct(paste(data$Approach.date, data$Approach.start),
                                  format="%Y%m%d %H:%M:%S")
data$Approach.end <- as.POSIXct(paste(data$Approach.date, data$Approach.end),
                                format="%Y%m%d %H:%M:%S")
data$Solving.time <- as.POSIXct(paste(data$Approach.date, data$Solving.time),
                                format="%Y%m%d %H:%M:%S")
data$Installation.time.end.<- as.POSIXct(paste(data$Installation.date, data$Installation.time.end.),
                                       format="%Y%m%d %H:%M")
data <- data %>% arrange(Roost, Level, Installation.date)
data$Position<- as.factor(data$Position)
data$Level<- as.factor(data$Level)
data$ApproachDuration<- as.numeric(difftime(data$Approach.end, data$Approach.start, units = "secs")) #Approach duration
data$Latency_1stapproach <- ifelse(data$Approach.n == 1, difftime(data$Approach.start, data$Installation.time.end., units = "secs"), NA) #Latency 1st approach

###Build the final dataset
exclude_column_index <- which(names(data) == "Solving.time")
firstapproach_data <- data[data$Approach.n == 1, -exclude_column_index]
solving_data<- na.omit(data[, c("Solving.time", "Roost", "Level")])
final_dataset<-left_join(firstapproach_data, solving_data, by = c("Roost", "Level"))

##To compute
final_dataset$Latency_Solving <- as.numeric(difftime(final_dataset$Solving.time, final_dataset$Approach.start, units = "secs")) #Latency until solving
final_dataset <- final_dataset %>% 
  left_join(aggregate(data$ApproachDuration ~ Roost + Level + Roost.size, data = data, mean), #Mean approach duration
            by = c("Roost", "Level", "Roost.size"))
approach_summary <- data %>%
  filter(Approach.type %in% c("Presence", "Attempt")) %>% #Index for n of approaches before solving or un-installation or data end
  group_by(Roost, Level) %>%
  summarize(Approaches = max(Approach.n, na.rm = TRUE))
final_dataset <- left_join(final_dataset, approach_summary, by = c("Roost", "Level")) #Join the data of the index with the final dataset
final_dataset <- final_dataset %>%
  mutate(Approaches = replace(Approaches, is.na(Approaches), 0)) #Replace missing values with 0 (it's still informative)
#1 = solved by SC cockatoos, 0 = unsolved or not solved by SC cockatoos
final_dataset$Solved <- ifelse(is.na(final_dataset$Latency_Solving), 0, 1)
approach_sum <- aggregate(ApproachDuration ~ Roost + Level, data = data, FUN = sum)
final_dataset <- merge(final_dataset, approach_sum, by = c("Roost", "Level"), all.x = TRUE)

##Tidy the dataset
final_dataset<- final_dataset %>% select(-Approach.date, -ApproachDuration.x, -Approach.n, -Approach.start, -Approach.end, -Approach.type) 
names(final_dataset)[names(final_dataset) == "data$ApproachDuration"] <- "Mean approach duration" #Rename the new column
names(final_dataset)[names(final_dataset) == "ApproachDuration.y"] <- "Approaches Duration"

