#Thuan Nguyen
#OMSBA5300
#Data_Exploration- The College Scoreboard

#Loading Libraries
library(tidyverse)
library(rio)
library(lubridate)
library(modelsummary)

# Load Google trends data
trends_files <- list.files(path = "data", pattern = "trends_up_to_.*\\.csv", full.names = TRUE)
trends_raw <- import_list(trends_files, rbind = TRUE)

# Clean up the trends data
trends_processed <- trends_raw %>%
  # Make a proper date variable from the date strings
  mutate(date = ymd(str_sub(monthorweek, 1, 10))) %>%
  # Standardize the Google Trends index since it's not comparable across different keywords
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup() %>%
  # Aggregate to monthly data to make analysis easier
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(schname, month_date) %>%
  summarise(index_std_agg = mean(index_std, na.rm = TRUE), .groups = 'drop')

# Load school ID linking file
id_link <- import("data/id_name_link.csv")
# Drop schools with duplicate names 
id_link_unique <- id_link %>%
  group_by(schname) %>%
  filter(n() == 1) %>%
  ungroup()

# Load College Scorecard data
scorecard <- import("data/Most+Recent+Cohorts+(Scorecard+Elements).csv")
scorecard_clean <- scorecard %>%
  # Only keep 4-year colleges (PREDDEG=3)
  filter(PREDDEG == 3) %>%
  select(UNITID, `md_earn_wne_p10-REPORTED-EARNINGS`) %>%
  # Convert earnings to numeric (this makes "PrivacySuppressed" into NA)
  mutate(median_earnings = as.numeric(`md_earn_wne_p10-REPORTED-EARNINGS`)) %>%
  filter(!is.na(median_earnings)) %>%
  # Split schools into high vs low earning based on median
  mutate(High_Earning = ifelse(median_earnings > median(median_earnings), 1, 0)) %>%
  select(UNITID, High_Earning)

# Merge all the datasets together
merged_data <- inner_join(trends_processed, id_link_unique, by = "schname")
final_data <- inner_join(merged_data, scorecard_clean, by = c("unitid" = "UNITID"))

# Set up variables for diff-in-diff analysis
analysis_df <- final_data %>%
  # Create Post variable for after Scorecard release
  mutate(Post = ifelse(month_date >= ymd("2015-09-01"), 1, 0),
         month = month(month_date)) %>%
  filter(!is.na(index_std_agg))

# Export cleaned data
export(analysis_df, "data/cleaned_data.rds")