#Thuan Nguyen
#OMBSA 5300 - Data Exploration 

library(tidyverse)
library(rio)
library(lubridate)


# Loading data ------------------------------------------------------------

#Read in Google trends files
trends_files <- list.files(path = "data", pattern = "trends_up_to_.*\\.csv", full.names = TRUE)

#Import and bind together
trends_raw <- import_list(trends_files, rbind = TRUE)


# Processing data ---------------------------------------------------------

# Process the combined trends data
trends_processed <- trends_raw %>%
  # Create a proper date variable. The date is the start of the week.
  mutate(date = ymd(str_sub(monthorweek, 1, 10))) %>%
  # Standardize index by creating a z-score (subtract mean, divide by standard deviation).
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup()
 