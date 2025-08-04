#Thuan Nguyen
#OMBSA 5300 - Data Exploration 

library(tidyverse)
library(rio)
library(lubridate)


# Read in trends data ------------------------------------------------------------

#Read in Google trends files
trends_files <- list.files(path = "data", pattern = "trends_up_to_.*\\.csv", full.names = TRUE)

#Import and bind together
trends_raw <- import_list(trends_files, rbind = TRUE)


# Processing Google data ---------------------------------------------------------

#Process the combined trends data
trends_processed <- trends_raw %>%
  #Create a proper date variable. The date is the start of the week.
  mutate(date = ymd(str_sub(monthorweek, 1, 10))) %>%
  #Standardize index by creating a z-score (subtract mean, divide by standard deviation).
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup()


#Processing school data ----------------------------------------------

#Read in Scorecard data
scorecard <- import("data/Most+Recent+Cohorts+(Scorecard+Elements).csv")

#Read in id_name_link
id_link <- import("data/id_name_link.csv")

#Drop duplicate schname
id_link_unique <- id_link %>%
  group_by(schname) %>%
  filter(n() == 1) %>% # Keep only names that appear exactly once
  ungroup()


# Linking data--------------------------------------------------
#First join trends to id_link by schname, then join to scorecard by UNITID
data_joined <-inner_join(id_link_unique, by = "schname") %>%
  inner_join(scorecard, by = c("unitid" = "UNITID"))

