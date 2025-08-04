#Thuan Nguyen
#OMBSA 5300 - Data Exploration 

library(tidyverse)
library(rio)
library(lubridate)

#Read in Google trends files
trends_files <- list.files(path = "rawdata", pattern = "data\trends_up_to_.*\\.csv", full.names = TRUE)

#Import and bind together
trends_raw <- import_list(trends_files, rbind = TRUE)
