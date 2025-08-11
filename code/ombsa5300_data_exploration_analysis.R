#Thuan Nguyen
#OMSBA5300
#Data_Exploration- The College Scoreboard

#Loading Libraries
library(tidyverse)
library(rio)
library(lubridate)
library(fixest)
library(modelsummary)

# Load cleaned data
analysis_df <- import("cleaned_data.rds")

# Make plot data for visualization
plot_data <- analysis_df %>%
  group_by(High_Earning, month_date) %>%
  summarise(avg_index = mean(index_std_agg, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Group = ifelse(High_Earning == 1, "High-Earning", "Low-Earning"))

# Create the diff-in-diff plot
did_plot <- ggplot(plot_data, aes(x = month_date, y = avg_index, color = Group, linetype = Group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = ymd("2015-09-01"), linetype = "dashed", color = "black") +
  annotate("text", x = ymd("2015-09-01") + days(10), y = 0.25,
           label = "Scorecard Release\n(Sept 2015)", hjust = 0, color = "black") +
  labs(
    title = "Student Interest in Colleges Before and After Scorecard Release",
    subtitle = "Monthly Avg of Google Search Index",
    x = "Date",
    y = "Avg Search Index",
    color = "College Category",
    linetype = "College Category"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("High-Earning" = "#0072B2", "Low-Earning" = "#D55E00"))

print(did_plot)

# Run the diff-in-diff regression
# This tests whether the Scorecard changed search interest differently for high vs low earning schools
# The interaction term Post * High_Earning is what we care about
did_model <- feols(index_std_agg ~ Post * High_Earning | month,
                   data = analysis_df,
                   cluster = ~unitid)

# Show results
summary(did_model)

