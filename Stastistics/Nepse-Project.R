# Import Data
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

banking_data <- read_csv("/home/tekraj/MSc-Mathematics-Github/Stastistics/data/nepsealpha_export_price_BANKING_2021-01-16_2026-01-16_unadjusted.csv")

# drop turn over column
banking_data <- banking_data[,-9]
# gsub() function is used to "find and replace" characters
# Remove commas
banking_data$Volume <- gsub(",", "", banking_data$Volume)

#convert to numeric and date
banking_data$Volume <- as.numeric(banking_data$Volume)
banking_data$Date <- as.Date(banking_data$Date)

# drop NA
banking_data <- banking_data[!is.na(banking_data$Volume), ]


# define which days were "Up" (Close > Open) or "Down"
banking_data <- banking_data |> mutate(direction = ifelse(Close >= Open, "Up", "Down"))

#  Build the plot
# 1. Prepare Data
# 1. Filter for the last 30 days
# We use max(Date) to find the most recent trading day in your CSV
one_month_data <- banking_data |>
  filter(Date >= (max(Date) - days(30))) |>
  mutate(direction = ifelse(Close >= Open, "Up", "Down"))

# 2. Build the focused Plot
one_month_plot <- ggplot(one_month_data, aes(x = Date)) +
  # The Wicks (High/Low)
  geom_segment(aes(xend = Date, y = Low, yend = High), color = "black") +
  
  # The Bodies (Open/Close)
  # Width is increased to 0.7 since there are fewer bars now
  geom_rect(aes(xmin = Date - 0.35, xmax = Date + 0.35, 
                ymin = pmin(Open, Close), ymax = pmax(Open, Close), 
                fill = direction), color = "black", size = 0.2) +
  
  # Colors & Scales
  scale_fill_manual(values = c("Up" = "#26a69a", "Down" = "#ef5350")) +
  
  # Improve X-Axis to show specific days
  scale_x_date(date_labels = "%d %b", date_breaks = "2 days") +
  
  theme_minimal() +
  labs(
    title = "NEPSE Banking Index: Last 30 Days",
    subtitle = "Daily Candlestick View",
    x = "Date",
    y = "Price (NPR)"
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 3. View in VS Code
options(vsc.plot = TRUE)
print(one_month_plot)

# Just typing the object name in VS Code triggers the Plot Viewer
ggsave("/mnt/c/Users/tekra/OneDrive/Desktop/nepse_candle_chart.png", plot = one_month_plot, width = 10, height = 6)
