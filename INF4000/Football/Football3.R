library(dplyr)
library(tidyr) 
library(stringr)
library(ggplot2) 
library(scales)
library(viridis)
library(ggsci)
library(ggalluvial)
library(fmsb)

# Load the dataset for player statistics
file_path <- "C:/Users/ZhuanZ/Desktop/due/4000/cervus-uefa-euro-2020/Match player statistics.csv"
player_stats <- read.csv(file_path)

# Define the distance-related statistics to be considered
distance_stats <- c("Distance covered in low activity (m)", 
                    "Distance covered in medium activity (m)", 
                    "Distance covered in high activity (m)", 
                    "Distance covered in very high activity (m)")

# Assign weights for each distance category (customizable)
weights <- c(low = 1, medium = 2, high = 3, very_high = 4)

# Filter the player stats for relevant distance data and calculate weighted distance
distance_data <- player_stats %>%
  filter(StatsName %in% distance_stats) %>%
  mutate(
    Weight = case_when(
      StatsName == "Distance covered in low activity (m)" ~ weights["low"],
      StatsName == "Distance covered in medium activity (m)" ~ weights["medium"],
      StatsName == "Distance covered in high activity (m)" ~ weights["high"],
      StatsName == "Distance covered in very high activity (m)" ~ weights["very_high"]
    ),
    WeightedValue = as.numeric(Value) * Weight,  # Calculate the weighted value
    FullName = ifelse(PlayerSurname == "", PlayerName, paste(PlayerName, PlayerSurname))  # Create a full name column
  ) %>%
  group_by(FullName) %>%  # Group data by player
  summarise(Distance = sum(WeightedValue, na.rm = TRUE), .groups = "drop")

# Transform data into a long format suitable for radar chart
distance_data_long <- distance_data %>%
  mutate(
    StatsName = "Distance",  # Set the statistic name
    TotalValue = Distance  # Assign the calculated distance to TotalValue
  ) %>%
  select(FullName, StatsName, TotalValue)

# Define the selected players for analysis
selected_players <- data.frame(
  PlayerName = c("Lorenzo", "Harry", "Álvaro", "Mikkel"),
  PlayerSurname = c("Insigne", "Kane", "Morata", "Damsgaard")  # Corresponding surnames
)

# Filter the player data for selected players and relevant statistics
player_data <- player_stats %>%
  mutate(FullName = paste(PlayerName, PlayerSurname)) %>%  # Combine first name and surname
  filter(FullName %in% paste(selected_players$PlayerName, selected_players$PlayerSurname) &
           StatsName %in% c("Goals", "Total Attempts", "Attempts on target", 
                            "Passes completed")) %>%
  group_by(FullName, StatsName) %>%
  summarise(TotalValue = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

# Merge the distance data with player data
distance_data_long <- distance_data_long %>%
  filter(FullName %in% player_data$FullName)  

player_data <- bind_rows(player_data, distance_data_long)

# Reshape the data into a wide format for radar chart
radar_data <- player_data %>%
  pivot_wider(names_from = StatsName, values_from = TotalValue, values_fill = 0)

# Add max and min rows for radar chart scaling
radar_data_fmsb <- radar_data %>%
  mutate(across(-FullName, ~ . / max(., na.rm = TRUE) * 20))  # Standardize to a 0-20 scale
radar_data_fmsb <- rbind(
  rep(20, ncol(radar_data)),  # Max values
  rep(0, ncol(radar_data)),   # Min values
  radar_data
)

radar_data_fmsb <- as.data.frame(radar_data_fmsb)
rownames(radar_data_fmsb) <- c("Max", "Min", radar_data$FullName)
radar_data_fmsb <- radar_data_fmsb %>%
  select(-FullName)

# Rescale values to fit between 5 and 20 for better visualization
radar_data_fmsb <- radar_data_fmsb %>%
  mutate(across(
    everything(),
    ~ scales::rescale(., to = c(5, 20)),
    .names = "{col}"
  ))

radar_data_fmsb[-c(1, 2), ] <- radar_data_fmsb[-c(1, 2), ] %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(5, 20))))

# Define colors for the radar chart
colors_border <- c(rgb(0.2, 0.5, 0.5, 0.9), rgb(0.8, 0.2, 0.5, 0.9), 
                   rgb(0.7, 0.5, 0.1, 0.9), rgb(0.1, 0.7, 0.8, 0.9))
colors_in <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4), 
               rgb(0.7, 0.5, 0.1, 0.4), rgb(0.1, 0.7, 0.8, 0.4))

title <- "Core Offensive Player Data from 2020 UEFA Euro Semi-Final Teams"

# Plot the radar chart
radarchart(radar_data_fmsb, axistype = 1,
           pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 20, 5), cglwd = 0.8,
           vlcex = 0.8)

# Add legend to the chart
legend(x = 0.7, y = 1, legend = rownames(radar_data_fmsb[-c(1, 2), ]), 
       bty = "n", pch = 20, col = colors_border, text.col = "black", cex = 0.8, pt.cex = 2)

# Add title to the chart
title(main = title, col.main = "black", font.main = 2, cex.main = 1.1, line = 1)
