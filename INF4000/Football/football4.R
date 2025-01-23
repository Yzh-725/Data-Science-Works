library(dplyr)
library(tidyr) 
library(stringr)
library(ggplot2) 
library(scales)
library(viridis)
library(ggsci)
library(ggalluvial)

# Read match information data from CSV file
match_info <- read.csv("C:/Users/ZhuanZ/Desktop/due/4000/cervus-uefa-euro-2020/Match information.csv")
str(match_info)  # Display structure of the dataset
head(match_info)  # Display first few rows of the dataset

# Re-group the match stages and calculate the total number of goals for each match
goal_data <- match_info %>%
  mutate(
    TotalGoals = ScoreHome + ScoreAway,  # Calculate total goals scored (home + away)
    RoundGroup = case_when(
      RoundName %in% c("quarter finals", "semi finals", "final") ~ "Knockout Rounds",  # Group knockout rounds
      RoundName == "final tournament" ~ "Final Tournament",  # Final tournament stage
      RoundName == "eighth finals" ~ "Eighth Finals"  # Round of 16
    )
  ) %>%
  select(RoundGroup, TotalGoals)  # Retain only the relevant columns (RoundGroup and TotalGoals)

# Plot a boxplot to visualize the distribution of total goals across different match stages
ggplot(goal_data, aes(x = RoundGroup, y = TotalGoals, fill = RoundGroup)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.8) +  # Outliers highlighted in red with increased transparency
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Apply a customized viridis color palette
  theme_minimal() +  # Use a minimal theme for the plot
  labs(
    title = "Goal Distribution Across Different Match Stages in Euro 2020",
    x = "Match Stage",  # Label for the x-axis
    y = "Total Goals",  # Label for the y-axis
    fill = "Stage Group"  # Label for the fill legend
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and center-align title
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  # Customize x-axis text appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold the x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold the y-axis title
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "none",  # Hide the legend
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add a border around the plot
  )

# Another version of the boxplot with slightly modified styling
ggplot(goal_data, aes(x = RoundGroup, y = TotalGoals, fill = RoundGroup)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.8) +  # Highlight outliers in red
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Use a softer viridis color palette
  theme_minimal() +  # Apply minimal theme
  labs(
    title = "Goal Distribution Across Different Match Stages in Euro 2020",
    x = "Match Stage",  # x-axis label
    y = "Total Goals Scored",  # y-axis label
    fill = "Stage Group"  # Legend label for event categories
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Larger, bold title
    axis.title.x = element_text(size = 14, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5),  # Adjust x-axis text size and alignment
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Add major gridlines with light grey color
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "none",  # Hide the legend
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add a border around the panel
  )
