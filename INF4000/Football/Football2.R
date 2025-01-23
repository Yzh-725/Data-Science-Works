library(dplyr)
library(tidyr) 
library(stringr)
library(ggplot2) 
library(scales)
library(viridis)
library(ggsci)
library(ggalluvial)

# Load all CSV files from the specified folder into a list
folder_path <- "C:/Users/ZhuanZ/Documents/INF4000Football/cervus-uefa-euro-2020"
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
data_list <- list()

# Read each CSV file and store in the data_list
for (file in file_list) {
  data <- read.csv(file)  
  data_list[[file]] <- data  
}

# Assign descriptive names to the datasets in the list
names(data_list) <- c("Match_events", "Match_information", "Match_lineups", 
                      "Match_player_statistics", "Match_team_statistics", "Pre_match_information")

# Combine match event data with match information (score details)
event_flow_data <- data_list$Match_events %>%
  left_join(data_list$Match_information %>%
              select(MatchID, ScoreHome, ScoreAway), 
            by = "MatchID") %>%
  mutate(
    Result = case_when(
      ScoreHome > ScoreAway ~ "Win",  # If home team wins
      ScoreHome < ScoreAway ~ "Loss",  # If away team wins
      TRUE ~ "Draw"  # If it's a draw
    ),
    EventGroup = case_when(
      Event %in% c("Goal", "GoalOnPenalty", "OwnGoal") ~ "Goal Events",  # Goal-related events
      Event %in% c("GoalAttemptOnTarget", "GoalAttemptBlocked", "GoalAttemptSaved", 
                   "GoalAttemptOffTarget", "GoalAttemptPost", "GoalAttemptBar") ~ "Shots",  # Shot-related events
      Event %in% c("Foul", "YellowCard", "RedCard") ~ "Fouls",  # Foul-related events
      Event %in% c("Corner", "FreeKick") ~ "Set Pieces",  # Set piece events
      TRUE ~ NA_character_  # Other events are ignored
    )
  ) %>%
  filter(!is.na(EventGroup))  # Remove events that do not fit into the predefined groups

# Prioritize events: Goal Events > Shots > Fouls > Set Pieces
event_flow_data <- event_flow_data %>%
  group_by(MatchID, Minute) %>%
  arrange(desc(case_when(
    EventGroup == "Goal Events" ~ 4,
    EventGroup == "Shots" ~ 3,
    EventGroup == "Fouls" ~ 2,
    EventGroup == "Set Pieces" ~ 1
  ))) %>%
  slice(1) %>%  # Keep the highest priority event for each minute
  ungroup()

# Generate event transitions by looking at the next event in the match
event_transitions <- event_flow_data %>%
  arrange(MatchID, Minute) %>%
  group_by(MatchID) %>%
  mutate(NextEvent = lead(EventGroup)) %>%  # Get the next event in the sequence
  filter(!is.na(NextEvent)) %>%  # Remove invalid event pairs
  ungroup() %>%
  count(EventGroup, NextEvent, name = "Count")  # Count the number of transitions between events

# Adjust the flow by applying weights to certain events
event_transitions <- event_transitions %>%
  mutate(
    Weight = case_when(
      EventGroup == "Goal Events" ~ 8,  # Goals are given the highest weight
      NextEvent == "Goal Events" ~ 8,  # Amplify goal-related event flows
      TRUE ~ 1  # Keep other events at normal scale
    ),
    AdjustedCount = Count * Weight  # Adjust the flow count by the weight
  )

# Create a dynamic alluvial plot to show event transitions and flows
ggplot(event_transitions, aes(axis1 = EventGroup, axis2 = NextEvent, y = AdjustedCount)) +
  geom_alluvium(aes(fill = EventGroup), width = 1/12, alpha = 0.8) +  # Use alluvial flows to show transitions
  geom_stratum(width = 1/5, fill = "grey", color = "black") +  # Use grey stratum to highlight event groups
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +  # Label the event groups
  scale_fill_viridis_d(option = "D") +  # Apply the Viridis color scale for better visual contrast
  theme_minimal() +  # Use minimal theme for a clean look
  labs(
    title = "Dynamic Match Event Flow",  # Title of the plot
    x = "Event Categories",  # Label for the x-axis
    y = "Relative Weight of Event Flows",  # Label for the y-axis
    fill = "Event Type"  # Legend label for event categories
  ) +
  theme(
    axis.title = element_text(size = 14),  # Adjust axis title size
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_blank(),  # Hide y-axis labels
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_rect(fill = "white", color = NA),  # Set a white background
    legend.position = "right",  # Position the legend on the right
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Style the title
  )
