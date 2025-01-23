library(dplyr)
library(tidyr) 
library(stringr)
library(ggplot2) 
library(scales)
library(viridis)
library(ggsci)

# Load dataset from specified folder path
folder_path <- "C:/Users/ZhuanZ/Documents/INF4000Football/cervus-uefa-euro-2020"
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
data_list <- list()

# Read each CSV file into the data_list
for (file in file_list) {
  data <- read.csv(file)  
  data_list[[file]] <- data  
}

# Assign names to the data frames in the list
names(data_list) <- c("Match_events", "Match_information", "Match_lineups", 
                      "Match_player_statistics", "Match_team_statistics", "Pre_match_information")

# Print the dimensions and column names for each dataset
lapply(seq_along(data_list), function(i) {
  cat("TableName：", names(data_list)[i], "\n")  
  print(dim(data_list[[i]]))                   
  print(names(data_list[[i]]))                  
  cat("-----------------\n")
})

# Show the first few rows of the match events dataset
head(data_list$Match_events)
# Get summary statistics for the match events dataset
summary(data_list$Match_events)

# Check for missing values in each dataset
lapply(data_list, function(df) {
  colSums(is.na(df))  
})

# Calculate the total number of matches
total_matches <- n_distinct(data_list$Match_events$MatchID)

# Define events that are not useful for analysis
useless_events <- c("StartFirstHalf", "FoulCausingPenalty", "FoulCausingPenalty", "EndFirstHalf", 
                    "StartSecondHalf", "EndSecondHalf", "StartFirstExtraTime", "VarNotification")

# Standardize event data by grouping and filtering events
event_trend_data <- data_list$Match_events %>%
  filter(Minute <= 90) %>%
  filter(!Event %in% useless_events) %>%
  mutate(
    Event = case_when(
      Event %in% c("GoalAttemptOnTarget", "GoalAttemptBlocked", "GoalAttemptSaved") ~ "Shots On Target",
      Event %in% c("GoalAttemptOffTarget", "GoalAttemptPost", "GoalAttemptBar") ~ "Shots Off Target",
      Event %in% c("PenaltySaved", "PenaltyWoodwork") ~ "Missed Penalties",
      Event %in% c("Substitution", "SubstitutionHalfTime") ~ "Substitutions",
      TRUE ~ Event
    ),
    EventGroup = case_when(
      Event %in% c("Goal", "GoalOnPenalty", "OwnGoal") ~ "Goal Events",
      Event %in% c("Shots On Target", "Shots Off Target", "Missed Penalties") ~ "Shots",
      Event %in% c("Foul", "YellowCard", "RedCard") ~ "Fouls",
      Event %in% c("Substitutions", "Corner", "FreeKick", "Offside") ~ "Other Events"
    )
  ) %>%
  filter(Event %in% c(
    "Goal", "GoalOnPenalty", "OwnGoal", 
    "Shots On Target", "Shots Off Target", "Missed Penalties", 
    "Foul", "YellowCard", "RedCard", 
    "Substitutions", "Corner", "FreeKick", "Offside"
  )) %>%
  mutate(
    Minute = as.numeric(Minute),
    TimeGroup = cut(Minute, breaks = seq(0, 90, by = 10), 
                    labels = paste(seq(0, 80, by = 10), seq(10, 90, by = 10), sep = "-"))
  ) %>%
  group_by(TimeGroup, EventGroup, Event) %>%
  summarise(EventCount = sum(n()) / total_matches, .groups = "drop")  # Standardize event count

# Log-transform the count for fouls and reorder the event groups
event_trend_data <- event_trend_data %>%
  mutate(
    EventCount = ifelse(EventGroup == "Fouls", log10(EventCount + 1), EventCount),
    EventGroup = factor(EventGroup, levels = c("Goal Events", "Shots", "Fouls", "Other Events"))
  )

# Define shape mapping for different event types
shape_mapping <- c(
  "Goal" = 16, "GoalOnPenalty" = 16, "OwnGoal" = 16, 
  "Shots On Target" = 17, "Shots Off Target" = 17, "Missed Penalties" = 17, 
  "Foul" = 18, "YellowCard" = 18, "RedCard" = 18, 
  "Substitutions" = 15, "Corner" = 15, "FreeKick" = 15, "Offside" = 15
)

# Define color mapping for different event types
color_mapping <- c(
  "Goal" = "#440154FF", "GoalOnPenalty" = "#31688EFF", "OwnGoal" = "#35B779FF", 
  "Shots On Target" = "#481F70FF", "Shots Off Target" = "#287C8EFF", "Missed Penalties" = "#5DC863FF", 
  "Foul" = "#443A83FF", "YellowCard" = "#21908CFF", "RedCard" = "#8FD744FF", 
  "Substitutions" = "#3B528BFF", "Corner" = "#20A486FF", "FreeKick" = "#8FD744FF", "Offside" = "#FDE725FF"
)

# Create the plot to show event trends over time
ggplot(event_trend_data, aes(x = TimeGroup, y = EventCount, color = Event, shape = Event, group = Event)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~EventGroup, scales = "free_y", ncol = 2) +
  theme_light() + 
  labs(
    title = "Match Event Trends Over Time (Log Scale for Fouls)",
    x = "Time Group (Minutes)",
    y = "Event Count",
    color = "Event Type",
    shape = "Event Type"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_color_manual(values = color_mapping) +
  scale_shape_manual(values = shape_mapping)