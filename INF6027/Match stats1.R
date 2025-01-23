
#===============================================================================
#STEP 1: LOAD LIBRARIES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) # For VIF in model diagnostics

#===============================================================================
#STEP 2: LOAD DATASET
#===============================================================================
# Read data
df <- read.csv('Match_player_statistics.csv')

#===============================================================================
#STEP 3: CREATE A DATAFRAME FOR IMPORTANT MATCH STATISTICS
#===============================================================================
# Define the list of stats to keep
selected_stats <- c(
  "Goals",
  "Big Chances",
  "Attempts on target",
  "Attempts off target",
  "Crosses completed",
  "Dribbling",
  "Passes completed",
  "Passes accuracy",
  "Delivery into penalty area",
  "Corners",
  "Attempts on direct free kick",
  "Tackles won",
  "Recovered balls",
  "Clearances successful",
  "Saves",
  "Blocks",
  "Fouls committed",
  "Tackles performed gaining the ball",
  "Crosses attempted"
)

# Filter the data frame for the selected stats
stats_df <- df[df$StatsName %in% selected_stats, ]

#===============================================================================
#STEP 4: RETAIN ONLY IMPORTANT COLUMNS IN THE DATAFRAME
#===============================================================================
# Select only the desired columns
stats_df <- stats_df[, c("MatchID", "HomeTeamName", "AwayTeamName", "PlayerID", "StatsName", "Value")]

#===============================================================================
#STEP 5: SET VALUE COLUMN AS NUMERIC
#===============================================================================
# Convert the Value column to numeric
stats_df$Value <- as.numeric(stats_df$Value)

#===============================================================================
#STEP 6: CREATE COLUMNS FOR EACH STATISTIC WITH VALUES FROM Value COLUMN
#===============================================================================
# Widen the stats_df while keeping the necessary columns
stats_df <- stats_df %>%
  distinct() %>%      # Ensure we have distinct rows for each PlayerID, MatchID, and StatsName combination
  pivot_wider(
    names_from = StatsName,   # Create columns from StatsName
    values_from = Value,      # Fill the columns with values from the Value column
    values_fill = list(Value = NA)  # Fill missing values with NA
  )

#===============================================================================
#STEP 7: GROUP STATS DATAFRAME BY MATCHID AND SPLIT DATA INTO TRAINING AND TEST SETS
#===============================================================================
# Group stats_df by MatchID and sum the remaining columns (calculating per match)
match_df <- stats_df %>%
  group_by(MatchID) %>%
  summarise(across(-c(HomeTeamName, AwayTeamName, PlayerID), 
                   \(x) sum(x, na.rm = TRUE))) 


# Randomly shuffle the data
set.seed(123) # For reproducibility
match_df <- match_df[sample(nrow(match_df)), ]

# Split data into 80% training and 20% testing
train_index <- floor(0.8 * nrow(match_df))
train_set <- match_df[1:train_index, ]
test_set <- match_df[(train_index + 1):nrow(match_df), ]

#===============================================================================
#STEP 8: Exploratory Data Analysis
#===============================================================================
#1. Summary Statistics
print("Summary Statistics for Match-Level Data:")
summary(match_df)  # Display min, max, median, mean, and quartiles of numeric variables

# 2. Visualize Distribution of Goals
library(ggplot2)
ggplot(match_df, aes(x = Goals)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Goals", x = "Goals", y = "Frequency")

#===============================================================================
#STEP 9: FIRST REGRESSION (Match-Level)
#===============================================================================
# Perform regression on match-level training data
match_regression <- lm(Goals ~ `Big Chances` + `Attempts on target` + `Attempts off target` + 
                         `Crosses completed` + `Dribbling` + `Passes completed` +
                         `Passes accuracy` + `Delivery into penalty area` + `Corners` + 
                         `Attempts on direct free kick` + `Tackles won` + `Recovered balls` + 
                         `Clearances successful` + `Saves` + `Blocks` + `Fouls committed`
                       + `Tackles performed gaining the ball` + `Crosses attempted`, 
                       data = train_set)

# View the summary of the regression results
summary(match_regression)


#===============================================================================
#STEP 10: MODEL DIAGNOSTICS
#===============================================================================
# Match-level diagnostics
par(mfrow = c(2, 2))  # Set plotting area for 4 diagnostic plots
plot(match_regression)

# Variance Inflation Factor (VIF) for multicollinearity check
vif(match_regression)

#===============================================================================
#STEP 11: CREATE PlayerTeam COLUMN
#===============================================================================
# Create PlayerTeam column based on the most frequent team for each player
player_teams <- stats_df %>%
  group_by(PlayerID) %>%
  summarise(
    teams = list(c(HomeTeamName, AwayTeamName)),
    .groups = 'drop'
  ) %>%
  mutate(
    team_counts = sapply(teams, function(teams_list) {
      table(teams_list)
    }),
    PlayerTeam = sapply(team_counts, function(counts) {
      most_frequent_team <- names(counts)[which.max(counts)]
      return(most_frequent_team)
    })
  ) %>%
  select(PlayerID, PlayerTeam)

# Join the PlayerTeam back to the stats_df
stats_df <- stats_df %>%
  left_join(player_teams, by = "PlayerID")


#===============================================================================
#STEP 12: GROUP STATS DATAFRAME BY PlayerTeam
#===============================================================================
# Group stats_df by PlayerTeam and sum the remaining columns (calculating per team)
stats_team_df <- stats_df %>%
  group_by(PlayerTeam) %>%
  summarise(across(-c(MatchID, HomeTeamName, AwayTeamName, PlayerID), 
                   \(x) sum(x, na.rm = TRUE))) 

#===============================================================================
#STEP 13: SECOND REGRESSION (Team-Level)
#===============================================================================
# Perform regression on team-level data to see how stats influence goals for each team
team_regression <- lm(Goals ~ `Big Chances` + `Attempts on target` + `Attempts off target` + 
                        `Crosses completed` + `Dribbling` + `Passes completed` +
                        `Passes accuracy` + `Delivery into penalty area` + `Corners` + 
                        `Attempts on direct free kick` + `Tackles won` + `Recovered balls` + 
                        `Clearances successful` + `Saves` + `Blocks` + `Fouls committed`
                      + `Tackles performed gaining the ball` + `Crosses attempted`, 
                      data = stats_team_df)

# View the summary of the team-level regression results
summary(team_regression)

#===============================================================================
# STEP 14: Analysis of Deviance Table for Match-Level Regression and Team-Level Regression
#===============================================================================
# Generate Analysis of Deviance Table for match-level regression
anova_match <- anova(match_regression)
print("Analysis of Deviance Table for Match-Level Regression:")
print(anova_match)

# Generate Analysis of Deviance Table for team-level regression
anova_team <- anova(team_regression)
print("Analysis of Deviance Table for Team-Level Regression:")
print(anova_team)
