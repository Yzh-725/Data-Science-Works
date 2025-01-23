# **INF6027 Project: Player Behaviors and Team Success Analysis**

## **Project Overview**
This project investigates player behaviors and their influence on match outcomes and team success in UEFA Euro 2020. Using data from 51 matches and over 1,500 player statistics, multiple regression models were applied to identify the most significant predictors of goals scored at both match and tournament levels.

## **Repository Structure**
The repository is organized as follows:
- **Code/**: `Match stats1.R` scripts used for data preprocessing, exploratory data analysis (EDA), and regression modeling.
- **Data/**: Contains processed datasets used for analysis.
- **Output/**: Includes generated plots, tables, and regression results.
- **Documentation/**: Contains the project report and supplementary documentation, such as this README file.

## **Key Findings**
1. **At the match level**:
   - **Attempts on Target** is the strongest predictor of goals scored (coefficient = 0.45, p < 0.001).
   - **Passes Accuracy** positively impacts goal-scoring efficiency (coefficient = 0.03, p < 0.01).
   - Defensive actions like **Clearances Successful** contribute indirectly to offensive opportunities (coefficient = 0.05, p < 0.01).
2. **At the tournament level**:
   - **Delivery into the Penalty Area** positively impacts overall success (coefficient = 0.04, p < 0.05).
   - Over-reliance on **Crosses Completed** showed a negative impact (coefficient = -0.05, p < 0.05).

For more detailed results, see the project report in the **Documentation/** folder.

## **How to Run the Code**
Follow these steps to reproduce the analysis:
1. **Clone the repository**:
   ```bash
   git clone https://github.com/Yzh-725/Data-Science-works
   
2.**Install required R libraries**: 
  Use the following command in R to install the necessary packages:install.packages(c("dplyr", "tidyr", "ggplot2", "car", "lmtest"))
  
3.**Run the scripts**:Open Match stats1.R in RStudio.
  Set the working directory to the repository root.
  Run each section of the script sequentially to preprocess the data, perform EDA, and build regression models.
## **Acknowledgments**
This project was conducted as part of the INF6027 module at University of Sheffield. Special thanks to the course instructors for providing guidance and resources throughout the analysis.
