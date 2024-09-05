library(tidyverse)
library(dplyr)
library(lubridate)
setwd("C:/Users/user/Desktop/Job searching/pacersproblem/Question1")

# Read the CSV files
box <- read_csv("combined_box.csv")
advanced <- read_csv("combined_advanced.csv")
schedule <- read_csv("schedule.csv")
scoring<- read_csv("scoring.csv")

# Add Game ID "202312230IND" to the advanced dataframe if Game ID is null
advanced <- advanced %>%
  mutate(`Game ID` = ifelse(is.na(`Game ID`), "202312230IND", `Game ID`))

# Add Game ID "202312230IND" to the advanced dataframe if Game ID is null
scoring <- scoring %>%
  mutate(`Game ID` = ifelse(is.na(`Game ID`), "202312210MEM", `Game ID`))

# Merge datasets
boxadvanced <- merge(box, advanced, by.x = c("PLAYER", "Game ID"), by.y = c("PLAYER", "Game ID"))
boxadvanced <- merge(boxadvanced, schedule, by = "Game ID")

boxadvanced <- boxadvanced %>%
  mutate(W = ifelse(`W/L` == "W", 1, 0))

columns_to_remove <- c("Web", "Away", "Opponent", "W/L", "Notes")  # Replace with the names of the columns you want to remove

# Remove specified columns from merged_df
boxadvanced <- boxadvanced %>%
  select(-one_of(columns_to_remove))

boxadvanced_player <- boxadvanced %>%
  filter(PLAYER != 'TOTALS' & !is.na(OFFRTG))

advanced_formula1 <- W ~ FGM + `FG%` + `3PM` + `3P%` + FTM + `FT%` + OREB + DREB +
  AST + STL + BLK + TO + PF  + OFFRTG + DEFRTG + `AST%` + `AST/TO` +
  `AST RATIO` + `OREB%` + `DREB%` + `TO RATIO` + `EFG%` + `TS%` + `USG%` + PACE + PIE

advancedlog_model1 <- glm(advanced_formula1, data = boxadvanced_player, family = "binomial")
summary(advancedlog_model1)

advanced_formula2 <- W ~ OREB + DREB +STL+ PF  + OFFRTG + DEFRTG  +
  `OREB%` + `DREB%` 

advancedlog_model2 <- glm(advanced_formula2, data = boxadvanced_player, family = "binomial")
summary(advancedlog_model2)

# hustle stats varibles through log modeling
# Read the CSV files
hustle <- read_csv("combined_hustle.csv")
# Merge datasets
hustle_df <- merge(hustle, schedule, by = "Game ID")

# Convert W/L to binary response variable W
hustle_df <- hustle_df %>%
  mutate(W = ifelse(`W/L` == "W", 1, 0))

# Remove unnecessary columns
columns_to_remove <- c("Web", "Away", "Opponent", "W/L", "Notes")  
hustle_df <- hustle_df %>%
  select(-one_of(columns_to_remove))

# Rename columns
current_colnames <- colnames(hustle_df)
new_colnames <- gsub("\n", " ", current_colnames)
colnames(hustle_df) <- new_colnames

hustle_df <- hustle_df %>%
  filter(PLAYER != 'TOTALS')

# Define model formulas
hustleformula1 <- W ~ `SCREEN AST` + `SCREEN AST PTS` + `deflections` +  
  `OFF LOOSE BALLS RECOVERED` + `DEF LOOSE BALLS RECOVERED` + 
  `CHARGES DRAWN` + `CONTESTED 2PT SHOTS` + `CONTESTED 3PT SHOTS` + 
  `OFF BOX OUTS` + `DEF BOX OUTS`
hustlelog1 <- glm(hustleformula1, data = hustle_df, family = "binomial")
summary(hustlelog1)
hustleformula2 <- W ~ `SCREEN AST` + `SCREEN AST PTS` +   
  `OFF LOOSE BALLS RECOVERED` + `DEF LOOSE BALLS RECOVERED` + 
  `CONTESTED 3PT SHOTS` + `DEF BOX OUTS`
hustlelog2 <- glm(hustleformula2, data = hustle_df, family = "binomial")
summary(hustlelog2)
hustleformula3 <- W ~ `SCREEN AST` + `SCREEN AST PTS` +   
  `OFF LOOSE BALLS RECOVERED`  + 
  `CONTESTED 3PT SHOTS` + `DEF BOX OUTS`
hustlelog3 <- glm(hustleformula3, data = hustle_df, family = "binomial")
summary(hustlelog3)

# Scoring Modeling
# Read the CSV files
scoring <- read_csv("scoring.csv")
# Merge datasets
scoring_df <- merge(scoring, schedule, by = "Game ID")

# Convert W/L to binary response variable W
scoring_df <- scoring_df %>%
  mutate(W = ifelse(`W/L` == "W", 1, 0))

# Remove unnecessary columns
columns_to_remove <- c("Web", "Away", "Opponent", "W/L", "Notes")  
scoring_df <- scoring_df %>%
  select(-one_of(columns_to_remove))

scoring_df <- scoring_df %>%
  filter(!grepl("[a-zA-Z]", MIN))

scoring_df <- scoring_df %>%
  filter(PLAYER != 'TOTALS')
##log formula
scoringformula1 <- W ~ `%FGA\n2PT` + `%FGA\n3PT` + `%PTS\n2PT` + `%PTS\n2PT MR` +
  `%PTS\n3PT` + `%PTS\nFBPS` + `%PTS\nFT` + `%PTS\nOFFTO` + `%PTS\nPITP` +
  `2FGM\n%AST` + `2FGM\n%UAST` + `3FGM\n%AST` + `3FGM\n%UAST` +
  `FGM\n%AST` + `FGM\n%UAST`

scoringlog1 <- glm(scoringformula1, data = scoring_df, family = "binomial")

# Summarize the model
summary(scoringlog1)

scoringformula2 <- W ~ `%PTS\nFBPS` + `%PTS\nOFFTO` 
scoringlog2<- glm(scoringformula2, data = scoring_df, family = "binomial")
summary(scoringlog2)

# combined dataframe:
# Merge hustle_df and scoring_df on 'Game ID' and 'PLAYER'
combined_df <- merge(hustle_df, scoring_df, by = c("Game ID", "PLAYER"))

# Merge combined_df and boxadvanced_player on 'Game ID' and 'PLAYER'
final_df <- merge(combined_df, boxadvanced_player, by = c("Game ID", "PLAYER"))

combined_formula1 <- W ~ OREB + DREB + STL + PF + OFFRTG + DEFRTG + `OREB%` + `DREB%` +
  `SCREEN AST` + `SCREEN AST PTS` + `OFF LOOSE BALLS RECOVERED` + 
  `CONTESTED 3PT SHOTS` + `DEF BOX OUTS` + `%FGA\n2PT` + `%PTS\nFBPS` + `%PTS\nOFFTO`

combinedformulalog1 <- glm(combined_formula1, data = final_df, family = "binomial")
summary(combinedformulalog1)

combined_formula2 <- W ~ OREB + DREB + STL + PF + OFFRTG + DEFRTG + `OREB%` + `DREB%` +
  `OFF LOOSE BALLS RECOVERED` + 
  `%PTS\nFBPS` + `%PTS\nOFFTO`

combinedformulalog2 <- glm(combined_formula2, data = final_df, family = "binomial")
summary(combinedformulalog2)

# Function to perform 5-fold cross-validation
k_fold_cv <- function(data, formula){
  set.seed(1)  # Set seed for reproducibility
  folds <- cut(seq(1, nrow(data)), breaks = 5, labels = FALSE)  # Create folds
  
  accuracies <- c()  # Store accuracies for each fold
  
  for (i in 1:5) {
    valid_inds <- which(folds == i)  # Indices of the validation set
    train_fold <- data[-valid_inds, ]  # Training set
    valid_fold <- data[valid_inds, ]   # Validation set
    
    # Train model
    model <- glm(formula, data = train_fold, family = "binomial")
    
    # Predict on validation set
    predictions <- predict(model, newdata = valid_fold, type = "response")
    classifications <- ifelse(predictions > 0.5, 1, 0)  # Adjust threshold as needed
    
    # Compute accuracy
    correct_classifications <- ifelse(classifications == valid_fold$W, 1, 0)
    accuracy <- sum(correct_classifications) / length(correct_classifications)
    
    accuracies <- c(accuracies, accuracy)
  }
  
  mean_accuracy <- mean(accuracies)
  return(mean_accuracy)
}

# Perform cross-validation for both model formulas
cv_accuracy_1 <- k_fold_cv(final_df, combined_formula1)
cv_accuracy_2 <- k_fold_cv(final_df, combined_formula2)

cv_accuracy_1  # Mean cross-validation accuracy for model 1
cv_accuracy_2  # Mean cross-validation accuracy for model 2

## Making Predictions
playerdata = read.csv("playeraverage.csv")
# Convert column names to uppercase
colnames(playerdata) <- toupper(colnames(playerdata))
colnames(playerdata) <- gsub("\\.", "%", colnames(playerdata))


# Update col_to_keep to match the column names in playerdata dataframe
col_to_keep <- c("PLAYER","TEAM","AGE","GP","MIN", "OREB", "DREB", "STL", "PF", "OFFRTG", "DEFRTG", 
                 "OREB%", "DREB%", "OFF%LOOSE%BALLSRECOVERED", 
                 "X%PTSFBPS", "X%PTSOFFTO")

# Select only the columns specified in col_to_keep
playerdata <- playerdata[col_to_keep]

# Correct the column name and rename
playerdata <- playerdata %>%
  rename(
    OREB = OREB,
    DREB = DREB,
    STL = STL,
    PF = PF,
    OFFRTG = OFFRTG,
    DEFRTG = DEFRTG,
    `OREB%` = `OREB%`,
    `DREB%` = `DREB%`,
    `OFF LOOSE BALLS RECOVERED` = `OFF%LOOSE%BALLSRECOVERED`,  # Corrected column name
    `%PTS\nFBPS` = `X%PTSFBPS`,
    `%PTS\nOFFTO` = `X%PTSOFFTO`
  )

# Predict probabilities for each player
playerdata$predicted_prob <- predict(combinedformulalog2, newdata = playerdata, type = "response")

# Rank players by predicted probabilities
ranked_players <- playerdata %>%
  arrange(desc(predicted_prob))  # Arrange in descending order of predicted probabilities

# Display the ranked players
print(ranked_players[, c("PLAYER", "predicted_prob")])

freeagent<- read.csv('FreeAgent.csv')
freeagent <- freeagent %>%
  rename(PLAYER = player)

freeagent_ranked_players <- left_join(freeagent, ranked_players, by ="PLAYER")

freeagent_ranked_players<- freeagent_ranked_players%>%
  mutate(MINPLAYED = MIN*GP)

library(ggplot2)

# Plotting MINPLAYED vs. predicted_prob
ggplot(data=freeagent_ranked_players, aes(x = MINPLAYED, y = predicted_prob)) +
  geom_point(size =2) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +  # Vertical line at 20 minutes
  labs(x = "Minutes Played", y = "Predicted Probability") +
  ggtitle("MINPLAYED vs. Predicted Probability")+
  theme_bw()

freeagent_ranked_players<-freeagent_ranked_players%>%
  filter(MINPLAYED>100)%>%
  mutate(salary = as.numeric(gsub("[^0-9.]", "",salary)))%>%
  mutate(value = predicted_prob/(salary/1e6))

ranked_players<-ranked_players%>%
  mutate(MINPLAYED = MIN*GP)%>%
  filter(MINPLAYED>100)%>%
  filter(AGE<29)

boxadvanced_team<- boxadvanced%>%
  filter(PLAYER=='TOTALS')

freeagent_ranked_players<-freeagent_ranked_players%>%
  filter(age<29)

pacersplayers<- playerdata%>%
  filter(TEAM=='IND')

pacers_rank<- left_join(pacersplayers, ranked_players, by="PLAYER")

write.csv(freeagent_ranked_players, "freeagentunder29.csv", row.names = FALSE)
write.csv(pacers_rank,"pacersrank.csv", row.names = FALSE)